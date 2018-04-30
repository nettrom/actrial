#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script that goes through creations of pages in the Draft namespace and
gathers predictions about their quality.

Copyright (c) 2018 Wikimedia Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
'''

import re
import bz2
import signal
import logging
import datetime as dt

from itertools import islice, chain
from collections import namedtuple, defaultdict

import db

import mwapi
import mwapi.cli
import mwapi.errors

import ores.api
import wikiclass
from revscoring import Model

import requests

import mwparserfromhell as mwp

## Named tuple to contain revision ID and timestamp
## Note: kept this because our revision retrieving method uses it,
## but we might deprecate it.
Revision = namedtuple('Revision', ['id', 'timestamp'])

class WPAPIError(Exception):
    '''Raised if our request to get namespace names from the WP API fails.'''
    pass

class Prediction:
    def __init__(self, rev_id, rev_timestamp):
        '''
        Instantiate a prediction for the given revision ID that
        was made at the given timestamp.
        '''
        self.rev_id = rev_id
        self.rev_timestamp = rev_timestamp

        ## Prediction and probabilities for draft quality
        self.draft_pred = ""
        self.ok_prob = 0.0
        self.attack_prob = 0.0
        self.vandal_prob = 0.0
        self.spam_pro = 0.0
        
        ## Prediction and probabilities of WP 1.0 assessment quality
        self.wp10_pred = ""
        self.stub_prob = 0.0
        self.start_prob = 0.0
        self.c_prob = 0.0
        self.b_prob = 0.0
        self.ga_prob = 0.0
        self.fa_prob = 0.0

class AfCSubmission:
    '''
    Class representing an AfC submission. This class typically maps
    the parameters of Template:Afc submission
    '''
    def __init__(self, page_id, timestamp, status, rev_id, prediction):
        '''
        An AfC submission:

        :parameter page_id: ID of the page that was submitted for review
        :type page_id: int

        :parameter timestamp: date and time the submission occurred
        :type timestamp: datetime.datetime

        :parameter status: the status of the submission when we find it
        :type status: status

        :parameter rev_id: revision where the submission occurred
        :type rev_id: int

        :parameter prediction: ORES draft quality and article quality model
                   predictions for the revision submitting the page of review
        :type prediction: Prediction
        '''

        self.page_id = page_id
        self.timestamp = timestamp

        ## Status can take one of four values:
        ## "T" means not submitted for review,
        ## "R" means currently under review,
        ## "D" means declined,
        ## "W" means it was withdrawn (deleted without review),
        ## "" means it is submitted for review
        self.status = status

        self.rev_id = rev_id # revision where the submission was added
        self.prediction = prediction
        
        self.username = ''
        self.namespace = 118 # default is Draft, but it could also be User

        self.decliner = '' # username of the reviewer who declined it
        self.decline_timestamp = None # timestamp of the decline

        ## if the submission was withdrawn, the timestamp of the revision where
        ## it occurred.
        self.withdraw_timestamp = None

        ## Note that if a draft passes AfC, it's moved into Main and the
        ## AfC submission template is removed.

class Draft:
    '''
    Class representing a page in the Draft namespace.
    '''
    def __init__(self, page_id, rev_id, creation_timestamp):
        '''
        A draft with the given `page_id` created by revision `rev_id`
        on `creation_timestamp`.
        '''
        self.page_id = page_id
        self.rev_id = rev_id
        self.creation_timestamp = creation_timestamp
        self.publication_timestamp = None
        self.deletion_timestamp = None

        ## List of AfC submissions, mapping timestamp to AfCSubmission object
        self.afc_history = {}

class QualityPredictor:
    def __init__(self):
        '''
        Instantiate the quality predictor.
        '''
        
        self.db_conn = None

        ## Our prediction models
        self.draft_model = None
        self.wp10_model = None

        ## Database setup
        self.db_host = 'analytics-store.eqiad.wmnet'
        self.db_name = 'staging'
        self.db_conf = '~/.my.research.cnf'
        self.db_conn = None # database connection when up

        ## Number of revisions we'll process per batch
        self.batch_size = 250

        ## API session
        self.api_session = None
        self.user_agent = "actrial quality predictor <mwang@wikimedia.org>"
        self.api_url = "https://en.wikipedia.org"

        ## ORES API session variables, kept for now. Since we'll need to
        ## diff revisions, we have revision text and have no need to call it.
        self.ores_api_session = None
        self.ores_url = "https://ores.wikimedia.org"
        self.ores_context = "enwiki"
        self.ores_models = ["draftquality", "wp10"]

        ## Dataset of draft page creations by date
        self.draft_creations = defaultdict(list)

        ## Set of valid namespace names for checking if a move is into Main
        self.namespaces = None

    def load_models(self, draft_model_file, wp10_model_file):
        '''
        Load in the ORES models.
        '''

        self.draft_model = Model.load(open(draft_model_file, 'rb'))
        self.wp10_model = Model.load(open(wp10_model_file, 'rb'))

        ## Then use wikiclass.score(model, text) to score it

    def start_api_session(self):
        self.api_session = mwapi.Session(self.api_url,
                                         formatversion=2,
                                         user_agent=self.user_agent)
        mwapi.cli.do_login(self.api_session, self.api_url)

    def load_drafts(self, input_filename):
        '''
        Read in the dataset of revisions creating pages in the Draft namespace,
        storing them by date of creation. Expects the dataset to be
        a bzip2-compressed TSV.

        :param input_filename: path to the dataset with Draft creations
        :type input_filename: str
        '''

        with bz2.open(input_filename, mode='rt') as infile:
            infile.readline() # skip header
            for line in infile:
                (timestamp, page_id, rev_id) = line.strip().split('\t')

                # if it's not a real timestamp, skip this line
                if len(timestamp) < 19:
                    continue
                elif len(timestamp) > 19:
                    timestamp = timestamp[:-2]
                
                timestamp = dt.datetime.strptime(timestamp,
                                                 '%Y-%m-%d %H:%M:%S')
                
                page_id = int(page_id)
                rev_id = int(rev_id)

                self.draft_creations[timestamp.date()].append(
                    Draft(page_id, rev_id, timestamp)
                    )

        # ok, done
        return()

    def get_namespaces(self):
        '''
        Make a request to the API to get the names and aliases of all namespaces.
        '''
    
        self.namespaces = set()

        api_ns_url = "https://en.wikipedia.org/w/api.php?action=query&meta=siteinfo&siprop=namespaces|namespacealiases&format=json"

        response = requests.get(api_ns_url)
        if not response.status_code == 200:
            logging.error('Wikipedia API request failed')
            raise(WPAPIError)

        res_json = response.json()
        if not 'query' in res_json or not 'namespaces' in res_json['query']:
            logging.error('Wikipedia API response not as expected')
            raise(WPAPIError)
    
        for ns, ns_data in res_json['query']['namespaces'].items():
            if not 'canonical' in ns_data:
                continue
        
            self.namespaces.add(ns_data['canonical'])
            if ns_data['*'] != '':
                self.namespaces.add(ns_data['*'])

        if 'namespacealiases' in res_json['query']:
            for ns_data in res_json['query']['namespacealiases']:
                self.namespaces.add(ns_data['*'])

        # ok, done
        return()
    
    def get_rev_content(self, revids, deleted=False, batch=50, **params):
        '''
        Get the revision content for the given revisions, query for deleted
        revision if the flag is set.

        :param revids: list of Revision named tuples that we're getting
                       revisions for
        :type revids: list
        '''

        ## Add the relevant parameters
        prop_key = 'revisions'
        
        if deleted:
            prop_key = 'deletedrevisions'
            params['drvprop'] = 'ids|content|timestamp'
        else:
            params['rvprop'] = 'ids|content|timestamp'

        params['prop'] = prop_key
        
        revids_iter = iter(revids)
        while True:
            batch_ids = list(islice(revids_iter, 0, batch))
            if len(batch_ids) == 0:
                break
            else:
                try:
                    doc = self.api_session.post(
                        action='query', revids=[r.id for r in batch_ids],
                        **params)
                except mwapi.errors.APIError as e:
                    logging.error('APIError: {}'.format(e))
                    break

                for page_doc in doc['query']['pages']:
                    page_meta = {k: v for k, v in page_doc.items()
                                 if k != prop_key}
                    if prop_key in page_doc:
                        for revision_doc in page_doc[prop_key]:
                            revision_doc['page'] = page_meta
                            yield revision_doc

    def store_results(self, drafts):
        '''
        Store information about the given drafts in the database.
        '''

        ## Query to insert drafts. Page ID followed by creation, publication,
        ## and deletion timestamps.
        insert_draft = '''INSERT INTO nettrom_drafts
                          VALUES (%s, %s, %s, %s)'''

        ## Query to insert an AfC submission. Page ID, submission timestamp,
        ## review timestamp, final status, revision that was submitted
        ## for review, and timestamp of withdrawal.
        insert_afc = '''INSERT INTO nettrom_afc_submissions
                        VALUES (%s, %s, %s, %s, %s, %s)'''

        ## Query to insert predictions into the database. Rev ID and timestamp,
        ## the draft prediction and its 4 probabilities,
        ## and lastly the WP 1.0 prediction and 6 probabilities.
        insert_prediction = '''INSERT INTO nettrom_draft_predictions
                               VALUES (%s, %s,
                                       %s, %s, %s, %s, %s,
                                       %s, %s, %s, %s, %s, %s, %s)'''

        for draft in drafts:
            with db.cursor(self.db_conn) as db_cursor:
                try:
                    # insert the draft
                    db_cursor.execute(insert_draft,
                                      (draft.page_id,
                                       draft.creation_timestamp,
                                       draft.publication_timestamp,
                                       draft.deletion_timestamp))

                    # for each afc submission
                    for afc_submission in draft.afc_history.values():
                        # insert the submission
                        db_cursor.execute(insert_afc,
                                          (afc_submission.page_id,
                                           afc_submission.timestamp,
                                           afc_submission.decline_timestamp,
                                           afc_submission.status,
                                           afc_submission.rev_id,
                                           afc_submission.withdraw_timestamp))

                        # insert its prediction, if there is one
                        if afc_submission.prediction:
                            db_cursor.execute(
                                insert_prediction,
                                (afc_submission.prediction.rev_id,
                                 afc_submission.prediction.rev_timestamp,
                                 afc_submission.prediction.draft_pred,
                                 afc_submission.prediction.spam_prob,
                                 afc_submission.prediction.vandal_prob,
                                 afc_submission.prediction.attack_prob,
                                 afc_submission.prediction.ok_prob,
                                 afc_submission.prediction.wp10_pred,
                                 afc_submission.prediction.stub_prob,
                                 afc_submission.prediction.start_prob,
                                 afc_submission.prediction.c_prob,
                                 afc_submission.prediction.b_prob,
                                 afc_submission.prediction.ga_prob,
                                 afc_submission.prediction.fa_prob))
                
                    self.db_conn.commit()
                except Exception as e:
                    print('Exception: {}'.format(e))

        # ok, done
        return()

    def get_drafts(self, start_date, end_date):
        '''
        Get a list of all drafts that were created starting on `start_date`
        and up to, but not including, `end_date`.

        :param start_date: start date to get drafts from
        :type start_date: datetime.date

        :param end_date: end date to get drafts to
        :type end_date: datetime.date
        '''

        ## Make sure we have data on namespaces loaded
        self.get_namespaces()
        
        ## Drafts we'll return
        found_drafts = []
        
        ## Query to see if a given page was deleted
        deleted_query = '''SELECT log_timestamp
                           FROM enwiki.logging
                           WHERE log_type='delete'
                           AND log_action='delete'
                           AND log_page = %(page_id)s
                           AND log_timestamp > %(creation_timestamp)s
                           ORDER BY log_timestamp
                           LIMIT 1'''

        ## Query to check if a given page was moved after it was created.
        move_query = '''
    SELECT DISTINCT rev_timestamp, rev_comment
    FROM (
      (SELECT rev_timestamp, rev_comment
       FROM enwiki.revision
       WHERE rev_page = %(page_id)s
       AND rev_timestamp > %(creation_timestamp)s
       AND rev_comment REGEXP ".*moved .*\\\\[\\\\[(Draft:[^\\\\]]+)\\\\]\\\\] to \\\\[\\\\[([^\\\\]]+)\\\\]\\\\].*")
      UNION
      (SELECT ar_timestamp AS rev_timestamp, ar_comment AS rev_comment
       FROM enwiki.archive
       WHERE ar_timestamp > %(creation_timestamp)s
       AND ar_page_id = %(page_id)s
       AND ar_comment REGEXP ".*moved .*\\\\[\\\\[(Draft:[^\\\\]]+)\\\\]\\\\] to \\\\[\\\\[([^\\\\]]+)\\\\]\\\\].*")
    ) AS revisiondata
    ORDER BY rev_timestamp
    LIMIT 1'''

        ## Regular expression to extract source and destination of the move
        ## from the revision comment. Source has to be in one of the namespaces
        ## we are interested in.
        move_comment_re = re.compile(".*moved .*\\[\\[((User|Draft|Wikipedia talk):[^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\]", re.U)
                
        ## For each day between start_date and end_date...
        one_day = dt.timedelta(days=1)
        cur_date = start_date
        while cur_date < end_date:
            ## Get all drafts created that day
            draft_creations = self.draft_creations[cur_date]

            ## For each, check if it was deleted...
            with db.cursor(self.db_conn, 'dict') as db_cursor:
                for draft in draft_creations:
                    db_cursor.execute(
                        deleted_query,
                        {'page_id': draft.page_id,
                         'creation_timestamp': draft.creation_timestamp.strftime('%Y%m%d%H%M%S')}
                    )
                    for row in db_cursor:
                        draft.deletion_timestamp = dt.datetime.strptime(
                            row['log_timestamp'].decode('utf-8'), '%Y%m%d%H%M%S')
                    
                    ## ...and check if it was published
                    db_cursor.execute(
                        move_query,
                        {'page_id': draft.page_id,
                         'creation_timestamp': draft.creation_timestamp.strftime('%Y%m%d%H%M%S')}
                    )
                    for row in db_cursor:
                        try:
                            rev_comment = row['rev_comment'].decode('utf-8')
                        except UnicodeDecodeError:
                            logging.warning('unable to decode rev_comment, unicode error?')
                            continue
                    
                        match = move_comment_re.match(rev_comment)
                        if not match:
                            continue
                    
                        source_ns = match.group(2)
                        target = match.group(3)

                        ## Is the target in a non-content namespace?
                        ## If so, the draft has not been published
                        if ':' in target:
                            (target_ns, title) = target.split(':', 1)
                            if target_ns in self.namespaces:
                                continue

                        ## Draft has been published
                        draft.publication_timestamp = dt.datetime.strptime(
                            row['rev_timestamp'].decode('utf-8'), '%Y%m%d%H%M%S')

            # extend the list of found drafts and iterate
            found_drafts.extend(draft_creations)
            cur_date += one_day

        return(found_drafts)

    def process_drafts(self, drafts):
        '''
        Go through the edit history of the given drafts, identify their
        AfC submission history, and get predictions for relevant revisions.
        '''

        ## current time, to be used in the revision query if necessary
        current_timestamp = dt.datetime.now(dt.timezone.utc)

        ## query to get all revisions for a live page from the revision table
        rev_live_query = '''SELECT rev_id, rev_timestamp
                            FROM enwiki.revision
                            WHERE rev_page = %(page_id)s
                            AND rev_timestamp >= %(start_timestamp)s
                            AND rev_timestamp < %(end_timestamp)s
                            ORDER BY rev_timestamp'''

        ## query to get all revisions for a deleted page from the archive table
        rev_archive_query = '''SELECT ar_rev_id AS rev_id,
                                      ar_timestamp AS rev_timestamp
                               FROM enwiki.archive
                               WHERE ar_page_id = %(page_id)s
                               AND ar_timestamp >= %(start_timestamp)s
                               AND ar_timestamp < %(end_timestamp)s
                               ORDER BY ar_timestamp'''
        
        for draft in drafts:
            ## get revisions between creation time and either publication,
            ## deletion, or current time depending on whether the page
            ## was deleted, published, or neither

            status = None
            end_timestamp = current_timestamp
            
            if draft.publication_timestamp:
                if draft.deletion_timestamp \
                   and draft.deletion_timestamp < draft.publication_timestamp:
                   status = 'deleted'
                   end_timestamp = draft.deletion_timestamp
                else:
                    status = 'published'
                    end_timestamp = draft.publication_timestamp
            elif draft.deletion_timestamp:
                status = 'deleted'
                end_timestamp = draft.deletion_timestamp

            revisions = []
            
            with db.cursor(self.db_conn, 'dict') as db_cursor:
                ## Default query for a live page, switch to archive if deleted.
                db_query = rev_live_query
                if status == 'deleted':
                    db_query = rev_archive_query

                db_cursor.execute(
                    db_query,
                    {'page_id': draft.page_id,
                         'start_timestamp': draft.creation_timestamp.strftime('%Y%m%d%H%M%S'),
                         'end_timestamp': end_timestamp.strftime('%Y%m%d%H%M%S')}
                )
                for row in db_cursor:
                    revisions.append(Revision(row['rev_id'],
                                              row['rev_timestamp'].decode('utf-8')))
            
            # grab content for those revisions,
            # store in a map from revid to content
            revision_map = {}
            for revision in self.get_rev_content(revisions,
                                                 status == 'deleted'):
                rev_id = revision['revid']
                content = revision['content']

                revision_map[rev_id] = content

            prev_rev_content = '' # content of the previous revision, for diff

            ## set of AfC submissions present in the previous revision, so that
            ## we can identify withdrawn (deleted) submissions.
            previous_submissions = set()
            
            # go through the edit history in chronological order
            for revision in revisions:
                try:
                    parsed_content = mwp.parse(revision_map[revision.id])
                except KeyError:
                    ## have no content for this revision
                    continue
                except mwp.parser.ParserError:
                    ## parsing failed, unable to process this revision
                    continue

                ## AfC submissions in this revision, which we'll diff against
                ## the previous one to identify withdrawn submissions
                current_submissions = set()
                
                templates = parsed_content.filter_templates()
                for template in templates:
                    if template.name.lower() != 'afc submission' \
                       and not template.name.matches('Articles for creation'):
                        continue

                    try: 
                        status = template.params[0].value
                    except IndexError:
                        status = ''

                    try:
                        comment = template.params[1].value
                    except IndexError:
                        comment = ''

                    try:
                        username = str(template.get('u').value)
                    except ValueError:
                        username = ''

                    try:
                        namespace = int(str(template.get('ns').value))
                    except ValueError:
                        namespace = 118

                    try:
                        timestamp = dt.datetime.strptime(
                            str(template.get('ts').value), '%Y%m%d%H%M%S')
                    except ValueError:
                        timestamp = revision.timestamp

                    if status.upper() == 'D':
                        try:
                            decliner = str(template.get('decliner').value)
                        except ValueError:
                            decliner = ''
                        try:
                            decline_timestamp = dt.datetime.strptime(
                                str(template.get('declinets').value),
                                '%Y%m%d%H%M%S')
                        except ValueError:
                            decline_timestamp = revision.timestamp

                    ## Is this an AfC submitted for review?
                    afc_submitted = False
                    if status == '':
                        afc_submitted = True

                    ## Is this a new entry or an existing one?
                    if timestamp in draft.afc_history:
                        afc = draft.afc_history[timestamp]

                        if afc.status != 'D' and status.upper() == 'D':
                            ## AfC was declined
                            afc.status = 'D'
                            afc.decliner = decliner
                            afc.decline_timestamp = decline_timestamp
                        elif afc.status == '' and status == '':
                            afc_submitted = False # submitted previously
                        elif afc.status == 'T' and status == '':
                            afc.status = '' # submitted now
                    else:
                       afc = AfCSubmission(draft.page_id, timestamp,
                                           status.upper(),
                                           revision.id, None)
                       draft.afc_history[timestamp] = afc

                    ## We have a submission, add it to the current set
                    current_submissions.add(afc)
                       
                    if afc_submitted:
                        draft_results = wikiclass.score(self.draft_model, content)
                        wp10_results = wikiclass.score(self.wp10_model, content)

                        # store stuff
                        afc.prediction = Prediction(revision.id,
                                                    revision.timestamp)
                        afc.prediction.draft_pred = draft_results['prediction']
                        afc.prediction.ok_prob = draft_results['probability']['OK']
                        afc.prediction.attack_prob = draft_results['probability']['attack']
                        afc.prediction.vandal_prob = draft_results['probability']['vandalism']
                        afc.prediction.spam_prob = draft_results['probability']['spam']
                        
                        afc.prediction.wp10_pred = wp10_results['prediction']
                        afc.prediction.stub_prob = wp10_results['probability']['Stub']
                        afc.prediction.start_prob = wp10_results['probability']['Start']
                        afc.prediction.c_prob = wp10_results['probability']['C']
                        afc.prediction.b_prob = wp10_results['probability']['B']
                        afc.prediction.ga_prob = wp10_results['probability']['GA']
                        afc.prediction.fa_prob = wp10_results['probability']['FA']

                ## We've completed checking all templates. Compare the current
                ## set with the previous set and set ones that have been
                ## submitted but been deleted to withdrawn ('W')
                submission_diff = previous_submissions - current_submissions
                for afc in submission_diff:
                    if afc.status == '':
                        afc.status = 'W'
                        afc.withdraw_timestamp = revision.timestamp

                ## Now we can iterate:
                previous_submissions = current_submissions

        # ok, done
        return()
    
    def process_historic(self, start_date, end_date, step=7):
        '''
        Process draft creations from the start date up to, but not including,
        the end date, processing a given number of days at a time.

        NOTE: Assumes that the prediction models and the dataset of draft
        creations are all loaded into memory.

        :param start_date: date of the first day to predict for
        :type start_date: datetime.date

        :param end_date: date of the last day to predict up to
        :type end_date: datetime.date

        :param draft_dataset_filename: path to the bzip2-compressed TSV dataset
                                       of page creations in the Draft namespace
        :type draft_dataset_filename: str

        :param step: number of days to process at a time
        :type step: int
        '''

        ## Connect to the database
        self.db_conn = db.connect(self.db_host, self.db_name,
                                  self.db_conf)
        if not self.db_conn:
            logging.warning('unable to connect to database server')
            return()

        
        ## Make sure we're logged in to the WP API
        self.start_api_session()

        ## Let's try to populate this a week at a time:
        time_step = dt.timedelta(days=step)

        cur_date = start_date
        while cur_date < end_date:
            stop_date = cur_date + time_step
            if stop_date > end_date:
                stop_date = end_date

            logging.info('gathering predictions from {} to {}'.format(
                cur_date, stop_date))

            # get draft creations, process them, then store the results
            drafts = self.get_drafts(cur_date, stop_date)
            self.process_drafts(drafts)
            self.store_results(drafts)

            cur_date += time_step

        # ok, done
        return()

def run_test(start_date, end_date,
             draft_model_filename, wp10_model_filename, draft_dataset_filename):
    '''
    For testing, prints some additional output.
    '''

    predictor = QualityPredictor()
    predictor.db_conn = db.connect(predictor.db_host, predictor.db_name,
                                   predictor.db_conf)
    print("Connected to database")
    predictor.load_models(draft_model_filename, wp10_model_filename)
    print("Loaded ORES models into memory")
    predictor.load_drafts(draft_dataset_filename)
    print("Loaded dataset of {} draft creations into memory".format(sum([len(val) for val in predictor.draft_creations.values()])))
    
    drafts = predictor.get_drafts(start_date, end_date)
    print("Got {} drafts between {} and {}".format(len(drafts), start_date, end_date))

    for draft in drafts:
        print('Draft {} created on {}, published on {}, deleted on {}'.format(draft.page_id, draft.creation_timestamp, draft.publication_timestamp, draft.deletion_timestamp))

    ## Make sure we're logged in to the WP API, process the draft testset
    predictor.start_api_session()
    predictor.process_drafts(drafts)

    for draft in drafts:
        print("Draft {} history:".format(draft.page_id))
        for afc in draft.afc_history.values():
            print('status "{}" submitted on {}, declined on {} by {}'.format(
                afc.status, afc.timestamp, afc.decline_timestamp, afc.decliner))
        
    ## predictor.process_historic(start_date, end_date)

    return()
                            
def main():
    import argparse

    def valid_date(d):
        try:
            return(dt.datetime.strptime(d, "%Y-%m-%d").date())
        except ValueError:
            raise argparse.ArgumentTypeError("Please write dates in the preferred format (YYYY-MM-DD)")
    
    cli_parser = argparse.ArgumentParser(
        description="script to process pages created in the Draft namespace between the given dates"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    # Testing option
    cli_parser.add_argument('-t', '--run_test', action='store_true',
                            help='run some tests')

    cli_parser.add_argument('start_date', type=valid_date,
                            help='start date for gathering data (format: YYYY-MM-DD)')

    cli_parser.add_argument('end_date', type=valid_date,
                            help='end date for gathering data (format: YYYY-MM-DD)')

    cli_parser.add_argument('draft_model_file', type=str,
                            help='path to the ORES draftquality model file')

    cli_parser.add_argument('wp10_model_file', type=str,
                            help='path to the ORES wp10 model file')

    cli_parser.add_argument('draft_dataset_file', type=str,
                            help='path to the bzip2-compressed TSV with Draft namespace page creations')
    
    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    if args.run_test:
        run_test(args.start_date, args.end_date,
                 args.draft_model_file, args.wp10_model_file,
                 args.draft_dataset_file)
        return()

    predictor = QualityPredictor()
    predictor.load_models(args.draft_model_file,
                          args.wp10_model_file)
    predictor.load_drafts(args.draft_dataset_file)
    predictor.process_historic(args.start_date, args.end_date)
    
    return()
            
if __name__ == "__main__":
    main()
    

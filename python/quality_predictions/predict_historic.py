#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to go through historic article creations, predict their draft and
Wikipedia 1.0 quality through ORES, then store that in a database.

Copyright (c) 2017 Morten Wang

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
import signal
import logging
import datetime as dt

from itertools import islice, chain
from collections import namedtuple

import db

import mwapi
import mwapi.cli
import mwapi.errors

import ores.api
import wikiclass
from revscoring import Model

## Named tuple to contain the lists of revisions we will get back,
## split by source.
RevisionLists = namedtuple('RevisionLists', ['live', 'deleted'])

## Named tuple to contain revision ID and timestamp
Revision = namedtuple('Revision', ['id', 'timestamp'])

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

## grab article creations
## figure out if the article exists or is deleted,
## push it to an appropriate queue
## process the queues efficiently to get revision data
## score the content and update the database.

## Nonono, it's really easy, see the usage example from wikiclass:
## https://github.com/wiki-ai/wikiclass/blob/master/README.md
## We'll just need to load two models and use them.

class HistoryPredictor:
    def __init__(self):
        '''
        Instantiate the history predictor.
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

        self.ores_api_session = None
        self.ores_url = "https://ores.wikimedia.org"
        self.ores_context = "enwiki"
        self.ores_models = ["draftquality", "wp10"]

    def load_models(self, draft_model_file, wp10_model_file):
        '''
        Load in the ORES models.
        '''

        self.draft_model = Model.load(open(draft_model_file, 'rb'))
        self.wp10_model = Model.load(open(wp10_model_file, 'rb'))

        ## Then use wikiclass.score(model, text) to score it

    def get_revisions(self, start_date, end_date):
        '''
        Get a list of all revisions that created an article from the given
        start date up to, but not including, the given end date. Returns
        a named tuple `RevisionLists` with lists of revisions based on whether
        the page has been deleted or not.

        :param start_date: start date to get revisions from
        :type start_date: datetime.date

        :param end_date: end date to get revisions up to
        :type end_date: datetime.date
        '''

        ## Query to get all revisions between the given dates from our
        ## database table with article creation events
        rev_query = '''SELECT ac_rev_id
                       FROM nettrom_articlecreations
                       WHERE ac_timestamp >= %(start_timestamp)s
                       AND ac_timestamp < %(end_timestamp)s'''
        
        ## Query to get a list of all available revisions in a given list
        ## of revision IDs
        source_query = '''SELECT rev_id, rev_timestamp, source
                         FROM 
                        ((SELECT rev_id, rev_timestamp, 'revision' AS source
                          FROM enwiki.revision
                          WHERE rev_id IN ({id_list})
                         )
                         UNION
                         (SELECT ar_rev_id AS rev_id,
                                 ar_timestamp as rev_timestamp,
                                 'archive' AS source
                          FROM enwiki.archive
                          WHERE ar_rev_id IN ({id_list})
                         )) AS revisions'''
        
        archived_revs = []
        live_revs = []

        ## Turn the dates into timestamps
        start_datetime = dt.datetime.combine(start_date, dt.time(0, 0, 0))
        end_datetime = dt.datetime.combine(end_date, dt.time(0, 0, 0))

        ## Get the revisions
        all_revisions = []
        with db.cursor(self.db_conn, 'dict') as db_cursor:
            db_cursor.execute(
                rev_query,
                {'start_timestamp': start_datetime.strftime('%Y%m%d%H%M%S'),
                 'end_timestamp': end_datetime.strftime('%Y%m%d%H%M%S')})
            for row in db_cursor:
                all_revisions.append(row['ac_rev_id'])

        i = 0
        while i < len(all_revisions):
            subset = all_revisions[i : i + self.batch_size]
            
            with db.cursor(self.db_conn, 'dict') as db_cursor:
                db_cursor.execute(
                    source_query.format(
                        id_list=','.join([str(r) for r in subset])))
                for row in db_cursor:
                    rev_timestamp = dt.datetime.strptime(
                        row['rev_timestamp'].decode('utf-8'), '%Y%m%d%H%M%S')
                    if row['source'] == 'revision':
                        live_revs.append(Revision(row['rev_id'],
                                                  rev_timestamp))
                    else:
                        archived_revs.append(Revision(row['rev_id'],
                                                      rev_timestamp))

            ## increment and iterate
            i += self.batch_size
        
        return(RevisionLists(live_revs, archived_revs))

    def start_api_session(self):
        self.api_session = mwapi.Session(self.api_url,
                                         formatversion=2,
                                         user_agent=self.user_agent)
        mwapi.cli.do_login(self.api_session, self.api_url)

    def start_ores_session(self):
        self.ores_api_session = ores.api.Session(self.ores_url,
                                                 self.user_agent,
                                                 parallel_requests=2)

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

    def insert_results(self, db_cursor,
                       rev_id, rev_timestamp, draft_res, wp10_res):
        ## Query to insert predictions into the database
        insert_query = '''INSERT INTO nettrom_articlecreation_predictions
                          VALUES (%s, %s,
                                  %s, %s, %s, %s, %s,
                                  %s, %s, %s, %s, %s, %s, %s)'''
        
        try:
            db_cursor.execute(
                insert_query, (rev_id,
                               rev_timestamp,
                               draft_res['prediction'],
                               draft_res['probability']['spam'],
                               draft_res['probability']['vandalism'],
                               draft_res['probability']['attack'],
                               draft_res['probability']['OK'],
                               wp10_res['prediction'],
                               wp10_res['probability']['Stub'],
                               wp10_res['probability']['Start'],
                               wp10_res['probability']['C'],
                               wp10_res['probability']['B'],
                               wp10_res['probability']['GA'],
                               wp10_res['probability']['FA']))
        except Exception as e:
            print('Exception: {}'.format(e))
    
    def process_queues(self, revlists):
        '''
        Process the lists of revisions by grabbing revision content through
        the appropriate API calls, getting predictions based on the content,
        then updating the database with those predictions.

        :param revlists: RevisionLists named tuple with lists of archived
                         and live revisions that we will process.
        :type revlists: namedtuple
        '''

        ## Empty prediction result for draftquality, inserted if we didn't
        ## get a prediciton back:
        draft_res_dummy = {'prediction': '',
                           'probability': {'spam': 0.0,
                                           'vandalism': 0.0,
                                           'attack': 0.0,
                                           'OK': 0.0}}

        ## Empty prediction result for wp10
        wp10_res_dummy = {'prediction': '',
                          'probability': {'FA': 0.0,
                                          'Start': 0.0,
                                          'B': 0.0,
                                          'Stub': 0.0,
                                          'C': 0.0,
                                          'GA': 0.0}}
        
        if not self.api_session:
            self.start_api_session()

        if not self.ores_api_session:
            self.start_ores_session()

        i = 0
        with db.cursor(self.db_conn) as db_cursor:        
            for revision in self.get_rev_content(revlists.deleted,
                                                 deleted=True):
                rev_id = revision['revid']
                rev_timestamp = dt.datetime.strptime(revision['timestamp'],
                                                     "%Y-%m-%dT%H:%M:%SZ")
                content = revision['content']

                draft_results = wikiclass.score(self.draft_model, content)
                wp10_results = wikiclass.score(self.wp10_model, content)

                self.insert_results(db_cursor, rev_id, rev_timestamp,
                                    draft_results, wp10_results)

                i += 1
                if i % 1000 == 0:
                    logging.info('inserted {} data predictions'.format(i))
                    self.db_conn.commit()

            ## It's more efficient to use ORES for the live revisions.
            for revision, revision_pred in zip(
                    revlists.live, self.ores_api_session.score(
                        self.ores_context, self.ores_models,
                        [r.id for r in revlists.live])):
                if not 'score' in revision_pred['draftquality'] \
                   and not 'score' in revision_pred['wp10']:
                    ## No data available, skip this revision
                    continue
                elif not 'score' in revision_pred['draftquality']:
                    revision_pred['draftquality']['score'] = draft_res_dummy
                elif not 'score' in revision_pred['wp10']:
                    revision_pred['wp10']['score'] = wp10_res_dummy
                
                self.insert_results(db_cursor, revision.id, revision.timestamp,
                                    revision_pred['draftquality']['score'],
                                    revision_pred['wp10']['score'])
                i += 1
                if i % 1000 == 0:
                    logging.info('inserted {} data predictions'.format(i))
                    self.db_conn.commit()
        
        logging.info('done inserting predictions, {} in total'.format(i))
        self.db_conn.commit()
                    
        # ok, done
        return()

    def process_historic(self, start_date, end_date, step=1):
        '''
        Process article creations from the start date up to, but not including,
        the end date, processing a given number of days at a time.

        NOTE: Assumes that the prediction models are loaded into memory.

        :param start_date: date of the first day to predict for
        :type start_date: datetime.date

        :param end_date: date of the last day to predict up to
        :type end_date: datetime.date

        :param step: number of days to process at a time
        :type step: int
        '''

        ## Connect to the database
        self.db_conn = db.connect(self.db_host, self.db_name,
                                  self.db_conf)
        if not self.db_conn:
            logging.warning('unable to connect to database server')
            return()

        ## Load in the modelss
        
        ## Let's try to populate this a week at a time:
        time_step = dt.timedelta(days=step)

        cur_date = start_date
        while cur_date < end_date:
            stop_date = cur_date + time_step
            if stop_date > end_date:
                stop_date = end_date

            logging.info('gathering predictions from {} to {}'.format(
                cur_date, stop_date))
            self.process_queues(self.get_revisions(cur_date, stop_date))

            cur_date += time_step

        # ok, done
        return()

def run_test(start_date, end_date, draft_model_filename, wp10_model_filename):
    '''
    Do some testing.
    '''

    predictor = HistoryPredictor()
    predictor.db_conn = db.connect(predictor.db_host, predictor.db_name,
                                   predictor.db_conf)
    predictor.load_models(draft_model_filename, wp10_model_filename)
    revisions = predictor.get_revisions(start_date, end_date)

    print('Got {} live revisions and {} archived revisions'.format(len(revisions.live), len(revisions.deleted)))
    print('First 10 live revisions: {}'.format(revisions.live[:10]))
    print('First 10 archived revisions: {}'.format(revisions.deleted[:10]))    

    print('|'.join([str(r) for r in revisions.deleted[:100]]))
    print('|'.join([str(r) for r in revisions.deleted[100:200]]))
    print('|'.join([str(r) for r in revisions.deleted[200:350]]))
    
    ## predictor.process_queues(revisions)

    return()
            
def main():
    import argparse

    def valid_date(d):
        try:
            return(dt.datetime.strptime(d, "%Y-%m-%d").date())
        except ValueError:
            raise argparse.ArgumentTypeError("Please write dates in the preferred format (YYYY-MM-DD)")
    
    cli_parser = argparse.ArgumentParser(
        description="script to pull article creation events from the event stream and store quality predictions in our database"
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
    
    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    if args.run_test:
        run_test(args.start_date, args.end_date,
                 args.draft_model_file, args.wp10_model_file)
        return()

    predictor = HistoryPredictor()
    predictor.load_models(args.draft_model_file,
                          args.wp10_model_file)
    predictor.process_historic(args.start_date, args.end_date)
    
    return()
            
if __name__ == "__main__":
    main()
    

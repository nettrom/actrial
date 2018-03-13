#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to read datasets of article creations and get predictions for initial
quality as well as quality after 30 days.

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
from collections import namedtuple

import db
import pymysql # for error handling

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

## Here's what we'll do:
## 1: read the two main creation datasets and dump them into the surviving
##    creation table.
## 2: do a left join with the prediction table to find revisions
##    that we don't have predictions for, try to get those and
##    add them
## 3: use the logging table to find out whether the articles were deleted
##    within 30 days after creation, keep articles that weren't.
## 4: identify articles where the last edit prior to the 30 day cutoff is
##    not the same as the first edit, store the revision ID of that.
## 5: grab predictions for those revisions.

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

        self.log_host = 'analytics-slave.eqiad.wmnet'
        self.log_name = 'log'
        self.log_conn = None # database connection to the log database when up
        
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

    def load_datasets(self, historic_filename, recent_filename,
                      deletion_filename):
        '''
        Read in the historic and recent datasets in the given files
        and insert article creation info into our surviving creations table.
        Then read in the dataset of deletions and update the creations table
        with information on all deleted pages.

        :param historic_filename: path to the historic dataset
        :type historic_filename: str

        :param recent_filename: path to the recent creations dataset
        :type recent_filename: str

        :param deletion_filename: path to the main namespace deletion dataset
        :type deletion_filename: str
        '''

        insert_query = '''INSERT INTO staging.nettrom_surviving_creations
                          (page_id, creation_timestamp, creation_rev_id) 
                          VALUES (%s, %s, %s)'''

        update_query = '''UPDATE staging.nettrom_surviving_creations
                          SET deletion_timestamp=%(timestamp)s
                          WHERE page_id=%(page_id)s'''

        ## We are only interested in pages created on or after 2014-07-01
        ## because we have reliable deletion timestamps for those.
        cutoff_timestamp = dt.datetime(2014,7,1,0,0,0)

        ## After July 21, 2017, we switch to the recent dataset
        recent_timestamp = dt.datetime(2017,7,21,0,0,0)

        self.connect_databases()
        
        # i = 0
        # with bz2.open(historic_filename, mode='rt') as infile:
        #     infile.readline() # skip header
        #     with db.cursor(self.db_conn) as db_cursor:
        #         for line in infile:
        #             (timestamp, page_id,
        #              rev_id, stuff) = line.strip().split('\t', 3)
        #             timestamp = dt.datetime.strptime(timestamp,
        #                                              '%Y-%m-%d %H:%M:%S.0')
        #             page_id = int(page_id)
        #             rev_id = int(rev_id)

        #             ## skip older pages
        #             if timestamp < cutoff_timestamp or \
        #                timestamp > recent_timestamp:
        #                 continue

        #             try:
        #                 db_cursor.execute(insert_query,
        #                                   (page_id, timestamp, rev_id))
                        
        #                 i += 1
        #                 if i % 1000 == 0:
        #                     logging.info('inserted {} historic creations'.format(i))
        #                     self.db_conn.commit()
        #             except pymysql.err.IntegrityError as e:
        #                 logging.warning('MySQL integrity error: {}:{}'.format(e.args[0], e.args[1]))
                            
        # # commit any outstanding inserts
        # logging.info('committing any outstanding inserts')
        # self.db_conn.commit()

        i = 0
        with bz2.open(recent_filename, mode='rt') as infile:
            infile.readline() # skip header
            with db.cursor(self.db_conn) as db_cursor:
                for line in infile:
                    (timestamp, namespace, page_id, page_title,
                     rev_id, user_id, stuff) = line.strip().split('\t', 6)
                    timestamp = dt.datetime.strptime(timestamp,
                                                     '%Y-%m-%d %H:%M:%S')
                    page_id = int(page_id)
                    rev_id = int(rev_id)

                    try:
                        db_cursor.execute(insert_query,
                                          (page_id, timestamp, rev_id))
                    except pymysql.err.IntegrityError as e:
                        logging.warning('MySQL integrity error: {}:{}'.format(e.args[0], e.args[1]))

                    i += 1
                    if i % 1000 == 0:
                        logging.info('inserted {} recent creations'.format(i))
                        self.db_conn.commit()

        # commit any outstanding inserts
        logging.info('committing any outstanding inserts')
        self.db_conn.commit()

        i = 0
        with bz2.open(deletion_filename, mode='rt') as infile:
            infile.readline() # skip header
            with db.cursor(self.db_conn) as db_cursor:
                for line in infile:
                    (page_id, timestamp) = line.strip().split('\t')
                    page_id = int(page_id)
                    timestamp = dt.datetime.strptime(timestamp, '%Y%m%d%H%M%S')

                    db_cursor.execute(update_query,
                                      {'page_id': page_id,
                                       'timestamp': timestamp})

                    i += 1
                    if i % 1000 == 0:
                        logging.info('updated deletion timestamp of {} pages'.format(i))
                        self.db_conn.commit()

        # commit any outstanding updates
        logging.info('committing any outstanding updates')
        self.db_conn.commit()               

        # ok, done
        return()
        
    def get_revisions(self, start_date, end_date):
        '''
        Get a list of all revisions that created an article from the given
        start date and up to, but not including, the given end date, and for
        which we do not already have a prediction. Add to the list the most
        recent revision 30 days after creation for articles that were not
        deleted within 30 days.

        :param start_date: start date to get revisions from
        :type start_date: datetime.date

        :param end_date: end date to get revisions up to
        :type end_date: datetime.date
        '''

        ## Query to get revision IDs of all article creations surviving for
        ## at least 30 days from our surviving creations table:
        creating_rev_query = '''
            SELECT page_id, creation_rev_id
            FROM staging.nettrom_surviving_creations
            WHERE creation_timestamp >= %(start_timestamp)s
            AND creation_timestamp < %(end_timestamp)s
            AND (deletion_timestamp IS NULL
                 OR TIMESTAMPDIFF(SECOND,
                                  deletion_timestamp,
                                  creation_timestamp) > 60*60*24*30)'''

        ## Query to identify all revision IDs in a list of revisions
        ## for which we already have a prediction in our table.
        prediction_query = '''SELECT rev_id
                              FROM staging.nettrom_articlecreation_predictions
                              WHERE rev_id IN ({id_list})'''
        
        ## Query to find the most recent revision of a given page
        ## that is within 30 days of its creation.
        recent_rev_query = '''
                        SELECT rev_id, rev_timestamp
                        FROM 
                        ((SELECT rev_id, rev_timestamp
                          FROM enwiki.revision r
                          JOIN staging.nettrom_surviving_creations c
                          ON r.rev_page=c.page_id
                          WHERE c.page_id = {page_id}
                          AND rev_timestamp > DATE_FORMAT(c.creation_timestamp,
                                                          "%Y%m%d%H%i%S")
                          AND rev_timestamp < DATE_FORMAT(
                                                DATE_ADD(
                                                  c.creation_timestamp,
                                                  INTERVAL 30 DAY),
                                                "%Y%m%d%H%i%S")
                         )
                         UNION
                         (SELECT ar_rev_id AS rev_id,
                                 ar_timestamp AS rev_timestamp
                          FROM enwiki.archive ar
                          JOIN staging.nettrom_surviving_creations c
                          ON ar.ar_page_id=c.page_id
                          WHERE c.page_id = {page_id}
                          AND ar_timestamp > DATE_FORMAT(c.creation_timestamp,
                                                         "%Y%m%d%H%i%S")
                          AND ar_timestamp < DATE_FORMAT(
                                               DATE_ADD(
                                                  c.creation_timestamp,
                                                 INTERVAL 30 DAY),
                                               "%Y%m%d%H%i%S")
                         )) AS page_edits
                        ORDER BY rev_timestamp DESC
                        LIMIT 1'''
        
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

        ## Query to update the surviving article table with the revision ID
        ## of an article at the 30-day threshold
        update_query = '''UPDATE nettrom_surviving_creations
                          SET 30day_rev_id=%(rev_id)s
                          WHERE page_id=%(page_id)s'''
        
        archived_revs = []
        live_revs = []

        ## Turn the dates into timestamps
        start_datetime = dt.datetime.combine(start_date, dt.time(0, 0, 0))
        end_datetime = dt.datetime.combine(end_date, dt.time(0, 0, 0))

        surviving_pages = []
        revs_needing_preds = set()

        ## Mapping page ID to 
        revs_at_30 = {}
        
        with db.cursor(self.db_conn, 'dict') as db_cursor:
            ## 1: find all pages between start date and end date that survived
            db_cursor.execute(
                creating_rev_query,
                {'start_timestamp': start_datetime.strftime('%Y%m%d%H%M%S'),
                 'end_timestamp': end_datetime.strftime('%Y%m%d%H%M%S')})
            for row in db_cursor:
                surviving_pages.append(row['page_id'])
                revs_needing_preds.add(row['creation_rev_id'])

            logging.info('found {} pages'.format(len(surviving_pages)))
                
            ## 2: identify all creations we don't have predictions for
            ## FIXME: commented out for now
            # db_cursor.execute(
            #     prediction_query.format(
            #         id_list=",".join([str(pid) for pid in revs_needing_preds])))
            # for row in db_cursor:
            #     revs_needing_preds.remove(row['rev_id'])

            ## 3: find the most recent revision at the 30 day cutoff for
            ##    all pages, add those to the set
            for page_id in surviving_pages:
                db_cursor.execute(recent_rev_query.format(page_id=page_id))
                for row in db_cursor:
                    revs_needing_preds.add(row['rev_id'])
                    revs_at_30[page_id] = row['rev_id']

        ## 3.1: Update the table with revision ID at 30 days:
        with db.cursor(self.db_conn) as db_cursor:
            for (page_id, rev_id) in revs_at_30.items():
                db_cursor.execute(update_query,
                                  {'page_id': page_id,
                                   'rev_id': rev_id})
            self.db_conn.commit()
                    
        ## 4: make a list of revisions and whether they're live or deleted
        ##    and return those
        all_revisions = list(revs_needing_preds)

        ## FIXME: Commented out to save time for now
        # i = 0
        # while i < len(all_revisions):
        #     subset = all_revisions[i : i + self.batch_size]
            
        #     with db.cursor(self.db_conn, 'dict') as db_cursor:
        #         db_cursor.execute(
        #             source_query.format(
        #                 id_list=','.join([str(r) for r in subset])))
        #         for row in db_cursor:
        #             rev_timestamp = dt.datetime.strptime(
        #                 row['rev_timestamp'].decode('utf-8'), '%Y%m%d%H%M%S')
        #             if row['source'] == 'revision':
        #                 live_revs.append(Revision(row['rev_id'],
        #                                           rev_timestamp))
        #             else:
        #                 archived_revs.append(Revision(row['rev_id'],
        #                                               rev_timestamp))

        #     ## increment and iterate
        #     i += self.batch_size
        
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

    def connect_databases(self):
        ## Connect to the database
        self.db_conn = db.connect(self.db_host, self.db_name,
                                  self.db_conf)
        self.log_conn = db.connect(self.log_host, self.log_name,
                                   self.db_conf)
    
    
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

        if not self.db_conn:
            self.connect_databases()
        if not self.db_conn or not self.log_conn:
            logging.warning('unable to connect to database server')
            return()
        
        ## Let's try to populate this step days at a time:
        time_step = dt.timedelta(days=step)

        cur_date = start_date
        while cur_date < end_date:
            stop_date = cur_date + time_step
            if stop_date > end_date:
                stop_date = end_date

            logging.info('gathering predictions from {} to {}'.format(
                cur_date, stop_date))
            ## FIXME: commented out the process_queue call because we're
            ## just using get_revisions to update the article table.
            self.get_revisions(cur_date, stop_date)
            ## self.process_queues(self.get_revisions(cur_date, stop_date))

            cur_date += time_step

        # ok, done
        return()

def run_test(start_date, end_date, draft_model_filename, wp10_model_filename):
    '''
    Do some testing.
    '''

    predictor = QualityPredictor()
    predictor.db_conn = db.connect(predictor.db_host, predictor.db_name,
                                   predictor.db_conf)
    predictor.log_conn = db.connect(predictor.log_host, predictor.log_name,
                                    predictor.db_conf)
    predictor.load_models(draft_model_filename, wp10_model_filename)
    revisions = predictor.get_revisions(start_date, end_date)

    print('Got {} live revisions and {} archived revisions'.format(len(revisions.live), len(revisions.deleted)))
    print('First 10 live revisions: {}'.format(revisions.live[:10]))
    print('First 10 archived revisions: {}'.format(revisions.deleted[:10]))    

    # print('|'.join([str(r) for r in revisions.deleted[:100]]))
    # print('|'.join([str(r) for r in revisions.deleted[100:200]]))
    # print('|'.join([str(r) for r in revisions.deleted[200:350]]))
    
    ## Test that getting a deleted revision works:
    # predictor.start_api_session()
    # rev_content = predictor.get_rev_content(revisions.deleted, deleted=True)
    # print("revision content:")
    # for rev in rev_content:
    #     print(rev)
    ## ok, that works, commenting out for now...

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

    # Option to load the datasets
    cli_parser.add_argument('-l', '--load_datasets', type=str,
                            help='load in the given datasets (supplied as a commpa-separated list)')
    
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

    predictor = QualityPredictor()
    
    if args.load_datasets:
        (historic_dataset, recent_dataset,
         deletion_dataset) = args.load_datasets.split(',')
        predictor.load_datasets(historic_dataset, recent_dataset,
                                deletion_dataset)

    predictor.load_models(args.draft_model_file,
                          args.wp10_model_file)
    predictor.process_historic(args.start_date, args.end_date)
    
    return()
            
if __name__ == "__main__":
    main()
    

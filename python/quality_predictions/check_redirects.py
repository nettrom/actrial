#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to check which of our articles that survived for 30 days were turned
into a redirect at the 30-day mark.

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

class RedirectChecker:
    def __init__(self):
        '''
        Instantiate the redirect checker.
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
        
    def get_revisions(self, start_date, end_date):
        '''
        Get a list of all revisions at the 30-day mark for articles that
        survived for at least 30 days. The list is split into archived and
        live revisions so we know how to ask the API to retrieve the content.

        :param start_date: start date to get revisions from
        :type start_date: datetime.date

        :param end_date: end date to get revisions up to
        :type end_date: datetime.date
        '''

        ## Query to get revision IDs of all article creations surviving for
        ## at least 30 days from our surviving creations table:
        rev30_query = '''
            SELECT page_id, 30day_rev_id
            FROM staging.nettrom_surviving_creations
            WHERE creation_timestamp >= %(start_timestamp)s
            AND creation_timestamp < %(end_timestamp)s
            AND 30day_rev_id IS NOT NULL'''
        
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

        all_revisions = []

        with db.cursor(self.db_conn, 'dict') as db_cursor:
            ## 1: find the revision ID of all pages we need to check
            db_cursor.execute(
                rev30_query,
                {'start_timestamp': start_datetime.strftime('%Y%m%d%H%M%S'),
                 'end_timestamp': end_datetime.strftime('%Y%m%d%H%M%S')})
            for row in db_cursor:
                all_revisions.append(row['30day_rev_id'])

            logging.info('found {} revisions'.format(len(all_revisions)))
                
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

    def process_queues(self, revlists):
        '''
        Process the lists of revisions by grabbing revision content through
        the appropriate API calls and checking if the revision is a redirect.

        :param revlists: RevisionLists named tuple with lists of archived
                         and live revisions that we will process.
        :type revlists: namedtuple
        '''

        ## Query to update the surviving articles table to set the flag
        ## of a set of revisions that are redirects
        update_query = '''UPDATE staging.nettrom_surviving_creations
                          SET redirect_at_30 = 1
                          WHERE 30day_rev_id IN ({rev_list})'''

        redirect_re = re.compile('#REDIRECT\s+\[\[', re.I)
        
        if not self.api_session:
            self.start_api_session()

        i = 0
        redirect_revisions = []
        with db.cursor(self.db_conn) as db_cursor:        
            for revision in self.get_rev_content(revlists.deleted,
                                                 deleted=True):
                rev_id = revision['revid']
                content = revision['content'].strip()

                if redirect_re.match(content):
                    redirect_revisions.append(rev_id)
                    i += 1
                    
                if redirect_revisions and \
                   len(redirect_revisions) % self.batch_size == 0:
                    db_cursor.execute(
                        update_query.format(
                            rev_list = ','.join(
                                [str(r) for r in redirect_revisions])))
                    self.db_conn.commit()
                    logging.info('inserted {} updates'.format(i))
                    redirect_revisions = []

            for revision in self.get_rev_content(revlists.live,
                                                 deleted=False):
                rev_id = revision['revid']
                content = revision['content'].strip()

                if redirect_re.match(content):
                    redirect_revisions.append(rev_id)
                    i += 1
                    
                if redirect_revisions and \
                   len(redirect_revisions) % self.batch_size == 0:
                    db_cursor.execute(
                        update_query.format(
                            rev_list = ','.join(
                                [str(r) for r in redirect_revisions])))
                    self.db_conn.commit()
                    logging.info('inserted {} updates'.format(i))
                    redirect_revisions = []

        ## update and commit any outstanding redirects
        if redirect_revisions:
            with db.cursor(self.db_conn) as db_cursor:
                db_cursor.execute(
                    update_query.format(
                        rev_list = ','.join(
                            [str(r) for r in redirect_revisions])))
            self.db_conn.commit()
        logging.info('done updating redirects, {} in total'.format(i))
                    
        # ok, done
        return()

    def connect_databases(self):
        ## Connect to the database
        self.db_conn = db.connect(self.db_host, self.db_name,
                                  self.db_conf)
    
    def process_historic(self, start_date, end_date, step=1):
        '''
        Process article creations from the start date up to, but not including,
        the end date, processing a given number of days at a time.

        :param start_date: date of the first day to predict for
        :type start_date: datetime.date

        :param end_date: date of the last day to predict up to
        :type end_date: datetime.date

        :param step: number of days to process at a time
        :type step: int
        '''

        if not self.db_conn:
            self.connect_databases()
        if not self.db_conn:
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
            self.process_queues(self.get_revisions(cur_date, stop_date))

            cur_date += time_step

        # ok, done
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

    cli_parser.add_argument('start_date', type=valid_date,
                            help='start date for gathering data (format: YYYY-MM-DD)')

    cli_parser.add_argument('end_date', type=valid_date,
                            help='end date for gathering data (format: YYYY-MM-DD)')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    checker = RedirectChecker()
    
    checker.process_historic(args.start_date, args.end_date)
    
    return()
            
if __name__ == "__main__":
    main()
    

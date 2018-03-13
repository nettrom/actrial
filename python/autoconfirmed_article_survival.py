#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script that goes through our datasets of non-autopatrolled article creations,
identifies those made by autoconfirmed users and figures out if the article
was deleted. We'll then use that information to answer our hypothesis about
whether the survival rates of articles created by autoconfirmed users has
changed during ACTRIAL.

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

import db

class SurvivalAnalyzer:
    def __init__(self):
        ## Database setup
        self.db_host = 'analytics-store.eqiad.wmnet'
        self.db_name = 'staging'
        self.db_conf = '~/.my.research.cnf'
        self.db_conn = None # database connection when up

        self.log_host = 'analytics-slave.eqiad.wmnet'
        self.log_name = 'log'
        self.log_conn = None # database connection to the log database when up

    def db_connect(self):
        '''
        Connect to the databases.
        '''

        self.db_conn = db.connect(self.db_host, self.db_name, self.db_conf)
        self.log_conn = db.connect(self.log_host, self.log_name, self.db_conf)
        
    def insert_creations(self, dataset_filename):
        '''
        Go through the given dataset of article creations and insert any
        done by an autoconfirmed user into a table. Connect to the log database
        and make a similar query for creations from it and insert those.
        '''

        ## Query to find non-autopatrolled creations by autoconfirmed users
        creation_query = '''SELECT page_id, rev_timestamp,
                                   performer_user_id, performer_user_edit_count,
                                   IFNULL(
                                       TIMESTAMPDIFF(SECOND,
                                                 performer_user_registration_dt,
                                                 rev_timestamp), 0)
                                   AS performer_account_age
                            FROM mediawiki_page_create_2
                            WHERE `database` = "enwiki"
                            AND page_namespace = 0
                            AND page_is_redirect = 0
                            AND NOT (performer_user_groups
                                     REGEXP "sysop|bot|autoreviewer")
                            AND performer_user_groups REGEXP "confirmed"
                            AND rev_timestamp >= "2017-07-21 00:00:00"
                            AND rev_timestamp < "2018-01-01 00:00:00"'''

        insert_query = '''INSERT INTO nettrom_autoconfirmed_creations
                          VALUES (%s, %s, %s, %s, %s, NULL)'''

        ## After July 21, 2017, we use the log table
        cutoff_time = dt.datetime(2017,7,21,0,0,0)

        ## Process the dataset exported from the Data Lake
        # i = 0
        # with db.cursor(self.db_conn) as db_cursor:
        #     with bz2.open(dataset_filename, mode='rt') as infile:
        #         infile.readline() # skip header
        #         for line in infile:
        #             (event_timestamp, page_id, user_id,
        #              edit_count, user_age) = line.strip().split('\t')
        #             event_timestamp = dt.datetime.strptime(event_timestamp,
        #                                                    "%Y-%m-%d %H:%M:%S.0")
        #             ## skip if too recent
        #             if event_timestamp >= cutoff_time:
        #                 continue
                
        #             page_id = int(page_id)
        #             user_id = int(user_id)
        #             edit_count = int(edit_count)
        #             user_age = int(user_age)
                    
        #             db_cursor.execute(insert_query,
        #                               (event_timestamp, page_id, user_id,
        #                                edit_count, user_age))

        #             i += 1
        #             if i % 1000 == 0:
        #                 logging.info('inserted {} rows, committing'.format(i))
        #                 self.db_conn.commit()

        # ## commit any outstanding inserts
        # self.db_conn.commit()
        # logging.info('committed {} historical rows to the database')

        ## Process any creations from the log database.
        i = 0
        with db.cursor(self.log_conn, 'dict') as log_cursor:
            with db.cursor(self.db_conn) as db_cursor:
                log_cursor.execute(creation_query)
                for row in log_cursor:
                    db_cursor.execute(insert_query,
                                      (row['rev_timestamp'],
                                       row['page_id'],
                                       row['performer_user_id'],
                                       row['performer_user_edit_count'],
                                       row['performer_account_age']))

                    i += 1
                    if i % 1000 == 0:
                        logging.info('inserted {} rows'.format(i))
                        self.db_conn.commit()

        ## commit any outstanding inserts
        self.db_conn.commit()
        logging.info('inserted {} log-table rows to the database'.format(i))

        # ok, done
        return()

    def process_creations(self, start_date, end_date):
        '''
        Get deletions for pages created between `start_date` and `end_date`
        '''

        create_temp_query = '''CREATE TEMPORARY TABLE
                               nettrom_autoconfirmed_deletions (
                               event_page_id INT UNSIGNED NOT NULL,
                               event_deletion_time DATETIME)'''

        ## Page ID and deletion time. This query assumes a page only gets
        ## deleted once. We need to check if that holds.
        insert_temp_query = '''
            INSERT INTO nettrom_autoconfirmed_deletions
            SELECT log_page,
                   STR_TO_DATE(log_timestamp, "%Y%m%d%H%i%S") AS log_time
            FROM nettrom_autoconfirmed_creations ac
            STRAIGHT_JOIN enwiki.logging l
            ON event_page_id=log_page
            WHERE log_type='delete'
            AND log_action='delete'
            AND log_timestamp > DATE_FORMAT(event_timestamp, "%Y%m%d%H%i%S")
            AND DATE(event_timestamp) >= "{start_date}"
            AND DATE(event_timestamp) < "{last_date}"'''

        ## This query assumes that page_id is a unique identifier. In other
        ## words, no page creation ever reuses a page ID. In our dataset,
        ## this assumption holds.
        update_query = '''UPDATE nettrom_autoconfirmed_creations c
                          JOIN nettrom_autoconfirmed_deletions d
                          USING (event_page_id)
                          SET c.event_deletion_time = d.event_deletion_time'''

        delete_temp_query = '''DELETE FROM nettrom_autoconfirmed_deletions'''

        with db.cursor(self.db_conn, 'dict') as db_cursor:
            db_cursor.execute(create_temp_query)
            logging.info('created temporary table')
            self.db_conn.commit()

        cur_date = start_date
        time_step = dt.timedelta(days=1)
        ## Note: <= because we also need to process _on_ the last date
        while cur_date <= end_date:
            with db.cursor(self.db_conn) as db_cursor:
                ## insert deletions
                db_cursor.execute(insert_temp_query.format(
                    start_date=cur_date, last_date=cur_date + time_step))
            
                ## update database
                db_cursor.execute(update_query)
                
                ## commit
                self.db_conn.commit()

                db_cursor.execute(delete_temp_query)

            logging.info('updated deletions from {} to {}'.format(
                cur_date, cur_date + time_step))

            ## ok, iterate
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
        description="script to process datasets of article creations by autoconfirmed users to determine if the article was deleted"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    #cli_parser.add_argument('dataset_file', type=str,
    #                        help='path to the dataset with historic creations')

    cli_parser.add_argument('start_date', type=valid_date,
                            help='start date for gathering data (format: YYYY-MM-DD)')

    cli_parser.add_argument('end_date', type=valid_date,
                            help='end date for gathering data (format: YYYY-MM-DD)')

    
    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    analyzer = SurvivalAnalyzer()
    analyzer.db_connect()
    ## analyzer.insert_creations(args.dataset_file)

    ## no need to keep this connection alive
    db.disconnect(analyzer.log_conn)
    
    analyzer.process_creations(args.start_date, args.end_date)

    # ok, done
    return()
            
if __name__ == "__main__":
    main()
    

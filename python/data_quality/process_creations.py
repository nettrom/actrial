#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to process datasets of page creations from the Data Lake, drop them
into a staging table, and use data from the revision and logging tables
in order to determine what their deletion timestamp refers to.

Copyright (c) 2018 Morten Wang

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

import logging
import datetime as dt

import db

class Processor:
    def __init__(self):
        ## Database setup
        self.db_host = 'analytics-store.eqiad.wmnet'
        self.db_name = 'staging'
        self.db_conf = '~/.my.research.cnf'
        self.db_conn = None # database connection when up

        self.creation_tables = {
            'page': 'nettrom_creations_from_page',
            'revision': 'nettrom_creations_from_revision'
        }

        self.source_tables = {
            'page': 'nettrom_creations_from_page_sources',
            'revision': 'nettrom_creations_from_revision_sources'
        }
        
    def process_datasets(self, page_creation_file, revision_creation_file):
        '''
        For each of the two datasets, import the files and process them as we
        go along.
        '''

        ## Query to get a revision ID and timestamp given a page
        ## and a given timestamp
        revision_query = '''SELECT rev_id, rev_timestamp
                            FROM
                            ((SELECT rev_id, rev_timestamp
                              FROM enwiki.revision
                              WHERE rev_page = %(page_id)s
                                    AND rev_timestamp = %(timestamp)s
                             )
                             UNION
                             (SELECT ar_rev_id AS rev_id,
                                     ar_timestamp as rev_timestamp
                              FROM enwiki.archive
                              WHERE ar_page_id = %(page_id)s
                                    AND ar_timestamp = %(timestamp)s
                             )
                            ) AS revisions'''

        ## Query to get the type and action of a log event based on
        ## the page and the timestamp
        logging_query = '''SELECT log_timestamp, log_type, log_action
                           FROM enwiki.logging
                           WHERE log_page = %(page_id)s
                           AND log_timestamp = %(timestamp)s'''

        ## Query to store a page in the creation table
        insert_creation_query = '''INSERT INTO {creation_table}
                                   VALUES (%s, %s, %s)'''

        ## Query to store a page in the source table
        insert_source_query = '''INSERT INTO {source_table}
                                 VALUES (%s, %s, %s, %s, %s)'''
        
        ## Connect to the database
        self.db_conn = db.connect(self.db_host, self.db_name,
                                  self.db_conf)
        if not self.db_conn:
            logging.warning('unable to connect to database server')
            return()

        datasets = {
            'page': page_creation_file,
            'revision': revision_creation_file
        }
        
        for data_source in ['page', 'revision']:
            with open(datasets[data_source]) as infile:
                infile.readline() # skip header
                n_pages = 0
                for line in infile:
                    (page_id,
                     creation_timestamp,
                     deletion_timestamp) = line.strip().split('\t')
                    page_id = int(page_id)
                    creation_timestamp = dt.datetime.strptime(
                        creation_timestamp, '%Y-%m-%d %H:%M:%S.0')
                    creation_timestamp = creation_timestamp.strftime('%Y%m%d%H%M%S')
                    deletion_timestamp = dt.datetime.strptime(
                        deletion_timestamp, '%Y-%m-%d %H:%M:%S.0')
                    deletion_timestamp = deletion_timestamp.strftime('%Y%m%d%H%M%S')
                    
                    rev_timestamp = None
                    log_timestamp = None
                    log_type= None
                    log_action = None

                    with db.cursor(self.db_conn, 'dict') as db_cursor:
                        db_cursor.execute(
                            revision_query, {'page_id': page_id,
                                             'timestamp': deletion_timestamp.encode('utf-8')})
                        for row in db_cursor:
                            rev_timestamp = row['rev_timestamp'].decode('utf-8')

                        db_cursor.execute(
                            logging_query, {'page_id': page_id,
                                            'timestamp': deletion_timestamp.encode('utf-8')})
                        n_rows = 0
                        for row in db_cursor:
                            n_rows += 1
                            log_timestamp = row['log_timestamp'].decode('utf-8')
                            log_type = row['log_type'].decode('utf-8')
                            log_action = row['log_action'].decode('utf-8')

                        if n_rows > 1:
                            logging.warning('page {} has {} rows at timestamp {} in the logging table'.format(page_id, n_rows, deletion_timestamp))

                        db_cursor.execute(insert_creation_query.format(creation_table=self.creation_tables[data_source]), (page_id, creation_timestamp, deletion_timestamp))
                        db_cursor.execute(insert_source_query.format(source_table=self.source_tables[data_source]), (page_id, rev_timestamp, log_timestamp, log_type, log_action))

                    n_pages += 1
                    if n_pages % 100 == 0:
                        print('processed {} pages'.format(n_pages))
                        self.db_conn.commit()

        ## commit any outstanding inserts
        self.db_conn.commit()

        ## ok, done
        return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to process datasets of creations from the Data Lake and determine sources of timestamps"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    cli_parser.add_argument('page_dataset', type=str,
                            help='path to the dataset from the page table')
    cli_parser.add_argument('revision_dataset', type=str,
                            help='path to the dataset from the revision table')
    
    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    p = Processor()
    p.process_datasets(args.page_dataset, args.revision_dataset)
        
    return()
            
if __name__ == "__main__":
    main()
    

                                             
                    

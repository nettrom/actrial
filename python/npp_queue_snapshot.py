#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to capture a snapshot of the size of the queue of pages awaiting
review and store it in our local database.

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
import logging

import db

def capture_snapshot():
    '''
    Grab a count of the size of the queue of articles in need of
    review and store it in our database.

    :param wiki_db_conn: database connection to the replicated Wikipedia database
    :type wiki_db_conn: MySQLdb.Connection

    :param local_db_conn: database connection to our local database
    :type local_db_conn: MySQLdb.Connection
    '''

    ## Query to get the number of articles awaiting review, as well
    ## as a timestamp of when it was run. This query reflects the one
    ## used in the PageTriage extension, ref:
    ## https://phabricator.wikimedia.org/diffusion/EPTR/browse/master/includes/PageTriageUtil.php

    snapshot_query = '''SELECT UTC_TIMESTAMP() AS npp_timestamp,
                        COUNT(ptrp_page_id) AS npp_count
                        FROM pagetriage_page
                        JOIN page
                        ON page_id = ptrp_page_id
                        WHERE ptrp_reviewed = 0
                        AND page_is_redirect = 0
                        AND page_namespace = 0'''

    insert_query = '''INSERT INTO npp_queue_size VALUES (%s, %s)'''

    db_conf = '~/replica.my.cnf'
    local_db = {'hostname': 'tools.labsdb',
                'dbname': 's53463__actrial_p',
                'dbconf': db_conf}
    wiki_db = {'hostname': 'enwiki.labsdb',
               'dbname': 'enwiki_p',
               'dbconf': db_conf}
    
    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    snapshot_timestamp = None
    snapshot_count = None

    with db.cursor(wiki_db_conn, 'dict') as db_cursor:
        db_cursor.execute(snapshot_query)


        for row in db_cursor:
            snapshot_timestamp = row['npp_timestamp']
            snapshot_count = row['npp_count']

    with db.cursor(local_db_conn, 'dict') as db_cursor:
        db_cursor.execute(insert_query,
                          (snapshot_timestamp, snapshot_count))
        local_db_conn.commit()
        
    # ok, done
    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to capture a snapshot of the New Page Patrol queue size and store it in our local database"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    capture_snapshot()
        
    return()
    
if __name__ == "__main__":
    main()

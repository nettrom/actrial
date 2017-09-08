#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to identify the time it took to review a given article.

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
import datetime as dt

from collections import namedtuple

import db

## Named tuple for a data point of review time.
DataPoint = namedtuple('DataPoint', ['page_id', 'creation_timestamp',
                                     'review_timestamp', 'time_to_review'])

## Batch size for database commits
batch_size = 1000

def gather_data(db_conn, start_timestamp, end_timestamp):
    '''
    Gather data about the time to review for all articles created between
    the given start and end timestamps. Assumes that the dataset of article
    creation does not include any autopatrolled creations.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_timestamp: timestamp to gather data from
    :type start_timestamp: str

    :param end_timestamp: timestamp to gather data to (but not including)
    :type end_timestamp: str
    '''

    ## Query to get time to review for articles created between a given
    ## start and end timestamp.

    ## Note: log_page might be NULL. However, out of the entire table
    ## only five patrolling-related entries are like that, and they're
    ## all in 2007, which is outside our window.

    ## Note: there's no need to look for "delete" events from the PageCuration
    ## extension, they are also logged as "reviewed"
    
    review_time_query = '''SELECT DISTINCT ac_page_id,
                                  ac_timestamp, log_timestamp,
                                  TIMESTAMPDIFF(SECOND,
                                                STR_TO_DATE(ac_timestamp,
                                                            '%Y%m%d%H%i%s'),
                                                STR_TO_DATE(log_timestamp,
                                                            '%Y%m%d%H%i%s'))
                                                AS time_to_review
                           FROM staging.nettrom_articlecreations ac
                           JOIN enwiki.logging log
                           ON ac.ac_page_id=log.log_page
                           WHERE ac.ac_timestamp >= '{start}'
                           AND ac.ac_timestamp < '{end}'
                           AND log.log_timestamp > ac.ac_timestamp
                           AND  ((log_type="patrol"
                                  AND log_params NOT LIKE '%::auto";i:1%')
                                 OR 
                                 (log_type='pagetriage-curation'
                                  AND log_action='reviewed'))'''

    ## The data points we'll return
    datapoints = []

    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute(review_time_query.format(
            start=start_timestamp, end=end_timestamp))

        for row in db_cursor:
            page_id = row['ac_page_id']
            creation_timestamp = row['ac_timestamp']
            review_timestamp = row['log_timestamp']
            time_to_review = row['time_to_review']
            
            datapoints.append(DataPoint(page_id, creation_timestamp,
                                        review_timestamp, time_to_review))
    return(datapoints)

def gather_and_update(start_date, end_date=None, step=7):
    '''
    Gather data from the start date up to, but not including, the end date,
    then update our database.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_date: First date to gather data for
    :type start_date: datetime.date

    :param end_date: Last date to gather data for
    :type end_date: datetime.date

    :param step: Number of days to gather data for in each iteration
    :type step: int
    '''

    update_query = '''UPDATE staging.nettrom_articlecreations
                      SET review_timestamp="{0.review_timestamp}",
                          time_to_review={0.time_to_review}
                      WHERE ac_page_id={0.page_id}
                      AND ac_timestamp="{0.creation_timestamp}"'''

    ## Connect to the database
    db_conn = db.connect('analytics-store.eqiad.wmnet',
                         'staging', '~/.my.research.cnf')
    
    cur_date = start_date
    delta_days = dt.timedelta(days=step)

    if not end_date:
        end_date = dt.date.today()
    
    while cur_date < end_date:
        stop_date = cur_date + delta_days
        if stop_date > end_date:
            stop_date = end_date

        logging.info('gathering data from {} to {}'.format(cur_date, stop_date))

        ## Turn dates into timestamps
        cur_date_timestamp = dt.datetime.combine(cur_date, dt.time(0, 0, 0))
        stop_date_timestamp = dt.datetime.combine(stop_date, dt.time(0, 0, 0))

        logging.info('after converting to timestamps:')
        logging.info('gathering data from {} to {}'.format(cur_date_timestamp,
                                                           stop_date_timestamp))

        datapoints = gather_data(
            db_conn, cur_date_timestamp.strftime('%Y%m%d%H%M%S'),
            stop_date_timestamp.strftime('%Y%m%d%H%M%S'))

        i = 0
        with db.cursor(db_conn, 'dict') as db_cursor:
            for datapoint in datapoints:
                db_cursor.execute(update_query.format(datapoint))

                i += 1
                if i % batch_size == 0:
                    logging.info('updated {} data points, committing!'.format(i))
                    db_conn.commit()

        ## Commit any outstanding updates
        db_conn.commit()
        logging.info('updated {} data points'.format(i))
        
        cur_date = stop_date

    ## Wrap up and return
    db_conn.close()
    return()

def run_tests():
    '''
    Run some tests.
    '''
    logging.basicConfig(level=logging.INFO)
    
    db_conn = db.connect('analytics-store.eqiad.wmnet',
                         'staging', '~/.my.research.cnf')
    
    start = dt.date(2017,1, 1)
    end = dt.date(2017,1,2)
    
    # datapoints = gather_data(db_conn, '20150908000000', '201509090000')
    datapoints = gather_historic(db_conn, start, end)
    
    print('got {} data points'.format(len(datapoints)))
    print(datapoints[1])
    print(datapoints[-1])
    
    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to gather historic measurements and populate our database tables"
    )

    def valid_date(d):
        try:
            return(dt.datetime.strptime(d, "%Y-%m-%d").date())
        except ValueError:
            raise argparse.ArgumentTypeError("Please write dates in the preferred format (YYYY-MM-DD)")
    
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

    gather_and_update(args.start_date, args.end_date)

    ## ok, done
    return()

if __name__ == "__main__":
    main()

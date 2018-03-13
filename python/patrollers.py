#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info on the number of patrol actions performed by each patroller.

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

## Named tuple for a data point of patrol activity: user ID of
## the patroller, the date it occurred, and the number of actions performed.
DataPoint = namedtuple('DataPoint', ['userid', 'date',
                                     'num_actions'])

def gather_data(db_conn, start_timestamp, end_timestamp):
    '''
    Gather data on the number of patrol actions performed by each patroller
    from the given start timestamp and up to, but not including, the given
    end timestamp. Returns a list of DataPoint tuples in no particular order.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_timestamp: timestamp to gather data from
    :type start_timestamp: str

    :param end_timestamp: timestamp to gather data to (but not including)
    :type end_timestamp: str
    '''

    activity_query = '''SELECT log_date, log_user, num_raw_logged -
                        IFNULL(num_duplicates, 0) AS num_actions
                        FROM (SELECT DATE(log_timestamp) AS log_date, log_user,
                              COUNT(*) AS num_raw_logged
                              FROM logging_logindex
                              WHERE log_namespace = 0
                              AND log_timestamp >= '{start}'
                              AND log_timestamp < '{end}'
                              AND ((log_type="patrol"
                                    AND log_params NOT LIKE '%::auto";i:1%')
                                   OR 
                                   (log_type="pagetriage-curation"
                                    AND log_action="reviewed"))
                              GROUP BY log_date, log_user) AS all_events
                        LEFT JOIN
                             (SELECT DATE(log_timestamp) AS log_date,
                                     l1.log_user,
                                     COUNT(*) AS num_duplicates
                              FROM logging_logindex l1
                              JOIN logging_logindex l2
                              USING (log_page, log_timestamp, log_namespace)
                              WHERE l1.log_namespace = 0
                              AND l1.log_timestamp >= '{start}'
                              AND l1.log_timestamp < '{end}'
                              AND l1.log_type='patrol'
                              AND l1.log_params NOT LIKE '%::auto";i:1%'
                              AND l2.log_type='pagetriage-curation'
                              AND l2.log_action='reviewed'
                              GROUP BY log_date, log_user) AS duplicate_events
                        USING (log_date, log_user)'''

    ## The data points we'll return
    datapoints = []

    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute(activity_query.format(start=start_timestamp,
                                                end=end_timestamp))

        for row in db_cursor:
            user_id = row['log_user']
            date = row['log_date']
            n_actions = row['num_actions']

            ## Ignore surpressed revisions
            if user_id is None:
                continue
            
            datapoints.append(DataPoint(user_id, date, n_actions))

    return(datapoints)

def gather_historic(db_conn, start_date, end_date=None, step=7):
    '''
    Gather historic data over a larger timespan covering a given number
    of days at a time. Returns a list of DataPoint named tuples in no
    specific order.

    :param local_db: Database connection to the local database
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection

    :param start_date: First date to gather data for
    :type start_date: datetime.date

    :param end_date: Last date to gather data for
    :type end_date: datetime.date

    :param step: Number of days to gather data for in each iteration
    :type step: int
    '''

    cur_date = start_date
    delta_days = dt.timedelta(days=step)

    datapoints = []

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

        datapoints.extend(gather_data(
            db_conn, cur_date_timestamp.strftime('%Y%m%d%H%M%S'),
            stop_date_timestamp.strftime('%Y%m%d%H%M%S')))

        cur_date = stop_date

    return(datapoints)

def main():
    '''
    Run some tests.
    '''
    logging.basicConfig(level=logging.INFO)
    
    db_conn = db.connect('enwiki.labsdb', 'enwiki_p', '~/replica.my.cnf')

    start = dt.date(2017,1, 1)
    end = dt.date(2017,1,2)
    
    # datapoints = gather_data(db_conn, '20150908000000', '201509090000')
    datapoints = gather_historic(db_conn, start, end)
    
    print('got {} data points'.format(len(datapoints)))
    print(datapoints[1])
    print(datapoints[-1])
    
    return()

if __name__ == "__main__":
    main()

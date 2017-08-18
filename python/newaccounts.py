#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with a count of the number of new accounts created on a given Wikipedia edition.

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

import logging
import datetime as dt

from collections import namedtuple

import db

## Named tuple for a data point with the number of new accounts created
## at a specific date.
DataPoint = namedtuple('DataPoint',
                       ['date', 'newusers', 'autocreate', 'byemail',
                        'create', 'create2'])

def gather_data(db_conn, start_timestamp, end_timestamp):
    '''
    Gather data from the given start timestamp up until, but not including,
    the given end timestamp. Returns a list of DataPoint named tuples ordered
    by date.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_timestamp: timestamp to gather data from
    :type start_timestamp: datetime.datetime

    :param end_timestamp: timestamp to gather data to (but not including)
    :type end_timestamp: datetime.datetime
    '''

    account_query = '''SELECT SUBSTRING(log_timestamp, 1, 8) AS date,
                              log_action, count(*) AS num_users
                       FROM logging_userindex
                       WHERE log_type='newusers'
                       AND log_timestamp >= %(start)s
                       AND log_timestamp < %(end)s
                       GROUP BY date, log_action'''

    datapoints = []
    
    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute(account_query, {'start': start_timestamp,
                                          'end': end_timestamp})

        ## Dummy data to start
        data = {'date': dt.date.today(), 'newusers': 0,
                'autocreate': 0, 'byemail': 0,
                'create': 0, 'create2': 0}

        for row in db_cursor:
            ## Some days have supressed log actions
            if not row['log_action']:
                continue
            
            date = dt.datetime.strptime(row['date'].decode('utf-8'),
                                        '%Y%m%d').date()
            log_action = row['log_action'].decode('utf-8')
            
            if date != data['date']:
                datapoints.append(DataPoint(**data))
                data = {'date': date, 'newusers': 0,
                        'autocreate': 0, 'byemail': 0,
                        'create': 0, 'create2': 0}

            data[log_action] = row['num_users']

        ## Add the last data point
        datapoints.append(DataPoint(**data))
        
    ## Return all data points except the first one, which is a dummy
    return(datapoints[1:])

def gather_historic(db_conn, start_date, end_date=None, step=90):
    '''
    Gather historic data over a larger timespan covering a given number
    of days at a time. Returns an ordered list of DataPoint named tuples
    ordered by date.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

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
    end = dt.date(2017,7,1)
    
    datapoints = gather_data(db_conn, '20050908000000', '200509090000')
    # datapoints = gather_historic(db_conn, start, end)
    print(datapoints[0])
    print(datapoints[-1])
    
    return()

if __name__ == "__main__":
    main()

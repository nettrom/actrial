#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info on accounts registered and whether the accounts were autocreated.

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

## Named tuple for a data point for a user registration, with a timestamp
## of when the registration was logged, and the type of registration it was
## (create/create2/autocreate/byemail).
DataPoint = namedtuple('DataPoint', ['userid', 'reg_timestamp',
                                     'create_type'])

## Batch size when gathering batches of data from the database
batch_size = 1000

def gather_data(db_conn, start_timestamp, end_timestamp):
    '''
    Gather data from the given start timestamp up until, but not including,
    the given end timestamp. Returns a list of DataPoint named tuples in
    no specific order.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_timestamp: timestamp to gather data from
    :type start_timestamp: datetime.datetime

    :param end_timestamp: timestamp to gather data to (but not including)
    :type end_timestamp: datetime.datetime
    '''

    ## Note: the logging timestamp might be slightly behind the actual account
    ## registration timestamp (e.g. by a second). It's a lot easier to just use
    ## the logging timestamp rather than trying to figure out if there's
    ## a discrepancy between the two, although we might be slightly off.
    accreg_query = '''SELECT log_action, log_timestamp, log_user, log_params
                      FROM logging_userindex
                      WHERE log_type='newusers'
                      AND log_timestamp >= %(start)s
                      AND log_timestamp < %(end)s'''

    ## The data we'll return
    datapoints = []

    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute(accreg_query, {'start': start_timestamp,
                                         'end': end_timestamp})

        for row in db_cursor:
            ## We might not have all data...
            if row['log_action'] is None or row['log_user'] is None \
               or row['log_params'] is None or row['log_timestamp'] is None:
                continue

            create_type = row['log_action'].decode('utf-8')
            user_id = row['log_user']
            params = row['log_params'].decode('utf-8')
            timestamp = dt.datetime.strptime(
                row['log_timestamp'].decode('utf-8'), "%Y%m%d%H%M%S")
                                             
            ## If the account was created by someone else, find the ID of the
            ## created account. Either a serialized PHP object or just a number.
            if create_type == 'create2' or create_type == 'byemail':
                match = re.search('i[:](\d+)', params)
                if not match:
                    try:
                        user_id = int(params)
                    except ValueError:
                        logging.info('failed to find ID in params: {}'.format(
                            params))
                        continue
                else:
                    user_id = int(match.group(1))
            else:
                user_id = row['log_user']

            ## If the user ID is 0 at this point, ignore the registration
            if user_id == 0:
                continue
            
            datapoints.append(DataPoint(user_id, timestamp, create_type))

    return(datapoints)

def gather_historic(db_conn, start_date, end_date=None, step=30):
    '''
    Gather historic data over a larger timespan covering a given number
    of days at a time. Returns a list of DataPoint named tuples in no
    specific order.

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
    end = dt.date(2017,1,2)
    
    # datapoints = gather_data(db_conn, '20150908000000', '201509090000')
    datapoints = gather_historic(db_conn, start, end)
    print('got {} data points'.format(len(datapoints)))
    print(datapoints[0])
    print(datapoints[-1])
    
    return()

if __name__ == "__main__":
    main()

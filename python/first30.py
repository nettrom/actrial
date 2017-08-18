#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info about the number of edits, number of namespaces, number of pages,
and whether the account reached autoconfirmed status in its first 30 days.

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

## Named tuple for a data point for a user's activity in the first 30 days:
DataPoint = namedtuple('DataPoint', ['userid', 'num_edits', 'num_namespaces',
                                     'num_pages'])

## Batch size when gathering batches of data from the database
batch_size = 100

def gather_data(db_conn, user_ids):
    '''
    Gather data about the user activity in the first 30 days for the given
    user IDs. Returns a list of data points of users who _had_ some activity,
    in no particular order.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param user_ids: list of user IDs to gather data for
    :type user_ids: list
    '''

    activity_query = '''SELECT user_id, COUNT(*) AS num_edits,
                        COUNT(DISTINCT(page_namespace)) AS num_namespaces,
                        COUNT(DISTINCT(page_id)) AS num_pages
                        FROM 
                        ((SELECT user_id, rev_id, page_namespace, page_id
                          FROM revision_userindex r
                          JOIN page p1
                          ON rev_page=page_id
                          JOIN user
                          ON rev_user=user_id
                          WHERE user_id IN ({idlist})
                          AND rev_timestamp > user_registration
                          AND rev_timestamp < DATE_FORMAT(
                                                DATE_ADD(
                                                  STR_TO_DATE(user_registration,
                                                              "%Y%m%d%H%i%S"),
                                                  INTERVAL 30 DAY),
                                                "%Y%m%d%H%i%S")
                         )
                         UNION
                         (SELECT user_id, ar_rev_id,
                                 ar_namespace AS page_namespace,
                                 ar_page_id AS page_id
                          FROM archive_userindex
                          JOIN user
                          ON ar_user=user_id
                          WHERE user_id IN ({idlist})
                          AND ar_timestamp > user_registration
                          AND ar_timestamp < DATE_FORMAT(
                                               DATE_ADD(
                                                 STR_TO_DATE(user_registration,
                                                             "%Y%m%d%H%i%S"),
                                                 INTERVAL 30 DAY),
                                               "%Y%m%d%H%i%S")
                         )) AS user_activity
                        GROUP BY user_id'''

    ## The data points we'll return
    datapoints = []

    i = 0
    while i < len(user_ids):
        subset = user_ids[i : i + batch_size]

        logging.info('gathering activity for subset [{}:{}]'.format(
            i, i + batch_size))

        with db.cursor(db_conn, 'dict') as db_cursor:
            db_cursor.execute(activity_query.format(
                idlist=','.join([str(u) for u in subset])))

            for row in db_cursor:
                datapoints.append(DataPoint(row['user_id'], row['num_edits'],
                                            row['num_namespaces'],
                                            row['num_pages']))

        i += batch_size

    return(datapoints)

def gather_historic(local_db, wiki_db, start_date, end_date=None, step=5):
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

    userid_query = '''SELECT as_userid
                      FROM account_stats
                      WHERE as_reg_timestamp >= %(start)s
                      AND as_reg_timestamp < %(end)s'''
    
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

        userids = []
        with db.cursor(local_db, 'dict') as db_cursor:
            db_cursor.execute(userid_query, {'start': cur_date,
                                             'end': stop_date})
            for row in db_cursor:
                userids.append(row['as_userid'])

        logging.info('checking activity for {} users'.format(len(userids)))
                
        datapoints.extend(gather_data(wiki_db, userids))

        cur_date = stop_date

    return(datapoints)

def main():
    '''
    Run some tests.
    '''
    logging.basicConfig(level=logging.INFO)
    
    db_conn = db.connect('enwiki.labsdb', 'enwiki_p', '~/replica.my.cnf')
    local_db_conn = db.connect('tools.labsdb', 's53463__actrial_p',
                               '~/replica.my.cnf')

    user_ids = [7990889,30975109,30975110,30975111,30975112,30975113,
                30975114,30975115,30975116,30975117,30975118,30975119]
    
    ## datapoints = gather_data(db_conn, user_ids)
    datapoints = gather_historic(local_db_conn, db_conn,
                                 dt.date(2011,1,1),
                                 dt.date(2011,2,1))
    print('got {} data points'.format(len(datapoints)))
    print(datapoints[0])
    print(datapoints[-1])
    
    return()

if __name__ == "__main__":
    main()

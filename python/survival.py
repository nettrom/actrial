#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info about the survival of newly registered users. A surviving editor
is someone who makes at least `k` edits in the first week, and at least
`k` edits in the fifth week. In our case `k` defaults to 1.

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

## Named tuple for a data point for a user's survival activity.
DataPoint = namedtuple('DataPoint', ['userid', 'num_edits_week1',
                                     'num_edits_week5'])

## Batch size when gathering batches of data from the database
batch_size = 100

def gather_data(db_conn, user_ids):
    '''
    Gather data about the user activity in the first and fifth weeks for
    the given set of user IDs. Returns a list of data points of users who
    _had_ some activity in both weeks.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param user_ids: list of user IDs to gather data for
    :type user_ids: list
    '''

    activity_query = '''SELECT user_id, COUNT(*) AS num_edits
                        FROM 
                        ((SELECT user_id, rev_id
                          FROM revision_userindex
                          JOIN user
                          ON rev_user=user_id
                          WHERE user_id IN ({idlist})
                          AND rev_timestamp > DATE_FORMAT(
                                                DATE_ADD(
                                                  STR_TO_DATE(user_registration,
                                                              "%Y%m%d%H%i%S"),
                                                  INTERVAL {start} WEEK),
                                                "%Y%m%d%H%i%S")
                          AND rev_timestamp < DATE_FORMAT(
                                                DATE_ADD(
                                                  STR_TO_DATE(user_registration,
                                                              "%Y%m%d%H%i%S"),
                                                  INTERVAL {end} WEEK),
                                                "%Y%m%d%H%i%S")
                         )
                         UNION
                         (SELECT user_id, ar_rev_id
                          FROM archive_userindex
                          JOIN user
                          ON ar_user=user_id
                          WHERE user_id IN ({idlist})
                          AND ar_timestamp > DATE_FORMAT(
                                               DATE_ADD(
                                                 STR_TO_DATE(user_registration,
                                                             "%Y%m%d%H%i%S"),
                                                 INTERVAL {start} WEEK),
                                               "%Y%m%d%H%i%S")
                          AND ar_timestamp < DATE_FORMAT(
                                               DATE_ADD(
                                                 STR_TO_DATE(user_registration,
                                                             "%Y%m%d%H%i%S"),
                                                 INTERVAL {end} WEEK),
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

        ## Maps user ID to activity in the first week
        candidate_map = {}
        
        with db.cursor(db_conn, 'dict') as db_cursor:
            ## First week
            start_week = 0
            end_week = 1
            db_cursor.execute(activity_query.format(
                idlist=','.join([str(u) for u in subset]),
                start=start_week, end=end_week))

            for row in db_cursor:
                user_id = row['user_id']
                num_edits = row['num_edits']
                
                candidate_map[user_id] = num_edits

            ## Fifth week
            start_week = 4
            end_week = 5
            db_cursor.execute(activity_query.format(
                idlist=','.join([str(u) for u in subset]),
                start=start_week, end=end_week))

            for row in db_cursor:
                user_id = row['user_id']
                num_edits = row['num_edits']
                
                if user_id in candidate_map:
                    datapoints.append(DataPoint(user_id,
                                                candidate_map[user_id],
                                                num_edits))
                    del(candidate_map[user_id])

        ## The remaining candidates didn't edit in the second period,
        ## so we add them to the datapoints with a 0 edit in that period.
        for user_id, first_week_edits in candidate_map.items():
            datapoints.append(DataPoint(user_id, first_week_edits, 0))
        
        # Move to the next subset
        i += batch_size

    return(datapoints)

def gather_historic(local_db, wiki_db, start_date, end_date=None, step=7):
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

    ## Get user IDs of users in our time span that have made at least
    ## one edit in the first 30 days. While that edit might not have
    ## happened in the first week, users who made 0 edits cannot possibly
    ## have survived, so we can just filter all of them out.
    userid_query = '''SELECT as_userid
                      FROM account_stats
                      WHERE as_reg_timestamp >= %(start)s
                      AND as_reg_timestamp < %(end)s
                      AND as_num_edits_30 > 0'''
    
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
                                 dt.date(2011,1,8))
    print('got {} data points'.format(len(datapoints)))
    print(datapoints[0])
    print(datapoints[-1])
    
    return()

if __name__ == "__main__":
    main()

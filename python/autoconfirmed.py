#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info on whether an account became autoconfirmed in the first 30 days.

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

## Named tuple for a data point for a user registration, with a 0/1
## flag for whether the account was autoconfirmed, and the timestamp
## when they were autoconfirmed (either 4 days after registration or
## the time of their 10th edit if > 4 days).
DataPoint = namedtuple('DataPoint', ['userid', 'autoconfirmed',
                                     'ac_timestamp'])

## Batch size when gathering batches of data from the database
batch_size = 100

def gather_data(db_conn, user_ids):
    '''
    Gather data about the autoconfirmed status in the account's first 30 days
    for the given user IDs. Returns a list of data points of users in no
    particular order. NOTE: This method assumes that the user made at least
    ten edits in the first 30 days.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param user_ids: list of user IDs to gather data for
    :type user_ids: list
    '''

    ## Get the user's tenth edit
    tenth_edit_query = '''SELECT user_id,
                                 STR_TO_DATE(user_registration,
                                             "%Y%m%d%H%i%S") AS reg_date,
                                 DATE_ADD(
                                    STR_TO_DATE(user_registration,
                                                "%Y%m%d%H%i%S"),
                                    INTERVAL 4 DAY) AS reg_plus_4,
                                 rev_id,
                                 STR_TO_DATE(rev_timestamp,
                                 "%Y%m%d%H%i%S") AS rev_timestamp
                         FROM 
                         ((SELECT user_id, user_registration,
                                  rev_id, rev_timestamp
                           FROM revision_userindex r
                           JOIN user
                           ON rev_user=user_id
                           WHERE user_id={user_id}
                           AND rev_timestamp > user_registration
                           AND rev_timestamp < DATE_FORMAT(
                                                 DATE_ADD(
                                                   STR_TO_DATE(user_registration,
                                                               "%Y%m%d%H%i%S"),
                                                 INTERVAL 30 DAY),
                                               "%Y%m%d%H%i%S")
                          )
                          UNION
                          (SELECT user_id, user_registration,
                                  ar_rev_id AS rev_id,
                                  ar_timestamp AS rev_timestamp
                           FROM archive_userindex
                           JOIN user
                           ON ar_user=user_id
                           WHERE user_id={user_id}
                           AND ar_timestamp > user_registration
                           AND ar_timestamp < DATE_FORMAT(
                                                DATE_ADD(
                                                  STR_TO_DATE(user_registration,
                                                              "%Y%m%d%H%i%S"),
                                                  INTERVAL 30 DAY),
                                                "%Y%m%d%H%i%S")
                          )) AS user_activity
                          ORDER BY rev_timestamp ASC
                          LIMIT 10,1'''

    ## The data points we'll return
    datapoints = []

    with db.cursor(db_conn, 'dict') as db_cursor:
        for user_id in user_ids:
            db_cursor.execute(tenth_edit_query.format(user_id=user_id))
            ac_timestamp = 0
            is_ac = 1 # ref assumption of 10 edits in 30 days above
            
            for row in db_cursor:
                ## Default is that they were autoconfirmed when they made
                ## their tenth edit:
                ac_timestamp = row['rev_timestamp']

                fourth_day_timestamp = row['reg_plus_4']

                ## If the tenth edit occurred less than four days after
                ## registration, we use the fourth day instead.
                if ac_timestamp < fourth_day_timestamp:
                    ac_timestamp = fourth_day_timestamp

            datapoints.append(DataPoint(user_id, is_ac, ac_timestamp))

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
    ## 10 edits in the first 30 days (otherwise they cannot be autoconfirmed)
    userid_query = '''SELECT as_userid
                      FROM account_stats
                      WHERE as_reg_timestamp >= %(start)s
                      AND as_reg_timestamp < %(end)s
                      AND as_num_edits_30 >= 10'''
    
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

    user_ids = [7990889,8625927,8625934,8625972,8626037,8626075,8626125,
                8626169,8626175,8626266,8626337,8626355,8626362,8626409,
                8626413,8626447,8626483,8626501,8626526,8626533,8626591,
                8626592,8626605,8626648,8626711,8626713]
    
    # datapoints = gather_data(db_conn, user_ids)
    datapoints = gather_historic(local_db_conn, db_conn,
                                 dt.date(2009,1,1),
                                dt.date(2009,2,1))
    print('got {} data points'.format(len(datapoints)))
    print(datapoints[:5])
    print(datapoints[-5:])
    
    return()

if __name__ == "__main__":
    main()

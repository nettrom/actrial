#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info about whether a newly registered account started out by creating
a new article, and if so, if the article survived for 30 days.

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

## Named tuple for a data point of whether the user created an article
## and if they did, whether the article survived for 30 days.
DataPoint = namedtuple('DataPoint', ['userid', 'created_article',
                                     'article_survived'])

## Number of accounts we'll process at a time when doing batches
batch_size = 100

def gather_data(wiki_db_conn, local_db_conn, user_ids, horizon=30):
    '''
    Gather data on whether the given user IDs started out by creating
    a new article, and if so, whether the article survived for a given
    number of days (typically 30). Returns a list of data points with
    information, in no specific order.

    :param wiki_db_conn: Database connection to the replicated Wikipedia DB
    :type wiki_db_conn: MySQLdb.Connection

    :param local_db_conn: Database connection to the local database
    :type local_db_conn: MySQLdb.Connection

    :param user_ids: list of user IDs we are interested in
    :type user_ids: list

    :param horizon: number of days an article needs to survive
    :type horizon: int
    '''

    ## Query to get the timestamp and page of a user's first
    ## edit within 30 days of registration
    firstedit_query = '''SELECT user_id, rev_id, rev_timestamp, page_id,
                                page_title, page_namespace, source
                         FROM 
                        ((SELECT user_id, rev_id, rev_timestamp, page_id,
                                 page_title, page_namespace,
                                 'revision' AS source
                          FROM revision_userindex
                          JOIN page
                          ON rev_page=page_id
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
                         (SELECT user_id, ar_rev_id AS rev_id,
                                 ar_timestamp AS rev_timestamp,
                                 ar_page_id AS page_id,
                                 ar_title AS page_title,
                                 ar_namespace AS page_namespace,
                                 'archive' AS source
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
                         ORDER BY rev_timestamp ASC LIMIT 1'''

    ## Query to see if that edit is found in the article creation database
    creation_query = '''SELECT *
                        FROM articlecreations
                        WHERE ac_rev_id = %(rev_id)s'''

    ## Query to find if a given article was deleted within thirty days
    ## after creation
    deletion_query = '''SELECT *
                        FROM logging
                        WHERE log_type='delete'
                        AND log_action='delete'
                        AND log_timestamp > %(creation_timestamp)s
                        AND log_timestamp < %(expiry_timestamp)s
                        AND log_page = %(page_id)s'''

    horizon_delta = dt.timedelta(days=horizon)

    ## Data points we'll return
    datapoints = []
    
    for user_id in user_ids:
        with db.cursor(wiki_db_conn, 'dict') as db_cursor:
            ## Find the user's first edit
            fe_timestamp = None
            fe_rev_id = None
            fe_page_id = None
            fe_page_namespace = None
            fe_page_title = None
            fe_source = None

            db_cursor.execute(firstedit_query.format(
                user_id=user_id))
            for row in db_cursor:
                fe_timestamp = row['rev_timestamp'].decode('utf-8')
                fe_rev_id = row['rev_id']
                fe_page_id = row['page_id']
                fe_page_namespace = row['page_namespace']
                fe_page_title = row['page_title'].decode('utf-8')
                fe_source = row['source']

        ## See if that first edit created an article
        fe_created_article = 0
        with db.cursor(local_db_conn, 'dict') as db_cursor:
            db_cursor.execute(creation_query,
                              {'rev_id': fe_rev_id})
            for row in db_cursor:
                fe_created_article = 1

        ## See if that article survived for 30 days
        if fe_created_article:
            ## Create a DB-compatible timestamp for the end of the horizon
            end_timestamp = dt.datetime.strptime(
                fe_timestamp, '%Y%m%d%H%M%S') + horizon_delta
            end_timestamp = end_timestamp.strftime('%Y%m%d%H%M%S')

            fe_article_survived = 1
            with db.cursor(wiki_db_conn, 'dict') as db_cursor:
                db_cursor.execute(
                    deletion_query,
                    {'creation_timestamp': fe_timestamp,
                     'expiry_timestamp': end_timestamp,
                     'page_id': fe_page_id})
                for row in db_cursor:
                    fe_article_survived = 0

            datapoints.append(DataPoint(user_id, fe_created_article,
                                        fe_article_survived))

    ## ok, done
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
    ## one edit in the first 30 days (because if they made no edits,
    ## they couldn't possibly have created an article)
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
                
        datapoints.extend(gather_data(wiki_db, local_db, userids))

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
    
    datapoints = gather_historic(local_db_conn, db_conn,
                                 dt.date(2016,12,27),
                                 dt.date(2017,1,1))
    print('got {} data points'.format(len(datapoints)))
    # print(datapoints[0])
    print(datapoints)
    
    return()

if __name__ == "__main__":
    main()

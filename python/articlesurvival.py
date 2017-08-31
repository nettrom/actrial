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
batch_size = 500

def gather_data(wiki_db_conn, local_db_conn, user_ids, horizon=30):
    '''
    Gather data on whether the given user IDs started out by creating
    a new article, and if so, whether the article survived for a given
    number of days (typically 30). Returns a list of data points with
    information, in no specific order.

    Note: this function assumes that the supplied user IDs made at least
          one edit in their first 30 days since account registration.

    Note: this function also assumes that a specifically named temporary table
          exists for storing creation information.

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

    ## More efficient query is to look up a whole bunch of them at the same
    ## time:
    creation_query = '''SELECT *
                        FROM articlecreations
                        WHERE ac_rev_id IN ({id_list})'''

    ## Query to insert an article creation event into the temporary
    ## table.
    temp_insert_query = '''INSERT INTO s53463__actrial_p.creations
                           VALUES (%s, %s, %s)'''

    ## Query to empty the temp table
    temp_delete_query = '''DELETE FROM s53463__actrial_p.creations'''

    ## Query to find if a group of articles were deleted within thirty
    ## days of their creation
    deletion_query = '''SELECT log_page, log_timestamp, c.user_id AS fe_user_id
                        FROM logging_logindex l
                        JOIN s53463__actrial_p.creations c
                        ON l.log_page=c.page_id
                        WHERE l.log_type='delete'
                        AND l.log_action='delete'
                        AND l.log_timestamp > c.rev_timestamp
                        AND l.log_timestamp < DATE_FORMAT(
                                                DATE_ADD(
                                                  STR_TO_DATE(c.rev_timestamp,
                                                              "%Y%m%d%H%i%S"),
                                                  INTERVAL 30 DAY),
                                                "%Y%m%d%H%i%S")'''

    horizon_delta = dt.timedelta(days=horizon)

    ## Data points we'll return
    datapoints = []

    i = 0
    while i < len(user_ids):
        logging.info('processing subset [{}:{}]'.format(i, i+batch_size))
        
        subset = user_ids[i : i + batch_size]

        ## Mapping user ID to info about the user's first edit,
        ## and mapping a revision ID to the user info
        candidates = {}
        rev_cand_map = {}

        with db.cursor(wiki_db_conn, 'dict') as db_cursor:
            for user_id in subset:
                ## Find the user's first edit
                db_cursor.execute(firstedit_query.format(
                    user_id=user_id))
                for row in db_cursor:
                    ## We might not get data
                    if not row['rev_id']:
                        continue
                    
                    candidates[user_id] = {
                        'fe_user_id': user_id,
                        'fe_timestamp': row['rev_timestamp'],
                        'fe_rev_id' : row['rev_id'],
                        'fe_page_id': row['page_id'],
                        'fe_page_namespace': row['page_namespace'],
                        'fe_page_title': row['page_title'].decode('utf-8'),
                        'fe_source': row['source']}

                    rev_cand_map[row['rev_id']] = candidates[user_id]

        logging.info('checking if first edits created an article')
                    
        ## See if that first edit created an article and insert that
        ## into the temporary table
        j = 0
        with db.cursor(local_db_conn, 'dict') as local_db_cursor:
            with db.cursor(wiki_db_conn, 'dict') as wiki_db_cursor:
                ## Make a comma-separated list of their first edit revision IDs
                rev_ids = ','.join(
                    [str(p['fe_rev_id']) for p in candidates.values()])
                local_db_cursor.execute(creation_query.format(
                    id_list = rev_ids))

                ## Insert first edits into the temp table and update
                ## the candidate info
                for row in local_db_cursor:
                    j += 1
                    candidate = rev_cand_map[row['ac_rev_id']]
                    candidate['fe_created_article'] = 1
                    candidate['fe_article_survived'] = 1
                    wiki_db_cursor.execute(temp_insert_query,
                                           (candidate['fe_page_id'],
                                            candidate['fe_timestamp'],
                                            candidate['fe_user_id']))

        wiki_db_conn.commit()
        logging.info('inserted {} first edits'.format(j))
                    
        logging.info('checking if articles survived')

        ## See if that article survived for 30 days
        with db.cursor(wiki_db_conn, 'dict') as db_cursor:
            db_cursor.execute(deletion_query)
            for row in db_cursor:
                candidate = candidates[row['fe_user_id']]
                candidate['fe_article_survived'] = 0

        ## Go through the candidates and make datapoints of all those
        ## that created an article
        for user in candidates.values():
            if 'fe_created_article' in user:
                datapoints.append(DataPoint(user['fe_user_id'],
                                            user['fe_created_article'],
                                            user['fe_article_survived']))

        logging.info('added users, now have {} data points'.format(
            len(datapoints)))

        ## Empty out the temp table in preparation for the next batch
        with db.cursor(wiki_db_conn, 'dict') as db_cursor:
            db_cursor.execute(temp_delete_query)
        wiki_db_conn.commit()
        
        i += batch_size

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

    ## Query to create a temporary table where we can store the article creation
    ## info we need
    temp_table_query = '''CREATE TEMPORARY TABLE s53463__actrial_p.creations (
                          page_id INT UNSIGNED NOT NULL PRIMARY KEY,
                          rev_timestamp BINARY(14) NOT NULL,
                          user_id INT UNSIGNED NOT NULL)'''

    ## Query to drop the temp table
    temp_drop_query = '''DROP TABLE s53463__actrial_p.creations'''
    
    cur_date = start_date
    delta_days = dt.timedelta(days=step)

    datapoints = []

    if not end_date:
        end_date = dt.date.today()

    ## Create the temporary table
    with db.cursor(wiki_db, 'dict') as db_cursor:
        db_cursor.execute(temp_table_query)
        
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

    ## Drop the temporary table
    with db.cursor(wiki_db, 'dict') as db_cursor:
        db_cursor.execute(temp_drop_query)

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
                                 dt.date(2016,4,14),
                                 dt.date(2016,4,15))
    print('got {} data points'.format(len(datapoints)))
    # print(datapoints[0])
    print(datapoints)
    
    return()

if __name__ == "__main__":
    main()

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

def gather_data(db_conn, user_ids, horizon=30):
    '''
    Gather data on whether the given user IDs started out by creating
    a new article, and if so, whether the article survived for a given
    number of days (typically 30). Returns a list of data points with
    information, in no specific order.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param user_ids: list of user IDs we are interested in
    :type user_ids: list

    :param horizon: number of days an article needs to survive
    :type horizon: int
    '''

    ## Query to get the timestamp and page of a user's first main namespace
    ## edit within 30 days of registration
    firstedit_query = '''SELECT user_id, rev_timestamp, page_id,
                                page_namespace, page_title, source
                         FROM 
                        ((SELECT user_id, rev_timestamp, page_id, page_title,
                                 page_namespace,
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
                         (SELECT user_id, ar_timestamp AS rev_timestamp,
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

    ## Query to find out whether the given page was moved
    rev_moved_query = """SELECT rev_id, rev_timestamp, rev_comment
FROM revision
WHERE rev_page={page_id}
AND rev_timestamp {sign} '{timestamp}'
AND rev_comment REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*:.*'
ORDER BY rev_timestamp {dir}
LIMIT 1"""

    ar_moved_query = """SELECT ar_rev_id AS rev_id,
                               ar_timestamp AS rev_timestamp,
                               ar_comment AS rev_comment
FROM archive
WHERE ar_page_id={page_id}
AND ar_timestamp {sign} '{timestamp}'
AND ar_comment REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*:.*'
ORDER BY ar_timestamp {dir}
LIMIT 1"""
    move_regex = '.*moved .*\[\[([^\]]+)\]\] to \[\[([^\]]+)\]\].*:.*'
    
    ## Note: If the edit was not to any of these namespaces, the user could not
    ## have created an article.
    main_namespace_ids = {0: '',
                           2: 'User',
                           5: 'Wikipedia talk',
                           118: 'Draft')
    main_namespace_names = {v:k for k:v in main_namespace_ids.items()
                            if k != 0}
    
    ## Rephrasing the problem makes it easier to solve. We are not interested
    ## in learning if their first edit was the first edit to a page,
    ## we are interested in knowing if there exists an earlier edit to a page
    ## than the first edit a user made.

    for user_id in user_ids:
        with db.cursor(db_conn, 'dict') as db_cursor:
            ## 1: get the user's first edit:
            fe_timestamp = None
            
            db_cursor.execute(firstedit_query.format(user_id=user_id))
            for row in db_cursor:
                fe_timestamp = row['rev_timestamp'].decode('utf-8')
                fe_pageid = row['page_id']
                fe_title = row['page_title'].decode('utf-8')
                fe_namespace = row['page_namespace']
                fe_source = row['source']

            if not fe_timestamp:
                ## The user didn't make an edit in the first 30 days
                continue

            ## If the edit wasn't to Main, User, WPT, or Draft, they couldn't
            ## have created an article.
            if not fe_namespace in main_namespaces:
                continue

            ## Was the page moved in/out of main after the user's edit?
            move_timestamp = None
            move_comment = None
            move_query = rev_moved_query
            if fe_source == 'archive':
                move_query = ar_moved_query

            db_cursor.execute(move_query.format(page_id=fe_pageid,
                                                sign='>',
                                                timestamp=fe_timestamp,
                                                dir='ASC'))
            for row in db_cursor:
                move_timestamp = row['rev_timestamp']
                move_comment = row['rev_comment'].decode('utf-8')

            com_match = re.match(move_regex, move_comment, re.U)
            source_page = com_match.group(1)
            target_page = com_match.group(2)
            
                
            
            

    ## Revised pseudo-code:
    ## 1: find the timestamp and the page the user first edited
    ## 1.1: Check to see if that page was ever moved in/out of main after
    ##      the edit timestamp.
    ## 1.1.1: If it was moved into main, the user didn't create an article.
    ## 1.1.1: If it was moved from main to user or draft space, user potentially
    ##        created an article.

    ## 2: if the source was 'revision', query the revision table to
    ##    see if there exists an edit with an earlier timestamp.
    ## 2.1 If there does, the user didn't create an article, simply return.
    ## 2.2 If there doesn't, the user potentially created an article.
    ##
    ## 3: If the source was 'archive', query the archive table to see if
    ##    there exists an edit with an earlier timestamp.
    ## 3.1: If there does, we need to check if the page was deleted at
    ##      the time the user edited it. We query the log table.
    ## 3.1.1: If the page was not deleted, the user didn't create an article,
    ##        simply return.
    ## 3.1.2: If the page was deleted, the user recreated an article.

    ## 3.2: If there doesn't, the user potentially created an article.

    ## 4: In all the potential creation scenarios, we want to know if the page
    ##    existed before, so check the deletion log to see if the page has
    ##    ever been deleted:
    ## 4.1: If it was deleted before the user's , mark it a creation.
    ## 4.2: If it was, mark it a recreation.

    ## 5: Check the deletion table to see if the page was deleted within 30 days.
    ## 5.1: If it was, mark it as deleted.
    
    ## Query to get the timestamp and page of a user's first main namespace
    ## edit within 30 days of registration
    firstedit_query = '''SELECT user_id, rev_timestamp, page_id,
                                page_namespace, page_title, source
                         FROM 
                        ((SELECT user_id, rev_timestamp, page_id, page_title,
                                 page_namespace,
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
                         (SELECT user_id, ar_timestamp AS rev_timestamp,
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
    
    revision_query = '''SELECT * FROM revision
    WHERE rev_page = 2317639
    AND rev_timestamp < '20110101000639'
    ORDER BY rev_timestamp DESC
    LIMIT 1'''

    archive_query = '''SELECT * FROM archive
    WHERE ar_namespace=0
    AND ar_title="Comparison_of_Internet_Relay_Chat_clients"
    AND ar_timestamp < '20110101000639'
    ORDER BY ar_timestamp DESC
    LIMIT 1'''

    ## 
    deleted_query = '''SELECT * FROM logging
    WHERE log_namespace=0
    AND log_title="Comparison_of_Internet_Relay_Chat_clients"
    AND log_timestamp < '20110101000639'
    ORDER BY log_timestamp DESC'''

    ## Looks like what I want to do is to do this on stat5, because the
    ## logging table on Labs is broken due to the need for supressing data.
    ## Yeah, if a page is deleted multiple times
    ## (e.g. "Michael_Brady_(baseball)") we'll have multiple sets of revisions
    ## in the archive table, and need the log table to identify whether the
    ## page was deleted at the time the edit was made.

    ## Yeah, we also want to know if the article was deleted within 30 days,
    ## and again, Labs isn't great for page-specific queries due to the
    ## suppression making the indexes not work. I'm not sure if the data
    ## lake is the place to go for this info, looks like the SQL database
    ## might be a good shot since we're not trying to create aggregate info.
    
    ## The challenge for this is to know whether an edit created an article
    ## in the main namespace. I wonder if what we'll have to do is to replay
    ## first edits of all articles currently in the main namespace, as well
    ## as all archived edits to build a support table for this. Then use the
    ## log table to identify moves in/out of main namespace, and deletions
    ## (if we are to track recreation of deleted articles).

    ## Is it easier to query the data lake for all create events from
    ## 2009-01-01 onwards? We can get all the create events for pages
    ## in historical namespace 0, and their associated user ID. Then all
    ## we need to check is whether a user's first edit matches one of those.
    ## If it does, check the deletion log to see if the page was deletd within
    ## 30 days.

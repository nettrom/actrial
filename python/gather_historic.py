#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical data for the statistics we're interested in.

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

import db
import MySQLdb # to catch exceptions

import first30
import newaccounts
import registrations
import autoconfirmed
import survival
import moves
import patrollers
import articlesurvival
import deletionreasons

def insert_newaccounts(local_db, wiki_db, start_date, end_date):
    '''
    Gather historic data about the number of newly created accounts
    from the logging table and insert that into our database.

    :param local_db: Database connection to the local database where our
                     summary data is to be put
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection
    '''

    insert_query = '''INSERT INTO newaccounts
                      (na_date, na_newusers, na_autocreate, na_byemail,
                       na_create, na_create2)
                      VALUES (%s, %s, %s, %s, %s, %s)'''

    ## Early dates use na_newusers, but it's a sum of all other types,
    ## so we update the dataset afterwards so newer dates also has this
    ## column populated.
    update_query = '''UPDATE newaccounts
                      SET na_newusers = na_autocreate + na_byemail +
                                        na_create + na_create2
                      WHERE na_newusers = 0'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    ## Gather data for new accounts (date set to 2005-09-08 because we only
    ## have partial data from 2005-09-07.
    if not start_date:
        start_date = dt.date(2005,9,8)

    if not end_date:
        end_date = dt.date(2017,8,6)

    ## Let's try to populate this a year at a time
    one_year_ish = dt.timedelta(days=365)

    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + one_year_ish
        if stop_date > end_date:
            stop_date = end_date

        datapoints = newaccounts.gather_historic(wiki_db_conn,
                                                 cur_date,
                                                 stop_date)

        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(insert_query,
                                  (datapoint.date,
                                   datapoint.newusers,
                                   datapoint.autocreate,
                                   datapoint.byemail,
                                   datapoint.create,
                                   datapoint.create2))
                i += 1
                if i % 100 == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()
            
        cur_date += one_year_ish

    ## Update `na_newusers` to be the sum of the other columns
    with db.cursor(local_db_conn) as db_cursor:
        db_cursor.execute(update_query)
        local_db_conn.commit()
        
    local_db_conn.close()
    wiki_db_conn.close()

    return()

def insert_registrations(local_db, wiki_db, start_date=None, end_date=None):
    '''
    Gather data on the registration of user accounts from the given Wikipedia
    edition, and populate our `account_stats` table. If start and end dates
    are given, data is gathered from the start date and up to, but not
    including, the end date.

    :param local_db: Database connection to the local database where our
                     summary data is to be put
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection

    :param start_date: date to start gathering data from
    :type start_date: datetime.date

    :param end_date: date to end gathering data from
    :type end_date: datetime.date
    '''

    insert_query = '''INSERT INTO account_stats
                      (as_userid, as_reg_timestamp, as_create_type)
                      VALUES (%s, %s, %s)'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()

    ## Batch size for database commits
    batch_size = 1000

    ## If start and end dates are not given, set them appropriately.
    ## Start date is set to 2009-01-01 because 2009 is the first year
    ## we have full data.
    if not start_date:
        start_date = dt.date(2009,1,1)
    if not end_date:
        end_date = dt.date(2017,8,6)

    ## Let's try to populate this a week at a time (if we do longer,
    ## the replicated database is likely to time out on us)
    one_week = dt.timedelta(days=7)

    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + one_week
        if stop_date > end_date:
            stop_date = end_date

        datapoints = registrations.gather_historic(wiki_db_conn,
                                                   cur_date,
                                                   stop_date)

        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                try:
                    db_cursor.execute(insert_query,
                                      (datapoint.userid,
                                       datapoint.reg_timestamp,
                                       datapoint.create_type))
                    i += 1
                except MySQLdb.IntegrityError as e:
                    ## Duplicate registration log entries do exist...
                    logging.warning('Attempted to insert duplicate ID {}'.format(datapoint.userid))
                    continue

                if i % batch_size == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()
            
        cur_date += one_week

    local_db_conn.close()
    wiki_db_conn.close()
        
    return()

def insert_first30(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on the registration of user accounts from the given Wikipedia
    edition, and populate our `account_stats` table.

    :param local_db: Database connection to the local database where our
                     summary data is to be put
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection

    :param start_date: date at which to start gathering user IDs to process
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering user IDs to process
    :type end_date: datetime.dt
    '''

    update_query = '''UPDATE account_stats
                      SET as_num_edits_30=%s,
                          as_num_namespaces_30=%s,
                          as_num_pages_30=%s
                      WHERE as_userid=%s'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    ## Batch size for database commits
    batch_size = 1000

    ## FIXME: Make better progress output indicators so I can figure out
    ## where to pick up if the process dies.
    
    ## Let's try to populate this five days at a time:
    time_step = dt.timedelta(days=5)

    ## If the end date is less than 30 days prior to today, set it
    ## to 30 days prior to today. We don't want to gather statistics for
    ## users who have not had 30 days of activity yet.
    if end_date > (dt.date.today() - dt.timedelta(days=30)):
        ## Calculate the interval between start and end date so we
        ## can correctly move both.
        interval = end_date - start_date
        
        end_date = dt.date.today() - dt.timedelta(days=30)
        start_date = end_date - interval
    
    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        datapoints = first30.gather_historic(local_db_conn, wiki_db_conn,
                                             cur_date, stop_date)

        ## Because data gathering takes a long time, the database might drop
        ## the connection. In that case, we reconnect
        try:
            with db.cursor(local_db_conn) as db_cursor:
                db_cursor.execute("SELECT * FROM account_stats LIMIT 1")
        except MySQLdb.OperationalError as e:
            local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                                       local_db['dbconf'])
            
        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(update_query,
                                  (datapoint.num_edits,
                                   datapoint.num_namespaces,
                                   datapoint.num_pages,
                                   datapoint.userid))
                i += 1
                if i % batch_size == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()

        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    wiki_db_conn.close()
    local_db_conn.close()
        
    return()

def insert_autoconfirmed(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on when accounts that made at least ten edits in the
    first 30 days reached autoconfirmed status, then update the data
    for those accounts in the `account_stats` table.

    :param local_db: Database connection to the local database where our
                     summary data is stored.
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database.
    :type wiki_db: MySQLdb.Connection

    :param start_date: date at which to start gathering user IDs to process.
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering user IDs to process.
    :type end_date: datetime.dt
    '''

    update_query = '''UPDATE account_stats
                      SET as_autoconfirmed_30=%s,
                          as_autoconfirmed_30_timestamp=%s
                      WHERE as_userid=%s'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    ## Batch size for database commits
    batch_size = 1000

    ## If the end date is less than 30 days prior to today, set it
    ## to 30 days prior to today. We don't want to gather statistics for
    ## users who have not had 30 days of activity yet.
    if end_date > (dt.date.today() - dt.timedelta(days=30)):
        ## Calculate the interval between start and end date so we
        ## can correctly move both.
        interval = end_date - start_date
        
        end_date = dt.date.today() - dt.timedelta(days=30)
        start_date = end_date - interval
    
    ## Let's try to populate this a week at a time (if we do longer,
    ## the replicated database is likely to time out on us)
    one_week = dt.timedelta(days=7)

    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + one_week
        if stop_date > end_date:
            stop_date = end_date

        datapoints = autoconfirmed.gather_historic(local_db_conn,
                                                   wiki_db_conn,
                                                   cur_date, stop_date)
        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(update_query,
                                  (datapoint.autoconfirmed,
                                   datapoint.ac_timestamp,
                                   datapoint.userid))
                i += 1
                if i % batch_size == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()

        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += one_week

    local_db_conn.close()
    wiki_db_conn.close()
        
    return()

def insert_survival(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on the survival of newly registered accounts based on
    activity in their first and fifth week after registration.

    :param local_db: Database connection to the local database where our
                     summary data is to be put
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection

    :param start_date: date at which to start gathering user IDs to process
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering user IDs to process
    :type end_date: datetime.dt
    '''

    update_query = '''UPDATE account_stats
                      SET as_num_edits_week_1=%s,
                          as_num_edits_week_5=%s
                      WHERE as_userid=%s'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    ## Batch size for database commits
    batch_size = 1000

    ## Let's try to populate this five days at a time:
    time_step = dt.timedelta(days=5)

    ## If the end date is less than 5 weeks prior to today, set it
    ## to five weeks prior to today. We don't want to gather statistics for
    ## users who have not completed the fifth week of activity yet.
    if end_date > (dt.date.today() - dt.timedelta(days=35)):
        ## Calculate the interval between start and end date so we
        ## can correctly move both.
        interval = end_date - start_date

        end_date = dt.date.today() - dt.timedelta(days=35)
        start_date = end_date - interval
    
    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        datapoints = survival.gather_historic(local_db_conn, wiki_db_conn,
                                              cur_date, stop_date)

        ## Because data gathering takes a long time, the database might drop
        ## the connection. In that case, we reconnect
        try:
            with db.cursor(local_db_conn) as db_cursor:
                db_cursor.execute("SELECT * FROM account_stats LIMIT 1")
        except MySQLdb.OperationalError as e:
            local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                                       local_db['dbconf'])
            
        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(update_query,
                                  (datapoint.num_edits_week1,
                                   datapoint.num_edits_week5,
                                   datapoint.userid))
                i += 1
                if i % batch_size == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()

        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    wiki_db_conn.close()
    local_db_conn.close()
        
    return()

def insert_moves(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on the number of moves from User and Draft namespaces
    to the main namespace. Since Wikipedia talk was earlier used as a draft
    namespace, moves from there to main is also counted. Counts per day
    is inserted from `start_date` up to, but not including, `end_date`.

    :param local_db: Connection information for connecting to the local
                     database where summary info is stored
    :type local_db: dict

    :param wiki_db: Connection information for connecting to the replicated
                    Wikipedia database
    :type wiki_db: dict

    :param start_date: date at which to start gathering data
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering data
    :type end_date: datetime.dt
    '''

    update_query = '''INSERT INTO moves_into_main
                      (mim_date, mim_num_moves_user, mim_num_moves_draft)
                      VALUES (%s, %s, %s)'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()

    ## Let's grab the namespaces only once:
    namespaces = moves.get_namespaces()
    
    ## We'll process a week in each iteration:
    time_step = dt.timedelta(days=7)
    
    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        datapoints = moves.gather_historic(wiki_db_conn, cur_date, stop_date,
                                           namespaces)

        ## Because data gathering takes a long time, the local database is
        ## likely to drop the connection, so we reconnect every time and close
        ## the connection after inserting the data.
        local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                                       local_db['dbconf'])
        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(update_query,
                                  (datapoint.date,
                                   datapoint.num_moves_user,
                                   datapoint.num_moves_draft))
            local_db_conn.commit()

        local_db_conn.close()
        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    wiki_db_conn.close()
    return()

def insert_patrollers(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on the numer of patrol actions performed by each active patroller
    from the given start date up to, but not including, the given end date,
    then insert that into our local database.

    :param local_db: Connection information for connecting to the local
                     database where summary info is stored
    :type local_db: dict

    :param wiki_db: Connection information for connecting to the replicated
                    Wikipedia database
    :type wiki_db: dict

    :param start_date: date at which to start gathering data
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering data
    :type end_date: datetime.dt
    '''

    update_query = '''INSERT INTO patrollers
                      (pat_userid, pat_date, pat_num_actions)
                      VALUES (%s, %s, %s)'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()

    ## Batch size for database commits
    batch_size = 1000
    
    ## We'll process a week in each iteration:
    time_step = dt.timedelta(days=7)
    
    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        datapoints = patrollers.gather_historic(wiki_db_conn,
                                                cur_date, stop_date)
        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(update_query,
                                  (datapoint.userid,
                                   datapoint.date,
                                   datapoint.num_actions))
                i += 1
                if i % batch_size == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()

        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    wiki_db_conn.close()
    local_db_conn.close()
    return()

def insert_articlesurvival(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on whether a newly registered account created an article
    in their first edit, and then whether that article survived for at
    least 30 days.

    :param local_db: Database connection to the local database where our
                     summary data is to be put
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection

    :param start_date: date at which to start gathering user IDs to process
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering user IDs to process
    :type end_date: datetime.dt
    '''

    update_query = '''UPDATE account_stats
                      SET as_created_article=%s,
                          as_article_survived=%s
                      WHERE as_userid=%s'''

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    ## Batch size for database commits
    batch_size = 1000

    ## Let's try to populate this a week at a time:
    time_step = dt.timedelta(days=7)
   
    ## If the end date is less than 30 days prior to today, set it
    ## to 30 days prior to today. We don't want to gather statistics for
    ## users who have not been around for 30 days yet, because if they
    ## created an article, we won't know if it has survived.
    if end_date > (dt.date.today() - dt.timedelta(days=30)):
        ## Calculate the interval between start and end date so we
        ## can correctly move both.
        interval = end_date - start_date
        
        end_date = dt.date.today() - dt.timedelta(days=30)
        start_date = end_date - interval

    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        datapoints = articlesurvival.gather_historic(local_db_conn,
                                                     wiki_db_conn,
                                                     cur_date, stop_date)

        ## Because data gathering takes a long time, the database might drop
        ## the connection. In that case, we reconnect
        try:
            with db.cursor(local_db_conn) as db_cursor:
                db_cursor.execute("SELECT * FROM account_stats LIMIT 1")
        except MySQLdb.OperationalError as e:
            local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                                       local_db['dbconf'])
            
        with db.cursor(local_db_conn) as db_cursor:
            i = 0
            for datapoint in datapoints:
                db_cursor.execute(update_query,
                                  (datapoint.created_article,
                                   datapoint.article_survived,
                                   datapoint.userid))
                i += 1
                if i % batch_size == 0:
                    logging.info('processed {} datapoints, comitting'.format(i))
                    local_db_conn.commit()

            ## Commit any outstanding inserts
            logging.info('processed {} datapoints, final commit'.format(i))
            local_db_conn.commit()

        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    wiki_db_conn.close()
    local_db_conn.close()
        
    return()

def insert_deletionreasons(local_db, wiki_db, start_date, end_date):
    '''
    Gather data on deletion reasons for deletions in Main (ns=0),
    User (ns=2), and Draft (ns=118) and insert that into the database.

    :param local_db: Database connection to the local database where our
                     summary data is to be put
    :type local_db: MySQLdb.Connection

    :param wiki_db: Database connection to the replicated Wikipedia database
    :type wiki_db: MySQLdb.Connection

    :param start_date: date at which to start gathering deletion data
    :type end_date: datetime.date

    :param end_date: date at which to stop gathering deletion data
    :type end_date: datetime.dt
    '''

    ## First two values are the date and namespace,
    ## the next row are the general reasons (G1-G13),
    ## then the article reasons (A1-A11, with some missing)
    ## then the user reasons (U1-U5, but no U4),
    ## lastly PROD, AFD, and "other"
    insert_query = '''INSERT INTO deletion_reasons
                      VALUES (%s, %s,
                      %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s,
                      %s, %s, %s, %s, %s, %s, %s, %s,
                      %s, %s, %s, %s,
                      %s, %s, %s)'''
    

    local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                               local_db['dbconf'])
    if not local_db_conn:
        logging.error('unable to connect to local database server {}'.format(local_db['hostname']))
        return()

    wiki_db_conn = db.connect(wiki_db['hostname'], wiki_db['dbname'],
                              wiki_db['dbconf'])
    if not wiki_db_conn:
        logging.error('unable to connect to replicated database server {}'.format(wiki_db['hostname']))
        return()
    
    ## Let's try to populate this a week at a time:
    time_step = dt.timedelta(days=7)
   
    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        datapoints = deletionreasons.gather_historic(wiki_db_conn,
                                                     cur_date, stop_date)

        ## Because data gathering might take a bit, the local DB might drop
        ## the connection. In that case, we reconnect
        try:
            with db.cursor(local_db_conn) as db_cursor:
                db_cursor.execute("SELECT * FROM account_stats LIMIT 1")
        except MySQLdb.OperationalError as e:
            local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                                       local_db['dbconf'])
            
        with db.cursor(local_db_conn) as db_cursor:
            for datapoint in datapoints:
                db_cursor.execute(insert_query,
                                  (datapoint.dr_date, datapoint.dr_namespace,
                                   datapoint.stats['G1'],
                                   datapoint.stats['G2'],
                                   datapoint.stats['G3'],
                                   datapoint.stats['G4'],
                                   datapoint.stats['G5'],
                                   datapoint.stats['G6'],
                                   datapoint.stats['G7'],
                                   datapoint.stats['G8'],
                                   datapoint.stats['G9'],
                                   datapoint.stats['G10'],
                                   datapoint.stats['G11'],
                                   datapoint.stats['G12'],
                                   datapoint.stats['G13'],
                                   datapoint.stats['A1'],
                                   datapoint.stats['A2'],
                                   datapoint.stats['A3'],
                                   datapoint.stats['A5'],
                                   datapoint.stats['A7'],
                                   datapoint.stats['A9'],
                                   datapoint.stats['A10'],
                                   datapoint.stats['A11'],
                                   datapoint.stats['U1'],
                                   datapoint.stats['U2'],
                                   datapoint.stats['U3'],
                                   datapoint.stats['U5'],
                                   datapoint.stats['PROD'],
                                   datapoint.stats['AFD'],
                                   datapoint.stats['other']))

                logging.info('processed {} datapoints, comitting'.format(len(datapoints)))
                local_db_conn.commit()

        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    wiki_db_conn.close()
    local_db_conn.close()
        
    return()

def insert_historic(start_date, end_date, run_everything, run_newaccounts,
                    run_registrations, run_first30, run_autoconfirmed,
                    run_survival, run_moves, run_patrollers,
                    run_article_survival, run_deletion_reasons):
    '''
    Gather historic data for the various statistics we are interested in
    and populate our database tables. Supplied start and end dates can be
    passed on to functions. Data gathering is expected to be from the
    start date and up to, but not including, the end date.

    :param start_date: start date for gathering data
    :type start_date: datetime.date

    :param end_date: end date for gathering data ()
    :type end_date: datetime.date

    :param run_everything: update/insert everything?
    :type run_everything: bool

    :param run_newaccounts: should we update new account summaries?
    :type run_newaccounts: bool

    :param run_registrations: should we update data on registration times?
    :type run_registrations: bool

    :param run_first30: should we update user edit stats in the first 30 days?
    :type run_first30: bool

    :param run_autoconfirmed: should we update autoconfirmed status for users?
    :type run_autoconfirmed: bool

    :param run_survival: should we update survival status for users?
    :type run_survival: bool

    :param run_moves: insert counts of moves from User/Draft namespaces into main
    :type run_moves: bool

    :param run_patrollers: insert counts of patroller actions?
    :type run_patrollers: bool

    :param run_article_survival: update status on whether a user created an
                                 article in their first edit, and if it survived?
    :type run_article_survival: bool

    :param run_deletion_reasons: insert data on deletion reasons?
    :type run_deletion_reasons: bool
    '''

    db_conf = '~/replica.my.cnf'
    local_db = {'hostname': 'tools.labsdb',
                'dbname': 's53463__actrial_p',
                'dbconf': db_conf}
    wiki_db = {'hostname': 'enwiki.analytics.db.svc.eqiad.wmflabs',
               'dbname': 'enwiki_p',
               'dbconf': db_conf}

    if run_everything or run_newaccounts:
        logging.info("Gathering counts of number of account registrations")
        insert_newaccounts(local_db, wiki_db, start_date, end_date)

    if run_everything or run_registrations:
        logging.info("Gathering registration info on new accounts to add to the data table")
        insert_registrations(local_db, wiki_db, start_date, end_date)

    if run_everything or run_first30:
        logging.info("Gathering user activity in first 30 days")
        insert_first30(local_db, wiki_db, start_date, end_date)

    if run_everything or run_autoconfirmed:
        logging.info("Gathering data on autoconfirmed users")
        insert_autoconfirmed(local_db, wiki_db, start_date, end_date)

    if run_everything or run_survival:
        logging.info("Gathering data on new account survival")
        insert_survival(local_db, wiki_db, start_date, end_date)

    if run_everything or run_moves:
        logging.info("Gathering counts of moves from User/Draft into main")
        insert_moves(local_db, wiki_db, start_date, end_date)

    if run_everything or run_patrollers:
        logging.info("Gathering counts of patrol actions done by patrollers")
        insert_patrollers(local_db, wiki_db, start_date, end_date)

    if run_everything or run_article_survival:
        logging.info("Gathering data on article survival")
        insert_articlesurvival(local_db, wiki_db, start_date, end_date)
                             
    if run_everything or run_deletion_reasons:
        logging.info("Gathering data on deletion reasons")
        insert_deletionreasons(local_db, wiki_db, start_date, end_date)
        
    return()

def main():
    import argparse

    def valid_date(d):
        try:
            return(dt.datetime.strptime(d, "%Y-%m-%d").date())
        except ValueError:
            raise argparse.ArgumentTypeError("Please write dates in the preferred format (YYYY-MM-DD)")
    
    cli_parser = argparse.ArgumentParser(
        description="script to gather historic measurements and populate our database tables"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    cli_parser.add_argument('--all', action='store_true',
                            help='insert/update everything')

    cli_parser.add_argument('--newaccounts', action='store_true',
                            help='insert counts of the number of registered accouvnts')

    cli_parser.add_argument('--registrations', action='store_true',
                            help='insert account registration data')

    cli_parser.add_argument('--first30', action='store_true',
                            help='update user activity stats in first 30 days')

    cli_parser.add_argument('--autoconfirmed', action='store_true',
                            help='update autoconfirmed status in first 30 days')

    cli_parser.add_argument('--survival', action='store_true',
                            help='update survival status based on edits in the first and fifth week')

    cli_parser.add_argument('--moves', action='store_true',
                            help='insert counts of the number of moves into article namespace from User and Draft namespaces')

    cli_parser.add_argument('--patrollers', action='store_true',
                            help='insert counts of the number of patrol actions per active patroller')

    cli_parser.add_argument('--article_survival', action='store_true',
                            help='update info on whether a new account started out by creating a new article, and if so, whether the article survived')

    cli_parser.add_argument('--deletion_reasons', action='store_true',
                            help='insert counts of deletion reasons for Main, User, and Draft namespaces')
    
    cli_parser.add_argument('start_date', type=valid_date,
                            help='start date for gathering data (format: YYYY-MM-DD)')

    cli_parser.add_argument('end_date', type=valid_date,
                            help='end date for gathering data (format: YYYY-MM-DD)')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    insert_historic(args.start_date, args.end_date, args.all, args.newaccounts,
                    args.registrations, args.first30, args.autoconfirmed,
                    args.survival, args.moves, args.patrollers,
                    args.article_survival, args.deletion_reasons)
        
    return()

if __name__ == "__main__":
    main()

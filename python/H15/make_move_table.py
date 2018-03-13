#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather the page ID, time of move, and source namespace
for moves into the Main namespace.

Copyright (c) 2017-2018 Wikimedia Foundation

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

import requests

class WPAPIError(Exception):
    pass

## A data point is in this case a page move. We capture the page ID,
## the timestamp of the revision that moved it, and the source namespace.
DataPoint = namedtuple('DataPoint',
                       ['page_id', 'timestamp', 'source_ns'])

def get_namespaces():
    '''
    Make a request to the API to get the names and aliases of all namespaces
    and return a set of them.
    '''
    
    namespaces = set()

    api_ns_url = "https://en.wikipedia.org/w/api.php?action=query&meta=siteinfo&siprop=namespaces|namespacealiases&format=json"

    response = requests.get(api_ns_url)
    if not response.status_code == 200:
        logging.error('Wikipedia API request failed')
        raise(WPAPIError)

    res_json = response.json()
    if not 'query' in res_json or not 'namespaces' in res_json['query']:
        logging.error('Wikipedia API response not as expected')
        raise(WPAPIError)
    
    for ns, ns_data in res_json['query']['namespaces'].items():
        if not 'canonical' in ns_data:
            continue
        
        namespaces.add(ns_data['canonical'])
        if ns_data['*'] != '':
            namespaces.add(ns_data['*'])

    if 'namespacealiases' in res_json['query']:
        for ns_data in res_json['query']['namespacealiases']:
            namespaces.add(ns_data['*'])

    return(namespaces)

def gather_data(db_conn, move_date, namespaces=None):
    '''
    Gather data on the moves from User and Draft/Wikipedia talk into Main
    for the given date. Returns a list of `DataPoint` named tuples, each
    containing data about a specific page move.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param move_date: date to gather number of moves for
    :type start_timestamp: datetime.date
    '''

    if not namespaces:
        namespaces = get_namespaces()
    
    ## Query to match revisions that indicate an article was moved from
    ## either User, Draft, or Wikipedia talk
    ## Note the usage of DISTINCT because a move creates two revisions with
    ## identical comments, one for each source/destination pair of pages.
    move_query = '''
    SELECT DISTINCT rev_page, rev_timestamp, rev_comment
    FROM (
      (SELECT rev_page, rev_timestamp, rev_comment
       FROM revision
       WHERE rev_timestamp >= %(start_timestamp)s
       AND rev_timestamp < %(end_timestamp)s
       AND rev_comment REGEXP ".*moved .*\\\\[\\\\[((User|Draft|Wikipedia talk):[^\\\\]]+)\\\\]\\\\] to \\\\[\\\\[([^\\\\]]+)\\\\]\\\\].*")
      UNION
      (SELECT ar_page_id AS rev_page,
              ar_timestamp AS rev_timestamp,
              ar_comment AS rev_comment
       FROM archive
       WHERE ar_timestamp >= %(start_timestamp)s
       AND ar_timestamp < %(end_timestamp)s
       AND ar_comment REGEXP ".*moved .*\\\\[\\\\[((User|Draft|Wikipedia talk):[^\\\\]]+)\\\\]\\\\] to \\\\[\\\\[([^\\\\]]+)\\\\]\\\\].*")
    ) AS revisiondata'''

    ## Regular expression to extract source and destination of the move
    ## from the revision comment. Source has to be in one of the namespaces
    ## we are interested in.
    move_comment_re = re.compile(".*moved .*\\[\\[((User|Draft|Wikipedia talk):[^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\]", re.U)

    ## The moves we identified on this given date
    moves = []
    
    ## Create timestamps at midnight on the given day,
    ## and midnight the next day
    start_timestamp = dt.datetime.combine(move_date, dt.time(0, 0, 0))
    end_timestamp = dt.datetime.combine(move_date + dt.timedelta(days=1),
                                        dt.time(0, 0, 0))
    
    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute('set profiling = 1')
        try:
            db_cursor.execute(
                move_query,
                {'start_timestamp': start_timestamp.strftime('%Y%m%d%H%M%S'),
                 'end_timestamp': end_timestamp.strftime('%Y%m%d%H%M%S')})
        except Exception as e:
            print(e)
            db_cursor.execute('show profiles')
            for row in db_cursor:
                print(row)        
            db_cursor.execute('set profiling = 0')
        
        for row in db_cursor:
            try:
                rev_comment = row['rev_comment'].decode('utf-8')
                rev_timestamp = row['rev_timestamp'].decode('utf-8')
                rev_timestamp = dt.datetime.strptime(rev_timestamp,
                                                     '%Y%m%d%H%M%S')
            except UnicodeDecodeError:
                logging.warning('unable to decode rev_comment or timestamp, unicode error?')
                continue
            except ValueError:
                logging.warning('unable to parse rev_timestamp {}'.format(row['rev_timestamp']))
                continue

            ## Round robin page swaps recommends using subpages of Draft:Move
            ## as temporary storage. This skews our results. Skip revisions
            ## referencing "Draft:Move/", "pageswap", or mentions "round-robin",
            ## indicating that type of move.
            if re.search('pageswap|(draft:move/)|(round[- ]robin)',
                         rev_comment, re.I):
                continue
            
            match = move_comment_re.match(rev_comment)
            if not match:
                continue

            source_ns = match.group(2)
            target = match.group(3)

            ## Is the target in a non-content namespace?
            if ':' in target:
                (potential_ns, title) = target.split(':', 1)
                if potential_ns in namespaces:
                    continue

            if source_ns == 'User':
                source_ns = 2
            elif source_ns == 'Wikipedia talk':
                source_ns = 5
            else: # Draft, because our regex filters to three namespaces
                source_ns = 118

            moves.append(DataPoint(row['rev_page'],
                                   rev_timestamp,
                                   source_ns))

    return(moves)

def gather_historic(db_conn, start_date, end_date=None, namespaces=None):
    '''
    Gather historic data over a larger timespan covering a given number
    of days at a time. Returns a list of DataPoint named tuples ordered
    by date.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_date: First date to gather data for
    :type start_date: datetime.date

    :param end_date: Last date to gather data for
    :type end_date: datetime.date
    '''

    cur_date = start_date

    datapoints = []

    if not end_date:
        end_date = dt.date.today()

    one_day = dt.timedelta(days=1)

    if not namespaces:
        namespaces = get_namespaces()
    
    while cur_date < end_date:
        logging.info('gathering data for {}'.format(cur_date))

        try:
            datapoints.extend(gather_data(db_conn, cur_date, namespaces))
        except WPAPIError:
            return(datapoints)

        cur_date += one_day

    return(datapoints)

def populate_database(start_date, end_date):
    '''
    Gather data of page moves between the given dates and populate our
    database with the results.

    :param start_date: First date to gather data for
    :type start_date: datetime.date

    :param last_date: Date to gather data up to, but not including
    :type last_date: datetime.date
    '''

    insert_query = '''INSERT INTO pages_moved_into_main
                      VALUES (%s, %s, %s)'''
    
    ## Database setups for our local tool database and the English WP replica
    db_conf = '~/replica.my.cnf'
    local_db = {'hostname': 'tools.labsdb',
                'dbname': 's53463__actrial_p',
                'dbconf': db_conf}
    wiki_db = {'hostname': 'enwiki.analytics.db.svc.eqiad.wmflabs',
               'dbname': 'enwiki_p',
               'dbconf': db_conf}

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

    enwiki_namespaces = get_namespaces()
    
    ## Let's try to populate this a week at a time:
    time_step = dt.timedelta(days=7)
   
    cur_date = start_date
    while cur_date < end_date:
        stop_date = cur_date + time_step
        if stop_date > end_date:
            stop_date = end_date

        moves = gather_historic(wiki_db_conn, cur_date, stop_date,
                                enwiki_namespaces)

        ## Because data gathering might take a bit, the local DB might drop
        ## the connection. In that case, we reconnect
        try:
            with db.cursor(local_db_conn) as db_cursor:
                db_cursor.execute("SELECT * FROM account_stats LIMIT 1")
        except MySQLdb.OperationalError as e:
            local_db_conn = db.connect(local_db['hostname'], local_db['dbname'],
                                       local_db['dbconf'])

        with db.cursor(local_db_conn) as db_cursor:
            db_cursor.executemany(insert_query,
                                  moves)

            logging.info('processed {} datapoints, comitting'.format(len(moves)))
            local_db_conn.commit()

        # iterate
        print('Completed inserting data from {} to {}'.format(cur_date,
                                                              stop_date))
        cur_date += time_step

    # ok, done
    return()

def main():
    import argparse

    def valid_date(d):
        try:
            return(dt.datetime.strptime(d, "%Y-%m-%d").date())
        except ValueError:
            raise argparse.ArgumentTypeError("Please write dates in the preferred format (YYYY-MM-DD)")
    
    cli_parser = argparse.ArgumentParser(
        description="script to gather data on page moves done during a given date range and populate our database"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    cli_parser.add_argument('start_date', type=valid_date,
                            help='start date for gathering data (format: YYYY-MM-DD)')

    cli_parser.add_argument('end_date', type=valid_date,
                            help='end date for gathering data (format: YYYY-MM-DD)')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    populate_database(args.start_date, args.end_date)
    
    return()

if __name__ == "__main__":
    main()

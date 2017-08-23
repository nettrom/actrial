#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical information about the number of articles that
get created by moves from drafts.

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

import requests

class WPAPIError(Exception):
    pass

## Named tuple for a data point with the number of moves that happened
## at a specific date.
DataPoint = namedtuple('DataPoint',
                       ['date', 'num_moves_user', 'num_moves_draft'])

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
    Gather data on the number of articles created by moves from User
    and Draft/Wikipedia talk for the given date. Returns a data point with
    the number of moves from the user namespace, and from the draft namespace
    (or Wikipedia talk namespace, beceause that was used before Draft).

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param move_date: date to gather number of moves for
    :type start_timestamp: datetime.date
    '''

    if not namespaces:
        namespaces = get_namespaces()
    
    ## Query to match revisions that indicate an article was moved from
    ## either User, Draft, or Wikipedia talk
    ## FIXME: search both revision and archive
    ## FIXME: need to use DISTINCT because a move creates two revisions with
    ##        identical comments, one for each source/destination pair of pages.
    move_query = '''
    SELECT DISTINCT(rev_comment)
    FROM (
      (SELECT rev_comment
       FROM revision
       WHERE rev_timestamp >= %(start_timestamp)s
       AND rev_timestamp < %(end_timestamp)s
       AND rev_comment REGEXP ".*moved .*\\\\[\\\\[((User|Draft|Wikipedia talk):[^\\\\]]+)\\\\]\\\\] to \\\\[\\\\[([^\\\\]]+)\\\\]\\\\].*")
      UNION
      (SELECT ar_comment AS rev_comment
       FROM archive
       WHERE ar_timestamp >= %(start_timestamp)s
       AND ar_timestamp < %(end_timestamp)s
       AND ar_comment REGEXP ".*moved .*\\\\[\\\\[((User|Draft|Wikipedia talk):[^\\\\]]+)\\\\]\\\\] to \\\\[\\\\[([^\\\\]]+)\\\\]\\\\].*")
    ) AS revisiondata'''

    ## Regular expression to extract source and destination of the move
    ## from the revision comment. Source has to be in one of the namespaces
    ## we are interested in.
    move_comment_re = re.compile(".*moved .*\\[\\[((User|Draft|Wikipedia talk):[^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\]", re.U)

    num_moves_user = 0
    num_moves_draft = 0

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
            except UnicodeDecodeError:
                logging.warning('unable to decode rev_comment, unicode error?')
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
                num_moves_user += 1
            else:
                num_moves_draft += 1

    return(DataPoint(move_date, num_moves_user, num_moves_draft))

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
            datapoints.append(gather_data(db_conn, cur_date, namespaces))
        except WPAPIError:
            return(datapoints)

        cur_date += one_day

    return(datapoints)

def main():
    '''
    Run some tests.
    '''
    logging.basicConfig(level=logging.INFO)
    
    db_conn = db.connect('enwiki.labsdb', 'enwiki_p', '~/replica.my.cnf')
   
    datapoint = gather_data(db_conn, dt.date(2017,1,7))
    print(datapoint)

    start = dt.date(2017,1, 1)
    end = dt.date(2017,1,8)
    # datapoints = gather_historic(db_conn, start, end)
    # print(datapoints[0])
    # print(datapoints[-1])
    
    return()

if __name__ == "__main__":
    main()

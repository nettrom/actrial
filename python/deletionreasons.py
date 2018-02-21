#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to gather historical statistics or update statistics for a given day
with info about deleted pages.

Copyright (c) 2018 Wikimedia Foundation

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

class DataPoint:
    def __init__(self, date, namespace):
        self.dr_date = date
        self.dr_namespace = namespace

        self.stats = {
            'G1' : 0,
            'G2' : 0,
            'G3' : 0,
            'G4' : 0,
            'G5' : 0,
            'G6' : 0,
            'G7' : 0,
            'G8' : 0,
            'G9' : 0,
            'G10' : 0,
            'G11' : 0,
            'G12' : 0,
            'G13' : 0,
            'A1' : 0,
            'A2' : 0,
            'A3' : 0,
            'A5' : 0,
            'A7' : 0,
            'A9' : 0,
            'A10' : 0,
            'A11' : 0,
            'U1' : 0,
            'U2' : 0,
            'U3' : 0,
            'U5' : 0,
            'R2' : 0,
            'R3' : 0,
            'X1' : 0,
            'PROD' : 0,
            'AFD' : 0,
            'other' : 0
        }
            

def gather_data(db_conn, start_timestamp, end_timestamp):
    '''
    Gather data on deletions between the given start timestamp and up to,
    but not including, the given end timestamp. Returns a list of DataPoint
    instances in no particular order.

    :param db_conn: Database connection to use for queries
    :type db_conn: MySQLdb.Connection

    :param start_timestamp: timestamp to gather data from
    :type start_timestamp: str

    :param end_timestamp: timestamp to gather data to (but not including)
    :type end_timestamp: str
    '''

    deletions_query = '''SELECT log_namespace, DATE(log_timestamp) AS log_date,
                                log_comment
                         FROM logging
                         WHERE log_type = 'delete'
                         AND log_action = 'delete'
                         AND log_timestamp >= "{start}"
                         AND log_timestamp < "{end}"
                         AND log_namespace IN (0,2,118)'''
    
    ## The data points we'll return
    datapoints = []

    ## Mapping dates to namespaces to data, because we will be processing
    ## every log action.
    data_map = {}

    ## Regular expressions for matching speedy deletions
    csd_re = re.compile('\[\[(WP|Wikipedia):(CSD|Criteria for speedy deletion)#((?:[AGURX])\d+)|\[\[(WP|Wikipedia):((?:[AGURX])\d+)(\||\]\])')
    prod_re = re.compile('\[\[WP:(BLP)?PROD')
    afd_re = re.compile('\[\[(Wikipedia|WP):Articles for deletion/')

    ## Mass deletion of pages, which generally fits into G5, see discussion on
    ## https://meta.wikimedia.org/wiki/Research_talk:Autoconfirmed_article_creation_trial#Effects_on_page_deletion
    nuke_re = re.compile('mass deletion of pages', re.I)

    ## The word "redirect" is unlikely to be used unless an actual redirect is
    ## being deleted. We'll use it for additional filtering of "other",
    ## to ensure that PROD/AfDs don't refer to redirects.
    redirect_re = re.compile('redirect', re.I)
    
    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute(deletions_query.format(start=start_timestamp,
                                                 end=end_timestamp))
        for row in db_cursor:
            log_date = row['log_date']
            log_namespace = row['log_namespace']
            if row['log_comment'] is None:
                log_comment = ''
            else:
                try:
                    log_comment = row['log_comment'].decode('utf-8')
                except UnicodeDecodeError:
                    log_comment = ''

            if not log_date in data_map:
                data_map[log_date] = {}

            if not log_namespace in data_map[log_date]:
                data_map[log_date][log_namespace] = DataPoint(log_date,
                                                              log_namespace)

            datapoint = data_map[log_date][log_namespace]
            csd_match = csd_re.search(log_comment)

            is_nuke = nuke_re.search(log_comment)
            is_redirect = redirect_re.search(log_comment)
            
            if csd_match:
                reason = csd_match.group(3) or csd_match.group(5)
                if reason in datapoint.stats:
                    datapoint.stats[reason] += 1
                elif is_nuke:
                    datapoint.stats['G5'] += 1
                elif not is_redirect:
                    datapoint.stats['other'] += 1
            elif not is_redirect:
                if prod_re.search(log_comment):
                    datapoint.stats['PROD'] += 1
                elif afd_re.search(log_comment):
                    datapoint.stats['AFD'] += 1
                else:
                    datapoint.stats['other'] += 1

    for date in data_map.keys():
        for namespace in data_map[date].keys():
            datapoints.append(data_map[date][namespace])

    return(datapoints)

def gather_historic(db_conn, start_date, end_date=None, step=7):
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
    
    db_conn = db.connect('enwiki.analytics.db.svc.eqiad.wmflabs',
                         'enwiki_p',
                         '~/replica.my.cnf')

    start = dt.date(2017,1, 1)
    end = dt.date(2017,1,8)
    
    # datapoints = gather_data(db_conn, '20150908000000', '201509090000')
    datapoints = gather_historic(db_conn, start, end)
    
    print('got {} data points'.format(len(datapoints)))
    for data in datapoints:
        print('{} : ns={} : G11={}, G13={}, A7={}, A9={}, U1={}, other={}'.format(
            data.dr_date, data.dr_namespace, data.stats['G11'],
            data.stats['G13'], data.stats['A7'], data.stats['A9'],
            data.stats['U1'], data.stats['other']))
        
    return()

if __name__ == "__main__":
    main()

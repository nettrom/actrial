#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script that uses the EventStream to capture page creation events, grabs
article quality predictions from ORES, and stores those predictions in
our database.

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
import signal
import logging
import datetime as dt

import json
from sseclient import SSEClient as EventSource

import db
import MySQLdb # for catching exceptions

from ores.api import Session as ORESSession

class PageCreationStreamer:
    def __init__(self):
        '''
        Instantiate the streamer.
        '''

        self.db_conf = "~/replica.my.cnf"
        
        self.rc_url = "https://stream.wikimedia.org/v2/stream/recentchange"

        self.ores_url = "https://ores.wikimedia.org"
        self.ores_user_agent = "User:Nettrom actrial v1.0"
        self.ores_models = ['draftquality', 'wp10']

        self.local_db_host = "tools.labsdb"
        self.local_db_name = "s53463__actrial_p"
        
        self.wiki = "enwiki"
        self.event_types = set(["new"])
        self.namespaces = [0]

        self.shutdown = False

    def handle_signal(self, signum, stack):
        '''
        Handle incoming signals, specifically SIGUSR1, which we'll use
        to quit gracefully.
        '''

        self.shutdown = True
        return()

    def filter_event(self, event):
        '''
        Check if the event is valid, parseable, and relevant to our setup.
        Returns an empty dictionary if it does not meet the criteria, and
        a populated dictionary if it does.

        :param event: event from the event stream
        :type event: sseclient.Event
        '''

        empty_data = {}
        
        if event.event != 'message':
            return(empty_data)

        try:
            event_data = json.loads(event.data)
        except ValueError:
            return(empty_data)

        if event_data['wiki'] != self.wiki:
            return(empty_data)

        if event_data['type'] not in self.event_types:
            return(empty_data)

        if event_data['namespace'] not in self.namespaces:
            return(empty_data)

        return(event_data)

    def run(self):
        '''
        Grab events from the stream until shutdown.
        '''

        ## SQL query to identify a redirect, disambiguation page, and list
        page_check_query = '''SELECT ap.page_is_redirect,
                                     IFNULL(c1.cl_from, 0) AS page_is_disambig,
                                     IFNULL(c2.cl_from, 0) AS page_is_list
                              FROM page ap
                              LEFT JOIN page tp
                              ON (ap.page_title=tp.page_title
                                  AND tp.page_namespace=1)
                              LEFT JOIN (
                                SELECT cl_from FROM categorylinks
                                WHERE cl_to='All_article_disambiguation_pages')
                                AS c1
                              ON c1.cl_from=ap.page_id
                              LEFT JOIN (
                                SELECT cl_from FROM categorylinks
                                WHERE cl_to REGEXP "^List-Class.*")
                                AS c2
                              ON c2.cl_from=tp.page_id
                              WHERE ap.page_namespace=0
                              AND ap.page_title=%(page_title)s
                              LIMIT 1'''

        ## SQL query to insert predictions for a given revision into
        ## our local database table, formatted so that the two groups
        ## of prediction results are easy to spot.
        insert_query = '''INSERT INTO page_predictions
                          VALUES (%s, %s,
                                  %s, %s, %s, %s, %s,
                                  %s, %s, %s, %s, %s, %s, %s)'''
        
        # Set up a signal handler for SIGUSR1
        signal.signal(signal.SIGUSR1, self.handle_signal);

        # Create the ORES session variable
        ores_session = ORESSession(self.ores_url, self.ores_user_agent)

        ## Connect to the database
        wiki_db_conn = db.connect("{}.labsdb".format(self.wiki),
                                  "{}_p".format(self.wiki),
                                  self.db_conf)
        if not wiki_db_conn:
            logging.error("unable to connect to Wiki database")
            return()

        local_db_conn = db.connect(self.local_db_host,
                                   self.local_db_name,
                                   self.db_conf)
        if not local_db_conn:
            logging.error("unable to connect to tools database")
            return()
        
        logging.info("Running...")
        for event in EventSource(self.rc_url):
            if self.shutdown:
                break

            data = self.filter_event(event)
            if not data:
                continue

            ## Turn the timestamp into a datetime object
            data['timestamp'] = dt.datetime.fromtimestamp(
                data['timestamp'], tz=dt.timezone.utc)
            
            logging.info('{user} created {title}'.format_map(data))

            ## Check that it's not a redirect, not a disambiguation page,
            ## and not a list page.
            page_is_redirect = 0
            page_is_disambig = 0
            page_is_list = 0
            
            try:
                with db.cursor(wiki_db_conn) as db_cursor:
                    db_cursor.execute('SELECT * FROM page LIMIT 1')
            except MySQLdb.OperationalError as e:
                wiki_db_conn = db.connect("{}.labsdb".format(self.wiki),
                                          "{}_p".format(self.wiki),
                                          self.db_conf)
                
            with db.cursor(wiki_db_conn, 'dict') as db_cursor:
                db_cursor.execute(
                    page_check_query,
                    {'page_title': data['title'].replace(" ", "_").encode('utf-8')})
                for row in db_cursor:
                    page_is_redirect = row['page_is_redirect']
                    page_is_disambig = row['page_is_disambig']
                    page_is_list = row['page_is_list']

            if page_is_redirect or page_is_disambig or page_is_list:
                continue

            logging.info("{user} created {title} which is not a redirect, not a disambiguation page, and not a list".format_map(data))

            try:
                with db.cursor(local_db_conn) as db_cursor:
                    db_cursor.execute('SELECT * FROM page_predictions LIMIT 1')
            except MySQLdb.OperationalError as e:
                local_db_conn = db.connect(self.local_db_host,
                                   self.local_db_name,
                                   self.db_conf)

            with db.cursor(local_db_conn, 'dict') as db_cursor:
                ## Grab the wp10 and draftquality predictions from ORES
                ## for the given revision and store it in the database:
                for prediction in ores_session.score(data['wiki'],
                                                     self.ores_models,
                                                     [data['revision']['new']]):
                    try:
                        draft_res = prediction['draftquality']['score']
                        wp10_res = prediction['wp10']['score']
                    except KeyError:
                        logging.warning('unexpected ORES result')
                        continue

                    try:
                        db_cursor.execute(
                            insert_query, (data['revision']['new'],
                                           data['timestamp'],
                                           draft_res['prediction'],
                                           draft_res['probability']['spam'],
                                           draft_res['probability']['vandalism'],
                                           draft_res['probability']['attack'],
                                           draft_res['probability']['OK'],
                                           wp10_res['prediction'],
                                           wp10_res['probability']['Stub'],
                                           wp10_res['probability']['Start'],
                                           wp10_res['probability']['C'],
                                           wp10_res['probability']['B'],
                                           wp10_res['probability']['GA'],
                                           wp10_res['probability']['FA']))
                        local_db_conn.commit()
                        print("inserted {}, created by {}, draftquality prediction {}, wp10 prediction {}".format(data['title'], data['user'], draft_res['prediction'], wp10_res['prediction']))
                    except Exception as e:
                        print(e)

        ## ok, done
        local_db_conn.close()
        wiki_db_conn.close()
        return()
            
def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to pull article creation events from the event stream and store quality predictions in our database"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)
    
    streamer = PageCreationStreamer()
    streamer.run()
            
if __name__ == "__main__":
    main()
    

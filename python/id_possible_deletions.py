#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to parse one part of the historical dataset of article creations from
the Data Lake and figure out if the articles were deleted.

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

import bz2
import logging
import datetime as dt

from more_itertools import chunked

import db

def populate_table(input_filename, slice_size=100, commit_size=1000):
    '''
    Process the given dataset, identify if an article was deleted and
    if it was, insert a timestamp of that into a table in staging database.

    :param input_filename: path to the input file with data
    :type input_filename: str

    :param slice_size: number of articles to process at a time when running
                       batch-style SQL queries
    :type slice_size: int

    :param commit_size: number of inserts to make before issuing a commit
    :type commit_size: int
    '''

    ## Unlike for "id_deletions.py", where we found that all articles had
    ## been deleted (meaning all their data was in the archive table),
    ## this dataset contains "possible deletions". We will therefore try to
    ## see if the article was deleted, primarily using the page ID as an
    ## authoritative identifier. In this case all pages have titles, so
    ## we don't need that intermediary step.

    db_name = "staging"
    table_name = "nettrom_deletion_data"
    temp_table_name = "nettrom_temp_table"

    create_temp_query = """CREATE TEMPORARY TABLE {}.{} (
       page_id INTEGER UNSIGNED NOT NULL,
       page_title VARCHAR(255) BINARY NOT NULL DEFAULT "",
       creation_timestamp DATETIME NOT NULL)""".format(db_name, temp_table_name)

    insert_temp_query = """INSERT INTO {}.{}
                           (page_id, page_title, creation_timestamp)
                           VALUES (%s, %s, %s)""".format(
                               db_name, temp_table_name)

    insert_main_query = """INSERT INTO {}.{} (page_id, deletion_timestamp)
                           VALUES (%s, %s)""".format(db_name, table_name)

    ## Note: STRAIGHT_JOIN forces the join order, results in the query
    ## using the much faster "log_page_id_time" index for the query, and
    ## besides, we know the temp table holds a small number of rows.
    logging_id_query = """SELECT log_page, MIN(log_timestamp) AS deletion_time
        FROM {}.{} t
        STRAIGHT_JOIN logging l
        ON l.log_page=t.page_id
        WHERE log_timestamp > DATE_FORMAT(t.creation_timestamp, "%Y%m%d%H%i%S")
        AND log_type='delete'
        AND log_action='delete'
        GROUP BY log_page""".format(db_name, temp_table_name)

    ## Note: Using page_id here because that's our authoritative identifier,
    ## the page ID in the logging table (log_page) is _not_.
    ## Note: this assumes the page was in namespace 0 when it was deleted,
    ## 
    
    ## (and again using a straight join to force the join order)
    logging_title_query = """SELECT page_id, MIN(log_timestamp) AS deletion_time
        FROM {}.{} t
        STRAIGHT_JOIN logging l
        ON l.log_title=t.page_title
        WHERE log_timestamp > DATE_FORMAT(t.creation_timestamp, "%Y%m%d%H%i%S")
        AND log_namespace = 0
        AND log_type='delete'
        AND log_action='delete'
        AND t.page_title != ''
        AND (log_page IS NULL
             OR log_page = 0)
        GROUP BY page_id""".format(db_name, temp_table_name)

    ### Query to join the temp table with the archive table
    ### and update it with all found page titles.
    page_title_query = """UPDATE {}.{} t
                          JOIN enwiki.archive ar
                          ON (t.page_id=ar.ar_page_id
                              AND DATE_FORMAT(t.creation_timestamp,
                                              "%Y%m%d%H%i%S")=ar.ar_timestamp
                              AND ar.ar_namespace=0)
                          SET t.page_title=ar.ar_title""".format(
                              db_name, temp_table_name)

    ## Delete a given page from the temp table
    delete_id_query = """DELETE FROM {}.{}
                         WHERE page_id = %s""".format(db_name, temp_table_name)

    ## Delete all pages from the temp table
    delete_temp_query = "DELETE FROM {}.{}".format(db_name, temp_table_name)
    
    ## Query to count the number of pages in the temp table
    temp_table_count_query = """SELECT count(*) AS num_pages
                                FROM {}.{}""".format(db_name, temp_table_name)

    ## Query to insert all pages in the temp table into the main table
    ## with NULL as their deletion timestamp
    insert_remaining = """INSERT INTO {d}.{m}
                          SELECT page_id, NULL AS deletion_timestamp
                          FROM {d}.{t}""".format(d=db_name, m=table_name,
                                                 t=temp_table_name)
    
    ## Query to find all existing page IDs in the main table
    page_id_query = """SELECT page_id FROM {}.{}""".format(db_name, table_name)

    db_conn = db.connect('analytics-store.eqiad.wmnet',
                         'enwiki', '~/.my.research.cnf')
    if not db_conn:
        logging.error("Unable to connect to database")
        return()

    with db.cursor(db_conn) as db_cursor:
        db_cursor.execute(create_temp_query)
        db_conn.commit()

    id_event_map = {}
    with bz2.BZ2File(input_filename, 'r') as infile:
        infile.readline() # skip header

        with db.cursor(db_conn) as db_cursor:
            for line in infile:
                (event_timestamp, page_id) = line.strip().split('\t')
                page_id = int(page_id)
                event_timestamp = dt.datetime.strptime(event_timestamp,
                                                       "%Y-%m-%d %H:%M:%S")
                id_event_map[page_id] = event_timestamp
                   
    logging.info("found {} pages in the dataset".format(len(id_event_map)))

    ## Get existing IDs stored in the database, delete those so we skip them
    with db.cursor(db_conn, 'dict') as db_cursor:
        db_cursor.execute(page_id_query)
        for row in db_cursor:
            page_id = row['page_id']
            if page_id in id_event_map:
                del(id_event_map[row['page_id']])

    logging.info("found {} pages to get deletion data for".format(len(id_event_map)))
    
    ## Iterate over IDs
    i = 0
    n_inserted = 0
    with db.cursor(db_conn, 'dict') as db_cursor:
        for subset in chunked(id_event_map.items(), slice_size):
            ## Add the page IDs and creation timestamps to the temp table
            db_cursor.executemany(insert_temp_query,
                                  subset)
            db_conn.commit()

            ## - Use the temp table to find all deletes using page ID
            db_cursor.execute(logging_id_query)
            for row in db_cursor:
                page_id = row['log_page']
                del_timestamp = dt.datetime.strptime(
                    row['deletion_time'].decode('utf-8'), "%Y%m%d%H%M%S")
                ## - Store in the main table, delete from the temp table
                db_cursor.execute(insert_main_query,
                                  (page_id, del_timestamp))
                db_cursor.execute(delete_id_query, (page_id,))

            ## - If the temp table is empty, commit and iterate
            n_remaining = 0
            db_cursor.execute(temp_table_count_query)
            for row in db_cursor:
                n_remaining = row['num_pages']

            if n_remaining == 0:
                db_conn.commit()
                continue

            logging.info('have {} remaining pages to process'.format(n_remaining))
            
            ## - Update the temp table with all available page titles
            db_cursor.execute(page_title_query)
            logging.info('got page titles for {} pages'.format(db_cursor.rowcount))

            ## - Use the temp table to find all deletes using page titles
            db_cursor.execute(logging_title_query)
            for row in db_cursor:
                page_id = row['page_id']
                del_timestamp = dt.datetime.strptime(
                    row['deletion_time'].decode('utf-8'), "%Y%m%d%H%M%S")
                ## - Store in the main table, delete from the temp table
                db_cursor.execute(insert_main_query,
                                  (page_id, del_timestamp))
                db_cursor.execute(delete_id_query, (page_id,))
            
            ## Invariant: We know that the current dataset contains _only_
            ## deleted pages. Therefore, we will insert all remaining pages
            ## from the temp table into the main table, but with NULL as
            ## their deletion date.
            db_cursor.execute(insert_remaining)
            logging.info('inserted {} remaining pages into the main table'.format(db_cursor.rowcount))

            ## Delete the remaining pages in the temp table
            db_cursor.execute(delete_temp_query)

            ## Commit and iterate
            db_conn.commit()

            i += 1
            logging.info('processed {} pages'.format(i*slice_size))

    ## ok, done
    db_conn.close()
    return()
    
    # ok, done
    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to identify article deletion for NULL-titled pages"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    cli_parser.add_argument('input_filename', type=str,
                            help='path to the input dataset file')

    cli_parser.add_argument('slice_size', type=int,
                            help='number of pages we process at a time (batch size)')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    populate_table(args.input_filename, args.slice_size)
    
    return()

if __name__ == "__main__":
    main()

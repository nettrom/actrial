#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to parse the historical dataset of article creations from the Data Lake
and insert it into a database we can query to get historical data on whether
users created articles.

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

import db

def populate_table(table_name, input_filename, batch_size=1000):
    '''
    Populate the given table with the dataset of article creation statistics.

    :param table_name: name of the database table we are populating
    :type table_name: str

    :param input_filename: path to the input file with data
    :type input_filename: str

    :param batch_size: number of inserts to make before issuing a commit
    :type batch_size: int
    '''

    insert_query = '''INSERT INTO {table}
                      (ac_rev_id, ac_timestamp, ac_user_id)
                      VALUES (%s, %s, %s)'''.format(table=table_name)

    db_conn = db.connect('tools.labsdb', 's53463__actrial_p', '~/replica.my.cnf')
    if not db_conn:
        logging.error('unable to connect to database server')
        return()

    with bz2.open(input_filename, 'rt') as infile:
        infile.readline() # skip header

        i = 0
        
        with db.cursor(db_conn) as db_cursor:
            for line in infile:
                (rev_timestamp, rev_id, rev_user_id) = line.strip().split('\t')
                rev_timestamp = dt.datetime.strptime(rev_timestamp,
                                                     '%Y-%m-%d %H:%M:%S.0')
                rev_id = int(rev_id)
                rev_user_id = int(rev_user_id)
                
                db_cursor.execute(insert_query,
                                  (rev_id, rev_timestamp, rev_user_id))
                i += 1
                if i % batch_size == 0:
                    logging.info('inserted {} entries, committing'.format(i))
                    db_conn.commit()

        # commit any outstanding inserts
        db_conn.commit()
        db_conn.close()

    logging.info('completed inserting {} entries'.format(i))
        
    # ok, done
    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to populate a database with article creation statistics to enable querying"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    cli_parser.add_argument('table_name', type=str,
                            help='name of the table we are populating')

    cli_parser.add_argument('input_filename', type=str,
                            help='path to the input dataset file')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    populate_table(args.table_name, args.input_filename)
    
    return()

if __name__ == "__main__":
    main()

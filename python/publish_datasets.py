#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to create/update published datasets based on our database.

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

import os
import re
import bz2
import yaml
import logging

import db

## Need: how to handle updating an existing dataset?
## Should I just specify that the first column is always a date
## (named date) or timestamp (named date_time)?

## FIXME: the backlog dataset and newaccount datasets are both fairly small,
## so let's just recreate those every day. The account stats dataset needs
## some trickery, so we'll append to that.

def publish_small_datasets(config, db_conn):
    '''
    Based on the given configuration of data to write, and the given
    database connection, run the queries and export the data that is
    labelled as "small".

    :param config: data export definitions (list of key/value pair dicts)
    :type config: list

    :param db_conn: connection to the database
    :type db_conn: MySQL.Connection
    '''

    # for stuff in config...
    for table_def in config:
        ## Only publish small datasets
        if not "is_small" in table_def:
            continue

        print("processing {}".format(table_def['output_file']))
        
        ## Open output file, 
        output_filename = os.path.expanduser(table_def['output_file'])
        if re.match(".*\.bz2$", output_filename):
            outfile = bz2.open(output_filename, 'wt')
        else:
            outfile = open(output_filename, 'w')

        # write header
        outfile.write('\t'.join(table_def['columns']))
        outfile.write('\n')

        #   execute query and write output
        with db.cursor(db_conn, 'dict') as db_cursor:
            db_cursor.execute(table_def['sql_query'])
            for row in db_cursor:
                output = []
                for col in table_def['columns']:
                    output.append(str(row[col]))
                outfile.write('{}\n'.format('\t'.join(output)))

        outfile.close()

    ## ok, done
    return()

def update_datasets(config_file, do_recreate=False):
    '''
    Update all published datasets with the most recent data.

    :param config_file: path to the YAML configuration file with tables
                        and output paths
    :type config_file: str

    :param do_recreate: should we delete the existing dataset and recreate
                        them from scratch?
    :type do_recreate: bool
    '''

    db_config = {'dbconf': '~/replica.my.cnf',
                 'dbname': 's53463__actrial_p',
                 'hostname': 'tools.db.svc.eqiad.wmflabs'}
    
    with open(config_file) as infile:
        config = yaml.load(infile)

    # connect to database
    db_conn = db.connect(db_config['hostname'],
                         db_config['dbname'],
                         db_config['dbconf'])
    
    # recreate all the small datasets
    publish_small_datasets(config, db_conn)
    
    # disconnect
    db_conn.close()

    ## ok, done
    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to update our published datasets"
    )

    ## Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    ## configuration file
    cli_parser.add_argument('config_filename', type=str,
                            help='path to the YAML configuration file')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    update_datasets(args.config_filename)
        
    return()
    
if __name__ == "__main__":
    main()

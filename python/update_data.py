#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to run a daily update of those of our datasets that require some
date math to work properly.

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

import gather_historic as gh

def update_data():
    '''
    Update our datasets.
    '''

    ## List defining which updates we'll run and which we skip
    updates = [False, # everything
               True, # counts of new accounts
               True, # registrations
               True, # activity stats in first 30 days
               True, # autoconfirmed status and time-to-autoconfirmed
               True, # surviving new editors (1st and 5th week)
               True, # moves into main
               True, # patroller data
               False, # article survival
               True] # deletion reasons
    
    ## Get today's date
    today = dt.date.today()
    
    ## Calculate yesterday's date
    yesterday = today - dt.timedelta(days=1)
    
    ## Update stuff
    gh.insert_historic(yesterday, today, *updates)

    ## ok, done
    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to make a daily update of our datasets"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    args = cli_parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    update_data()
        
    return()
    
if __name__ == "__main__":
    main()

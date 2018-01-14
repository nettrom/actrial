#!/usr/env/python
# -*- coding: utf-8 -*-
'''
Script to query ORES for information about the draft quality model
and store that information in a small TSV dataset.

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

import logging
import requests

from time import sleep

def grab_dataset(output_filename, recall_start=50,
                 recall_end=99, step=1):
    '''
    Make requests to ORES for information about the draft quality model
    and the "OK" class based on !recall at a given setting. Writes out
    information to the given output filename as a TSV.

    Note that the start, end, and step parameters are percentages and will
    be divided by 100 before being fed to ORES.
    '''

    ores_url = "https://ores.wikimedia.org/v3/scores/enwiki/?models=draftquality&model_info=statistics.thresholds.OK.%22maximum%20!precision%20@%20!recall%20%3E=%20{not_recall}%22"

    with open(output_filename, 'w', encoding='utf-8') as outfile:
        outfile.write('n_recall_given\tn_f1\tn_precision\tn_recall\taccuracy\tf1\tfilter_rate\tfpr\tmatch_rate\tprecision\trecall\tthreshold\n')
        
    
        for n_recall in range(recall_start, recall_end+1, step):
            res = requests.get(ores_url.format(not_recall=n_recall/100))

            if res.status_code != 200:
                logging.error('ORES request returned {}'.format(r.status_code))
                return()

            ores_data = res.json()
            try:
                model_data = ores_data['enwiki']['models']['draftquality']['statistics']['thresholds']['OK'][0]
            except KeyError:
                logging.error('ORES returned unknown data format')
                logging.error(ores_data)
                return()

            model_data['n_recall_given'] = n_recall/100
            model_data['n_f1'] = model_data['!f1']
            model_data['n_precision'] = model_data['!precision']
            model_data['n_recall'] = model_data['!recall']
            
            outfile.write('{n_recall_given}\t{n_f1}\t{n_precision}\t{n_recall}\t{accuracy}\t{f1}\t{filter_rate}\t{fpr}\t{match_rate}\t{precision}\t{recall}\t{threshold}\n'.format(**model_data))

            sleep(0.1)

    return()

def main():
    import argparse

    cli_parser = argparse.ArgumentParser(
        description="script to grab data about the draft quality model from ORES"
    )

    # Verbosity option
    cli_parser.add_argument('-v', '--verbose', action='store_true',
                            help='write informational output')

    cli_parser.add_argument('output_filename', type=str,
                            help='path to the output file')

    args = cli_parser.parse_args()
        
    if args.verbose:
        logging.basicConfig(level=logging.INFO)

    grab_dataset(args.output_filename)

if __name__ == "__main__":
    main()

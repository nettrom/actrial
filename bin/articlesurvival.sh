#!/bin/bash

# Name the job "artsurv".
#$ -N artsurv

# Tell the server we'll be running for a maximum of 6 hours
#$ -l h_rt=6:00:00

# Store output in a different place.
#$ -o $HOME/projects/actrial/logs/article_survival.out

# Store errors in a different place.
#$ -e $HOME/projects/actrial/logs/article_survival.err

# Ask for a gig of memory
#$ -l h_vmem=1024M

# Set the Debian version to trusty
#$ -l release=trusty

## Launch the script with the two supplied dates as parameters
$HOME/venv/py3/bin/python $HOME/projects/actrial/python/gather_historic.py \
			  --article_survival $1 $2

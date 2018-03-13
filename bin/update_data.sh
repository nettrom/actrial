#!/bin/bash

# Name the job "updata".
#$ -N updata

# Tell the server we'll be running for a maximum of 3 hours
#$ -l h_rt=03:00:00

# Store output in a different place.
#$ -o $HOME/projects/actrial/logs/updata.out

# Store errors in a different place.
#$ -e $HOME/projects/actrial/logs/updata.err

# Ask for a gig of memory
#$ -l h_vmem=1024M

# Set the Debian version to trusty
#$ -l release=trusty

## Update our datasets
$HOME/venv/py3/bin/python $HOME/projects/actrial/python/update_data.py

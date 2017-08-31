#!/bin/bash

# Name the job "nppsnap".
#$ -N nppsnap

# Tell the server we'll be running for a maximum of 5 minutes
#$ -l h_rt=00:05:00

# Store output in a different place.
#$ -o $HOME/projects/actrial/logs/npp_queue_snapshot.out

# Store errors in a different place.
#$ -e $HOME/projects/actrial/logs/npp_queue_snapshot.err

# Ask for a gig of memory
#$ -l h_vmem=1024M

# Set the Debian version to trusty
#$ -l release=trusty

## Launch the script (it takes no parameters)
$HOME/venv/py3/bin/python $HOME/projects/actrial/python/npp_queue_snapshot.py

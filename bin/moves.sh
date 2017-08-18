#!/bin/bash

# Name the job "moves".
#$ -N moves

# Tell the server we'll be running for a maximum of 6.5 hours
#$ -l h_rt=6:30:00

# Store output in a different place.
#$ -o $HOME/projects/actrial/logs/moves.out

# Store errors in a different place.
#$ -e $HOME/projects/actrial/logs/moves.err

# Ask for a gig of memory
#$ -l h_vmem=1024M

# Set the Debian version to trusty
#$ -l release=trusty

## Launch the script with the two supplied dates as parameters
$HOME/venv/py3/bin/python $HOME/projects/actrial/python/gather_historic.py \
			  --moves $1 $2

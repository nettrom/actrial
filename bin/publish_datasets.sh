#!/bin/bash

# Name the job "pubdata".
#$ -N pubdata

# Tell the server we'll be running for a maximum of 30 minutes
#$ -l h_rt=00:30:00

# Store output in a different place.
#$ -o $HOME/projects/actrial/logs/pubdata.out

# Store errors in a different place.
#$ -e $HOME/projects/actrial/logs/pubdata.err

# Ask for a gig of memory
#$ -l h_vmem=1024M

# Set the Debian version to trusty
#$ -l release=trusty

## Launch the script with the YAML configuration file as the parameter
$HOME/venv/py3/bin/python $HOME/projects/actrial/python/publish_datasets.py \
			  $HOME/projects/actrial/datasets/export_config.yaml

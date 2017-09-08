#!/bin/bash

## Run a continuous job called "streampred", asking for 1G of memory
## because Python is a memory hog on startup.

jstart -N streampred -mem 1G \
     $HOME/venv/py3/bin/python \
     $HOME/projects/actrial/python/stream_creation_events.py

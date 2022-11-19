#!/bin/bash
DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

if [ -z "$VIRTUAL_ENV" ]; then
    (cd $DIR; python3 -m venv .venv)
    echo "Using virtualenv $DIR/.venv/"
    source $DIR/.venv/bin/activate
fi

(cd $DIR; pip install -U -r requirements.txt)

(cd $DIR; make latexpdf)
(cd $DIR; make info)
(cd $DIR; make html)

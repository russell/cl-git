#!/bin/bash
DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

(cd $DIR; pip install -U -r requirements.txt)

(cd $DIR; make latexpdf)
(cd $DIR; make info)
(cd $DIR; make html)

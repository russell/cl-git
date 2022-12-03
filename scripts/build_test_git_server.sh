#!/bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

cd $DIR

if [ ! -e id_gituser_rsa ]; then
    ssh-keygen -t rsa -b 4096 -C "cl-git@example.com" -f id_gituser_rsa -N ""
fi


docker build -t cl-git-test:latest -f Dockerfile.testserver .


# docker run --rm -it --name cl-git-test -p 2222:22 cl-git-test:latest

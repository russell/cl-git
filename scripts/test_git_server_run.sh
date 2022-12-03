#!/bin/bash
DIR="$( cd "$( dirname "$0" )" && pwd )"

cd $DIR


./build_test_git_server.sh
trap 'echo -e "\nCtrl+c Detected."; docker stop cl-git-test' SIGINT
docker run -d --rm --name cl-git-test -p 9419:9419 -p 2222:22 -v ~/tmp/hooks:/tmp/hooks cl-git-test:latest
docker logs --follow cl-git-test
docker rm cl-git-test


# git remote add origin ssh://git@localhost:2222/~/git_test
# git push -u origin master

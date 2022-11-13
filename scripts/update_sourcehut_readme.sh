#!/bin/bash

repo_id=217262
readme=README.html

emacs -Q --batch --eval "(require 'org)" README.org --funcall org-html-export-to-html

jq -sR '{
    "query": "mutation UpdateRepo($id: Int!, $readme: String!) {
      updateRepository(id: $id, input: { readme: $readme }) { id }
    }", "variables": {
      "id": '$repo_id',
      "readme": .
    } }' < $readme \
        | curl --oauth2-bearer $SOURCEHUT_TOKEN \
               -H "Content-Type: application/json" \
               -d@- https://git.sr.ht/query

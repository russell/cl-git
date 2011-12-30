Commits
=======

.. cl:package:: cl-git


Creating Commits
----------------

.. cl:function:: (defun git-commit-create (oid message &key (update-ref "HEAD") (author nil) (committer nil) (parents nil)))


Walking Commits
---------------

.. cl:macro:: (defmacro with-git-revisions ((commit &key sha head) &body body))


Reading Commit Details
----------------------

.. cl:function:: (defun git-commit-message (commit))

.. cl:function:: (defun git-commit-author (commit))

.. cl:function:: (defun git-commit-committer (commit))

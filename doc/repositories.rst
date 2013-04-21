Repositories
============

.. cl:package:: cl-git

.. cl:type:: repository

.. cl:variable:: *git-repository*

Creating
--------

.. cl:generic:: git-init

   .. code-block:: common-lisp

      CL-GIT> (git-init :repository #p"/tmp/test-repo/")
      #<REPOSITORY 644AF0 {100611F003}>

Using
-----

.. cl:generic:: git-open


.. cl:macro:: with-repository

   :param path: the path to the git repository.
   :param body: the body of the macro.

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
                 (git-reference-listall))
      ("refs/remotes/origin/master" "refs/remotes/origin/verrazano"
      "refs/heads/master" "refs/heads/verrazano")

Status
------

.. cl:function:: git-status

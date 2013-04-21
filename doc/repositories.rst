Repositories
============

.. cl:package:: cl-git


.. cl:type:: repository

.. cl:variable:: *git-repository*

Creating
--------

.. cl:generic:: git-init

   :param path: the path to the git repository.
   :param bare: if truthful, then create a bare repository.

   .. code-block:: common-lisp

      CL-GIT> (git-init #p"/tmp/test-repo/")
      #P"/tmp/test-repo/"

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

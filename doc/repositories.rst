Repositories
============

.. cl:package:: cl-git

.. cl:type:: repository

.. cl:variable:: *git-repository*

.. cl:method:: git-config repository

.. cl:method:: git-index repository

.. cl:method:: git-odb repository

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


With-repository is a convenience macro that binds a repository to
*GIT-REPOSITORY*.  It wraps GIT-OPEN and specializes it on
:REPOSITORY.

.. code-block:: common-lisp

   CL-GIT> (with-repository (#p"/home/russell/projects/cl-git/")
                 (git-list :reference))
   ("refs/remotes/origin/master" "refs/remotes/origin/verrazano"
   "refs/heads/master" "refs/heads/verrazano")


Status
------

.. cl:function:: git-status


.. code-block:: common-lisp

   CL-GIT> (with-repository (#p"/home/russell/projects/lisp/cl-git/")
              (git-status))

   (("src/status.lisp" :CURRENT :WORKTREE-MODIFIED)
    ("src/package.lisp" :CURRENT :WORKTREE-MODIFIED)
    ("fabfile.pyc" :CURRENT :IGNORED)
    ("doc/repositories.rst" :CURRENT :WORKTREE-MODIFIED)
    ("doc/cl-git.html" :CURRENT :WORKTREE-NEW)
    ("doc/.installed.cfg" :CURRENT :IGNORED))

.. cl:method:: git-head repository

.. cl:function:: git-head-detached

.. cl:method:: git-path repository

.. cl:method:: git-workdir repository

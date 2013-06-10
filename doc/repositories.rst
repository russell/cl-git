Repositories
============

.. cl:package:: cl-git

.. cl:type:: repository

Creating
--------

.. cl:generic:: init-repository

   .. code-block:: common-lisp-repl

      GIT> (init-repository #p"/tmp/test-repo/")
      #<REPOSITORY 7FFFE8006800 {1005F3CE43}>


.. cl:generic:: empty-p repository

   .. code-block:: common-lisp-repl

      GIT> (empty-p (open-repository #p"/tmp/test-repo/"))
      T
       
Bare
~~~~

Bare repositories can be created by passing a truthful value to the
key argument BARE when initialising a repository.

.. code-block:: common-lisp-repl

   GIT> (init-repository #p"/tmp/test-bare/" :bare t)
   #<REPOSITORY 7FFFE8008CD0 {10062DE723}>

Whether an existing repository is bare can be determined using the
bare-p method.

.. cl:generic:: bare-p repository
                                  
   .. code-block:: common-lisp-repl

      GIT> (bare-p (open-repository #p"/tmp/test-bare/"))
      T

Using
-----

.. cl:generic:: open-repository

   .. code-block:: common-lisp-repl

      GIT> (open-repository #p"/home/russell/projects/ecl/")
      #<REPOSITORY 7FFFE8003E00 {1004CCDBA3}>
      GIT> (open-repository "/home/russell/projects/ecl/")
      #<REPOSITORY 7FFFE8004000 {1004D617F3}>


.. cl:macro:: with-repository

   :param pathname-or-string: the location of the repository.
   :param path: the path to the git repository.
   :param body: the body of the macro.


   .. code-block:: common-lisp-repl

      CL-GIT> (with-repository (repository #p"/home/russell/projects/ecl/")
                (prin1 repository) 
                nil)
      #<REPOSITORY 7FFFE8003E00 {1003880DA3}>
      NIL


Head
----

.. cl:generic:: repository-head repository

.. cl:generic:: head-detached-p repository

.. cl:generic:: head-orphaned-p repository


Path
----

.. cl:generic:: repository-path repository

.. cl:generic:: repository-workdir repository


Index
-----

.. cl:method:: index repository


ODB
---

.. cl:method:: open-odb repository

Status
------

.. cl:function:: repository-status


.. code-block:: common-lisp

   CL-GIT> (with-repository (#p"/home/russell/projects/lisp/cl-git/")
              (git-status))

   (("src/status.lisp" :CURRENT :WORKTREE-MODIFIED)
    ("src/package.lisp" :CURRENT :WORKTREE-MODIFIED)
    ("fabfile.pyc" :CURRENT :IGNORED)
    ("doc/repositories.rst" :CURRENT :WORKTREE-MODIFIED)
    ("doc/cl-git.html" :CURRENT :WORKTREE-NEW)
    ("doc/.installed.cfg" :CURRENT :IGNORED))


Configuration
-------------

:doc:`Configuration </config>` details of a particular repository can be done with
the :cl:symbol:`~GIT-CONFIG` method.

.. cl:method:: git-config repository


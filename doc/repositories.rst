Repositories
============

.. cl:package:: cl-git

.. cl:type:: repository

.. cl:variable:: *git-repository*

.. cl:method:: index repository

.. cl:method:: open-odb repository

Creating
--------

.. cl:generic:: init-repository

   .. code-block:: common-lisp-repl

      GIT> (init-repository #p"/tmp/test-repo/")
      #<REPOSITORY 7FFFE8006800 {1005F3CE43}>

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

.. cl:method:: repository-head repository

.. cl:method:: head-detached-p repository

.. cl:method:: head-orphaned-p repository

.. cl:method:: bare-p repository
                                  
.. cl:method:: empty-p repository

.. cl:method:: repository-path repository

.. cl:method:: repository-workdir repository

.. cl:method:: git-config repository


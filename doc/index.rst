cl-git
======

.. contents::

About
-----

cl-git is available under an `LLGPL`_ license.

It is actively developed, but not an ambitious project whose aim is to
provide a simple interface for manipulating git repositories from
lisp.  This library uses the `CFFI` to create a lisp wrapper around
the libgit2 C library.

The following configuration on Linux (Debian Sid) is known to work:

* SBCL 1.0.56 (sbcl-1.0.56.55-b06f72f-linux-x86)
* libgit2 0.16.0


.. _LLGPL: http://opensource.franz.com/preamble.html
.. _libgit2: http://libgit2.github.com/

Download
--------

Since there is currently no released version, the source can be
downloaded from `github`_.

.. _github: https://github.com/russell/cl-git/

Repositories
------------

.. cl:package:: cl-git

.. cl:macro:: with-git-repository

   :param path: the path to the git repository.
   :param body: the body of the macro.

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
			     (git-reference-listall))
      ("refs/remotes/origin/master" "refs/remotes/origin/verrazano"
      "refs/heads/master" "refs/heads/verrazano")

.. cl:function:: ensure-repository-exist

   :param path: the path to the git repository.
   :param bare: if truthful, then create a bare repository.

   .. code-block:: common-lisp

      CL-GIT> (ensure-git-repository-exist #p"/tmp/test-repo/")
      #P"/tmp/test-repo/"


References
----------

.. cl:function:: git-reference-listall

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
			     (git-reference-listall))
      ("refs/remotes/origin/master" "refs/remotes/origin/verrazano"
      "refs/heads/master" "refs/heads/verrazano")


Manipulating the Index
----------------------

.. cl:package:: cl-git

.. cl:macro:: with-repository-index

   :param body: the body of the macro.

.. cl:generic:: git-add

   :param path: the relative path of a file to be added to the repository.

.. cl:generic:: git-clear

.. cl:generic:: git-write

.. cl:function:: git-oid-from-index


Commits
-------

.. cl:package:: cl-git


Creating Commits
~~~~~~~~~~~~~~~~

.. cl:function:: git-commit-create


Walking Commits
~~~~~~~~~~~~~~~

.. cl:macro:: with-git-revisions

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
                (with-git-revisions (commit :sha "69fec1d5938a0c1c8c14a3a120936aa8937af163")
                  (princ (git-commit-message commit))))
      added git str to oid
      added some lowlevel methods for revtree walking
      added error condition strings
      added repository open and list all refs
      initial commit
      NIL

Reading Commit Details
~~~~~~~~~~~~~~~~~~~~~~

.. cl:macro:: bind-git-commits

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/txirods/")
                (with-git-commits ((commit :sha "46153869f3652de5dbb6ddd598c58445383dcba2"))
                  (princ (git-commit-message commit))))
      added better logging messages
      "added better logging messages
      "

.. cl:generic:: git-message

.. cl:generic:: git-author

.. cl:generic:: git-committer


Signatures
----------

.. cl:package:: cl-git

.. cl:function:: git-signature-create


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

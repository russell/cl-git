cl-git
======

cl-git is available under an `LLGPL`_ license.

It is actively developed, but not an ambitious project whose aim is to
provide a simple interface for manipulating git repositories from
lisp.  This library uses the `CFFI` to create a lisp wrapper around the libgit2 C library.


.. _LLGPL: http://opensource.franz.com/preamble.html
.. _libgit2: http://libgit2.github.com/

.. contents::


Repositories
------------

.. cl:package:: cl-git

.. cl:variable:: *git-repository*

.. cl:macro:: with-git-repository

   :param path: the path to the git repository
   :param body: the body of the macro

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
			     (git-reference-listall))
      ("refs/remotes/origin/master" "refs/remotes/origin/verrazano"
      "refs/heads/master" "refs/heads/verrazano")

.. cl:function:: ensure-git-repository-exist

   :param path: the path to the git repository
   :param bare: if truthful, then create a bare repository.

   .. code-block:: common-lisp

      CL-GIT> (ensure-git-repository-exist #p"/tmp/test-repo/")
      #P"/tmp/test-repo/"


References
----------

.. cl:function:: git-reference-listall


Manipulating the Index
----------------------

.. cl:package:: cl-git

.. cl:variable:: *git-repository-index*

.. cl:macro:: with-git-repository-index

   :param body: the body of the macro

.. cl:function:: git-index-add

.. cl:function:: git-index-clear

.. cl:function:: git-index-write

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


Reading Commit Details
~~~~~~~~~~~~~~~~~~~~~~

.. cl:function:: git-commit-message

.. cl:function:: git-commit-author

.. cl:function:: git-commit-committer


Signatures
----------

.. cl:package:: cl-git

.. cl:function:: git-signature-create


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

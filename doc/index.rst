cl-git
======

About
-----

This library wraps the C library libgit2. As the name implies it deals
with git and allows you to create/examine and alter git repositories
programmatically.

Some knowledge if git is required to make sense of all the
concepts. Also this being a wrapper around the libgit2 library, the
documentation libgit2 http://libgit2.github.com/libgit2/#HEAD is helpful.

The following configuration on Linux (Debian Sid) is known to work:

* SBCL
* libgit2 0.18

CL-GIT's API is modelled off the underlying Libgit2 interface.  The
CL-GIT project takes advantage of it and provides a object oriented
interface to libgit2.  Git concepts such as commits, tags, blobs etc
are instances of corresponding classes in CL-GIT.

.. _LLGPL: http://opensource.franz.com/preamble.html
.. _libgit2: http://libgit2.github.com/


Download
--------

Since there is currently no released version, the source is available
on `github`_.

.. _github: https://github.com/russell/cl-git/

Installation
------------

You'll need a recent version of the libgit2 library, which
can from http://github.com/libgit2/libgit2.

cl-git package should be added to the systems load path so that ASDF
can find it.

The dependencies for cl-git itself are: `cffi`_, `local-time`_,
`cl-fad`_, `trivial-garbage`_, `anaphora`_ and `alexandria`_. The
tests in package cl-git-tests, need in addition to cl-git the
following packages: `FiveAM`_ and `inferior-shell`_.

.. _cffi: http://common-lisp.net/project/cffi/
.. _local-time: http://common-lisp.net/project/local-time/
.. _cl-fad: http://weitz.de/cl-fad/
.. _trivial-garbage: http://common-lisp.net/project/trivial-garbage/
.. _alexandria: http://common-lisp.net/project/alexandria/
.. _anaphora: http://common-lisp.net/project/anaphora/
.. _FiveAM: https://github.com/cl-fiveam/fiveam
.. _inferior-shell: http://www.cliki.net/inferior-shell

Usage
-----

.. toctree::
   :maxdepth: 4
   :numbered:

   repositories
   objects
   references
   tag
   commits
   tree
   blob
   indexes
   remote
   config
   odb
   internals


Contributing
------------

.. toctree::
   :maxdepth: 4

   developing


Indices and tables
------------------

* :ref:`genindex`
* :ref:`search`

.. include:: ../CHANGELOG

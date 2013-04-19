cl-git
======

About
-----

This library wraps the C library libgit2. As the name implies it deals
with git and allows you to create/examine and alter git repositories
programmatically.

Some knowledge if git is required to make sense of all the
concepts. Also this being a wrapper around the libgit2 library, the
documentation libgit2 http://libgit2.github.com/libgit2/#HEAD is help
full.

The following configuration on Linux (Debian Sid) is known to work:

* SBCL 1.0.56 (sbcl-1.0.56.55-b06f72f-linux-x86)
* libgit2 0.17.0

CL-GIT's API is modelled off the underlying Libgit2 interface.  The
CL-GIT project takes advantage of it and provides a object oriented
interface to libgit2.  Git concepts such as commits, tags, blobs etc
are instances of corresponding classes in CL-GIT.

.. _LLGPL: http://opensource.franz.com/preamble.html
.. _libgit2: http://libgit2.github.com/


Download
--------

Since there is currently no released version, the source can be
downloaded from `github`_.

.. _github: https://github.com/russell/cl-git/

Installation
------------

You'll need a recent version of the libgit2 library, which
can from http://github.com/libgit2/libgit2.

The libgit2 library should be installed as usual, per the libgit2
documentation, and the cl-git package should be downloaded into a
place that ASDF can find it.

The dependencies for CL-git itself are: cffi, local-time, cl-fad,
trivial-garbage, anaphora. The tests in package cl-git-tests, need in
addition to cl-git the following packages: FiveAM and inferior-shell.


Usage
-----

.. toctree::
   :maxdepth: 4

   repositories
   references
   indexes
   commits

Contributing
------------

.. toctree::
   :maxdepth: 4

   developing


Indices and tables
------------------

* :ref:`genindex`
* :ref:`search`

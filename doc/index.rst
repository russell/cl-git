cl-git
======

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


Documentation
-------------

.. toctree::
   :maxdepth: 2

   repositories
   references
   indexs
   commits



Indices and tables
------------------

* :ref:`genindex`
* :ref:`search`

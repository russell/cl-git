cl-git
======

This is common lisp wrapper around the `libgit2`_ library. It's still
early days and the abstraction is far from complete, but there is
enough for this library to be useful.

- `Documentation`_
- `Bug Tracker`_

.. _Documentation: http://cl-git.russellsim.org/
.. _Bug Tracker: https://github.com/russell/cl-git/issues


What It Can Do
--------------

- SHA conversions, formatting
- create commits
- revision walking
- index file (staging area) manipulation
- reference management listing


Requires
--------

* SBCL 1.0.56 (sbcl-1.0.56.55-b06f72f-linux-x86)
* libgit2: 0.17.0


Building Documentation
----------------------

Documentation requires sphinxcontrib.cldomain which depends on

* Sphinx
* cl-launch 3.21.7
* asdf 2.33.3
* quicklisp

Using `virtualenv`_ run::

   pip install -r doc/requirements.txt
   
If you do not have fabric::

   pip install fabric

To build run::

   fab build


License
-------

This library is licensed under the LLGPL essentially it's the same as
the LGPL but with `preamble`_ that puts the Lisp terminology in context.


.. _libgit2: http://libgit2.github.com/
.. _preamble: http://opensource.franz.com/preamble.html
.. _virtualenv: http://www.virtualenv.org/

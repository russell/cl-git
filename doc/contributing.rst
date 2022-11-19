Contributing
============

Testing
-------

CL-Git uses `FiveAM`_ as the testing harness and `inferior shell`_ to
call `lsof`_ to check for file descriptor leaks.

follow the `Roswell install documentation`_.

install lsof::

  apt install lsof

install SBCL and run tests.::

   ros install sbcl

   ./run-tests.lisp

.. _fiveam: http://common-lisp.net/project/fiveam/
.. _inferior shell: http://www.cliki.net/inferior-shell/
.. _lsof: https://github.com/lsof-org/lsof
.. _Roswell install documentation: https://roswell.github.io/Installation.html

CI Builds
~~~~~~~~~

CI builds are done with `Earthly`_ it bulids and tests using buildkit
(either docker or podman containers), once it is installed, you can
list the build targets like this::

   % earthly ls
   +base
   +build
   +deps
   +libgit2
   +test-libgit2-0.27-sbcl
   +test-libgit2-0.28-sbcl
   +test-libgit2-1.0-sbcl
   +test-libgit2-1.1-sbcl
   +test-libgit2-1.2-sbcl
   +test-libgit2-1.3-sbcl
   +test-libgit2-1.4-sbcl
   +test-libgit2-1.5-ccl
   +test-libgit2-1.5-clasp
   +test-libgit2-1.5-ecl
   +test-libgit2-1.5-sbcl
   +test-libgit2-ccl
   +test-libgit2-clasp
   +test-libgit2-ecl
   +test-libgit2-sbcl

All the targets with the format `test-libgit2-<version>-<lisp>` can be
executed to test that version combination.

To run a build you would use a command like::

   earthly +test-libgit2-1.5-sbcl

Currently SBCL is the only working Lisp, but others are there in case
we want to extend our support to them.

.. _Earthly: https://earthly.dev/get-earthly

Documentation
-------------

To build the documentation you need sphinxcontrib.cldomain which
depends on:

* Sphinx
* `sphinxcontrib.cldomain`_ latest `pypi <https://pypi.python.org/pypi/sphinxcontrib-cldomain>`_
* `pygments-cl-repl`_ latest `pypi <https://pypi.python.org/pypi/pygments-cl-repl>`_
* `roswell`_ 3.21.7 `git <https://github.com/roswell/roswell>`_
* `Quicklisp`_

.. _roswell: https://roswell.github.io/
.. _asdf: http://common-lisp.net/project/asdf/
.. _pygments: https://bitbucket.org/russell/pygments-main
.. _sphinxcontrib.cldomain: http://cldomain.russellsim.org/

build documentation::

  ./doc/build.sh

The compiled documentation is output into ``doc/build/html``.

.. _virtualenv: http://www.virtualenv.org/
.. _quicklisp: http://www.quicklisp.org/beta/

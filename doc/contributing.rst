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

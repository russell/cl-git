Contributing
============

Testing
-------

CL-GIT uses FiveAM as the testing harness and inferior shell to help
check for leaks.

.. _fiveam: http://common-lisp.net/project/fiveam/
.. _inferior shell: http://www.cliki.net/inferior-shell/

Download these with `Quicklisp`_


Documentation
-------------

To build the documentation you need sphinxcontrib.cldomain which
depends on:

* Sphinx
* `pygments`_ (with common lisp repl support) `mecurial <https://bitbucket.org/russell/pygments-main>`_
* `sphinxcontrib.cldomain`_ latest `pypi <https://pypi.python.org/pypi/sphinxcontrib-cldomain>`_

  * `cl-launch`_ 3.21.7 `git <http://common-lisp.net/project/xcvb/git/cl-launch.git>`_
  * `asdf`_ 2.33.3 `git <http://common-lisp.net/projects/asdf/asdf.git>`_
  * `Quicklisp`_

.. _cl-launch: http://cliki.net/cl-launch
.. _asdf: http://common-lisp.net/project/asdf/
.. _pygments: https://bitbucket.org/russell/pygments-main
.. _sphinxcontrib.cldomain: http://cldomain.russellsim.org/

Using `virtualenv`_ run::

   pip install -r doc/requirements.txt
   
If you do not have fabric::

   pip install fabric

To build run::

   fab build

The compiled documentation is output into ``doc/html``.

.. _virtualenv: http://www.virtualenv.org/
.. _quicklisp: http://www.quicklisp.org/beta/

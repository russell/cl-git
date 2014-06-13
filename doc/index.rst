CL-Git is an interface to the C library `libgit2`_.  Some knowledge of
git can be useful to make sense of all the concepts but largely this
library aims to make access to git repositories as easy as possible.

License `LLGPL`_

.. _LLGPL: http://opensource.franz.com/preamble.html
.. _libgit2: http://libgit2.github.com/


.. code-block:: common-lisp-repl

   GIT> (resolve
         (get-object 'reference "HEAD"
                     (open-repository 
                      (merge-pathnames #p"projects/lisp/cl-git"
                                       (user-homedir-pathname)))))
   #<COMMIT 276EE31DD4F35E49AEB7C7FCFB8094D557A25AD1 {100817E383}>
   (#<REFERENCE refs/heads/master {100817C403}> #<REFERENCE HEAD {100817C0C3}>)

   GIT> (tree-directory (commit-tree *))
   (#<TREE-BLOB .gitignore (weak) {100A8A1E73}>
    #<TREE-BLOB AUTHORS (weak) {100A8A3E33}>
    #<TREE-BLOB CHANGELOG (weak) {100A8A5483}>
    ...)

Documentation
-------------

.. toctree::
   :maxdepth: 2
   :numbered:

   quickstart
   objects
   repositories
   references
   commits
   tag
   tree
   blob
   indexes
   remote
   config

Low Level
---------

.. toctree::
   :maxdepth: 2

   odb
   internals

Indices and tables
------------------

* :ref:`genindex`
* :ref:`search`


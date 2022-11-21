CL-Git is an interface to the C library `libgit2`_.  The API is an
almost complete exposure of the underlying library.

License `LLGPL`_

This is the HTML documentation, but there is also, :download:`latex
<build/latex/cl-git.pdf>` and :download:`info
<build/texinfo/cl-git.info>`

The source is available on `SourceHut`_.

.. _LLGPL: http://opensource.franz.com/preamble.html
.. _libgit2: http://libgit2.github.com/
.. _SourceHut: https://git.sr.ht/~rsl/cl-git



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
   :caption: Topic Areas

   usage/objects
   usage/repositories
   usage/references
   usage/reflog
   usage/commits
   usage/tag
   usage/tree
   usage/blob
   usage/indexes
   usage/remote
   usage/config

.. toctree::
   :maxdepth: 2
   :caption: Low Level

   low-level/odb
   low-level/internals

.. toctree::
   :maxdepth: 2
   :caption: Developing

   changelog
   contributing

Indices and tables
------------------

* :ref:`genindex`
* :ref:`search`

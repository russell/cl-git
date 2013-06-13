Index
=====

.. cl:package:: cl-git

.. warning::

   This functionally is still under development.  So stability of this
   part of the API is uncertain.

.. cl:type:: index

.. cl:variable:: *git-repository-index*

.. cl:generic:: index

.. cl:macro:: with-index

   :param var: the symbol the opened index will be bound to.
   :param repository-or-path: the repository or path to open the index from.
   :param body: the body of the macro.

.. cl:generic:: git-add

   :param path: the relative path of a file to be added to the repository.

.. cl:method:: index common-lisp:pathname

.. cl:method:: index common-lisp:string

.. cl:method:: git-read index

.. cl:method:: git-write index

.. cl:generic:: git-write-tree

.. cl:generic:: index-clear

.. cl:method:: git-entry-by-path index string

.. cl:method:: git-entry-by-index index t

.. cl:generic:: index-conflicts-p

.. cl:generic:: index-refresh

Index
=====

.. cl:package:: cl-git

.. cl:type:: index

.. cl:variable:: *git-repository-index*

.. cl:macro:: with-repository-index

   :param body: the body of the macro.

.. cl:macro:: with-index

   :param var: the symbol the opened index will be bound to.
   :param repository-or-path: the repository or path to open the index from.
   :param body: the body of the macro.

.. cl:generic:: git-add

   :param path: the relative path of a file to be added to the repository.

.. cl:generic:: git-clear

.. cl:generic:: git-write

.. cl:generic:: git-read

.. cl:generic:: git-write-tree

.. cl:method:: git-entry-by-path index string

.. cl:method:: git-entry-by-index index t

.. cl:function:: git-has-conflicts

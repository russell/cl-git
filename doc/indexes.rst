Index
=====

.. cl:package:: cl-git

.. cl:type:: index

.. cl:variable:: *git-repository-index*

.. cl:macro:: with-repository-index

   :param body: the body of the macro.

.. cl:generic:: git-add

   :param path: the relative path of a file to be added to the repository.

.. cl:generic:: git-clear

.. cl:generic:: git-write

.. cl:generic:: git-read

.. cl:generic:: git-write-tree

.. cl:method:: git-entry-by-path index string

.. cl:method:: git-entry-by-index index t

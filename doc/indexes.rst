Index
=====

.. cl:package:: cl-git

.. cl:type:: index

Using
-----

.. cl:generic:: open-index

.. cl:macro:: with-index

   :param var: the symbol the opened index will be bound to.
   :param repository-or-path: the repository or path to open the index from.
   :param body: the body of the macro.

Adding
~~~~~~

.. cl:generic:: index-add-file

   :param path: the relative path of a file to be added to the repository.

State
~~~~~

.. cl:generic:: index-conflicts-p


These functions assist management of the state of the in memory copy
of the index.

.. cl:generic:: index-reload

.. cl:generic:: index-write


Clearing
~~~~~~~~

.. cl:generic:: index-clear

Convert to Tree
~~~~~~~~~~~~~~~

.. cl:generic:: index-to-tree

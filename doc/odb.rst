ODB
===

.. cl:type:: odb

.. warning::

   This functionally is still under development.  So stability of this
   part of the API is uncertain.

.. cl:type:: odb-object

.. cl:generic:: open-odb

.. cl:method:: list-objects :oid cl-git:odb common-lisp:t

.. cl:method:: list-objects :oid cl-git:repository common-lisp:t

.. cl:method:: get-object odb-object common-lisp:t cl-git:odb

.. cl:method:: get-object odb-object common-lisp:t cl-git:repository

.. cl:method:: odb-type cl-git:odb-object

.. cl:generic:: odb-data cl-git:odb-object

.. cl:generic:: odb-size cl-git:odb-object





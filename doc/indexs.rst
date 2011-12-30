Manipulating the Index
======================

.. cl:package:: cl-git

.. cl:parameter:: (defparameter *git-repository-index*)

.. cl:macro:: (defmacro with-git-repository-index (&body body))

.. cl:function:: (defun git-index-add (path))

.. cl:function:: (defun git-index-clear ())

.. cl:function:: (defun git-index-write ())

.. cl:function:: (defun git-oid-from-index ())

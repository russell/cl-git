Repositories
============

.. cl:package:: cl-git

.. cl:parameter:: (defparameter *git-repository*)

.. cl:macro:: (defmacro with-git-repository ((path) &body body))

   :param path: the path to the git repository
   :param body: the body of the macro

.. cl:function:: (defun ensure-git-repository-exist (path &optional bare))

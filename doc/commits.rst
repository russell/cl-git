Commits
-------

.. cl:package:: cl-git

.. cl:type:: commit

Creating Commits
~~~~~~~~~~~~~~~~

.. cl:function:: make-commit


Walking Commits
~~~~~~~~~~~~~~~

.. cl:macro:: with-git-revisions

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
                (with-git-revisions (commit :sha "69fec1d5938a0c1c8c14a3a120936aa8937af163")
                  (princ (git-commit-message commit))))
      added git str to oid
      added some lowlevel methods for revtree walking
      added error condition strings
      added repository open and list all refs
      initial commit
      NIL

Reading Commit Details
~~~~~~~~~~~~~~~~~~~~~~

.. cl:macro:: bind-git-commits

   .. code-block:: common-lisp

      CL-GIT> (with-git-repository (#p"/home/russell/projects/txirods/")
                (with-git-commits ((commit :sha "46153869f3652de5dbb6ddd598c58445383dcba2"))
                  (princ (git-commit-message commit))))
      added better logging messages
      "added better logging messages
      "

.. cl:generic:: git-message

.. cl:generic:: git-author

.. cl:generic:: git-committer


Signatures
----------

.. cl:package:: cl-git

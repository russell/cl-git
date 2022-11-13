References
==========

.. cl:package:: cl-git

.. cl:type:: reference

Creating
--------

.. cl:method:: make-object reference common-lisp:t common-lisp:t

Accessing
---------

To access a single reference get object can be used, but the fully
qualified name of the reference must be specified.

.. cl:method:: get-object reference common-lisp:t common-lisp:t

   .. code-block:: common-lisp-repl

      GIT> (get-object 'reference "refs/heads/master"
               (open-repository (merge-pathnames #p"projects/lisp/cl-git"
                                                 (user-homedir-pathname))))
      #<REFERENCE refs/heads/master {100BA0E543}>

Details
~~~~~~~

.. cl:method:: full-name reference

   .. code-block:: common-lisp-repl

      GIT> (full-name
             (get-object 'reference "refs/heads/master"
                           (open-repository (merge-pathnames #p"projects/lisp/cl-git"
                                                             (user-homedir-pathname)))))
      "refs/heads/master"

.. cl:method:: short-name reference

   .. code-block:: common-lisp-repl

      GIT> (short-name
             (get-object 'reference "refs/heads/master"
                           (open-repository (merge-pathnames #p"projects/lisp/cl-git"
                                                             (user-homedir-pathname)))))
      "master"

Listing
~~~~~~~

.. cl:method:: list-objects reference common-lisp:t

   To list all the references present in a repository we use the
   function :cl:symbol:`LIST-OBJECTS` and tell to only list objects of
   type :cl:symbol:`REFERENCE`.

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference
                         (open-repository "/home/russell/projects/lisp/cl-git/"))
      (#<REFERENCE refs/remotes/origin/master (weak) {1004A66973}>
       #<REFERENCE refs/remotes/origin/0.17.0 (weak) {1004A66C53}>
       #<REFERENCE refs/remotes/origin/0.18.0 (weak) {1004A66DC3}>
       #<REFERENCE refs/remotes/origin/HEAD (weak) {1004A66F33}>
       #<REFERENCE refs/tags/0.1 (weak) {1004A677D3}>
       #<REFERENCE refs/heads/master (weak) {1004A67943}>
       #<REFERENCE refs/heads/0.18.0 (weak) {1004A67C23}>)


Filtering Results
^^^^^^^^^^^^^^^^^

.. cl:generic:: branch-p reference

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference repo :test #'branch-p)
      (#<REFERENCE refs/heads/master (weak) {1004A67943}>
       #<REFERENCE refs/heads/0.18.0 (weak) {1004A67C23}>)

.. cl:generic:: symbolic-p reference

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference repo :test #'symbolic-p)
      (#<REFERENCE refs/remotes/origin/HEAD (weak) {1004A66F33}>)

.. cl:generic:: remote-p reference

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference repo :test #'remote-p)
      (#<REFERENCE refs/remotes/origin/master (weak) {1004A66973}>
       #<REFERENCE refs/remotes/origin/0.17.0 (weak) {1004A66C53}>
       #<REFERENCE refs/remotes/origin/0.18.0 (weak) {1004A66DC3}>
       #<REFERENCE refs/remotes/origin/HEAD (weak) {1004A66F33}>)

.. cl:generic:: head-p reference

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference repo :test #'head-p)
      (#<REFERENCE refs/remotes/origin/master (weak) {1004A66973}>)


Resolving
~~~~~~~~~

.. cl:method:: target reference

   .. code-block:: common-lisp-repl

      GIT> (target (get-object 'reference "HEAD"
               (open-repository (merge-pathnames #p"projects/ecl"
                                                 (user-homedir-pathname)))))
      #<REFERENCE refs/heads/master {1007CD3D53}>

.. cl:method:: resolve reference

   .. code-block:: common-lisp-repl

      GIT> (resolve (get-object 'reference "HEAD"
               (open-repository (merge-pathnames #p"projects/ecl"
                                                 (user-homedir-pathname)))))
      #<COMMIT E92F8C418F626A5041FC242C0FB1CEB1BEC4D61B {1007497AB3}>
      (#<REFERENCE refs/heads/master {1007496723}> #<REFERENCE HEAD {1007495C53}>)




Branches
~~~~~~~~

In libgit2 and in cl-git, branches are references but in a different
namespace.  Which means that, the same function used to list
references is used to list branches.  To limit the references to
branches only use :cl:symbol:`~BRANCH-P`.

.. cl:method:: branch-p reference

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference repo :test #'branch-p)
      (#<REFERENCE refs/heads/master (weak) {10051CF843}>
       #<REFERENCE refs/heads/0.18.0 (weak) {10051CF9B3}>)

   So a branch is a special kind of reference.  In git there are a few
   differences between branches and references:

   - branches are stored in a special location in the .git folder
   - branches are moved/updated during a git commit operation

   For a user of the git repository, this small difference between
   branches and normal references makes a huge difference.  You
   commit on branches and merge different branches.  But typically
   you will not deal with non branch references.

   Listing remote branches can be done with.

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'reference (open-repository #p"/home/russell/projects/ecl/")
                         :test #'remote-p)
      (#<REFERENCE refs/remotes/origin/master (weak) {1007A39EA3}>
       #<REFERENCE refs/remotes/origin/HEAD (weak) {1007A3A2F3}>)

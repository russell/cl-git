References
==========

.. cl:package:: cl-git

.. cl:type:: reference

.. cl:method:: full-name reference

.. cl:method:: short-name reference

.. cl:method:: make-object reference common-lisp:t common-lisp:t

.. cl:method:: get-object reference common-lisp:t

.. cl:method:: git-list :reference

.. cl:method:: target reference

.. cl:function:: git-resolve


Listing
-------


Lets start simple, we are going to list all the references present in
a repository. For this we first have to open a repository with

.. code-block:: common-lisp-repl

   GIT> (setf repo (open-repository "/home/russell/projects/lisp/cl-git/"))
   #<REPOSITORY 79002A0 {100853C603}>

This returns a repository. Getting a list of references is easy, use
the generic list command and tell it you want references.

.. code-block:: common-lisp-repl

   GIT> (list-objects 'reference repo)
   (#<REFERENCE refs/remotes/origin/master (weak) {1004A66973}>
    #<REFERENCE refs/remotes/origin/0.17.0 (weak) {1004A66C53}>
    #<REFERENCE refs/remotes/origin/0.18.0 (weak) {1004A66DC3}>
    #<REFERENCE refs/remotes/origin/HEAD (weak) {1004A66F33}>
    #<REFERENCE refs/tags/0.1 (weak) {1004A677D3}>
    #<REFERENCE refs/heads/master (weak) {1004A67943}>
    #<REFERENCE refs/heads/0.18.0 (weak) {1004A67C23}>)

If you require filtering there are several filters that are available
to limit the results: :cl:generic:`~branch-p`.

.. code-block:: common-lisp-repl

   GIT> (list-objects 'reference repo :test #'branch-p)
   (#<REFERENCE refs/heads/master (weak) {10051CF843}>
    #<REFERENCE refs/heads/0.18.0 (weak) {10051CF9B3}>)

Filtering Results
~~~~~~~~~~~~~~~~~

.. cl:generic:: branch-p reference
   :nospecializers:

   .. cl:method:: branch-p reference
      :nospecializers:

   .. cl:method:: branch-p common-list:string
      :nospecializers:

.. cl:generic:: symbolic-p reference

.. cl:generic:: remote-p reference


.. cl:generic:: head-p


Branches
--------

In libgit2 and in cl-git, branches are represented as references.
Which means that, as soon as you retrieve a branch with for example

.. code-block:: common-lisp-repl

   > (cl-git:git-lookup :branch "origin/master" :repository *repo-cl-git* :type :remote)

   #<CL-GIT:REFERENCE 500CF0 {10094B4DB3}>

you see you get an instance of the reference class.

So a branch is a special kind of reference.  In git there are a few
differences between branches and references:

- branches are stored in a special location in the .git folder
- branches are moved/updated during a git commit operation

For a user of the git repository, this small difference between
branches and normal references makes a huge difference.  You
commit on branches and merge different branches.  But typically 
you will not deal with non branch references.

Because for actual usage of a git repository branches are special, there
are a few functions which helps listing all the branches, as 

.. code-block:: common-lisp-repl

  > (cl-git:git-list :branch :repository *repo-cl-git*)
  
  (("origin/OID-translation" :REMOTE) ("origin/HEAD" :REMOTE)
   ("gh-pages" :LOCAL) ("convert-to-classes" :LOCAL) ("origin/verrazano" :REMOTE)
   ("origin/master" :REMOTE))


The results of this call are almost the same as the following

.. code-block:: common-lisp-repl

  > (cl-git:git-list :reference :repository *repo-cl-git*)

  ("refs/heads/convert-to-classes" "refs/heads/gh-pages" 
   ...
   "refs/remotes/origin/OID-translation" "refs/tags/0.1")

There are two obvious differences:

- The :reference call returns strings, the names of the references.
  The :branch call returns pairs, of which the first element is a name
  and the second indicates if it is a remote or a local branch

- The names are different.  The :reference call returns longer names.
  The :branch call returns branch names, which are shorted reference names.
  (The prefix identifying them as a branch has been removed).
  This is also the name that the user expects.








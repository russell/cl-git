Branches
========

.. cl:package:: cl-git

.. cl:method:: git-list :branch

.. cl:method:: git-lookup :branch common-lisp:string

.. cl:method:: git-lookup :branch common-lisp:cons

.. cl:function:: git-is-head

Branches and References
-----------------------

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








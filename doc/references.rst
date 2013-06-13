References
==========

.. cl:package:: cl-git

.. cl:type:: reference

Details
-------

.. cl:method:: full-name reference

.. cl:method:: short-name reference

Resolving
---------

.. cl:method:: target reference

.. cl:method:: resolve reference

:cl:symbol:`TARGET` is used to find the object that a
:cl:symbol:`REFERENCE` or :cl:symbol:`TAG` points to

.. code-block:: common-lisp-repl

   GIT> (target (get-object 'reference "HEAD"
            (open-repository (merge-pathnames #p"projects/ecl"
                                              (user-homedir-pathname)))))
   #<REFERENCE refs/heads/master {1007CD3D53}>

To avoid having to manually traverse many symbolic
:cl:symbol:`REFERENCE`, resolve will traverse for you and return the
resulting object and the references traversed along the way.

.. code-block:: common-lisp-repl

   GIT> (resolve (get-object 'reference "HEAD"
            (open-repository (merge-pathnames #p"projects/ecl"
                                              (user-homedir-pathname)))))

   #<COMMIT E92F8C418F626A5041FC242C0FB1CEB1BEC4D61B {1007497AB3}>
   (#<REFERENCE refs/heads/master {1007496723}> #<REFERENCE HEAD {1007495C53}>)

Creating
--------

.. cl:method:: make-object reference common-lisp:t common-lisp:t


Accessing
---------

To access a single reference get object can be used, but the fully
qualified name of the reference must be specified.

.. cl:method:: get-object reference common-lisp:t common-lisp:t

Listing
~~~~~~~

To list references :cl:symbol:`LIST-OBJECTS` is used.  It returns a
list of the references as CLOS objects.

.. cl:method:: list-objects reference common-lisp:t


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


Filtering Results
~~~~~~~~~~~~~~~~~

.. cl:generic:: branch-p reference

.. cl:generic:: symbolic-p reference

.. cl:generic:: remote-p reference

.. cl:generic:: head-p


Branches
--------

In libgit2 and in cl-git, branches references but in a different
namespace.  Which means that, the same function used to list
references is used to list branches.  To limit the references to
branches only use :cl:symbol:`~BRANCH-P`.

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

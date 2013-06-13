Commits
=======

.. cl:package:: cl-git

.. cl:type:: commit

Creating
--------

.. cl:function:: make-commit


Accessing
---------

.. cl:method:: get-object commit common-lisp:t common-lisp:t

.. cl:macro:: bind-git-commits

.. cl:method:: list-objects commit common-lisp:t

There are a few ways to find commits in the repository, the easiest is
to find a commit when we know the SHA-1 has. In that case the process
is as follows:

.. code-block:: common-lisp-repl

   GIT> (get-object 'commit "15b8410814ec05d63b85c5e4b735dcdc77719a08"
                    (open-repository #p"/home/russell/projects/ecl/"))
   #<COMMIT 15B8410814EC05D63B85C5E4B735DCDC77719A08 {10060B27A3}>

To get access to a single reference.

.. code-block:: common-lisp-repl

   GIT> (get-object 'reference "refs/heads/master"
                    (open-repository "/home/russell/projects/ecl/"))
   #<REFERENCE refs/heads/master {1006F13CB3}>

However to get from a reference to a commit is easy using the
:cl:symbol:`TARGET` method.

.. code-block:: common-lisp-repl

   GIT> (target (get-object 'reference "refs/heads/master"
                            (open-repository "/home/russell/projects/ecl/")))
   #<COMMIT E92F8C418F626A5041FC242C0FB1CEB1BEC4D61B {10071DE5B3}>

In this case we ended up with a commit, however a reference can refer
to any object in the git database, so tags, blobs and trees are also
possible.

Now in normal use you do not see references to blobs or trees very
frequently, but references to tags are more common.

So in normal code you have to check for that and act accordingly.

Iterating
~~~~~~~~~

.. cl:function:: revision-walk

.. cl:macro:: with-git-revisions

   .. code-block:: common-lisp-repl

      GIT> (with-repository (repository #p"/home/russell/projects/lisp/cl-git/")
             (with-git-revisions (commit :sha "28483cbb5b9747354b520c2cc034211c11dbb63b"
                                         :repository repository)
               (print (message commit))))

      "added some git commit and revwalk functions
      "
      "added git str to oid
      "
      "added some lowlevel methods for revtree walking
      "
      "added error condition strings
      "
      "added repository open and list all refs
      "
      "initial commit
      "
      NIL

Inspecting
----------

.. cl:generic:: message

.. cl:generic:: author

.. cl:generic:: committer

.. cl:generic:: parents commit

If we have found a commit and assinged it *commit* we can inspect this
object to find out various bits of information.

First we get the commit message and author as follows:


.. code-block:: common-lisp-repl

   GIT> (message
         (get-object 'commit "ea010dee347e50666331b77edcf0588735c3205a"
                     (open-repository #p"/home/russell/projects/ecl/")))
   "Add new declaration, si::c-export-fname, which produces lisp compiled files with
   meaningful names for the exported functions. For instance,
       (proclaim '(si::c-export-fname union))
   is used to produce a C function with name clLunion, which can be directly used
   in other compiled files. This feature has been applied to almost all functions
   in the Lisp runtime.
   "

.. code-block:: common-lisp-repl

   GIT> (author
         (get-object 'commit "ea010dee347e50666331b77edcf0588735c3205a"
                     (open-repository #p"/home/russell/projects/ecl/")))
   (:NAME "jjgarcia" :EMAIL "jjgarcia" :TIME @2001-07-13T02:32:15.000000+10:00
    :TIMEZONE #<LOCAL-TIME::TIMEZONE +0000>)

Or we can see what is parents are,

.. code-block:: common-lisp-repl


   GIT> (parents
         (get-object 'commit "ea010dee347e50666331b77edcf0588735c3205a"
                     (open-repository #p"/home/russell/projects/ecl/")))
   (#<COMMIT F2DA18A5913EEA2D3F8BBD336F08AB48D9D3ECCE (weak) {100559E5A3}>)

To see the state of the repository when this commit was made, use the
:cl:symbol:`GET-TREE`.

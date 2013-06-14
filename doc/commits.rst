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

.. cl:generic:: next-revision

To walk commits, :cl:symbol:`REVISION-WALK` and
:cl:symbol:`NEXT-REVISION` are provided.

.. code-block:: common-lisp-repl

   GIT> (let ((repository (open-repository (merge-pathnames #p"projects/ecl" 
                                              (user-homedir-pathname)))))
          (loop 
            :with walker = (revision-walk 
                            (get-object 'commit "ea010dee347e50666331b77edcf0588735c3205a" 
                                        repository))
            :for revision = (next-revision walker)
            :until (null revision)
            :collect revision))

   (#<COMMIT EA010DEE347E50666331B77EDCF0588735C3205A {1007BA1003}>
    #<COMMIT F2DA18A5913EEA2D3F8BBD336F08AB48D9D3ECCE {1007BA1253}>
    #<COMMIT DC4DF60020DF2BFF026B26E6227127F6A3CC9FC {1007BA14A3}>
    #<COMMIT FE8BCD1B8BD27891F260892CC16BBA4A93999D89 {1007BA16F3}>
    #<COMMIT 2D8D0CD44B87C724ACBCA9F835C2142778007DA9 {1007BA1943}>)


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

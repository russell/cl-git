Commits
=======

.. cl:package:: cl-git

.. cl:type:: commit

.. cl:function:: make-commit

.. cl:macro:: bind-git-commits

.. cl:generic:: author

.. cl:generic:: committer

.. cl:generic:: message

.. cl:generic:: parents commit

Creating
--------


Accessing
---------

There are a few ways to find commits in the repository, the easiest is
to find a commit when we know the SHA-1 has. In that case the process
is as follows:

.. code-block:: common-lisp-repl

   GIT> (get-object 'commit "d8274ef1fe9d70f73cb5490576981d2ee562d68f"
                    (open-repository #p"/home/russell/projects/lisp/cl-git/"))
   #<COMMIT D8274EF1FE9D70F73CB5490576981D2EE562D68F {100603E723}>

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

      GIT> (with-git-repository (#p"/home/russell/projects/cl-git/")
              (with-git-revisions (commit :sha "69fec1d5938a0c1c8c14a3a120936aa8937af163")
                (princ (git-commit-message commit))))
      added git str to oid
      added some lowlevel methods for revtree walking
      added error condition strings
      added repository open and list all refs
      initial commit
      NIL

Inspecting
----------

If we have found a commit and assinged it *commit* we can inspect this
object to find out various bits of information.

First we get the commit message and author as follows:

.. code-block:: common-lisp-repl

   > (cl-git:git-message *commit*)
   "Started to write some documentation
   "
   > (cl-git:git-author *commit*)
   (:NAME "Willem Rein Oudshoorn" :EMAIL "woudshoo+github@xs4all.nl" :TIME
    @2012-05-06T18:46:35.000000+02:00)

Or we can see what is parents are,

.. code-block:: common-lisp-repl

   > (cl-git:git-parentcount *commit*)
   1
   > (cl-git:git-parent-oids *commit*)
   (706478223342774799146743734860864842687841202176)

What is important to notice here is that the result of git-parent-oids
is a list of numbers. These numbers are the same as the SHA-1 hashes
you normally see in git, except displayed, in base 10. The SHA-1 that
are displayed in by the git command line tools are normally written in
base 16. CL-git uses numbers to identify the commits etc. But you can
lookup objects with the base 16 SHA strings, as we did above. This
works because if a SHA-1 hash is expected and a string is supplied,
CL-git converts the string to an integer by reading it in base 16.

If we have found a commit and assinged it *commit* we can inspect this
object to find out information.

First we get the commit message and author as follows:

.. code-block:: common-lisp-repl

   > (cl-git:git-message *commit*)
   "Started to write some documentation
   "
   > (cl-git:git-author *commit*)
   (:NAME "Willem Rein Oudshoorn" :EMAIL "woudshoo+github@xs4all.nl" :TIME
    @2012-05-06T18:46:35.000000+02:00)

Or we can see what is parents are,

.. code-block:: common-lisp-repl

   > (cl-git:git-parentcount *commit*)
   1
   > (cl-git:git-parent-oids *commit*)
   (706478223342774799146743734860864842687841202176)

What is important to notice here is that the result of git-parent-oids
is a list of numbers. These numbers are the same as the SHA-1 hashes
you normally see in git, except displayed, in base 10. The SHA-1 that
are displayed in by the git command line tools are normally written in
base 16. CL-git uses numbers to identify the commits etc. But you can
lookup objects with the base 16 SHA strings, as we did above. This
works because if a SHA-1 hash is expected and a string is supplied,
CL-git converts the string to an integer by reading it in base 16.
3.3.1 The Content of A Commit

To see what is in the commit we can get the tree out of the commit
with

.. code-block:: common-lisp-repl

   > (cl-git:git-tree *commit*)
   #<CL-GIT:TREE 812BDA0 {1009C89393}>
   > (cl-git:git-entries *)
   ((:ATTR 33188 :FILENAME ".gitignore" :FILENAME-LENGTH 10 :OID
     1166326251727089714911644542196064058758301591936 :REMOVED 0)
    (:ATTR 33188 :FILENAME "AUTHORS" :FILENAME-LENGTH 7 :OID
     241890539580627595024686576348750077422898574058 :REMOVED 0)
   ...
    (:ATTR 16384 :FILENAME "src" :FILENAME-LENGTH 3 :OID
     229929308993846155940317335928954649878590463873 :REMOVED 0)
   ...)

So this gives you a list of files and directories in the commit. Note
that this gives only the top level entries, you need to traverse sub
directories yourself. In the example above src is a directory, which
you can tell by interpreting the :ATTR value. The meaning of the
attribute flag is the same as in C and the flags are defined in
/usr/include/cpio.h ad /usr/include/sys/stat.h. For us the important
thing to know is that 16384 (= #8R40000) indicates it is a directory.


And to convert it to a string you can do the following (however this is only likely to work for ASCII).

.. code-block:: common-lisp-repl

   CL-USER> (map 'string #'code-char *)
   "*~
   *.fasl
   
   /doc/eggs/
   ......"

For proper decoding you should use your favorite method, e.g. use
babel. 

Sub Directories
...............

As mentioned before, you can see from the attribute directory if an
entry in the tree is a sub directory. The way to access a sub
directory is by looking up the OID from the entry. For this example we
take the OID from the src entry in the section above.

.. code-block:: common-lisp-repl

   > (cl-git:git-lookup :object
                         229929308993846155940317335928954649878590463873
                         :repository *repo*)
   #<CL-GIT:TREE 812D4D0 {100A22AA93}>

Now notice that we knew beforehand that this was a directory because
the :ATTR was #8R40000, but we can also tell it was a sub directory
because the return value for looking up the entry is a a tree object.

This tree object has entries, containing the files and sub directories
of the src directory of the git project.

And we can repeat the same inspection, lookup etc as in the previous
section.

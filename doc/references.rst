
References
==========

.. cl:package:: cl-git

.. cl:method:: git-list :reference

Lets start simple, we are going to list all the references present in
a repository. For this we first have to open a repository with

.. code-block:: common-lisp

     > (git-open :repository "/Users/woudshoo/Development/Source/Lisp/cl-git/")
     #<CL-GIT::REPOSITORY 79002A0 {100853C603}>

This returns a repository. Getting a list of references is easy, use
the generic list command and tell it you want references.

.. code-block:: common-lisp

     > (cl-git:git-list :reference :repository *)
     ("refs/heads/convert-to-classes" "refs/heads/gh-pages" "refs/heads/master"
       ...
      "refs/tags/my-working-version")

Because the repository is a libgit2 construct, it needs to be
freed. We can do this explicitly with the the generic git-free
function like this..

.. code-block:: common-lisp

     > (cl-git:git-free **)
     nil
     > ***
     #<CL-GIT::REPOSITORY (disposed) {1008507BA3}>

Or we can wait until the garbage collector comes around to do this for
us.

Now a better way of doing this is with the convenience macro
with-repository which opens the repository, sets the special variable
*git-repository* which holds the current (default) repository, and
executes a body of code. Like this:

.. code-block:: common-lisp

     > (cl-git:with-repository ("/Users/woudshoo/Development/Source/Lisp/cl-git/")
           (cl-git:git-list :reference))
     ("refs/heads/convert-to-classes" "refs/heads/gh-pages"
       ... )

References
==========

.. cl:package:: cl-git

.. cl:type:: reference

.. cl:method:: git-type reference

.. cl:method:: git-name reference

.. cl:method:: git-create :reference common-lisp:t

.. cl:method:: git-lookup :reference common-lisp:t

.. cl:method:: git-list :reference

.. cl:function:: git-resolve

Listing
-------

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

Now a better way of doing this is with the convenience macro
with-repository which opens the repository, sets the special variable
*git-repository* which holds the current (default) repository, and
executes a body of code. Like this:

.. code-block:: common-lisp

     > (cl-git:with-repository ("/Users/woudshoo/Development/Source/Lisp/cl-git/")
           (cl-git:git-list :reference))
     ("refs/heads/convert-to-classes" "refs/heads/gh-pages"
       ... )

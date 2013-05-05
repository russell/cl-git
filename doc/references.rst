References
==========

.. cl:package:: cl-git

.. cl:type:: reference

.. cl:method:: git-type reference

.. cl:method:: git-name reference

.. cl:method:: git-create :reference common-lisp:t

.. cl:method:: git-lookup :reference common-lisp:t

.. cl:method:: git-list :reference

.. cl:method:: git-target reference

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

   CL-GIT> (with-repository ("/Users/woudshoo/Development/Source/Lisp/cl-git/")
                (git-list :reference))
   ("refs/remotes/origin/master" "refs/remotes/origin/verrazano"
   "refs/remotes/origin/0.17.0" "refs/remotes/origin/gc"
   "refs/remotes/woudshoo/gh-pages" "refs/remotes/woudshoo/master"
   "refs/remotes/woudshoo/verrazano" "refs/remotes/woudshoo/convert-to-classes"
   "refs/tags/0.1" "refs/heads/master" "refs/heads/verrazano" "refs/heads/gc")

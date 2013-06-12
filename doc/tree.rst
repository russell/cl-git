Tree
====

.. cl:package:: cl-git

.. cl:type:: tree

.. cl:method:: get-object tree common-lisp:t common-lisp:t

.. cl:method:: list-objects tree common-lisp:t

.. cl:generic:: get-tree

   .. code-block:: common-lisp-repl

      GIT> (get-tree
            (target
             (repository-head
              (open-repository (merge-pathnames #p"projects/ecl"
                                                (user-homedir-pathname))))))
      #<TREE 96F8A446E020204589710FE1BF0CE1DD5B5B5AD0 {10079C9C03}>

Listing Contents
----------------

.. cl:generic:: tree-directory

   .. code-block:: common-lisp-repl

      GIT> (tree-directory
            (get-tree
             (target
              (repository-head
               (open-repository (merge-pathnames #p"projects/ecl" (user-homedir-pathname)))))))
      (#<TREE-BLOB .gitignore (weak) {1007732933}>
       #<TREE-BLOB ANNOUNCEMENT (weak) {1007733AE3}>
       #<TREE-BLOB Copyright (weak) {1007734C53}>
       #<TREE-BLOB INSTALL (weak) {1007735DC3}>
       #<TREE-BLOB LGPL (weak) {1007736F13}>
       #<TREE-BLOB Makefile.in (weak) {10077380D3}>
       #<TREE-BLOB README.1st (weak) {1007739283}>
       #<TREE-BLOB configure (weak) {100773A3F3}>
       #<TREE-TREE contrib/ (weak) {100773B523}>
       #<TREE-TREE examples/ (weak) {100773C683}>
       #<TREE-TREE msvc/ (weak) {100773D7D3}>
       #<TREE-TREE src/ (weak) {100773E8D3}>)

Filtering
~~~~~~~~~

In the same way that :cl:symbol:`COMMON-LISP:DIRECTORY` can take
wildcard pathnames to produce limited list of results.

.. code-block:: common-lisp-repl

   GIT> (directory (merge-pathnames #p"projects/ecl/co*"
                                    (user-homedir-pathname)))
   (#P"/home/russell/projects/ecl/configure"
    #P"/home/russell/projects/ecl/contrib/")


:cl:symbol:`TREE-DIRECTORY` can be used to limit the results from the
directory tree.  Using a wild card will filter the results that are
returned.  Extending on the example above we can just list a subset of
the content with.

.. code-block:: common-lisp-repl

   GIT> (tree-directory
         (get-tree
          (target
           (repository-head
            (open-repository (merge-pathnames #p"projects/ecl"
                                              (user-homedir-pathname))))))
         #P"co*")
   (#<TREE-BLOB configure (weak) {1007C11A23}>
    #<TREE-TREE contrib/ (weak) {1007C130F3}>)

Blob
====

.. cl:package:: cl-git

.. cl:type:: blob

.. cl:function:: git-raw-content

.. cl:function:: git-raw-size

Content
-------

Before we dive into this, we can get the content of the file by
extracting the OID, 1166326251727089714911644542196064058758301591936
and lookup this object:

.. code-block:: common-lisp-repl

   > (cl-git:git-lookup :object
                        1166326251727089714911644542196064058758301591936
                       :repository *repo*)
   #<CL-GIT::BLOB 7915440 {1009EA5893}>

A blob is just raw data, stored as raw bytes. Basically everything in
the git database is stored as blobs. So to extract the content we can
do

.. code-block:: common-lisp-repl

   > (cl-git:git-raw-content *)
   #(42 126 10 42 46 102 97 115 108 10 10 47 100 111 99 47 101 103 103 115 47 10
     47 100 111 99 47 100 111 99 116 114 101 101 115 47 10 47 100 111 99 47 104
     116 109 108 47 10 47 100 111 99 47 98 105 110 47 10 47 100 111 99 47 46 105
     110 115 116 97 108 108 101 100 46 99 102 103 10 47 100 111 99 47 77 97 107
     101 102 105 108 101 10 47 100 111 99 47 109 97 107 ...)

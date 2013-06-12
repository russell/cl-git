Blob
====

.. cl:package:: cl-git

.. cl:type:: blob

.. cl:generic:: blob-size

.. cl:generic:: blob-content

.. cl:generic:: binary-p


Accessing
---------

Blobs can be accessed usually via :cl:symbol:`TREE` objects or
directly by :term:`OID`.

.. cl:method:: get-object blob common-lisp:t common-lisp:t

.. cl:method:: list-objects blob common-lisp:t


Content
-------

Before we dive into this, we can get the content of the file by
extracting the :term:`OID`, "6AEF1FC9F802DB1D903200F39E1D776CE355565F"
and lookup this object:

.. code-block:: common-lisp-repl


   GIT> (get-object 'blob "6AEF1FC9F802DB1D903200F39E1D776CE355565F"
                    (open-repository (merge-pathnames #p"projects/ecl"
                                              (user-homedir-pathname))))
   #<BLOB 6AEF1FC9F802DB1D903200F39E1D776CE355565F {10068F1493}>

A blob is just raw data, stored as raw bytes. Basically everything in
the git database is stored as blobs. So to extract the content we can
do

.. code-block:: common-lisp-repl

   GIT> (blob-content
         (get-object 'blob "6AEF1FC9F802DB1D903200F39E1D776CE355565F"
                     (open-repository (merge-pathnames #p"projects/ecl"
                                                       (user-homedir-pathname)))))
   #(35 33 47 98 105 110 47 115 104 10 35 10 35 32 84 104 105 115 32 105 115 32
     106 117 115 116 32 97 32 100 114 105 118 101 114 32 102 111 114 32 99 111 110
     102 105 103 117 114 101 44 32 116 104 101 32 114 101 97 108 32 99 111 110 102
     105 103 117 114 101 32 105 115 32 105 110 32 115 114 99 46 10 35 32 84 104
     105 115 32 115 99 114 105 112 116 32 105 100 101 110 116 105 102 105 101 115
     32 116 104 101 32 109 97 99 104 105 110 101 44 32 97 110 100 32 99 114 ....)

If the content is of a string type then we can print it's contents.

.. code-block:: common-lisp-repl

   GIT> (binary-p
         (get-object 'blob "6AEF1FC9F802DB1D903200F39E1D776CE355565F"
                     (open-repository (merge-pathnames #p"projects/ecl"
                                                       (user-homedir-pathname)))))
   NIL

Since this is a string then we can convert it, `flexi-streams`_ has a
convenient mechanism to convent this to a string.

.. _flexi-streams: http://weitz.de/flexi-streams/

.. code-block:: common-lisp-repl

   GIT> (flexi-streams:octets-to-string
         (blob-content
          (get-object 'blob "6AEF1FC9F802DB1D903200F39E1D776CE355565F"
                      (open-repository (merge-pathnames #p"projects/ecl"
                                                        (user-homedir-pathname))))))
   "#!/bin/sh
   #
   # This is just a driver for configure, the real configure is in src.
   # This script identifies the machine, and creates a directory for
   # the installation, where it runs ${srcdir}/configure.
   set -e

   #if uname -a | grep -i 'mingw32' > /dev/null; then
   #  srcdir=`pwd -W`/src;

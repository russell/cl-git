Tag
===

.. cl:package:: cl-git


Within git there are 2 types of tags, lightweight and annotated.
Lightweight tags are :cl:symbol:`references <REFERENCE>` within the tag
namespace.  Annotated tags are objects stored in the ODB.  Annotated
tags have a TARGET and a TAGGER, which makes them different from
commits.


.. cl:type:: tag

TAG-P can be used to determine if a tag or reference is a "tag".

.. cl:generic:: tag-p


Details
-------

.. cl:method:: full-name tag

.. cl:method:: short-name tag

.. cl:generic:: tagger tag

.. cl:method:: message tag


Target
~~~~~~

.. cl:method:: target tag

.. cl:method:: resolve tag


Creating
--------

.. cl:function:: make-tag

Accessing
---------

.. cl:method:: get-object tag common-lisp:t common-lisp:t

.. cl:method:: list-objects tag common-lisp:t

   .. code-block:: common-lisp-repl

      GIT> (list-objects 'tag (open-repository #p"/home/russell/projects/ecl/"))

      (#<TAG refs/tags/ECL.8.12.0 {1006621153}>
       #<REFERENCE refs/tags/ECL.9.8.3 {1006B277C3}>
       #<REFERENCE refs/tags/ECL.9.8.4 {1006B279C3}>
       #<REFERENCE refs/tags/ECL.9.8.2 {1006B27BC3}>
       #<REFERENCE refs/tags/ECLS.0.4 {1006B27DC3}>
       #<REFERENCE refs/tags/ECL.13.5.1 {1006B27FD3}>
       ...)

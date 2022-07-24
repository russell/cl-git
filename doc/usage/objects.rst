Objects
=======

.. cl:package:: cl-git

Accessing
---------

.. cl:generic:: get-object

.. cl:generic:: list-objects

Note that although we are looking up a commit we specify as class
OBJECT. The advantage of specifying OBJECT instead of COMMIT is that
you do not need to know that the SHA refers to a commit. If the SHA
refers to a tag a tag will be returned.

However if we do not know the SHA-1 but we do know a reference, such
as a branch name or tag. We can get to the commit in a slightly more
cumbersome way. (A list of references is easy to get, see the previous
section.)


Inspecting
----------

.. cl:generic:: oid

.. cl:generic:: full-name

.. cl:generic:: short-name


Errors
------

.. cl:type:: git-error

.. cl:type:: not-found

.. cl:type:: exists

.. cl:type:: ambiguous-error

.. cl:type:: buffer-error

.. cl:type:: user-error

.. cl:type:: barerepo-error

.. cl:type:: orphanedhead-error

.. cl:type:: unmerged-error

.. cl:type:: non-fast-forward-error

.. cl:type:: invalid-spec-error

.. cl:type:: merge-conflict-error

Error conditions can be raised from libgit2 and will be converted into
conditions instead of returning NIL values.

.. code-block:: common-lisp-repl

   GIT> (get-object 'object 1
                    (open-repository #p"/home/russell/projects/ecl/"))
   ; Raises NOT-FOUND condition

For each of the possible libgit2 errors there is a different condition
that will be raised.

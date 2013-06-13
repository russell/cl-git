Objects
=======

.. cl:package:: cl-git

.. cl:type:: object

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


Lazy Loading
~~~~~~~~~~~~

Some types like references are lazy loaded when possible. This can be
seen when they are printed.

.. code-block:: common-lisp-repl

   GIT> (list-objects 'reference  (open-repository #p"/home/russell/projects/ecl/"))
   (...
   #<REFERENCE refs/tags/ECL.0.9f (weak) {100657CCB3}>
   ...)

The `(weak)` identifier shows that this object hasn't been looked up
in the odb yet.

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


Memory Management
-----------------

Because C has manual memory management and Lisp automatic memory
management there is the question on how these two systems integrate.

First most libgit2 objects need to be freeed. There are different free
calls in libgit2, but in CL-git they are all replaced by git-free.

Second of all, this call is made optional. The package
‘trivial-garbage’ takes care of freeing the object when the garbage
collector collects the Lisp git object wrappers.

So normally you do not have to call the free explicitly. However there
are a few reasons you might want to do it anyway:

* Having a repository and commit objects open has the side effect that
  file descriptors to the underlying git files stay open. When you
  iterate over may commits manually (not using the convenience macros)
  can trigger the Lisp process to run out of available file handles.

* Some libgit2 calls can potentially allocate lots of memory. Because
  the Lisp garbage collector does not see the memory allocated by the
  libgit2 library, it helps to call the git-free call to avoid usage
  build up.

.. cl:generic:: free

Object that have been freed have `(disposed)` in their title when
printed.

Dependant Objects
~~~~~~~~~~~~~~~~~

Some objects, such as commits, are only valid as long as another
object is valid, like a repository. This means that as soon as a
repository is git-free'ed the commit becomes invalid. Also conversely,
as long as we keep a reference to a commit and we expect that one to
be valid, the repository can not be collected. We call the commit the
depend object and the repository the facilitating object.

These dependencies are handled in CL-git in the following way:

* When a facilitating object is explicitly freed, or when a
  convenience macro such as with-repository frees the object because
  the execution path leaves scope, all dependend objects on that
  facilitating object are freed.
* Any depenend object holds a reference to its facilitator as long as
  it is not freed.

The consequences are that the following is not correct

.. code-block:: common-lisp

     (with-repository (..)
        (object-get ...))

Because the returned object from the lookup call is not valid anymore
because the repository is closed.

However the following, although uncertain when the repository is
closed, is correct

.. code-block:: common-lisp

     (object-get ... (repository-open ...))

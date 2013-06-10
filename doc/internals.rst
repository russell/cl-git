Internals
=========

.. cl:package:: cl-git

Libgit2
-------

.. cl:function:: libgit2-capabilities

.. cl:function:: libgit2-version


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

.. cl:generic:: git-free

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

     (with-git-repository (..)
        (git-lookup ...))

Because the returned object from the lookup call is not valid anymore because the repository is closed.

However the following, although uncertain when the repository is closed, is correct

.. code-block:: common-lisp

     (git-lookup ... :repository (git-open :repository ...))

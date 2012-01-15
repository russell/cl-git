cl-git
======

This is common lisp wrapper around the `libgit2`_ library. It's still
early days and the abstraction is far from complete, but there is
enough for this library to be useful.

- `Documentation`_
- `Bug Tracker`_

.. _Documentation: http://cl-git.russellsim.org/
.. _Bug Tracker: https://github.com/russell/cl-git/issues


What It Can Do
--------------

- SHA conversions, formatting
- create commits
- revision walking
- index file (staging area) manipulation
- reference management listing


Examples
--------

Listing specific details of a commit:

   (with-git-repository ("/hame/user/projects/repo/.git/")
                      (with-git-revisions (commit :sha "859d33fcbeeafeb65a9ed4c07d7d9e4f40684694")
		        (princ (git-commit-message commit)
		        (princ (git-commit-author commit)))))


License
-------

This library is licensed under the LLGPL essentially it's the same as
the LGPL but with `preamble`_ that puts the Lisp terminology in context.


.. _libgit2: http://libgit2.github.com/
.. _preamble: http://opensource.franz.com/preamble.html

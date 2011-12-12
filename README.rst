cl-git
======

This is common lisp wrapper around the `libgit2`_ library.

Listing specific details of a commit:

   (with-git-repository ("/hame/user/projects/repo/.git/")
                      (with-git-revisions (commit :sha "859d33fcbeeafeb65a9ed4c07d7d9e4f40684694")
		        (princ (git-commit-message commit)
		        (princ (git-commit-author)))))

.. _libgit2: http://libgit2.github.com/

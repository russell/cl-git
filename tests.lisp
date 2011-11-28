(asdf:oos 'asdf:load-op :FiveAM)

(defpackage :cl-git.tests
    (:use :common-lisp
	  :cl-git
	  :it.bese.FiveAM))

(in-package :cl-git.tests)


(defun gen-letter ()
  (gen-character :code (gen-integer :min (char-code #\a)
                                    :max (char-code #\z))))

(test repository-init
  "create a repository and open it to make sure that works"
  (for-all ((str (gen-string :length (gen-integer :min 5 :max 10)
			     :elements (gen-letter))))
    (finishes
      (let ((path (concatenate 'string "/tmp/cl-git.test-" str)))
	(unwind-protect
	     (progn
	       (cl-git:git-repository-init path :bare)
	       (cl-git:git-repository-free)
	       (cl-git:git-repository-open path)
	       (cl-git:git-repository-free))
	  (progn
	    (cl-fad:delete-directory-and-files path)))))))

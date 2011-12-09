(asdf:oos 'asdf:load-op :FiveAM)

(defpackage :cl-git.tests
    (:use :common-lisp
	  :cl-git
	  :it.bese.FiveAM))

(in-package :cl-git.tests)


(defun gen-letter ()
  (gen-character :code (gen-integer :min (char-code #\a)
                                    :max (char-code #\z))))

(defun gen-temp-path ()
  (concatenate 'string "/tmp/cl-git.test-"
	       (funcall
		(gen-string :length (gen-integer :min 5 :max 10)
			    :elements (gen-letter)))))

(test repository-init
  "create a repository and open it to make sure that works"
  (for-all ((path 'gen-temp-path))
    (finishes
      (unwind-protect
	   (progn
	     (cl-git:git-repository-init path :bare)
	     (cl-git:git-repository-free)
	     (cl-git:git-repository-open path)
	     (cl-git:git-repository-free))
	(progn
	  (cl-fad:delete-directory-and-files path))))))

(test create-commits
  "create a repository and add some files to it."
  (let ((path (gen-temp-path)))
    (finishes
      (unwind-protect
	   (progn
	     (cl-git:git-repository-init path :bare)
	     (cl-git:git-repository-free))
	(progn
	  (cl-fad:delete-directory-and-files path))))))

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ~ (path)
    (merge-pathnames path (user-homedir-pathname)))

  (defun load-quicklisp ()
    (block nil
      (flet ((try (x) (when (probe-file x) (return (load x)))))
        (try (~ "quicklisp/setup.lisp"))
        (try (~ ".quicklisp/setup.lisp"))
        (error "Can't find an installation of quicklisp."))))

  #-quicklisp
  (load-quicklisp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-fiveam
  (quicklisp:quickload 'fiveam)
  #-unixoptions
  (quicklisp:quickload 'unix-options))

;; CFFI grovel is needed for cl-git.asd
(quicklisp:quickload 'cffi)

(push (truename #p"./") asdf:*central-registry*)
(quicklisp:quickload 'cl-git)
(quicklisp:quickload 'cl-git/tests)

(defun safe-trace (sym package)
  (handler-bind ((simple-error
                  #'(lambda (c)
                      (declare (ignore c))
                      (format t "Skipping Symbol (probably a macro)~%")
                      (invoke-restart 'skip-symbol))))
    (when (and (fboundp sym)
               (functionp (symbol-function sym))
                                        ;(not (macrop sym))
               (equal (package-name (symbol-package sym))
                      package))
      (format t "Trying to trace Symbol ~A~%" sym)
      (restart-case
          (eval `(trace ,(intern (symbol-name sym) package)))
        (skip-symbol () nil)))))

(unix-options:with-cli-options ()
    (help trace)
  (when help
    (unix-options:print-usage-summary
     "Usage:~%~@{~A~%~}"
     '(((#\t "trace") nil "trace the functions during a test run.")))
    (uiop::quit :unix-status 1))
  (when trace
    (do-symbols (sym 'cl-git)
      (safe-trace sym "CL-GIT"))))

(let ((result-list (fiveam:run :cl-git)))
  (fiveam:explain! result-list)
  (uiop:quit
   (if (remove-if-not
        (lambda (res)
          (typep res 'fiveam::test-failure))
        result-list)
       1 0)))

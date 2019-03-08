;;;; run-tests.lisp
;;;; Part of cl-hackasm.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(ql:quickload 'prove :silent t)

(eval-when (:load-toplevel :execute)
  (pushnew (truename (sb-unix:posix-getcwd/))
	   ql:*local-project-directories*)
  (setf prove:*enable-colors* t)
  (sb-ext:exit :code (if (prove:run "t/test.lisp") 0 1)))

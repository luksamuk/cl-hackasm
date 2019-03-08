;;;; interface.lisp
;;;; Part of cl-hackasm.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackasm)

(defun read-asm-file (file-path)
  (cleanup-instructions
   (handler-case (with-open-file (stream file-path :direction :input)
		   (loop for line = (read-line stream nil)
		      while line
		      collect line))
     (error () (format t "Error opening file.") nil))))

(defun parse-asm-file (file-path)
  (let ((instruction-list (read-asm-file file-path)))
    (when instruction-list
      (handler-case (parse-instructions instruction-list)
	(error (err)
	  (format t "~a~%" err)
	  (format t "Error parsing assembly file.~%")
	  nil)))))

(defun write-asm-file (file-path instruction-list)
  (handler-case
      (with-open-file (stream file-path :direction :output :if-exists :supersede)
	(format stream "~a"
		(with-output-to-string (*standard-output*)
		  (print-instructions instruction-list)))
	t)
    (error () (format t "Cannot output to file.") nil)))

(defmacro if-let (variable value &body body)
  `(let ((,variable ,value))
     (if ,variable
	 (progn ,@body)
	 (format t "Assemble failed. Bailing out.~%"))))

(defun deduce-output-file-name (file-path)
  (let ((extension-location (search ".asm" file-path :from-end t)))
    (concatenate 'string
		 (if (null extension-location)
		     file-path
		     (subseq file-path 0 extension-location))
		 ".hack")))

(defun assemble-file (file-path)
  (let ((output-path (deduce-output-file-name file-path)))
    (if-let parsed-instructions (parse-asm-file file-path)
      (if-let written (write-asm-file output-path parsed-instructions)
	(format t "Assemble successful!~%Wrote \"~a\".~%"
		output-path)))))

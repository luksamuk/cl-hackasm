;;;; parser.lisp
;;;; Part of cl-hackasm.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackasm)

(defparameter *symbol-table*
  '(("SP"     #x0000)
    ("LCL"    #x0001)
    ("ARG"    #x0002)
    ("THIS"   #x0003)
    ("THAT"   #x0004)
    ("R0"     #x0000)
    ("R1"     #x0001)
    ("R2"     #x0002)
    ("R3"     #x0003)
    ("R4"     #x0004)
    ("R5"     #x0005)
    ("R6"     #x0006)
    ("R7"     #x0007)
    ("R8"     #x0008)
    ("R9"     #x0009)
    ("R10"    #x000A)
    ("R11"    #x000B)
    ("R12"    #x000C)
    ("R13"    #x000D)
    ("R14"    #x000E)
    ("R15"    #x000F)
    ("SCREEN" #x4000)
    ("KBD"    #x6000)))

(defparameter *comp-table*
  '(("0"    "0101010")
    ("1"    "0111111")
    ("-1"   "0111010")
    ("D"    "0001100")
    ("A"    "0110000")
    ("!D"   "0001101")
    ("!A"   "0110001")
    ("-D"   "0001111")
    ("-A"   "0110011")
    ("D+1"  "0011111")
    ("A+1"  "0110111")
    ("D-1"  "0001110")
    ("A-1"  "0110010")
    ("D+A"  "0000010")
    ("D-A"  "0010011")
    ("A-D"  "0000111")
    ("D&A"  "0000000")
    ("D|A"  "0010101")
    ("M"    "1110000")
    ("!M"   "1110001")
    ("-M"   "1110011")
    ("M+1"  "1110111")
    ("M-1"  "1110010")
    ("D+M"  "1000010")
    ("D-M"  "1010011")
    ("M-D"  "1000111")
    ("D&M"  "1000000")
    ("D|M"  "1010101")))

(defparameter *dest-table*
  '((nil   "000")
    ("M"   "001")
    ("D"   "010")
    ("MD"  "011")
    ("A"   "100")
    ("AM"  "101")
    ("AD"  "110")
    ("AMD" "111")))

(defparameter *jump-table*
  '((nil    "000")
    ("JGT"  "001")
    ("JEQ"  "010")
    ("JGE"  "011")
    ("JLT"  "100")
    ("JNE"  "101")
    ("JLE"  "110")
    ("JMP"  "111")))

(defun asm-symbol-value (symbol table)
  (declare (list table))
  (cadr (assoc symbol table
	       :test #'equal)))

(defun table-push (symbol value table)
  (cons (list symbol value) table))

(defun clear-instruction (string)
  (let ((comment-start (search "//" string)))
    (string-trim '(#\Space #\Newline #\Tab #\Linefeed #\Return)
		 (if comment-start
		     (subseq string 0 comment-start)
		     string))))

(defun cleanup-instructions (instruction-list)
  (loop for instruction in instruction-list
     for cleaned-instruction = (clear-instruction instruction)
     unless (string-equal cleaned-instruction "")
     collect cleaned-instruction))

(defun a-instruction-p (instruction)
  (or  (and (consp instruction)
	    (eq (car instruction) 'var))
       (and (not (numberp instruction))
	    (not (string-equal instruction ""))
	    (char-equal (uiop:first-char instruction) #\@))))

(defun label-instruction-p (instruction)
  (and (not (string-equal instruction ""))
       (char-equal (uiop:first-char instruction) #\()
       (char-equal (uiop:last-char instruction) #\))))

;; Potential bug: a label such as "()"
(defun instruction-label (instruction)
  (let ((length (length instruction)))
    (subseq instruction 1 (1- length))))

;; Potential bug: an a-instruction such as "@"
(defun a-instruction-value (instruction sym-table &key (first-pass nil))
  (let ((value (subseq instruction 1)))
    (or (asm-symbol-value value sym-table)
	(parse-integer value :junk-allowed t)
	(if first-pass
	    instruction
	    (cons 'VAR value)))))

(defun a-instruction-print (value)
  (if (or (a-instruction-p value)
	  (consp value))
      value
      (format nil "0~15,'0b" value)))

(defun c-instruction-parse (instruction)
  (let* ((dest-end  (search "=" instruction))
	 (jmp-begin (search ";" instruction))
	 (dest (asm-symbol-value (and (not (null dest-end))
				      (subseq instruction 0 dest-end))
				 *dest-table*))
	 (jmp  (asm-symbol-value (and (not (null jmp-begin))
				      (subseq instruction (1+ jmp-begin)))
				 *jump-table*))
	 (comp (asm-symbol-value
		(cond ((and (null dest-end) (null jmp-begin)) instruction)
		      ((null dest-end) (subseq instruction 0 jmp-begin))
		      ((null jmp-begin) (subseq instruction (1+ dest-end)))
		      (t (subseq instruction (1+ dest-end) jmp-begin)))
		*comp-table*)))
    (when (or (not dest) (not jmp) (not comp))
      (error "Invalid instruction: ~a." instruction))
    (concatenate 'string "111" comp dest jmp)))

(defun instructions-first-pass (instruction-list)
  (let* ((sym-table *symbol-table*)
	 (clean-instructions
	  (loop with next = nil
	     with instruction-num = 0
	     for instruction in instruction-list
	     collect (if (label-instruction-p instruction)
			 (if next
			     (error "Label preceeded by another: ~a" instruction)
			     (progn (setf next (instruction-label instruction))
				    nil))
			 (progn (when next
				  (push (list next instruction-num) sym-table)
				  (setf next nil))
				(incf instruction-num)
				(cond ((a-instruction-p instruction)
				       (a-instruction-print
					(a-instruction-value instruction
							     sym-table
							     :first-pass t)))
				      (t (c-instruction-parse instruction))))))))
    (values clean-instructions sym-table)))
				      

(defun instructions-second-pass (instruction-list sym-table)
  (let ((newvar-slot #x0010)
	(var-table   nil))
    (labels ((var-value-q (var-name)
	       (or (asm-symbol-value var-name var-table)
		   (progn (let ((var-address newvar-slot))
			    (incf newvar-slot)
			    (push (list var-name var-address) var-table)
			    var-address)))))
      (loop for instruction in instruction-list
	 unless (null instruction)
	 collect (if (a-instruction-p instruction)
		     (let ((ainstr
			    (a-instruction-print
			     (a-instruction-value instruction sym-table))))
		       (if (consp ainstr)
			   (a-instruction-print (var-value-q (cdr ainstr)))
			   ainstr))
		     instruction)))))

(defun parse-instructions (instruction-list)
  (multiple-value-bind (pre-parsed-instructions symbol-table)
      (instructions-first-pass (cleanup-instructions instruction-list))
    (instructions-second-pass pre-parsed-instructions symbol-table)))

(defun print-instructions (instruction-list)
  (format t "~{~a~%~}" instruction-list))

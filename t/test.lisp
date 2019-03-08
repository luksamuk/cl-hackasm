;;;; test.lisp
;;;; Part of cl-hackasm.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(defpackage #:cl-hackasm/test
  (:use #:cl #:prove))

(ql:quickload :cl-hackasm)

(in-package :cl-hackasm/test)

(defparameter *test-program*
  '("// Adds 1 + ... + 100"
    "    @i"
    "    M=1    // i=1"
    "    @sum"
    "    M=0    // sum=0"
    "(LOOP)"
    "    @i"
    "    D=M    // D=i"
    "    @100"
    "    D=D-A  // D=i-100"
    "    @END"
    "    D;JGT  // if (i-100) > 0 goto END"
    "    @i"
    "    D=M    // D=i"
    "    @sum"
    "    M=D+M  // sum=sum+i"
    "    @i"
    "    M=M+1  // i=i+1"
    "    @LOOP"
    "    0;JMP  // goto LOOP"
    "(END)"
    "    @END"
    "    0;JMP  // infinite loop"))

(defparameter *clean-test-program*
  '("@i"
    "M=1"
    "@sum"
    "M=0"
    "(LOOP)"
    "@i"
    "D=M"
    "@100"
    "D=D-A"
    "@END"
    "D;JGT"
    "@i"
    "D=M"
    "@sum"
    "M=D+M"
    "@i"
    "M=M+1"
    "@LOOP"
    "0;JMP"
    "(END)"
    "@END"
    "0;JMP"))

(defparameter *binary-test-program*
  '("0000000000010000"
    "1110111111001000"
    "0000000000010001"
    "1110101010001000"
    ;;erased
    "0000000000010000"
    "1111110000010000"
    "0000000001100100"
    "1110010011010000"
    "0000000000010010"
    "1110001100000001"
    "0000000000010000"
    "1111110000010000"
    "0000000000010001"
    "1111000010001000"
    "0000000000010000"
    "1111110111001000"
    "0000000000000100"
    "1110101010000111"
    ;;erased
    "0000000000010010"
    "1110101010000111"))

(plan nil)

(subtest "Parse test"
  (diag "Comment and whitespace cleanup")
  (loop for instr1 in (cl-hackasm:cleanup-instructions *test-program*)
     for instr2 in *clean-test-program*
     always (is instr1 instr2))
  (diag "Assembling code")
  (loop for instr1 in (cl-hackasm:parse-instructions *test-program*)
	 for instr2 in *binary-test-program*
	 always (is instr1 instr2)))

(finalize)

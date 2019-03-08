;;;; cl-hackasm.asd
;;;; Part of cl-hackasm.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(asdf:defsystem #:cl-hackasm
  :description "Assembler for the nand2tetris Hack platform"
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src" :components ((:file "parser")
					   (:file "interface")))))


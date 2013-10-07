;;;; Last modified: 2013-10-07 23:41:21 tkych

;; cl-intset/cl-intset.asd

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-intset/LICENSE


;;====================================================================
;; CL-INTSET
;;====================================================================
;; cl-intset/
;;   cl-intset.asd
;;   intset.lisp
;;   test.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for CL-INTSET
;;====================================================================

(asdf:defsystem #:cl-intset
  :name        "cl-intset"
  :description "Library for a set of a non-negative integers."
  :version     "0.1.00"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>" 
  :components  ((:file "intset"))
  )


;;====================================================================

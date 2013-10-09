;;;; Last modified: 2013-10-09 19:14:19 tkych

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
  :description "A library for non-negative integer sets.
It implements sets using bitsets."
  :version     "0.1.01"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>" 
  :components  ((:file "intset"))
  )


;;====================================================================

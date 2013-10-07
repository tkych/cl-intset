;;;; Last modified: 2013-10-07 23:27:33 tkych

;; cl-intset/test.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-intset/LICENSE


;;====================================================================
;; Test for cl-intset
;;====================================================================

(in-package :cl-user)
(defpackage #:cl-intset-test
  (:use :cl))
(in-package #:cl-intset-test)


(defun =>? (expr want &key (test #'equal))
  (assert (funcall test expr want)))


;;--------------------------------------------------------------------
;; Tests
;;--------------------------------------------------------------------

(defparameter is1 (intset:make-intset))
(defparameter is2 (intset:integers->intset '(42)))
(defparameter is3 (intset:integers->intset '(42 42 42 42)))
(defparameter is4 (intset:integers->intset '(41 42 43 44)))

(defun init ()
  (setf is1 (intset:make-intset)
        is2 (intset:integers->intset '(42))
        is3 (intset:integers->intset '(42 42 42 42))
        is4 (intset:integers->intset '(41 42 43 44))))


;; intset
(=>? (typep is1 'intset:intset) t)
(=>? (typep is2 'intset:intset) t)
(=>? (typep is3 'intset:intset) t)
(=>? (typep is4 'intset:intset) t)
(=>? (typep 1   'intset:intset)   nil)
(=>? (typep "is1" 'intset:intset) nil)


;; intsetp
(=>? (intset:intsetp is1) t)
(=>? (intset:intsetp is2) t)
(=>? (intset:intsetp is3) t)
(=>? (intset:intsetp is4) t)
(=>? (intset:intsetp 42)    nil)
(=>? (intset:intsetp "is1") nil)


;; emptyp
(=>? (intset:emptyp is1) t)
(=>? (intset:emptyp is2) nil)
(=>? (intset:emptyp is3) nil)
(=>? (intset:emptyp is4) nil)


;; size
(=>? (intset:size is1) 0)
(=>? (intset:size is2) 1)
(=>? (intset:size is3) 1)
(=>? (intset:size is4) 4)


;; memberp
(=>? (intset:memberp 42 is1) nil)
(=>? (intset:memberp 42 is2) t)
(=>? (intset:memberp 43 is2) nil)
(=>? (intset:memberp 44 is4) t)


;; intset:subsetp
(=>? (intset:subsetp is1 is1) t)
(=>? (intset:subsetp is1 is2) t)
(=>? (intset:subsetp is2 is1) nil)
(=>? (intset:subsetp is3 is2) t)
(=>? (intset:subsetp is3 is4) t)


;; proper-subset-p
(=>? (intset:proper-subset-p is1 is1) nil)
(=>? (intset:proper-subset-p is1 is2) t)
(=>? (intset:proper-subset-p is2 is3) nil)
(=>? (intset:proper-subset-p is2 is4) t)


;; set-equal
(=>? (intset:set-equal is1 is1) t)
(=>? (intset:set-equal is1 is2) nil)
(=>? (intset:set-equal is2 is3) t)
(=>? (intset:set-equal is2 is4) nil)


;; copy
(=>? (intset:set-equal is1 (intset:copy is1)) t)
(=>? (intset:size (intset:copy is4)) 4)
(=>? (intset:memberp 42 (intset:copy is3)) t)


;; clear
(=>? (intset:intset->integers is4) '(41 42 43 44))
(=>? (intset:intset->integers (intset:clear (intset:copy is4))) ())
(=>? (intset:intset->integers is4) '(41 42 43 44))


;; add
(=>? (intset:size (intset:add 42 is1)) 1)
(=>? (intset:emptyp is1) t)
(=>? (intset:size (intset:add 42 is2)) 1)


;; remove
(=>? (intset:size (intset:remove 42 is2)) 0)
(=>? (intset:size (intset:remove 42 is1)) 0)
(=>? (intset:size (intset:remove 42 is4)) 3)
(=>? (intset:size is4)                    4)


;; insert, delete
(=>? (intset:size (intset:insert 42 is1)) 1)
(=>? (intset:memberp 42 is1) t)
(=>? (intset:size (intset:insert 42 is1)) 1)
(intset:insert 41 is1)
(intset:insert 42 is1)
(intset:insert 43 is1)
(intset:insert 44 is1)
(=>? (intset:size is1) 4)
(=>? (intset:set-equal is1 is4) t)
(=>? (intset:intset->integers is1) '(41 42 43 44))
(=>? (intset:proper-subset-p is1 is4) nil)
(intset:delete 42 is1)
(=>? (intset:memberp 42 is1) nil)
(=>? (intset:proper-subset-p is1 is4) t)
(init)


;; integers->intset
(=>? (intset:intset->integers is4) '(41 42 43 44))
(=>? (intset:integers->intset '(41 42 43 44))
     is4
     :test #'intset:set-equal)
(=>? (intset:integers->intset '())
     is1
     :test #'intset:set-equal)
(=>? (intset:integers->intset '(42 42))
     is2
     :test #'intset:set-equal)


;; intset:intersection
(=>? (intset:emptyp (intset:intersection is1 is3)) t)
(=>? (intset:intersection is2 is4)
     (intset:integers->intset '(42))
     :test #'intset:set-equal)
(=>? (intset:intersection is4 is4)
     (intset:integers->intset '(42 41 43 44))
     :test #'intset:set-equal)


;; intset:nintersection
(=>? (intset:emptyp (intset:nintersection is1 is3)) t)
(=>? (intset:nintersection is2 is4)
     (intset:integers->intset '(42))
     :test #'intset:set-equal)
(=>? (intset:nintersection is4 is4)
     (intset:integers->intset '(42 41 43 44))
     :test #'intset:set-equal)
(=>? (intset:nintersection is4 is2)
     (intset:integers->intset '(42))
     :test #'intset:set-equal)
(=>? is4
     (intset:integers->intset '(42))
     :test #'intset:set-equal)
(init)


;; intset:set-difference
(=>? (intset:emptyp (intset:set-difference is1 is1)) t)
(=>? (intset:emptyp (intset:set-difference is4 is4)) t)
(=>? (intset:set-difference is4 is3)
     (intset:integers->intset '(41 43 44)) :test #'intset:set-equal)
(=>? (intset:set-difference is3 is4)
     (intset:integers->intset '()) :test #'intset:set-equal)
(=>? (intset:set-difference is4 (intset:integers->intset '(40 41 42)))
     (intset:integers->intset '(43 44)) :test #'intset:set-equal)


;; intset:nset-difference
(=>? (intset:emptyp (intset:nset-difference is1 is1)) t)
(intset:nset-difference is4 is3)
(=>? is4
     (intset:integers->intset '(41 43 44)) :test #'intset:set-equal)
(=>? (intset:nset-difference is3 is4)
     (intset:integers->intset '(42)) :test #'intset:set-equal)
(=>? (intset:nset-difference is4 (intset:integers->intset '(40 41 42)))
     (intset:integers->intset '(43 44)) :test #'intset:set-equal)
(init)


;; intset:union
(=>? (intset:union is4 is4)
     is4 :test #'intset:set-equal)
(=>?  (intset:union is1 is2)
     is3 :test #'intset:set-equal)
(=>? (intset:union (intset:integers->intset '(0 1 2 3 4))
                   (intset:integers->intset '(2 3 4 5 6)))
     (intset:integers->intset '(0 1 2 3 4 5 6)) :test #'intset:set-equal)


;; intset:nunion
(=>? (intset:nunion is4 is4)
     is4 :test #'intset:set-equal)
(=>? (intset:nunion is1 is4)
     (intset:integers->intset '(41 42 43 44)) :test #'intset:set-equal)
(=>? is1
     (intset:integers->intset '(41 42 43 44)) :test #'intset:set-equal)
(intset:clear is1)
(=>? (intset:emptyp is1) t)
(=>? (intset:nunion (intset:integers->intset '(0 1 2 3 4))
                    (intset:integers->intset '(2 3 4 5 6)))
     (intset:integers->intset '(0 1 2 3 4 5 6)) :test #'intset:set-equal)
(init)


;; intset:set-exclusive-or
(=>? (intset:set-exclusive-or (intset:integers->intset '(0 1 2 3 4))
                              (intset:integers->intset '(2 3 4 5 6)))
     (intset:integers->intset '(0 1 5 6)) :test #'intset:set-equal)
(=>? (intset:set-exclusive-or (intset:integers->intset '(0 1 2 3 4))
                              (intset:integers->intset '()))
     (intset:integers->intset '(0 1 2 3 4)) :test #'intset:set-equal)
(=>? (intset:set-exclusive-or (intset:integers->intset '())
                              (intset:integers->intset '()))
     (intset:integers->intset '()) :test #'intset:set-equal)


;; intset:nset-exclusive-or
(=>? (intset:nset-exclusive-or (intset:integers->intset '(0 1 2 3 4))
                               (intset:integers->intset '(2 3 4 5 6)))
     (intset:integers->intset '(0 1 5 6)) :test #'intset:set-equal)
(=>? (intset:nset-exclusive-or (intset:integers->intset '(0 1 2 3 4))
                               (intset:integers->intset '()))
     (intset:integers->intset '(0 1 2 3 4)) :test #'intset:set-equal)
(=>? (intset:nset-exclusive-or (intset:integers->intset '())
                               (intset:integers->intset '()))
     (intset:integers->intset '()) :test #'intset:set-equal)
(=>? (intset:nset-exclusive-or is4 is3)
     (intset:integers->intset '(41 43 44)) :test #'intset:set-equal)
(init)


;; intset:random
(defun test-intest-random (n)
  (let* ((num-int 100)
         (intset (loop :for i :from 0 :below num-int :collect i :into acc
                       :finally (return (intset:integers->intset acc))))
         (counts (make-sequence 'vector num-int :initial-element 0)))

    (dotimes (_ n)
      (incf (svref counts (intset:random intset))))

    (let* ((expected (/ n num-int 1.0))
           (results (map 'vector (lambda (count) (/ count expected))
                         counts)))
      
      (print results)
      (every (lambda (result) (<= 0.9 result 1.1))
             results))))

(=>? (test-intest-random 100000) t)


;; intset:map
(=>? (intset:map 'intset:intset #'1+ is1)
     (intset:make-intset) :test #'intset:set-equal)
(=>? (intset:map 'intset:intset (lambda (x) (* 10 x)) is3)
     (intset:integers->intset '(420)) :test #'intset:set-equal)
(=>? (intset:map 'intset:intset #'1- is4)
     (intset:integers->intset '(40 41 42 43)) :test #'intset:set-equal)

(=>? (intset:map 'list #'1+ is1)
     ())
(=>? (intset:map 'list (lambda (x) (* 10 x)) is3)
     '(420))
(=>? (intset:map 'list #'1- is4)
     '(40 41 42 43))

(=>? (intset:map 'vector #'1+ is1)
     #() :test #'equalp)
(=>? (intset:map 'vector (lambda (x) (* 10 x)) is3)
     #(420) :test #'equalp)
(=>? (intset:map 'vector #'1- is4)
     #(40 41 42 43) :test #'equalp)

(=>? (intset:map 'string
                 #'code-char
                 (intset:integers->intset
                  (loop :for c :across "Autumn Leaves"
                        :collect (char-code c))))
     " ALaemnstuv")


;; intset:for-each
(=>? (with-output-to-string (*standard-output*)
       (intset:for-each #'print is1))
     "")
(=>? (with-output-to-string (*standard-output*)
       (intset:for-each #'print is1 :desc? t))
     "")
(=>? (with-output-to-string (*standard-output*)
       (intset:for-each #'print is4))
     "
41 
42 
43 
44 ")
(=>? (with-output-to-string (*standard-output*)
       (intset:for-each #'print is4 :desc? t))
     "
44 
43 
42 
41 ")


;; intset:max
(=>? (intset:max is1) nil)
(=>? (intset:max is2) 42)
(=>? (intset:max is3) 42)
(=>? (intset:max is4) 44)

;; intset:min
(=>? (intset:min is1) nil)
(=>? (intset:min is2) 42)
(=>? (intset:min is3) 42)
(=>? (intset:min is4) 41)


;;====================================================================

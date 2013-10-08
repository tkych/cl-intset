;;;; Last modified: 2013-10-08 19:24:12 tkych

;; cl-intset/test.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-intset/LICENSE


;;====================================================================
;; Test for cl-intset
;;====================================================================

(in-package :cl-user)
(defpackage #:cl-intset-test (:use :cl))
(in-package #:cl-intset-test)


(defun =>? (expr want &key (test #'equal))
  (assert (funcall test expr want)))

(defmacro =>error? (expr)
  `(assert (handler-case
               (progn ,expr nil)
             (error () t))))


;;--------------------------------------------------------------------
;; Tests
;;--------------------------------------------------------------------

(defparameter is1 (iset:make-intset))
(defparameter is2 (iset:list->intset '(42)))
(defparameter is3 (iset:list->intset '(42 42 42 42)))
(defparameter is4 (iset:list->intset '(41 42 43 44)))

(defun initialize ()
  (setf is1 (iset:make-intset)
        is2 (iset:list->intset '(42))
        is3 (iset:list->intset '(42 42 42 42))
        is4 (iset:list->intset '(41 42 43 44))))


;; intset
(=>? (typep is1 'iset:intset) t)
(=>? (typep is2 'iset:intset) t)
(=>? (typep is3 'iset:intset) t)
(=>? (typep is4 'iset:intset) t)
(=>? (typep 1   'iset:intset)   nil)
(=>? (typep "is1" 'iset:intset) nil)


;; intsetp
(=>? (iset:intsetp is1) t)
(=>? (iset:intsetp is2) t)
(=>? (iset:intsetp is3) t)
(=>? (iset:intsetp is4) t)
(=>? (iset:intsetp 42)    nil)
(=>? (iset:intsetp "is1") nil)


;; emptyp
(=>? (iset:emptyp is1) t)
(=>? (iset:emptyp is2) nil)
(=>? (iset:emptyp is3) nil)
(=>? (iset:emptyp is4) nil)


;; singletonp
(=>? (iset:singletonp is1) nil)
(=>? (iset:singletonp is2) t)
(=>? (iset:singletonp is3) t)
(=>? (iset:singletonp is4) nil)


;; size
(=>? (iset:size is1) 0)
(=>? (iset:size is2) 1)
(=>? (iset:size is3) 1)
(=>? (iset:size is4) 4)


;; memberp
(=>? (iset:memberp 42 is1) nil)
(=>? (iset:memberp 42 is2) t)
(=>? (iset:memberp 43 is2) nil)
(=>? (iset:memberp 44 is4) t)


;; iset:subsetp
(=>? (iset:subsetp is1 is1) t)
(=>? (iset:subsetp is1 is2) t)
(=>? (iset:subsetp is2 is1) nil)
(=>? (iset:subsetp is3 is2) t)
(=>? (iset:subsetp is3 is4) t)


;; proper-subset-p
(=>? (iset:proper-subset-p is1 is1) nil)
(=>? (iset:proper-subset-p is1 is2) t)
(=>? (iset:proper-subset-p is2 is3) nil)
(=>? (iset:proper-subset-p is2 is4) t)


;; set-equal
(=>? (iset:set-equal is1 is1) t)
(=>? (iset:set-equal is1 is2) nil)
(=>? (iset:set-equal is2 is3) t)
(=>? (iset:set-equal is2 is4) nil)


;; copy
(=>? (iset:set-equal is1 (iset:copy is1)) t)
(=>? (iset:size (iset:copy is4)) 4)
(=>? (iset:memberp 42 (iset:copy is3)) t)


;; clear
(=>? (iset:intset->list is4) '(41 42 43 44))
(=>? (iset:intset->list (iset:clear (iset:copy is4))) ())
(=>? (iset:intset->list is4) '(41 42 43 44))


;; add
(=>? (iset:size (iset:add 42 is1)) 1)
(=>? (iset:emptyp is1) t)
(=>? (iset:size (iset:add 42 is2)) 1)
(=>error? (iset:add "42" is1))
(=>error? (iset:add 'foo is1))


;; remove
(=>? (iset:size (iset:remove 42 is2)) 0)
(=>? (iset:size (iset:remove 42 is1)) 0)
(=>? (iset:size (iset:remove 42 is4)) 3)
(=>? (iset:size is4)                    4)
(=>error? (iset:remove -42 is1))
(=>error? (iset:remove "42" is1))
(=>error? (iset:remove 'foo is1))


;; insert, delete
(=>? (iset:size (iset:insert 42 is1)) 1)
(=>? (iset:memberp 42 is1) t)
(=>? (iset:size (iset:insert 42 is1)) 1)
(iset:insert 41 is1)
(iset:insert 42 is1)
(iset:insert 43 is1)
(iset:insert 44 is1)
(=>? (iset:size is1) 4)
(=>? (iset:set-equal is1 is4) t)
(=>error? (iset:insert -42 is1))
(=>error? (iset:insert "42" is1))
(=>error? (iset:insert 'foo is1))

(=>? (iset:intset->list is1) '(41 42 43 44))
(=>? (iset:proper-subset-p is1 is4) nil)
(iset:delete 42 is1)
(iset:delete 42 is1)
(=>? (iset:memberp 42 is1) nil)
(=>? (iset:proper-subset-p is1 is4) t)
(=>error? (iset:delete -42 is1))
(=>error? (iset:delete "42" is1))
(=>error? (iset:delete 'foo is1))
(initialize)


;; list->intset
(=>? (iset:intset->list is4) '(41 42 43 44))
(=>? (iset:list->intset '(41 42 43 44))
     is4 :test #'iset:set-equal)
(=>? (iset:list->intset '())
     is1 :test #'iset:set-equal)
(=>? (iset:list->intset '(42 42))
     is2 :test #'iset:set-equal)
(=>error? (iset:list->intset '(41 -42 43 44)))
(=>error? (iset:list->intset '(41 "42" 43 44)))
(=>error? (iset:list->intset '(41 foo 43 44)))


;; iset:intersection
(=>? (iset:emptyp (iset:intersection is1 is3)) t)
(=>? (iset:intersection is2 is4)
     (iset:list->intset '(42)) :test #'iset:set-equal)
(=>? (iset:intersection is4 is4)
     (iset:list->intset '(42 41 43 44)) :test #'iset:set-equal)


;; iset:nintersection
(=>? (iset:emptyp (iset:nintersection is1 is3)) t)
(=>? (iset:nintersection is2 is4)
     (iset:list->intset '(42)) :test #'iset:set-equal)
(=>? (iset:nintersection is4 is4)
     (iset:list->intset '(42 41 43 44)) :test #'iset:set-equal)
(=>? (iset:nintersection is4 is2)
     (iset:list->intset '(42)) :test #'iset:set-equal)
(=>? is4
     (iset:list->intset '(42)) :test #'iset:set-equal)
(initialize)


;; iset:set-difference
(=>? (iset:emptyp (iset:set-difference is1 is1)) t)
(=>? (iset:emptyp (iset:set-difference is4 is4)) t)
(=>? (iset:set-difference is4 is3)
     (iset:list->intset '(41 43 44)) :test #'iset:set-equal)
(=>? (iset:set-difference is3 is4)
     (iset:list->intset '()) :test #'iset:set-equal)
(=>? (iset:set-difference is4 (iset:list->intset '(40 41 42)))
     (iset:list->intset '(43 44)) :test #'iset:set-equal)


;; iset:nset-difference
(=>? (iset:emptyp (iset:nset-difference is1 is1)) t)
(iset:nset-difference is4 is3)
(=>? is4
     (iset:list->intset '(41 43 44)) :test #'iset:set-equal)
(=>? (iset:nset-difference is3 is4)
     (iset:list->intset '(42)) :test #'iset:set-equal)
(=>? (iset:nset-difference is4 (iset:list->intset '(40 41 42)))
     (iset:list->intset '(43 44)) :test #'iset:set-equal)
(initialize)


;; iset:union
(=>? (iset:union is4 is4)
     is4 :test #'iset:set-equal)
(=>?  (iset:union is1 is2)
      is3 :test #'iset:set-equal)
(=>? (iset:union (iset:list->intset '(0 1 2 3 4))
                 (iset:list->intset '(2 3 4 5 6)))
     (iset:list->intset '(0 1 2 3 4 5 6)) :test #'iset:set-equal)


;; iset:nunion
(=>? (iset:nunion is4 is4)
     is4 :test #'iset:set-equal)
(=>? (iset:nunion is1 is4)
     (iset:list->intset '(41 42 43 44)) :test #'iset:set-equal)
(=>? is1
     (iset:list->intset '(41 42 43 44)) :test #'iset:set-equal)
(iset:clear is1)
(=>? (iset:emptyp is1) t)
(=>? (iset:nunion (iset:list->intset '(0 1 2 3 4))
                  (iset:list->intset '(2 3 4 5 6)))
     (iset:list->intset '(0 1 2 3 4 5 6)) :test #'iset:set-equal)
(initialize)


;; iset:set-exclusive-or
(=>? (iset:set-exclusive-or (iset:list->intset '(0 1 2 3 4))
                            (iset:list->intset '(2 3 4 5 6)))
     (iset:list->intset '(0 1 5 6)) :test #'iset:set-equal)
(=>? (iset:set-exclusive-or (iset:list->intset '(0 1 2 3 4))
                            (iset:list->intset '()))
     (iset:list->intset '(0 1 2 3 4)) :test #'iset:set-equal)
(=>? (iset:set-exclusive-or (iset:list->intset '())
                            (iset:list->intset '()))
     (iset:list->intset '()) :test #'iset:set-equal)


;; iset:nset-exclusive-or
(=>? (iset:nset-exclusive-or (iset:list->intset '(0 1 2 3 4))
                             (iset:list->intset '(2 3 4 5 6)))
     (iset:list->intset '(0 1 5 6)) :test #'iset:set-equal)
(=>? (iset:nset-exclusive-or (iset:list->intset '(0 1 2 3 4))
                             (iset:list->intset '()))
     (iset:list->intset '(0 1 2 3 4)) :test #'iset:set-equal)
(=>? (iset:nset-exclusive-or (iset:list->intset '())
                             (iset:list->intset '()))
     (iset:list->intset '()) :test #'iset:set-equal)
(=>? (iset:nset-exclusive-or is4 is3)
     (iset:list->intset '(41 43 44)) :test #'iset:set-equal)
(initialize)


;; iset:random
(defun intset-random-test1 (integers n)
  (let* ((iset (iset:list->intset integers))
         (want (iset:intset->list iset))
         ;; Trial
         (got (loop :repeat n
                    :collect (iset:random iset) :into acc
                    :finally (return
                               (sort (remove-duplicates acc) #'<)))))
    ;; Print
    (format t "~2&CL-INTSET:RANDOM test1~
               ~%whether return-value is contained in source intset?~
               ~%want: ~A~%got:  ~A~%" want got)
    ;; Return
    (equal want got)))

(=>? (intset-random-test1 '(2 3 4 5 6 7) 1000) t)

(defun intset-random-test2 (n)
  (let* ((num-int 100)
         (intset (loop :for i :from 0 :below num-int :collect i :into acc
                       :finally (return (iset:list->intset acc))))
         (counts (make-sequence 'vector num-int :initial-element 0)))
    ;; Trial
    (dotimes (_ n)
      (incf (svref counts (iset:random intset))))
    ;; Mining
    (let* ((expected (/ n num-int 1.0))
           (results (map 'vector (lambda (count) (/ count expected))
                         counts)))
      ;; Print
      (format t "~2&CL-INTSET:RANDOM test2~
                 ~%got-count/expected-count ratio ~
                   for { i | 0 <= i < ~A }~%~A~%"
              num-int results)
      ;; Return
      (every (lambda (result) (<= 0.9 result 1.1))
             results))))

(=>? (intset-random-test2 1000000) t)


;; iset:map
(=>? (iset:map 'iset:intset #'1+ is1)
     (iset:make-intset) :test #'iset:set-equal)
(=>? (iset:map 'iset:intset (lambda (x) (* 10 x)) is3)
     (iset:list->intset '(420)) :test #'iset:set-equal)
(=>? (iset:map 'iset:intset #'1- is4)
     (iset:list->intset '(40 41 42 43)) :test #'iset:set-equal)

(=>? (iset:map 'list #'1+ is1)
     ())
(=>? (iset:map 'list (lambda (x) (* 10 x)) is3)
     '(420))
(=>? (iset:map 'list #'1- is4)
     '(40 41 42 43))
(=>? (iset:map 'list #'1- is4 :desc? t)
     '(43 42 41 40))

(=>? (iset:map 'vector #'1+ is1)
     #() :test #'equalp)
(=>? (iset:map 'vector (lambda (x) (* 10 x)) is3)
     #(420) :test #'equalp)
(=>? (iset:map 'vector #'1- is4)
     #(40 41 42 43) :test #'equalp)
(=>? (iset:map 'vector #'1- is4 :desc? t)
     #(43 42 41 40) :test #'equalp)

(=>? (iset:map 'string
               #'code-char
               (iset:list->intset
                (loop :for c :across "Autumn Leaves"
                      :collect (char-code c))))
     " ALaemnstuv")
(=>? (iset:map 'string
               #'code-char
               (iset:list->intset
                (loop :for c :across "Autumn Leaves"
                      :collect (char-code c)))
               :desc? t)
     "vutsnmeaLA ")

(=>error? (iset:map 'symbol  #'identity is4))
(=>error? (iset:map 'package #'identity is4))
(=>error? (iset:map 'vector  nil is4))


;; iset:for-each
(=>? (with-output-to-string (*standard-output*)
       (iset:for-each #'print is1))
     "")
(=>? (with-output-to-string (*standard-output*)
       (iset:for-each #'print is1 :desc? t))
     "")
(=>? (with-output-to-string (*standard-output*)
       (iset:for-each #'print is4))
     "
41 
42 
43 
44 ")
(=>? (with-output-to-string (*standard-output*)
       (iset:for-each #'print is4 :desc? t))
     "
44 
43 
42 
41 ")

(=>error? (iset:for-each nil is4))


;; iset:max
(=>? (iset:max is1) nil)
(=>? (iset:max is2) 42)
(=>? (iset:max is3) 42)
(=>? (iset:max is4) 44)


;; iset:min
(=>? (iset:min is1) nil)
(=>? (iset:min is2) 42)
(=>? (iset:min is3) 42)
(=>? (iset:min is4) 41)


;;====================================================================

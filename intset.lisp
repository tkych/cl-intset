;;;; Last modified: 2013-10-07 23:36:39 tkych

;; cl-intset/intset.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-intset/LICENSE


;;====================================================================
;; IntSet
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-intset
  (:nicknames #:intset)
  (:use :cl)
  (:shadow :subsetp :remove :delete :random :map :max :min
           :intersection :nintersection :union :nunion :set-difference
           :nset-difference :set-exclusive-or :nset-exclusive-or)
  (:export #:intset
           #:make-intset
           #:intsetp
           #:emptyp
           #:copy
           #:size
           #:clear
           #:memberp
           #:set-equal
           #:subsetp
           #:proper-subset-p
           #:add
           #:insert
           #:remove
           #:delete
           #:integers->intset
           #:intset->integers
           #:intersection
           #:nintersection
           #:set-difference
           #:nset-difference
           #:union
           #:nunion
           #:set-exclusive-or
           #:nset-exclusive-or
           #:random
           #:map
           #:for-each
           #:max
           #:min
           ))

(in-package #:cl-intset)


;;--------------------------------------------------------------------

(defstruct (intset (:constructor make-intset ())
                   (:copier nil)
                   (:print-function
                    (lambda (obj stream depth)
                      (declare (ignore depth))
                      (print-unreadable-object
                          (obj stream :type t :identity t)))))
  (bits 0 :type (integer 0 *)))

(defmacro make-new-intset (init)
  (let ((new-intset (gensym "NEW-INTSET-")))
   `(let ((,new-intset (make-intset)))
      (setf (intset-bits ,new-intset) ,init)
      ,new-intset)))

(defun intsetp (object)
  "INTSETP object => boolean"
  (typep object 'intset))

(defun emptyp (intset)
  "EMPTYP intset => boolean"
  (zerop (intset-bits intset)))

(defun copy (intset)
  "COPY intset => new-intset"
  (make-new-intset (intset-bits intset)))

(defun clear (intset)
  "CLEAR intset => modified-intset"
  (setf (intset-bits intset) 0)
  intset)

(defun size (intset)
  "SIZE intset => integer"
  (logcount (intset-bits intset)))

(defun memberp (integer intset)
  "MEMBERP integer intset => boolean"
  ;; MEMO: 2013-10-06 tkych
  ;; Should signal an error of type type-error if logbitp's index is not
  ;; a non-negative integer.
  ;; see. CLHS, Function logbitp and Error Terminology
  (and (logbitp integer (intset-bits intset))
       t))  ; ensure-boolean, c.f. quickutil

(defun set-equal (intset1 intset2)
  "SET-EQUAL intset1 intset2 => boolean"
  (= (intset-bits intset1) (intset-bits intset2)))

(defun intset:subsetp (intset1 intset2)
  "intset:SUBSETP intset1 intset2 => boolean
If `intset1' is a subset of `intset2', then return T, otherwise NIL."
  (let ((bits1 (intset-bits intset1))
        (bits2 (intset-bits intset2)))
    (= bits1 (logand bits1 bits2))))

(defun proper-subset-p (intset1 intset2)
  "PROPER-SUBSET-P intset1 intset2 => boolean
If `intset1' is a proper subset of `intset2', then return T,
otherwise NIL."
  (let ((bits1 (intset-bits intset1))
        (bits2 (intset-bits intset2)))
    (and (/= bits1 bits2)
         (= bits1 (logand bits1 bits2)))))

(defun add (integer intset)
  "ADD integer intset => new-intset
If `integer' is not a non-negative integer, it signals a type-error."
  (check-type integer (integer 0 *))
  (make-new-intset (logior (intset-bits intset)
                           (ash 2 (1- integer)))))

(defun insert (integer intset)
  "INSERT integer intset => modified-intset
If `integer' is not a non-negative integer, it signals a type-error."
  (check-type integer (integer 0 *))
  (setf (intset-bits intset)
        (logior (intset-bits intset)
                (ash 2 (1- integer))))
  intset)

(defun intset:remove (integer intset)
  "intset:REMOVE integer intset => new-intset
If `integer' is not a non-negative integer, it signals a type-error."
  (check-type integer (integer 0 *))
  (make-new-intset (logandc2 (intset-bits intset)
                             (ash 2 (1- integer)))))

(defun intset:delete (integer intset)
  "intset:DELETE integer intset => modified-intset
If `integer' is not a non-negative integer, it signals a type-error."
  (check-type integer (integer 0 *))
  (setf (intset-bits intset)
        (logandc2 (intset-bits intset)
                  (ash 2 (1- integer))))
  intset)

(defun integers->intset (integers)
  "INTEGERS->INTSET list-of-integers => intset
If `integers' contains an element which is not a non-negative integer,
it signals a type-error."
  (make-new-intset
   (reduce (lambda (x y)
             (check-type y (integer 0 *))
             (logior x (ash 2 (1- y))))
           integers :initial-value 0)))

(defun intset->integers (intset &key (desc? nil))
  "INTSET->INTEGERS intset &key (desc? nil) => list
If keyword `desc?' is a non-NIL, return descending-ordered-list,
otherwise ascending-ordered-list."
  (let ((bits (intset-bits intset)))
    (if desc?
        (loop :for i :downfrom (1- (integer-length bits)) :to 0
              :when (logbitp i bits)
                :collect i)
        (loop :for i :from 0 :below (integer-length bits)
              :when (logbitp i bits)
                :collect i))))

(defun intset:intersection (intset1 intset2)
  "intset:INTERSECTION intset1 intset2 => new-intset"
  (make-new-intset (logand (intset-bits intset1)
                           (intset-bits intset2))))

(defun intset:nintersection (intset1 intset2)
  "intset:NINTERSECTION intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07 tkych
  ;; nintersection can modify list-1, but not list-2.
  ;; see. CLHS, function nintersection
  (setf (intset-bits intset1) (logand (intset-bits intset1)
                                      (intset-bits intset2)))
  intset1)

(defun intset:set-difference (intset1 intset2)
  "intset:SET-DIFFERENCE intset1 intset2 => new-intset"
  (make-new-intset (logandc2 (intset-bits intset1)
                             (intset-bits intset2))))

(defun intset:nset-difference (intset1 intset2)
  "intset:NSET-DIFFERENCE intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07 tkych
  ;; nset-difference may destroy list-1.
  ;; see. CLHS, function nset-difference
  (setf (intset-bits intset1) (logandc2 (intset-bits intset1)
                                        (intset-bits intset2)))
  intset1)

(defun intset:union (intset1 intset2)
  "intset:UNION intset1 intset2 => new-intset"
  (make-new-intset (logior (intset-bits intset1)
                           (intset-bits intset2))))

(defun intset:nunion (intset1 intset2)
  "intset:NUNION intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07 tkych
  ;; nunion is permitted to modify any part, car or cdr, of the list
  ;; structure of list-1 or list-2.
  ;; see. CLHS, function nunion
  (setf (intset-bits intset1) (logior (intset-bits intset1)
                                      (intset-bits intset2)))
  intset1)

(defun intset:set-exclusive-or (intset1 intset2)
  "intset:SET-EXCLUSIVE-OR intset1 intset2 => new-intset"
  (make-new-intset (logxor (intset-bits intset1)
                           (intset-bits intset2))))

(defun intset:nset-exclusive-or (intset1 intset2)
  "intset:NSET-EXCLUSIVE-OR intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07 tkych
  ;; nset-exclusive-or is permitted to modify any part, car or cdr, of
  ;; the list structure of list-1 or list-2.
  ;; see. CLHS, function nset-exclusive-or
  (setf (intset-bits intset1) (logxor (intset-bits intset1)
                                      (intset-bits intset2)))
  intset1)

;; !!! UGLY:
(defun intset:random (intset)
  "intset:RANDOM intset => integer/null
If `intset' is an empty set, it returns NIL."
  (if (emptyp intset)
      nil
      (let* ((bits (intset-bits intset))
             (size (logcount bits))
             (nth  (cl:random size)))
        (loop :for i :from 0 :below (integer-length bits)
              :with j := 0
              :do (when (= j nth)
                    (RETURN i))
                  (when (logbitp i bits)
                    (incf j))))))

(defun intset:map (type function intset &key (desc? nil))
  "intset:MAP type function intset => result-object
The argument `type' must be a one of {intset list string vector}.
If `type' is intset, then it returns a new intset. If keyword `desc?'
is a non-NIL, it calls `function' with descending-ordered arguments,
otherwise ascending-ordered."
  (check-type type     (member intset list string vector))
  (check-type function function)
  (if (eq type 'intset)
      (loop :with bits0 := (intset-bits intset)
            :with bits1 := 0
            :for i :from 0 :below (integer-length bits0)
            :when (logbitp i bits0)
              :do (setf bits1
                        (logior bits1
                                (ash 2 (1- (funcall function i)))))
            :finally (return (make-new-intset bits1)))
      (cl:map type function (intset->integers intset :desc? desc?))))

(defun for-each (function intset &key (desc? nil))
  "FOR-EACH function intset => non-modified-intset
If keyword `desc?' is a non-NIL, it calls `function' with
descending-ordered arguments, otherwise ascending-ordered."
  (check-type function function)
  (let ((bits (intset-bits intset)))
    (if desc?
        (loop :for i :downfrom (1- (integer-length bits)) :to 0
              :do (when (logbitp i bits)
                    (funcall function i)))
        (dotimes (i (integer-length bits))
          (when (logbitp i bits)
            (funcall function i)))))
  intset)

(defun intset:max (intset)
  "intset:MAX intset => integer/null
If `intset' is an empty set, it returns NIL."
  (if (emptyp intset)
      nil
      (1- (integer-length (intset-bits intset)))))

(defun intset:min (intset)
  "intset:MIN intset => integer/null
If `intset' is an empty set, it returns NIL."
  (if (emptyp intset)
      nil
      (let* ((bits (intset-bits intset))
             ;; MEMO: 2013-10-07 tkych
             ;; The formula [ x & -x ] extracts the rightmost 1 from x.
             ;; e.g. (logand #b101010 (- #b101010)) => #b10
             ;; c.f. Hacker's Delight, 2-1
             ;; c.f. TAOCP, vol. 4A, p.140, (37)
             (rightmost1 (logand bits (- bits))))
        (1- (integer-length rightmost1)))))


;;====================================================================

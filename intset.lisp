;;;; Last modified: 2013-10-09 20:52:05 tkych

;; cl-intset/intset.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-intset/LICENSE


;;====================================================================
;; IntSet
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-intset
  (:nicknames #:intset #:iset)
  (:use :cl)
  (:shadow :subsetp :remove :delete :random :map :max :min
           :intersection :nintersection :union :nunion :set-difference
           :nset-difference :set-exclusive-or :nset-exclusive-or)
  (:export #:intset
           #:make-intset
           #:intsetp
           #:emptyp
           #:singletonp
           #:copy
           #:clear
           #:size
           #:memberp
           #:set-equal
           #:subsetp
           #:proper-subset-p
           #:add
           #:insert
           #:remove
           #:delete
           #:list->intset
           #:intset->list
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
;;
;; <intset> ::= [<bitset>]
;; <bitset> ::= (integer 0 *), e.g. 42 (<=> #b101010) ~ {1 3 5}

(defstruct (intset (:constructor make-intset ())
                   (:copier nil)
                   (:conc-name nil)
                   (:print-function
                    (lambda (obj stream depth)
                      (declare (ignore depth))
                      (print-unreadable-object
                          (obj stream :type t :identity t)))))
  (bitset 0 :type (integer 0 *)))

(setf (documentation 'make-intset 'function)
      "MAKE-INTSET => new-intset
It returns a new empty intset.")

(declaim (inline make-new-intset))
(defun make-new-intset (init-bitset)
  (let ((new-intset (make-intset)))
    (setf (bitset new-intset) init-bitset)
    new-intset))

(defun intsetp (object)
  "INTSETP object => boolean"
  (typep object 'intset))

(defun emptyp (intset)
  "EMPTYP intset => boolean"
  (zerop (bitset intset)))

(defun singletonp (intset)
  "SINGLETONP intset => boolean"
  ;; MEMO: 2013-10-08
  ;; The formula [ x & (x-1) ] remove the rightmost 1 from x.
  ;; If x is #b0, then return #b0.
  ;; e.g. (logand #b101010 (1- #b101010)) => #b101000
  ;; c.f. Hacker's Delight, 2-1
  ;; c.f. TAOCP, vol. 4A, p.140, (36)
  (let ((bs (bitset intset)))
    (and (not (zerop bs))
         (zerop (logand bs (1- bs))))))

(defun copy (intset)
  "COPY intset => new-intset"
  (make-new-intset (bitset intset)))

(defun clear (intset)
  "CLEAR intset => modified-intset"
  (setf (bitset intset) 0)
  intset)

(defun size (intset)
  "SIZE intset => non-negative-integer"
  (logcount (bitset intset)))

(defun memberp (non-negative-integer intset)
  "MEMBERP non-negative-integer intset => boolean"
  ;; MEMO: 2013-10-06
  ;; Should signal an error of type type-error if logbitp's index is not
  ;; a non-negative integer.
  ;; c.f. CLHS, Function logbitp and Error Terminology
  (and (logbitp non-negative-integer (bitset intset))
       t))  ; ensure-boolean, c.f. quickutil

(defun set-equal (intset1 intset2)
  "SET-EQUAL intset1 intset2 => boolean"
  (= (bitset intset1) (bitset intset2)))

(defun intset:subsetp (intset1 intset2)
  "intset:SUBSETP intset1 intset2 => boolean
If `intset1' is a subset of `intset2', it returns T, otherwise NIL."
  (let ((bs1 (bitset intset1))
        (bs2 (bitset intset2)))
    (= bs1 (logand bs1 bs2))))

(defun proper-subset-p (intset1 intset2)
  "PROPER-SUBSET-P intset1 intset2 => boolean
If `intset1' is a proper subset of `intset2', it returns T,
otherwise NIL."
  (let ((bs1 (bitset intset1))
        (bs2 (bitset intset2)))
    (and (/= bs1 bs2)
         (= bs1 (logand bs1 bs2)))))

(defun add (non-negative-integer intset)
  "ADD non-negative-integer intset => new-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (make-new-intset (logior (bitset intset)
                           (ash 2 (1- non-negative-integer)))))

(defun insert (non-negative-integer intset)
  "INSERT non-negative-integer intset => modified-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (setf (bitset intset)
        (logior (bitset intset)
                (ash 2 (1- non-negative-integer))))
  intset)

(defun intset:remove (non-negative-integer intset)
  "intset:REMOVE non-negative-integer intset => new-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (make-new-intset (logandc2 (bitset intset)
                             (ash 2 (1- non-negative-integer)))))

(defun intset:delete (non-negative-integer intset)
  "intset:DELETE non-negative-integer intset => modified-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (setf (bitset intset)
        (logandc2 (bitset intset)
                  (ash 2 (1- non-negative-integer))))
  intset)

(defun list->intset (non-negative-integers)
  "LIST->INTSET non-negative-integer-list => new-intset
If `non-negative-integers' contains an element which is not
a non-negative integer, it signals a type-error."
  (make-new-intset
   (reduce (lambda (x y)
             (check-type y (integer 0 *))
             (logior x (ash 2 (1- y))))
           non-negative-integers :initial-value 0)))

(defun intset->list (intset &key (desc? nil))
  "INTSET->LIST intset &key (desc? nil) => non-negative-integer-list
If keyword `desc?' is a non-NIL, return descending-ordered-list,
otherwise ascending-ordered-list."
  (let ((bs (bitset intset)))
    (if desc?
        (loop :for i :downfrom (1- (integer-length bs)) :to 0
              :when (logbitp i bs)
                :collect i)
        (loop :for i :from 0 :below (integer-length bs)
              :when (logbitp i bs)
                :collect i))))

(defun intset:intersection (intset1 intset2)
  "intset:INTERSECTION intset1 intset2 => new-intset"
  (make-new-intset (logand (bitset intset1)
                           (bitset intset2))))

(defun intset:nintersection (intset1 intset2)
  "intset:NINTERSECTION intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nintersection can modify list-1, but not list-2.
  ;; c.f. CLHS, function nintersection
  (setf (bitset intset1) (logand (bitset intset1)
                                 (bitset intset2)))
  intset1)

(defun intset:set-difference (intset1 intset2)
  "intset:SET-DIFFERENCE intset1 intset2 => new-intset"
  (make-new-intset (logandc2 (bitset intset1)
                             (bitset intset2))))

(defun intset:nset-difference (intset1 intset2)
  "intset:NSET-DIFFERENCE intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nset-difference may destroy list-1.
  ;; c.f. CLHS, function nset-difference
  (setf (bitset intset1) (logandc2 (bitset intset1)
                                   (bitset intset2)))
  intset1)

(defun intset:union (intset1 intset2)
  "intset:UNION intset1 intset2 => new-intset"
  (make-new-intset (logior (bitset intset1)
                           (bitset intset2))))

(defun intset:nunion (intset1 intset2)
  "intset:NUNION intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nunion is permitted to modify any part, car or cdr, of the list
  ;; structure of list-1 or list-2.
  ;; c.f. CLHS, function nunion
  (setf (bitset intset1) (logior (bitset intset1)
                                 (bitset intset2)))
  intset1)

(defun intset:set-exclusive-or (intset1 intset2)
  "intset:SET-EXCLUSIVE-OR intset1 intset2 => new-intset"
  (make-new-intset (logxor (bitset intset1)
                           (bitset intset2))))

(defun intset:nset-exclusive-or (intset1 intset2)
  "intset:NSET-EXCLUSIVE-OR intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nset-exclusive-or is permitted to modify any part, car or cdr, of
  ;; the list structure of list-1 or list-2.
  ;; c.f. CLHS, function nset-exclusive-or
  (setf (bitset intset1) (logxor (bitset intset1)
                                 (bitset intset2)))
  intset1)

(defun intset:random (intset)
  "intset:RANDOM intset => non-negative-integer/null
If `intset' is an empty set, it returns NIL."
  (if (emptyp intset)
      nil
      (let* ((bs   (bitset intset))
             (size (logcount bs))
             (nth  (cl:random size)))
        (loop :for i :from 0 :to (integer-length bs)
              :with j := -1
              :do (when (= j nth)
                    (RETURN (1- i)))
                  (when (logbitp i bs)
                    (incf j))))))

(defun intset:map (result-type function intset &key (desc? nil))
  "intset:MAP result-type function intset &key (desc? nil) => new-object
The argument `result-type' must be a one of {intset list string vector}.
If `result-type' is intset, then it returns a new intset. If keyword `desc?'
is a non-NIL, it calls `function' with descending-ordered arguments,
otherwise ascending-ordered."
  (check-type result-type (member intset list string vector))
  (check-type function    function)
  (if (eq result-type 'intset)
      (let ((bs (bitset intset))
            (new-bs 0))
        (if desc?
            (loop :for i :downfrom (1- (integer-length bs)) :to 0
                  :do (when (logbitp i bs)
                        (setf new-bs
                              (logior new-bs
                                      (ash 2 (1- (funcall function i)))))))
            (loop :for i :from 0 :below (integer-length bs)
                  :do (when (logbitp i bs)
                        (setf new-bs
                              (logior new-bs
                                      (ash 2 (1- (funcall function i))))))))
        (make-new-intset new-bs))
      (cl:map result-type function (intset->list intset :desc? desc?))))

(defun for-each (function intset &key (desc? nil))
  "FOR-EACH function intset &key (desc? nil) => non-modified-intset
If keyword `desc?' is a non-NIL, it calls `function' with
descending-ordered arguments, otherwise ascending-ordered."
  (check-type function function)
  (let ((bs (bitset intset)))
    (if desc?
        (loop :for i :downfrom (1- (integer-length bs)) :to 0
              :do (when (logbitp i bs)
                    (funcall function i)))
        (loop :for i :from 0 :below (integer-length bs)
              :do (when (logbitp i bs)
                    (funcall function i)))))
  intset)

(defun intset:max (intset)
  "intset:MAX intset => non-negative-integer/null
If `intset' is an empty set, it returns NIL."
  (let ((bs (bitset intset)))
    (if (zerop bs)
        nil
        (1- (integer-length bs)))))

(defun intset:min (intset)
  "intset:MIN intset => non-negative-integer/null
If `intset' is an empty set, it returns NIL."
  (let ((bs (bitset intset)))
    (if (zerop bs)
        nil
        ;; MEMO: 2013-10-07
        ;; The formula [ x & -x ] extracts the rightmost 1 from x.
        ;; If x is #b0, then return #b0.
        ;; e.g. (logand #b101010 (- #b101010)) => #b10
        ;; c.f. Hacker's Delight, 2-1
        ;; c.f. TAOCP, vol. 4A, p.140, (37)
        (let ((rightmost1 (logand bs (- bs))))
          (1- (integer-length rightmost1))))))


;;====================================================================

;;;; Last modified: 2013-10-09 19:14:41 tkych

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
;; <intset> ::= [<bitmap>]
;; <bitmap> ::= (integer 0 *), e.g. 42 (<=> #b101010) ~ {1 3 5}

(defstruct (intset (:constructor make-intset ())
                   (:copier nil)
                   (:conc-name nil)
                   (:print-function
                    (lambda (obj stream depth)
                      (declare (ignore depth))
                      (print-unreadable-object
                          (obj stream :type t :identity t)))))
  (bitmap 0 :type (integer 0 *)))

(setf (documentation 'make-intset 'function)
      "MAKE-INTSET => new-intset
It returns a new empty intset.")

(declaim (inline make-new-intset))
(defun make-new-intset (init-bitmap)
  (let ((new-intset (make-intset)))
    (setf (bitmap new-intset) init-bitmap)
    new-intset))

(defun intsetp (object)
  "INTSETP object => boolean"
  (typep object 'intset))

(defun emptyp (intset)
  "EMPTYP intset => boolean"
  (zerop (bitmap intset)))

(defun singletonp (intset)
  "SINGLETONP intset => boolean"
  ;; MEMO: 2013-10-08
  ;; The formula [ x & (x-1) ] remove the rightmost 1 from x.
  ;; If x is #b0, then return #b0.
  ;; e.g. (logand #b101010 (1- #b101010)) => #b101000
  ;; c.f. Hacker's Delight, 2-1
  ;; c.f. TAOCP, vol. 4A, p.140, (36)
  (let ((bm (bitmap intset)))
    (and (not (zerop bm))
         (zerop (logand bm (1- bm))))))

(defun copy (intset)
  "COPY intset => new-intset"
  (make-new-intset (bitmap intset)))

(defun clear (intset)
  "CLEAR intset => modified-intset"
  (setf (bitmap intset) 0)
  intset)

(defun size (intset)
  "SIZE intset => non-negative-integer"
  (logcount (bitmap intset)))

(defun memberp (non-negative-integer intset)
  "MEMBERP non-negative-integer intset => boolean"
  ;; MEMO: 2013-10-06
  ;; Should signal an error of type type-error if logbitp's index is not
  ;; a non-negative integer.
  ;; c.f. CLHS, Function logbitp and Error Terminology
  (and (logbitp non-negative-integer (bitmap intset))
       t))  ; ensure-boolean, c.f. quickutil

(defun set-equal (intset1 intset2)
  "SET-EQUAL intset1 intset2 => boolean"
  (= (bitmap intset1) (bitmap intset2)))

(defun intset:subsetp (intset1 intset2)
  "intset:SUBSETP intset1 intset2 => boolean
If `intset1' is a subset of `intset2', it returns T, otherwise NIL."
  (let ((bm1 (bitmap intset1))
        (bm2 (bitmap intset2)))
    (= bm1 (logand bm1 bm2))))

(defun proper-subset-p (intset1 intset2)
  "PROPER-SUBSET-P intset1 intset2 => boolean
If `intset1' is a proper subset of `intset2', it returns T,
otherwise NIL."
  (let ((bm1 (bitmap intset1))
        (bm2 (bitmap intset2)))
    (and (/= bm1 bm2)
         (= bm1 (logand bm1 bm2)))))

(defun add (non-negative-integer intset)
  "ADD non-negative-integer intset => new-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (make-new-intset (logior (bitmap intset)
                           (ash 2 (1- non-negative-integer)))))

(defun insert (non-negative-integer intset)
  "INSERT non-negative-integer intset => modified-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (setf (bitmap intset)
        (logior (bitmap intset)
                (ash 2 (1- non-negative-integer))))
  intset)

(defun intset:remove (non-negative-integer intset)
  "intset:REMOVE non-negative-integer intset => new-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (make-new-intset (logandc2 (bitmap intset)
                             (ash 2 (1- non-negative-integer)))))

(defun intset:delete (non-negative-integer intset)
  "intset:DELETE non-negative-integer intset => modified-intset
If `non-negative-integer' is not a non-negative integer, it signals a
type-error."
  (check-type non-negative-integer (integer 0 *))
  (setf (bitmap intset)
        (logandc2 (bitmap intset)
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
  (let ((bm (bitmap intset)))
    (if desc?
        (loop :for i :downfrom (1- (integer-length bm)) :to 0
              :when (logbitp i bm)
                :collect i)
        (loop :for i :from 0 :below (integer-length bm)
              :when (logbitp i bm)
                :collect i))))

(defun intset:intersection (intset1 intset2)
  "intset:INTERSECTION intset1 intset2 => new-intset"
  (make-new-intset (logand (bitmap intset1)
                           (bitmap intset2))))

(defun intset:nintersection (intset1 intset2)
  "intset:NINTERSECTION intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nintersection can modify list-1, but not list-2.
  ;; c.f. CLHS, function nintersection
  (setf (bitmap intset1) (logand (bitmap intset1)
                                 (bitmap intset2)))
  intset1)

(defun intset:set-difference (intset1 intset2)
  "intset:SET-DIFFERENCE intset1 intset2 => new-intset"
  (make-new-intset (logandc2 (bitmap intset1)
                             (bitmap intset2))))

(defun intset:nset-difference (intset1 intset2)
  "intset:NSET-DIFFERENCE intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nset-difference may destroy list-1.
  ;; c.f. CLHS, function nset-difference
  (setf (bitmap intset1) (logandc2 (bitmap intset1)
                                   (bitmap intset2)))
  intset1)

(defun intset:union (intset1 intset2)
  "intset:UNION intset1 intset2 => new-intset"
  (make-new-intset (logior (bitmap intset1)
                           (bitmap intset2))))

(defun intset:nunion (intset1 intset2)
  "intset:NUNION intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nunion is permitted to modify any part, car or cdr, of the list
  ;; structure of list-1 or list-2.
  ;; c.f. CLHS, function nunion
  (setf (bitmap intset1) (logior (bitmap intset1)
                                 (bitmap intset2)))
  intset1)

(defun intset:set-exclusive-or (intset1 intset2)
  "intset:SET-EXCLUSIVE-OR intset1 intset2 => new-intset"
  (make-new-intset (logxor (bitmap intset1)
                           (bitmap intset2))))

(defun intset:nset-exclusive-or (intset1 intset2)
  "intset:NSET-EXCLUSIVE-OR intset1 intset2 => modified-intset1"
  ;; MEMO: 2013-10-07
  ;; nset-exclusive-or is permitted to modify any part, car or cdr, of
  ;; the list structure of list-1 or list-2.
  ;; c.f. CLHS, function nset-exclusive-or
  (setf (bitmap intset1) (logxor (bitmap intset1)
                                 (bitmap intset2)))
  intset1)

(defun intset:random (intset)
  "intset:RANDOM intset => non-negative-integer/null
If `intset' is an empty set, it returns NIL."
  (if (emptyp intset)
      nil
      (let* ((bm   (bitmap intset))
             (size (logcount bm))
             (nth  (cl:random size)))
        (loop :for i :from 0 :to (integer-length bm)
              :with j := -1
              :do (when (= j nth)
                    (RETURN (1- i)))
                  (when (logbitp i bm)
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
      (let ((bm (bitmap intset))
            (new-bm 0))
        (if desc?
            (loop :for i :downfrom (1- (integer-length bm)) :to 0
                  :do (when (logbitp i bm)
                        (setf new-bm
                              (logior new-bm
                                      (ash 2 (1- (funcall function i)))))))
            (loop :for i :from 0 :below (integer-length bm)
                  :do (when (logbitp i bm)
                        (setf new-bm
                              (logior new-bm
                                      (ash 2 (1- (funcall function i))))))))
        (make-new-intset new-bm))
      (cl:map result-type function (intset->list intset :desc? desc?))))

(defun for-each (function intset &key (desc? nil))
  "FOR-EACH function intset &key (desc? nil) => non-modified-intset
If keyword `desc?' is a non-NIL, it calls `function' with
descending-ordered arguments, otherwise ascending-ordered."
  (check-type function function)
  (let ((bm (bitmap intset)))
    (if desc?
        (loop :for i :downfrom (1- (integer-length bm)) :to 0
              :do (when (logbitp i bm)
                    (funcall function i)))
        (loop :for i :from 0 :below (integer-length bm)
              :do (when (logbitp i bm)
                    (funcall function i)))))
  intset)

(defun intset:max (intset)
  "intset:MAX intset => non-negative-integer/null
If `intset' is an empty set, it returns NIL."
  (let ((bm (bitmap intset)))
    (if (zerop bm)
        nil
        (1- (integer-length bm)))))

(defun intset:min (intset)
  "intset:MIN intset => non-negative-integer/null
If `intset' is an empty set, it returns NIL."
  (let ((bm (bitmap intset)))
    (if (zerop bm)
        nil
        ;; MEMO: 2013-10-07
        ;; The formula [ x & -x ] extracts the rightmost 1 from x.
        ;; If x is #b0, then return #b0.
        ;; e.g. (logand #b101010 (- #b101010)) => #b10
        ;; c.f. Hacker's Delight, 2-1
        ;; c.f. TAOCP, vol. 4A, p.140, (37)
        (let ((rightmost1 (logand bm (- bm))))
          (1- (integer-length rightmost1))))))


;;====================================================================

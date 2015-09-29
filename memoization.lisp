#+xcvb (module ())

(cl:defpackage #:fare-memoization
  (:nicknames #:fmemo)
  (:use #:common-lisp)
  (:export #:memoize #:unmemoize #:unmemoize-1
           #:define-memo-function #:memoizing
           #:memoized-funcall #:memoized-apply #:*memoized*))

(in-package :fare-memoization)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass memoization-info ()
  ((function
    :initarg :function :reader memoized-function
    :documentation "The original, unmemoized, function")
   (wrapped-function
    :accessor wrapped-function
    :documentation "The memoizing version of the function.")
   (table
    :initarg :table :reader memoized-table :initform (make-hash-table :test 'equal)
    :documentation "a hash-table containing the memoized computations so far")
   (normalization
    :initarg :normalization :reader memoized-normalization :initform nil
    :documentation "either NIL or a function to normalize arguments before memoization"))
  (:documentation "information about a function that was memoized"))

(defun compute-memoized-function (info arguments)
  "the basic helper for computing with a memoized function FUNCTION,
with a hash-table TABLE, being called with arguments ARGUMENTS"
  (with-slots (function table normalization) info
    (flet ((f (&rest arguments)
             (multiple-value-bind (results foundp) (gethash arguments table)
               (if foundp (apply #'values results)
                   (let ((results (multiple-value-list (apply function arguments))))
                     (setf (gethash arguments table) results)
                     (apply #'values results))))))
      (if normalization (apply normalization #'f arguments)
          (apply #'f arguments)))))

(defun unmemoize-1 (symbol &rest arguments)
  "Forget the memoized result of calling SYMBOL with arguments ARGUMENTS.
Do not forget memoized results of calling the function with other arguments.
Returns T if a stored result was found and removed, NIL otherwise."
 (let ((info (get symbol 'memoization-info)))
   (when info
     (assert (typep info 'memoization-info))
     (with-slots (function table normalization) info
       (flet ((f (&rest arguments)
                (multiple-value-bind (results foundp) (gethash arguments table)
                  (declare (ignore results))
                  (remhash arguments table)
                  foundp)))
         (if normalization (apply normalization #'f arguments)
             (apply #'f arguments)))))))

(defun unmemoize (symbol)
  "undoing the memoizing function, return the memoization-info record for the function"
  (let ((info (get symbol 'memoization-info)))
    (when info
      (assert (typep info 'memoization-info))
      (unless (eq (wrapped-function info) (symbol-function symbol))
        (cerror "Restore the function to pre-memoization state anyway"
                "Tried to unmemoize ~S, but the symbol's function was changed since. ~
                Maybe you need to UNTRACE it first, or cancel some ADVICE; ~
                or maybe you failed to unmemoize before you redefined it, ~
                and will need to re-redefine it after restoring the old version."
                symbol))
      (setf (symbol-function symbol) (memoized-function info))
      (remprop symbol 'memoization-info)
      info)))

(defun memoize (symbol &key (table (make-hash-table :test 'equal)) normalization)
  "Memoize the function associated to given SYMBOL.

Beware that unless the function was declared NOTINLINE, callers may have inlined
the original definition and will not see the memoized function.
Moreover, if the function is self-recursive,
this declaration must have happened before it was defined.

Keyword argument TABLE (default: a fresh EQUAL hash-table) lets you
specify an existing hash-table for the memoized computations;
it may have been created with appropriate options regarding equality predicate
and weak pointers, initial contents, etc., and you may clear it when needed.
Keyword argument NORMALIZATION (default: NIL) lets you specify a function
taking a continuation and the function arguments,
e.g. with lambda-list (CONTINUATION &REST ARGUMENTS)
which may transform the argument list before to call the continuation
with a normalized argument list that will be used to query the computation cache
and invoke the actual computation function; NIL means no such transformation,
which has the same effect as specifying #'APPLY as a transformation.

If the function was already being memoized, any previous memoization information,
i.e. TABLE and NORMALIZATION, is replaced with the newly specified values."
  (unmemoize symbol)
  (let* ((function (symbol-function symbol))
         (info (make-instance 'memoization-info
                 :function function :table table
                 :normalization normalization)))
    (setf (symbol-function symbol) #'(lambda (&rest args)
                                       (compute-memoized-function info args))
          (wrapped-function info) (symbol-function symbol)
          (get symbol 'memoization-info) info)))

(defmacro define-memo-function (name formals &body body)
  "Like defun, but creates a memoized function.
Also, if the name is a CONS, then the first element is the name, and the rest
is a list of keyword arguments, TABLE and NORMALIZATION as per MEMOIZE."
  (multiple-value-bind (name keys)
      (if (consp name) (values (car name) (cdr name)) (values name ()))
    `(progn
       (declaim (notinline ,name))
       (unmemoize ',name)
       (defun ,name ,formals
         ,@body)
       (memoize ',name ,@keys))))

(defun memoizing (function &rest keys &key table normalization)
  "Given a function, return a memoizing version of same function.
Keyword arguments TABLE and NORMALIZATION are as per MEMOIZE."
  (declare (ignore table normalization))
  (let ((info (apply 'make-instance 'memoization-info :function function keys)))
    (setf (wrapped-function info)
          #'(lambda (&rest arguments) (compute-memoized-function info arguments)))))

;;; This is your generic memoized function.
;;; If you want to make sure that a given function is only ever called once
;;; with the "same" list of arguments and thus ensure that it always returns
;;; the same value for a "same" list of arguments, it is up to YOU
;;; to normalize the arguments of the function you call such that EQUAL
;;; will properly compare argument lists. You may pass any additional
;;; arguments that you don't want memoized in dynamic variable bindings.
(defvar *memoized* (make-hash-table :test 'equal))

(define-memo-function (memoized-funcall :table *memoized*) (function &rest arguments)
  "This is a generic memoized function"
  (apply function arguments))

(defun memoized-apply (function &rest arguments)
  (apply #'apply #'memoized-funcall function arguments))

);eval-when

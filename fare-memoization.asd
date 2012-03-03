;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :fare-memoization
  :description "Simple library for memoizing functions"
  :long-description "define memoized functions and memoize previously defined functions"
  :components
  ((:file "memoization")))

(defmethod perform ((op test-op) (system (eql (find-system :fare-memoization))))
  (asdf:load-system :fare-memoization-test)
  (funcall (asdf::find-symbol* :test-memoization :fare-memoization-test)))

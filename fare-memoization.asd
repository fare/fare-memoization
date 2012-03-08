;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :fare-memoization
  :description "memoizing functions the correct, portable way"
  :long-description "define memoized functions and memoize previously defined functions"
  :license "MIT"
  :components
  ((:file "memoization")))

(defmethod perform ((op test-op) (system (eql (find-system :fare-memoization))))
  (asdf:load-system :fare-memoization-test)
  (funcall (asdf::find-symbol* :test-memoization :fare-memoization-test)))

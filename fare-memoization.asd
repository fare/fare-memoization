;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem "fare-memoization"
  :description "memoizing functions the correct, portable way"
  :long-description "define memoized functions and memoize previously defined functions"
  :license "MIT"
  :author "Francois-Rene Rideau"
  :version "1.0.0"
  :components
  ((:file "memoization"))
  :in-order-to ((test-op (load-op "fare-memoization/test")))
  :perform (test-op :after (o c)
             (symbol-call :fare-memoization-test :test-memoization)))

(defsystem "fare-memoization/test"
  :description "memoizing functions the correct, portable way"
  :license "MIT"
  :author "Francois-Rene Rideau"
  :version "1.0.0"
  :depends-on ("fare-memoization" "hu.dwim.stefil")
  :components
  ((:file "memoization-test")))

;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :fare-memoization-test
  :depends-on (:fare-memoization :hu.dwim.stefil)
  :components
  ((:file "memoization-test")))

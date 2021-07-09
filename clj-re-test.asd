(in-package :cl-user)

(defpackage :clj-re-asd-test
  (:use :cl :asdf))

(in-package :clj-re-asd-test)

(defsystem :clj-re-test
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Tests for :clj-re"
  :depends-on (:clj-re :fiveam)
  :components ((:file "clj-re-test")))

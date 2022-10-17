(in-package :cl-user)

(defpackage :clj-re-asd
  (:use :cl :asdf))

(in-package :clj-re-asd)

(defsystem :clj-re
  :version "1.0.2"
  :license "MIT"
  :author "Dave Tenny"
  :description "Implements Clojure-styled regexp operations such as `re-matches` and `re-find`."
  :depends-on (:cl-ppcre :named-readtables)
  :serial t
  :components ((:file "package")
               (:file "clj-re")))

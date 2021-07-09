(in-package :cl-user)

(defpackage :clj-re-asd
  (:use :cl :asdf))

(in-package :clj-re-asd)

(defsystem :clj-re
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Implements Clojure-styled regexp operations such as `re-matches` and `re-find`."
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "clj-re")))

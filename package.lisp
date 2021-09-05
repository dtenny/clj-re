(in-package :cl-user)

(defpackage :clj-re
  (:use :cl :cl-ppcre)
  (:documentation "Functions that implement Clojure style regexp operations.")
  (:export
   ;; Functions taking regular expressions
   #:re-find
   #:re-groups
   #:re-matcher
   #:re-matches
   #:re-pattern
   #:re-quote-replacement               ;ala clojure.string/re-quotareplacement
   #:re-replace                         ;ala clojure.string/replace, not clojure.core/replace
   #:re-replace-first                   ;ala clojure.string/replace-first
   #:re-seq
   #:re-split                           ;ala clojure.string/split

   ;; Optional readtable support
   #:readtable
   #:readtable-mixin
   ))



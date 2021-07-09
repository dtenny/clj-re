(in-package :cl-user)

(defpackage :clj-re-test
  (:use :cl :fiveam :clj-re)
  (:export #:run-tests)
  (:documentation "Tests for the :clj-re package."))

(in-package :clj-re-test)

(def-suite test-suite :description ":clj-re tests")
(in-suite test-suite)

(test re-find                                 ;and implicitly, re-matcher, re-groups
  (is (equalp "aaab" (re-find "a*b" "aaab"))) ;no registers, this is how clojure works :-/
  (is (equalp "aaab" (re-find "a*b" "aaabc")))
  (is (equalp "aaab" (re-find "a*b" "caaabc")))
  (is (equalp '("aaab" "b") (re-find "a*(b)" "aaab")))
  (is (equalp '("aaab" "b") (re-find "a*(b)" "caaabc")))
  (is (equalp '("aaab" "b") (re-find (re-matcher "a*(b)" "aaab"))))
  (let ((m (re-matcher "a*(b)" "aabcaaab")))
    (is (equalp '("aab" "b") (re-find m)))
    (is (equalp '("aaab" "b") (re-find m)))
    (is (null (re-find m))))) 

(test re-matches                        ;match full input, vs re-find's "find anywhere in input"
  (is (equal "aaab" (re-matches "a*b" "aaab")))
  (is (null (re-matches "a*b" "aaabc")))
  (is (null (re-matches "a*b" "aaabcd")))
  (is (null (re-matches "a*b" "baaab")))
  (is (equal '("aaab" "b") (re-matches "a*(b)" "aaab"))))

(test re-pattern
  (let ((s "ab*c"))
    (is (eq s (re-pattern s)))))

(test re-quote-replacement
  (is (string= "\\$1" (re-quote-replacement "$1")))
  (is (string= "ab\\$2\\$3" (re-quote-replacement "ab$2$3"))))

(test clojure-replacement-translation
  (is (string= "\\1abc\\2def\\3" (clj-re::clojure-replacement-translation "$1abc$2def$3")))
  (is (string= "\\1" (clj-re::clojure-replacement-translation "$1")))
  ;; Escaped as per re-quote-replacement, shouldn't be translated to cl-ppcre notation
  (is (string= "$1" (clj-re::clojure-replacement-translation "\\$1")))
  (is (string= "abc$1def" (clj-re::clojure-replacement-translation "abc\\$1def"))))

(test ppcre-replacement-quoter
  (is (string= "abc" (clj-re::ppcre-replacement-quoter "abc")))
  (is (string= "abc\\\\1\\\\{2}\\\\&\\\\`\\\\'" 
               (clj-re::ppcre-replacement-quoter "abc\\1\\{2}\\&\\`\\'")))
  (is (string= "\\\\'" (clj-re::ppcre-replacement-quoter "\\'")))
  ;; \\n isn't actually the pattern, it has to be a number, sanity check
  (is (string= "\\\\2\\n" (clj-re::ppcre-replacement-quoter "\\2\\n"))))

(test re-replace
  ;; Many of these are adapted examples from https://clojuredocs.org/clojure.string/replace
  ;; (with double-escapes where necessary and string vs regex literal syntax).
  (is (string= "The color is blue" (re-replace "The color is red" "red" "blue")))
  (is (string= "lmostAay igPay atinLay"
               (re-replace "Almost Pig Latin" "\\b(\\w)(\\w+)\\b" "$2$1ay"))) 
  ;; No registers, with functional replacement
  (is (string= "Thee cooloor iis reed."
               (re-replace "The color is red." "[aeiou]" 
                           (lambda (match)
                             (concatenate 'string match match)))))
  ;; With registers and functional replacement
  (is (string= "Thee cooloor iis reed."
               (re-replace "The color is red." "([aeiou])" 
                           (lambda (match-with-regs)
                             (is (string= (first match-with-regs) (second match-with-regs)))
                             (concatenate 'string 
                                          (first match-with-regs)
                                          (first match-with-regs))))))

  (is (string= "fabulous ddero oo doo"
               (re-replace "fabulous fodder foo food" "f(o+)(\\S+)" "$2$1")))
  (is (string= "fabulous $2$1 $2$1 $2$1"
               (re-replace "fabulous fodder foo food" "f(o+)(\\S+)" "\\$2\\$1")))
  (is (string= "fabulous $2$1 $2$1 $2$1"
               (re-replace "fabulous fodder foo food" "f(o+)(\\S+)" 
                           (re-quote-replacement "$2$1"))))
  (is (string= "1 2 1" 
               (re-replace "a b a" "a|b" 
                           ;; In Clojure the 'function' was {"a" "1" "b" "2"}
                           (lambda (match)
                             (cdr (assoc match '(("a" . "1") ("b" . "2")) :test #'string=))))))
  (is (string= "Hello World"
               (re-replace "hello world" "\\b." #'string-upcase)))

  ;; Character operands
  (is (string= "aaaa" (re-replace "abba" #\b #\a)))
  (is (string= "aaaaaa" (re-replace "abba" #\b "aa")))

  ;; No match
  (is (string= "Vegeta" (re-replace "Vegeta" "Goku" "Gohan"))))

(test re-replace-first
  (is (string= "A good night to you, sir.  Good day."
               (re-replace-first "A good day to you, sir.  Good day." "day" "night")))
  (is (string= "A good day to you, sir."
               (re-replace-first "A good day to you, sir." "madam" "master")))
  (is (string= "night need not be SHOUTED."
               (re-replace-first "Day need not be SHOUTED." "(?i)day" "night")))
  (is (string= "name" (re-replace-first "/path/to/file/name" "^.*/" "")))
  (is (string= "path/to/file/name"
               (re-replace-first "/path/to/file/name" "^.*?/" "")))
  (is (string= "fabulous ddero foo food"
               (re-replace-first "fabulous fodder foo food" "f(o+)(\\S+)" "$2$1")))
  (is (string= "fabulous $2$1 foo food"
               (re-replace-first "fabulous fodder foo food" "f(o+)(\\S+)" "\\$2\\$1"))))

(test re-seq
  (is (equalp '("1" "1" "0")
               (re-seq "\\d" "clojure 1.1.0")))
  (is (equalp '("mary" "had" "a" "little" "lamb")
              (re-seq "\\w+" "mary had a little lamb")))
  (is (equalp '(("pkts:18" "pkts" "18") ("err:5" "err" "5") ("drop:48" "drop" "48"))
              (re-seq "(\\S+):(\\d+)" " RX pkts:18 err:5 drop:48")))
  (is (equalp '("Manish" "Kumar" "12332")
              (re-seq "[A-Z][a-z]+|[0-9]+" "ManishKumar12332"))))

(test re-split
  (is (equalp '("Clojure" "is" "awesome!")
              (re-split "Clojure is awesome!" " ")))
  (is (equalp '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
              (re-split "q1w2e3r4t5y6u7i8o9p0" "\\d+")))
  (is (equalp '("q" "w" "e" "r" "t5y6u7i8o9p0")
              (re-split "q1w2e3r4t5y6u7i8o9p0" "\\d+" 5)))
  (is (equalp '(" " "q" "1" "w" "2" " ")
              (re-split " q1w2 " "")))
  ;; (?=foo) Lookahead a.k.a. "zero width" matches, doesn't move the match cursor.
  ;; The engine still has to advance after each match though.
  (is (equalp '(" something and " "A" "Camel" "Name ")
              (re-split " something and ACamelName " "(?=[A-Z])")))
  ;; Pattern not found, get original string back
  (is (equalp '("a")
              (re-split "a" "b")))
  (is (equalp '(" ")
              (re-split " " "b")))

  (is (equalp '("")
              (re-split "" "b")))       ;cl-ppcre:split would return nil

  (is (equalp '() (re-split "a" "a")))  ;clojure => [], we return (), which is nil.

  (is (equalp '() (re-split "aaa" "a")))

  (is (equalp '("") (re-split "" "")))
  
  (is (equalp '("Some" "words" "with" "other" "whitespace")
              ;; Tab between 'words' 'with'
              (re-split "Some    words   with	other whitespace      
" "\\s+")))

  (is (equalp '("" "Leading" "whitespace" "causes" "empty" "first" "string")
              (re-split "   Leading whitespace causes empty first string" "\\s+")))
  (is (equalp '("Some" "" "" "" "words" "" "" "with" "other" "whitespace")
              (re-split "Some    words   with	other whitespace      
" "\\s")))

  ;; Empty strings at the end are omitted.
  (is (equalp '("root" "" "0" "0" "admin")
              (re-split "root::0:0:admin::" ":")))

  ;; A negative limit will return trailing empty strings.
  (is (equalp '("root" "" "0" "0" "admin" "" "")
              (re-split "root::0:0:admin::" ":" -1)))

  (is (equalp '("1321" "61")
              (re-split "1321-61" "-"))))

(defun run-tests ()
  "Run all :clj-re tests."
  (explain! (run 'test-suite)))


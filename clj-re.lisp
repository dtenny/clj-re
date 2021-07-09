(in-package :clj-re)

;;   #:re-quote-replacement               ;ala clojure.string/re-quote-replacement
;;   #:re-replace                         ;ala clojure.string/replace, not clojure.core/replace
;;   #:re-replace-first                   ;ala clojure.string/replace-first
;;   #:re-seq
;;   #:re-split                           ;ala clojure.string/split

;;; Notes to self in trying to figure out API equivalents:

;;; The sequence of things in java tends to be
;;; * pattern.compile(<pattern>)
;;; * pattern.matcher(<string-to-match>) - to get a matcher
;;; * matcher.matches() - to do the matching
;;;
;;; Pattern.matches(<pattern>, <string>) is a shortcut of the above for patterns used only once.

;;; The sequence of things in cl-ppcre is:
;;; * (create-scanner regexp) - returns a (compiled) closure/function (if regexp is constant)
;;; * (scan regexp string) - invokes scan on a compiled implicitly created scanner for regexp
;;;                        i.e. (SCAN (LOAD-TIME-VALUE (CREATE-SCANNER "a*b")) "aaaab")
;;;                        So scan basically really operates on scanner functions.

;;; DIFFERENCES between clojure/java & cl-ppcre.
;;; 1.'Matcher' is always a stateful binding of a compiled pattern to a string.
;;; It can be used as an iterator of sorts, across successive matches in the string.
;;; A ppcre scanner is basically just the compiled pattern, it doesn't retain state across
;;; match operations, but can return and accept start/end data that facilitate iteration.
;;; 2. Clojure/java uses Patterns to represent regex, cl-pprcre uses strings or parse trees.
;;;
;;; HOW WE BRIDGE THE DIFFERENCES.
;;; 1. In order to bridge the semantic gap relied upon by Clojure with Matcher's ties to a string
;;;    we use an intermediate 'matcher' object.
;;; 2. We use strings for regexps, they will require double-escaping for some constructs, 
;;;    e.g. "\\d" and not just "\d".
;;; 3. We return lists where clojure would return vectors.

;;; Oversights:
;;; 1. I didn't implement support for named capture groups, though cl-ppcre supports
;;;    named registers. This was simply laziness on my part.

(defstruct matcher
  "Emulate java Matcher binding to a string to be scanned."
  (scanner)
  (string "" :type string)
  ;; Match state from a scan, set only when matcher.next is called
  (done? nil)                           ;T if last call to scanner returned nil
  (match-start 0 :type integer)
  (match-end 0 :type integer)
  (reg-starts #() :type (array integer))
  (reg-ends #() :type (array integer)))

(defun next (matcher)
  "Find the next match, return nil if there aren't any T if there are, in which case
  the matcher values are updated."
  (declare (type matcher matcher))
  (if (matcher-done? matcher)
      nil
      (multiple-value-bind (start end regstarts regends)
          (scan (matcher-scanner matcher) (matcher-string matcher)
                :start (matcher-match-end matcher))
        (if (eql start nil)
            (progn (setf (matcher-done? matcher) t)
                   nil)
            (progn (setf (matcher-match-start matcher) start
                         (matcher-match-end matcher) end
                         (matcher-reg-starts matcher) regstarts
                         (matcher-reg-ends matcher) regends)
                   t)))))

(defun re-groups (matcher)
  "Returns the groups from the most recent call to `re-find` or `re-matches`. If there are no
  nested groups, returns a string of the entire match. If there are
  nested groups, returns a list of the groups, the first element
  being the entire match.  Returns nil if there is no match indicated."
  (declare (type matcher matcher))
  (and (not (matcher-done? matcher))
       (let* ((source (matcher-string matcher))
              (whole (subseq source (matcher-match-start matcher) (matcher-match-end matcher)))
              (regs (loop for s across (matcher-reg-starts matcher)
                          for e across (matcher-reg-ends matcher)
                          collecting (subseq source s e))))
         (if (= 0 (length regs))
             whole
             (concatenate 'list (list whole) regs)))))

(defun re-find (matcher-or-regexp &optional string)
  ;;([m] [re s])
  "Attempts to find the next subsequence of the input sequence that matches the pattern,
  as per java.util.regex.Matcher.find(). Uses re-groups to return the groups.

  Returns:
  * If there no match, nil.
  * If there is a match, but no groups, returns the match.  E.g.
    (re-find \"a*b\" \"ab\") => \"ab\"
  * If there is a match groups are involved, returns a list whose car is the full match,
    an whose remaining elements are the matched groups.

  Call as `(re-find matcher)` or `(re-find regexp string)`.

  Note that repeated calls to a matcher act like an iterator, while repeated calls with
  regexp and string arguments do not."
  (declare (type (or matcher string) matcher-or-regexp)
           (type (or string null) string))
  (let ((matcher (if string
                     (re-matcher matcher-or-regexp string)
                     matcher-or-regexp)))
    (and (next matcher)
         (re-groups matcher))))

(defun re-matches (regexp string)
  "Attempts to match the _entire region_ of string against the pattern as per 
  java.util.regex.Matcher.matches(). Uses re-groups to return the
  groups. Returns nil if the pattern doesn't match the entire string."
  (declare (type string regexp string))
  (let ((matcher (re-matcher regexp string)))
    (if (and (next matcher)
             (= (matcher-match-end matcher)
                (length string)))
        (re-groups matcher)
        nil)))

(defun re-matcher (regexp string)
  ;;([re s])
  "Returns a matcher for use in operations such as `re-groups` or `re-find`."

  ;; No need to expose create-scanner's keywords
  ;; => &key case-insensitive-mode multi-line-mode single-line-mode extended-mode 
  ;; You can specify them via (?imsx) embedded regexp modifiers.
  ;; 
  ;; Java/Clojure also has the following, cl-ppcre status *TBD*: 
  ;; 'd' => only the '\n' line terminator is recognized in the behavior of ., ^, and $. 
  ;; 'u' => case-insensitive matching, when enabled by the CASE_INSENSITIVE flag,
  ;;        is done in a manner consistent with the Unicode Standard. By default, 
  ;;        case-insensitive matching assumes that only characters in the US-ASCII charset
  ;;        are being matched. 
  ;; 'U' => implies UNICODE_CASE, that is, it enables Unicode-aware case folding. 

  ;;*TBD*: Bind *allow-named-registers* to true to match java?
  (make-matcher :scanner (create-scanner regexp) :string string))


(defun re-pattern (s)
  "In Clojure: Returns an instance of java.util.regex.Pattern, for use, e.g. in re-matcher.
  In Common Lisp: just returns the input string, as we don't currently have compiled pattern
  API.  If we did we'd return a cl-ppcre scanner. Meanwhile, 's' can be passed to re-matcher."
  s)
  
(defun re-quote-replacement (replacement-string)
  "In clojure this would be `clojure.string/re-quote-replacement`.

  Given a replacement string that you wish to be a literal
  replacement for a pattern match in `re-replace` or `re-replace-first`, do the
  necessary escaping of special characters in the replacement."
  ;; This is not the same as ppcre-replacement-quoter, which is quoting cl-ppcre directives.
  ;; What we want in _this_ function is to quote the clojure directives, e.g. $1.
  ;; ... I think.  Not supporting ${name} or $g right now, per  Matcher.appendReplacement
  ;; Note that $0 is the whole match, $1-9 are registers. 9 is the maximum value.
  (regex-replace-all "\\$(\\d)" replacement-string "\\\\$\\1"))

(defun clojure-replacement-translation (replacement-string)
  "Translate clojure-style replacement operations, e.g. $1, into cl-ppcre replacement operations
  e.g. \\1.  Do NOT do the translations if the replacement string has ben quoted by the user
  via re-quote-replacement.  Presently assumes repalcement-string has NOT been quoted
  with cl-ppcre:quote-meta-chars, but more selectively quoted just to 'literalize' 
  cl-ppcre replacement directives like \N."
  (regex-replace-all "\\\\?\\$(\\d)" replacement-string 
                     (lambda (match &rest regs)
                       (if (char= (elt match 0) #\\)
                           ;; Don't emit the escape, just the escaped '$1' or whatever
                           (subseq match 1)        ;was escaped with `re-quote-replacement
                           (concatenate 'string "\\" (first regs))))
                     :simple-calls t))

(defun ppcre-replacement-quoter (replacement)
  "Given a string which may contain replacement directives for cl-pprcre:replace[-all]
  quote them, since they have no meaning under clojure replacement syntax and/or semantics.
  The directives we're looking for are: \N or \{N} (for some digit), \&, \`, \'"
  (declare (string replacement))
  ;; For reasons I don't understand, \\{N} does not work. I need to type N-times \\ pairs.
  ;; We could probably use cl-ppcre/quote-meta-chars, except that it would quote
  ;; the $1 $2 stuff we may need to substitute.
  (or 
   #+nil (quote-meta-chars replacement) ; the 'or' isn't strictly necessary for this
   (regex-replace-all "(\\\\\\d+)|(\\\\\\{\\d+\\})|(\\\\`)|(\\\\&)|(\\\\')"
                      replacement       ;really the input here
                      #'(lambda (target-string &rest args)
                          (declare (ignore args))
                          (concatenate 'string "\\" target-string))
                      :simple-calls t)
   replacement))

(defun re-replace-aux (string match replacement replace-fn)
  "Does the work of re-replace and re-replace-first.
  The logic is identical between the two except for which cl-ppcre function is called,
  as specified by the last argument."
  ;; - Convert single character match/replacement values to string operands.
  ;; - Convert replacement fn (if it is a function designator)
  ;;   from cl-ppcre fn of many args to clojure fn of 1 arg.
  ;; - If replacement is a string, neutralize any cl-ppcre replacement syntax.
  ;;   I.e. a replacement string of "\\1" should not do any substitution, it should be 
  ;;   literally the replacement. (ppcre-replacement-quoter handles this)
  ;; - Translate clojure replacement syntax: $n (clojure) to \n (ppcre).
  ;;   Ideally we'd only do this if match is pattern, see docstring caveats.
  ;;   Beware `re-quote-replacement` of clojure directives in which case no translation
  ;;   occurs.
  (let* ((match (etypecase match
                  (string match)
                  (character (make-string 1 :initial-element match))))
         (replacement (etypecase replacement
                        (symbol   #'(lambda (&rest args) (funcall replacement args)))
                        (function 
                         #'(lambda (&rest args) 
                             (let ((match (first args)))
                               (if (endp (rest args))
                                   (funcall replacement match) ;no registers
                                   (funcall replacement args))))) ;registers
                        (character (make-string 1 :initial-element replacement))
                        (string (clojure-replacement-translation
                                 (ppcre-replacement-quoter replacement))))))
    (funcall replace-fn match string replacement :simple-calls t)))

(defun re-replace (string match replacement)
  "In clojure this would be `clojure.string/replace`.

  Replaces all instances of 'match' with 'replacement' in 'string'.
  If there are no matches, the input 'string' value is returned.

  Note that if you want more power, use `cl-ppcre\regex-replace[-all]` instead, but that 
  it has different replacement directives which are disabled for clojure compatability.

  `match`/`replacement` can be:

      string / string
      char / char
      pattern / (string or function of match).

  See also `re-replace-first`.

  Note that, at least at present, this function doesn't know whether `'match`' is a 
  pattern or a string that isn't meant  to be a pattern, since we presently use strings
  for both. So match will always be treated as a pattern.  If you want it not to be interpreted
  as a pattern, use `(cl-ppcre:quote-meta-chars match)`. *FINISH* may need somnething different
  like re-quote-match??

  If replacement is a function it should take one argument (the match to be replaced)
  and return the replacement value. Note that the argument could be a list if the pattern
  contains capturing groups (as per `re-groups`), i.e. `(match, register1, register2, ...)`.

  The `replacement` is literal (i.e. none of its characters are treated
  specially) for all cases above except pattern / string.

  For pattern / string, $1, $2, etc. in the replacement string are
  substituted with the string that matched the corresponding
  parenthesized group in the pattern.  If you wish your replacement
  string 'r' to be used literally, use `(re-quote-replacement r)` as the
  replacement argument.  See also documentation for
  java.util.regex.Matcher's appendReplacement method.

  Example:
  `(/re-replace \"Almost Pig Latin\" \"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
   -> \"lmostAay igPay atinLay\""
  (re-replace-aux string match replacement #'regex-replace-all))

(defun re-replace-first (string match replacement)
  "In clojure this would be `clojure.string/replace-first`.

  Replaces the _first_ instance of 'match' with 'replacement' in 'string'.
  See `re-replace` for argument syntax and semantics."
  (re-replace-aux string match replacement #'regex-replace))

(defun re-seq (regexp string)
  "Returns a list of successive matches of pattern in string as by
  using java.util.regex.Matcher.find(), each such match processed with
  re-groups.  Note that the clojure version would return a lazy sequence, 
  but we don't have those."
  (let ((matcher (re-matcher regexp string)))
    (loop while (next matcher)
          collecting (re-groups matcher))))

(defun re-split (string regexp &optional limit)
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits. Returns list of the splits.

  Resulting strings do not share structure with the input.

  Note that capture groups (cl-ppcre registers) have no effect on the operation except perhaps
  to make it perform more slowly"
  ;; Some clojure fiddliness for edge cases
  (or (split regexp string :with-registers-p nil :limit limit)
      (if (string= "" string)
          (list string)))) ;(split "b" "") => "", but (split "b" "b") => () (no matches, nil)
          
# `clj-re` - clojure style regular expression functions

This package wraps cl-ppcre's regexp handling, which is nearly identical to
[java.util.regex.Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
which in turn is used by Clojure, in a series of regexp supporting functions that attempt to 
behave like their Clojure namesakes.

It provides the following functions:

    #:re-find
    #:re-groups
    #:re-matcher
    #:re-matches
    #:re-pattern
    #:re-quote-replacement           ;clojure.string/re-quote-replacement
    #:re-replace                     ;clojure.string/replace, distinct from clojure.core/replace
    #:re-replace-first               ;clojure.string/replace-first
    #:re-seq
    #:re-split                       ;clojure.string/split

Successfully tested on `sbcl` and `clisp`.

## Differences from Clojure

### `clojure.string` namespace functions

Clojure has several functions which are normally in the `clojure.string` namespace
that do not reside in separate Common Lisp packages (they're all in the `:clj-re`).
Following are the clojure.string functions and what we have called them here:

* `clojure.string/replace`              => `re-replace`           ('re' prefix added)
* `clojure.string/replace-first`        => `re-replace-first`     ('re' prefix added)
* `clojure.string/re-quote-replacement` => `re-quote-replacement` (name unchanged)
* `clojure.string/split`                => `re-split`             ('re' prefix added)

We could have left `replace-first` alone, but with every other exported symbol
in this package prefixed with 're', it seemed like the consistent thing to do.

### No provision for `#"pattern"` regular expression literals.

Clojure supports a 'regular expression literal' syntax of the form
`#"pattern"` - note the sharpsign. We could have added a `#""` readtable
syntax here, but it would conflict with the syntax used for C-style literals
in the [trivial-escapes](https://github.com/williamyaoh/trivial-escapes/)
package. Also, `#""` isn't technically a standards-compliant dispatch sequence.

Similarly, the `#p` readtable entry is customarily used for pathnames ('p'
might have been nice for 'patterns'), and `#r` is used for lisp radix
specifications ('r' might have been nice for 'regex'). These are also are not
spec-compliant for user-defined readtable entries.

The regular expression literal syntax (if you have it) means you do not need
to double-escape regular expression constructs such as `\d`.  So for now
you're stuck having to double-escape such constructs, i.e. `\\d`.

It is _really useful_ to have the additional notation because double-escaping
regex constructs gets old really fast. Just remember `princ` is your friend for
debugging escape-related pitfalls.

For example, which regexp matches `"\\\\"` with `\{n}` notation?

    `"\\{2}"`
    or
    `"\\\\{2}"`

Princ makes it clearer. The answer is the second, but once you get a big old string full of
`\\\\` sequences readability goes into the toilet. Clojure's regular expression literal
goes a long way to making it more readable. Perhaps someday we'll have something.

### Named capturing groups (a.k.a. registers) are not supported.

Clojure/java has them, cl-ppcre has them, this was purely laziness on my part since 
I never use them.

## Usage

    (ql:quickload :clj-re)
    (use-package :clj-re)
    (re-find "a*b" "aaab") => "aaab"

    or to test

    (ql:quickload :clj-re-test)
    (clj-re-test:run-tests)

See unit tests for more examples.

Adjusting for the previously mentioned caveats:

1. Renaming of functions from the clojure.string namespace.
2. Replacing regexp literal syntax (#"") with doubly-escaped string regexps.
3. Replacement of vector results with list results.
4. Missing support for named capture groups.

You will hopefully find this sufficient for casual Clojureish regexp needs.  If you're
going to do performance/memory critical stuff, I suggest you learn to use
cl-ppcre directly because issues like string sharing and pattern compilation
may be important for your app.


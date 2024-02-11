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

### Optional support for clojure pattern literal syntax, i.e. #"pattern"

Using `:named-readtables`
[http://melisgl.github.io/named-readtables](http://melisgl.github.io/named-readtables),
the `:clj-re` pacakge exports `readtable` and `readtable-mixin` readtables
that will return compiled cl-ppcre patterns when the pattern literal syntax is
used.

In order to make the literal use optional, where clojure functions would require a Pattern
object, you are free to use a string expressing a pattern, which will in turn be fed to
`re-pattern` to obtain a pattern.  This is hopefully a superset-compatible feature
compare to clojure, but if you wanted clojure's exceptions on invalid arguments or types
you won't get them here.

You can enable pattern literal syntax either by:

    (named-readtables:in-readtable clj-re:readtable)

which augments the standard readtable with a dispatch function for `#""`
literals, or by using named readtable composition capabilities (e.g. merge or
fuse) with `clj-re:readtable-mixin`, which contains only the dispatch function
for pattern literals, without any other reader macros.

The literal syntax makes it easier to ensure you have compiled cl-ppcre scanners
when the code is compiled and/or loaded, roughly the equivalent of 
`#.(re-pattern "pattern string")`.

The literal syntax also means you don't need to double-escape regular
expression constructs such as `\d`, which must be expressed as `\\d` on a conventional
Common Lisp string.

TIP: If you're not using pattern literals, 
remember `princ` is your friend for debugging escape-related pitfalls.

For example, which regexp matches `"\\\\"` with `\{n}` notation?

    `"\\{2}"`
    or
    `"\\\\{2}"`

Princ makes it clearer. The answer is the second, but once you get a big old string full of
`\\\\` sequences readability goes into the toilet. Clojure's regular expression literal
goes a long way to making regular expressions more readable.

### Named capturing groups (a.k.a. registers) are not supported.

Clojure/java has them, cl-ppcre has them, this was purely laziness on my part since 
I never use them.

## Strings as patterns gotchas

If you eschew use of the read-table for #"..." pattern syntax, many of the
functions here will happily take a string containing a regexp and convert
it to a pattern representation to make your life easier so you don't
constantly have to call `re-pattern`.

The replacements for `clojure.string/replace-first` and `replace` are
different though, because string 'match' parameters are treated literally.
They are treated that way here too, so in that regard clojure and this lisp
library match.  But you can get so into the mindset of "strings can express
patterns" that it's easy to forget that `replace-first` and `replace`
won't take the strings as patterns (and that this is the intended behavior).

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


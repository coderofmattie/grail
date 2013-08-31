;;----------------------------------------------------------------------
;; parser tests/examples
;;----------------------------------------------------------------------

(require 'parser) ;; evaluate this line to load the parser.

;; error test, see what happens with bad input.
(parser-compile
  /bogus foo bar)

;; test the single token/function case

(parser-define 'test-parser
  (parser-compile
    (/token whitespace "[[:blank:]]+")))

(functionp 'test-parser)

;; simple test of the or relation.
(parser-define 'test-parser
  (parser-compile
    ;; dump
    (/token whitespace "[[:blank:]]+")
    (/token word "[[:alpha:]]+")))

  foo

;; test the and operator.
(parser-define 'test-parser
  (parser-compile
    ;; dump
    /and
    (/token word "[[:alpha:]]+")
    (/token whitespace "[[:blank:]]+")))

foo bar

;; test define lists to pre-decl token/terms
(parser-define 'test-parser
  (parser-compile
    ;; dump
    (define
      (/token word "[[:alpha:]]+")
      (/token whitespace "[[:blank:]]+"))

    /and word whitespace))

;; test greed
(parser-define 'test-parser
  (parser-compile
    ;; dump
    /greedy
    /and
    (/token word "[[:alpha:]]+")
    (/token whitespace "[[:blank:]]+")))

parser foo bar baz||

;; not operator test.

(parser-define 'test-parser
  (parser-compile
    ;; dump
    (define
      (/token whitespace "[[:blank:]]+" null)
      (/token word "[[:alpha:]]+")
      (/token terminate "stop"))

    /greedy
    /and
    terminate /not

    word whitespace))

start stop go less more

;; attempt to parse a few lines from the paludis package manager reports.

(parser-trace-list paludis-trace
  (package t)
  (repository t))

(parser-define 'paludis-query
  (parser-compile
    dump
    (define
      (/token record-delim       "$" null)
      (/token whitespace        "[[:blank:]]+"  null)
      (/token pkg-name          "\\([^[:blank:]]+\\)$" parser-token-string)
      (/token repo-name         "\\([^[:blank:]]+\\):" 1 parser-token-string)

      (/term pkg-version
        /or
        (/token pkg-slot       "{:\\([[:digit:]]+\\)}" 1 parser-token-string)
        (/token pkg-ver-masked "(\\([^[:blank:]]+\\))[^[:blank:]]+" 1 parser-token-string)
        (/token pkg-ver-stable "[^[:blank:]]+" parser-token-string) ))

    (/production package (/token package-record "\\\*" null) whitespace pkg-name)
    (/production repository whitespace repo-name whitespace
      (/greedy record-delim /not-skip pkg-version whitespace)) ))



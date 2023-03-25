;; generated automatically.  DO NOT EDIT
#!no-fold-case
(define-module parser.peg (use scheme.list) (use scheme.charset) (use srfi.13) (use gauche.collection) (use gauche.generator) (use gauche.lazy) (use text.tree) (use util.match) (export <parse-error> make-peg-parse-error peg-run-parser peg-parse-string peg-parse-port peg-parser->generator peg-parser->lseq parse-success? return-result return-failure return-failure/expect return-failure/unexpect return-failure/message return-failure/compound $bind $return $fail $expect $lift $lift* $debug $let $let* $try $assert $not $or $fold-parsers $fold-parsers-right $seq $seq0 $list $list* $between $many $many1 $many_ $many1_ $repeat $repeat_ $many-till $many-till_ $optional $sep-by $end-by $sep-end-by $chain-left $chain-right $lazy $parameterize $cut $raise $any $eos $. $string $string-ci $char $one-of $none-of $satisfy $match1 $match1* $binding $lbinding $->rope $->string $->symbol rope->string rope-finalize))
(select-module parser.peg)
(dynamic-load "parser--peg")
(define-syntax $lazy (syntax-rules () ((_ parse) (let ((p (delay parse))) (lambda (s) ((force p) s))))))
(define-syntax $lbinding (syntax-rules () ((_ x ...) ($lazy ($binding x ...)))))

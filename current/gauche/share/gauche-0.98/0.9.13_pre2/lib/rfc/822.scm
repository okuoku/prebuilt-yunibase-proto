;; generated automatically.  DO NOT EDIT
#!no-fold-case
(define-module rfc.822 (use scheme.list) (use srfi.13) (use srfi.19) (use text.parse) (use gauche.regexp) (use util.match) (export <rfc822-parse-error> rfc822-parse-errorf rfc822-read-headers rfc822-header->list rfc822-header-ref rfc822-skip-cfws *rfc822-atext-chars* *rfc822-standard-tokenizers* rfc822-atom rfc822-dot-atom rfc822-quoted-string rfc822-next-token rfc822-field->tokens rfc822-parse-date rfc822-date->date date->rfc822-date rfc822-invalid-header-field rfc822-write-headers))
(select-module rfc.822)
(dynamic-load "rfc--822")

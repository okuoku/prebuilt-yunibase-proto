;; generated automatically.  DO NOT EDIT
#!no-fold-case
(define-module rfc.mime (use srfi.13) (use scheme.charset) (use rfc.822) (use util.match) (export mime-parse-version mime-parse-content-type mime-parse-content-disposition mime-parse-parameters mime-compose-parameters mime-encode-word mime-encode-text mime-decode-word mime-decode-text <mime-part> mime-parse-message mime-retrieve-body mime-body->string mime-body->file mime-make-boundary mime-compose-message mime-compose-message-string))
(select-module rfc.mime)
(dynamic-load "rfc--mime")

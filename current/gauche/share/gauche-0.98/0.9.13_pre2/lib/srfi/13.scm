;; generated automatically.  DO NOT EDIT
#!no-fold-case
(define-module srfi.13 (export string-null? string-every string-any string-tabulate reverse-list->string substring/shared string-copy! string-take string-take-right string-drop string-drop-right string-pad string-pad-right string-trim string-trim-right string-trim-both string-compare string-compare-ci string= string<> string< string> string<= string>= string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>= string-hash string-hash-ci string-prefix-length string-suffix-length string-prefix-length-ci string-suffix-length-ci string-prefix? string-suffix? string-prefix-ci? string-suffix-ci? string-index string-index-right string-skip string-skip-right string-count string-contains string-contains-ci string-titlecase string-titlecase! string-upcase string-upcase! string-downcase string-downcase! string-reverse string-reverse! string-concatenate string-append/shared string-concatenate/shared string-concatenate-reverse string-concatenate-reverse/shared string-map string-map! string-fold string-fold-right string-unfold string-unfold-right string-for-each string-for-each-index xsubstring string-xcopy! string-replace string-tokenize string-filter string-delete string-parse-start+end string-parse-final-start+end let-string-start+end check-substring-spec substring-spec-ok? make-kmp-restart-vector kmp-step string-kmp-partial-search string? make-string string string->list list->string string-join string-length string-ref string-copy string-set! string-fill! string-append string-map string-for-each))
(select-module srfi.13)
(dynamic-load "srfi--13")
(define-syntax let-string-start+end (syntax-rules () ((_ (?start ?end ?rest) ?proc ?s ?args . ?body) (call-with-values (lambda () (string-parse-start+end ?proc ?s ?args)) (lambda (?rest ?start ?end) . ?body))) ((_ (?start ?end) ?proc ?s ?args . ?body) (call-with-values (lambda () (string-parse-final-start+end ?proc ?s ?args)) (lambda (?start ?end) . ?body)))))

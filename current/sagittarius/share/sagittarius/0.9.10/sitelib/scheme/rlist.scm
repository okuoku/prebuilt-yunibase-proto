;; This file is automatically generated. DO NOT EDIT!!
(define-library
  (scheme rlist)
  (export :all :export-reader-macro)
  (import
    (rename
      (prefix (srfi 101) r)
      (rmake-list make-rlist)
      (rrandom-access-list->linear-access-list
        rlist->list)
      (rlinear-access-list->random-access-list
        list->rlist))))

#!nobacktrace
(define-library (srfi srfi-1)
  (export xcons
          cons*
          make-list
          list-tabulate
          list-copy
          circular-list
          iota
          proper-list?
          circular-list?
          dotted-list?
          not-pair?
          null-list?
          list=
          first
          second
          third
          fourth
          fifth
          sixth
          seventh
          eighth
          ninth
          tenth
          car+cdr
          take
          drop
          take-right
          drop-right
          take!
          drop-right!
          split-at
          split-at!
          last
          last-pair
          length+
          concatenate
          append!
          concatenate!
          reverse!
          append-reverse
          append-reverse!
          zip
          unzip1
          unzip2
          unzip3
          unzip4
          unzip5
          count
          fold
          unfold
          pair-fold
          reduce
          fold-right
          unfold-right
          pair-fold-right
          reduce-right
          append-map
          append-map!
          map!
          pair-for-each
          filter-map
          map-in-order
          filter
          partition
          remove
          filter!
          partition!
          remove!
          find
          find-tail
          any
          every
          list-index
          take-while
          drop-while
          take-while!
          span
          break
          span!
          break!
          delete
          delete-duplicates
          delete!
          delete-duplicates!
          alist-cons
          alist-copy
          alist-delete
          alist-delete!
          lset<=
          lset=
          lset-adjoin
          lset-union
          lset-union!
          lset-intersection
          lset-intersection!
          lset-difference
          lset-difference!
          lset-xor
          lset-xor!
          lset-diff+intersection
          lset-diff+intersection!)
  (import (srfi 1)))

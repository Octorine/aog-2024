(define-module (octorine aog-2024 set)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
)

(define-record-type <set>
  (set-cons table)
  set?
  (table set-table))

(define-public (make-set)
  (set-cons (make-hash-table)))

(define-public (set-add! s thing)
  (hash-set! (set-table s) thing thing))

(define-public (in-set? s thing)
  (eq? thing (hash-ref (set-table s) thing)))

(define-public (set->list s)
  (hash-map->list (lambda (k v) k) (set-table s)))

(define-public (list->set ls)
  (let ((s (make-set)))
    (for-each (lambda (entry) (set-add! s entry)) ls)
    s))

(define-public (dedup ls)
  (set->list (list->set ls)))

(define-public (set-size s)
  (hash-count (lambda (k v ) #t) (set-table s)))

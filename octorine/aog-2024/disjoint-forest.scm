(define-module (octorine aog-2024 disjoint-forest)
  #:use-module (octorine aog-2024 set)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export
  (empty-forest add-tree find-root merge-trees forest-members forest-trees forest-tops))

(define-record-type <forest>
  (make-forest trees)
  forest?
  (trees forest-trees set-forest-trees!))


(define (empty-forest)
  (make-forest (make-hash-table)))

(define (add-tree f k)
  (hash-set!  (forest-trees f) k k))
  

(define (find-root f k)
  (let ((v (hash-ref  (forest-trees f) k)))
    (if (equal? k v)
	k
	(let ((result (find-root f v)))
	  (hash-set! (forest-trees f) k result)
	  result))))

;;; merge t2 into t1  after this nothing will belong to t2
(define (merge-trees f t1 t2)
  (let ((tr1 (find-root f t1))
	(tr2 (find-root f t2)))
    (if (not (equal? tr1 tr2))
	(hash-set! (forest-trees f) tr2 tr1))))
	
(define (forest-members f)
(hash-map->list (lambda (k v) k)  (forest-trees f)))

(define (forest-tops f)
(set->list (list->set (hash-map->list (lambda (k v) v)  (forest-trees f)))))

	     

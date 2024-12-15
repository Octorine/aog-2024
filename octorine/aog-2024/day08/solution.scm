(define-module (octorine aog-2024 day08 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (octorine aog-2024 grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)

  )

(define (p1 filename)
  (solve-puzzle filename find-simple-antinodes))

(define (solve-puzzle filename find-antinodes)
    (call-with-input-file filename
    (lambda (input)
      (let* ((g (read-grid input))
	     (frequencies (all-frequencies g))
	     (node-pairs
	      (apply
	       append
	       (hash-map->list
		(lambda (k v) (all-pairs v))
		frequencies)))
	     (antinodes (find-antinodes g node-pairs)))
	(length (dedup antinodes))))))

(define (find-simple-antinodes g node-pairs)
  
	      (filter (lambda (c) (in-bounds? c g))
		      (apply
	       append
	       (map (lambda (pr)
		      (list
		       (sub (add (car pr) (car pr)) (cdr pr))
		       (sub (add (cdr pr) (cdr pr)) (car pr))))
		    node-pairs))))


(define (coords->string c)
  (match c (($ <coords> x y)
	    (format #f "<~a, ~a>" x y))))
(define (dedup l)
  (if (null? l)
      l
      (let ((hd (car l))
	    (tl (cdr l)))
	(cons hd (filter (lambda (elt) (not (equal? elt hd)))
			 (dedup tl))))))
	    
	       
(define (all-frequencies g)
  (let ((table (make-hash-table)))
    (vector-for-each
     (lambda (j v)
       (vector-for-each
	(lambda (i c)
	  (if (not (char=? c #\.))
	      (begin
		(if (not (hash-ref table c))
		    (hash-set! table c '()))
		(hash-set! table c (cons (coords i j) (hash-ref table c))))))
	v))
     g)
    table))
(define (all-pairs ls)
  (cond
   ((null? ls) ls)
   ((null? (cdr ls)) '())
   ((null? (cdr (cdr ls))) (list (cons (car ls) (cadr ls))))
   (else (append (map (lambda (other) (cons (car ls) other)) (cdr ls))
		  (all-pairs (cdr ls))))))

(define (in-bounds? c g)
  (match c (($ <coords> x y)
	    (and (>= x 0) (>= y 0)
		 (< x (grid-width g))
		 (< y (grid-height g))))))

(define (p2 filename)
  (solve-puzzle filename find-all-antinodes))


(define (find-all-antinodes g node-pairs)
  (apply append
	 (map (lambda (np)
		(let* ((fst (car np))
		       (snd (cdr np))
		       (delta (sub snd fst))
		       (origin (coords 0 0))
		       (min-point 0)
		       (output '()))
		  (set! min-point
			(do ((current fst (sub current delta)))
		      ((not (in-bounds? current g)) (add current delta))))
		  (do ((current min-point (add current delta)))
		      ((not (in-bounds? current g)))
		    (set! output (cons current output)))
		  output))
	      node-pairs)))


(define g (call-with-input-file "octorine/aog-2024/day08/input" read-grid))

(let ((input "octorine/aog-2024/day08/input"))
  (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input)))



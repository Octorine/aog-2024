(define-module (octorine aog-2024 day06 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (octorine aog-2024 grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  
  )

(define-record-type <sim>
  (sim position direction smap visited)
  sim?
  (position sim-position set-sim-position!)
  (direction sim-direction set-sim-direction!)
  (smap sim-map)
  (visited sim-visited set-sim-visited!))


(define (p1 filename)
  (call-with-input-file filename
    (lambda (input)
      (let* ((grid (read-grid input))
	     (sim (make-sim grid)))
	(let loop ()
	  (if (escaped? sim)
	      (+ 1 (hash-count (lambda (x y) #t) (sim-visited sim)))
	      (begin
		
		(sim-move! sim)
		(loop))))))))

(define (grid-find g c)
  (let ((x #f)(y #f))
    (vector-map
     (lambda (j v)
       (vector-map
	(lambda (i cell)
	  (if (char=? c cell)
	      (begin
		(set! x i)
		(set! y j)))) v)) g)
    (coords x y)))



(define (next-direction direction)
  (match direction
    ( ($ <coords> 0 -1) (coords 1 0))
    ( ($ <coords> 1 0) (coords 0 1))
    ( ($ <coords> 0 1) (coords -1 0))
    ( ($ <coords> -1 0) (coords 0 -1))))

(define (sim-move! sim)
  (match sim
    (($ <sim> position direction map visited)
     (if (and
	  (in-bounds (add position direction) map)
	  (not-loopy? sim))
	 (begin
	       (sim-visit! sim)
	 (if (char=? (cell (add position direction) map) #\.)
	       (set-sim-position! sim (add position direction))
	       (set-sim-direction! sim (next-direction direction))))))))

  
(define (in-bounds pos grid)
  (match pos
    (($ <coords> x y)
     (and (>= x 0) (>= y 0) (< x (grid-width grid)) (< y (grid-height grid))))))

(define (not-loopy? sim)
  (match sim (($ <sim> position direction smap visited)
	      (or (not (hash-ref visited position))
		  (not (member direction (hash-ref visited position)))))))

(define (sim-visit! sim)
  (match sim (($ <sim> position direction smap visited)
	      (if (or (not (hash-ref visited position))
		      (null? (hash-ref visited position)))
		  (hash-set! visited position (list direction))
		  (hash-set! visited position (cons direction (hash-ref visited position)))))))
	      
  
(define (make-sim map)
  (let ((initial-position (grid-find map #\^))
	(initial-hash (make-hash-table)))
    (vector-set! (vector-ref map (coords-y initial-position))
		 (coords-x initial-position)
		 #\.)
    (sim initial-position (coords 0 -1) map initial-hash)))

(define (sim->string sim)
  (match sim (($ <sim> position direction smap visited)
	  (format #f "pos: ~a\tdir: ~a\nVisited: ~a\nMap:\n~a\n"
		  (coords->string position)
		  (coords->string direction)
		  (hash-count (lambda (k v) #t) visited)
		  (grid->string smap)))))

(define (coords->string c)
  (match c
    (($ <coords> x y)
     (format #f "<~a, ~a>" x y))))

(define (grid->string grid)
  (list->string
   (fold append '()
	 (map (lambda (v) (append (vector->list v) '(#\newline))) (vector->list grid)))))
	  
(define (escaped? sim)
  (not (in-bounds (add (sim-position sim) (sim-direction sim)) (sim-map sim))))

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let* ((grid (read-grid input))
	     (sim (make-sim (copy-grid grid))))
	(let loop ()
	  (if (escaped? sim) 

	      (length
	       (filter
		(lambda (pos) (try-for-loop pos (copy-grid grid)))
		(cons (sim-position sim) (hash-map->list (lambda (k v) k) (sim-visited sim)))))
	      (begin
		
		(sim-move! sim)
		(loop))))))))
      
(define (try-for-loop pos grid)
  (let ((sim (make-sim grid)))
    (set-cell! pos (sim-map sim) #\#)
    	(let loop ()
	  (if (escaped? sim)
	      #f
	      (if (not-loopy? sim)
		  
	      (begin
			 
		(sim-move! sim)
		(loop))
	      (begin
	      #t))))))
(define my-grid (call-with-input-file "octorine/aog-2024/day06/input" read-grid))
(define-public  (run)
  (let ((input "octorine/aog-2024/day06/sample"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))

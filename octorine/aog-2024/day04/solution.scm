(define-module (octorine aog-2024 day04 solution)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (octorine aog-2024 grid)
  )

(define (p1 filename)
  (call-with-input-file filename
      (lambda (input)
	(xmases (read-grid input)))))

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((grid (read-grid input))
	    (total 0))
	(do ((j 1 (+ 1 j)))
	    ((>= j (- (grid-height grid) 1)))
	  (do ((i 1 (+ 1 i)))
	      ((>= i (- (grid-width grid) 1)))
	    (let ((current (coords i j)))
		  (if (and (char=? (cell current grid) #\A)
			   (and
			    (or
			    (and
			   (char=? (cell (add (coords 1 1) current) grid) #\S)
			   (char=? (cell (add (coords -1 -1) current) grid) #\M))
			    (and
			   (char=? (cell (add (coords 1 1) current) grid) #\M)
			   (char=? (cell (add (coords -1 -1) current) grid) #\S)))
			    (or (and
			   (char=? (cell (add (coords 1 -1) current) grid) #\S)
			   (char=? (cell (add (coords -1 1) current) grid) #\M))
			    (and
			   (char=? (cell (add (coords 1 -1) current) grid) #\M)
			   (char=? (cell (add (coords -1 1) current) grid) #\S)))
			  ))
		      (set! total (+ 1 total))))))
	total))))
			 

(define (row n grid)
  (list->string (vector->list (vector-ref grid n))))

(define (column n grid)
  (list->string
   (vector->list
    (vector-map (lambda (i v) (vector-ref v n)) grid))))

(define (wrap-x max c)
  (coords (modulo (coords-x c) max) (coords-y c)))

;; diagonal that goes from top left to bottom right
(define (zig j grid)
  (let* ((start (coords (- (grid-width grid) j 1) 0))
	 (sideways (coords 1 1))
	 (number-of-indices (min (+ j 1) (grid-width grid)))
	 (number-to-skip (max 0 (- j (- (grid-width grid) 1))))
	 (indices (iota (- number-of-indices number-to-skip) number-to-skip)))
    (list->string
     (map (lambda (i)
	    (cell 
	     (add start
		  (scalar i sideways)) grid))
     indices))))

;; top right to bottem left
(define (zag j grid)
  (let* ((start (coords j 0))
	(sideways (coords -1 1))
	 (number-of-indices (min (+ j 1) (grid-width grid)))
	 (number-to-skip (max 0 (- j (- (grid-width grid) 1))))
	 (indices (iota (- number-of-indices number-to-skip) number-to-skip)))
    (list->string
     (map (lambda (i)
	    (cell 
	     (add start
		  (scalar i sideways)) grid))
     indices))))

(define (counter grid word selector indices)
  (+ (fold + 0
	   (map
	    (lambda (c)
	      (length (list-matches word (selector c grid))))
	    indices))))
(define (trace label thing)
  (format #t "~a: ~a\n" label thing)
  thing)
(define (xmases grid)
  (let ((forwards "XMAS")
	(backwards (string-reverse "XMAS"))
	(row-indices (iota (grid-height grid)))
	(column-indices (iota (grid-width grid)))
	(diagonal-indices (iota (+ (grid-height grid) (grid-width grid)))))
    (+  (counter grid forwards column column-indices)
        (counter grid backwards column column-indices)
        (counter grid forwards row row-indices)
        (counter grid backwards row row-indices)
        (counter grid forwards zig diagonal-indices)
        (counter grid backwards zig diagonal-indices)
        (counter grid forwards zag diagonal-indices)
        (counter grid backwards zag diagonal-indices))
       
    ))

(define my-grid (call-with-input-file "octorine/aog-2024/day04/sample" read-grid))

(let ((input "octorine/aog-2024/day04/input"))
  (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input)))

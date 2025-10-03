(define-module (octorine aog-2024 day10 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (octorine aog-2024 grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-43)
  )

(define (p1 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((g (read-grid input))
	    (destinations (make-hash-table)))
	(map
	 (lambda (trail-tail)
	   (paths-to-destination g destinations trail-tail))
	 (grid-find g #\9))
	(sum
	 (map
	  (lambda (trail-head)
	    (length (hash-ref destinations trail-head)))
	  (grid-find g #\0)))))))
	     

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((g (read-grid input))
	    (destinations (make-hash-table)))
	(map
	 (lambda (trail-tail)
	   (paths-to-destination-2 g destinations trail-tail))
	 (grid-find g #\9))
	(sum
	 (map
	  (lambda (trail-head)
	    (length (hash-ref destinations trail-head)))
	  (grid-find g #\0)))))))

;;------------------------------------------------------------------------------------------  
(define-public (test)
  (let ((input "octorine/aog-2024/day10/sample"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))


(define-public (run)
  (let ((input "octorine/aog-2024/day10/input"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))
 
(define g
  (call-with-input-file "octorine/aog-2024/day10/sample"
    read-grid))


(define (grid-find g target)
  (let ((output '()))
    (let loop ((x 0) (y 0))
      (if (>= y (grid-height g))
	  output
	  (if (>= x (grid-width g))
	      (loop 0 (+ y 1))
	      (begin
		(if (char=? (cell  (coords x y) g) target)
		    (set! output (cons (coords x y) output)))
		(loop (+ 1 x) y)))))))

(define destinations (make-hash-table (* (grid-width g) (grid-height g) 2)))

(define (add-destination! table pos dest)
  (let ((current (hash-ref table pos)))
    (cond
     ((not current) (hash-set! table pos (list dest)))
     ((and (list? current) (not (member dest current)))
      (hash-set! table pos (cons dest current))))))

(define (add-destination-path! table  pos dest)
  (let ((current (hash-ref table pos)))
    (cond
     ((not current) (hash-set! table pos (list dest)))
     ((list? current)
      (hash-set! table pos (cons dest current))))))

	 
(define  (paths-to-destination g table dest)
  (let ((to-visit (list dest)))
    (let loop ()
      (if (not (null? to-visit))
	  (let ((current (car to-visit)))
	    (set! to-visit (cdr to-visit))
	    (map
	     (lambda (dir)
	       (if (check-step current dir g)
		   (begin
		     (set! to-visit (cons (add current dir) to-visit))
		     (add-destination! table (add current dir) dest))))
	     (list (coords 1 0) (coords -1 0) (coords 0 1) (coords 0 -1)))
	    (loop))))))

(define  (paths-to-destination-2 g table dest)
  (let ((to-visit (list dest)))
    (let loop ()
      (if (not (null? to-visit))
	  (let ((current (car to-visit)))
	    (set! to-visit (cdr to-visit))
	    (map
	     (lambda (dir)
	       (if (check-step current dir g)
		   (begin
		     (set! to-visit (cons (add current dir) to-visit))
		     (add-destination-path! table  (add current dir) dest))))
	     (list (coords 1 0) (coords -1 0) (coords 0 1) (coords 0 -1)))
	    (loop))))))

(define (check-step from step g)
  (let ((to (add from step)))
    (and
     (>= (coords-x to) 0)
     (< (coords-x to) (grid-width g))
     (>= (coords-y to) 0)
     (< (coords-y to) (grid-height g))
      (= (char->integer (cell to g)) (+ -1 (char->integer (cell from g)))))))

(define (sum l)
  (fold + 0 l))

(define (test)
  (format #t
	  "Part 1: ~A\nPart 2: ~A\n"
	  (p1 "octorine/aog-2024/day10/sample")
	  (p2 "octorine/aog-2024/day10/sample")))

(define (run)
  (format #t
	  "Part 1: ~A\nPart 2: ~A\n"
	  (p1 "octorine/aog-2024/day10/input")
	  (p2 "octorine/aog-2024/day10/input")))

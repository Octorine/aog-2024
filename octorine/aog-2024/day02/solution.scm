(define-module (octorine aog-2024 day02 solution)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  )

(define (p1 input-file)
  (call-with-input-file input-file
    (lambda (input)
	      (do ((current (get-line input) (get-line input))
		   (total 0 (if (safe? (parse current)) (+ total 1) total)))
		  ((eof-object? current) total)))))

(define (p2 input-file)
  (call-with-input-file input-file
    (lambda (input)
	      (do ((current (get-line input) (get-line input))
		   (total 0 (if (near-safe? (parse current)) (+ total 1) total)))
		  ((eof-object? current) total)))))
(define (remove-nth n ls)
  (cond
   ((null? ls) ls)
   ((= n 0) (cdr ls))
   (else (cons (car ls) (remove-nth (- n 1) (cdr ls))))))

(define (near-safe? lvls)
  (let ((len (length lvls)))
    (or (safe? lvls)
	(any safe? (map (lambda (n) (remove-nth n lvls)) (iota len))))))

(define (parse line)
  (map string->number (string-tokenize line char-set:digit)))

(define (safe?  lvls)
  (and (compact? lvls) (or (increasing? lvls) (decreasing? lvls))))

(define (shape-test local)
  (lambda (lvls)
    (or (null? lvls)
	(null? (cdr lvls))
	(and
	 (local (car lvls) (cadr lvls))
	 ((shape-test local) (cdr lvls))))))
(define increasing? (shape-test <=))

(define decreasing? (shape-test >=))

(define compact? (shape-test
		  (lambda (a b)
		    (let ((diff (abs (- a b))))
		      (and (>= diff 1) (<= diff 3))))))


 (define (test)
  (format #t "Part 1: ~a\nPart 2: ~a\n"
	  (p1 "octorine/aog-2024/day02/sample")
  	  (p2 "octorine/aog-2024/day02/sample")))
	  
(define (run)
    (format #t "Part 1: ~a\nPart 2: ~a\n"
	  (p1 "octorine/aog-2024/day02/input")
  	  (p2 "octorine/aog-2024/day02/input")))

(run)
(newline)


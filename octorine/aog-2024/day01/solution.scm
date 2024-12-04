(define-module (octorine aog-2024 day01 solution)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)

  )

(define (p1 input-file)
  (call-with-input-file input-file
    (lambda (input)
      (let ((a-list '())
	    (b-list '()))
	(do ((current (get-line input) (get-line input)))
	    ((eof-object? current) #f)
	  (let* ((a+b (string-tokenize current char-set:letter+digit))
		 (a (car a+b))
		 (b (cadr a+b)))
	    (set! a-list (cons (string->number a) a-list))
	    (set! b-list (cons (string->number b) b-list))))
	(fold + 0 (map (lambda (a b) (abs (- a b))) (sort a-list <) (sort b-list <)))))))


(define (p2 input-file)
  (call-with-input-file input-file
    (lambda (input)
      (let ((a-list '())
	    (b-list '()))
	(do ((current (get-line input) (get-line input)))
	    ((eof-object? current) #f)
	  (let* ((a+b (string-tokenize current char-set:letter+digit))
		 (a (car a+b))
		 (b (cadr a+b)))
	    (set! a-list (cons (string->number a) a-list))
	    (set! b-list (cons (string->number b) b-list))))
	(fold + 0 (map (lambda (a)
			 (* a
			    (length (filter (lambda (b) (= a b)) b-list))))
		       a-list))))))


(define (test)
  (format #t "Part 1: ~a\nPart 2: ~a\n"
	  (p1 "octorine/aog-2024/day01/sample")
  	  (p2 "octorine/aog-2024/day01/sample")))
	  
(define (run)
    (format #t "Part 1: ~a\nPart 2: ~a\n"
	  (p1 "octorine/aog-2024/day01/input")
  	  (p2 "octorine/aog-2024/day01/input")))

(run)
(newline)


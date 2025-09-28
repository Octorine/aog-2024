(define-module (octorine aog-2024 day07 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (octorine aog-2024 grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  )

;;;  A record type representing  a line from the puzzle input
(define-record-type <equation>
  (equation test-value operands)
  equation?
  (test-value eq-test-value)
  (operands eq-operands))

(define (p1 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((total 0))
	(do ((current (get-line input) (get-line input)))
	    ((eof-object? current))
	  (let ((eq (parse-equation current)))
	    (if (has-solution?
		 (eq-test-value eq)
		 (eq-operands eq)
		 (list + *))
		(set! total (+ total (eq-test-value eq))))))
	total))))

(define (concatnum a b)
  (string->number
   (string-append (number->string a) (number->string b ))))

(define (parse-equation line)
  (let ((atoms (string-tokenize line char-set:graphic)))
    (equation (string->number
	       (string-filter char-set:digit (car atoms)))
	      (map string->number (cdr atoms)))))

(define (has-solution? goal nums operators)
  (cond
   ((null? nums) #t)
   ((null? (cdr nums))
    (= goal (car nums)))
   (else
    (any
     (lambda (op)
       (has-solution?
	goal
	(cons (op (car nums) (cadr nums)) (cddr nums))
	operators))
     operators))))

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((total 0))
	(do ((current (get-line input) (get-line input)))
	    ((eof-object? current))
	  (let ((eq (parse-equation current)))
;;	    (format #t "~a -> ~a: ~a\n" current eq (has-solution? eq))
	    (if (has-solution?
		 (eq-test-value eq)
		 (eq-operands eq)
		 (list + * concatnum))
		(set! total (+ total (eq-test-value eq))))))
	total))))


(define-public (run)
  (let ((input "octorine/aog-2024/day07/input"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))

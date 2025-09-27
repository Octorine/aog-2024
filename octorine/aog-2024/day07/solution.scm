(define-module (octorine aog-2024 day07 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (octorine aog-2024 grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  )

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
	    (if (has-solution? eq (list + *))
		(set! total (+ total (eq-test-value eq))))))
	total))))

(define (concatnum b a)
  (string->number
   (string-append (number->string a) (number->string b ))))

(define (parse-equation line)
  (let ((atoms (string-tokenize line char-set:graphic)))
    (equation (string->number
	       (string-filter char-set:digit (car atoms)))
	      (map string->number (cdr atoms)))))

(define (has-solution? eq operators)
  (any
   (lambda (ops)
     (= (operate ops (reverse (eq-operands eq))) (eq-test-value eq)))
   (choose (- (length (eq-operands eq)) 1) operators)))

(define (flat-map f ls)
  (apply append (map f ls)))

(define (ap xs ys)
  (flat-map (lambda (x) (map (lambda (y) (cons x y)) ys))xs))

(define (choose times  population)
  (cond ((= 0 times)
	 (map (lambda (x) '()) population))
	((= 1 times)
	 (map list population))
	(else
	 (ap population (choose (- times 1) population)))))

(define (operate operators operands)
  (if (null? operators)
      (car operands)
      (catch 'numerical-overflow
	(lambda ()
	  ((car operators)
	   (car operands)
	   (operate (cdr operators) (cdr operands))))
	(lambda (key . args) 0))))

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((total 0))
	(do ((current (get-line input) (get-line input)))
	    ((eof-object? current))
	  (let ((eq (parse-equation current)))
;;	    (format #t "~a -> ~a: ~a\n" current eq (has-solution? eq))
	    (if (has-solution? eq (list + * concatnum))
		(set! total (+ total (eq-test-value eq))))))
	total))))


(define-public (run)
  (let ((input "octorine/aog-2024/day07/input"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))

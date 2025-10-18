(define-module (octorine aog-2024 day11 solution)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match))

(define (p1 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((nums (read-atoms input)))
	(sum
	 (map
	  (lambda (n)
	    (blink-count #:num n #:times 25))
	       nums))))))

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let ((nums (read-atoms input)))
	(sum
	 (map
	  (lambda (n)
	    (blink-count #:num n #:times 75))
	  nums))))))

;;; Global variable containing a cache used to memoize the blink
;;; function.

(define cache (make-hash-table 100000))

(define* (cache-get #:key num times)
  (hash-ref cache (cons num times)))

(define* (cache-set! v #:key num times)
  (hash-set! cache (cons num times) v))

(define* (blink-count #:key num times)
    (let ((v (cache-get #:num num #:times times)))
      (or
       v
       (let ((new-v
	      (if (= times 0)
		  1
		  (sum
		   (map
		    (lambda (n)
		      (blink-count #:num n #:times (- times 1)))
		    (blink-num num))))))
	 (cache-set! new-v #:num num #:times times)
	 new-v))))

(define (sum nums) (fold + 0 nums))
    
(define (read-atoms input)
    (let ((atoms '())) 
      (do ((atom #f (read input)))
	((eof-object? atom) (reverse atoms))
	(if atom (set! atoms (cons atom atoms))))))

(define-public (blink-num n)
  (let ((ns (number->string n)))
    (cond
     ((= n 0) '(1))
     ((= 0 (modulo (string-length ns) 2))
      (let* ((nslen (string-length ns))
	     (half (/ nslen 2)))
	(list (string->number (substring ns 0 half))
	      (string->number (substring ns half nslen)))))
     (else (list (* n 2024))))))

(define-public (test)
 (format #t
	  "Part 1: ~A\nPart 2: ~A\n"
	 (p1 "octorine/aog-2024/day11/sample")
	 (p2 "octorine/aog-2024/day11/sample")))

(define-public (run)
  (format #t
	  "Part 1: ~A\nPart 2: ~A\n"
	 (p1 "octorine/aog-2024/day11/input")
	 (p2 "octorine/aog-2024/day11/input")))





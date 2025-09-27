(define-module (octorine aog-2024 day03 solution)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  )


(define pattern "mul[(]([0-9]+),([0-9]+)[)]")
(define (calculate txt)
  (fold + 0
	    (map
	     (lambda (m)
	       (* (string->number (match:substring m 1))
		  (string->number (match:substring m 2))))
	     (list-matches
	      pattern
	      txt))))
(define (p1 filename)
  (call-with-input-file filename
    (lambda (port)
      (calculate (get-string-all port)))))
(define-record-type
    <snip>
  (make-snip start stop)
  snip?
  (start snip-start)
  (stop snip-stop))

(define (snip-string snip txt)
  (substring txt (snip-start snip) (snip-stop snip)))

(define (next-on txt start)
  (string-contains txt "do()" start))

(define (next-off txt start)
  (string-contains txt "don't()" start))

(define (get-snips txt start)
  (if (next-off txt start)
      (let ((stop (next-off txt start)))
	(cons (make-snip start stop)
	      (if (next-on txt stop)
		  (get-snips txt (next-on txt stop))
		  '())))
      (list (make-snip start (string-length txt)))))

	
(define (snip-for-p2 txt)
  (let ((snips (get-snips txt 0)))
    (apply string-append (map (lambda (s) (snip-string s txt)) snips))))

(define (p2 filename)
  (call-with-input-file filename
    (lambda (port)
      (calculate
       (snip-for-p2 (get-string-all port))))))
(define-public (run)
  (let ((input "octorine/aog-2024/day03/input"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))

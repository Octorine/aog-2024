(define-module (octorine aog-2024 day05 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  )

(define (p1 filename)
  (call-with-input-file filename
      (lambda (input)
	(let ((rules '())
	      (updates '()))
	  (do ((current (get-line input) (get-line input)))
	      ((eof-object? current))
	    (cond ((string-contains current "|")
		   (set! rules (cons (read-rule current) rules)))
		  ((string-contains current ",")
		   (set! updates (cons  (read-update current) updates)))))
	  
	  (fold + 0 (map update-middle-page (filter (lambda (u) (ordered? u rules)) updates)))))))

(define (all pred ls)
   (or (null? ls) #f
     (and (pred (car ls)) (all pred (cdr ls)))))

(define (update-middle-page u)
  (let ((pages (update-pages u)))
    (list-ref pages (floor (/ (length pages) 2)))))

(define (ordered? update rules)
  (let ((pages (update-pages update)))
    (all (lambda (rule)
	   (match rule
	     (($ <rule> before after)
	      (let ((mb (member before pages))
		    (ma (member after pages)))
		(or (not mb) (not ma) (>= (length mb) (length ma)))))))
	 rules)))

  (define-record-type <rule>
    (rule before after)
    rule?
    (before rule-before)
    (after rule-after))
  (define-record-type <update>
    (update pages)
    update?
    (pages update-pages))

  (define (read-rule line)
    (match (map string->number (string-tokenize line char-set:digit))
      ((before after)
       (rule before after))))

  (define (read-update line)
    (let  (( pages (map string->number (string-tokenize line char-set:digit))))
      (update pages)))

(define (sort-with-rules rs u)
  (update (sort (update-pages u) (rules-compare rs))))

(define (rules-compare rules)
  (lambda (p1 p2)
    (or (member (rule p1 p2) rules)
	(not (member (rule p2 p1) rules)))))

(define (p2 filename)
  (call-with-input-file filename
      (lambda (input)
	(let ((rules '())
	      (updates '()))
	  (do ((current (get-line input) (get-line input)))
	      ((eof-object? current))
	    (cond ((string-contains current "|")
		   (set! rules (cons (read-rule current) rules)))
		  ((string-contains current ",")
		   (set! updates (cons  (read-update current) updates)))))
	  
	  (fold + 0 (map (lambda (u) (update-middle-page (sort-with-rules rules u))) (filter (lambda (u) (not (ordered? u rules))) updates)))))))
			 
(let ((input "octorine/aog-2024/day05/input"))
  (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input)))

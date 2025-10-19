(define-module (octorine aog-2024 day09 solution)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (octorine aog-2024 grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  )

(define (p1 filename)
  (call-with-input-file filename
    (lambda (input)
      (let*
	  ((txt  (string-filter char-numeric? (get-string-all input)))
	     (buffer (parse-buffer txt)))
	(compact-buffer! buffer)
	(score-buffer buffer)))))
	     

(define (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (let*
	  ((txt (string-filter char-numeric? (get-string-all input)))
	     (blocks (parse-blocks txt)))
	(set! blocks (compact-blocks! blocks))
	(score-buffer (blocks->buffer blocks))))))

(define-record-type <block>
  (make-block name position size type)
  block?
  (name block-name)
  (position block-position set-block-position!)
  (size block-size set-block-size!)
  (type block-type))

(define (copy-block b)
  (match b
    (($ <block> name position size type)
	  (make-block name position size type))))

(define (nudge-block! block distance )
  (set-block-position! block (+ distance (block-position block))))

(define (next-block block new-size)
  (match block
    (($ <block> name position size type)
     (make-block
      (if (eq? type 'file) name (+ name 1))
      (+ position size)
      new-size
      (if (eq? type 'file) 'free 'file)))))

(define (digit->integer d)
  (let ((code (char->integer d))
	(zero (char->integer #\0))
	(nine (char->integer #\9)))
  (if (and (>= code zero) (<= code nine))
      (- code zero)
      (error (format #f "invalid digit ~a" d)))))

(define (integer->digit i)
  (if (and (>= i 0) (<= i 9))
      (integer->char (+ i (char->integer #\0)))
      (error (format #f "Invalid integer ~a" i))))

(define (write-block block buffer)
  (match block
    (($ <block> name  position size type)
     (do ((i 0 (+ i 1)))
	 ((= i size))
       (vector-set! buffer
		    (+ position i)
		    (if (eq? type 'free) 'free name))))))

(define (sum items) (fold + 0 items))

(define (maximum items) (fold max 0 items))

;;; Since we're only using this in one place, and that place is a
;;; sorted list, we can assume ITEMS is sorted;

(define (min-by lt items)
  (if (null? items)
      #f
  (fold (lambda (a b) (if (lt a b) a b))
	(car items)
	(cdr items))))

(define (parse-blocks txt)
  (let ((block (make-block -1 0 0 'free))
	(zero (char->integer #\0))
	(blocks '()))
    (string-for-each-index
     (lambda (i)
       (set! block
	     (next-block block
			 (- (char->integer (string-ref txt i)) zero)))
       (set! blocks (cons (copy-block block) blocks)))
     txt)
    (filter (lambda (b) (> (block-size b) 0)) blocks)))

(define (blocks->buffer blocks)
  (let ((buffer #f))
    (set! buffer (make-vector (sum (map block-size blocks)) 'free))
    (for-each (lambda (b) (write-block b buffer)) blocks)
    buffer))

(define (parse-buffer txt)
  (blocks->buffer (parse-blocks txt)))

(define (compact-buffer! buffer)
  (let loop
      ((front 0) (back (- (vector-length buffer) 1)))
    (let ((fval (vector-ref buffer front))
	  (bval (vector-ref buffer back)))
    (cond
     ((>= front back) #f)
     ((and (eq? fval 'free) (neq? bval 'free))
      (begin
	(vector-set! buffer front bval)
	(vector-set! buffer back 'free)
	(loop (+ front 1) (- back 1))))
     ((eq? fval 'free) (loop front (- back 1)))
     (#t (loop (+ front 1) back))))))

(define (compare-block-position b1 b2)
  (< (block-position b1) (block-position b2)))

;; Given a list of free blocks, consolidate any that are in a row.

(define (defrag free)
  (cond
   ((null? free) free)
   ((null? (cdr free)) free)
   ((let ((first (car free))
	  (second (car (cdr free)))
	  (rest (cdr (cdr free))))
      (and (eq? (block-type first) 'free)
	   (eq? (block-type second) 'free)
	   (= (+ (block-position first) (block-size first))
	      (block-position second))))
    (let ((first (car free))
	  (second (car (cdr free)))
	  (rest (cdr (cdr free))))
      (defrag
	(cons
       (make-block
	(block-name first)
	(block-position first)
	(+ (block-size first) (block-size second))
	'free)
       rest))))
      (#t (cons (car free) (defrag (cdr free))))))

(define (compact-blocks! blocks)
  (let
      ((free
	(sort (filter (lambda (b) (eq? (block-type b) 'free)) blocks)
	      compare-block-position))
       (filled
	(filter (lambda (b) (neq? (block-type b) 'free)) blocks))
       (more-free '()))
    (set! free (defrag free))
    (for-each
     (lambda (b)
       (let ((f
	      (find
	       (lambda (f)
		 (and (eq? (block-type f) 'free)
		      (< (block-position f)
			 (block-position b))
		      (>= (block-size f) (block-size b))))
	       free))
	     )
	 (if f
	     (begin
	       (set! more-free (cons  (make-block
				       'free
				       (block-position b)
				       (block-size b)
				       'free)
				      more-free))
	       (set-block-position! b (block-position f))
	       (set-block-position! f
				    (+ (block-position f)
				       (block-size b)))
	       (set-block-size!  f (- (block-size f) (block-size b)))))))
     (reverse (sort filled compare-block-position)))
    (append blocks more-free)))


(define (neq? a b) (not (eq? a b)))

(define (score-buffer buffer)
  (sum (vector->list
	(vector-map
	(lambda (i v)
	  (* i  (if (eq? v 'free) 0 v)))
	buffer))))

;;------------------------------------------------------------------------------------------
;; Some values and functions used for debugging

(define g
  (string-filter char-numeric?
		 (call-with-input-file "octorine/aog-2024/day09/input"
		   get-string-all)))
(define ex "2333133121414131402")
(define exb (parse-blocks ex))
(define (reset)   (set! exb (parse-blocks ex)))
(define (bshow blocks)
  (format #t "blocks: ~a\n\nbuffer: ~a\n"
	  blocks
	  (blocks->buffer blocks)))
;;------------------------------------------------------------------------------------------  

(define-public (run)
  (let ((input "octorine/aog-2024/day09/input"))
    (format #t "Part 1: ~a\nPart 2: ~a\n" (p1 input) (p2 input))))

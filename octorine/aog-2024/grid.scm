(define-module (octorine aog-2024 grid)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-9)

  #:export (coords <coords> coords-x coords-y coords?
		   read-grid grid-width grid-height
		   copy-grid
		   cell set-cell!
		   add sub scalar distance)
  )  
(define (grid-width grid)
  (cond
   ((= (vector-length grid) 0) 0)
   (else (vector-length (vector-ref grid 0)))))

(define (grid-height grid)
  (vector-length grid))

(define-record-type <coords>
  (coords x y)
  coords?
  (x coords-x)
  (y coords-y))

(define (read-grid input)
  (do ((line (get-line input) (get-line input))
       (rows '() (cons (list->vector (string->list line)) rows)))
      ((eof-object? line) (list->vector (reverse rows)))))
(define (copy-grid g)
  (vector-map (lambda (i v) (vector-copy v)) g))

(define (cell coords grid)
  (vector-ref (vector-ref grid (coords-y coords)) (coords-x coords)))

(define (set-cell! coords grid val)
  (vector-set! (vector-ref grid (coords-y coords)) (coords-x coords) val))

(define (add c1 c2)
  (match (list c1 c2)
    ((($ <coords> x1 y1) ($ <coords> x2 y2))
     (coords (+ x1 x2) (+ y1 y2)))))
(define (sub c1 c2)
  (add c1 (scalar -1 c2)))

(define (scalar s c)
  (match c
    (($ <coords> x y)
     (coords (* s x) (* s y)))))

(define (dot c1 c2)
  (match (list c1 c2)
    ((($ <coords> x1 y1) ($ <coords> x2 y2))
     (+ (* x1 x2) (* y1 y2)))))

(define (distance c1 c2)
  (match (list c1 c2)
    ((($ <coords> x1 y1) ($ <coords> x2 y2))
     (+ (abs (- x1 x2)) (abs (- y1 y2))))))


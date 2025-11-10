(define-module (octorine aog-2024 day12 solution)
  #:use-module (octorine aog-2024 grid)
  #:use-module (octorine aog-2024 set)
  #:use-module (octorine aog-2024 disjoint-forest)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))


(define-public (p1 filename)
  (call-with-input-file 
    filename
    (lambda (input)
      (call-with-values
        (lambda () (find-stats (read-grid input)))
        (lambda (groups group-keys areas pers)
          (let ((total 0))
            (for-each
              (lambda (g)
                (let* ((area (set-size (hash-ref areas g)))
                       (per (set-size (hash-ref pers g)))
                       (prod (* area per)))
                  (if (not (= area 0))
                    (begin
                      (if verbose
                        (format 
                          #t
                          "~a: Area ~a * Perimeter ~a = ~a\n"
                          (show-coords g) area per prod))
                      (set! total (+ total prod))))))
              group-keys)
            total))))))

(define-public (p2 filename)
  (call-with-input-file filename
    (lambda (input)
      (call-with-values
        (lambda () (find-stats (read-grid input)))
        (lambda (groups group-keys areas pers)
          (let ((total 0))
            (for-each
              (lambda (g)
                (let* ((area (set-size (hash-ref areas g)))
                       (per (length (sides (hash-ref pers g))))
                       (prod (* area per)))
                  (if (not (= area 0))
                    (begin
                      (if verbose
                        (format 
                          #t
                          "~a: Area ~a * Sides ~a = ~a\n"
                          (show-coords g) area per prod))
                      (set! total (+ total prod))))))
              group-keys)
            total))))))

(define (sides s)
  (let ((disjoint-sides (empty-forest))
        (edgelist (set->list s)))
    (for-each (lambda (side) (add-tree disjoint-sides side)) edgelist)
    (for-each 
      (lambda (s1)
        (for-each
          (lambda (s2)
            (match (list (car s1) (cdr s1) (car s2) (cdr s2))
                   ((($ <coords> x1 y1)
                     ($ <coords> x2 y2)
                     ($ <coords> x3 y3)
                     ($ <coords> x4 y4)) 
                    (if (member
                          (list (- x1 x3) (- y1 y3)
                                (- x2 x4) (- y2 y4)
                                )
                          '((1 0 1 0)
                            (1 0 1 0)
                            (-1 0 -1 0)
                            (0 1 0 1)
                            (0 -1 0 -1)))
                      (merge-trees disjoint-sides s1 s2)))))
          edgelist))
      edgelist)
    (forest-tops disjoint-sides)))

(define (find-stats g)
  (let* ((w (grid-width g))
         (h (grid-height g))
         (groups (find-groups g))
         (areas (make-hash-table))
         (pers (make-hash-table))
         (group-keys (forest-members groups)))
    (for-each
      (lambda (g) (hash-set! areas g (make-set))
        (hash-set! pers g (make-set)))
      group-keys)
    (grid-for-each!
      (lambda (c l)
        (let ((g (find-root groups c))
              (directions
                (list (coords -1 0)
                      (coords 1 0)
                      (coords 0 -1)
                      (coords 0 1))))
          (set-add! (hash-ref areas g) c)
          (for-each
            (lambda (dir)
              (let ((new-pos(add dir c)))
                (if (or
                      (< (coords-x new-pos) 0)
                      (< (coords-y new-pos) 0)
                      (>= (coords-x new-pos) w)
                      (>= (coords-y new-pos) h)
                      (not (coords=? (find-root groups (add dir c)) g)))
                  (set-add! (hash-ref pers g) (cons c (add dir c))))))
            directions)))
      g)

    (values groups group-keys areas pers)))

(define (find-groups g)
  (let ((groups (empty-forest)))
    (grid-for-each!
      (lambda (c v)
        (add-tree groups c))
      g)
    (grid-for-each!
      (lambda (c v)
        (let ((right (add c (coords 1 0)))
              (down (add c (coords 0 1)))
              (left (add c (coords -1 0)))
              (up (add c (coords 0 -1))))
          (if (and (< (coords-x c) (- (grid-width g) 1))
                   (char=? (cell c g) (cell right g)))
            (merge-trees groups c right))
          (if (and (< (coords-y c) (- (grid-height g) 1))
                   (char=? (cell c g) (cell down g)))
            (merge-trees groups c down))
          (if (and (> (coords-x c) 0)
                   (char=? (cell c g) (cell left g)))
            (merge-trees groups c left))
          (if (and (> (coords-y c) 0)
                   (char=? (cell c g) (cell up g)))
            (merge-trees groups c up))))
      g)
    groups))

(define verbose #f)

(define (coords=? c1 c2)
  (and (= (coords-x c1) (coords-x c2))
       (= (coords-y c1) (coords-y c2))))


(define (grid-for-each! proc g)
  (let ((s (make-set)))
    (do (( y 0 (+ y 1)))
      ((>= y (grid-height g)) (set->list s))
      (do ((x 0 (+ x 1)))
        ((>= x (grid-width g)))
        (proc (coords x y) (cell (coords x y) g))))))

(define (show-coords c)
  (format #f "<~a, ~a>" (coords-x c) (coords-y c)))
(define g #f)

(define (get-grid name)
  (set! g
    (call-with-input-file
      (format #f "octorine/aog-2024/day12/~a" name) read-grid)))

(define (set-verbose)
  (set! verbose #t))

(define (test)
  (format #t "Part 1: ~a\n"
    (p1 "octorine/aog-2024/day12/sample"))
  (format #t "Part 2: ~a\n"
    (p2 "octorine/aog-2024/day12/sample")))
(define (test2)
  (format #t "Part 1: ~a\n"
    (p1 "octorine/aog-2024/day12/sample2"))
  (format #t "Part 2: ~a\n"
    (p2 "octorine/aog-2024/day12/sample2")))

(define (test3)
  (format #t "Part 1: ~a\n"
    (p1 "octorine/aog-2024/day12/sample3"))
  (format #t "Part 2: ~a\n"
    (p2 "octorine/aog-2024/day12/sample3")))

(define (test4)
    (format #t "Part 1: ~a\n"
                (p1 "octorine/aog-2024/day12/sample4"))
      (format #t "Part 2: ~a\n"
                  (p2 "octorine/aog-2024/day12/sample4")))

(define (test5)
    (format #t "Part 1: ~a\n"
                (p1 "octorine/aog-2024/day12/sample5"))
      (format #t "Part 2: ~a\n"
                  (p2 "octorine/aog-2024/day12/sample5")))

(define-public (run)
  (format #t "Part 1: ~a\n"
    (p1 "octorine/aog-2024/day12/input"))
  (format #t "Part 2: ~a\n"
    (p2 "octorine/aog-2024/day12/input")))

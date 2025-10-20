(define-module (octorine aog-2024 web)
  #:use-module (web client)
  #:use-module (ice-9 textual-ports))

(define (base-url year day)
  (format #f "https://adventofcode.com/~A/day/~A" year day))

(define-public (get-puzzle year day)
  (http-get
   (base-url year day)
   #:decode-body? #t
   #:headers (list (cons 'Cookie (string-append "session=" (get-local-cookie))))))
(define-public (get-input year day)
  (call-with-values
      (lambda ()
	(http-get
	 (string-append (base-url year day) "/input")
	 #:decode-body? #t
	 #:headers (list (cons 'Cookie (string-append "session=" (get-local-cookie))))))
    (lambda (headers body) body)))


(define (get-local-cookie)
  (call-with-input-file
      (string-append
       (getenv "HOME")
       "/.config/adventofcode.session")
    get-string-all))

(define-public (download-input day)
  (call-with-output-file (format #f "octorine/aog-2024/day~a/input" day)
    (lambda (output) (write (get-input 2024 day) output))))

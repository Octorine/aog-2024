(use-modules
 ((octorine aog-2024 day01 solution) #:prefix d01-)
 ((octorine aog-2024 day02 solution) #:prefix d02-)
 ((octorine aog-2024 day03 solution) #:prefix d03-)
 ((octorine aog-2024 day04 solution) #:prefix d04-)
 ((octorine aog-2024 day05 solution) #:prefix d05-)
 ((octorine aog-2024 day06 solution) #:prefix d06-)
 ((octorine aog-2024 day07 solution) #:prefix d07-)
 ((octorine aog-2024 day08 solution) #:prefix d08-)
 ((octorine aog-2024 day09 solution) #:prefix d09-)
 ((octorine aog-2024 day10 solution) #:prefix d10-)
 ((octorine aog-2024 day11 solution) #:prefix d11-)
 (octorine aog-2024 web)
 (statprof))


(define-syntax run
  (lambda (x)
    (syntax-case x ()
      ((_ day)
       (with-syntax ((fun
		      (datum->syntax
		       #'day
		       (string->symbol
			(format #f "d~a-run" (syntax->datum #'day))))))
	 #'(let ((input-path (format #f "octorine/aog-2024/day~a/input" day)))
	     (if (not (file-exists? input-path))
		 (download-input day))
	     (format #t "Day ~a\n" day)
	     (statprof-reset 0 1 #f)
	     (statprof-start)
	     (fun)
	     (statprof-stop)
	     (format #t "Time: ~a s\n" (statprof-accumulated-time))))))))

(begin
  (run "01")
  (run "02")
  (run "03")
  (run "04")
  (run "05")
  (run "06")
  (run "07")
  (run "08")
  (run "09")
  (run "10")
  (run "11")
)

  
	   



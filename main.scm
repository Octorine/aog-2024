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
 (statprof))

(define (run-day name fun)
  (format #t "Dau ~a\n" name)
  (statprof-reset 0 1 #f)
  (statprof-start)
  (fun)
  (statprof-stop)
  (format #t "Time: ~a s\n" (statprof-accumulated-time)))

(begin
  (run-day "1" d01-run)
  (run-day "2" d02-run)
  (run-day "3" d03-run)
  (run-day "4" d04-run)
  (run-day "5" d05-run)
  (run-day "6" d06-run)
  (run-day "7" d07-run)
  (run-day "8" d08-run)
  (run-day "9" d09-run)
  (run-day "10" d10-run)
  (run-day "11" d11-run))

  
	   



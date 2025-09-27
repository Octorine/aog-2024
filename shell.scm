(use-modules (guix packages))
(define glp "GUILE_LOAD_PATH")
(setenv glp (string-append ".:" (or (getenv glp) "...")))
(specifications->manifest '("guile"))

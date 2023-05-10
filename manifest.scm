(use-modules (guix packages))

(define packages
    (specifications->packages
        (list "guile" "guile-fibers@1.0.0" "guile-g-golf" "gtk@4")))

(packages->manifest packages)

;; Use this to build a Guix package from the current git checkout.
;; Note that uncommitted changes will not be included!

;; Use like this:
;;   guix build -f guix.scm
;; Or:
;;   guix time-machine -C channels.scm -- build -f guix.scm

(use-modules (guix build-system r)
             (guix gexp)
             (guix git)
             (guix download)
             (guix git-download)
             (guix packages)
             (guix licenses)
             (gnu packages))

(define-public r-mergen
  (package
    (name "r-mergen")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mergen" version))
       (sha256
        (base32 "0nzgqsmbgk4472c64pr1qhmbhdk69qmicb5qni744crxlar92g5x"))))
    (properties `((upstream-name . "mergen")))
    (build-system r-build-system)
    (propagated-inputs
     (map specification->package
          (list "r-assertthat"
                "r-biocmanager"
                "r-httr"
                "r-jsonlite"
                "r-openai"
                "r-rmarkdown")))
    (native-inputs
     (map specification->package
          (list "r-knitr")))
    (home-page "https://cran.r-project.org/package=mergen")
    (synopsis
     "AI-Driven Code Generation, Explanation and Execution for Data Analysis")
    (description
     "Employing artificial intelligence to convert data analysis
questions into executable code, explanations, and algorithms.  The
self-correction feature ensures the generated code is optimized for
performance and accuracy.  mergen features a user-friendly chat
interface, enabling users to interact with the AI agent and extract
valuable insights from their data effortlessly.")
    (license expat)))

(define-public r-shiny-i18n
  (package
    (name "r-shiny-i18n")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shiny.i18n" version))
       (sha256
        (base32 "0kcdvan8ds1kdqdxk6yvqpxlxv6xj4nxr8mp1qns3fzklyf4n4gy"))))
    (properties `((upstream-name . "shiny.i18n")))
    (build-system r-build-system)
    (propagated-inputs
     (map specification->package
          (list "r-glue"
                "r-jsonlite"
                "r-r6"
                "r-rstudioapi"
                "r-shiny"
                "r-stringr"
                "r-yaml")))
    (home-page "https://appsilon.github.io/shiny.i18n/")
    (synopsis "Shiny Applications Internationalization")
    (description
     "This package provides easy internationalization of Shiny
applications.  It can be used as standalone translation package to
translate reports, interactive visualizations or graphical elements as
well.")
    (license expat)))

(define-public r-mergenstudio
  (package
    (name "r-mergenstudio")
    (version "1.0.0")
    (source (git-checkout (url (dirname (current-filename)))
                          (branch "main")))
    (properties `((upstream-name . "mergenstudio")))
    (build-system r-build-system)
    (propagated-inputs
     (cons* r-mergen
            r-shiny-i18n
            (map specification->package
                 (list "r-assertthat"
                       "r-bslib"
                       "r-cli"
                       "r-colorspace"
                       "r-fontawesome"
                       "r-fs"
                       "r-glue"
                       "r-htmltools"
                       "r-htmlwidgets"
                       "r-httr2"
                       "r-ids"
                       "r-jsonlite"
                       "r-magrittr"                
                       "r-purrr"
                       "r-rlang"
                       "r-rmarkdown"
                       "r-rstudioapi"
                       "r-rvest"
                       "r-shiny"                      
                       "r-shinyfiles"
                       "r-shinyjs"
                       "r-stringr"
                       "r-waiter"
                       "r-yaml"))))
    (home-page "https://github.com/BIMSBbioinfo/mergenstudio")
    (synopsis "RStudio Addin wrapper for the mergen package")
    (description "This package provides an RStudio Addin wrapper for
the mergen package.")
    (license expat)))

r-mergenstudio

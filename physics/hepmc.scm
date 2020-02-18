(define-module (physics hepmc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages rsync))

(define-public hepmc
  (package
   (name "hepmc")
   (version "2.06.10")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "http://hepmc.web.cern.ch/hepmc/releases/hepmc"
       version
       ".tgz"))
     (sha256
      (base32
       "190i9jlnwz1xpc495y0xc70s4zdqb9s2zdq1zkjy2ivl7ygdvpjs"))))
   (build-system cmake-build-system)
   (native-inputs `(("rsync" ,rsync)))
   (inputs `(("cc" ,gcc-toolchain)
	     ("bash" ,bash)
	     ))
   (arguments
    `(#:configure-flags
      '("-Dmomentum=GEV"
	"-Dlength=MM")
      #:phases
      (modify-phases
       %standard-phases
       (delete 'check))))
   (synopsis "HepMC event record library")
   (description "The HepMC package is an object oriented, C++ event record for High Energy Physics Monte Carlo generators and simulation.")
   (home-page "http://hepmc.web.cern.ch/hepmc/")
   (license fsdg-compatible)))

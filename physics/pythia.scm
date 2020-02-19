(define-module (physics pythia)
  #:use-module (physics hepmc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages rsync))

(define-public pythia
  (package
   (name "pythia")
   (version "8.2.44")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "http://home.thep.lu.se/~torbjorn/pythia8/pythia"
	   (string-filter (lambda (x) (not (eq? #\. x))) version)
	   ".tgz"))
     (sha256 (base32
	      "1jlj9hgmk2gcm5p0zqsiz0dpv9vvj8ip261si7frrwfsk7wq0j73"))))
   (build-system gnu-build-system)
   (native-inputs `(("rsync" ,rsync)))
   (inputs `(("cc" ,gcc-toolchain)
	     ("bash" ,bash)
	     ("hepmc" ,hepmc)
	     ))
   (arguments `(#:configure-flags (list (string-append "--with-hepmc2=" (assoc-ref %build-inputs "hepmc")))
		#:phases
		(modify-phases
		 %standard-phases
		 (delete 'check))))
   (synopsis "A framework for fast simulation of a generic collider experiment")
   (description " Delphes is a C++ framework, performing a fast multipurpose detector response simulation. The simulation includes a tracking system, embedded into a magnetic field, calorimeters and a muon system. The framework is interfaced to standard file formats (e.g. Les Houches Event File or HepMC) and outputs observables such as isolated leptons, missing transverse energy and collection of jets which can be used for dedicated analyses. The simulation of the detector response takes into account the effect of magnetic field, the granularity of the calorimeters and sub-detector resolutions. Visualisation of the final state particles is also built-in using the corresponding ROOT library.")
   (home-page "https://cp3.irmp.ucl.ac.be/projects/pythia")
   (license cc-by-sa4.0)
   ))

(define-module (physics delphes)
             #:use-module (physics fastjet)
             #:use-module (physics root)
             #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system cmake)
               #:use-module (guix licenses)
               #:use-module (gnu packages algebra)
               #:use-module (gnu packages base)
               #:use-module (gnu packages bash)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages commencement)
               #:use-module (gnu packages astronomy)
               #:use-module (gnu packages digest)
               #:use-module (gnu packages maths)
               #:use-module (gnu packages pkg-config)
               #:use-module (gnu packages image)
               #:use-module (gnu packages sqlite)
               #:use-module (gnu packages pcre)
               #:use-module (gnu packages tbb)
               #:use-module (gnu packages xml)
	       #:use-module (gnu packages xorg)
	       #:use-module (gnu packages gl)
               #:use-module (gnu packages python))

(define-public delphes
  (package
   (name "delphes")
   (version "3.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "http://cp3.irmp.ucl.ac.be/downloads/Delphes-"
	   version
	   ".tar.gz"))
     (sha256 (base32
	      "0yq9ikafk6v9vbcb7kh765sg2cgfzqap2dw9vgphnrfyfia7qsnl"))))
   (build-system cmake-build-system)
   (inputs `(("cc" ,gcc-toolchain)
	     ("bash" ,bash)
	     ("fastjet" ,fastjet)
	     ("root" ,root)
	     ("python" ,python-2.7)
	     ))
   (arguments `(#:phases
		(modify-phases
		 %standard-phases
		 (delete 'check))))
   (synopsis "A framework for fast simulation of a generic collider experiment")
   (description " Delphes is a C++ framework, performing a fast multipurpose detector response simulation. The simulation includes a tracking system, embedded into a magnetic field, calorimeters and a muon system. The framework is interfaced to standard file formats (e.g. Les Houches Event File or HepMC) and outputs observables such as isolated leptons, missing transverse energy and collection of jets which can be used for dedicated analyses. The simulation of the detector response takes into account the effect of magnetic field, the granularity of the calorimeters and sub-detector resolutions. Visualisation of the final state particles is also built-in using the corresponding ROOT library.")
   (home-page "https://cp3.irmp.ucl.ac.be/projects/delphes")
   (license cc-by-sa4.0)
   ))

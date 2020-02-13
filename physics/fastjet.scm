(define-module (root)
             #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system gnu)
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

(define-public fastjet
  (package
   (name "fastjet")
   (version "3.3.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "http://fastjet.fr/repo/fastjet-"
	   version
	   ".tar.gz"))
     (sha256 (base32
	      "0avkgn19plq593p872hirr0yj2vgjvsi88w68ngarbp55hla1c1h"))))
   (build-system gnu-build-system)
   (native-inputs `(("sed" ,sed) ("pkg-config" ,pkg-config)))
   (inputs `(("cc" ,gcc-toolchain)
	     ("python" ,python)
	     ("bash" ,bash)
	     ("gsl" ,gsl)
	      ))
   (synopsis "A software package for jet finding in pp and e+e− collisions.")
   (description "A software package for jet finding in pp and e+e− collisions. It includes fast native implementations of many sequential recombination clustering algorithms, plugins for access to a range of cone jet finders and tools for advanced jet manipulation.")
   (home-page "http://fastjet.fr/")
   (license gpl2)
   ))

fastjet

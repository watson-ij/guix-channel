(define-module (physics lhapdf)
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

(define-public lhapdf
  (package
   (name "lhapdf")
   (version "6.2.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://lhapdf.hepforge.org/downloads/?f=LHAPDF-"
	   version
	   ".tar.gz"))
     (sha256 (base32
	      "1l9dv37k4jz18wahyfm9g53nyl81v5bgqgy4dllbcmvcqpfkmrnn"))))
   (build-system gnu-build-system)
   (inputs `(("cc" ,gcc-toolchain)
	     ("bash" ,bash)
	     ("python" ,python-2.7)
	     ))
   (arguments
    `(#:configure-flags '("--disable-python")
      #:phases
      (modify-phases %standard-phases
		     (add-after 'unpack 'patch-shebangs
				(lambda* (#:key inputs outputs #:allow-other-keys)
					 (substitute* "bin/lhapdf"
						      (("#! /usr/bin/env python") (string-append "#!" (assoc-ref inputs "python") "/bin/python2"))
						      (("add_add_mutually_exclusive_group") "add_mutually_exclusive_group"))
					 #t))
         ;; (add-after 'install 'wrap-executable
         ;;   (lambda* (#:key inputs outputs #:allow-other-keys)
         ;;     (let ((out (assoc-ref outputs "out")))
         ;;       (wrap-program (string-append out "/bin/lhapdf")
         ;;         `("PYTHONPATH" ":" prefix (string-append out "/lib")))
         ;;       #t)))
      )))
   (synopsis "LHAPDF is a general purpose C++ interpolator, used for evaluating PDFs from discretised data files.")
   (description "LHAPDF is a general purpose C++ interpolator, used for evaluating PDFs from discretised data files.")
   (home-page "https://lhapdf.hepforge.org/")
					;   (license lgpl2.1)
   (license gpl3)
   ))

lhapdf

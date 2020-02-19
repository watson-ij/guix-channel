(define-module (physics lhapdf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
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
  #:use-module (gnu packages compression)
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
	     ;; ("index" ,(origin
	     ;; 		(source
	     ;; 		 (method fetch-url)
	     ;; 		 (source "http://lhapdfsets.web.cern.ch/lhapdfsets/current/pdfsets.index")
	     ;; 		 (sha256 (base32 "12iwrrwi0iapn5l1pmr6i6rg3493m0xmanqyqh9fifjhpqrr4m7v")))))
	     ))
   (arguments
    `(#:make-flags
      (list
       (string-append "LDFLAGS=-Wl,-rpath="
		      (assoc-ref %outputs "out")
		      "/lib"))

      #:phases
      (modify-phases
       %standard-phases
       (add-after 'unpack 'patch-shebangs
		  (lambda* (#:key inputs outputs #:allow-other-keys)
			   (substitute* "bin/lhapdf"
					(("#! /usr/bin/env python") (string-append "#!" (assoc-ref inputs "python") "/bin/python2"))
					(("add_add_mutually_exclusive_group") "add_mutually_exclusive_group"))
			   #t))

       ; There is a built-in index, but not necessarily up-to-date
       ;; (add-after 'install 'link-index
       ;; 		  (lambda* (#:key inputs outputs #:allow-other-keys)
       ;; 			   (symlink (assoc-ref inputs "index") (string-append (assoc-ref outputs "out") "/"))))
       
       (add-after 'install 'wrap-executable
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (wrap-program (string-append out "/bin/lhapdf")
               `("PYTHONPATH" ":" prefix (,(string-append out "/lib/python2.7/site-packages"))))
             #t)))
      )))
   (synopsis "LHAPDF is a general purpose C++ interpolator, used for evaluating PDFs from discretised data files.")
   (description "LHAPDF is a general purpose C++ interpolator, used for evaluating PDFs from discretised data files.")
   (home-page "https://lhapdf.hepforge.org/")
   (license gpl3)
   ))

(define-syntax pdf-package
  (syntax-rules ()
    ((pdf-package pdf pdf-name sha)
     (define-public pdf
     (package
      (name pdf-name)
      (version "0")
      (home-page "https://lhapdf.hepforge.org/")
      (source (origin
	       (method url-fetch)
	       (uri (string-append "http://lhapdfsets.web.cern.ch/lhapdfsets/current/" pdf-name ".tar.gz"))
	       (sha256 (base32 sha))))
      (inputs `(("tar" ,tar)
		("gzip" ,gzip)))
      (build-system trivial-build-system)
      (arguments `(#:modules ((guix build utils))
		   #:builder
		   (begin
		     (use-modules (guix build utils))
		     (let* ((out (assoc-ref %outputs "out"))
			    (source (assoc-ref %build-inputs "source"))
			    (tar (assoc-ref %build-inputs "tar"))
			    (gzip (assoc-ref %build-inputs "gzip")))
		       (mkdir-p out)
		       (setenv "PATH" (string-append gzip "/bin"))
		       (invoke (string-append tar "/bin/tar") "-C" out "-xf" source)))
		   ))
      (synopsis "A PDF")
      (description "A PDF")
      (license gpl3))
     )))
  )

(pdf-package CT10 "CT10" "17glhnqj4yknqy70zs7m097n1qq9fqljj3mna6qxchmgql04dvxw")
(pdf-package CT10nlo "CT10nlo" "14ib003sxpxc8awywjckbw124aqhmi70wg4hlwc2nvdh46sqk11b")
(pdf-package NNPDF31_nnlo_hessian_pdfas "NNPDF31_nnlo_hessian_pdfas" "1m5wdnj7hvg2a51w1qmhm934glhjf9db0x5nry5blp90y5x0v185")

lhapdf
CT10nlo
NNPDF31_nnlo_hessian_pdfas

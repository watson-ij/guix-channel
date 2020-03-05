(define-module (physics cadabra)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages boost)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses))

(define-public cadabra2
  (package
   (name "cadabra2") (version "git-240b7a")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/kpeeters/cadabra2/archive/240b7a8dfb631011b8205cbf6415c5667877496c.tar.gz")
     (sha256 (base32 "0s8rdznbrnnkg5sy1plcqggbzmn953c7srn9mq198vziz8irdcv5"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags
      (list
       (string-append "-DSQLITE3_INCLUDE_DIR=" (assoc-ref %build-inputs "sqlite") "/include"))
      #:phases
      (modify-phases
       %standard-phases
       (add-after 'install 'link-more
	(lambda* (#:key inputs outputs #:allow-other-keys)
	  (let ((out (assoc-ref outputs "out")))
	    (mkdir-p (string-append out "/lib/python3.7/site-packages/"))
	    (for-each (lambda (file)
			(symlink (string-append out "/share/cadabra2/python/" file)
				 (string-append out "/lib/python3.7/site-packages/" file)))
	     '("cadabra2.so" "cadabra2_defaults.py" "cdb" "cdb_appdirs.py"))
	    #t)))
       (delete 'check))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs `(("boost" ,boost-with-python3)
	     ("gmp" ,gmp)
	     ("glibmm" ,glibmm)
	     ("gtkmm" ,gtkmm)
	     ("python" ,python)
	     ("sqlite" ,sqlite)
	     ("sympy" ,python-sympy)
	     ))
   (home-page "https://cadabra.science/")
   (synopsis "a field-theory motivated approach to computer algebra ")
   (description "Cadabra is a symbolic computer algebra system (CAS) designed specifically for the solution of problems encountered in field theory. It has extensive functionality for tensor computer algebra, tensor polynomial simplification including multi-term symmetries, fermions and anti-commuting variables, Clifford algebras and Fierz transformations, component computations, implicit coordinate dependence, multiple index types and many more. The input format is a subset of TeX. Both a command-line and a graphical interface are available.")
   (license gpl3)))

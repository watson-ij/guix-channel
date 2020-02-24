(define-module (physics root)
             #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system cmake)
               #:use-module (guix build-system gnu)
               #:use-module ((guix licenses) #:prefix licenses:)
               #:use-module (gnu packages algebra)
               #:use-module (gnu packages base)
               #:use-module (gnu packages bash)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages commencement)
               #:use-module (gnu packages curl)
               #:use-module (gnu packages astronomy)
               #:use-module (gnu packages digest)
               #:use-module (gnu packages maths)
               #:use-module (gnu packages pkg-config)
               #:use-module (gnu packages fontutils)
               #:use-module (gnu packages image)
               #:use-module (gnu packages gcc)
               #:use-module (gnu packages sqlite)
               #:use-module (gnu packages pcre)
               #:use-module (gnu packages tbb)
               #:use-module (gnu packages xml)
	       #:use-module (gnu packages xorg)
	       #:use-module (gnu packages gl)
	       #:use-module (gnu packages tbb)
	       #:use-module (gnu packages python-xyz)
               #:use-module (gnu packages python))

(define-public root-5
  (package
   (name "root")
   (version "5.34.38")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://root.cern.ch/download/root_v"
	   version
	   ".source.tar.gz"))
     (sha256 (base32
	      "1ln448lszw4d6jmbdphkr2plwxxlhmjkla48vmmq750xc1lxlfrc"))))
   (build-system cmake-build-system)
   ;; (build-system gnu-build-system)
   (native-inputs `(("sed" ,sed) ("pkg-config" ,pkg-config)))
   (inputs `(
	     ("cc" ,gcc-toolchain)
	     ("fortran" ,gfortran)
	     ("python" ,python-2.7)
	     ("python-numpy" ,python2-numpy)
	     ("bash" ,bash)
	     ("gsl" ,gsl)
	     ("pcre" ,pcre)
					; ("Zlib" ,zlib)
					; "lzma" ,lzma
	     ("lz4" ,lz4) ("xxhash" ,xxhash)
	     ("xz" ,xz)
	     ("gsl" ,gsl)
	     ("fftw3" ,fftw)
	     ("libpng" ,libpng-1.2)
	     ("libjpeg" ,libjpeg)
	     ("libtiff" ,libtiff)
	     ("sqlite" ,sqlite)

	     ("cfitsio" ,cfitsio)

	     ("libxml2" ,libxml2)

	     ("pthread" ,libpthread-stubs)
	     ("libX11" ,libx11)
	     ("libXpm" ,libxpm)
	     ("libXft" ,libxft)
	     ("libXext" ,libxext)
	     ("libGLU" ,glu)
	      ; libGLU libGL
	      ))
   (arguments
    `(#:configure-flags
      '(
	"-Drpath=ON"
	"-DCMAKE_INSTALL_LIBDIR=lib"
	"-DCMAKE_INSTALL_INCLUDEDIR=include"
	"-Dgviz=OFF"
	"-Dhdfs=OFF"
	"-Dkrb5=OFF"
	"-Dldap=OFF"
	"-Dalien=OFF"
	"-Dbonjour=OFF"
	"-Dcastor=OFF"
	"-Dchirp=OFF"
	"-Ddavix=OFF"
	"-Ddcache=OFF"
	"-Dfftw3=ON"
	"-Dfitsio=ON"
	"-Dfortran=ON"
	"-Dgfal=OFF"
	"-Dssl=OFF"
	"-Droofit=ON"
	"-Dtmva=ON"
	"-Dmathmore=ON"
	"-Dmysql=OFF"
	"-Dmonalisa=OFF"
	"-Dasimage=ON"
	"-Dbuiltin_afterimage=ON"
	)
      #:phases
      (modify-phases
       %standard-phases
       (delete 'check)
       (add-after
	'unpack 'patch-paths
	(lambda* (#:key inputs #:allow-other-keys)
	  (substitute* "cmake/modules/FindPCRE.cmake"
		       (("set.PCRE_FOUND 0.")
			(string-append "set(PCRE_FOUND 1)
set(PCRE_PREFIX \"" (assoc-ref inputs "pcre") "\")
set(PCRE_INCLUDE_DIR \"${PCRE_PREFIX}/include\")
set(PCRE_LIBRARIES \"-L${PCRE_PREFIX}/lib -lpcre\")")))
	  (substitute* "graf2d/asimage/src/libAfterImage/configure"
		       (("#! /bin/sh")
			(string-append "#!" (assoc-ref inputs "bash") "/bin/bash"))
		       (("SHELL=")
			(string-append "SHELL=" (assoc-ref inputs "bash") "/bin/bash #")))
	  #t)))
      ))
   (native-search-paths
    ;; This is a Guix-specific environment variable that takes a single
    ;; entry, not an actual search path.
    (list (search-path-specification
	   (variable "PYTHONPATH")
	   (files '("lib")))
	  (search-path-specification
	   (variable "ROOTSYS")
	   (files '("")))))
   (synopsis "ROOT - Data Analysis Framework")
   (description "ROOT - Data Analysis Framework")
   (home-page "https://root.cern.ch/")
					;   (license lgpl2.1)
   (license licenses:bsd-3)
   ))



(define-public root-6
  (package
   (name "root")
   (version "6.18.04")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://root.cern.ch/download/root_v"
	   version
	   ".source.tar.gz"))
     (sha256 (base32
	      "196ghma6g5a7sqz52wyjkgvmh4hj4vqwppm0zwdypy33hgy8anii"))))
   (build-system cmake-build-system)
   ;; (build-system gnu-build-system)
   (native-inputs `(("sed" ,sed) ("pkg-config" ,pkg-config)))
   (inputs `(("cc" ,gcc-toolchain)
	     ("fortran" ,gfortran)
	     ("python" ,python)
	     ("bash" ,bash)
	     ("gsl" ,gsl)
	     ("pcre" ,pcre)
	     ("curl" ,curl)
					; ("Zlib" ,zlib)
					; "lzma" ,lzma
	     ("lz4" ,lz4) ("xxhash" ,xxhash)
	     ("xz" ,xz)
	     ("gsl" ,gsl)
	     ("fftw3" ,fftw)
	     ("libpng" ,libpng-1.2)
	     ("libjpeg" ,libjpeg)
	     ("libtiff" ,libtiff)
	     ("sqlite" ,sqlite)

	     ("cfitsio" ,cfitsio)

	     ("libxml2" ,libxml2)

	     ("pthread" ,libpthread-stubs)
	     ("libX11" ,libx11)
	     ("libXpm" ,libxpm)
	     ("libXft" ,libxft)
	     ("libXext" ,libxext)
	     ("libGLU" ,glu)
	     ("mesa" ,mesa)
					; libGLU libGL
	     ("tbb" ,tbb)
	      ))
   (arguments
    `(#:configure-flags
      (list
	"-Drpath=ON"
	"-DCMAKE_INSTALL_LIBDIR=lib"
	"-DCMAKE_INSTALL_INCLUDEDIR=include"
	(string-append "-DC_INCLUDE_DIRS=" (assoc-ref %build-inputs "cc") "/include")
	"-Dcxx17=ON"
	"-Droot7=ON"
	"-Dgviz=OFF"
	"-Dhdfs=OFF"
	"-Dkrb5=OFF"
	"-Dldap=OFF"
	"-Dalien=OFF"
	"-Dbonjour=OFF"
	"-Dcastor=OFF"
	"-Dchirp=OFF"
	"-Ddavix=OFF"
	"-Ddcache=OFF"
	"-Dfftw3=ON"
	"-Dpng=ON"
	"-Dfitsio=ON"
	"-Dfortran=ON"
	"-Dgfal=OFF"
	"-Dssl=OFF"
	"-Droofit=ON"
	"-Dtmva=ON"
	"-Dmathmore=ON"
	"-Dmysql=OFF"
	"-Dmonalisa=OFF"
	"-Dclad=OFF"
	"-Dvdt=OFF"
	"-Dasimage=ON"
	"-Dbuiltin_afterimage=ON"
	)
      #:phases
      (modify-phases
       %standard-phases
       (delete 'check)
       (add-after
	'unpack 'patch-paths
	(lambda*
	 (#:key inputs outputs #:allow-other-keys)
	 (substitute* "graf2d/asimage/src/libAfterImage/configure"
		      (("#! /bin/sh")
		       (string-append "#!" (assoc-ref inputs "bash") "/bin/bash"))
		      (("SHELL=")
		       (string-append "SHELL=" (assoc-ref inputs "bash") "/bin/bash #")))))

       )))
   (native-search-paths
    ;; This is a Guix-specific environment variable that takes a single
    ;; entry, not an actual search path.
    (list (search-path-specification
	   (variable "PYTHONPATH")
	   (files '("lib")))
	  (search-path-specification
	   (variable "ROOTSYS")
	   (files '("")))))
   (synopsis "ROOT - Data Analysis Framework")
   (description "ROOT - Data Analysis Framework")
   (home-page "https://root.cern.ch/")
   (license licenses:lgpl2.1)
   ))

(define-public root root-6)
root-5

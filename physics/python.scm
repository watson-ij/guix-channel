;; Misc. python projects

(define-module (physics python)
  #:use-module (physics hepmc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz))

(define-public hepunits
  (package
   (name "hepunits")
   (version "dbc7b228548ebe4afdf1067a6c5ee1e8a42990ef")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/scikit-hep/hepunits/archive/"
	   version
	   ".tar.gz"))
     (sha256 (base32
	      "14ailp5m1gfsgfx4c2qvis56bp4hp73dzgvbqmdlrlg7mq1ipi97"))))
   (build-system python-build-system)
   (native-inputs `(("pytest" ,python-pytest)
		    ("pytest-runner" ,python-pytest-runner)))
   (synopsis "Units for HEP")
   (description "")
   (home-page "https://github.com/scikit-hep/hepunits")
   (license bsd-3)))

(define-public particle
  (package
   (name "particle")
   (version "fecf918885d7a5a5d297213148ed8bd4fd317e0e")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/scikit-hep/particle/archive/"
	   version
	   ".tar.gz"))
     (sha256 (base32
	      "0d5bq0yqx1704j2jqilb7xjqzw0ris7r9cj2y3sn1hdyixamdqgl"))))
   (build-system python-build-system)
   (native-inputs `(("pytest" ,python-pytest)
		    ("pytest-runner" ,python-pytest-runner)
		    ("pandas" ,python-pandas)))
   (inputs `(("python-attrs" ,python-attrs)
	     ("python-tabulate" ,python-tabulate)
	     ("hepunits" ,hepunits)))
   (synopsis "Particle ID for HEP")
   (description "")
   (home-page "https://github.com/scikit-hep/particle")
   (license bsd-3)))

(define-public pyhepmc
  (package
   (name "pyhepmc")
   (version "753072aed9914b3fa88ca03e9d02a1e19e68523b")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/scikit-hep/pyhepmc/archive/"
	   version
	   ".tar.gz"))
     (sha256 (base32
	      "103kxxaldrnwcbxf5d7hjz5j7knm6j5vlv5v0jqldj6p7yv7fzx6"))))
   (build-system python-build-system)
   (native-inputs `(("pybind11" ,pybind11)
		    ("pytest" ,python-pytest)
		    ("pytest-runner" ,python-pytest-runner)))
   (inputs `(("particle" ,particle)
	     ("hepmc" ,hepmc-3)
	     ("python-numpy" ,python-numpy)
	     ("python-pygraphviz" ,python-pygraphviz)))
   (synopsis "HEPMC Reader for Python")
   (description "")
   (home-page "https://github.com/scikit-hep/pyhepmc")
   (license bsd-3)))

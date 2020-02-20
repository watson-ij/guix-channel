(define-module (physics madgraph)
  #:use-module (physics pythia)
  #:use-module (physics hepmc)
  #:use-module (physics lhapdf)
  #:use-module (physics fastjet)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix licenses)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python))

(define-public madgraph
  (package
   (name "madgraph")
   (version "2.7.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://launchpad.net/mg5amcnlo/2.0/2.7.x/+download/MG5_aMC_v2.7.0.tar.gz")
     (sha256
      (base32
       "0khy6ijfcjhdc608rx061iyxn1yldh0is72yx8p73z78npn1w037")))
    )
   (build-system trivial-build-system)
   (native-inputs `(
		    ("libtirpc" ,libtirpc)
		    ))
   (inputs `(("bash" ,bash)
	     ("coreutils" ,coreutils)
	     ("find" ,findutils)
	     ("wget" ,wget)
	     ("tar" ,tar)
	     ("boost" ,boost)
	     ("gzip" ,gzip)
	     ("bc" ,bc)
	     ("make" ,gnu-make)
	     ("pkgconfig" ,pkg-config)
	     ("python" ,python-2.7)
	     ("numpy" ,python2-numpy)
	     ("hepmc" ,hepmc)
	     ("fastjet" ,fastjet)
	     ("lhapdf" ,lhapdf)
	     ("pythia" ,pythia)
	     ("fortran" ,gfortran-toolchain)
	     ("perl" ,perl)
	     ("zlib" ,zlib)
	     ("sed" ,sed)
	     ("grep" ,grep)
	     ("headers" ,linux-libre-headers)
	     ("gs" ,ghostscript)
	     ("which" ,which)
	     ("gcc" ,gcc-toolchain)

	     ;; from the offline heptools file
	     ("ma5"  ,(origin
		    (method url-fetch)
		    ;; (uri "http://madanalysis.irmp.ucl.ac.be/raw-attachment/wiki/MA5SandBox/ma5_latest.tgz")
		    ;; (sha256 (base32 "0l1v7ykf5idci5j1mvfkr3cnq9kx931z98sp3isvgpi3qaiwispz"))
		    (uri "https://launchpad.net/madanalysis5/trunk/v1.7/+download/ma5_v1.7.tgz")
		    (sha256 (base32 "0wsjk8h92zylygz72ggmvqbdkdbbdc7xvl5z4iiz4bfldgm13jdr"))
		    ;; (uri "https://launchpad.net/madanalysis5/trunk/v1.6/+download/ma5_v1.6.tgz")
		    ;; (sha256 (base32 "1dy9vqvxvgm53n6nz8pf6x8xbxibhlfkca1x5jvfgcwhq7hw2h4m"))
		    ))
	     
					; input list http://madgraph.phys.ucl.ac.be/package_info.dat
	     ("mg5amc_py8"
	      ,(origin
		(method url-fetch)
		(uri "http://madgraph.phys.ucl.ac.be/Downloads/MG5aMC_PY8_interface/MG5aMC_PY8_interface_V1.0.tar.gz")
		(sha256 (base32
			 "1l1fxpa279vm6la4h79wpnqf1vydqrbrp93la1ybymfkq1yqzgc1"
			 ))))

					; example of adding an additional model
	     ("zprime"
	      ,(origin
		(method url-fetch)
		(uri "https://cms-project-generators.web.cern.ch/cms-project-generators/ZprimeToMuMu/zprime_UFO.tar.gz")
		(sha256 (base32 "09qiqr25r6clban4ph3gcq8ikz1y6yhvdxwahwwp7mmp7fd95j96"))))
	     
	     ))
   (propagated-inputs `(("gfortran" ,gfortran)
			))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
	(use-modules (guix build utils))
	(let* ((out (assoc-ref %outputs "out"))
	       (bash (assoc-ref %build-inputs "bash"))
	       (tar (assoc-ref %build-inputs "tar"))
	       (gzip (assoc-ref %build-inputs "gzip"))
	       (wget (assoc-ref %build-inputs "wget"))
	       (lhapdf (assoc-ref %build-inputs "lhapdf"))
	       (python (assoc-ref %build-inputs "python"))
	       (numpy (assoc-ref %build-inputs "numpy"))
	       (fortran (assoc-ref %build-inputs "fortran"))
	       (gcc (assoc-ref %build-inputs "gcc"))
	       (boost (assoc-ref %build-inputs "boost"))
	       (bc (assoc-ref %build-inputs "bc"))
	       (make (assoc-ref %build-inputs "make"))
	       (find (assoc-ref %build-inputs "find"))
	       (pkgconfig (assoc-ref %build-inputs "pkgconfig"))
	       (perl (assoc-ref %build-inputs "perl"))
	       (sed (assoc-ref %build-inputs "sed"))
	       (zlib (assoc-ref %build-inputs "zlib"))
	       (headers (assoc-ref %build-inputs "headers"))
	       (which (assoc-ref %build-inputs "which"))
	       (libtirpc (assoc-ref %build-inputs "libtirpc"))
	       (grep (assoc-ref %build-inputs "grep"))
	       (gs (assoc-ref %build-inputs "gs"))
	       (pythia (assoc-ref %build-inputs "pythia"))
	       (mg5amc-py8 (assoc-ref %build-inputs "mg5amc_py8"))
	       (ma5 (assoc-ref %build-inputs "ma5"))
	       (hepmc (assoc-ref %build-inputs "hepmc"))
	       (fastjet (assoc-ref %build-inputs "fastjet"))
	       (source (assoc-ref %build-inputs "source"))
	       (zprime (assoc-ref %build-inputs "zprime"))
	       (coreutils (assoc-ref %build-inputs "coreutils")))
	  (mkdir-p out)
	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" gcc "/bin:" python "/bin && "
		   tar "/bin/tar "
		   " -xf " source " && "
		   "python MG5_aMC_v2_7_0/bin/mg5"
		   ))

	  ;; fortran needs to link to the runtime setup
	  ;; (substitute* '("MG5_aMC_v2_7_0/Template/LO/Source/make_opts" "MG5_aMC_v2_7_0/Template/NLO/Source/make_opts.inc")
	  ;; 	       (("# Options: dynamic, lhapdf") (string-append "LDFLAGS += -L" fortran "/lib \n# Options: dynamic, lhapdf")))

	  ;; lhapdf links to boost ?
	  (substitute* '("MG5_aMC_v2_7_0/Template/LO/Source/make_opts"
			 "MG5_aMC_v2_7_0/Template/NLO/Source/make_opts.inc")
	  	       (("ifneq \\(\\$\\(lhapdf\\),\\)\n")
			(string-append "ifneq ($(lhapdf),)\nCXXFLAGS += -I" boost "/include -I"
				       headers "/include -I"
				       libtirpc "/include/tirpc\n")))

	  (substitute* '("MG5_aMC_v2_7_0/bin/mg5" "MG5_aMC_v2_7_0/bin/mg5_aMC")
	  	       (("#! /usr/bin/env python")
	  		(string-append "#!" python "/bin/python"))
	  	       (("import os\n")
	  		(string-append "import os\n"
	  			       "os.environ[\"PATH\"] = \""
	  			       fortran "/bin:"
	  			       coreutils "/bin:"
	  			       make "/bin:"
	  			       sed "/bin:"
	  			       grep "/bin:"
	  			       tar "/bin:"
	  			       bc "/bin:"
	  			       gzip "/bin:"
	  			       find "/bin:"
	  			       gs "/bin:"
	  			       gcc "/bin:\"+os.getenv(\"PATH\")\n"
				       "os.environ[\"LIBRARY_PATH\"] = \""
				       fortran "/lib:\""
				       "+(os.getenv(\"LIBRARY_PATH\") or \"\")\n"
				       "os.environ[\"FC\"] = \""
				       fortran "/bin/gfortran\"\n"
	  			       ;; "os.environ[\"LD_LIBRARY_PATH\"] = \""
	  			       ;; fortran "/lib:"
	  			       ;; gcc "/lib:\"+(os.getenv(\"LD_LIBRARY_PATH\") or \"\")\n"
	  			       ;; "\n"
	  			       ))
	  	       )
	  
	  (substitute* (append (find-files "MG5_aMC_v2_7_0/Template/LO/bin" "$")
			       (find-files "MG5_aMC_v2_7_0/Template/MadWeight/bin" "$")
			       (find-files "MG5_aMC_v2_7_0/Template/NLO/bin" "$")
			       (find-files "MG5_aMC_v2_7_0/Template/Common/bin" "$")
			       (find-files "MG5_aMC_v2_7_0/Template/NLO/SubProcesses/" ".*sh$")
			       (find-files "MG5_aMC_v2_7_0/Template/NLO/SubProcesses/ajob_template" "$")
			       ;; (find-files "MG5_aMC_v2_7_0/" ".*py$")
			       (find-files "MG5_aMC_v2_7_0/Template/LO/SubProcesses/" ".*sh$"))
		       (("/bin/bash") (string-append bash "/bin/bash"))
		       (("#!/usr/bin/perl") (string-append "#!" perl "/bin/perl"))
		       (("#! /usr/bin/env python")
			(string-append "#!" python "/bin/python"))
		       (("#!/usr/bin/env python")
			(string-append "#!" python "/bin/python"))
		       (("#!/usr/bin/python")
			(string-append "#!" python "/bin/python")))
	  ; fix permissions after copying files
	  (substitute* '("MG5_aMC_v2_7_0/madgraph/iolibs/export_v4.py" "MG5_aMC_v2_7_0/madgraph/iolibs/export_fks.py")
		       (("self.dir_path)\n")
			"self.dir_path); os.system('chmod -R +w '+self.dir_path)\n"))
	  (substitute* '("MG5_aMC_v2_7_0/madgraph/iolibs/export_v4.py" "MG5_aMC_v2_7_0/madgraph/iolibs/export_fks.py")
		       (("dir_path, True)") "dir_path, True); os.system('chmod -R +w '+self.dir_path)"))
	  (substitute* '("MG5_aMC_v2_7_0/madgraph/iolibs/export_v4.py" "MG5_aMC_v2_7_0/madgraph/iolibs/export_fks.py")
		       (("internal/cluster.py')") "internal/cluster.py'); os.system('chmod -R +w '+self.dir_path)"))
	  (substitute* '("MG5_aMC_v2_7_0/madgraph/iolibs/export_v4.py" "MG5_aMC_v2_7_0/madgraph/iolibs/export_fks.py")
		       (("ignore=shutil.ignore_patterns\\(\\*IGNORE_PATTERNS\\)\\)") "ignore=shutil.ignore_patterns(*IGNORE_PATTERNS)); os.system('chmod -R +w '+self.dir_path)"))

	  (substitute* "MG5_aMC_v2_7_0/input/mg5_configuration.txt"
		       (("# fortran_compiler = None")
			(string-append "fortran_compiler = " fortran "/bin/gfortran"))
		       (("# f2py_compiler = None")
			(string-append "f2py_compiler = " numpy "/bin/f2py"))
		       (("# cpp_compiler = None")
			(string-append "cpp_compiler = " gcc "/bin/g++"))
		       (("# pythia8_path = ./HEPTools/pythia8")
			(string-append "pythia8_path = " pythia ))
		       (("# hepmc_path = ")
			(string-append "hepmc_path = " hepmc))
		       (("# lhapdf = lhapdf-config")
			(string-append "lhapdf = " lhapdf "/bin/lhapdf-config"))
		       (("# fastjet = fastjet-config")
			(string-append "fastjet = " fastjet "/bin/fastjet-config"))
		       ;; (("# run_mode = 2")
		       ;; 	"run_mode = 0")
		       )
	  
	  (substitute* "MG5_aMC_v2_7_0/madgraph/iolibs/files.py"
		       (("import madgraph.various.misc as misc\n")
			"import madgraph.various.misc as misc; print(\"failure\",why)\n")
		       (("def rm\\(path")
			"    os.system(\"chmod +w \"+path2)\ndef rm(path"))

	  (substitute* "MG5_aMC_v2_7_0/vendor/StdHEP/src/stdhep/GNUmakefile"
		       (("INCS = ") (string-append "INCS = -I"
						   headers "/include -I"
						   libtirpc "/include/tirpc "))
		       (("CFLAGS \\+= ") (string-append "CFLAGS += -I"
							headers "/include -I"
							libtirpc "/include/tirpc "))
		       )
	  (substitute* "MG5_aMC_v2_7_0/vendor/StdHEP/mcfio/src/GNUmakefile"
		       (("FINC = ") (string-append
				     "FINC = -I"
				     headers "/include -I"
				     libtirpc "/include/tirpc "))
		       (("CINC = ") (string-append
				     "CINC = -I"
				     headers "/include -I"
				     libtirpc "/include/tirpc "))
		       )

	  (substitute* "MG5_aMC_v2_7_0/Template/NLO/SubProcesses/makefile_fks_dir"
		       (("-I\\.") (string-append "-I" headers "/include -I.")))

	  ;; mg5-pythia8 interface
	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" wget "/bin:" coreutils "/bin:" python "/bin:" gcc "/bin:" make "/bin:" bash "/bin:" fortran "/bin:" tar "/bin && "
		   "mkdir -p MG5_aMC_v2_7_0/HEPTools && tar -C MG5_aMC_v2_7_0/HEPTools -xf MG5_aMC_v2_7_0/vendor/OfflineHEPToolsInstaller.tar.gz &&"
		   "mkdir -p MG5_aMC_v2_7_0/HEPTools/MG5aMC_PY8_interface && tar -C MG5_aMC_v2_7_0/HEPTools/MG5aMC_PY8_interface -xf " mg5amc-py8))	  

	  (substitute* "MG5_aMC_v2_7_0/HEPTools/MG5aMC_PY8_interface/Makefile_mg5amc_py8_interface_static"
		       (("-I\\$\\(HEPMC2_INCLUDE\\)") (string-append "-I$(HEPMC2_INCLUDE) -I"
						   headers "/include -I"
						   libtirpc "/include/tirpc "))
		       )
	  (substitute* "MG5_aMC_v2_7_0/HEPTools/MG5aMC_PY8_interface/MG5aMC_PY8_interface.cc"
		       (("\\\\\\%") "%"))

	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" wget "/bin:" coreutils "/bin:" python "/bin:" gcc "/bin:" make "/bin:" bash "/bin:" fortran "/bin:" tar "/bin && "
		   "export LIBRARY_PATH=" gcc "/lib &&"
		   "cd MG5_aMC_v2_7_0/HEPTools/MG5aMC_PY8_interface && python compile.py " pythia " && cd - &&"
		   "echo DONE"))

	  (substitute* "MG5_aMC_v2_7_0/input/mg5_configuration.txt"
		       (("# mg5amc_py8_interface_path = ./HEPTools/MG5aMC_PY8_interface")
			(string-append "mg5amc_py8_interface_path = " out "/HEPTools/MG5aMC_PY8_interface")))

	  (substitute* "MG5_aMC_v2_7_0/input/mg5_configuration.txt"
		       (("# mg5amc_py8_interface_path = ./HEPTools/MG5aMC_PY8_interface")
			(string-append "mg5amc_py8_interface_path = " out "/HEPTools/MG5aMC_PY8_interface")))
	  
	  ;; ma5
	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" wget "/bin:" coreutils "/bin:" python "/bin:" gcc "/bin:" make "/bin:" bash "/bin:" fortran "/bin:" tar "/bin && "
		   "mkdir -p MG5_aMC_v2_7_0/HEPTools && "
		   "mkdir -p MG5_aMC_v2_7_0/HEPTools/ && tar -C MG5_aMC_v2_7_0/HEPTools/ -xf " ma5))

	  ; veto everything (for now)
	  (substitute* "MG5_aMC_v2_7_0/HEPTools/madanalysis5/madanalysis/input/installation_options.dat"
		       (("# webaccess_veto = 0") "webaccess_veto = 1")
		       (("# root_veto     = 0") "root_veto = 1")
		       (("# matplotlib_veto = 0") "matplotlib_veto = 1")
		       (("# delphes_veto     = 0") "delphes_veto = 1")
		       (("# delphesMA5tune_veto     = 0") "delphesMA5tune_veto = 1")
		       (("# latex_veto     = 0") "latex_veto = 1")
		       (("# pdflatex_veto     = 0") "pdflatex_veto = 1")
		       (("# fastjet_veto     = 0") "fastjet_veto = 1")
		       (("# zlib_veto     = 0") "zlib_veto = 1")
		       (("# scipy_veto = 0") "scipy_veto = 1")
		       )

	  (substitute* "MG5_aMC_v2_7_0/HEPTools/madanalysis5/madanalysis/build/makefile_writer.py"
		       (("cxxflags.extend\\(\\['-Wall'")
			(string-append
			 "file.write('LIBRARY_PATH=" gcc "/lib\\n')\n        "
			 "cxxflags.extend(['-I" headers "/include', '-I" libtirpc "/include/tirpc', '-Wall'"))
		       (("libs.extend\\(\\['-lz'\\]\\)") (string-append "libs.extend(['-L" zlib "/lib', '-lz'])"))
		       )
		   
	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" fastjet "/bin:" which "/bin:" wget "/bin:" coreutils "/bin:" python "/bin:" gcc "/bin:" make "/bin:" bash "/bin:" fortran "/bin:" tar "/bin && "
		   "python MG5_aMC_v2_7_0/HEPTools/HEPToolsInstallers/installMadAnalysis5.py --ma5_path=$PWD/MG5_aMC_v2_7_0/HEPTools/madanalysis5 --mg5_path=MG5_aMC_v2_7_0/ --zlib=" zlib
		   "&& echo DONE"))

	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" wget "/bin:" coreutils "/bin:" python "/bin:" gcc "/bin:" make "/bin:" bash "/bin:" fortran "/bin:" tar "/bin && "
		   "export LIBRARY_PATH=" gcc "/lib &&"
		   "(echo "
		   " \"generate p p > Z [QCD] \noutput abcd\n\n\" "
		   " | "
		   python "/bin/python MG5_aMC_v2_7_0/bin/mg5_aMC)"))

	  ; example of adding an additional model
	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append
		   "export PATH=" gzip "/bin:" wget "/bin:" coreutils "/bin:" python "/bin:" gcc "/bin:" make "/bin:" bash "/bin:" fortran "/bin:" tar "/bin && "
		   "tar -C MG5_aMC_v2_7_0/models -xf " zprime))
	  
	  ; finally we're done, copy to the output directory
	  (invoke (string-append bash "/bin/bash") "-c"
		  (string-append coreutils "/bin/cp -r MG5_aMC_v2_7_0/* " out))
	  )))
    ; madanalysis5
    ;http://madanalysis.irmp.ucl.ac.be/raw-attachment/wiki/MA5SandBox/ma5_latest.tgz
    )
   (home-page "https://launchpad.net/mg5amcnlo")
   (synopsis "MadGraph5_aMC@NLO")
   (description "
MadGraph5_aMC@NLO is a framework that aims at providing all the elements necessary for SM and BSM phenomenology, such as the computations of cross sections, the generation of hard events and their matching with event generators, and the use of a variety of tools relevant to event manipulation and analysis. Processes can be simulated to LO accuracy for any user-defined Lagrangian, an the NLO accuracy in the case of models that support this kind of calculations -- prominent among these are QCD and EW corrections to SM processes. Matrix elements at the tree- and one-loop-level can also be obtained.")
   (license bsd-3)
   ))
madgraph

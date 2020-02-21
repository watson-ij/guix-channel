(define-module (physics madgraph)
  #:use-module (physics pythia)
  #:use-module (physics hepmc)
  #:use-module (physics lhapdf)
  #:use-module (physics fastjet)
  #:use-module (physics delphes)
  #:use-module (physics root)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gawk)
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
  #:use-module (gnu packages less)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python))

(define-public madgraph
  (package
   (name "madgraph")
   (version "2.6.7")
   (source
    (origin
     (method url-fetch)
     ;; (uri "https://launchpad.net/mg5amcnlo/2.0/2.7.x/+download/MG5_aMC_v2.7.0.tar.gz")
     ;; (sha256
     ;;  (base32
     ;;   "0khy6ijfcjhdc608rx061iyxn1yldh0is72yx8p73z78npn1w037")))
     (uri "https://launchpad.net/mg5amcnlo/2.0/2.6.x/+download/MG5_aMC_v2.6.7.tar.gz")
     (sha256
      (base32
       "1cacvs2hff9c808k29ac6lb3k6bzfla7bqn99gcbf5i09cd9knyl")))
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
	     ("delphes" ,delphes)
	     ("lhapdf" ,lhapdf)
	     ("pythia" ,pythia)
	     ("root" ,root)
	     ("fortran" ,gfortran-toolchain)
	     ("perl" ,perl)
	     ("zlib" ,zlib)
	     ("sed" ,sed)
	     ("grep" ,grep)
	     ("less" ,less)
	     ("headers" ,linux-libre-headers)
	     ("gs" ,ghostscript)
	     ("awk" ,gawk)
	     ("which" ,which)
	     ("gnuplot" ,gnuplot)
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

	     ;; input list http://madgraph.phys.ucl.ac.be/package_info.dat
	     ;; MG5aMC_PY8_interface http://madgraph.phys.ucl.ac.be/Downloads/MG5aMC_PY8_interface/MG5aMC_PY8_interface_V1.0.tar.gz
	     ;; QCDLoop http://madgraph.phys.ucl.ac.be/Downloads/qcdloop1-1.9.0.tar.gz
	     ;; Delphes3 http://cp3.irmp.ucl.ac.be/downloads/Delphes-3-current.tar.gz
	     ;; Delphes2 http://cp3.irmp.ucl.ac.be/downloads/Delphes-2-current.tar.gz
	     ;; MCatNLO-utilities http://madgraph.phys.ucl.ac.be/Downloads/MCatNLO-utilities_V3.6.tar.gz
	     ;; SysCalc http://madgraph.phys.ucl.ac.be/Downloads/SysCalc_V1.1.7.tar.gz
	     ;; maddump http://madgraph.phys.ucl.ac.be/Downloads/maddump/maddump_V1.0.4.tar.gz
	     ;; MadAnalysis http://madgraph.phys.ucl.ac.be/Downloads/MadAnalysis_V1.1.8.tar.gz
	     ;; Golem95 https://www.hepforge.org/archive/golem/golem95-1.3.1.tar.gz
	     ;; pythia-pgs http://madgraph.phys.ucl.ac.be/Downloads/pythia-pgs_V2.4.5.tar.gz
	     ;; lhapdf http://www.hepforge.org/archive/lhapdf/lhapdf-5.9.1.tar.gz
	     ;; rosetta http://madgraph.phys.ucl.ac.be/Downloads/rosetta/rosetta_V2.1.tar.gz
	     ;; Delphes http://cp3.irmp.ucl.ac.be/downloads/Delphes-2-current.tar.gzDelphes_V_2.0.3.tar.gz
	     ;; HEPToolsInstaller http://madgraph.phys.ucl.ac.be/Downloads/HEPToolsInstaller/HEPToolsInstaller_V126.tar.gz
	     ;; PPPC4DMID http://madgraph.phys.ucl.ac.be/Downloads/maddm/PPPC4DMID_V1.9.tar.gz
	     ;; MadAnalysis5 http://madanalysis.irmp.ucl.ac.be/raw-attachment/wiki/PhysicsAnalysisDatabase/MadAnalysis5-current.tgz
	     ;; maddm http://madgraph.phys.ucl.ac.be/Downloads/maddm/maddm_V3.0.7.tar.gz
	     ;; PJFry http://madgraph.phys.ucl.ac.be/Downloads/pjfry-1.1.0-beta2.tar.gz
	     ;; ExRootAnalysis http://madgraph.phys.ucl.ac.be/Downloads/ExRootAnalysis/ExRootAnalysis_V1.1.5.tar.gz
	     ("mg5amc_py8"
	      ,(origin
		(method url-fetch)
		; the madgraph site seems to have some kind of counter that ruins the sha, so take it from a github clone
		(uri "https://github.com/HEPcodes/MG5aMC_PY8_interface/archive/c1b01c2721826ccd6eb0f583d9b6ea77df1a320e.tar.gz")
		(sha256 (base32 "17iygvphaks9dk22pfmwqi12gjznlvzmdlrycwlbdrjfwz64r38y"))
		;; (uri "http://madgraph.phys.ucl.ac.be/Downloads/MG5aMC_PY8_interface/MG5aMC_PY8_interface_V1.0.tar.gz")
		;; (sha256 (base32
		;; 	 "1l1fxpa279vm6la4h79wpnqf1vydqrbrp93la1ybymfkq1yqzgc1"))
		))

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
	       (delphes (assoc-ref %build-inputs "delphes"))
	       (which (assoc-ref %build-inputs "which"))
	       (libtirpc (assoc-ref %build-inputs "libtirpc"))
	       (grep (assoc-ref %build-inputs "grep"))
	       (gs (assoc-ref %build-inputs "gs"))
	       (root (assoc-ref %build-inputs "root"))
	       (awk (assoc-ref %build-inputs "awk"))
	       (less (assoc-ref %build-inputs "less"))
	       (pythia (assoc-ref %build-inputs "pythia"))
	       (mg5amc-py8 (assoc-ref %build-inputs "mg5amc_py8"))
	       (ma5 (assoc-ref %build-inputs "ma5"))
	       (hepmc (assoc-ref %build-inputs "hepmc"))
	       (fastjet (assoc-ref %build-inputs "fastjet"))
	       (gnuplot (assoc-ref %build-inputs "gnuplot"))
	       (source (assoc-ref %build-inputs "source"))
	       (zprime (assoc-ref %build-inputs "zprime"))
	       (coreutils (assoc-ref %build-inputs "coreutils")))
	  (mkdir-p out)
	  (setenv "PATH" (string-append coreutils "/bin:" gzip "/bin:" gcc "/bin:" fortran "/bin:"
					python "/bin:" wget "/bin:" make "/bin:" tar "/bin:" root "/bin:"
					gs "/bin:" find "/bin:" sed "/bin:" grep "/bin:" awk "/bin:" less "/bin:"
					bc "/bin:" which "/bin:" gnuplot "/bin:" fastjet "/bin:" bash "/bin"))
	  (setenv "LIBRARY_PATH" (string-append gcc "/lib"))
	  (invoke "tar" "-xf" source)
	  (chdir "MG5_aMC_v2_6_7")
	  (invoke "python" "bin/mg5_aMC")

	  (invoke "bash" "-c" (string-append "cp -r ./* " out))
	  (chdir out)

	  (substitute* (append (find-files "Template/LO/bin" "$")
	  		       (find-files "Template/MadWeight/bin" "$")
	  		       (find-files "Template/MadWeight" "py$")
	  		       (find-files "Template/NLO/bin" "$")
	  		       (find-files "Template/Common/bin" "$")
	  		       (find-files "Template/NLO/SubProcesses/" ".*sh$")
	  		       (find-files "Template/NLO/SubProcesses/ajob_template" "$")
	  		       (find-files "Template/NLO/MCatNLO/Scripts/" "$")
	  		       (find-files "Template/NLO/MCatNLO/" "sh$")
	  		       (find-files "Template/NLO/MCatNLO/" "inputs$")
			       (find-files "bin/" "$")
			       (find-files "MadSpin/" "$")
	  		       ;; (find-files "./" ".*py$")
	  		       (find-files "Template/LO/SubProcesses/" ".*sh$"))
	  	       (("/bin/bash") (string-append bash "/bin/bash"))
	  	       (("#!/usr/bin/perl") (string-append "#!" perl "/bin/perl"))
	  	       (("#! /usr/bin/env python")
	  		(string-append "#!" python "/bin/python"))
	  	       (("#!/usr/bin/env python")
	  		(string-append "#!" python "/bin/python"))
	  	       (("#!/usr/bin/python")
	  		(string-append "#!" python "/bin/python")))
	  
	  (for-each (lambda (prog)
		      (wrap-program prog
				    `("PATH" ":" = (,(getenv "PATH")))
				    `("LIBRARY_PATH" ":" prefix (,(string-append fortran "/lib")))
				    `("FC" ":" = (,(string-append fortran "/bin/gfortran")))
				    ))
		    '("bin/mg5" "bin/mg5_aMC"))
	  (substitute* "bin/.mg5-real" (("mg5_aMC") ".mg5_aMC-real"))

	  ;; lhapdf links to boost ?
	  (substitute* '("Template/LO/Source/make_opts"
	  		 "Template/NLO/Source/make_opts.inc")
	  	       (("ifneq \\(\\$\\(lhapdf\\),\\)\n")
	  		(string-append "ifneq ($(lhapdf),)\nCXXFLAGS += -I" boost "/include -I"
	  			       headers "/include -I"
	  			       libtirpc "/include/tirpc\n")))
	  
	  ; fix permissions after copying files
	  (substitute* '("madgraph/iolibs/export_v4.py" "madgraph/iolibs/export_fks.py")
	  	       (("self.dir_path)\n")
	  		"self.dir_path); os.system('chmod -R +w '+self.dir_path)\n"))
	  (substitute* '("madgraph/iolibs/export_v4.py" "madgraph/iolibs/export_fks.py")
	  	       (("dir_path, True)") "dir_path, True); os.system('chmod -R +w '+self.dir_path)"))
	  (substitute* '("madgraph/iolibs/export_v4.py" "madgraph/iolibs/export_fks.py")
	  	       (("internal/cluster.py')") "internal/cluster.py'); os.system('chmod -R +w '+self.dir_path)"))
	  (substitute* '("madgraph/iolibs/export_v4.py" "madgraph/iolibs/export_fks.py")
	  	       (("ignore=shutil.ignore_patterns\\(\\*IGNORE_PATTERNS\\)\\)") "ignore=shutil.ignore_patterns(*IGNORE_PATTERNS)); os.system('chmod -R +w '+self.dir_path)"))

	  (substitute* "input/mg5_configuration.txt"
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
	  	       )
	  
	  (substitute* "madgraph/iolibs/files.py"
	  	       (("def rm\\(path")
	  		"    os.system(\"chmod +w \"+path2)\ndef rm(path"))

	  (substitute* "vendor/StdHEP/src/stdhep/GNUmakefile"
	  	       (("INCS = ") (string-append "INCS = -I"
	  					   headers "/include -I"
	  					   libtirpc "/include/tirpc "))
	  	       (("CFLAGS \\+= ") (string-append "CFLAGS += -I"
	  						headers "/include -I"
	  						libtirpc "/include/tirpc "))
	  	       )
	  (substitute* "vendor/StdHEP/mcfio/src/GNUmakefile"
	  	       (("FINC = ") (string-append
	  			     "FINC = -I"
	  			     headers "/include -I"
	  			     libtirpc "/include/tirpc "))
	  	       (("CINC = ") (string-append
	  			     "CINC = -I"
	  			     headers "/include -I"
	  			     libtirpc "/include/tirpc "))
	  	       )

	  (substitute* "Template/NLO/SubProcesses/makefile_fks_dir"
	  	       (("-I\\.") (string-append "-I" headers "/include -I.")))

	  ;; mg5-pythia8 interface
	  (mkdir-p "HEPTools/MG5aMC_PY8_interface")
	  (invoke "tar" "-C" "HEPTools" "-xf" "vendor/OfflineHEPToolsInstaller.tar.gz")
	  (invoke "tar" "-C" "HEPTools/MG5aMC_PY8_interface" "--strip=1" "-xf" mg5amc-py8)

	  (substitute* "HEPTools/MG5aMC_PY8_interface/Makefile_mg5amc_py8_interface_static"
	  	       (("-I\\$\\(HEPMC2_INCLUDE\\)") (string-append "-I$(HEPMC2_INCLUDE) -I"
	  					   headers "/include -I"
	  					   libtirpc "/include/tirpc "
						   "-L" gcc "/lib -Wl,-rpath," gcc "/lib "))
	  	       )
	  (substitute* "HEPTools/MG5aMC_PY8_interface/MG5aMC_PY8_interface.cc"
	  	       (("\\\\\\%") "%")
	  	       ;; (("Pythia pythia;") (string-append "Pythia pythia(\"" pythia "/share/Pythia8/xmldoc\");"))
		       )

	  (invoke "bash" "-c"
	  	  (string-append
	  	   "cd HEPTools/MG5aMC_PY8_interface && python compile.py " pythia " && cd -"))

	  (substitute* "input/mg5_configuration.txt"
	  	       (("# mg5amc_py8_interface_path = ./HEPTools/MG5aMC_PY8_interface")
	  		(string-append "mg5amc_py8_interface_path = " out "/HEPTools/MG5aMC_PY8_interface")))
	  
	  ;; ma5
	  (invoke "tar" "-C" "HEPTools/" "-xf" ma5)

	  ; veto not installed packages
	  (substitute* "HEPTools/madanalysis5/madanalysis/input/installation_options.dat"
	  	       (("# webaccess_veto.*") "webaccess_veto = 1\n")
	  	       (("# matplotlib_veto.*") "matplotlib_veto = 1\n")
	  	       (("# delphesMA5tune_veto.*") "delphesMA5tune_veto = 1\n")
	  	       (("# latex_veto.*") "latex_veto = 1\n")
	  	       (("# pdflatex_veto.*") "pdflatex_veto = 1\n")
	  	       (("# scipy_veto.*") "scipy_veto = 1\n")
	  	       )

	  (substitute* "HEPTools/madanalysis5/madanalysis/build/makefile_writer.py"
	  	       (("cxxflags.extend\\(\\['-Wall'")
	  		(string-append
	  		 "cxxflags.extend(['-I" headers "/include', '-I" libtirpc "/include/tirpc', '-Wall'"))
	  	       (("libs.extend\\(\\['-lz'\\]\\)") (string-append "libs.extend(['-L" zlib "/lib', '-lz'])"))
	  	       )

	  (invoke "python" "HEPTools/HEPToolsInstallers/installMadAnalysis5.py"
		  (string-append "--ma5_path=" out "/HEPTools/madanalysis5/")
		  (string-append "--mg5_path=" out "/")
		  (string-append "--zlib=" zlib))
	  
	  (substitute* "input/mg5_configuration.txt"
	  	       (("# madanalysis5_path = ./HEPTools/madanalysis5/madanalysis5")
	  		(string-append "madanalysis5_path = " out "/HEPTools/madanalysis5")))

	  (substitute* (append (find-files "HEPTools/madanalysis5/bin" "$") )
	  	       (("/bin/bash") (string-append bash "/bin/bash"))
	  	       (("#!/usr/bin/perl") (string-append "#!" perl "/bin/perl"))
	  	       (("#! /usr/bin/env python")
	  		(string-append "#!" python "/bin/python"))
	  	       (("#!/usr/bin/env python")
	  		(string-append "#!" python "/bin/python"))
	  	       (("#!/usr/bin/python")
	  		(string-append "#!" python "/bin/python")))

	  ;; delphes
	  (substitute* "input/mg5_configuration.txt"
	  	       (("# delphes_path = ./Delphes")
	  		(string-append "delphes_path = " delphes)))

	  ;; compile nlo libraries, then clean up
	  (invoke "bash" "-c" "bin/mg5_aMC < <(echo \"generate p p > Z [QCD]\noutput abcd\n\n\")")
	  (invoke "rm" "-rf" "abcd")
	  (invoke "mv" "README" "VERSION" "UpdateNotes.txt" "proc_card.dat" "madgraph")
	  (invoke "rm" "-rf" "INSTALL" "LICENSE" "py.py" "py.pyc" "py.pyo" "additional_command")
	  (invoke "mv" "doc.tgz" "doc")

	  ; example of adding an additional model
	  (invoke "tar" "-C" "models" "-xf" zprime)

	  (wrap-program "HEPTools/madanalysis5/bin/ma5"
			`("PATH" ":" = (,(getenv "PATH")))
			`("LD_LIBRARY_PATH" ":" = (""))
			`("LIBRARY_PATH" ":" prefix (,(string-append fortran "/lib")))
			`("FC" ":" = (,(string-append fortran "/bin/gfortran"))))
	  (invoke "bash" "-c" "HEPTools/madanalysis5/bin/ma5 < <(echo \"\n\n\n\n\n\")")
	  (invoke "bash" "-c" "HEPTools/madanalysis5/bin/ma5 < <(echo \"\n\n\n\n\n\")")

	  ;; force it to be happy with the configuration
	  (substitute* "HEPTools/madanalysis5/madanalysis/core/main.py"
		       (("rebuild = forced or FirstUse or UpdateNeed or Missing") "rebuild = False"))
	  (substitute* "HEPTools/madanalysis5/madanalysis/system/checkup.py"
		       (("self.logger.error\\('impossible to remove the file") "# "))
	  
	  )))
    ; madanalysis5
    ;http://madanalysis.irmp.ucl.ac.be/raw-attachment/wiki/MA5SandBox/ma5_latest.tgz
    )
   (home-page "https://launchpad.net/mg5amcnlo")
   (synopsis "MadGraph5_aMC@NLO")
   (description "
MadGraph5_aMC@NLO is a framework that aims at providing all the elements necessary for SM and BSM phenomenology, such as the computations of cross sections, the generation of hard events and their matching with event generators, and the use of a variety of tools relevant to event manipulation and analysis. Processes can be simulated to LO accuracy for any user-defined Lagrangian, an the NLO accuracy in the case of models that support this kind of calculations -- prominent among these are QCD and EW corrections to SM processes. Matrix elements at the tree- and one-loop-level can also be obtained.")
   (license licenses:bsd-3)
   ))
madgraph

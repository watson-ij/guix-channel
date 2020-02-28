(define-module (physics lanhep)
  #:use-module (ice-9 regex)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages commencement))

(define-public lanhep
  (package
   (name "lanhep")
   (version "4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://theory.sinp.msu.ru/~semenov/lhep"
       (regexp-substitute/global #f "\\." version 'pre 'post)
       ".tgz"))
     (sha256
      (base32
       "0sy5nralkwdl1aw4nhd3z9fgqb9g3c9rly9icpkvy89fjsnwy7c8"))))
   (build-system gnu-build-system)
   (arguments `(#:phases
		(modify-phases
		 %standard-phases
		 (delete 'configure)
		 ;; (add-after 'unpack 'paths
		 ;; 	    (lambda* (#:key inputs outputs #:allow-other-keys)
		 ;; 	      (let* ((out (assoc-ref outputs "out"))
		 ;; 		     (share (string-append out "/share/lanhep")))
		 ;; 		;; hard-code some utility files provided with the distribution
		 ;; 		(substitute*
		 ;; 		 "lagr.c"
		 ;; 		 (("sprintf\\(cbuf,\"%s/ufo-static\",InputDirectory\\);")
		 ;; 		  (string-append "sprintf(cbuf,\"%s/ufo-static\",\"" share "/mdl\");"))
		 ;; 		 (("sprintf\\(cbuf,\"%s/ufo-static/%s\",InputDirectory")
		 ;; 		  (string-append "sprintf(cbuf,\"%s/ufo-static\",\"" share "/mdl\""))
		 ;; 		 ))))
				 
		 (replace 'install
			  (lambda* (#:key inputs outputs #:allow-other-keys)
			    (let* ((out (assoc-ref outputs "out"))
				   (bin (string-append out "/bin"))
				   (share (string-append out "/share/lanhep")))
			      (mkdir-p bin)
			      (invoke "cp" "lhep" bin)
			      (mkdir-p share)
			      (invoke "cp" "README" share)
			      (invoke "cp" "-r" "mdl" bin) ; lanhep uses the directory of the binary as a search
			      (invoke "cp" "-r" "mdl" "SLHAplus" "susy8" "susyLHA" "minsusy" "test" share)
			      (wrap-program
			       (string-append out "/bin/lhep")
			       `("PATH" ":" prefix (,share))))))
		 (delete 'check))))
   (synopsis " LanHEP software package for Feynman rules generation")
   (description "
 The LanHEP program for Feynman rules generation in momentum representation is presented. It reads the Lagrangian written in the compact form close to one used in publications. It means that Lagrangian terms can be written with summation over indices of broken symmetries and using special symbols for complicated expressions, such as covariant derivative and strength tensor for gauge fields. The output is Feynman rules in terms of physical fields and independent parameters. This output can be written in LaTeX format and in the form of CompHEP model files, which allows one to start calculations of processes in the new physical model. Although this job is rather straightforward and can be done manually, it requires careful calculations and in the modern theories with many particles and vertices can lead to errors and misprints. The program allows one to introduce into CompHEP new gauge theories as well as various anomalous terms.")
   (home-page "https://theory.sinp.msu.ru/~semenov/lanhep.html")
   (license
    (non-copyleft
     "https://theory.sinp.msu.ru/~semenov/licence"
     "Academic use license. Cite references on http://theory.npi.msu.su/~semenov/lanhep.html"))))

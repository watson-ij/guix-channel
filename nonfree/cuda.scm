;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2018, 2019 Inria

;; modifications 2020-03-05 for cuda-9.2 and cudnn-7.2.1 - ijw

(define-module (non-free cuda)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (ice-9 match))

(define (make-cuda version origin)
  (package
    (name "cuda-toolkit")
    (version version)
    (source origin)
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;196 MiB
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))

       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f

       #:strip-binaries? #f                       ;no need

       ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
       ;; not DT_RUNPATH.
       #:validate-runpath? #f

       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((source (assoc-ref inputs "source")))
                        (invoke "sh" source "--keep" "--noexec")
                        (chdir "pkg/run_files")
                        (match (find-files "." "^cuda-linux.*\\.run$")
                          ((run)
                           (invoke "sh" run "--keep" "--noexec")))
                        (chdir "pkg"))))
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out
                        (assoc-ref outputs "out"))
                      (define libc
                        (assoc-ref inputs "libc"))
                      (define gcc-lib
                        (assoc-ref inputs "gcc:lib"))
                      (define ld.so
                        (string-append libc ,(glibc-dynamic-linker)))
                      (define rpath
                        (string-join (list "$ORIGIN"
                                           (string-append out "/lib")
                                           (string-append out "/nvvm/lib64")
                                           (string-append libc "/lib")
                                           (string-append gcc-lib "/lib"))
                                     ":"))

                      (define (patch-elf file)
                        (make-file-writable file)
                        (unless (string-contains file ".so")
                          (format #t "Setting interpreter on '~a'...~%" file)
                          (invoke "patchelf" "--set-interpreter" ld.so
                                  file))
                        (format #t "Setting RPATH on '~a'...~%" file)
                        (invoke "patchelf" "--set-rpath" rpath
                                "--force-rpath" file))

                      (for-each (lambda (file)
                                  (when (elf-file? file)
                                    (patch-elf file)))
                                (find-files "."
                                            (lambda (file stat)
                                              (eq? 'regular
                                                   (stat:type stat)))))
                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out   (assoc-ref outputs "out"))
                             (lib   (string-append out "/lib"))
                             (lib64 (string-append out "/lib64")))
                        (mkdir-p out)
                        (setenv "PERL5LIB" (getcwd)) ;for InstallUtils.pm
                        (invoke "perl" "install-linux.pl"
                                (string-append "--prefix=" out))
                        (rename-file lib64 lib)
                        #t)))
                  (add-after 'install 'move-documentation
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out    (assoc-ref outputs "out"))
                             (doc    (assoc-ref outputs "doc"))
                             (docdir (string-append doc "/share/doc/cuda")))
                        (mkdir-p (dirname docdir))
                        (rename-file (string-append out "/doc") docdir)
                        #t))))))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("perl" ,perl)
       ("python" ,python-2)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (synopsis
     "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (license #f)
    (supported-systems '("x86_64-linux"))))

(define-syntax-rule (cuda-source url hash)
  (origin
    (uri url)
    (sha256 (base32 hash))
    (method url-fetch)))

(define-public cuda-9.2
  (make-cuda "9.2.148"
             (cuda-source
              "https://developer.nvidia.com/compute/cuda/9.2/Prod2/local_installers/cuda_9.2.148_396.37_linux.run"
              "04c6v9b50l4awsf9w9zj5vnxvmc0hk0ypcfjksbh4vnzrz14wigm")))

(define-public cudnn-7.2.1
  (package
   (name "cudnn")
   (version "7.2.1")
   (source (cuda-source "https://developer.download.nvidia.com/compute/redist/cudnn/v7.2.1/cudnn-9.2-linux-x64-v7.2.1.38.tgz"
			"1sf215wm6zgr17gs6sxfhw61b7a0qmcxiwhgy1b4nqdyxpqgay1y"))
   (build-system gnu-build-system)
   (arguments `(#:substitutable? #f
		#:strip-binaries? #f ;no need
		;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
		;; not DT_RUNPATH.
		#:validate-runpath? #f
		#:modules ((guix build utils)
			   (guix build gnu-build-system)
			   (ice-9 match))
		#:phases
		(modify-phases
		 %standard-phases
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out (assoc-ref outputs "out"))
                      (define cuda (assoc-ref inputs "cuda"))
                      (define libc (assoc-ref inputs "libc"))
                      (define gcc-lib (assoc-ref inputs "gcc:lib"))
                      (define ld.so (string-append libc ,(glibc-dynamic-linker)))
                      (define rpath
                        (string-join (list "$ORIGIN"
                                           (string-append out "/lib")
                                           (string-append cuda "/lib")
                                           (string-append cuda "/lib/stubs")
                                           (string-append cuda "/nvvm/lib64")
                                           (string-append libc "/lib")
                                           (string-append gcc-lib "/lib"))
                                     ":"))

                      (define (patch-elf file)
                        (make-file-writable file)
                        (unless (string-contains file ".so")
                          (format #t "Setting interpreter on '~a'...~%" file)
                          (invoke "patchelf" "--set-interpreter" ld.so
                                  file))
                        (format #t "Setting RPATH on '~a'...~%" file)
                        (invoke "patchelf" "--set-rpath" rpath
                                "--force-rpath" file))

                      (for-each (lambda (file)
                                  (when (elf-file? file)
                                    (patch-elf file)))
                                (find-files "."
                                            (lambda (file stat)
                                              (eq? 'regular
                                                   (stat:type stat)))))
                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out   (assoc-ref outputs "out"))
                             (lib   (string-append out "/lib"))
			     (out-include (string-append out "/include")))
                        (mkdir-p out)
                        (copy-recursively "lib64" lib)
                        (copy-recursively "include" out-include)
                        #t)))
		 (delete 'configure)
                 (delete 'check))))
   (native-inputs `(("patchelf" ,patchelf)))
   (propagated-inputs `(("cuda" ,cuda-9.2)))
   (inputs `(("gcc:lib" ,gcc "lib")))
   (synopsis
     "NVIDIA CUDA® Deep Neural Network library")
   (description
    "This package provides the NVIDIA CUDA® Deep Neural Network library (cuDNN), a GPU-accelerated library of primitives for deep neural networks, which is proprietary.")
    (home-page "https://developer.nvidia.com/cudnn")
    (license #f)
    (supported-systems '("x86_64-linux"))))

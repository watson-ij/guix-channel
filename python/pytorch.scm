(define-module (python pytorch)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (ice-9 match))

(define-public pytorch
  (package
    (name "pytorch")
    (version "v1.4.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/pytorch/pytorch.git")
            (commit version)
            (recursive? #t)))
      (sha256
       (base32
        ;; "1biyq2p48chakf2xw7hazzqmr5ps1nx475ql8vkmxjg5zaa071cz" ; v1.2.0
        "1gdglwrky4zq7w2zp9srrdqz8x2j89sv4n91x2m4c6b4fbj52gsr")))) ; v1.4.0
    (build-system python-build-system)
    (native-inputs
     `(("cmake" ,cmake)
       ("ninja" ,ninja)
       ("patchelf" ,patchelf)
       ("which" ,which)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (propagated-inputs
     `(("requests" ,python-requests)
       ("numpy" ,python-numpy)
       ("six" ,python-six)
       ("future" ,python-future)
       ("pyyaml" ,python-pyyaml)
       ("click" ,python-click)
       ("cffi" ,python-cffi)
       ("setuptools" ,python-setuptools)))
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (add-after 'unpack 'no-errors
		  (lambda _
		    (substitute*
		     "third_party/benchmark/CMakeLists.txt"
		     (("add_cxx_compiler_flag\\(-pedantic-errors\\)") "#")
		     (("add_cxx_compiler_flag\\(-Werror") "#"))
		    (for-each
		     (lambda (file) (invoke "chmod" "+w" file))
		     (append
		      (find-files "third_party/sleef/src/libm/")
		      (find-files "third_party/sleef/src/dft/")
		      (find-files "third_party/sleef/src/quad/")))))
       (add-after 'install 'rpath
		  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (define gcc-lib (assoc-ref inputs "gcc:lib"))
                    (define libc (assoc-ref inputs "libc"))
		    (define out (assoc-ref outputs "out"))
                    (define rpath
                      (string-join (list "$ORIGIN"
                                         (string-append out "/lib/python3.7/site-packages/torch/lib")
                                         (string-append libc "/lib")
                                         (string-append gcc-lib "/lib"))
                                     ":"))
		    (for-each
		     (lambda (file)
		       (format #t "Setting RPATH on '~a'...~%" file)
                       (invoke "patchelf" "--set-rpath" rpath "--force-rpath" file))
		     (append
		      (find-files (string-append out "/lib/python3.7/site-packages/torch/test/"))
		      (find-files (string-append out "/lib/python3.7/site-packages/torch/bin/"))))
		    #t))
       (delete 'check)
       (delete 'strip)
       (delete 'validate-runpath))))
   (home-page "https://pytorch.org/")
   (synopsis "Tensors and Dynamic neural networks in Python with strong GPU acceleration")
   (description "PyTorch is a Python package that provides two high-level features:
    Tensor computation (like NumPy) with strong GPU acceleration
    Deep neural networks built on a tape-based autograd system")
   (license bsd-3)))

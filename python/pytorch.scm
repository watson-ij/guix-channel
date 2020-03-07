(define-module (python pytorch)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (non-free cuda)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages check)
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

(define-public pytorch-cuda
  (package
   (inherit pytorch)
   (version "v1.4.0+cu92")
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("cuda" ,cuda-9.2)
       ("cudnn" ,cudnn-7.2.1)))))

(define pillow-simd
  (package
   (name "pillow-simd") (version "7.0.0")
   (source (origin (method url-fetch)
		   (uri (string-append "https://github.com/uploadcare/pillow-simd/archive/" version ".tar.gz"))
		   (sha256 (base32 "1jwycn5rpzh8pgdl3imb1q0xib5y8d667nzjq6djcxb2h7f8ndmq"))))
   (build-system python-build-system)
;;   (arguments `(#:phases (modify-phases %standard-phases (delete 'check))))
   (native-inputs `(("python-pytest" ,python-pytest)
		    ("python-pytest-runner" ,python-pytest-runner)))
   (inputs `(("zlib" ,zlib)
	     ("libwebp" ,libwebp)
	     ("libtiff" ,libtiff)
	     ("libjpeg" ,libjpeg)
	     ("libpng" ,libpng)
	     ("lcms" ,lcms)
	     ("openjpeg" ,openjpeg)
	     ("freetype" ,freetype)))
   (propagated-inputs `())
   (synopsis "Pillow-SIMD is \"following\" Pillow. Pillow-SIMD versions are 100% compatible drop-in replacements for Pillow of the same version.")
   (description "")
   (home-page "https://github.com/pytorch/vision")
   (license #f)))

(define torchvision
  (package
   (name "torchvision") (version "v0.5.0")
   (source (origin (method url-fetch)
		   (uri (string-append "https://github.com/pytorch/vision/archive/" version ".tar.gz"))
		   (sha256 (base32 "0kfplj3w4mdra30mck8b65r54bsjm5bl14g0bsbls5rxvy9zr6pb"))))
   (build-system python-build-system)
   (arguments `(#:phases (modify-phases %standard-phases (delete 'check))))
   (native-inputs `(("which" ,which)))
   (propagated-inputs `(("pillow" ,pillow-simd)
			("pytorch" ,pytorch)))
   (synopsis "Datasets, Transforms and Models specific to Computer Vision")
   (description "")
   (home-page "https://github.com/pytorch/vision")
   (license #f)))

(define torchaudio
  (package
   (name "torchaudio") (version "v0.4.0")
   (source (origin (method url-fetch)
		   (uri (string-append "https://github.com/pytorch/audio/archive/" version ".tar.gz"))
		   (sha256 (base32 "0rnw89bq0nra4flzczj7mxpwlfq54jq85sj8qc7qiaxi34ik2qck"))))
   (build-system python-build-system)
   (arguments `(#:phases (modify-phases %standard-phases (delete 'check))))
   (native-inputs `(("which" ,which)))
   (inputs `(("sox" ,sox)))
   (propagated-inputs `(("pytorch" ,pytorch)))
   (synopsis " Data manipulation and transformation for audio signal processing, powered by PyTorch")
   (description "")
   (home-page "https://github.com/pytorch/audio")
   (license #f)))

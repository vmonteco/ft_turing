(load ".quicklisp/setup")
(require :asdf)
(require :uiop)

;; Retrieve project's root directory:
(defparameter *ft_turing-base-dir*
  (uiop:pathname-directory-pathname *load-truename*))

;; Add link farm path to asdf:*central-registry*:
;; https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems.html
(push (merge-pathnames "link-farm/" *ft_turing-base-dir*) asdf:*central-registry*)

;; We load quicklisp, the libraries manager.
;; It is necessary to retrieve dependencies (the tests framework
;; "fiveAM" for this current project).
;; We need to explicitly do so because of the --script parameter
;; in the shebang that implies both --no-sysinit and --no-userinit.
;; At first we were using ASDF (and a `(require :asdf)`) but this isn't
;; useful in the end.
(load (merge-pathnames ".quicklisp/setup" *ft_turing-base-dir*))

;; Load the whole project
;; It's possible to use asdf:load-system to load a system, but asdf
;; can't install dependencies (which ql:quickload can).
(ql:quickload :ft_turing :silent t)

;; Settings:
;; Uncomment lines to modify the default value.
;; main:
;; (defparameter ft_turing:*show-code* nil)
;; (defparameter ft_turing:*machine-description-file* "description.txt")
;; (defparameter ft_turing:*generated-code-file* "lambda.lisp")
;; (defparameter ft_turing:*machine-output-file* "output.log")

;; Hardware:
;; (defparameter hardware:*hw-side-display-size* 10)

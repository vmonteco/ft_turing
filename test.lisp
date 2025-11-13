#!/usr/bin/env -S sbcl --script

;; This script is the entrypoint for testing.

;; ASDF is the builder. We require it here.
(require :asdf)

;; We load quicklisp, the libraries manager.
;; It is necessary to retrieve dependencies (the tests framework
;; "fiveAM" for this current project).
;; We need to explicitly do so because of the --script parameter
;; in the shebang that implies both --no-sysinit and --no-userinit.
(load "quicklisp/setup")

;; ft_turing.asd is what the tools will consume to inquire the project's
;; structure.
(load "ft_turing.asd")

;; It's possible to use asdf:load-system to load a system, but asdf
;; can't install dependencies (which ql:quickload can).
(ql:quickload :ft_turing/tests)

;; This is the actual tests entrypoint.
(asdf:test-system :ft_turing)

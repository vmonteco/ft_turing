#!/usr/bin/env -S sbcl --noinform --script

;;; This script is for running the project in interpreted mode.

;; We load quicklisp, the libraries manager.
;; It is necessary to retrieve dependencies (the tests framework
;; "fiveAM" for this current project).
;; We need to explicitly do so because of the --script parameter
;; in the shebang that implies both --no-sysinit and --no-userinit.
;; At first we were using ASDF (and a `(require :asdf)`) but this isn't
;; useful in the end.
(load "../.quicklisp/setup")

;; ft_turing system dependencies:
(load "utils/utils.asd")
(load "emulator/emulator.asd")

;; ft_turing.asd is what the tools will consume to inquire the project's
;; structure.
(load "ft_turing.asd")

;; It's possible to use asdf:load-system to load a system, but asdf
;; can't install dependencies (which ql:quickload can).
(ql:quickload :ft_turing)

;; We call the main function as entrypoint.
(ft_turing-pkg:main)

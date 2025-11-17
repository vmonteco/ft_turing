#!/usr/bin/env -S sbcl --noinform --script

;; This script is for running the project in interpreted mode.

;; For this script we don't need third party dependencies.
;; Therefore we don't need quicklisp and can use just ASDF.
(require :asdf)

;; ft_turing.asd is what the tools will consume to inquire the project's
;; structure.
(load "src/ft_turing.asd")

;; We load the system defined in the .asd file.
(asdf:load-system :ft_turing)

;; We call the main function as entrypoint.
(ft_turing-pkg:main)

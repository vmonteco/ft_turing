#!/usr/bin/env -S sbcl --script

;; This script is for building the project.

;; We don't need external dependencies for the main function.
;; Therefore we can use only ASDF.
(require :asdf)

;; ft_turing.asd is what the tools will consume to inquire the project's
;; structure.
(load "src/ft_turing.asd")

;; Once we have loaded the .asd file, this is as simple as that.
(asdf:make :ft_turing)

#!/usr/bin/env -S sbcl --script

;;; This script is for building the project.

;; We load quicklisp, the libraries manager.
;; It is necessary to retrieve dependencies.
;; We need to explicitly do so because of the --script parameter
;; in the shebang that implies both --no-sysinit and --no-userinit.
;; At first we were using ASDF (and a `(require :asdf)`) but this isn't
;; useful in the end.
(load "../.quicklisp/setup")

;; We don't need external dependencies for the main function.
;; Therefore we can use only ASDF.
(require :asdf)

;; Quickloading depencendies for build.
;; ASDF won't install 3rd party dependencies, this ensures they're available.
(ql:quickload "com.inuoe.jzon")

;; Load system that are dependencies:
(load "utils/utils.asd")
(load "emulator/emulator.asd")

;; ft_turing.asd is what the tools will consume to inquire the project's
;; structure.
(load "ft_turing.asd")

;; Once we have loaded the .asd file, this is as simple as that.
(asdf:make :ft_turing)

#!/usr/bin/env -S sbcl --script

;;; This script is for building the project.

(require :asdf)

;; Load project configuration:
(load "settings")

;; It's possible to use asdf:load-system to load a system, but asdf
;; can't install dependencies (which ql:quickload can).
(ql:quickload :ft_turing)

;; Once we have loaded the .asd file, this is as simple as that.
(asdf:make :ft_turing)

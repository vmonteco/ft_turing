#!/usr/bin/env -S sbcl --script

;;; This script is for building the project.

(require :asdf)

;; Load project configuration:
(load "settings")

;; Once we have loaded the .asd file, this is as simple as that.
(asdf:make :ft_turing)

#!/usr/bin/env -S sbcl --noinform --script

;;; This script is for running the project in interpreted mode.

(require :asdf)

;; Load project configuration:
(load "settings")

;; We call the main function as entrypoint.
(ft_turing:main)

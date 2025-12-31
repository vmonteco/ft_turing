#!/usr/bin/env -S sbcl --noinform --script

;;; This script is for running the project in interpreted mode.

(require :asdf)

;; Load project configuration:
(load "settings")

;; It's possible to use asdf:load-system to load a system, but asdf
;; can't install dependencies (which ql:quickload can).
(ql:quickload :ft_turing)

;; We call the main function as entrypoint.
(ft_turing:main)

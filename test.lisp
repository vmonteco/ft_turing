#!/usr/bin/env -S sbcl --script

;;; This script is the entrypoint for testing.

(require :asdf)

;; Load project configuration:
(load "settings")

;; It's possible to use asdf:load-system to load a system, but asdf
;; can't install dependencies (which ql:quickload can).
(ql:quickload :machine-description :verbose t)

;; This is the actual tests entrypoint.
(asdf:test-system :machine-description/tests :verbose t)

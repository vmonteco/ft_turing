#!/usr/bin/env -S sbcl --script

;;; This script is the entrypoint for testing.

(require :asdf)

;; Load project configuration:
(load "settings")

;; This is the actual tests entrypoint.
(asdf:test-system :ft_turing :verbose t)

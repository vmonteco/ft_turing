#!/usr/bin/env -S sbcl --script

;;; This script is the entrypoint for testing.

(require :asdf)

;; Load project configuration:
(load "settings")

(format *standard-output* "~&~%Loading and testing utils:~%")
(ql:quickload :utils/tests :silent t)
(asdf:test-system :utils :verbose t)

(format *standard-output* "~&~%Loading and testing hardware:~%")
(ql:quickload :hardware/tests :silent t)
(asdf:test-system :hardware :verbose t)

(format *standard-output* "~&~%Loading and testing machine-description:~%")
(ql:quickload :machine-description/tests :silent t)
(asdf:test-system :machine-description :verbose t)

(format *standard-output* "~&~%Loading and testing machine-maker:~%")
(ql:quickload :machine-maker/tests :silent t)
(asdf:test-system :machine-maker :verbose t)

(format *standard-output* "~&~%Loading and testing bonus:~%")
(ql:quickload :bonus/tests :silent t)
(asdf:test-system :bonus :verbose t)

(format *standard-output* "~&~%Loading and testing ft_turing:~%")
(ql:quickload :ft_turing/tests :silent t)
(asdf:test-system :ft_turing :verbose t)

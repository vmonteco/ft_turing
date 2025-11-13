#!/usr/bin/env -S sbcl --script

(require :asdf)
(load "ft_turing.asd")
(asdf:make :ft_turing)

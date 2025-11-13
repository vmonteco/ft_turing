#!/usr/bin/env -S sbcl --script

(require :asdf)
(load "ft_turing.asd")
(asdf:load-system :ft_turing)
(ft_turing:main)

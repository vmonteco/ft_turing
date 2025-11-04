# ft_turing:

This is project from 42 on Turing machines implementation.

The assignement consists in making an executable using the functional paradigm that would take a Turing machine description, implement it, then run it.

For this reason we decided to use a Lisp language because the list handling affinities for representations, but even more to take advantage of the Lisp macros features.

We choose Common Lisp with the CLisp interpreter.

## CLisp:
### Installation:
CLisp can be installed through Homebrew (on 42 machines, Homebrew can be installed without permissions as instructed [here](https://docs.brew.sh/Installation#alternative-installs).

Then adding the homebrew's `bin` directory to the `PATH`, installing `clisp` and using `#!/usr/bin/env clisp` as a shebang does the trick.

Alternatives could include [running CLisp code with Emacs](https://www.gnu.org/software/emacs/manual/html_node/emacs/Executing-Lisp.html), building from [sources](https://ftp.gnu.org/gnu/clisp/release/2.49/clisp-2.49.tar.bz2) and asking 42's staff to add the interpreter to the dumps.

### Resources:
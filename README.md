# ft_turing

This is the 42's Turing machine emulator project.

## Introduction

We chose to make this project with Common Lisp, to take full advantage of its capabilities for meta-programming.

This project requires SBCL, which can be installed with brew.

## Usage
### Running in a Docker container:
In the repository, run `docker build -t ft_turing .` to build the image, then simply run `docker run -ti ft_turing`.

### Building:

```
$ make
```

### Testing

```
$ make test
```

### Running

The executables take two parameters:

- The first one is the path to a Turing machine description under JSON format.

- The second is a string depictin the initial state of the machine.

```
$ ./ft_turing path/to/machine_description.json initial_state
```

There's also a script to run the project in interpreted mode:

```
$ ./ft_turing.lisp path/to/machine_description.json initial_state
```

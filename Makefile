re:
	$(MAKE) clean
	$(MAKE) all
# Makefile for ft_turing

.PHONY: all install build run clean

# Variables
SBCL_VERSION = 2.4.0

# Detect OS and architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Darwin)
    OS = darwin
    ifeq ($(UNAME_M),arm64)
        ARCH = arm64
    else
        ARCH = x86-64
    endif
else ifeq ($(UNAME_S),Linux)
    OS = linux
    ARCH = x86-64
else
	$(error Unsupported operating system: $(UNAME_S))
endif

SBCL_URL = http://prdownloads.sourceforge.net/sbcl/sbcl-$(SBCL_VERSION)-$(ARCH)-$(OS)-binary.tar.bz2?download
SBCL_DIR = sbcl-$(SBCL_VERSION)-$(ARCH)
QUICKLISP_URL = https://beta.quicklisp.org/quicklisp.lisp
EXECUTABLE = ft_turing


all: install build

install: bin/sbcl deps/quicklisp/setup.lisp
	@echo "Installation completed."

bin/sbcl:
	@echo "Downloading and installing SBCL..."
	@mkdir -p bin
	@curl -L $(SBCL_URL) -o sbcl.tar.bz2
	@tar -xjf sbcl.tar.bz2
	@mv $(SBCL_DIR)-* sbcl-temp
	@cp sbcl-temp/run-sbcl.sh bin/sbcl
	@cp -r sbcl-temp/* bin/
	@rm -rf sbcl-temp sbcl.tar.bz2
	@chmod +x bin/sbcl
	@echo "SBCL installed."

deps/quicklisp/setup.lisp:
	@echo "Installing Quicklisp..."
	@mkdir -p deps
	@cd deps && curl -O $(QUICKLISP_URL)
	@echo | ./bin/sbcl --load deps/quicklisp.lisp \
					  --eval "(quicklisp-quickstart:install :path \"$(PWD)/deps/quicklisp\")" \
					  --quit


build: $(EXECUTABLE)

$(EXECUTABLE): src/main.lisp ft-turing.asd
	@echo "Compiling the program..."
	@./bin/sbcl --eval "(push (truename \".\") asdf:*central-registry*)" \
				--eval "(ql:quickload :ft-turing)" \
				--eval "(sb-ext:save-lisp-and-die \"$(EXECUTABLE)\" :toplevel #'ft-turing:main :executable t)" \
				--quit

run: $(EXECUTABLE)
	@echo "Running the program..."
	@./$(EXECUTABLE)

clean:
	@echo "Cleaning..."
	@rm -rf bin deps $(EXECUTABLE) sbcl.tar.bz2

help:
	@echo "Available commands:"
	@echo "  make install  - Install dependencies (SBCL + Quicklisp)"
	@echo "  make build    - Build the executable program"
	@echo "  make run      - Run the program"
	@echo "  make clean    - Clean temporary files"
	@echo "  make all      - Install and build"
	@echo "  make re       - Clean and rebuild"
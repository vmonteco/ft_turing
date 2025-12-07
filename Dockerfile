FROM debian

RUN apt-get update -y && apt-get install -y sbcl emacs tmux cmake
ADD . /app
WORKDIR /app

ENTRYPOINT bash


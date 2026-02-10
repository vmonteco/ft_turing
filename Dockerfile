FROM debian

RUN apt-get update -y && apt-get install -y sbcl emacs tmux cmake rlwrap bash
RUN echo "alias sbcl=\"rlwrap sbcl\"" >> ~/.bashrc
ADD . /app
WORKDIR /app

ENTRYPOINT ["bash"]

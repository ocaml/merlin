FROM ocaml/opam:ubuntu-20.04-ocaml-5.3

WORKDIR /app

RUN sudo apt-get update
RUN sudo apt-get -y install jq
COPY . .
RUN sudo chown -R opam /app
RUN eval $(opam env)

# install merlin
RUN opam install .

# install merl-an
## cache workaround: https://github.com/ocurrent/current-bench/issues/468#issuecomment-1621030354
ADD https://api.github.com/repos/xvw/merl-an/git/refs/heads/merl-an-530 .merl-an-info
RUN opam pin -y merl-an https://github.com/xvw/merl-an.git#344a079bf502f27e9269498db9ed5369753f2ddb

RUN eval $(opam env)

# create directory for projects to run benchmarks on
RUN sudo mkdir /projects
RUN sudo chown opam /projects
WORKDIR /projects

# build irmin
RUN git clone https://github.com/mirage/irmin.git
WORKDIR /projects/irmin
RUN sudo rm -rf examples/ test/
RUN git checkout 421c09fa57784f155a3d6ad23e0ecc2c9cd0a352
RUN sudo apt install -y gnuplot-x11 libgmp-dev pkg-config libffi-dev
RUN opam switch import /app/bench/irmin.opam.export --no-checksums
RUN opam exec -- dune build

WORKDIR /app

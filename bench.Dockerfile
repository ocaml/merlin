FROM ocaml/opam:ubuntu-20.04-ocaml-4.14

WORKDIR /app

RUN sudo apt-get update
RUN sudo apt-get -y install jq
COPY . .
RUN sudo chown -R opam /app
RUN eval $(opam env)

# install merlin
RUN opam install .

# install merl-an
RUN echo "uncache this thing"
RUN opam pin -y merl-an https://github.com/pitag-ha/merl-an.git

RUN eval $(opam env)

# create directory for projects to run benchmarks on
RUN sudo mkdir /projects
RUN sudo chown opam /projects
WORKDIR /projects

# build irmin
RUN git clone https://github.com/mirage/irmin.git
WORKDIR /projects/irmin
RUN git checkout 8da4d16e7cc8beddfc8a824feca325426bae08a9
RUN sudo apt install -y gnuplot-x11 libgmp-dev pkg-config libffi-dev
RUN opam switch import /app/bench/irmin.opam.export --no-checksums
RUN opam exec -- dune build

WORKDIR /app

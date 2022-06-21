# The purpose of this file is to set up the environment necessary to launch benchmarks on current-bench.
# The goal here is to launch ./tests/bench.sh which launches benchmarks on irmin files

FROM ocaml/opam:debian-10-ocaml-4.14
RUN sudo apt-get install -y gnuplot-x11 libgmp-dev pkg-config libffi-dev jq
RUN sudo rm /usr/bin/opam && sudo ln -s /usr/bin/opam-2.1 /usr/bin/opam
RUN git clone https://github.com/mirage/irmin.git \
    && cd irmin \
    && git checkout d23b43e238c7fd417d2bbcab8c9766386c1b0a09 \
    && opam install --deps-only --with-test . \
    && opam exec -- dune build @check
RUN mkdir merlin
WORKDIR merlin
COPY --chown=opam:opam ./*.opam ./
RUN opam pin add -ny --with-version=dev . \
    && opam install --deps-only .
COPY --chown=opam:opam . ./
RUN opam install .

FROM ocaml/opam:debian-11-ocaml-5.0

WORKDIR /app

RUN sudo apt-get update
RUN sudo apt-get -y install jq
COPY . .
RUN sudo chown -R opam /app
RUN eval $(opam env)

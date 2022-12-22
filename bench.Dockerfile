FROM ocaml/opam:ubuntu-20.04-ocaml-4.14

WORKDIR /app

RUN sudo apt-get update
RUN sudo apt-get -y install jq
COPY . .
RUN sudo chown -R opam /app
RUN eval $(opam env)

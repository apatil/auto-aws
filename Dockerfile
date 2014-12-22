FROM avsm/docker-opam:ubuntu-trusty-4.02.1

RUN opam install Yojson
RUN opam install utop
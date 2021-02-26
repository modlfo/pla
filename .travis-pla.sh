sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install dune ppxlib compiler-libs
dune build
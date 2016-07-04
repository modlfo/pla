sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install cppo ocamlfind ocamlbuild
./configure
make
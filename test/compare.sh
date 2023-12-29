#!/bin/bash

dune exec -- ../src/porc.exe -l rust  -i test.atd -o rust/src/test.rs
# dune exec -- ../src/porc.exe -l ocaml -i test.atd -o ocaml/test.ml

rm -f rust.out ocaml.out

pushd rust
cargo run main > ../rust.out
popd

pushd ocaml/
dune exec ./main.exe > ../ocaml.out
popd

diff rust.out ocaml.out
if [ $? -eq 0 ]
then
    echo "pass"
else
    echo "fail"
fi

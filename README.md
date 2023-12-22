`porc` is a wire protocol code generator which supports OCaml and Rust
(*P*rotocol for *O*Caml and *R*ust *C*odegen). The code it generates
relies on `bin_prot` (de)serialization protocol, which supports both
[OCaml](https://github.com/janestreet/bin_prot) and
[Rust](https://github.com/LaurentMazare/binprot-rs).

`porc` relies on [jane_rope](https://github.com/janestreet/jane_rope),
which can be installed using:
```
opam pin add git@github.com:janestreet/jane_rope.git
```

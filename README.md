This is a work-in-progress (WIP), and is not suitable for production
use.

`porc` is a wire protocol code generator which supports OCaml and Rust
(*P*rotocol for *O*Caml and *R*ust *C*odegen). The code it generates
relies on `bin_prot` (de)serialization protocol, which supports both
[OCaml](https://github.com/janestreet/bin_prot) and
[Rust](https://github.com/LaurentMazare/binprot-rs). The definition
language is based on [ATD](https://github.com/ahrefs/atd). `porc` is
named, in part, as an homage to [ATD](https://github.com/ahrefs/atd)'s
[creator](https://github.com/mjambon).

`porc` relies on [jane_rope](https://github.com/janestreet/jane_rope),
which can be installed using:
```
opam pin add git@github.com:janestreet/jane_rope.git
```

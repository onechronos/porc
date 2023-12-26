(lang dune 3.7)

(generate_opam_files true)

(name porc)

(source
 (github onechronos/porc))

(license MIT)

(authors "Mika Illouz")

(maintainers "mika@illouz.net")

(package
 (name porc)
 (synopsis "binary protocol compiler for OCaml and Rust based on ATD and bin_prot")
 (description "protocol compiler")
 (depends
  (odoc :with-doc)
  (ocaml (>= "5.0.0"))
  dune
  cmdliner
  atd
  jane_rope
  camelsnakekebab
 )
)

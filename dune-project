(lang dune 3.9)

(name clam)
(using menhir 2.1)

(generate_opam_files true)

(source
 (github MaximeMulder/clam))

(authors "Maxime Mulder")

(maintainers "Maxime Mulder")

(license MIT)

(documentation https://github.com/MaximeMulder/clam/README.md)

(package
 (name clam)
 (synopsis "Small functionnal programming language with many type features")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

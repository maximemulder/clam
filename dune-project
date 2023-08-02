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
 (synopsis "Minimalist purely functionnal language with a C-family syntax")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

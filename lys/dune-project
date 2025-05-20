(lang dune 3.13)

(using menhir 3.0)

(name lys)

(generate_opam_files)

(source
 (github lyslang/lys))

(maintainers
 "Aljebriq <143266740+aljebriq@users.noreply.github.com>"
 "Łukasz Bartkiewicz <lukasku@proton.me>")

(authors
 "Aljebriq <143266740+aljebriq@users.noreply.github.com>"
 "Łukasz Bartkiewicz <lukasku@proton.me>")

(license GPL-3.0-only)

(package
 (name lys)
 (synopsis "The Lys purely functional programming language")
 (description
  "Lys is a purely functional language with an elegant and practical syntax, with a robust Hindley-Milner type system for strong static type inference and seamless currying support.")
 (depends
  (ocaml
   (>= 5))
  sedlex
  menhir
  menhirLib
  ppx_deriving
  uuseg
  yojson
  ppx_inline_test)
 (tags
  (ml fp pl)))

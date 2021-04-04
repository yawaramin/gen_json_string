(lang dune 2.7)

(name gen_json_string)
(version 0.1.0)
(generate_opam_files true)
(license GPL-3.0-or-later)
(authors "Yawar Amin <yawar.amin@gmail.com>")
(maintainers "Yawar Amin <yawar.amin@gmail.com>")

(package
  (name gen_json_string)
  (synopsis "Generate a random JSON value from a JSON Schema")
  (description "Generate a random JSON value from a JSON Schema")
  (depends
    (dune (and (>= 2.7) :with-build))
    (ocaml (>= 4.08.0))
    (ppx_deriving_yojson (and (>= 3.6.1) (< 4.0.0)))
    (qcheck-core (and (>= 0.17) (< 1.0.0)))
    (regenerate (and (>= 0.2) (< 1.0.0)))
    (yojson (and (>= 1.7.0) (< 2.0.0)))))


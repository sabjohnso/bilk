(lang dune 3.6)

(name wile)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name wile)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  cmdliner
  yojson
  zarith
  (odoc :with-doc)
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

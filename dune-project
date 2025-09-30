(lang dune 3.20)

(name logf)

(generate_opam_files true)

;(source (github username/reponame))

(authors "Guillaume Bury <guillaume.bury@gmail.com>")

(maintainers "Guillaume Bury <guillaume.bury@gmail.com>")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name logf)
 (synopsis "Structured Logs")
 (description "TODO")
 (depends
   ocaml
   fmt
   logs
   cmdliner
   uutf
 )
 (tags ("logging"))
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

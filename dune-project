(lang dune 3.16)

(name ocaml-lwt-chat)

(generate_opam_files true)

(source
 (github kaylynhl/ocaml-lwt-chat))

(authors "Kaylyn H")

(maintainers "Kaylyn H")

(license "All rights reserved")

(package
 (name ocaml-lwt-chat)
 (synopsis "Terminal chat application in OCaml using Lwt")
 (description "A small TCP chat server/client built with OCaml, Lwt, and Dune.")
 (depends ocaml dune lwt lwt_ppx ounit2)
 (tags
  (topics "ocaml" "lwt" "networking" "chat")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

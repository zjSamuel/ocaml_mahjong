(lang dune 3.20)
  (generate_opam_files true)
  (authors "Zeli Ma" "Jin Zhou" "Jiewen Luo")
  (maintainers "hxxlyhjh@gmail.com""jzhou171@jh.edu")
  (package
   (name mahjong) ; put in your own project name here
   (allow_empty)
   (synopsis "An OCaml japanese mahjong engine and AI assistant in Ocaml")
   (description "A Riichi Mahjong game engine with A*-based shanten/efficiency analysis and multi-level bot AI.")
   (depends
    (ounit2 (>= 2.2.7)) ; for each opam dependency list the version
    (core (>= 0.17.1))
    (dream (>= 1.0.0)) ; `opam list core` will display which version of core you have
 ))
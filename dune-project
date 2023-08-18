(lang dune 3.9)

(name stronglytyped)

(version 0.81)

(generate_opam_files true)

(source
 (github rdavison/stronglytyped))

(authors "Richard Davison")

(maintainers "richardneildavison@gmail.com")

(license MIT)

(package
 (name stronglytyped)
 (synopsis "Keyboard layout analyzer and generator")
 (description "Stronglytyped analyzes and generates keyboard layouts")
 (depends
  (ocaml
   (>= 5.0.0))
  dune
  incremental
  incr_map
  textutils_kernel
  core))

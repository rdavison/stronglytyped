(lang dune 3.9)

(using dune_site 0.1)

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
 (sites
  (share corpus))
 (synopsis "Keyboard layout analyzer and generator")
 (description "Stronglytyped analyzes and generates keyboard layouts")
 (depends
  (ocaml
   (>= 5.0.0))
  dune
  dune-site
  incremental
  incr_map
  textutils_kernel
  core))

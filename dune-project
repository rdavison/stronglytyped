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
 (synopsis "Keyboard layout analysis")
 (sites
  (share corpus))
 (depends
  (ocaml
   (>= 5.0.0))
  domainslib
  dune
  dune-site
  incremental
  incr_map
  textutils_kernel
  core
  core_unix))

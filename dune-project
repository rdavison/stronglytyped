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
   (= 5.1.1))
  dune
  dune-site
  (base
   (and
    (>= v0.16.0)
    (< v0.17.0)))
  (core
   (and
    (>= v0.16.0)
    (< v0.17.0)))
  (bonsai
   (and
    (>= v0.16.0)
    (< v0.17.0)))
  (ppx_css
   (and
    (>= v0.16.0)
    (< v0.17.0)))
  (ppx_jane
   (and
    (>= v0.16.0)
    (< v0.17.0)))))

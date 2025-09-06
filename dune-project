(lang dune 3.20)

(name project)

(using ctypes 0.3)

(using dune_site 0.1)

(version 0.01)

(generate_opam_files true)

(source
 (github rdavison/stronglytyped))

(authors "Richard Davison")

(maintainers "richardneildavison@gmail.com")

(license MIT)

(package
 (name project)
 (synopsis "Keyboard layout analysis")
 (sites
  (share corpus))
 (depends
  (ocaml
   (= 5.2.1))
  (dune
   (> 3.20.0))
  bonsai
  bonsai_web_components
  virtual_dom
  core
  core_unix
  async
  async_extra
  async_kernel
  async_rpc_kernel
  async_rpc_websocket
  ppx_css))

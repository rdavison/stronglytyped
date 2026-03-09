FROM ocamlformat AS ocamlformat
FROM lazygit AS lazygit
FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

RUN sudo apt update && sudo apt install -y m4 pkg-config libgmp-dev libffi-dev libssl-dev git ripgrep zlib1g-dev wget inotify-tools
RUN opam update && opam install ocaml-lsp-server
RUN opam repo add janestreet-bleeding https://github.com/janestreet/opam-repository.git
RUN opam repo add dune-universe git+https://github.com/dune-universe/opam-overlays.git

RUN sudo apt update && \
  sudo apt install -y git build-essential cmake ninja-build pkg-config libtool autoconf automake gettext curl && \
  git clone --single-branch -b stable https://github.com/neovim/neovim.git && \
  cd neovim && \
  make CMAKE_BUILD_TYPE=RelWithDebInfo && \
  sudo make install

RUN mkdir -p /home/opam/app/_build && chown opam:opam /home/opam/app/_build
RUN mkdir -p /home/opam/.config && chown opam:opam /home/opam/.config

ENV SHELL=/bin/bash
COPY --from=ocamlformat /out/ocamlformat /usr/local/bin/ocamlformat
COPY --from=ocamlformat /out/ocamlformat-rpc /usr/local/bin/ocamlformat-rpc
COPY --from=lazygit /usr/local/bin/lazygit /usr/local/bin/lazygit
COPY --chmod=0755 entrypoint.sh /home/opam/entrypoint.sh
ENTRYPOINT ["/home/opam/entrypoint.sh"]

ARG TAG
COPY ${TAG}.opam /home/opam/app/${TAG}.opam
COPY ${TAG}.opam.locked /home/opam/app/${TAG}.opam.locked
RUN opam-2.4 install /home/opam/app --deps-only --locked && rm /home/opam/app/${TAG}.opam /home/opam/app/${TAG}.opam.locked

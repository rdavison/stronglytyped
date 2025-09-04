# ---------- Stage 1: build ocamlformat with opam ----------
FROM ocaml/opam:ubuntu-22.04-ocaml-5.2 AS builder

ARG OCAML_VERSION=5.2.1
ARG OCFMT_VERSION=0.27.0

# Optional: speed/certainty — make sure we have basic build tools.
RUN sudo apt update && sudo apt install -y --no-install-recommends \
      build-essential m4 pkg-config git ca-certificates \
  && sudo rm -rf /var/lib/apt/lists/*

# Create a fresh, named switch so the path is deterministic.
# (Using a named switch avoids COPY wildcards later.)
RUN opam update && \
    opam install -y ocamlformat.${OCFMT_VERSION}

RUN sudo install -D "$(opam var bin)/ocamlformat" "/out/ocamlformat"

# ---------- Stage 2: minimal runtime with just the formatter ----------
FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

RUN sudo apt update && sudo apt install -y m4 pkg-config libgmp-dev libffi-dev libssl-dev git ripgrep zlib1g-dev
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

ENV SHELL=/bin/bash
COPY --from=builder /out/ocamlformat /usr/local/bin/ocamlformat
COPY --chmod=0755 entrypoint.sh /home/opam/entrypoint.sh
ENTRYPOINT ["/home/opam/entrypoint.sh"]

ARG TAG
COPY ${TAG}.opam /home/opam/app/${TAG}.opam
COPY ${TAG}.opam.locked /home/opam/app/${TAG}.opam.locked
RUN opam-2.4 install /home/opam/app --deps-only --locked && rm /home/opam/app/${TAG}.opam /home/opam/app/${TAG}.opam.locked


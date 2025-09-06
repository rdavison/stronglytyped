FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

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
RUN sudo install -D "$(opam var bin)/ocamlformat-rpc" "/out/ocamlformat-rpc"

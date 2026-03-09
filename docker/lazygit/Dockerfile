FROM ocaml/opam:ubuntu-22.04-ocaml-5.2
USER root
RUN \
  VERSION=0.54.2 && \
  ARCH=arm64 && \
  PLATFORM=linux && \
  FILENAME=lazygit_${VERSION}_${PLATFORM}_${ARCH}.tar.gz && \
  apt update && \
  apt install -y wget && \
  cd /usr/local/bin && \
  mkdir tmp-lazygit && \
  cd tmp-lazygit && \
  wget https://github.com/jesseduffield/lazygit/releases/download/v${VERSION}/${FILENAME} && \
  tar xzf ${FILENAME} && \
  rm ${FILENAME} && \
  cd .. && \
  mv tmp-lazygit/lazygit . && \
  rm -rf tmp-lazygit && \
  lazygit --version
USER opam

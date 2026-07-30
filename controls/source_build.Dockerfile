FROM rust@sha256:11fbefae37d29d4b00e0b566b044cc3a6b872505aef297eb9b155fdacf677a60 AS rust
FROM ocaml/opam@sha256:c4cdef7d942b5bb747668b9757c1599842ffda0c184ff5381b1f0ebaa9ced009 AS build
USER root
COPY --from=rust /usr/local/cargo /usr/local/cargo
COPY --from=rust /usr/local/rustup /usr/local/rustup
RUN printf '%s\n' \
  'deb [snapshot=20260711T000000Z] http://archive.ubuntu.com/ubuntu jammy main universe restricted multiverse' \
  'deb [snapshot=20260711T000000Z] http://archive.ubuntu.com/ubuntu jammy-updates main universe restricted multiverse' \
  'deb [snapshot=20260711T000000Z] http://archive.ubuntu.com/ubuntu jammy-backports main universe restricted multiverse' \
  'deb [snapshot=20260711T000000Z] http://security.ubuntu.com/ubuntu jammy-security main universe restricted multiverse' \
  > /etc/apt/sources.list
RUN rm -rf /etc/apt/sources.list.d/* && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    git \
    libev-dev \
    libgmp-dev \
    liblmdb-dev \
    libsqlite3-dev \
    m4 \
    pkg-config && \
  rm -rf /var/lib/apt/lists/*
ENV ARFLAGS=crD
ENV BUILD_PATH_PREFIX_MAP=/workspace=.
ENV CARGO_HOME=/usr/local/cargo
ENV CFLAGS="-ffile-prefix-map=/workspace=. -fdebug-prefix-map=/workspace=."
ENV CXXFLAGS="-ffile-prefix-map=/workspace=. -fdebug-prefix-map=/workspace=."
ENV DUNE_CACHE=disabled
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV PATH=/usr/local/cargo/bin:/home/opam/.opam/4.14/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENV RUSTFLAGS=--remap-path-prefix=/workspace=.
ENV RUSTUP_HOME=/usr/local/rustup
ENV SOURCE_DATE_EPOCH=1785373979
ENV TZ=UTC
ENV ZERO_AR_DATE=1
USER opam
WORKDIR /workspace
COPY --chown=opam:opam . .
RUN opam install --locked --deps-only --require-checksums -y .
RUN make -C mcl MCL_FP_BIT=256 MCL_FR_BIT=256 lib/libmcl.a
RUN opam exec -- dune build \
  --root /workspace \
  --profile release \
  bin/octra_node.exe \
  bin/octra_pvac_worker.exe \
  bin/octra_state_sync_client.exe \
  bin/octra_state_sync_manifest.exe \
  bin/bft_control_tx.exe
RUN mkdir /out && \
  cp _build/default/bin/octra_node.exe /out/octra_node.exe && \
  cp _build/default/bin/octra_pvac_worker.exe /out/octra_pvac_worker.exe && \
  cp _build/default/bin/octra_state_sync_client.exe /out/octra_state_sync_client.exe && \
  cp _build/default/bin/octra_state_sync_manifest.exe /out/octra_state_sync_manifest.exe && \
  cp _build/default/bin/bft_control_tx.exe /out/bft_control_tx.exe
FROM scratch
COPY --from=build /out/ /
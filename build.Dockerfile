FROM debian:bookworm-slim AS build

ARG GHC_VERSION=9.10.1
ARG CABAL_VERSION=3.12.1.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    gcc \
    libffi-dev \
    libffi8 \
    libgmp-dev \
    g++ \
    libncurses-dev \
    ca-certificates 

RUN useradd -m builder

RUN mkdir /target && chown builder /target

USER builder

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN bash -c ". $HOME/.ghcup/env && ghcup install ghc ${GHC_VERSION} && ghcup set ghc ${GHC_VERSION}"
RUN bash -c ". $HOME/.ghcup/env && ghcup install cabal ${CABAL_VERSION} && ghcup set cabal ${CABAL_VERSION}"

COPY --chown=builder . /src

WORKDIR /src

RUN bash -c ". $HOME/.ghcup/env && cabal build"

RUN outfile=$(find dist-newstyle -name "fedi-emoji-tool" -type f -executable) && \
    cp $outfile /target/fedi-emoji-tool && \
    strip /target/fedi-emoji-tool

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libffi8 \
    libgmp10 \
    ca-certificates \
    locales && \
    rm -rf /var/lib/apt/lists/* && \
    sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

COPY --from=build --chown=root:root /target/fedi-emoji-tool /usr/local/bin/fedi-emoji-tool

RUN chmod +x /usr/local/bin/fedi-emoji-tool && useradd -m user

ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
USER user

ENTRYPOINT ["/usr/local/bin/fedi-emoji-tool"]
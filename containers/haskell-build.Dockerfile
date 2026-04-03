ARG GHC_VERSION=latest
FROM haskell:${GHC_VERSION}

# Buster is EOL, update sources to use archive
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
  sed -i 's|security.debian.org|archive.debian.org|g' /etc/apt/sources.list && \
  sed -i '/buster-updates/d' /etc/apt/sources.list && \
  apt-get update && \
  apt-get install -y libpcre3-dev && \
  rm -rf /var/lib/apt/lists/*

RUN cabal update

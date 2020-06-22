ARG GHC_VERSION=latest
FROM haskell:${GHC_VERSION}

RUN apt-get update && \
  apt-get install -y libpcre3-dev && \
  rm -rf /var/lib/apt/lists/*

RUN cabal new-update

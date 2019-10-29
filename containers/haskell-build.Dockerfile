FROM haskell:8.6
RUN cabal v2-update
RUN cabal v2-install happy HTF

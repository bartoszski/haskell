# Install additional Haskell packages using stack
RUN cabal update \
    && cabal install --lib aeson \
    && cabal install --lib sqlite-simple\
    && cabal install --lib split\
    && cabal install --lib prettyprinter \
    && cabal install --lib vector \
    && cabal install --lib pretty-simple \
    && cabal install --lib pptable \
    && cabal install --lib pretty

RUN install pandas
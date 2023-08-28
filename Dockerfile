# Use the base ihaskell image
FROM gibiansky/ihaskell:latest



# Install additional Haskell packages using stack
RUN cabal update \
    && stack aeson tensorflow

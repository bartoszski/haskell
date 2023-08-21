# Use the base image gibiansky/ihaskell
FROM gibiansky/ihaskell

# Update cabal as root user
USER root
# Install ca-certificates as root user
RUN apt-get update && apt-get install -y ca-certificates

USER jovyan

RUN cabal update
RUN cabal install postgresql-simple




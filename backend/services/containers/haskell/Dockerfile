# Ancient Compute - Haskell Language Container
FROM debian:bookworm-slim

# Install GHC (Glasgow Haskell Compiler)
RUN apt-get update && apt-get install -y \
    ghc \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash runner -u 1000

USER runner
WORKDIR /workspace

ENV LANG=C.UTF-8

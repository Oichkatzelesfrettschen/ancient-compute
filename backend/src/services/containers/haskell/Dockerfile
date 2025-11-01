# Ancient Compute - Haskell Language Container
# Includes GHC 9.6.2, Stack, and QuickCheck for property testing
FROM haskell:9.6.2

# Install Stack and runtime dependencies
RUN apt-get update && apt-get install -y \
    stack \
    cabal-install \
    libgmp-dev \
    zlib1g-dev \
    --no-install-recommends && \
    rm -rf /var/lib/apt/lists/*

# Setup Stack (pre-cache tools)
RUN stack setup --install-ghc

# Pre-compile common dependencies to avoid rebuild on each run
RUN stack install --in-place \
    QuickCheck \
    HUnit \
    containers \
    mtl \
    base \
    && rm -rf /root/.stack/indices /root/.stack/programs/*/ghc-*/lib

# Create non-root user with no-new-privileges
RUN useradd -m -s /bin/bash runner -u 1000

# Setup working directory
WORKDIR /workspace

# Set up Haskell environment
ENV LANG=C.UTF-8
ENV GHC_PACKAGE_PATH=/root/.stack/snapshots/x86_64-linux/*/lib:/opt/ghc/lib

# Security: non-root execution
USER runner

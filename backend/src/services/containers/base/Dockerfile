# Ancient Compute - Base Container Image
FROM debian:bookworm-slim

# Install minimal dependencies
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash runner -u 1000

USER runner
WORKDIR /workspace

# Set environment
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

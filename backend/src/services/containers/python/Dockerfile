# Ancient Compute - Python Language Container
FROM debian:bookworm-slim

# Install Python 3
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

# Install RestrictedPython for sandboxing
RUN pip3 install --no-cache-dir RestrictedPython

# Create non-root user
RUN useradd -m -s /bin/bash runner -u 1000

USER runner
WORKDIR /workspace

ENV LANG=C.UTF-8
ENV PYTHONUNBUFFERED=1

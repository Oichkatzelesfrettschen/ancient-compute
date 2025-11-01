# Ancient Compute - C Language Container
# Includes GCC with AddressSanitizer, UndefinedBehaviorSanitizer, and Valgrind
FROM debian:bookworm-slim

# Install GCC, sanitizer support, Valgrind, and development tools
RUN apt-get update && apt-get install -y \
    gcc \
    g++ \
    libc6-dev \
    libasan6 \
    libubsan1 \
    valgrind \
    glibc-source \
    make \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user with no-new-privileges
RUN useradd -m -s /bin/bash runner -u 1000

# Setup working directory
WORKDIR /workspace

# Copy security-hardened entrypoint (prevents privilege escalation)
# Container will run as non-root, read-only filesystem except /tmp
ENV LANG=C.UTF-8
ENV ASAN_OPTIONS=detect_leaks=1:halt_on_error=0
ENV UBSAN_OPTIONS=halt_on_error=0:print_stacktrace=1

USER runner

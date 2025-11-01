FROM debian:bookworm-slim

# Install SBCL (Steel Bank Common Lisp)
RUN apt-get update && apt-get install -y \
    sbcl \
    python3 \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy requirements.txt and install Python dependencies if any
COPY requirements.txt .
RUN pip3 install -r requirements.txt

# Copy source code
COPY src/ ./src/

# Set entrypoint
ENTRYPOINT ["python3", "src/execute.py"]


#!/bin/bash
set -euo pipefail

# Ancient Compute Debian/Linux Setup Script

echo "================================================"
echo "Ancient Compute - Debian/Linux Setup"
echo "================================================"
echo ""

# Check for root privileges
if [[ $EUID -eq 0 ]]; then
   echo "ERROR: This script should NOT be run as root"
   echo "Please run as your regular user"
   exit 1
fi

# Update package list
echo "Updating package list..."
sudo apt-get update

# Install build essentials
echo "Installing build essentials..."
sudo apt-get install -y \
    build-essential \
    git \
    curl \
    wget \
    ca-certificates \
    gnupg \
    lsb-release

# Install Python 3.11
echo "Installing Python 3.11..."
sudo apt-get install -y \
    python3.11 \
    python3.11-venv \
    python3.11-dev \
    python3-pip

# Install Node.js 20 LTS
echo "Installing Node.js 20 LTS..."
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs

# Install pnpm
echo "Installing pnpm..."
npm install -g pnpm

# Install Bazel via Bazelisk
echo "Installing Bazelisk (Bazel version manager)..."
wget https://github.com/bazelbuild/bazelisk/releases/latest/download/bazelisk-linux-amd64 \
    -O /tmp/bazelisk
chmod +x /tmp/bazelisk
sudo mv /tmp/bazelisk /usr/local/bin/bazel

# Install Docker if not present
if ! command -v docker &> /dev/null; then
    echo "Installing Docker..."
    curl -fsSL https://get.docker.com -o /tmp/get-docker.sh
    sudo sh /tmp/get-docker.sh
    sudo usermod -aG docker $USER
    echo "Docker installed. You'll need to log out and back in for group changes to take effect"
fi

# Install gVisor runtime
echo "Installing gVisor runtime for enhanced container security..."
curl -fsSL https://gvisor.dev/archive.key | sudo gpg --dearmor -o /usr/share/keyrings/gvisor-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/gvisor-archive-keyring.gpg] https://storage.googleapis.com/gvisor/releases release main" | sudo tee /etc/apt/sources.list.d/gvisor.list > /dev/null
sudo apt-get update && sudo apt-get install -y runsc

# Configure Docker to use gVisor
echo "Configuring Docker to use gVisor runtime..."
sudo mkdir -p /etc/docker
cat << EOF | sudo tee /etc/docker/daemon.json
{
  "runtimes": {
    "runsc": {
      "path": "/usr/bin/runsc"
    }
  }
}
EOF

# Restart Docker
sudo systemctl restart docker

# Install LaTeX for documentation
echo "Installing LaTeX for documentation generation..."
sudo apt-get install -y \
    texlive-xetex \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    texlive-science \
    latexmk

# Configure Git
echo "Configuring Git..."
git config --global core.autocrlf input
git config --global core.eol lf

echo ""
echo "================================================"
echo "Setup Complete!"
echo "================================================"
echo ""
echo "Next steps:"
echo "1. Log out and back in for Docker group changes to take effect"
echo "2. Run: docker-compose up -d"
echo "3. Run: bazel build //..."
echo ""
echo "Development servers:"
echo "  Frontend: http://localhost:3000"
echo "  Backend:  http://localhost:8000"
echo "  API Docs: http://localhost:8000/docs"
echo ""

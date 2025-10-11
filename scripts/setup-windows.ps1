# Ancient Compute Windows Setup Script
# Requires: Windows 11, Administrator privileges

Write-Host "Ancient Compute - Windows 11 Setup" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host ""

# Check for Administrator privileges
if (-NOT ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "ERROR: This script requires Administrator privileges" -ForegroundColor Red
    Write-Host "Please run PowerShell as Administrator and try again" -ForegroundColor Yellow
    exit 1
}

# Install Chocolatey if not present
if (!(Get-Command choco -ErrorAction SilentlyContinue)) {
    Write-Host "Installing Chocolatey package manager..." -ForegroundColor Yellow
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
}

# Install required tools
Write-Host "Installing development tools..." -ForegroundColor Yellow

choco install -y git
choco install -y python --version=3.11.6
choco install -y nodejs-lts --version=20.10.0
choco install -y bazelisk
choco install -y docker-desktop
choco install -y vscode

# Install pnpm
Write-Host "Installing pnpm..." -ForegroundColor Yellow
npm install -g pnpm

# Enable WSL2
Write-Host "Enabling WSL2..." -ForegroundColor Yellow
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

# Set WSL2 as default
wsl --set-default-version 2

# Install Debian
Write-Host "Installing Debian on WSL2..." -ForegroundColor Yellow
wsl --install -d Debian

# Configure Git
Write-Host "Configuring Git for cross-platform development..." -ForegroundColor Yellow
git config --global core.autocrlf input
git config --global core.eol lf

Write-Host ""
Write-Host "Setup Complete!" -ForegroundColor Green
Write-Host "===============" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "1. Restart your computer to complete WSL2 installation" -ForegroundColor White
Write-Host "2. Start Docker Desktop and enable WSL2 integration" -ForegroundColor White
Write-Host "3. Run: cd ancient_compute && ./scripts/setup-debian.sh (from WSL2)" -ForegroundColor White
Write-Host "4. Run: docker-compose up -d" -ForegroundColor White
Write-Host ""

# Docker Compose and Security Infrastructure Specification

## Overview

Complete Docker infrastructure for Ancient Compute platform with multi-layer security, language service isolation, and cross-platform compatibility (Windows 11 + Debian).

## Docker Compose Architecture

### Main docker-compose.yml (Development)

```yaml
version: '3.9'

x-common-variables: &common-variables
  NODE_ENV: development
  LOG_LEVEL: debug
  TZ: UTC

x-security-opts: &security-opts
  security_opt:
    - no-new-privileges:true
    - seccomp:./config/seccomp/default.json
    - apparmor:docker-default
  cap_drop:
    - ALL
  read_only: true
  tmpfs:
    - /tmp:noexec,nosuid,size=100M
    - /run:noexec,nosuid,size=10M

services:
  # Core Infrastructure
  nginx:
    image: nginx:alpine
    container_name: ancient-nginx
    <<: *security-opts
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./config/nginx/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./config/nginx/sites:/etc/nginx/sites-enabled:ro
      - ./certs:/etc/nginx/certs:ro
      - nginx-cache:/var/cache/nginx
    networks:
      - frontend-network
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "wget", "--spider", "-q", "http://localhost/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  redis:
    image: redis:7-alpine
    container_name: ancient-redis
    <<: *security-opts
    command: redis-server /usr/local/etc/redis/redis.conf
    volumes:
      - ./config/redis/redis.conf:/usr/local/etc/redis/redis.conf:ro
      - redis-data:/data
    networks:
      - backend-network
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5

  postgres:
    image: postgres:15-alpine
    container_name: ancient-postgres
    environment:
      POSTGRES_DB: ancient_compute
      POSTGRES_USER: ${DB_USER:-ancient}
      POSTGRES_PASSWORD: ${DB_PASSWORD:-changeme}
      POSTGRES_INITDB_ARGS: "--encoding=UTF-8 --locale=C"
    volumes:
      - postgres-data:/var/lib/postgresql/data
      - ./migrations:/docker-entrypoint-initdb.d:ro
    networks:
      - backend-network
    restart: unless-stopped
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ${DB_USER:-ancient}"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Application Services
  backend:
    build:
      context: ./backend
      dockerfile: Dockerfile
      target: development
    container_name: ancient-backend
    <<: *security-opts
    environment:
      <<: *common-variables
      DATABASE_URL: postgresql://${DB_USER:-ancient}:${DB_PASSWORD:-changeme}@postgres:5432/ancient_compute
      REDIS_URL: redis://redis:6379
      SECRET_KEY: ${SECRET_KEY:-development-secret-key}
    volumes:
      - ./backend/src:/app/src:ro
      - backend-tmp:/tmp
    networks:
      - backend-network
      - language-network
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
    restart: unless-stopped
    command: uvicorn main:app --host 0.0.0.0 --port 8000 --reload

  frontend:
    build:
      context: ./frontend
      dockerfile: Dockerfile
      target: development
    container_name: ancient-frontend
    <<: *security-opts
    environment:
      <<: *common-variables
      PUBLIC_API_URL: http://backend:8000
    volumes:
      - ./frontend/src:/app/src:ro
      - ./frontend/static:/app/static:ro
      - frontend-tmp:/tmp
    networks:
      - frontend-network
    restart: unless-stopped
    command: npm run dev -- --host 0.0.0.0

  # Language Services (Sandboxed)
  c-service:
    build:
      context: ./services/c
      dockerfile: Dockerfile
    container_name: ancient-c-service
    <<: *security-opts
    cap_add:
      - SYS_PTRACE  # For debugging compiled programs
    environment:
      MAX_EXECUTION_TIME: 10
      MAX_MEMORY_MB: 256
      MAX_OUTPUT_SIZE: 1048576
    volumes:
      - ./services/c/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc  # gVisor runtime for enhanced isolation

  python-service:
    build:
      context: ./services/python
      dockerfile: Dockerfile
    container_name: ancient-python-service
    <<: *security-opts
    environment:
      RESTRICTED_PYTHON: "true"
      MAX_EXECUTION_TIME: 10
      MAX_MEMORY_MB: 256
    volumes:
      - ./services/python/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  haskell-service:
    build:
      context: ./services/haskell
      dockerfile: Dockerfile
    container_name: ancient-haskell-service
    <<: *security-opts
    environment:
      STACK_ROOT: /tmp/stack
      MAX_EXECUTION_TIME: 15
      MAX_MEMORY_MB: 512
    volumes:
      - ./services/haskell/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  idris-service:
    build:
      context: ./services/idris
      dockerfile: Dockerfile
    container_name: ancient-idris-service
    <<: *security-opts
    environment:
      MAX_EXECUTION_TIME: 20
      MAX_MEMORY_MB: 512
      PROOF_CHECKING: "true"
    volumes:
      - ./services/idris/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  lisp-service:
    build:
      context: ./services/lisp
      dockerfile: Dockerfile
    container_name: ancient-lisp-service
    <<: *security-opts
    environment:
      SBCL_HOME: /usr/local/lib/sbcl
      MAX_EXECUTION_TIME: 10
      MAX_MEMORY_MB: 256
    volumes:
      - ./services/lisp/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  assembly-service:
    build:
      context: ./services/assembly
      dockerfile: Dockerfile
    container_name: ancient-assembly-service
    <<: *security-opts
    environment:
      QEMU_USER_MODE: "true"
      MAX_EXECUTION_TIME: 5
      MAX_MEMORY_MB: 128
    volumes:
      - ./services/assembly/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  java-service:
    build:
      context: ./services/java
      dockerfile: Dockerfile
    container_name: ancient-java-service
    <<: *security-opts
    environment:
      JAVA_OPTS: "-Xmx256m -Djava.security.manager=default"
      MAX_EXECUTION_TIME: 10
      MAX_MEMORY_MB: 256
    volumes:
      - ./services/java/sandbox-config.json:/app/sandbox-config.json:ro
      - ./services/java/security.policy:/app/security.policy:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  systemf-service:
    build:
      context: ./services/systemf
      dockerfile: Dockerfile
    container_name: ancient-systemf-service
    <<: *security-opts
    environment:
      MAX_EXECUTION_TIME: 15
      MAX_MEMORY_MB: 256
      TYPE_CHECKING: "strict"
    volumes:
      - ./services/systemf/sandbox-config.json:/app/sandbox-config.json:ro
    networks:
      - language-network
    restart: unless-stopped
    runtime: runsc

  # Documentation Service
  latex-service:
    build:
      context: ./docs
      dockerfile: Dockerfile.latex
    container_name: ancient-latex
    <<: *security-opts
    environment:
      TEXMFHOME: /tmp/texmf
    volumes:
      - ./docs:/workspace:ro
      - ./docs/output:/output
      - latex-cache:/tmp/texmf
    networks:
      - backend-network
    restart: unless-stopped

  # Monitoring Stack
  prometheus:
    image: prom/prometheus:latest
    container_name: ancient-prometheus
    <<: *security-opts
    volumes:
      - ./config/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - prometheus-data:/prometheus
    networks:
      - monitoring-network
    restart: unless-stopped

  grafana:
    image: grafana/grafana:latest
    container_name: ancient-grafana
    <<: *security-opts
    environment:
      GF_SECURITY_ADMIN_PASSWORD: ${GRAFANA_PASSWORD:-admin}
      GF_INSTALL_PLUGINS: grafana-piechart-panel
    volumes:
      - ./config/grafana/dashboards:/etc/grafana/provisioning/dashboards:ro
      - ./config/grafana/datasources:/etc/grafana/provisioning/datasources:ro
      - grafana-data:/var/lib/grafana
    networks:
      - monitoring-network
      - frontend-network
    restart: unless-stopped

networks:
  frontend-network:
    driver: bridge
    ipam:
      config:
        - subnet: 172.20.0.0/24
  backend-network:
    driver: bridge
    ipam:
      config:
        - subnet: 172.21.0.0/24
  language-network:
    driver: bridge
    internal: true  # No external access
    ipam:
      config:
        - subnet: 172.22.0.0/24
  monitoring-network:
    driver: bridge
    ipam:
      config:
        - subnet: 172.23.0.0/24

volumes:
  postgres-data:
  redis-data:
  nginx-cache:
  backend-tmp:
  frontend-tmp:
  latex-cache:
  prometheus-data:
  grafana-data:
```

### docker-compose.prod.yml (Production Override)

```yaml
version: '3.9'

services:
  nginx:
    image: ancient-compute/nginx:${VERSION:-latest}
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"

  backend:
    image: ancient-compute/backend:${VERSION:-latest}
    environment:
      NODE_ENV: production
      LOG_LEVEL: info
    deploy:
      replicas: 3
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G

  frontend:
    image: ancient-compute/frontend:${VERSION:-latest}
    environment:
      NODE_ENV: production
    deploy:
      replicas: 2
      resources:
        limits:
          cpus: '1'
          memory: 1G

  # Language services with production limits
  c-service:
    image: ancient-compute/c-service:${VERSION:-latest}
    deploy:
      replicas: 2
      resources:
        limits:
          cpus: '0.5'
          memory: 512M

  # Additional production-specific overrides...
```

## Security Configuration

### Seccomp Profile (config/seccomp/default.json)

```json
{
  "defaultAction": "SCMP_ACT_ERRNO",
  "architectures": [
    "SCMP_ARCH_X86_64",
    "SCMP_ARCH_X86"
  ],
  "syscalls": [
    {
      "names": [
        "read", "write", "open", "close", "stat", "fstat",
        "mmap", "mprotect", "munmap", "brk", "rt_sigaction",
        "rt_sigprocmask", "ioctl", "access", "execve", "getuid",
        "getgid", "geteuid", "getegid", "fcntl", "dup", "dup2",
        "pipe", "select", "sched_yield", "mremap", "msync",
        "mincore", "madvise", "shmget", "shmat", "shmctl",
        "dup", "nanosleep", "getitimer", "setitimer", "alarm",
        "getpid", "sendfile", "socket", "connect", "accept",
        "sendto", "recvfrom", "sendmsg", "recvmsg", "shutdown",
        "bind", "listen", "getsockname", "getpeername", "clone",
        "fork", "vfork", "exit", "wait4", "kill", "newuname",
        "semget", "semop", "semctl", "shmdt", "msgget", "msgsnd",
        "msgrcv", "msgctl", "getpgrp", "setsid", "sigaltstack",
        "arch_prctl", "mount", "umount", "prctl", "gettid",
        "futex", "set_thread_area", "get_thread_area",
        "set_tid_address", "restart_syscall", "exit_group",
        "epoll_wait", "epoll_ctl", "tgkill", "mbind",
        "set_mempolicy", "get_mempolicy", "openat", "mkdirat",
        "fstatat", "unlinkat", "readlinkat", "fchmodat",
        "faccessat", "pselect6", "ppoll", "set_robust_list",
        "get_robust_list", "epoll_pwait", "epoll_create1",
        "pipe2", "getrandom"
      ],
      "action": "SCMP_ACT_ALLOW"
    }
  ]
}
```

### Sandbox Configuration (services/*/sandbox-config.json)

```json
{
  "execution": {
    "max_time_seconds": 10,
    "max_memory_mb": 256,
    "max_processes": 5,
    "max_file_descriptors": 100,
    "max_output_bytes": 1048576,
    "max_input_bytes": 102400
  },
  "filesystem": {
    "readonly_paths": ["/usr", "/lib", "/bin", "/sbin"],
    "writable_paths": ["/tmp/sandbox"],
    "denied_paths": ["/proc", "/sys", "/dev"],
    "max_file_size_bytes": 10485760,
    "max_total_size_bytes": 104857600
  },
  "network": {
    "allowed": false,
    "allowed_ports": [],
    "allowed_hosts": []
  },
  "syscalls": {
    "denied": [
      "mount", "umount", "pivot_root", "chroot",
      "setuid", "setgid", "setgroups", "capset",
      "reboot", "kexec_load", "open_by_handle_at",
      "init_module", "finit_module", "delete_module"
    ]
  },
  "environment": {
    "clear_env": true,
    "allowed_vars": ["PATH", "HOME", "TMPDIR"],
    "set_vars": {
      "PATH": "/usr/local/bin:/usr/bin:/bin",
      "HOME": "/tmp/sandbox",
      "TMPDIR": "/tmp/sandbox"
    }
  }
}
```

### Language Service Dockerfile Template

```dockerfile
# Base image with minimal attack surface
FROM alpine:3.18 AS base

# Install only essential packages
RUN apk add --no-cache \
    ca-certificates \
    && rm -rf /var/cache/apk/*

# Create non-root user for running services
RUN addgroup -g 1000 -S appuser \
    && adduser -u 1000 -S appuser -G appuser

# Build stage for language-specific compilation
FROM base AS builder

# Install build dependencies (language-specific)
RUN apk add --no-cache \
    gcc \
    musl-dev \
    make

# Copy source code
WORKDIR /build
COPY requirements.txt ./
COPY src ./src

# Build/compile language service
RUN make build

# Runtime stage with minimal dependencies
FROM base AS runtime

# Copy built artifacts from builder
COPY --from=builder /build/dist /app

# Set up sandbox environment
RUN mkdir -p /tmp/sandbox \
    && chown appuser:appuser /tmp/sandbox \
    && chmod 700 /tmp/sandbox

# Copy sandbox configuration
COPY sandbox-config.json /app/

# Drop privileges
USER appuser
WORKDIR /app

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ["/app/healthcheck"]

# Run service
CMD ["/app/server"]
```

### C Service Specific Configuration

```dockerfile
FROM alpine:3.18 AS base

RUN apk add --no-cache \
    gcc \
    musl-dev \
    valgrind \
    strace \
    && rm -rf /var/cache/apk/*

FROM base AS runtime

COPY server.py compiler.py sandbox.py /app/
COPY sandbox-config.json /app/

# Install Python for service wrapper
RUN apk add --no-cache python3 py3-pip \
    && pip3 install --no-cache-dir \
        fastapi \
        uvicorn \
        pydantic

# Seccomp filter for C compilation/execution
COPY c-seccomp.json /app/

USER 1000:1000
WORKDIR /app

CMD ["uvicorn", "server:app", "--host", "0.0.0.0", "--port", "8001"]
```

### Python Service with RestrictedPython

```dockerfile
FROM python:3.11-alpine AS base

FROM base AS runtime

# Install RestrictedPython and dependencies
RUN pip install --no-cache-dir \
    RestrictedPython==6.2 \
    fastapi==0.104.0 \
    uvicorn==0.24.0 \
    mypy==1.6.0

COPY server.py executor.py type_checker.py /app/
COPY sandbox-config.json /app/

# Create sandbox environment
RUN mkdir -p /tmp/sandbox \
    && chmod 700 /tmp/sandbox

USER 1000:1000
WORKDIR /app

CMD ["uvicorn", "server:app", "--host", "0.0.0.0", "--port", "8002"]
```

## Kubernetes Deployment (Alternative)

### Deployment Manifest

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ancient-backend
  namespace: ancient-compute
spec:
  replicas: 3
  selector:
    matchLabels:
      app: backend
  template:
    metadata:
      labels:
        app: backend
    spec:
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        fsGroup: 1000
        seccompProfile:
          type: RuntimeDefault
      containers:
      - name: backend
        image: ancient-compute/backend:latest
        securityContext:
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
          capabilities:
            drop:
              - ALL
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8000
          initialDelaySeconds: 5
          periodSeconds: 5
        volumeMounts:
        - name: tmp
          mountPath: /tmp
        - name: config
          mountPath: /app/config
          readOnly: true
      volumes:
      - name: tmp
        emptyDir:
          sizeLimit: 100Mi
      - name: config
        configMap:
          name: backend-config
```

### Network Policy

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: language-service-isolation
  namespace: ancient-compute
spec:
  podSelector:
    matchLabels:
      tier: language-service
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: backend
    ports:
    - protocol: TCP
      port: 8000
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: backend
    ports:
    - protocol: TCP
      port: 8000
```

## Security Monitoring

### Falco Rules (runtime-security.yaml)

```yaml
- rule: Unexpected Process in Language Container
  desc: Detect processes that shouldn't run in language containers
  condition: >
    container.id != host and
    container.image.repository contains "ancient-compute" and
    container.image.repository contains "-service" and
    not proc.name in (allowed_processes)
  output: >
    Unexpected process in language container
    (user=%user.name container=%container.name process=%proc.name)
  priority: WARNING

- list: allowed_processes
  items: [python, gcc, java, ghc, idris2, sbcl, qemu-x86_64]

- rule: Write to System Directory
  desc: Detect attempts to write to system directories
  condition: >
    container.id != host and
    fd.name startswith (/bin, /sbin, /usr, /lib) and
    evt.type in (open, openat) and
    evt.is_open_write=true
  output: >
    Write attempt to system directory
    (user=%user.name file=%fd.name container=%container.name)
  priority: ERROR

- rule: Network Connection from Language Service
  desc: Language services should not make network connections
  condition: >
    container.id != host and
    container.image.repository contains "-service" and
    evt.type in (connect, accept)
  output: >
    Network activity from language service
    (container=%container.name connection=%fd.name)
  priority: CRITICAL
```

## Container Registry Security

### Registry Configuration

```yaml
# docker-registry.yml
version: '3.9'

services:
  registry:
    image: registry:2
    container_name: ancient-registry
    environment:
      REGISTRY_AUTH: htpasswd
      REGISTRY_AUTH_HTPASSWD_PATH: /auth/htpasswd
      REGISTRY_AUTH_HTPASSWD_REALM: Registry Realm
      REGISTRY_HTTP_TLS_CERTIFICATE: /certs/cert.pem
      REGISTRY_HTTP_TLS_KEY: /certs/key.pem
      REGISTRY_STORAGE_DELETE_ENABLED: "true"
    volumes:
      - ./auth:/auth:ro
      - ./certs:/certs:ro
      - registry-data:/var/lib/registry
    ports:
      - "5000:5000"
    networks:
      - registry-network

  registry-ui:
    image: joxit/docker-registry-ui:latest
    container_name: ancient-registry-ui
    environment:
      REGISTRY_TITLE: Ancient Compute Registry
      REGISTRY_URL: https://registry:5000
      DELETE_IMAGES: "true"
      SINGLE_REGISTRY: "true"
    ports:
      - "5001:80"
    depends_on:
      - registry
    networks:
      - registry-network

volumes:
  registry-data:

networks:
  registry-network:
    driver: bridge
```

## Build and Deployment Scripts

### build.sh

```bash
#!/bin/bash
set -euo pipefail

VERSION=${1:-latest}
REGISTRY=${REGISTRY:-localhost:5000}

# Build base images
docker build -t $REGISTRY/ancient-base:$VERSION -f Dockerfile.base .

# Build application images
for service in backend frontend c python haskell idris lisp assembly java systemf; do
    echo "Building $service..."
    docker build \
        --build-arg BASE_IMAGE=$REGISTRY/ancient-base:$VERSION \
        -t $REGISTRY/ancient-$service:$VERSION \
        -f services/$service/Dockerfile \
        services/$service
done

# Run security scanning
for image in $(docker images $REGISTRY/ancient-* -q); do
    trivy image --severity HIGH,CRITICAL $image
done

# Push to registry
docker-compose push
```

### deploy.sh

```bash
#!/bin/bash
set -euo pipefail

ENVIRONMENT=${1:-development}
VERSION=${2:-latest}

# Load environment configuration
source .env.$ENVIRONMENT

# Pull latest images
docker-compose -f docker-compose.yml -f docker-compose.$ENVIRONMENT.yml pull

# Run database migrations
docker-compose run --rm backend alembic upgrade head

# Deploy services with zero downtime
docker-compose -f docker-compose.yml -f docker-compose.$ENVIRONMENT.yml up -d --no-deps --scale backend=3

# Health check
./scripts/healthcheck.sh

# Cleanup old containers
docker container prune -f
```

## Windows-Specific Considerations

### Docker Desktop for Windows Configuration

```json
{
  "builder": {
    "gc": {
      "enabled": true,
      "defaultKeepStorage": "20GB"
    }
  },
  "experimental": false,
  "features": {
    "buildkit": true
  },
  "wslEngineEnabled": true,
  "resources": {
    "cpus": 4,
    "memoryMiB": 8192,
    "diskSizeMiB": 65536
  }
}
```

### WSL2 Integration

```powershell
# setup-wsl2.ps1
# Enable WSL2 and install required distributions

# Enable WSL2
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

# Set WSL2 as default
wsl --set-default-version 2

# Install Debian for consistency with production
wsl --install -d Debian

# Configure Docker Desktop to use WSL2
# This is done through Docker Desktop UI settings

# Install gVisor runtime for enhanced isolation
wsl -d Debian -e bash -c "curl -fsSL https://gvisor.dev/archive.key | sudo apt-key add -"
wsl -d Debian -e bash -c "echo 'deb https://storage.googleapis.com/gvisor/releases release main' | sudo tee /etc/apt/sources.list.d/gvisor.list"
wsl -d Debian -e bash -c "sudo apt-get update && sudo apt-get install -y runsc"
```

## Monitoring and Observability

### Prometheus Configuration

```yaml
# config/prometheus/prometheus.yml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'backend'
    static_configs:
      - targets: ['backend:8000']
    metrics_path: '/metrics'

  - job_name: 'language-services'
    static_configs:
      - targets:
        - 'c-service:8001'
        - 'python-service:8002'
        - 'haskell-service:8003'
        - 'idris-service:8004'
        - 'lisp-service:8005'
        - 'assembly-service:8006'
        - 'java-service:8007'
        - 'systemf-service:8008'

  - job_name: 'docker'
    static_configs:
      - targets: ['localhost:9323']
```

This comprehensive Docker infrastructure provides:
1. Multi-layer security with gVisor, seccomp, and AppArmor
2. Complete language service isolation
3. Cross-platform compatibility (Windows 11 + Debian)
4. Production-ready configurations
5. Monitoring and observability
6. Zero-downtime deployment strategies
7. Container registry for image management

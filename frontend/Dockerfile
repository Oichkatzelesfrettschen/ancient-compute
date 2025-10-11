# Ancient Compute Frontend Dockerfile
# Multi-stage build for development and production

# Base stage - common dependencies
FROM node:20-alpine AS base

# Set environment variables
ENV NODE_ENV=development \
    PNPM_HOME="/pnpm" \
    PATH="$PNPM_HOME:$PATH"

# Install pnpm
RUN corepack enable && corepack prepare pnpm@latest --activate

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -u 1000 -D -G appuser appuser

WORKDIR /app

# Dependencies stage
FROM base AS deps

# Copy package files
COPY package.json pnpm-lock.yaml* ./

# Install dependencies
RUN pnpm install --frozen-lockfile

# Development stage
FROM base AS development

# Copy dependencies from deps stage
COPY --from=deps /app/node_modules ./node_modules

# Copy application code
COPY --chown=appuser:appuser . .

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:3000/ || exit 1

# Development command (overridden in docker-compose)
CMD ["pnpm", "dev", "--host", "0.0.0.0"]

# Builder stage for production
FROM base AS builder

# Copy dependencies
COPY --from=deps /app/node_modules ./node_modules

# Copy source code
COPY . .

# Build application
RUN pnpm build

# Production stage
FROM node:20-alpine AS production

# Install only production dependencies
RUN addgroup -g 1000 appuser && \
    adduser -u 1000 -D -G appuser appuser

WORKDIR /app

# Copy built application
COPY --from=builder --chown=appuser:appuser /app/build ./build
COPY --from=builder --chown=appuser:appuser /app/package.json .

# Install only production dependencies
RUN corepack enable && \
    corepack prepare pnpm@latest --activate && \
    pnpm install --prod --frozen-lockfile

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:3000/ || exit 1

# Production command
CMD ["node", "build"]

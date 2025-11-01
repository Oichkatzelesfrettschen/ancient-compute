# Ancient Compute Backend - Configuration Management

import os
from typing import List

from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    """Application settings loaded from environment variables"""

    # Application
    APP_NAME: str = "Ancient Compute"
    VERSION: str = "0.1.0"
    ENVIRONMENT: str = "development"
    DEBUG: bool = True

    # Server
    HOST: str = "0.0.0.0"
    PORT: int = 8000

    # Security
    SECRET_KEY: str = "development-secret-key-change-in-production"
    ALLOWED_ORIGINS: List[str] = [
        "http://localhost:3000",
        "http://localhost:8000",
        "http://frontend:3000",
    ]

    # Database
    DATABASE_URL: str = "postgresql://ancient:development_password@postgres:5432/ancient_compute"
    DB_POOL_SIZE: int = 5
    DB_MAX_OVERFLOW: int = 10

    # Redis
    REDIS_URL: str = "redis://redis:6379"
    REDIS_MAX_CONNECTIONS: int = 10

    # Language Services
    LANGUAGE_SERVICE_TIMEOUT: int = 30
    MAX_EXECUTION_TIME: int = 10
    MAX_MEMORY_MB: int = 256

    # Logging
    LOG_LEVEL: str = "DEBUG"

    class Config:
        env_file = ".env"
        case_sensitive = True


# Create global settings instance
settings = Settings()

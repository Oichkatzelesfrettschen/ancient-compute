# Provides 'ancient-compute' CLI entry point. Tool config lives in root pyproject.toml.
from setuptools import find_packages, setup

setup(
    name="ancient-compute-backend",
    version="0.1.0",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    install_requires=[
        "fastapi>=0.104.0",
        "uvicorn[standard]>=0.24.0",
        "sqlalchemy>=2.0.0",
        "alembic>=1.12.0",
        "pydantic>=2.0.0",
        "python-dotenv>=1.0.0",
        "docker>=6.1.0",
        "RestrictedPython>=6.0",
        "pytest>=7.4.0",
        "pytest-asyncio>=0.21.0",
        "click>=8.0",
        "rich>=14.0",
        "textual>=8.0",
    ],
    entry_points={
        "console_scripts": [
            "ancient-compute=backend.src.emulator.cli.app:cli",
        ],
    },
    python_requires=">=3.11",
    author="Ancient Compute Team",
    description="Educational platform for learning computation history",
)

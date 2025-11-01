#!/usr/bin/env python3
"""
Ancient Compute - Backend Restructuring Script
Fixes the package structure issues identified in the architectural review
"""
import os
import shutil
from pathlib import Path
import sys


def restructure_backend():
    """Restructure backend to fix import path issues"""
    backend = Path(__file__).parent
    src = backend / "src"
    services = backend / "services"

    print("Ancient Compute Backend Restructuring")
    print("=" * 50)

    # Step 1: Move services under src
    if services.exists() and not (src / "services").exists():
        print(f"Moving {services} -> {src / 'services'}...")
        shutil.move(str(services), str(src / "services"))
        print("[SUCCESS] Services moved under src/")
    elif (src / "services").exists():
        print("[INFO] Services already under src/")
    else:
        print("[ERROR] Services directory not found!")
        return False

    # Step 2: Update imports in code_execution.py
    code_exec = src / "api" / "code_execution.py"
    if code_exec.exists():
        print(f"Updating imports in {code_exec}...")
        content = code_exec.read_text()

        # Fix the import path
        old_import = "from ...services.languages import get_executor"
        new_import = "from ..services.languages import get_executor"

        if old_import in content:
            content = content.replace(old_import, new_import)
            code_exec.write_text(content)
            print("[SUCCESS] Updated import path in code_execution.py")
        elif new_import in content:
            print("[INFO] Import path already correct")
        else:
            print("[WARNING] Expected import not found in code_execution.py")

    # Step 3: Fix database import
    if code_exec.exists():
        content = code_exec.read_text()
        # Fix the models import
        old_models = "from ..models import CodeSubmission, Lesson"
        new_models = "from ..models import CodeSubmission"

        if old_models in content:
            content = content.replace(old_models, new_models)
            code_exec.write_text(content)
            print("[SUCCESS] Fixed models import")

    # Step 4: Create setup.py for proper package management
    setup_py = backend / "setup.py"
    if not setup_py.exists():
        print("Creating setup.py for package management...")
        setup_content = '''from setuptools import setup, find_packages

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
    ],
    python_requires=">=3.11",
    author="Ancient Compute Team",
    description="Educational platform for learning computation history",
)
'''
        setup_py.write_text(setup_content)
        print("[SUCCESS] Created setup.py")

    # Step 5: Update __init__.py files to ensure proper imports
    services_init = src / "services" / "__init__.py" if (src / "services").exists() else None
    if services_init and services_init.exists():
        content = services_init.read_text()
        if not content.strip() or content.strip() == '# Ancient Compute - Language Execution Services\n__version__ = "0.1.0"':
            new_content = '''# Ancient Compute - Language Execution Services
__version__ = "0.1.0"

from .base_executor import BaseExecutor, ExecutionResult, ExecutionStatus

__all__ = [
    'BaseExecutor',
    'ExecutionResult',
    'ExecutionStatus',
]
'''
            services_init.write_text(new_content)
            print("[SUCCESS] Updated services/__init__.py")

    # Step 6: Show new structure
    print("\nNew structure:")
    print("backend/")
    print("├── src/")
    print("│   ├── api/")
    print("│   │   └── code_execution.py (imports fixed)")
    print("│   ├── models/")
    print("│   └── services/  <-- Moved here")
    print("│       ├── base_executor.py")
    print("│       ├── languages/")
    print("│       └── security/")
    print("└── setup.py (new)")

    print("\n" + "=" * 50)
    print("Restructuring complete!")
    print("\nNext steps:")
    print("1. Install package in development mode:")
    print("   cd backend && pip install -e .")
    print("2. Run tests to verify:")
    print("   python -m pytest tests/")

    return True


def verify_structure():
    """Verify the new structure is correct"""
    backend = Path(__file__).parent
    src = backend / "src"

    checks = [
        (src / "services", "services directory under src"),
        (src / "services" / "base_executor.py", "base_executor.py"),
        (src / "services" / "languages", "languages directory"),
        (src / "api" / "code_execution.py", "code_execution.py"),
        (backend / "setup.py", "setup.py"),
    ]

    print("\nVerifying structure...")
    all_good = True
    for path, description in checks:
        if path.exists():
            print(f"  [OK] {description}")
        else:
            print(f"  [MISSING] {description}")
            all_good = False

    return all_good


if __name__ == "__main__":
    success = restructure_backend()
    if success:
        verify_structure()
    else:
        print("\n[ERROR] Restructuring failed!")
        sys.exit(1)
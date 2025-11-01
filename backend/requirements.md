# Backend Module - Requirements and Architecture

**Module Path**: `backend/`
**Language**: Python 3.11+
**Framework**: FastAPI + SQLAlchemy
**Status**: Phase 1 Complete, Phase 2 In Progress

---

## Overview

The backend is a FastAPI-based REST/WebSocket service that orchestrates code execution across isolated language services, manages user data, and handles curriculum content delivery. It implements a 4-layer architecture:

1. **API Layer** (`src/api/`) - HTTP/WebSocket endpoints
2. **Service Layer** (`src/services/`) - Language service orchestration
3. **Model Layer** (`src/models/`) - Database models and validation
4. **Infrastructure** (`src/`) - Configuration, database, IR definitions

---

## Directory Structure

```
backend/
├── src/
│   ├── __init__.py
│   ├── main.py                    # FastAPI app initialization
│   ├── config.py                  # Environment configuration
│   ├── database.py                # SQLAlchemy setup
│   ├── ir_types.py                # Babbage ISA definitions
│   ├── api/
│   │   ├── __init__.py
│   │   ├── router.py              # API endpoint definitions
│   │   └── code_execution.py      # Execution endpoint
│   ├── models/
│   │   ├── __init__.py            # Model exports
│   │   ├── user.py                # User model (TODO)
│   │   ├── lesson.py              # Lesson model
│   │   ├── module.py              # Module model
│   │   └── code_submission.py     # Code execution results (TODO)
│   ├── services/
│   │   ├── __init__.py            # Service factory
│   │   └── languages/
│   │       ├── __init__.py        # Language service registry
│   │       ├── c_service.py       # C language service
│   │       ├── python_service.py  # Python language service
│   │       ├── haskell_service.py # Haskell language service
│   │       ├── idris_service.py   # IDRIS2 service (TODO)
│   │       ├── lisp_service.py    # LISP service (TODO)
│   │       ├── systemf_service.py # System F service (TODO)
│   │       ├── java_service.py    # Java service (TODO)
│   │       └── assembly_service.py # Babbage assembly service
│   ├── compilers/
│   │   ├── c_*.py                 # C compiler pipeline
│   │   ├── python_*.py            # Python compiler pipeline
│   │   ├── haskell_*.py           # Haskell compiler pipeline
│   │   └── test_*.py              # Compiler tests
│   └── codegen/
│       ├── codegen.py             # IR → Assembly code generator
│       ├── emitter.py             # Babbage assembly emitter
│       ├── liveness.py            # Liveness analysis
│       ├── regalloc.py            # Register allocation
│       └── selector.py            # Instruction selection
├── alembic/
│   ├── env.py                     # Migration environment
│   ├── script.py.mako             # Migration template
│   └── versions/                  # Database migrations
│       └── (auto-generated)
├── tests/
│   ├── __init__.py
│   ├── conftest.py                # pytest fixtures
│   ├── test_api.py                # API endpoint tests
│   ├── test_database.py           # Database tests
│   └── unit/
│       ├── test_config.py         # Configuration tests
│       └── test_models.py         # Model validation tests
├── requirements.txt               # Python dependencies
├── setup.py                       # Package setup
├── .env.example                   # Environment template
└── README.md                      # Module documentation
```

---

## Python Dependencies

### Production Dependencies

**Web Framework** (3 packages):
```
fastapi==0.104.1                   # REST/WebSocket framework
uvicorn[standard]==0.24.0          # ASGI server
python-multipart==0.0.6            # Form data parsing
```

**Database** (3 packages):
```
sqlalchemy==2.0.23                 # ORM and schema
alembic==1.12.1                    # Database migrations
psycopg2-binary==2.9.9             # PostgreSQL driver
```

**Caching** (2 packages):
```
redis==5.0.1                       # Redis client
hiredis==2.2.3                     # C parser for protocol
```

**Authentication & Security** (2 packages):
```
python-jose[cryptography]==3.3.0   # JWT tokens
passlib[bcrypt]==1.7.4             # Password hashing
```

**Configuration & Validation** (4 packages):
```
pydantic==2.5.0                    # Data validation
pydantic-settings==2.1.0           # Environment vars
email-validator==2.1.0             # Email validation
python-dotenv==1.0.0               # .env loading
```

**HTTP Clients** (2 packages):
```
httpx==0.25.1                      # Async HTTP client
aiohttp==3.9.0                     # Async requests
```

**Monitoring & Logging** (1 package):
```
prometheus-client==0.19.0          # Metrics export
```

**Language Execution** (2 packages):
```
docker==7.0.0                      # Docker API client
RestrictedPython==6.2              # Sandboxed execution
```

### Development Dependencies

**Testing** (4 packages):
```
pytest==7.4.3                      # Test framework
pytest-asyncio==0.21.1             # Async test support
pytest-cov==4.1.0                  # Coverage reporting
pytest-mock==3.12.0                # Mocking utilities
```

**Code Quality** (4 packages):
```
black==23.11.0                     # Code formatter
pylint==3.0.2                      # Linter
mypy==1.7.1                        # Type checker
ruff==0.1.6                        # Fast linter
```

**Development Tools** (2 packages):
```
watchfiles==0.21.0                 # File watching
ipython==8.17.2                    # Enhanced REPL
```

**Total**: 51 packages, ~110 MB installed

---

## Python Version Requirements

**Minimum**: 3.11.0
- Supports async context managers and type hints
- Required for FastAPI async features

**Recommended**: 3.12.x
- Performance improvements
- Better error messages

**Not Supported**: Python 3.10 or earlier
- Lacks required features for FastAPI/Pydantic v2

---

## Database Requirements

### PostgreSQL

**Version**: 15.0+ (required)
- Supports JSONB fields (for storing execution results)
- Full text search capabilities
- Connection pooling support

**Connection String**:
```
postgresql://[user]:[password]@[host]:[port]/[database]
# Example: postgresql://postgres:password@localhost:5432/ancient_compute
```

**Database Setup**:
```bash
# 1. Create database (as postgres user)
createdb ancient_compute

# 2. Run migrations
alembic upgrade head

# 3. Verify
psql -U postgres -d ancient_compute -c "SELECT * FROM alembic_version;"
```

**Tables** (created by migrations):
- `users` - User accounts and authentication
- `modules` - Curriculum modules (7 total)
- `lessons` - Individual lessons within modules
- `code_submissions` - User code execution history
- `alembic_version` - Migration tracking

### Redis

**Version**: 7.0+ (recommended)
- In-memory caching for performance
- Session management
- Job queue support (future)

**Configuration**:
```
redis://[host]:[port]/[database]
# Example: redis://localhost:6379/0
```

**Use Cases**:
- Cache compiled code
- Store user sessions
- Rate limiting
- Temporary execution results

**Memory**: 512 MB minimum (1 GB recommended)

---

## API Architecture

### Main Application File

**File**: `src/main.py`

**Initialization**:
```python
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from src.api.router import router

app = FastAPI(
    title="Ancient Compute API",
    version="0.1.0",
    description="Educational platform for computation history"
)

# CORS configuration
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routers
app.include_router(router)
```

### Key Endpoints

**Health & Status**:
- `GET /health` - Basic health check
- `GET /ready` - Readiness probe (checks DB, Redis)
- `GET /metrics` - Prometheus metrics
- `GET /languages` - Available language services

**Code Execution** (main feature):
- `POST /execute` - Execute code in language service
  - Request: code, language, timeout_seconds
  - Response: output, errors, assembly, machine_code, timing

- `POST /validate` - Static analysis without execution
  - Request: code, language
  - Response: errors, warnings, type_info

**Curriculum**:
- `GET /modules` - List all 7 modules
- `GET /modules/{id}/lessons` - Lessons in module
- `GET /lessons/{id}` - Lesson details and exercises

**User Management** (TODO):
- `POST /auth/register` - Create user account
- `POST /auth/login` - Authenticate user
- `GET /users/me` - Current user profile
- `POST /code-submissions` - Save code execution result

---

## Service Architecture

### Compiler Pipeline

Each language service implements the same 4-phase pipeline:

**Phase 1: Lexing**
- Input: Source code (string)
- Output: Token list with metadata
- File: `backend/src/compilers/{language}_lexer.py`

**Phase 2: Parsing**
- Input: Token list
- Output: Abstract Syntax Tree (AST)
- File: `backend/src/compilers/{language}_parser.py`

**Phase 3: Semantic Analysis**
- Input: AST
- Output: Symbol tables, type information
- File: Part of `{language}_compiler.py`

**Phase 4: IR Generation**
- Input: AST + symbol tables
- Output: Babbage Intermediate Representation (IR)
- File: Part of `{language}_compiler.py`

**Phase 5: Code Generation** (shared):
- Input: IR
- Output: Babbage assembly code
- File: `backend/src/codegen/codegen.py`

### Service Factory Pattern

**File**: `backend/src/services/languages/__init__.py`

**Current Implementations**:
```python
def get_executor(language: str) -> LanguageExecutor:
    executors = {
        "c": CService(),
        "python": PythonService(),
        "haskell": HaskellService(),
        "assembly": BabbageAssemblyService(),
    }
    return executors.get(language)
```

**TODO: Add**:
- "idris": IDRISService()
- "lisp": LISPService()
- "systemf": SystemFService()
- "java": JavaService()

---

## IR (Intermediate Representation)

### Babbage ISA Definition

**File**: `backend/src/ir_types.py`

**Architecture**:
- 4 registers: A (accumulator), B (secondary), C (counter), D (destination)
- 2000-word memory (50-bit decimal)
- Stack-based calling convention
- Instruction set: 50+ operations

**IR Types** (map to Babbage):
```python
class IRType(Enum):
    I64 = "i64"         # 64-bit integer
    F64 = "f64"         # 64-bit floating point
    DEC50 = "dec50"     # 50-bit decimal
    PTR = "ptr"         # Memory pointer
    VOID = "void"       # No return value
```

**IR Instructions**:
- Assignment: `result = value`
- BinaryOp: `result = left OP right`
- Load: `result = mem[address]`
- Store: `mem[address] = value`
- Call: `result = function(args)`
- Jump/Branch: Control flow

---

## Configuration Management

### Environment Variables

**File**: `.env` (not in git)
**Template**: `.env.example` (in git)

**Required Variables**:
```bash
# Application
APP_NAME=AncientCompute
ENVIRONMENT=development
HOST=127.0.0.1
PORT=8000
SECRET_KEY=<generate-with-openssl>

# Database
DATABASE_URL=postgresql://postgres:password@localhost:5432/ancient_compute
DB_POOL_SIZE=20
DB_MAX_OVERFLOW=10

# Redis
REDIS_URL=redis://localhost:6379/0
REDIS_MAX_CONNECTIONS=10

# Language Services
LANGUAGE_SERVICE_TIMEOUT=30     # seconds per execution
MAX_EXECUTION_TIME=10           # seconds per code execution
MAX_MEMORY_MB=512               # memory limit per execution

# Logging
LOG_LEVEL=INFO                  # DEBUG, INFO, WARNING, ERROR
```

### Settings Class

**File**: `src/config.py`

**Usage**:
```python
from src.config import settings

print(settings.DATABASE_URL)     # From .env or environment
print(settings.LANGUAGE_SERVICE_TIMEOUT)
```

---

## Database Migrations

### Alembic Setup

**Directory**: `backend/alembic/`

**Typical Workflow**:
```bash
# 1. Make model changes in backend/src/models/

# 2. Generate migration
alembic revision --autogenerate -m "Add users table"

# 3. Review generated file
cat alembic/versions/xxxx_add_users_table.py

# 4. Apply migration
alembic upgrade head

# 5. Verify
psql -d ancient_compute -c "SELECT * FROM information_schema.tables;"
```

### Migration Files

**Location**: `alembic/versions/`
**Format**: Python files with `upgrade()` and `downgrade()` functions

**Example**:
```python
def upgrade() -> None:
    op.create_table(
        'users',
        sa.Column('id', sa.Integer, primary_key=True),
        sa.Column('email', sa.String(255), unique=True),
        sa.Column('password_hash', sa.String(255)),
    )

def downgrade() -> None:
    op.drop_table('users')
```

---

## Testing Strategy

### Unit Tests

**Location**: `backend/tests/`
**Framework**: pytest
**Command**: `pytest tests/ --cov=src -v`

**Target Coverage**: > 90%

**Test Structure**:
```
tests/
├── conftest.py              # Shared fixtures
├── test_api.py              # API endpoint tests
├── test_database.py         # Database tests
└── unit/
    ├── test_config.py       # Configuration validation
    └── test_models.py       # Model validation
```

### Fixtures (conftest.py)

```python
@pytest.fixture
def db_session():
    """Create isolated database session for each test"""
    engine = create_engine("sqlite:///:memory:")
    SessionLocal = sessionmaker(bind=engine)
    Base.metadata.create_all(engine)
    yield SessionLocal()

@pytest.fixture
def client():
    """FastAPI test client"""
    return TestClient(app)

@pytest.fixture
def redis_mock():
    """Mock Redis for testing"""
    return fakeredis.FakeStrictRedis()
```

### Integration Tests

**Scope**: Multiple components working together

**Examples**:
- User registration → Login → Code submission flow
- Code execution → Result storage → User retrieval
- Database transaction rollback on error

---

## Running the Backend

### Development Server

```bash
# 1. Create virtual environment
python3.11 -m venv venv
source venv/bin/activate

# 2. Install dependencies
pip install -r requirements.txt

# 3. Set up environment
cp .env.example .env
# Edit .env with local values

# 4. Initialize database
alembic upgrade head

# 5. Start development server
uvicorn src.main:app --reload

# 6. Access API
# Browser: http://localhost:8000/docs
# API: http://localhost:8000/
# Database: localhost:5432
```

### Production Server

```bash
# Using gunicorn with multiple workers
gunicorn -w 4 -k uvicorn.workers.UvicornWorker \
  --bind 0.0.0.0:8000 \
  src.main:app

# Using Docker
docker build -t ancient-compute-backend .
docker run -p 8000:8000 --env-file .env ancient-compute-backend
```

---

## Code Quality Standards

### Type Hints

**Requirement**: 100% of functions must have type hints

**Enforcement**: `mypy --strict`

**Example**:
```python
def execute_code(
    code: str,
    language: str,
    timeout_seconds: float = 10.0
) -> CompilationResult:
    """Execute code in specified language service."""
    pass
```

### Code Formatting

**Tool**: Black
**Command**: `black src/ tests/`
**Configuration**: `pyproject.toml` (if exists)

### Linting

**Tools**:
- pylint (strict)
- ruff (fast checks)

**Command**:
```bash
pylint src/
ruff check src/
```

**No warnings allowed**: All violations must be fixed or explicitly allowed

---

## Compiler Pipeline Details

### C Service (`backend/src/compilers/c_*.py`)

- **Lexer**: 350+ lines, 40+ token types
- **Parser**: 500+ lines, proper operator precedence
- **Type System**: Static C types (int, float, void, pointer)
- **Compiler**: 550+ lines, 4-phase pipeline
- **Tests**: 600+ lines, 58 tests

### Python Service (`backend/src/compilers/python_*.py`)

- **Lexer**: 400+ lines, keyword/operator distinction
- **Parser**: 550+ lines, handles indentation
- **Type System**: Dynamic typing with inference
- **Compiler**: 500+ lines, loop/function handling
- **Tests**: 600+ lines, 58 tests

### Haskell Service (`backend/src/compilers/haskell_*.py`)

- **Lexer**: 400+ lines, indentation-based syntax
- **Parser**: 550+ lines, pattern matching
- **Type System**: Polymorphic with unification
- **Compiler**: 600+ lines, guard translation
- **Tests**: 700+ lines, 68 tests

### Planned Services (Phase 2)

**IDRIS2** (Dependent types):
- Lines: 2,500-3,000
- Tests: 70+
- Unique: Type-level computation, proof checking

**LISP** (Meta-programming):
- Lines: 1,800-2,200
- Tests: 65+
- Unique: S-expression syntax, symbol manipulation

**System F** (Polymorphic lambda calculus):
- Lines: 2,000-2,500
- Tests: 60+
- Unique: Type-level abstraction, rank polymorphism

**Java** (OOP paradigm):
- Lines: 2,200-2,800
- Tests: 70+
- Unique: Class hierarchy, method resolution

---

## Common Issues and Fixes

### Issue: "No module named 'src'"

**Cause**: Virtual environment not activated or requirements not installed

**Fix**:
```bash
source venv/bin/activate
pip install -r requirements.txt
```

### Issue: Database connection error

**Cause**: PostgreSQL not running or wrong connection URL

**Fix**:
```bash
# Check if PostgreSQL is running
pg_isready -h localhost -U postgres

# Create database if needed
createdb ancient_compute

# Check connection string in .env
DATABASE_URL=postgresql://postgres:password@localhost:5432/ancient_compute
```

### Issue: "pytest: error: unrecognized arguments"

**Cause**: pytest plugins not installed

**Fix**:
```bash
pip install pytest-asyncio pytest-cov
pytest tests/ --cov=src -v
```

---

## Integration with Frontend

### WebSocket Connection

**Frontend**: Connects to `ws://localhost:8000/ws/execute`

**Backend Handler**:
```python
@app.websocket("/ws/execute")
async def websocket_execute(websocket: WebSocket):
    await websocket.accept()
    while True:
        data = await websocket.receive_json()
        result = execute_code(data['code'], data['language'])
        await websocket.send_json(result)
```

### CORS Configuration

**Frontend URL**: http://localhost:5173
**Backend CORS Allowed**: ['http://localhost:5173', 'http://localhost:3000']

---

## References

- **FastAPI**: https://fastapi.tiangolo.com
- **SQLAlchemy**: https://docs.sqlalchemy.org
- **Alembic**: https://alembic.sqlalchemy.org
- **pytest**: https://docs.pytest.org
- **Python Type Hints**: https://docs.python.org/3/library/typing.html

---

**End of Backend Requirements**

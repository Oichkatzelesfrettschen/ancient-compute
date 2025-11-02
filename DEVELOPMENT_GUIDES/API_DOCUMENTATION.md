# Ancient Compute API Documentation

**Version**: 1.0
**Date**: November 2, 2025
**Status**: Comprehensive API Reference
**Base URL**: `https://ancient-compute.com/api/v1` (production) or `http://localhost:8000/api/v1` (development)

---

## Table of Contents

1. [Authentication](#authentication)
2. [Rate Limiting](#rate-limiting)
3. [Error Handling](#error-handling)
4. [Endpoints](#endpoints)
   - [Health & Monitoring](#health--monitoring)
   - [Authentication & Users](#authentication--users)
   - [Code Execution](#code-execution)
   - [Curriculum & Learning](#curriculum--learning)
5. [WebSocket API](#websocket-api)
6. [Language Services](#language-services)
7. [Examples](#examples)

---

## Authentication

Ancient Compute API uses JWT (JSON Web Token) based authentication for protected endpoints.

### Obtaining a Token

**POST /api/v1/auth/login**

```http
POST /api/v1/auth/login HTTP/1.1
Content-Type: application/json

{
  "username": "student@example.com",
  "password": "secure_password"
}
```

**Response**:
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "bearer",
  "expires_in": 1800
}
```

### Using Tokens

Include the token in the Authorization header for protected endpoints:

```http
GET /api/v1/user/profile HTTP/1.1
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

### Token Expiration

Access tokens expire after 30 minutes. Refresh tokens or re-authenticate to continue.

---

## Rate Limiting

API endpoints are rate-limited to prevent abuse:

| Endpoint | Limit | Window |
|----------|-------|--------|
| `/api/v1/execute` | 10 requests | 60 seconds |
| `/api/v1/compile` | 20 requests | 60 seconds |
| `/api/v1/auth/login` | 5 requests | 300 seconds |
| `/api/v1/auth/register` | 3 requests | 3600 seconds |
| Other endpoints | 100 requests | 60 seconds |

**Rate Limit Headers**:
- `X-RateLimit-Limit`: Maximum requests allowed
- `X-RateLimit-Window`: Time window in seconds
- `Retry-After`: Seconds to wait before retry (on 429 response)

**Example 429 Response**:
```json
{
  "error": "Rate limit exceeded",
  "message": "Too many requests. Maximum 10 requests per 60 seconds.",
  "retry_after": 45
}
```

---

## Error Handling

All errors follow a consistent format:

```json
{
  "error": "ErrorType",
  "message": "Human-readable error description",
  "details": {
    "field": "Additional context"
  }
}
```

### HTTP Status Codes

| Code | Meaning | When It Occurs |
|------|---------|----------------|
| 200 | Success | Request completed successfully |
| 201 | Created | Resource created successfully |
| 400 | Bad Request | Invalid request parameters |
| 401 | Unauthorized | Missing or invalid authentication |
| 403 | Forbidden | Authenticated but insufficient permissions |
| 404 | Not Found | Resource doesn't exist |
| 429 | Too Many Requests | Rate limit exceeded |
| 500 | Internal Server Error | Server-side error |
| 503 | Service Unavailable | Service temporarily down |

---

## Endpoints

### Health & Monitoring

#### GET /health
Basic health check.

**Response**:
```json
{
  "status": "healthy",
  "service": "ancient-compute-backend"
}
```

#### GET /ready
Readiness check (verifies dependencies).

**Response**:
```json
{
  "status": "ready",
  "service": "ancient-compute-backend",
  "checks": {
    "database": "ok",
    "redis": "ok"
  }
}
```

#### GET /metrics
Prometheus-style metrics (for monitoring systems).

**Response**: Prometheus text format
```
# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total{method="GET",endpoint="/api/v1/modules",status="200"} 1234
...
```

---

### Authentication & Users

#### POST /api/v1/auth/register
Register a new user account.

**Request**:
```json
{
  "username": "student123",
  "email": "student@example.com",
  "password": "secure_password_123",
  "full_name": "Jane Student"
}
```

**Response** (201 Created):
```json
{
  "id": 42,
  "username": "student123",
  "email": "student@example.com",
  "full_name": "Jane Student",
  "created_at": "2025-11-02T10:30:00Z"
}
```

#### POST /api/v1/auth/login
Authenticate and obtain access token.

**Request**:
```json
{
  "username": "student@example.com",
  "password": "secure_password_123"
}
```

**Response**:
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "bearer",
  "expires_in": 1800,
  "user": {
    "id": 42,
    "username": "student123",
    "email": "student@example.com"
  }
}
```

#### GET /api/v1/user/profile
Get current user's profile (requires authentication).

**Headers**:
```
Authorization: Bearer <token>
```

**Response**:
```json
{
  "id": 42,
  "username": "student123",
  "email": "student@example.com",
  "full_name": "Jane Student",
  "progress": {
    "modules_completed": 3,
    "lessons_completed": 27,
    "exercises_completed": 45
  },
  "created_at": "2025-11-02T10:30:00Z"
}
```

---

### Code Execution

#### POST /api/v1/execute
Execute code in specified language.

**Rate Limit**: 10 requests/minute

**Request**:
```json
{
  "code": "def factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n-1)\n\nprint(factorial(5))",
  "language": "python",
  "timeout": 10,
  "memory_limit": 256
}
```

**Parameters**:
- `code` (string, required): Source code to execute
- `language` (string, required): One of: `c`, `python`, `haskell`, `idris`, `lisp`, `systemf`, `java`, `assembly`
- `timeout` (integer, optional): Execution timeout in seconds (1-30, default: 10)
- `memory_limit` (integer, optional): Memory limit in MB (1-512, default: 256)

**Response** (200 OK):
```json
{
  "status": "success",
  "stdout": "120\n",
  "stderr": "",
  "execution_time_ms": 45,
  "memory_used_kb": 8192,
  "exit_code": 0,
  "ir_generated": true,
  "ir_instructions": 127
}
```

**Error Response** (400 Bad Request):
```json
{
  "status": "error",
  "error_type": "SyntaxError",
  "message": "Line 2: unexpected indent",
  "line": 2,
  "column": 5,
  "stdout": "",
  "stderr": "SyntaxError: invalid syntax\n"
}
```

#### POST /api/v1/compile
Compile code to Babbage IR without execution.

**Rate Limit**: 20 requests/minute

**Request**:
```json
{
  "code": "int factorial(int n) { return n <= 1 ? 1 : n * factorial(n-1); }",
  "language": "c",
  "options": {
    "emit_ir": true,
    "optimize": true
  }
}
```

**Response**:
```json
{
  "status": "success",
  "ir": "function factorial(n: int) -> int\nbasic_block entry:\n  t0 = n <= 1\n  ...",
  "ir_size": 127,
  "compilation_time_ms": 23,
  "warnings": []
}
```

#### GET /api/v1/languages/{language}/capabilities
Query language service metadata.

**Example**: GET /api/v1/languages/python/capabilities

**Response**:
```json
{
  "language": "python",
  "version": "3.12.0",
  "paradigm": "multi-paradigm (imperative, functional, OOP)",
  "type_system": "dynamic, duck-typing",
  "features": [
    "first-class functions",
    "list comprehensions",
    "generators",
    "decorators",
    "async/await"
  ],
  "compile_to_ir": true,
  "max_code_size": 10240,
  "max_execution_time": 30,
  "sandboxed": true
}
```

---

### Curriculum & Learning

#### GET /api/v1/modules
List all curriculum modules.

**Response**:
```json
{
  "modules": [
    {
      "id": 1,
      "title": "Module 0: Prehistory (20,000 BC - 3000 BC)",
      "description": "Tally marks, counting systems, early algorithms",
      "lessons_count": 8,
      "estimated_hours": 4,
      "prerequisite_modules": [],
      "languages": ["python", "c"]
    },
    {
      "id": 2,
      "title": "Module 1: Ancient Civilizations (3000 BC - 300 BC)",
      "description": "Babylonian algorithms, Egyptian mathematics, Greek logic",
      "lessons_count": 12,
      "estimated_hours": 6,
      "prerequisite_modules": [1],
      "languages": ["python", "c", "haskell"]
    }
  ]
}
```

#### GET /api/v1/modules/{module_id}
Get detailed module information.

**Response**:
```json
{
  "id": 1,
  "title": "Module 0: Prehistory",
  "description": "...",
  "lessons": [
    {
      "id": 101,
      "title": "Lesson 1: Tally Marks and Early Counting",
      "order": 1,
      "estimated_minutes": 30,
      "exercises_count": 5
    }
  ],
  "progress": {
    "completed_lessons": 3,
    "total_lessons": 8,
    "completion_percentage": 37.5
  }
}
```

#### GET /api/v1/lessons/{lesson_id}
Get lesson content and exercises.

**Response**:
```json
{
  "id": 101,
  "title": "Tally Marks and Early Counting",
  "content": "# Introduction\n\nTally marks are one of the earliest...",
  "exercises": [
    {
      "id": 1001,
      "title": "Implement tally counter",
      "description": "Write a function to count items using tally marks",
      "starter_code": "def count_tallies(marks: str) -> int:\n    pass",
      "test_cases": [
        {"input": "||||", "expected": 4},
        {"input": "|||| |||| |", "expected": 11}
      ],
      "hints": ["Count groups of 5", "Handle remainder"]
    }
  ]
}
```

#### POST /api/v1/exercises/{exercise_id}/submit
Submit exercise solution.

**Request**:
```json
{
  "code": "def count_tallies(marks: str) -> int:\n    return marks.count('|')",
  "language": "python"
}
```

**Response**:
```json
{
  "status": "passed",
  "test_results": [
    {
      "test_case": 1,
      "passed": true,
      "input": "||||",
      "expected": 4,
      "actual": 4
    },
    {
      "test_case": 2,
      "passed": true,
      "input": "|||| |||| |",
      "expected": 11,
      "actual": 11
    }
  ],
  "score": 100,
  "feedback": "Perfect! All test cases passed."
}
```

---

## WebSocket API

For real-time code execution with streaming output.

### Connection

```javascript
const ws = new WebSocket('ws://localhost:8000/ws/execute');
```

### Send Execution Request

```javascript
ws.send(JSON.stringify({
  action: 'execute',
  code: 'for i in range(10):\n    print(i)',
  language: 'python'
}));
```

### Receive Messages

**stdout event**:
```json
{
  "type": "stdout",
  "content": "0\n"
}
```

**stderr event**:
```json
{
  "type": "stderr",
  "content": "Warning: deprecated function\n"
}
```

**complete event**:
```json
{
  "type": "complete",
  "exit_code": 0,
  "execution_time_ms": 123
}
```

**error event**:
```json
{
  "type": "error",
  "error_type": "SyntaxError",
  "message": "Line 1: invalid syntax"
}
```

---

## Language Services

### Supported Languages

| Language | Version | Type System | Paradigm | Status |
|----------|---------|-------------|----------|--------|
| C | GCC 13 | Static, explicit | Imperative | ✓ Complete |
| Python | 3.12 | Dynamic | Multi-paradigm | ✓ Complete |
| Haskell | GHC 9.6 | Static, inferred | Functional | ✓ Complete |
| LISP | SBCL 2.3 | Dynamic | Functional | → Week 9 |
| IDRIS2 | Latest | Dependent types | Functional | → Week 10 |
| System F | Custom | Polymorphic | Lambda calculus | → Week 10 |
| Java | OpenJDK 21 | Static, explicit | OOP | → Week 11 |
| Assembly | x86-64 | Untyped | Low-level | ✓ Complete |

### Language-Specific Examples

See [LANGUAGE_SERVICE_ARCHITECTURE.md](../LANGUAGE_SERVICE_ARCHITECTURE.md) for comprehensive examples of factorial implementation across all 8 languages.

---

## Examples

### Python Execution

```python
import httpx

async def execute_python(code: str):
    async with httpx.AsyncClient() as client:
        response = await client.post(
            "http://localhost:8000/api/v1/execute",
            json={
                "code": code,
                "language": "python",
                "timeout": 10
            }
        )
        return response.json()

# Usage
result = await execute_python("print('Hello, Ancient Compute!')")
print(result["stdout"])  # "Hello, Ancient Compute!\n"
```

### JavaScript (Frontend)

```javascript
async function executeCode(code, language) {
  const response = await fetch('http://localhost:8000/api/v1/execute', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${localStorage.getItem('token')}`
    },
    body: JSON.stringify({
      code: code,
      language: language,
      timeout: 10
    })
  });
  
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
  }
  
  return await response.json();
}

// Usage
try {
  const result = await executeCode('print(2 + 2)', 'python');
  console.log('Output:', result.stdout);
} catch (error) {
  console.error('Execution failed:', error);
}
```

### cURL

```bash
# Execute code
curl -X POST http://localhost:8000/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{
    "code": "def greet():\n    print(\"Hello!\")\n\ngreet()",
    "language": "python"
  }'

# Login
curl -X POST http://localhost:8000/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "username": "student@example.com",
    "password": "password123"
  }'

# Get profile (authenticated)
curl -X GET http://localhost:8000/api/v1/user/profile \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
```

---

## Versioning

API versioning is managed through URL path (`/api/v1/`). Breaking changes will result in a new version (`/api/v2/`).

Current version: **v1**

---

## Support

- **Documentation**: https://ancient-compute.com/docs
- **GitHub**: https://github.com/Oichkatzelesfrettschen/ancient-compute
- **Issues**: https://github.com/Oichkatzelesfrettschen/ancient-compute/issues

---

**Last Updated**: November 2, 2025
**API Version**: 1.0

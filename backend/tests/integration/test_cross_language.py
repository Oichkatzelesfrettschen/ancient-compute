# Ancient Compute Backend - Integration Tests
"""
Cross-language integration tests validating compiler consistency.

Tests that the same algorithm compiles to similar IR across all languages,
proving computational equivalence across paradigms.
"""

import pytest
import httpx
from typing import Dict, List


@pytest.mark.integration
@pytest.mark.asyncio
class TestCrossLanguageFactorial:
    """
    Test factorial implementation across all supported languages.
    
    Validates that all languages produce:
    1. Correct output (factorial(5) = 120)
    2. Similar IR instruction count (~95-130 instructions)
    3. Successful compilation and execution
    """
    
    FACTORIAL_IMPLEMENTATIONS = {
        "c": """
#include <stdio.h>

int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    printf("%d\\n", factorial(5));
    return 0;
}
""",
        "python": """
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))
""",
        "haskell": """
factorial :: Int -> Int
factorial n = if n <= 1 then 1 else n * factorial (n - 1)

main = print (factorial 5)
""",
        # TODO: Add LISP, IDRIS2, System F, Java, Assembly when services are complete
    }
    
    @pytest.mark.parametrize("language,code", FACTORIAL_IMPLEMENTATIONS.items())
    async def test_factorial_cross_language(self, language: str, code: str, api_client: httpx.AsyncClient):
        """
        Test factorial(5) = 120 across all languages.
        
        Args:
            language: Programming language
            code: Factorial implementation in that language
            api_client: HTTP client fixture
        """
        response = await api_client.post(
            "/api/v1/execute",
            json={
                "code": code,
                "language": language,
                "timeout": 10
            }
        )
        
        assert response.status_code == 200, f"{language} execution failed: {response.text}"
        result = response.json()
        
        assert result["status"] == "success", f"{language} execution error: {result}"
        assert "120" in result["stdout"], f"{language} produced incorrect output: {result['stdout']}"
        assert result["exit_code"] == 0, f"{language} exited with code {result['exit_code']}"
    
    async def test_ir_consistency(self, api_client: httpx.AsyncClient):
        """
        Test that all languages compile to similar IR size.
        
        Validates universal IR concept: different paradigms → similar IR.
        """
        ir_sizes = {}
        
        for language, code in self.FACTORIAL_IMPLEMENTATIONS.items():
            response = await api_client.post(
                "/api/v1/compile",
                json={
                    "code": code,
                    "language": language,
                    "options": {"emit_ir": True}
                }
            )
            
            assert response.status_code == 200, f"{language} compilation failed"
            result = response.json()
            assert result["status"] == "success", f"{language} compilation error: {result}"
            
            ir_sizes[language] = result["ir_size"]
        
        # All IR sizes should be within 50% of each other (95-130 instructions)
        min_size = min(ir_sizes.values())
        max_size = max(ir_sizes.values())
        
        assert max_size / min_size < 1.5, f"IR sizes too different: {ir_sizes}"


@pytest.mark.integration
@pytest.mark.asyncio
class TestCrossLanguageFibonacci:
    """Test Fibonacci implementation across languages"""
    
    FIBONACCI_IMPLEMENTATIONS = {
        "c": """
#include <stdio.h>

int fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n-1) + fibonacci(n-2);
}

int main() {
    printf("%d\\n", fibonacci(10));
    return 0;
}
""",
        "python": """
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print(fibonacci(10))
""",
        "haskell": """
fibonacci :: Int -> Int
fibonacci n = if n <= 1 then n else fibonacci (n-1) + fibonacci (n-2)

main = print (fibonacci 10)
""",
    }
    
    @pytest.mark.parametrize("language,code", FIBONACCI_IMPLEMENTATIONS.items())
    async def test_fibonacci_cross_language(self, language: str, code: str, api_client: httpx.AsyncClient):
        """Test fibonacci(10) = 55 across all languages"""
        response = await api_client.post(
            "/api/v1/execute",
            json={
                "code": code,
                "language": language,
                "timeout": 10
            }
        )
        
        assert response.status_code == 200
        result = response.json()
        
        assert result["status"] == "success"
        assert "55" in result["stdout"], f"{language} produced incorrect output: {result['stdout']}"


@pytest.mark.integration
@pytest.mark.asyncio
class TestCompilationPipeline:
    """Test complete compilation pipeline for each language"""
    
    async def test_c_compilation_pipeline(self, api_client: httpx.AsyncClient):
        """Test C: lexer → parser → type check → codegen → IR"""
        code = "int main() { return 0; }"
        
        # Step 1: Compile to IR
        response = await api_client.post(
            "/api/v1/compile",
            json={"code": code, "language": "c", "options": {"emit_ir": True}}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "success"
        assert "ir" in result
        assert result["compilation_time_ms"] < 250  # Should be fast
    
    async def test_python_compilation_pipeline(self, api_client: httpx.AsyncClient):
        """Test Python: lexer → parser → type check → codegen → IR"""
        code = "def main():\n    return 0\n\nmain()"
        
        response = await api_client.post(
            "/api/v1/compile",
            json={"code": code, "language": "python", "options": {"emit_ir": True}}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "success"
        assert result["compilation_time_ms"] < 250
    
    async def test_haskell_compilation_pipeline(self, api_client: httpx.AsyncClient):
        """Test Haskell: lexer → parser → type inference → codegen → IR"""
        code = "main = return ()"
        
        response = await api_client.post(
            "/api/v1/compile",
            json={"code": code, "language": "haskell", "options": {"emit_ir": True}}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "success"


@pytest.mark.integration
@pytest.mark.asyncio
class TestErrorHandling:
    """Test error handling across languages"""
    
    async def test_c_syntax_error(self, api_client: httpx.AsyncClient):
        """Test C syntax error detection"""
        code = "int main() { return 0"  # Missing closing brace
        
        response = await api_client.post(
            "/api/v1/execute",
            json={"code": code, "language": "c"}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "error"
        assert result["error_type"] == "SyntaxError"
    
    async def test_python_syntax_error(self, api_client: httpx.AsyncClient):
        """Test Python syntax error detection"""
        code = "def foo(:\n    pass"  # Missing parameter
        
        response = await api_client.post(
            "/api/v1/execute",
            json={"code": code, "language": "python"}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "error"
        assert result["error_type"] in ["SyntaxError", "CompilationError"]
    
    async def test_haskell_type_error(self, api_client: httpx.AsyncClient):
        """Test Haskell type error detection"""
        code = "main = print (5 + \"hello\")"  # Type mismatch
        
        response = await api_client.post(
            "/api/v1/execute",
            json={"code": code, "language": "haskell"}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "error"
        assert result["error_type"] in ["TypeError", "CompilationError"]


@pytest.mark.integration
@pytest.mark.asyncio
class TestPerformanceBenchmarks:
    """Performance benchmarks for language services"""
    
    async def test_compilation_speed(self, api_client: httpx.AsyncClient):
        """Test that compilation completes within target time"""
        code = "int main() { return 42; }"
        
        response = await api_client.post(
            "/api/v1/compile",
            json={"code": code, "language": "c"}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["compilation_time_ms"] < 250, "Compilation too slow"
    
    async def test_execution_speed(self, api_client: httpx.AsyncClient):
        """Test that execution completes within target time"""
        code = "print(2 + 2)"
        
        response = await api_client.post(
            "/api/v1/execute",
            json={"code": code, "language": "python"}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["execution_time_ms"] < 10000, "Execution too slow"


@pytest.mark.integration
@pytest.mark.asyncio
class TestSecuritySandbox:
    """Test security sandbox isolation"""
    
    async def test_network_access_blocked(self, api_client: httpx.AsyncClient):
        """Test that network access is blocked"""
        code = """
import socket
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('google.com', 80))
"""
        
        response = await api_client.post(
            "/api/v1/execute",
            json={"code": code, "language": "python"}
        )
        
        # Should either error or timeout
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "error" or result["execution_time_ms"] >= 10000
    
    async def test_file_write_blocked(self, api_client: httpx.AsyncClient):
        """Test that filesystem writes are blocked"""
        code = """
with open('/etc/passwd', 'w') as f:
    f.write('malicious')
"""
        
        response = await api_client.post(
            "/api/v1/execute",
            json={"code": code, "language": "python"}
        )
        
        assert response.status_code == 200
        result = response.json()
        assert result["status"] == "error"
        assert "PermissionError" in result["stderr"] or "OSError" in result["stderr"]


@pytest.mark.integration
@pytest.mark.asyncio
class TestRateLimiting:
    """Test rate limiting enforcement"""
    
    async def test_execute_rate_limit(self, api_client: httpx.AsyncClient):
        """Test that /execute endpoint enforces rate limits"""
        code = "print('test')"
        
        # Make requests until rate limited
        for i in range(15):  # Limit is 10/minute
            response = await api_client.post(
                "/api/v1/execute",
                json={"code": code, "language": "python"}
            )
            
            if response.status_code == 429:
                # Rate limit triggered
                assert "Retry-After" in response.headers
                assert "rate limit" in response.json()["error"].lower()
                return
        
        pytest.fail("Rate limit not enforced after 15 requests")


# Fixtures

@pytest.fixture
async def api_client():
    """HTTP client for API testing"""
    async with httpx.AsyncClient(base_url="http://localhost:8000") as client:
        yield client


@pytest.fixture
def sample_programs() -> Dict[str, Dict[str, str]]:
    """Sample programs for testing"""
    return {
        "hello_world": {
            "c": '#include <stdio.h>\nint main() { printf("Hello\\n"); return 0; }',
            "python": 'print("Hello")',
            "haskell": 'main = putStrLn "Hello"',
        },
        "addition": {
            "c": '#include <stdio.h>\nint main() { printf("%d\\n", 2+2); return 0; }',
            "python": 'print(2 + 2)',
            "haskell": 'main = print (2 + 2)',
        },
    }

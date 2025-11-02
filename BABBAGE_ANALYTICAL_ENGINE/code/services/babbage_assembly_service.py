"""Babbage Assembly Language Service.

Provides assembly language execution for the Babbage Analytical Engine.
Takes Babbage assembly code and produces executable machine code.

Unlike other language services, assembly execution is CPU-independent and
requires no containerization - the assembler is pure Python with no
external dependencies.
"""

from __future__ import annotations

import asyncio
import time
from typing import Any

from backend.src.assembler import Assembler, AssemblyError
from backend.src.services.base_executor import ExecutionResult, ExecutionStatus


class BabbageAssemblyService:
    """Babbage Assembly language executor.

    Assembles human-readable Babbage assembly to 50-bit machine code.
    """

    def __init__(self, timeout: int = 10) -> None:
        """Initialize Babbage Assembly service.

        Args:
            timeout: Maximum execution time in seconds (for consistency with other services)
        """
        self.language = "babbage-assembly"
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute assembly code by assembling to machine code.

        Args:
            code: Babbage assembly source code
            input_data: Unused for assembly (assembly is static)

        Returns:
            ExecutionResult with status, machine code output, and errors
        """
        start_time = time.time()

        try:
            # Run assembly in thread pool to avoid blocking
            loop = asyncio.get_event_loop()
            result = await asyncio.wait_for(
                loop.run_in_executor(None, self._assemble, code),
                timeout=self.timeout,
            )

            execution_time = time.time() - start_time

            if result["error_count"] > 0:
                return ExecutionResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stdout="",
                    stderr="\n".join(result["errors"]),
                    compile_output=result["assembly_output"],
                    execution_time=execution_time,
                )

            return ExecutionResult(
                status=ExecutionStatus.SUCCESS,
                stdout=result["assembly_output"],
                stderr="",
                compile_output=result["hex_dump"],
                execution_time=execution_time,
                exit_code=0,
            )

        except asyncio.TimeoutError:
            return ExecutionResult(
                status=ExecutionStatus.TIMEOUT,
                stdout="",
                stderr=f"Assembly timeout after {self.timeout}s",
                execution_time=self.timeout,
            )

        except Exception as exc:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=f"Assembly error: {str(exc)}",
                execution_time=time.time() - start_time,
            )

    def _assemble(self, assembly_code: str) -> dict[str, Any]:
        """Assemble code (runs in thread pool).

        Args:
            assembly_code: Babbage assembly source

        Returns:
            Dictionary with assembly results
        """
        try:
            # Create assembler
            assembler = Assembler(assembly_code)
            result = assembler.assemble(verbose=False)

            # Generate output
            machine_code_str = "\n".join(
                [f"Address {i}: 0x{word:012x}" for i, word in enumerate(result.machine_code)]
            )

            return {
                "error_count": result.error_count,
                "errors": result.errors,
                "assembly_output": machine_code_str,
                "hex_dump": result.get_hex_dump(),
                "symbol_map": result.get_symbol_map(),
            }

        except AssemblyError as exc:
            return {
                "error_count": 1,
                "errors": [str(exc)],
                "assembly_output": "",
                "hex_dump": "",
                "symbol_map": "",
            }

        except Exception as exc:
            return {
                "error_count": 1,
                "errors": [f"Internal error: {str(exc)}"],
                "assembly_output": "",
                "hex_dump": "",
                "symbol_map": "",
            }


# ============================================================================
# TESTING
# ============================================================================


if __name__ == "__main__":

    async def test_assembly():
        """Test Babbage Assembly Service."""
        service = BabbageAssemblyService(timeout=10)

        # Test 1: Simple program
        print("=" * 70)
        print("Test 1: Simple Assembly Program")
        print("=" * 70)
        code1 = """
        .global main
        .text
        main:
            LOAD A, 10
            LOAD B, 5
            ADD A, B
            WRPRN A
            RET
        """
        result1 = await service.execute(code1)
        print(f"Status: {result1.status.value}")
        print(f"Output:\n{result1.stdout}")
        print(f"Execution time: {result1.execution_time:.3f}s")
        print()

        # Test 2: Loop with label
        print("=" * 70)
        print("Test 2: Loop with Labels")
        print("=" * 70)
        code2 = """
        .global sum_loop
        .text
        sum_loop:
            LOAD A, 0       # Sum = 0
            LOAD B, 1       # i = 1
        loop_start:
            CMP B, 11       # Compare i with 11
            JGT loop_end    # Exit if i > 10
            ADD A, B        # Sum += i
            ADD B, 1        # i++
            JMP loop_start  # Loop back
        loop_end:
            RET
        """
        result2 = await service.execute(code2)
        print(f"Status: {result2.status.value}")
        print(f"Output:\n{result2.stdout}")
        print(f"Execution time: {result2.execution_time:.3f}s")
        print()

        # Test 3: Error handling
        print("=" * 70)
        print("Test 3: Error Handling (Invalid Mnemonic)")
        print("=" * 70)
        code3 = """
        .global error_test
        .text
        error_test:
            ADDD A, B       # Typo: should be ADD
            RET
        """
        result3 = await service.execute(code3)
        print(f"Status: {result3.status.value}")
        print(f"Error:\n{result3.stderr}")
        print()

        # Test 4: Forward references
        print("=" * 70)
        print("Test 4: Forward Reference Resolution")
        print("=" * 70)
        code4 = """
        .global forward_ref
        .text
        forward_ref:
            JMP skip_data
            # This would be data in a real program
            ADD A, 1
        skip_data:
            LOAD A, 42
            RET
        """
        result4 = await service.execute(code4)
        print(f"Status: {result4.status.value}")
        print(f"Output:\n{result4.stdout}")
        print(f"Execution time: {result4.execution_time:.3f}s")

    # Run tests
    asyncio.run(test_assembly())

"""AE Execution Service -- full pipeline: source code -> Babbage ISA -> AE execution.

This is the critical missing link identified in the 2026-03-20 audit.
Previously, CService and PythonService compiled to machine code hex (a dead end).
This service connects the output of codegen directly to the AE Engine's run loop.

Pipeline:
  source (C or Python)
    -> compiler (CCompiler / PythonCompiler)
    -> IR (backend/src/ir_types.py)
    -> CodeGenerator (backend/src/codegen/codegen.py)
    -> assembly text
    -> Engine.load_program_from_text()
    -> Engine.run()
    -> captured output / register state

Calling convention (AE Babbage ABI):
  - First 4 arguments in registers A, B, C, D (left-to-right).
  - Arguments beyond 4 are pushed right-to-left before CALL (stack-based).
  - Return value is in register A on RET.
  - Callee-saved: none (all registers are caller-saved on the AE ISA).

Limitations (2026-03-20):
  - Only C (freestanding subset) and Python compilers reach Babbage ISA end-to-end.
  - HALT must be present in source or the engine runs until PC past end.
  - No dynamic memory allocation; no OS services.
  - Selector does not yet emit PUSH for call arguments > 4 registers (deferred).
"""

from __future__ import annotations

import time
from dataclasses import dataclass, field
from enum import StrEnum
from typing import Any


class AEExecutionStatus(StrEnum):
    SUCCESS = "success"
    COMPILE_ERROR = "compile_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"


@dataclass
class AEExecutionResult:
    status: AEExecutionStatus
    stdout: str = ""
    stderr: str = ""
    assembly_text: str = ""
    register_state: dict[str, Any] = field(default_factory=dict)
    memory_snapshot: list[int] = field(default_factory=list)
    clock_cycles: int = 0
    execution_time_s: float = 0.0


class AEExecutionService:
    """Compile source code and execute it on the Analytical Engine emulator.

    Supported languages:
      - "c"      : C freestanding subset (CCompiler -> Babbage ISA)
      - "python" : Python subset (PythonCompiler -> Babbage ISA)

    Usage:
        svc = AEExecutionService()
        result = svc.execute("int add(int a, int b) { return a + b; }", language="c")
        print(result.stdout)
    """

    _DEFAULT_STEP_LIMIT = 100_000  # Safety: stop after this many instructions

    def __init__(self, step_limit: int = _DEFAULT_STEP_LIMIT) -> None:
        self.step_limit = step_limit

    def execute(
        self,
        source: str,
        language: str = "c",
        entry_point: str | None = None,
    ) -> AEExecutionResult:
        """Compile and execute source on the AE.

        Args:
            source:      Source code string.
            language:    "c" or "python".
            entry_point: Optional function name to call after loading
                         (defaults to first defined function).

        Returns:
            AEExecutionResult with status, stdout captured from WRPRN, and
            final register/memory state.
        """
        t0 = time.monotonic()

        try:
            assembly_text, err = self._compile_to_assembly(source, language)
        except Exception as exc:
            return AEExecutionResult(
                status=AEExecutionStatus.COMPILE_ERROR,
                stderr=str(exc),
                execution_time_s=time.monotonic() - t0,
            )

        if err:
            return AEExecutionResult(
                status=AEExecutionStatus.COMPILE_ERROR,
                stderr=err,
                execution_time_s=time.monotonic() - t0,
            )

        # Prepend a CALL + HALT stub so PC=0 is the entry point call, not the
        # function prologue.  The 2-pass assembler resolves forward label refs.
        full_asm = assembly_text
        if entry_point:
            full_asm = f"_ae_main_entry:\n    CALL {entry_point}\n    HALT\n\n" + assembly_text

        try:
            result = self._run_on_engine(full_asm)
        except Exception as exc:
            return AEExecutionResult(
                status=AEExecutionStatus.RUNTIME_ERROR,
                stderr=str(exc),
                assembly_text=full_asm,
                execution_time_s=time.monotonic() - t0,
            )

        result.assembly_text = full_asm
        result.execution_time_s = time.monotonic() - t0
        return result

    # ------------------------------------------------------------------
    # Internal: compilation
    # ------------------------------------------------------------------

    def _compile_to_assembly(self, source: str, language: str) -> tuple[str, str]:
        """Compile source to Babbage assembly text.

        Returns:
            (assembly_text, error_string)  -- error_string is "" on success.
        """
        from backend.src.codegen.codegen import CodeGenerator

        codegen = CodeGenerator()

        if language == "c":
            from backend.src.compilers.c_compiler import CCompiler

            ir_program = CCompiler().compile(source)
        elif language == "python":
            from backend.src.compilers.python_compiler import PythonCompiler

            ir_program = PythonCompiler().compile(source)
        else:
            return "", f"Unsupported language: {language!r}. Choose 'c' or 'python'."

        if not ir_program.functions:
            return "", "Compilation produced no functions."

        parts: list[str] = []
        for ir_func in ir_program.functions.values():
            codegen_result = codegen.generate_function(ir_func)
            parts.append(codegen_result.get_assembly_text())

        return "\n".join(parts), ""

    # ------------------------------------------------------------------
    # Internal: engine execution
    # ------------------------------------------------------------------

    def _run_on_engine(self, assembly_text: str) -> AEExecutionResult:
        """Load assembly into AE Engine and run it, capturing all output."""
        from backend.src.emulator.analytical_engine.engine import Engine

        output_lines: list[str] = []

        def _capture(msg: str) -> None:
            output_lines.append(msg)

        engine = Engine(output_callback=_capture)
        engine.load_program_from_text(assembly_text)

        steps = 0
        engine.running = True

        while engine.running and len(engine.instruction_cards) > engine.PC:
            if steps >= self.step_limit:
                return AEExecutionResult(
                    status=AEExecutionStatus.RUNTIME_ERROR,
                    stdout="\n".join(output_lines),
                    stderr=f"Execution exceeded {self.step_limit} step limit (possible infinite loop).",
                    register_state=self._snapshot_registers(engine),
                    clock_cycles=engine.clock_time,
                )
            engine.step_one_instruction()
            steps += 1

        regs = self._snapshot_registers(engine)
        mem_snap = [int(engine.memory[i].to_decimal()) for i in range(min(64, len(engine.memory)))]

        return AEExecutionResult(
            status=AEExecutionStatus.SUCCESS,
            stdout="\n".join(output_lines),
            register_state=regs,
            memory_snapshot=mem_snap,
            clock_cycles=engine.clock_time,
        )

    @staticmethod
    def _snapshot_registers(engine: Any) -> dict[str, Any]:
        return {k: float(v.to_decimal()) for k, v in engine.registers.items()}

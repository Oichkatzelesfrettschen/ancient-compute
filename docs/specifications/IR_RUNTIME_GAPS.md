# IR Runtime Gap Closure Notes

**Date**: 2026-02-24  
**Status**: In Progress  
**Scope**: Dynamic dispatch + heap/GC primitives required by higher-level language services.

## Implemented in this cycle

1. `IndirectCall` IR instruction added in `backend/src/ir_types.py`.
2. `IRBuilder.emit_indirect_call(...)` added for compiler-facing construction.
3. Instruction selector lowering for indirect calls added in `backend/src/codegen/selector.py`.
4. Runtime heap model added in `backend/src/runtime/heap.py`:
- first-fit `malloc`
- explicit `free`
- `mark` + `sweep` cycle
5. Unit tests added:
- `backend/tests/unit/test_ir_dynamic_dispatch.py`
- `backend/tests/unit/test_heap_runtime.py`

## Interface additions

### IR type additions

```python
@dataclass
class IndirectCall(Instruction):
    function_pointer: Operand
    arguments: List[Operand]
    target: Optional[str]
```

### IRBuilder additions

```python
emit_indirect_call(function_pointer: Operand, arguments: List[Operand], target: Optional[str] = None)
```

### Runtime additions

`MemoryHeap` API:

1. `malloc(size_words: int) -> int`
2. `free(ptr: int) -> None`
3. `write(ptr: int, offset: int, value: float) -> None`
4. `read(ptr: int, offset: int) -> float`
5. `mark(ptr: int) -> None`
6. `sweep() -> None`

## Remaining integration work

1. Route closure conversion outputs (LISP/System F/Haskell lambdas) through `emit_indirect_call`.
2. Define calling convention details for pointer-based calls across all backends.
3. Connect heap allocation strategy to list/object lowering paths in compilers.
4. Add end-to-end tests proving heap-backed cons/object semantics.

## Acceptance targets

1. Indirect call lowering emits valid assembly for register and spilled-pointer cases.
2. Heap allocator passes allocation/free/reuse and bounds tests.
3. Mark-and-sweep reclaims unmarked blocks and preserves marked blocks.
4. No regressions in existing unit tests for language services and emulator cores.

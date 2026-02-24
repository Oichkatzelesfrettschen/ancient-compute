"""Simple heap manager for IR/runtime experiments.

This module models a bounded address space and supports:
- First-fit allocation (`malloc`)
- Explicit deallocation (`free`)
- Mark-and-sweep collection (`mark`, `sweep`)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List


class HeapAllocationError(RuntimeError):
    """Raised when heap allocation cannot be satisfied."""


@dataclass
class HeapBlock:
    """Allocated or free heap segment."""

    start: int
    size_words: int
    marked: bool = False

    @property
    def end(self) -> int:
        """Exclusive end address."""
        return self.start + self.size_words


class MemoryHeap:
    """Bounded heap with first-fit allocation and mark-and-sweep support."""

    def __init__(self, heap_start: int = 512, heap_end: int = 2048) -> None:
        if heap_start < 0 or heap_end <= heap_start:
            raise ValueError("Invalid heap bounds")

        self.heap_start = heap_start
        self.heap_end = heap_end
        self._cells: List[float] = [0.0] * heap_end

        self._free_blocks: List[HeapBlock] = [HeapBlock(start=heap_start, size_words=heap_end - heap_start)]
        self._allocated: Dict[int, HeapBlock] = {}

    def malloc(self, size_words: int) -> int:
        """Allocate `size_words` and return pointer to block start."""
        if size_words <= 0:
            raise ValueError("Allocation size must be positive")

        for idx, block in enumerate(self._free_blocks):
            if block.size_words < size_words:
                continue

            ptr = block.start
            self._allocated[ptr] = HeapBlock(start=ptr, size_words=size_words, marked=False)

            if block.size_words == size_words:
                self._free_blocks.pop(idx)
            else:
                self._free_blocks[idx] = HeapBlock(
                    start=block.start + size_words,
                    size_words=block.size_words - size_words,
                    marked=False,
                )
            return ptr

        raise HeapAllocationError(f"Out of heap memory for allocation size {size_words}")

    def free(self, ptr: int) -> None:
        """Free a previously allocated pointer."""
        block = self._allocated.pop(ptr, None)
        if block is None:
            raise KeyError(f"Pointer {ptr} is not allocated")

        for addr in range(block.start, block.end):
            self._cells[addr] = 0.0

        self._free_blocks.append(HeapBlock(start=block.start, size_words=block.size_words))
        self._coalesce_free_blocks()

    def write(self, ptr: int, offset: int, value: float) -> None:
        """Write value into allocated block at pointer+offset."""
        block = self._require_allocated(ptr)
        if offset < 0 or offset >= block.size_words:
            raise IndexError("Heap write out of bounds")
        self._cells[ptr + offset] = value

    def read(self, ptr: int, offset: int) -> float:
        """Read value from allocated block at pointer+offset."""
        block = self._require_allocated(ptr)
        if offset < 0 or offset >= block.size_words:
            raise IndexError("Heap read out of bounds")
        return self._cells[ptr + offset]

    def mark(self, ptr: int) -> None:
        """Mark a live block for the current GC cycle."""
        block = self._allocated.get(ptr)
        if block is not None:
            block.marked = True

    def sweep(self) -> None:
        """Collect all unmarked blocks and unmark survivors."""
        to_collect = [ptr for ptr, block in self._allocated.items() if not block.marked]

        for ptr in to_collect:
            self.free(ptr)

        # Survivors are reset for next collection cycle.
        for block in self._allocated.values():
            block.marked = False

    @property
    def allocated_count(self) -> int:
        """Number of allocated blocks."""
        return len(self._allocated)

    @property
    def used_words(self) -> int:
        """Total words currently allocated."""
        return sum(block.size_words for block in self._allocated.values())

    @property
    def free_words(self) -> int:
        """Total words currently free."""
        return sum(block.size_words for block in self._free_blocks)

    def _require_allocated(self, ptr: int) -> HeapBlock:
        block = self._allocated.get(ptr)
        if block is None:
            raise KeyError(f"Pointer {ptr} is not allocated")
        return block

    def _coalesce_free_blocks(self) -> None:
        if not self._free_blocks:
            return

        self._free_blocks.sort(key=lambda b: b.start)
        merged: List[HeapBlock] = [self._free_blocks[0]]

        for block in self._free_blocks[1:]:
            last = merged[-1]
            if last.end == block.start:
                merged[-1] = HeapBlock(start=last.start, size_words=last.size_words + block.size_words)
            else:
                merged.append(block)

        self._free_blocks = merged

"""Unit tests for runtime heap allocation and GC helpers."""

from __future__ import annotations

import pytest

from backend.src.runtime.heap import HeapAllocationError, MemoryHeap


def test_malloc_write_read_round_trip() -> None:
    heap = MemoryHeap()
    ptr = heap.malloc(4)

    heap.write(ptr, 0, 42.0)
    heap.write(ptr, 3, 9.5)

    assert heap.read(ptr, 0) == 42.0
    assert heap.read(ptr, 3) == 9.5


def test_free_reuses_space_with_first_fit() -> None:
    heap = MemoryHeap()
    ptr = heap.malloc(16)
    heap.free(ptr)

    reused = heap.malloc(8)
    assert reused == ptr


def test_malloc_rejects_invalid_size() -> None:
    heap = MemoryHeap()
    with pytest.raises(ValueError):
        heap.malloc(0)


def test_free_unknown_pointer_raises() -> None:
    heap = MemoryHeap()
    with pytest.raises(KeyError):
        heap.free(999)


def test_mark_and_sweep_reclaims_unmarked_blocks() -> None:
    heap = MemoryHeap()
    ptr_a = heap.malloc(10)
    ptr_b = heap.malloc(20)
    ptr_c = heap.malloc(5)

    heap.mark(ptr_a)
    heap.mark(ptr_c)
    heap.sweep()

    assert heap.allocated_count == 2
    assert heap.used_words == 15
    with pytest.raises(KeyError):
        heap.read(ptr_b, 0)


def test_out_of_memory_raises_allocation_error() -> None:
    heap = MemoryHeap()
    heap.malloc(heap.heap_end - heap.heap_start)
    with pytest.raises(HeapAllocationError):
        heap.malloc(1)

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


class TestMemoryHeapBasics:
    def test_initial_allocated_count_zero(self) -> None:
        assert MemoryHeap().allocated_count == 0

    def test_initial_used_words_zero(self) -> None:
        assert MemoryHeap().used_words == 0

    def test_heap_start_is_positive(self) -> None:
        heap = MemoryHeap()
        assert heap.heap_start > 0

    def test_heap_end_greater_than_start(self) -> None:
        heap = MemoryHeap()
        assert heap.heap_end > heap.heap_start

    def test_malloc_size_1_succeeds(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(1)
        assert ptr >= heap.heap_start

    def test_malloc_returns_address_in_heap_range(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(4)
        assert heap.heap_start <= ptr < heap.heap_end

    def test_allocated_count_increments(self) -> None:
        heap = MemoryHeap()
        heap.malloc(4)
        heap.malloc(8)
        assert heap.allocated_count == 2

    def test_used_words_increments(self) -> None:
        heap = MemoryHeap()
        heap.malloc(4)
        heap.malloc(8)
        assert heap.used_words == 12

    def test_negative_size_raises_value_error(self) -> None:
        with pytest.raises(ValueError):
            MemoryHeap().malloc(-1)

    def test_multiple_pointers_are_distinct(self) -> None:
        heap = MemoryHeap()
        ptrs = [heap.malloc(4) for _ in range(5)]
        assert len(set(ptrs)) == 5


class TestHeapReadWrite:
    def test_write_and_read_float(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(2)
        heap.write(ptr, 0, 3.14)
        assert heap.read(ptr, 0) == 3.14

    def test_write_and_read_zero(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(1)
        heap.write(ptr, 0, 0.0)
        assert heap.read(ptr, 0) == 0.0

    def test_out_of_bounds_write_raises_index_error(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(2)
        with pytest.raises(IndexError):
            heap.write(ptr, 10, 1.0)

    def test_write_to_multiple_slots(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(4)
        for i in range(4):
            heap.write(ptr, i, float(i * 10))
        for i in range(4):
            assert heap.read(ptr, i) == float(i * 10)


class TestHeapGarbageCollection:
    def test_free_decrements_allocated_count(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(4)
        heap.free(ptr)
        assert heap.allocated_count == 0

    def test_sweep_empty_heap_is_noop(self) -> None:
        heap = MemoryHeap()
        ptr = heap.malloc(4)
        heap.mark(ptr)
        heap.sweep()
        assert heap.allocated_count == 1

    def test_all_unmarked_blocks_reclaimed(self) -> None:
        heap = MemoryHeap()
        ptrs = [heap.malloc(4) for _ in range(5)]
        # Mark only first and last
        heap.mark(ptrs[0])
        heap.mark(ptrs[4])
        heap.sweep()
        assert heap.allocated_count == 2
        assert heap.used_words == 8

"""
Phase 3.W4: Printer and Stereotyper Unit Tests

Comprehensive test suite for output subsystems including:
- Mechanical printing to paper
- Stereotype plate generation for mass printing
- Output formatting and line management
- Mold creation, extraction, and storage
- Integrated printer-stereotyper system
- Historical accuracy and synchronization

All tests validate that output mechanisms correctly format and store
computed results from the Difference Engine No. 2.
"""

import pytest

from backend.src.emulator.printer import (
    Printer,
    PrinterSnapshot,
    PrinterState,
    PrinterStereotyperSystem,
    Stereotyper,
    StereotyperSnapshot,
)


class TestPrinterInitialization:
    """Test Printer initialization and state management."""

    def test_initialization(self):
        """Test Printer initializes with correct state."""
        printer = Printer()
        assert printer.state == PrinterState.IDLE
        assert printer.platen_position == 0
        assert len(printer.printed_lines) == 0
        assert len(printer.type_wheels) == 8
        assert all(wheel == 0 for wheel in printer.type_wheels)
        assert printer.inking_engaged is False
        assert printer.hammer_ready is False

    def test_constants_correct(self):
        """Test that printer constants are correct."""
        assert Printer.NUM_COLUMNS == 8
        assert Printer.LINES_PER_PAGE == 50
        assert Printer.DIGITS_PER_NUMBER == 8

    def test_total_operations_initialized(self):
        """Test that operations counter starts at zero."""
        printer = Printer()
        assert printer.total_operations == 0


class TestPrinterBasicOperations:
    """Test basic printing operations."""

    def test_print_single_digit(self):
        """Test printing single-digit number."""
        printer = Printer()
        output = printer.print_number(5)

        assert output == "0 0 0 0 0 0 0 5"
        assert len(printer.printed_lines) == 1
        assert printer.platen_position == 1

    def test_print_zero(self):
        """Test printing zero."""
        printer = Printer()
        output = printer.print_number(0)

        assert output == "0 0 0 0 0 0 0 0"
        assert printer.printed_lines[0] == "0 0 0 0 0 0 0 0"

    def test_print_maximum_value(self):
        """Test printing maximum 8-digit number."""
        printer = Printer()
        output = printer.print_number(99_999_999)

        assert output == "9 9 9 9 9 9 9 9"
        assert printer.printed_lines[0] == "9 9 9 9 9 9 9 9"

    def test_print_multiple_numbers(self):
        """Test printing sequence of numbers."""
        printer = Printer()
        numbers = [1, 22, 333, 4444, 55555]

        results = printer.print_multiple(numbers)

        assert len(results) == 5
        assert results[0] == "0 0 0 0 0 0 0 1"
        assert results[1] == "0 0 0 0 0 0 2 2"
        assert results[2] == "0 0 0 0 0 3 3 3"
        assert results[3] == "0 0 0 0 4 4 4 4"
        assert results[4] == "0 0 0 5 5 5 5 5"

    def test_print_operations_increment(self):
        """Test that operations counter increments."""
        printer = Printer()
        printer.print_number(42)

        # Each print_number should increment by 4 (position, inking, strike, advance)
        assert printer.total_operations >= 4


class TestPrinterTypeWheels:
    """Test type wheel positioning."""

    def test_type_wheels_update_on_print(self):
        """Test that type wheels update to match printed value."""
        printer = Printer()
        printer.print_number(12345678)

        assert printer.type_wheels == [1, 2, 3, 4, 5, 6, 7, 8]

    def test_type_wheels_zero_padded(self):
        """Test that type wheels handle zero-padding."""
        printer = Printer()
        printer.print_number(42)

        assert printer.type_wheels == [0, 0, 0, 0, 0, 0, 4, 2]

    def test_type_wheels_persist_between_prints(self):
        """Test that type wheels show current value (last print)."""
        printer = Printer()
        printer.print_number(11111111)
        printer.print_number(22222222)

        assert printer.type_wheels == [2, 2, 2, 2, 2, 2, 2, 2]


class TestPrinterLineManagement:
    """Test line and page advancement."""

    def test_advance_line(self):
        """Test advancing to next line."""
        printer = Printer()
        printer.print_number(42)
        assert printer.platen_position == 1

        printer.advance_line()
        assert printer.platen_position == 2
        assert len(printer.printed_lines) == 2

    def test_advance_page(self):
        """Test advancing to next page."""
        printer = Printer()
        printer.print_number(42)
        assert printer.platen_position == 1

        printer.advance_page()
        assert printer.platen_position == 0

    def test_page_boundaries(self):
        """Test that page respects line limit."""
        printer = Printer()

        # Fill nearly a full page
        for i in range(49):
            printer.print_number(i)

        assert printer.platen_position == 49

        # Next print should stay on page
        printer.print_number(99)
        assert printer.platen_position == 50


class TestPrinterFormatting:
    """Test output formatting."""

    def test_print_formatted_with_separator(self):
        """Test custom formatting with separators."""
        printer = Printer()
        output = printer.print_formatted(12345678, separator=",")

        assert output == "1,2,3,4,5,6,7,8"

    def test_print_formatted_default_separator(self):
        """Test default space separator."""
        printer = Printer()
        output = printer.print_formatted(12345678)

        assert output == "1 2 3 4 5 6 7 8"

    def test_output_consistency(self):
        """Test that print_number and print_formatted produce consistent output."""
        printer = Printer()
        num = 98765432

        out1 = printer.print_number(num)
        assert out1 == "9 8 7 6 5 4 3 2"


class TestPrinterOutput:
    """Test output retrieval and management."""

    def test_get_printed_output(self):
        """Test retrieving all printed output."""
        printer = Printer()
        printer.print_number(11)
        printer.print_number(22)
        printer.print_number(33)

        output = printer.get_printed_output()
        lines = output.split("\n")

        assert len(lines) == 3
        assert lines[0] == "0 0 0 0 0 0 1 1"
        assert lines[1] == "0 0 0 0 0 0 2 2"
        assert lines[2] == "0 0 0 0 0 0 3 3"

    def test_get_current_page(self):
        """Test retrieving current page."""
        printer = Printer()
        for i in range(25):
            printer.print_number(i)

        page = printer.get_current_page()
        assert len(page) == 25

    def test_get_printed_output_empty(self):
        """Test getting output from empty printer."""
        printer = Printer()
        output = printer.get_printed_output()

        assert output == ""


class TestPrinterSnapshot:
    """Test printer state snapshots."""

    def test_snapshot_validity(self):
        """Test that snapshot captures complete state."""
        printer = Printer()
        printer.print_number(42)

        snapshot = printer.get_snapshot()

        assert isinstance(snapshot, PrinterSnapshot)
        assert snapshot.platen_position == 1
        assert snapshot.state == PrinterState.ADVANCING
        assert len(snapshot.printed_lines) == 1
        assert snapshot.total_operations >= 4

    def test_snapshot_independence(self):
        """Test that snapshot is independent copy."""
        printer = Printer()
        printer.print_number(42)
        snapshot = printer.get_snapshot()

        printer.print_number(99)
        assert len(snapshot.printed_lines) == 1
        assert len(printer.printed_lines) == 2


class TestPrinterReset:
    """Test printer reset functionality."""

    def test_reset_clears_state(self):
        """Test that reset clears all state."""
        printer = Printer()
        printer.print_number(42)
        assert printer.total_operations > 0

        printer.reset()

        assert printer.state == PrinterState.IDLE
        assert printer.platen_position == 0
        assert len(printer.printed_lines) == 0
        assert printer.total_operations == 0

    def test_reset_allows_fresh_print(self):
        """Test that printing after reset works."""
        printer = Printer()
        printer.print_number(11)
        printer.reset()
        printer.print_number(22)

        assert len(printer.printed_lines) == 1
        assert printer.printed_lines[0] == "0 0 0 0 0 0 2 2"


class TestPrinterErrorHandling:
    """Test error handling in printer."""

    def test_negative_number_error(self):
        """Test that negative numbers are rejected."""
        printer = Printer()

        with pytest.raises(ValueError):
            printer.print_number(-1)

    def test_number_too_large_error(self):
        """Test that oversized numbers are rejected."""
        printer = Printer()

        with pytest.raises(ValueError):
            printer.print_number(100_000_000)


class TestStereotyperInitialization:
    """Test Stereotyper initialization."""

    def test_initialization(self):
        """Test Stereotyper initializes with empty mold."""
        stereo = Stereotyper()

        assert stereo.x_position == 0
        assert stereo.y_position == 0
        assert len(stereo.mold_image) == 0
        assert len(stereo.completed_molds) == 0
        assert stereo.total_operations == 0

    def test_constants_correct(self):
        """Test that stereotyper constants are correct."""
        assert Stereotyper.MOLD_WIDTH == 8
        assert Stereotyper.MOLD_HEIGHT == 50
        assert Stereotyper.MAX_MOLDS == 100


class TestStereotyperEngraving:
    """Test engraving digits and numbers onto molds."""

    def test_engrave_single_digit(self):
        """Test engraving a single digit."""
        stereo = Stereotyper()
        stereo.engrave_digit(0, 5)

        assert (0, 0) in stereo.mold_image
        assert stereo.total_operations >= 1

    def test_engrave_all_digits(self):
        """Test engraving all digit values 0-9."""
        stereo = Stereotyper()

        for digit in range(10):
            stereo.engrave_digit(digit % 8, digit)
            assert stereo.total_operations == digit + 1

    def test_engrave_number(self):
        """Test engraving an 8-digit number."""
        stereo = Stereotyper()
        stereo.engrave_number(12345678)

        assert stereo.y_position == 1
        assert len(stereo.mold_image) > 0

    def test_engrave_multiple(self):
        """Test engraving multiple numbers."""
        stereo = Stereotyper()
        stereo.engrave_multiple([11, 22, 33, 44, 55])

        assert stereo.y_position == 5
        assert stereo.get_completed_mold_count() == 0  # No extraction yet

    def test_engrave_advances_y_position(self):
        """Test that engraving advances to next row."""
        stereo = Stereotyper()
        stereo.engrave_number(42)
        assert stereo.y_position == 1

        stereo.engrave_number(99)
        assert stereo.y_position == 2


class TestStereotyperValidation:
    """Test validation in stereotyper."""

    def test_invalid_x_position(self):
        """Test that out-of-range X raises error."""
        stereo = Stereotyper()

        with pytest.raises(ValueError):
            stereo.engrave_digit(-1, 5)

        with pytest.raises(ValueError):
            stereo.engrave_digit(8, 5)

    def test_invalid_digit_value(self):
        """Test that invalid digits are rejected."""
        stereo = Stereotyper()

        with pytest.raises(ValueError):
            stereo.engrave_digit(0, -1)

        with pytest.raises(ValueError):
            stereo.engrave_digit(0, 10)

    def test_invalid_number(self):
        """Test that invalid numbers are rejected."""
        stereo = Stereotyper()

        with pytest.raises(ValueError):
            stereo.engrave_number(-1)

        with pytest.raises(ValueError):
            stereo.engrave_number(100_000_000)

    def test_mold_full_error(self):
        """Test that full mold raises error."""
        stereo = Stereotyper()

        # Fill mold
        for _ in range(Stereotyper.MOLD_HEIGHT):
            stereo.engrave_number(42)

        # Next engrave should fail
        with pytest.raises(ValueError):
            stereo.engrave_number(99)


class TestStereotyperMoldManagement:
    """Test mold extraction and storage."""

    def test_extract_mold(self):
        """Test extracting a completed mold."""
        stereo = Stereotyper()
        stereo.engrave_number(12345678)

        mold = stereo.extract_mold()

        assert stereo.get_completed_mold_count() == 1
        assert stereo.y_position == 0
        assert len(stereo.mold_image) == 0

    def test_multiple_molds(self):
        """Test creating and extracting multiple molds."""
        stereo = Stereotyper()

        for _ in range(3):
            stereo.engrave_number(42)
            for _ in range(49):
                stereo.engrave_number(99)
            stereo.extract_mold()

        assert stereo.get_completed_mold_count() == 3

    def test_mold_fullness(self):
        """Test mold fullness calculation."""
        stereo = Stereotyper()

        assert stereo.get_mold_fullness() == 0.0

        for i in range(25):
            stereo.engrave_number(42)

        assert stereo.get_mold_fullness() == 0.5

        for _ in range(25):
            stereo.engrave_number(99)

        assert stereo.get_mold_fullness() == 1.0

    def test_mold_is_full(self):
        """Test mold fullness check."""
        stereo = Stereotyper()
        assert stereo.mold_is_full() is False

        for _ in range(Stereotyper.MOLD_HEIGHT):
            stereo.engrave_number(42)

        assert stereo.mold_is_full() is True


class TestStereotyperGridRepresentation:
    """Test grid representation of molds."""

    def test_get_mold_as_grid(self):
        """Test converting mold to grid."""
        stereo = Stereotyper()
        stereo.engrave_number(12345678)

        grid = stereo.get_mold_as_grid()

        assert len(grid) == Stereotyper.MOLD_HEIGHT
        assert len(grid[0]) == Stereotyper.MOLD_WIDTH

    def test_grid_empty_mold(self):
        """Test grid of empty mold."""
        stereo = Stereotyper()
        grid = stereo.get_mold_as_grid()

        for row in grid:
            for cell in row:
                assert cell is False


class TestStereotyperSnapshot:
    """Test stereotyper snapshots."""

    def test_snapshot_validity(self):
        """Test that snapshot captures state."""
        stereo = Stereotyper()
        stereo.engrave_number(42)

        snapshot = stereo.get_snapshot()

        assert isinstance(snapshot, StereotyperSnapshot)
        assert snapshot.y_position == 1
        assert snapshot.molds_completed == 0


class TestStereotyperReset:
    """Test reset functionality."""

    def test_reset_current_mold(self):
        """Test resetting current mold."""
        stereo = Stereotyper()
        stereo.engrave_number(42)
        stereo.reset()

        assert stereo.y_position == 0
        assert len(stereo.mold_image) == 0

    def test_reset_preserves_completed(self):
        """Test that reset preserves completed molds."""
        stereo = Stereotyper()
        stereo.engrave_number(42)
        stereo.extract_mold()
        stereo.engrave_number(99)
        stereo.reset()

        assert stereo.get_completed_mold_count() == 1
        assert stereo.y_position == 0

    def test_clear_all(self):
        """Test clearing everything."""
        stereo = Stereotyper()
        stereo.engrave_number(42)
        stereo.extract_mold()
        stereo.engrave_number(99)
        stereo.clear_all()

        assert stereo.get_completed_mold_count() == 0
        assert stereo.y_position == 0


class TestCombinedSystem:
    """Test PrinterStereotyperSystem."""

    def test_initialization(self):
        """Test combined system initialization."""
        system = PrinterStereotyperSystem()

        assert isinstance(system.printer, Printer)
        assert isinstance(system.stereotyper, Stereotyper)
        assert system.total_operations == 0

    def test_output_to_both(self):
        """Test outputting to both printer and stereotyper."""
        system = PrinterStereotyperSystem()
        output = system.output_number(42)

        assert output == "0 0 0 0 0 0 4 2"
        assert len(system.printer.printed_lines) == 1
        assert system.stereotyper.y_position == 1

    def test_output_printer_only(self):
        """Test outputting to printer only."""
        system = PrinterStereotyperSystem()
        system.output_number(42, to_printer=True, to_stereotyper=False)

        assert len(system.printer.printed_lines) == 1
        assert system.stereotyper.y_position == 0

    def test_output_stereotyper_only(self):
        """Test outputting to stereotyper only."""
        system = PrinterStereotyperSystem()
        system.output_number(42, to_printer=False, to_stereotyper=True)

        assert len(system.printer.printed_lines) == 0
        assert system.stereotyper.y_position == 1

    def test_output_sequence(self):
        """Test outputting sequence."""
        system = PrinterStereotyperSystem()
        results = system.output_sequence([11, 22, 33])

        assert len(results) == 3
        assert len(system.printer.printed_lines) == 3

    def test_system_snapshot(self):
        """Test combined system snapshot."""
        system = PrinterStereotyperSystem()
        system.output_number(42)

        snapshot = system.get_snapshot()

        assert "printer" in snapshot
        assert "stereotyper" in snapshot
        assert snapshot["total_operations"] >= 1

    def test_system_reset(self):
        """Test resetting combined system."""
        system = PrinterStereotyperSystem()
        system.output_number(42)
        system.reset()

        assert len(system.printer.printed_lines) == 0
        assert system.stereotyper.y_position == 0
        assert system.total_operations == 0

    def test_get_printed_output(self):
        """Test retrieving printed output."""
        system = PrinterStereotyperSystem()
        system.output_sequence([11, 22, 33])

        output = system.get_printed_output()
        assert "1 1" in output
        assert "2 2" in output
        assert "3 3" in output

    def test_get_mold_count(self):
        """Test mold counting."""
        system = PrinterStereotyperSystem()

        assert system.get_mold_count() == 0

        # Fill a mold (system auto-extracts when full)
        for _ in range(Stereotyper.MOLD_HEIGHT):
            system.output_number(42)

        # Output one more to trigger auto-extraction
        system.output_number(99)
        assert system.get_mold_count() == 1


class TestEdgeCasesAndIntegration:
    """Edge cases and integration tests."""

    def test_printer_large_sequence(self):
        """Test printing many numbers."""
        printer = Printer()

        for i in range(100):
            printer.print_number(i % 100_000_000)

        assert len(printer.printed_lines) == 100

    def test_stereotyper_auto_mold_extraction(self):
        """Test automatic mold extraction in system."""
        system = PrinterStereotyperSystem()

        # Output more than one mold
        for _ in range(Stereotyper.MOLD_HEIGHT + 10):
            system.output_number(42)

        assert system.get_mold_count() == 1
        assert system.stereotyper.y_position == 10

    def test_polynomial_output_pipeline(self):
        """Test complete polynomial output pipeline."""
        system = PrinterStereotyperSystem()

        # Polynomial: f(x) = x^2 + x + 1 for x in 1..5
        results = [3, 7, 13, 21, 31]  # Expected values

        system.output_sequence(results)

        output = system.get_printed_output()
        # Check for formatted digits in output (spaces separate digits)
        assert "0 0 0 0 0 0 0 3" in output  # f(1) = 3
        assert "0 0 0 0 0 0 3 1" in output  # f(5) = 31

    def test_multiple_systems_independent(self):
        """Test that multiple systems don't interfere."""
        system1 = PrinterStereotyperSystem()
        system2 = PrinterStereotyperSystem()

        system1.output_number(42)
        system2.output_number(99)

        assert len(system1.printer.printed_lines) == 1
        assert len(system2.printer.printed_lines) == 1
        assert system1.printer.printed_lines[0] != system2.printer.printed_lines[0]

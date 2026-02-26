"""Tests for the printer formatter.

Verifies output formatting matches PRINTER_SPEC.md specifications.
"""

import pytest

from hardware_twin.printer_formatter import (
    format_de_table,
    format_page,
    format_row,
    format_value,
)


class TestFormatValue:
    def test_positive_zero(self):
        assert format_value(0, width=5) == "+00000"

    def test_positive_number(self):
        assert format_value(42, width=5) == "+00042"

    def test_negative_number(self):
        assert format_value(-7, width=5) == "-00007"

    def test_default_width(self):
        result = format_value(1)
        assert len(result) == 32  # sign + 31 digits

    def test_unsigned(self):
        result = format_value(42, width=5, signed=False)
        assert result == "00042"


class TestFormatRow:
    def test_single_column(self):
        row = format_row(1, [42], num_columns=1, digits_per_column=5)
        assert "001" in row
        assert "+00042" in row

    def test_pads_missing_columns(self):
        row = format_row(1, [1], num_columns=3, digits_per_column=3)
        assert "+001" in row
        assert "+000" in row


class TestFormatPage:
    def test_page_has_header(self):
        page = format_page([[1, 2]], header="Test Page", num_columns=2, digits_per_column=5)
        assert "Test Page" in page

    def test_page_has_rows(self):
        rows = [[i * i] for i in range(3)]
        page = format_page(rows, num_columns=1, digits_per_column=5)
        lines = page.splitlines()
        # Should have column header, separator, and 3 data rows
        assert len(lines) >= 5


class TestFormatDETable:
    def test_formats_tabulator_output(self):
        from hardware_twin.de_tabulator import tabulate_polynomial
        states = tabulate_polynomial([0, 0, 1], num_rows=3, num_columns=4)
        output = format_de_table(states, title="x^2 table")
        assert "x^2 table" in output
        # D0 values should appear: 0, 1, 4, 9
        assert "+0000000000000000000000000000000" in output  # 0
        assert "+0000000000000000000000000000001" in output  # 1

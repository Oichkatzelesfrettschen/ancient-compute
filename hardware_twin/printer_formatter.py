"""Printer formatter: column values -> formatted print output.

Emulates the DE2 type-wheel printer output format, converting
column digit values into the printed page format specified in
PRINTER_SPEC.md.

References:
  SOURCE:SMG-DE2-MANUAL -- Print format
"""

from __future__ import annotations

from typing import List, Optional


def format_value(value: int, width: int = 31, signed: bool = True) -> str:
    """Format a single column value as it would appear on the printed page.

    Args:
        value: Integer value to format
        width: Number of digit positions (DE2: 31)
        signed: Whether to include sign prefix

    Returns:
        Formatted string, e.g. "+0000000000000000000000000000001"
    """
    sign = "+" if value >= 0 else "-"
    digits = str(abs(value)).zfill(width)[-width:]
    if signed:
        return f"{sign}{digits}"
    return digits


def format_row(
    row_number: int,
    column_values: List[int],
    num_columns: int = 8,
    digits_per_column: int = 31,
) -> str:
    """Format one printed row (all columns).

    Args:
        row_number: Row number (1-based)
        column_values: List of integer values, one per column
        num_columns: Number of columns to print
        digits_per_column: Digits per column

    Returns:
        Formatted row string
    """
    cols = list(column_values) + [0] * (num_columns - len(column_values))
    formatted_cols = [format_value(v, digits_per_column) for v in cols[:num_columns]]
    return f"  {row_number:03d}   {'  '.join(formatted_cols)}"


def format_page(
    rows: List[List[int]],
    header: Optional[str] = None,
    num_columns: int = 8,
    digits_per_column: int = 31,
) -> str:
    """Format a full printed page.

    Args:
        rows: List of rows, each row is a list of column values
        header: Optional page header
        num_columns: Number of columns
        digits_per_column: Digits per column

    Returns:
        Complete page text
    """
    lines = []

    if header:
        lines.append(header)
        lines.append("")

    # Column header
    col_headers = "  ".join(f"{'Col' + str(i):>{digits_per_column + 1}s}"
                            for i in range(num_columns))
    lines.append(f"  Row   {col_headers}")
    lines.append("  " + "-" * (6 + (digits_per_column + 3) * num_columns))

    for i, row in enumerate(rows):
        lines.append(format_row(i + 1, row, num_columns, digits_per_column))

    return "\n".join(lines)


def format_de_table(
    tabulator_states: list,
    title: str = "Difference Engine Tabulation",
) -> str:
    """Format DE tabulator output as a printed page.

    Accepts output from de_tabulator.tabulate() or tabulate_polynomial().

    Args:
        tabulator_states: List of TabulatorState from de_tabulator
        title: Page title

    Returns:
        Formatted page text
    """
    rows = [state.differences for state in tabulator_states]
    num_cols = len(rows[0]) if rows else 8
    return format_page(rows, header=title, num_columns=num_cols, digits_per_column=31)

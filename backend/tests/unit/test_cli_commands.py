"""Unit tests for the ancient-compute CLI commands.

Uses Click's CliRunner for lightweight in-process invocation -- no
subprocess or filesystem side effects except for temp files.
"""

import os

import pytest
from click.testing import CliRunner

from backend.src.emulator.cli.app import cli


@pytest.fixture
def runner():
    return CliRunner()


@pytest.fixture
def basm_file(tmp_path):
    """A minimal valid .basm program."""
    src = tmp_path / "test.basm"
    src.write_text("LOAD A 5\nLOAD B 3\nADD A B\n")
    return str(src)


@pytest.fixture
def basm_nop(tmp_path):
    """A NOP-only program."""
    src = tmp_path / "nop.basm"
    src.write_text("NOP\n")
    return str(src)


# ---------------------------------------------------------------------------
# --help / version
# ---------------------------------------------------------------------------


def test_cli_help(runner):
    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0
    assert "ancient-compute" in result.output.lower() or "Babbage" in result.output


def test_cli_version(runner):
    result = runner.invoke(cli, ["--version"])
    assert result.exit_code == 0
    assert "0.1.0" in result.output


def test_run_help(runner):
    result = runner.invoke(cli, ["run", "--help"])
    assert result.exit_code == 0
    assert "--trace" in result.output
    assert "--dump" in result.output
    assert "--physics" in result.output


def test_assemble_help(runner):
    result = runner.invoke(cli, ["assemble", "--help"])
    assert result.exit_code == 0
    assert "--format" in result.output


def test_trace_help(runner):
    result = runner.invoke(cli, ["trace", "--help"])
    assert result.exit_code == 0


def test_deck_help(runner):
    result = runner.invoke(cli, ["deck", "--help"])
    assert result.exit_code == 0
    assert "--note" in result.output
    assert "--physics" in result.output


# ---------------------------------------------------------------------------
# assemble command
# ---------------------------------------------------------------------------


def test_assemble_text_output(runner, basm_file):
    result = runner.invoke(cli, ["assemble", basm_file])
    assert result.exit_code == 0
    assert "LOAD" in result.output
    assert "ADD" in result.output


def test_assemble_json_output(runner, basm_file):
    result = runner.invoke(cli, ["assemble", basm_file, "--format", "json"])
    assert result.exit_code == 0
    import json

    data = json.loads(result.output)
    assert isinstance(data, list)
    assert len(data) == 3


def test_assemble_output_file(runner, basm_file, tmp_path):
    out = str(tmp_path / "out.txt")
    result = runner.invoke(cli, ["assemble", basm_file, "-o", out])
    assert result.exit_code == 0
    assert os.path.exists(out)
    with open(out) as fh:
        content = fh.read()
    assert "LOAD" in content


def test_assemble_nonexistent_file(runner):
    result = runner.invoke(cli, ["assemble", "/nonexistent/path.basm"])
    assert result.exit_code != 0


# ---------------------------------------------------------------------------
# run command
# ---------------------------------------------------------------------------


def test_run_basic(runner, basm_file):
    result = runner.invoke(cli, ["run", basm_file])
    assert result.exit_code == 0
    assert "Completed" in result.output or "Running" in result.output


def test_run_dump(runner, basm_file):
    result = runner.invoke(cli, ["run", basm_file, "--dump"])
    assert result.exit_code == 0


def test_run_trace_table(runner, basm_file):
    result = runner.invoke(cli, ["run", basm_file, "--trace"])
    assert result.exit_code == 0


def test_run_trace_json(runner, basm_file):
    result = runner.invoke(cli, ["run", basm_file, "--trace", "--trace-format", "json"])
    assert result.exit_code == 0


def test_run_physics_flag(runner, basm_nop):
    result = runner.invoke(cli, ["run", basm_nop, "--physics"])
    assert result.exit_code == 0
    # Physics coupling message should appear
    assert "Physics" in result.output or "physics" in result.output


def test_run_nonexistent_file(runner):
    result = runner.invoke(cli, ["run", "/nonexistent.basm"])
    assert result.exit_code != 0


# ---------------------------------------------------------------------------
# trace command
# ---------------------------------------------------------------------------


def test_trace_table(runner, basm_file):
    result = runner.invoke(cli, ["trace", basm_file])
    assert result.exit_code == 0


def test_trace_json(runner, basm_file):
    result = runner.invoke(cli, ["trace", basm_file, "--format", "json"])
    assert result.exit_code == 0


def test_trace_output_file(runner, basm_file, tmp_path):
    out = str(tmp_path / "trace.json")
    result = runner.invoke(cli, ["trace", basm_file, "--format", "json", "-o", out])
    assert result.exit_code == 0
    assert os.path.exists(out)


# ---------------------------------------------------------------------------
# deck command
# ---------------------------------------------------------------------------


def test_deck_note_g(runner):
    result = runner.invoke(cli, ["deck", "--note", "g", "--n", "2"])
    assert result.exit_code == 0
    # Should produce Bernoulli numbers
    assert "B_" in result.output


def test_deck_note_g_physics_deferred_message(runner):
    result = runner.invoke(cli, ["deck", "--note", "g", "--n", "1", "--physics"])
    assert result.exit_code == 0
    # Should note that deck physics is deferred
    assert "physics" in result.output.lower() or "deferred" in result.output.lower()


class TestCLIHelpAndSubcommands:
    """CLI --help output exposes all subcommands and key flags."""

    def test_help_mentions_run(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert result.exit_code == 0
        assert "run" in result.output

    def test_help_mentions_assemble(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "assemble" in result.output

    def test_help_mentions_trace(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "trace" in result.output

    def test_help_mentions_deck(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "deck" in result.output

    def test_version_contains_dot_notation(self) -> None:
        result = CliRunner().invoke(cli, ["--version"])
        assert "." in result.output

    def test_run_help_mentions_trace_flag(self) -> None:
        result = CliRunner().invoke(cli, ["run", "--help"])
        assert "--trace" in result.output

    def test_run_help_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["run", "--help"])
        assert result.exit_code == 0


class TestCLIDeckExtended:
    """Extended deck command output tests."""

    def test_deck_n1_outputs_b_value(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "1"])
        assert result.exit_code == 0
        assert "B_" in result.output

    def test_deck_n3_outputs_three_b_values(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "3"])
        assert result.exit_code == 0
        assert result.output.count("B_") >= 3

    def test_deck_n5_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "5"])
        assert result.exit_code == 0

    def test_deck_n4_more_output_than_n1(self) -> None:
        r1 = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "1"])
        r4 = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "4"])
        assert len(r4.output) >= len(r1.output)

    def test_deck_output_contains_fraction_slash(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "2"])
        # Bernoulli numbers are fractions like 1/6, -1/30
        assert "/" in result.output

    def test_deck_physics_flag_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "1", "--physics"])
        assert result.exit_code == 0

    def test_deck_n2_output_non_empty(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "2"])
        assert len(result.output.strip()) > 0


class TestCliVersionAndHelp:
    """Version, help, and subcommand presence."""

    def test_version_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["--version"])
        assert result.exit_code == 0

    def test_version_output_non_empty(self) -> None:
        result = CliRunner().invoke(cli, ["--version"])
        assert len(result.output.strip()) > 0

    def test_help_lists_run_command(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "run" in result.output

    def test_help_lists_deck_command(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "deck" in result.output

    def test_help_lists_assemble_command(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "assemble" in result.output

    def test_help_lists_trace_command(self) -> None:
        result = CliRunner().invoke(cli, ["--help"])
        assert "trace" in result.output

    def test_run_help_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["run", "--help"])
        assert result.exit_code == 0

    def test_deck_help_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--help"])
        assert result.exit_code == 0

    def test_assemble_help_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["assemble", "--help"])
        assert result.exit_code == 0


class TestDeckCommandNotes:
    """Deck command with different Lovelace notes."""

    def test_deck_note_b_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "b"])
        assert result.exit_code == 0

    def test_deck_note_c_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "c"])
        assert result.exit_code == 0

    def test_deck_note_d_exits_zero(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "d"])
        assert result.exit_code == 0

    def test_deck_g_n1_output_non_empty(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "1"])
        assert result.exit_code == 0
        assert len(result.output.strip()) > 0

    def test_deck_g_n2_output_has_two_bernoulli(self) -> None:
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "2"])
        assert result.exit_code == 0
        assert result.output.count("B_") >= 2


class TestCliDeckCommand:
    """CLI deck command produces non-empty output."""

    def test_deck_g_n3_exits_zero(self) -> None:
        from click.testing import CliRunner

        from backend.src.emulator.cli.app import cli
        result = CliRunner().invoke(cli, ["deck", "--note", "g", "--n", "3"])
        assert result.exit_code == 0

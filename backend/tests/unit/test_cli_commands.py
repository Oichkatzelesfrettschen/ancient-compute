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

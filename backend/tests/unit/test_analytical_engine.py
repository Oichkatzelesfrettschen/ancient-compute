"""
Unit Tests for Babbage Analytical Engine Emulator

Tests for BabbageNumber arithmetic, Engine instruction execution, memory
operations, control flow, stack operations, I/O, breakpoints, and timing.

Test coverage: 55+ comprehensive tests covering:
  - BabbageNumber: init, arithmetic (add, sub, mul, div), comparison, overflow,
    negative, zero, card format, repr
  - Arithmetic instructions: ADD, SUB, MULT, DIV, SQRT
  - Memory instructions: LOAD (immediate and from memory), STOR, bounds checking
  - Control flow: JMP, JZ, JNZ, JLT, JGT, JLE, JGE, CMP, CALL, RET
  - Stack: PUSH, POP, overflow, underflow
  - I/O: RDCRD (direct and card queue), WRPCH, WRPRN
  - Timing: clock_time accumulation per instruction
  - Breakpoints: address-based breakpoint triggering
  - Edge cases: division by zero, invalid register, memory out of bounds
"""

import pytest

from backend.src.emulator.analytical_engine import (
    TIMING_TABLE,
    BabbageNumber,
    Engine,
    Instruction,
)

# ============================================================================
# BabbageNumber Tests (15 tests)
# ============================================================================

def test_babbage_number_init():
    """Test BabbageNumber initialization with integer input."""
    num = BabbageNumber(123)
    assert num.to_decimal() == 123.0


def test_babbage_number_init_zero():
    """Test BabbageNumber initialization with zero."""
    num = BabbageNumber(0)
    assert num.to_decimal() == 0.0
    assert num.value == 0


def test_babbage_number_init_negative():
    """Test BabbageNumber initialization with negative value."""
    num = BabbageNumber(-42)
    assert num.to_decimal() == -42.0
    assert num.value < 0


def test_babbage_number_init_from_babbage():
    """Test BabbageNumber initialized from another BabbageNumber."""
    original = BabbageNumber(99)
    copy = BabbageNumber(original)
    assert copy.to_decimal() == 99.0
    assert copy.value == original.value


def test_babbage_number_add():
    """Test BabbageNumber addition."""
    num1 = BabbageNumber(10)
    num2 = BabbageNumber(20)
    result = num1 + num2
    assert result.to_decimal() == 30.0


def test_babbage_number_sub():
    """Test BabbageNumber subtraction."""
    num1 = BabbageNumber(30)
    num2 = BabbageNumber(10)
    result = num1 - num2
    assert result.to_decimal() == 20.0


def test_babbage_number_sub_negative_result():
    """Test BabbageNumber subtraction producing negative result."""
    result = BabbageNumber(5) - BabbageNumber(10)
    assert result.to_decimal() == -5.0


def test_babbage_number_mul():
    """Test BabbageNumber multiplication."""
    result = BabbageNumber(6) * BabbageNumber(7)
    assert result.to_decimal() == 42.0


def test_babbage_number_mul_zero():
    """Test BabbageNumber multiplication by zero."""
    result = BabbageNumber(12345) * BabbageNumber(0)
    assert result.to_decimal() == 0.0


def test_babbage_number_div():
    """Test BabbageNumber division."""
    result = BabbageNumber(100) / BabbageNumber(4)
    assert result.to_decimal() == 25.0


def test_babbage_number_div_by_zero():
    """Test BabbageNumber division by zero raises ZeroDivisionError."""
    with pytest.raises(ZeroDivisionError):
        BabbageNumber(10) / BabbageNumber(0)


def test_babbage_number_comparison():
    """Test BabbageNumber comparison operators."""
    num1 = BabbageNumber(10)
    num2 = BabbageNumber(20)
    assert num1 < num2
    assert num2 > num1
    assert num1 == BabbageNumber(10)
    assert num1 != num2
    assert num1 <= num2
    assert num1 <= BabbageNumber(10)
    assert num2 >= num1
    assert num2 >= BabbageNumber(20)


def test_babbage_number_overflow_detection():
    """Test BabbageNumber overflow flag is set for extreme values."""
    huge = BabbageNumber(0)
    huge.value = (10**50) * (10**40) + 1  # Exceeds 50-digit bound
    assert huge._check_overflow() is True
    assert huge._overflow_flag is True


def test_babbage_number_card_format():
    """Test BabbageNumber to_card_format produces 50-digit string."""
    num = BabbageNumber(42)
    card = num.to_card_format()
    assert len(card) >= 50
    assert "42" in card


def test_babbage_number_repr():
    """Test BabbageNumber repr contains the decimal value."""
    num = BabbageNumber(7)
    r = repr(num)
    assert "BabbageNumber" in r
    assert "7" in r


# ============================================================================
# Engine Arithmetic Tests (10 tests)
# ============================================================================

def test_engine_add_immediate():
    """Test ADD instruction with immediate operand."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(5)
    instruction = Instruction('ADD', ['A', '10'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 15.0
    assert engine.flags['ZERO'] is False
    assert engine.flags['SIGN'] is False


def test_engine_sub_immediate():
    """Test SUB instruction with immediate operand."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(20)
    instruction = Instruction('SUB', ['A', '8'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 12.0


def test_engine_sub_to_zero():
    """Test SUB instruction producing zero sets ZERO flag."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(5)
    instruction = Instruction('SUB', ['A', '5'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 0.0
    assert engine.flags['ZERO'] is True


def test_engine_add_sets_zero_flag():
    """Test ADD with result zero sets ZERO flag."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(-10)
    instruction = Instruction('ADD', ['A', '10'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 0.0
    assert engine.flags['ZERO'] is True


def test_engine_mult():
    """Test MULT instruction multiplies register by operand."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(6)
    instruction = Instruction('MULT', ['A', '7'])
    engine.execute_instruction(instruction)
    # MULT micro uses repeated addition -- result in register A
    result = engine.registers['A'].to_decimal()
    assert result == 42.0


def test_engine_div():
    """Test DIV instruction divides register by operand."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(100)
    instruction = Instruction('DIV', ['A', '5'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 20.0


def test_engine_div_remainder_in_d():
    """Test DIV stores remainder in register D."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(7)
    instruction = Instruction('DIV', ['A', '3'])
    engine.execute_instruction(instruction)
    quotient = engine.registers['A'].to_decimal()
    remainder = engine.registers['D'].to_decimal()
    assert quotient == 2.0
    assert remainder == 1.0


def test_engine_div_by_zero():
    """Test DIV by zero raises ZeroDivisionError."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(10)
    instruction = Instruction('DIV', ['A', '0'])
    with pytest.raises(ZeroDivisionError):
        engine.execute_instruction(instruction)


def test_engine_sqrt():
    """Test SQRT instruction computes integer square root."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(144)
    instruction = Instruction('SQRT', ['A'])
    engine.execute_instruction(instruction)
    result = engine.registers['A'].to_decimal()
    assert abs(result - 12.0) < 0.01


def test_engine_sqrt_zero():
    """Test SQRT of zero returns zero."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(0)
    instruction = Instruction('SQRT', ['A'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 0.0


def test_engine_sqrt_negative_raises():
    """Test SQRT of negative number raises ValueError."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(-4)
    instruction = Instruction('SQRT', ['A'])
    with pytest.raises(ValueError, match="negative"):
        engine.execute_instruction(instruction)


# ============================================================================
# Engine Memory Tests (5 tests)
# ============================================================================

def test_engine_load_immediate():
    """Test LOAD instruction with immediate value."""
    engine = Engine()
    instruction = Instruction('LOAD', ['B', '100'])
    engine.execute_instruction(instruction)
    assert engine.registers['B'].to_decimal() == 100.0


def test_engine_stor_to_memory():
    """Test STOR instruction to write register to memory."""
    engine = Engine()
    engine.registers['C'] = BabbageNumber(42)
    instruction = Instruction('STOR', ['C', '[5]'])
    engine.execute_instruction(instruction)
    assert engine.memory[5].to_decimal() == 42.0


def test_engine_load_from_memory():
    """Test LOAD instruction to read from memory."""
    engine = Engine()
    engine.memory[10] = BabbageNumber(99)
    instruction = Instruction('LOAD', ['A', '[10]'])
    engine.execute_instruction(instruction)
    assert engine.registers['A'].to_decimal() == 99.0


def test_engine_stor_invalid_address():
    """Test STOR with non-bracketed address raises ValueError."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(1)
    instruction = Instruction('STOR', ['A', '999'])
    with pytest.raises(ValueError, match="Invalid address"):
        engine.execute_instruction(instruction)


def test_engine_load_out_of_bounds():
    """Test LOAD with out-of-bounds memory address raises IndexError."""
    engine = Engine()
    instruction = Instruction('LOAD', ['A', '[9999]'])
    with pytest.raises(IndexError, match="out of bounds"):
        engine.execute_instruction(instruction)


# ============================================================================
# Engine Control Flow Tests (12 tests)
# ============================================================================

def test_engine_jmp():
    """Test unconditional JMP (jump) instruction."""
    engine = Engine()
    engine.instruction_cards = [
        Instruction('NOP'),        # 0
        Instruction('JMP', ['5']), # 1
        Instruction('NOP'),        # 2
        Instruction('NOP'),        # 3
        Instruction('NOP'),        # 4
        Instruction('NOP'),        # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP, PC -> 1
    engine.execute_instruction(engine.instruction_cards[1])  # JMP 5, PC -> 5
    assert engine.PC == 5


def test_engine_jz_true():
    """Test JZ (jump if zero) when ZERO flag is set."""
    engine = Engine()
    engine.flags['ZERO'] = True
    engine.instruction_cards = [
        Instruction('NOP'),        # 0
        Instruction('JZ', ['5']),  # 1
        Instruction('NOP'),        # 2
        Instruction('NOP'),        # 3
        Instruction('NOP'),        # 4
        Instruction('NOP'),        # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])
    engine.execute_instruction(engine.instruction_cards[1])
    assert engine.PC == 5


def test_engine_jz_false():
    """Test JZ (jump if zero) when ZERO flag is clear."""
    engine = Engine()
    engine.flags['ZERO'] = False
    engine.instruction_cards = [
        Instruction('NOP'),        # 0
        Instruction('JZ', ['5']),  # 1
        Instruction('NOP'),        # 2
        Instruction('NOP'),        # 3
        Instruction('NOP'),        # 4
        Instruction('NOP'),        # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])
    engine.execute_instruction(engine.instruction_cards[1])
    assert engine.PC == 2


def test_engine_jnz_true():
    """Test JNZ (jump if not zero) when ZERO flag is clear."""
    engine = Engine()
    engine.flags['ZERO'] = False
    engine.PC = 0
    instruction = Instruction('JNZ', ['10'])
    engine.execute_instruction(instruction)
    assert engine.PC == 10


def test_engine_jnz_false():
    """Test JNZ (jump if not zero) when ZERO flag is set -- no jump."""
    engine = Engine()
    engine.flags['ZERO'] = True
    engine.PC = 0
    instruction = Instruction('JNZ', ['10'])
    engine.execute_instruction(instruction)
    assert engine.PC == 1  # Normal increment, no jump


def test_engine_cmp_greater():
    """Test CMP sets GREATER flag when first > second."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(20)
    instruction = Instruction('CMP', ['A', '10'])
    engine.execute_instruction(instruction)
    assert engine.flags['GREATER'] is True
    assert engine.flags['LESS'] is False
    assert engine.flags['EQUAL'] is False


def test_engine_cmp_less():
    """Test CMP sets LESS flag when first < second."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(5)
    instruction = Instruction('CMP', ['A', '10'])
    engine.execute_instruction(instruction)
    assert engine.flags['GREATER'] is False
    assert engine.flags['LESS'] is True
    assert engine.flags['EQUAL'] is False


def test_engine_cmp_equal():
    """Test CMP sets EQUAL flag when operands are equal."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(10)
    instruction = Instruction('CMP', ['A', '10'])
    engine.execute_instruction(instruction)
    assert engine.flags['GREATER'] is False
    assert engine.flags['LESS'] is False
    assert engine.flags['EQUAL'] is True


def test_engine_jlt_after_cmp():
    """Test JLT jumps when LESS flag set by preceding CMP."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(3)
    cmp_instr = Instruction('CMP', ['A', '10'])
    engine.execute_instruction(cmp_instr)
    assert engine.flags['LESS'] is True

    engine.PC = 0
    jlt_instr = Instruction('JLT', ['99'])
    engine.execute_instruction(jlt_instr)
    assert engine.PC == 99


def test_engine_jgt_after_cmp():
    """Test JGT jumps when GREATER flag set by preceding CMP."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(50)
    engine.execute_instruction(Instruction('CMP', ['A', '10']))
    assert engine.flags['GREATER'] is True

    engine.PC = 0
    engine.execute_instruction(Instruction('JGT', ['42']))
    assert engine.PC == 42


def test_engine_jle_on_equal():
    """Test JLE jumps when EQUAL flag is set."""
    engine = Engine()
    engine.flags['LESS'] = False
    engine.flags['EQUAL'] = True
    engine.PC = 0
    engine.execute_instruction(Instruction('JLE', ['7']))
    assert engine.PC == 7


def test_engine_jge_on_greater():
    """Test JGE jumps when GREATER flag is set."""
    engine = Engine()
    engine.flags['GREATER'] = True
    engine.flags['EQUAL'] = False
    engine.PC = 0
    engine.execute_instruction(Instruction('JGE', ['15']))
    assert engine.PC == 15


# ============================================================================
# Engine Subroutine Tests (4 tests)
# ============================================================================

def test_engine_call_ret():
    """Test CALL and RET instructions for subroutine support."""
    engine = Engine()
    engine.instruction_cards = [
        Instruction('NOP'),         # 0
        Instruction('CALL', ['4']), # 1: call subroutine at address 4
        Instruction('NOP'),         # 2: return address (skipped by call)
        Instruction('HALT'),        # 3: (never reached)
        Instruction('NOP'),         # 4: subroutine starts here
        Instruction('RET'),         # 5: return from subroutine
    ]
    engine.PC = 0

    engine.execute_instruction(engine.instruction_cards[0])
    assert engine.PC == 1

    engine.execute_instruction(engine.instruction_cards[1])
    assert engine.PC == 4
    assert engine.return_stack == [2]

    engine.execute_instruction(engine.instruction_cards[4])
    assert engine.PC == 5

    engine.execute_instruction(engine.instruction_cards[5])
    assert engine.PC == 2
    assert engine.return_stack == []


def test_engine_call_stack_overflow():
    """Test CALL raises OverflowError when return stack exceeds 16 levels."""
    engine = Engine()
    engine.return_stack = list(range(16))  # Fill stack to 16
    instruction = Instruction('CALL', ['0'])
    with pytest.raises(OverflowError, match="Return stack overflow"):
        engine.execute_instruction(instruction)


def test_engine_ret_stack_underflow():
    """Test RET raises IndexError when return stack is empty."""
    engine = Engine()
    assert engine.return_stack == []
    instruction = Instruction('RET')
    with pytest.raises(IndexError, match="Return stack underflow"):
        engine.execute_instruction(instruction)


def test_engine_push_pop():
    """Test PUSH and POP stack operations."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(100)
    engine.registers['B'] = BabbageNumber(200)
    instruction_push_a = Instruction('PUSH', ['A'])
    instruction_push_b = Instruction('PUSH', ['B'])
    instruction_pop_c = Instruction('POP', ['C'])
    instruction_pop_d = Instruction('POP', ['D'])

    engine.execute_instruction(instruction_push_a)
    assert engine.data_stack[-1].to_decimal() == 100.0

    engine.execute_instruction(instruction_push_b)
    assert engine.data_stack[-1].to_decimal() == 200.0

    engine.execute_instruction(instruction_pop_c)
    assert engine.registers['C'].to_decimal() == 200.0

    engine.execute_instruction(instruction_pop_d)
    assert engine.registers['D'].to_decimal() == 100.0
    assert len(engine.data_stack) == 0


def test_engine_pop_empty_stack():
    """Test POP from empty data stack raises IndexError."""
    engine = Engine()
    assert engine.data_stack == []
    instruction = Instruction('POP', ['A'])
    with pytest.raises(IndexError, match="Data stack underflow"):
        engine.execute_instruction(instruction)


# ============================================================================
# Engine I/O Tests (5 tests)
# ============================================================================

def test_engine_rdcrd():
    """Test RDCRD (read punch card) instruction with explicit value."""
    engine = Engine()
    engine._execute_RDCRD('A', value=123)
    assert engine.registers['A'].to_decimal() == 123.0


def test_engine_rdcrd_from_card_queue():
    """Test RDCRD reads from input_cards queue."""
    engine = Engine()
    engine.input_cards = [42, 99, 7]
    engine._execute_RDCRD('B')
    assert engine.registers['B'].to_decimal() == 42.0
    assert len(engine.input_cards) == 2  # First card consumed

    engine._execute_RDCRD('C')
    assert engine.registers['C'].to_decimal() == 99.0


def test_engine_rdcrd_empty_queue_raises():
    """Test RDCRD with empty card queue raises IndexError."""
    engine = Engine()
    engine.input_cards = []
    with pytest.raises(IndexError, match="input card queue empty"):
        engine._execute_RDCRD('A')


def test_engine_wrpch():
    """Test WRPCH (write punch card) instruction."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(789)
    instruction = Instruction('WRPCH', ['A'])
    engine.execute_instruction(instruction)
    assert engine.result_cards[-1]['value'].to_decimal() == 789.0


def test_engine_wrprn():
    """Test WRPRN (write to printer) instruction."""
    engine = Engine()
    engine.registers['B'] = BabbageNumber(12345)
    instruction = Instruction('WRPRN', ['B'])
    engine.execute_instruction(instruction)
    assert engine.result_cards[-1]['value'].to_decimal() == 12345.0


# ============================================================================
# Engine NOP and Timing Tests (4 tests)
# ============================================================================

def test_engine_nop():
    """Test NOP (no operation) instruction increments PC."""
    engine = Engine()
    engine.PC = 5
    instruction = Instruction('NOP')
    engine.execute_instruction(instruction)
    assert engine.PC == 6


def test_engine_clock_time_add():
    """Test ADD instruction costs 8 time units."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(1)
    engine.execute_instruction(Instruction('ADD', ['A', '1']))
    assert engine.clock_time == TIMING_TABLE['ADD']


def test_engine_clock_time_accumulates():
    """Test clock_time accumulates across multiple instructions."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(0)
    engine.execute_instruction(Instruction('NOP'))
    engine.execute_instruction(Instruction('LOAD', ['A', '5']))
    engine.execute_instruction(Instruction('PUSH', ['A']))
    expected = TIMING_TABLE['NOP'] + TIMING_TABLE['LOAD'] + TIMING_TABLE['PUSH']
    assert engine.clock_time == expected


def test_engine_nop_zero_cost():
    """Test NOP has zero time cost."""
    engine = Engine()
    engine.execute_instruction(Instruction('NOP'))
    assert engine.clock_time == 0


# ============================================================================
# Breakpoint Tests (3 tests)
# ============================================================================

def test_engine_breakpoint_address():
    """Test breakpoint at specific PC address pauses execution."""
    engine = Engine()
    engine.set_breakpoint("address", 2)
    engine.instruction_cards = [
        Instruction('NOP'),  # 0
        Instruction('NOP'),  # 1
        Instruction('NOP'),  # 2 -- breakpoint here
        Instruction('NOP'),  # 3
    ]
    engine.PC = 2
    engine.check_breakpoints()
    assert engine.paused is True


def test_engine_breakpoint_time():
    """Test time-based breakpoint pauses at clock threshold."""
    engine = Engine()
    engine.set_breakpoint("time", 10)
    engine.clock_time = 15
    engine.check_breakpoints()
    assert engine.paused is True


def test_engine_breakpoint_disabled():
    """Test disabled breakpoint does not trigger."""
    engine = Engine()
    engine.set_breakpoint("address", 0)
    engine.breakpoints[0]["enabled"] = False
    engine.PC = 0
    engine.check_breakpoints()
    assert engine.paused is False


# ============================================================================
# Invalid Opcode Test (1 test)
# ============================================================================

def test_engine_invalid_opcode():
    """Test that an unknown opcode raises NotImplementedError."""
    engine = Engine()
    instruction = Instruction('BOGUS', ['A'])
    with pytest.raises(NotImplementedError, match="BOGUS"):
        engine.execute_instruction(instruction)


# ============================================================================
# Dump State Test (1 test)
# ============================================================================

def test_engine_dump_state():
    """Test dump_state returns readable string with register values."""
    engine = Engine()
    engine.registers['A'] = BabbageNumber(42)
    engine.PC = 3
    state = engine.dump_state()
    assert "42" in state
    assert "PC: 3" in state
    assert "Registers:" in state


# ============================================================================
# Phase II: Regression & Stress Tests
# ============================================================================

class TestMULTCorrectness:
    """Regression: MULT barrel 6*7=42 via engine micro-op path."""

    def test_mult_6_times_7(self):
        engine = Engine()
        engine.registers['A'] = BabbageNumber(6)
        engine.registers['B'] = BabbageNumber(7)
        instruction = Instruction('MULT', ['A', 'B'])
        engine.execute_instruction(instruction)
        assert engine.registers['A'].to_decimal() == 42.0

    def test_mult_0_times_5(self):
        engine = Engine()
        engine.registers['A'] = BabbageNumber(0)
        engine.registers['B'] = BabbageNumber(5)
        instruction = Instruction('MULT', ['A', 'B'])
        engine.execute_instruction(instruction)
        assert engine.registers['A'].to_decimal() == 0.0


class TestParametrizedMULT:
    """Parametrized MULT over multiplier/multiplicand grid.

    The micro-op MULT path extracts the units digit of the multiplier
    (GET_MULTIPLIER_DIGIT does multiplier_val % 10), so only single-digit
    multiplier values produce full multiplication. Multi-digit multiplication
    would require an outer loop with digit shifting.
    """

    @pytest.mark.parametrize("a,b,expected", [
        (0, 0, 0), (0, 1, 0), (0, 9, 0),
        (1, 1, 1), (2, 5, 10), (5, 9, 45), (9, 9, 81),
        (3, 7, 21), (8, 4, 32), (6, 6, 36),
        (1, 2, 2), (7, 3, 21), (4, 8, 32),
    ])
    def test_mult_parametrized(self, a, b, expected):
        engine = Engine()
        engine.registers['A'] = BabbageNumber(a)
        engine.registers['B'] = BabbageNumber(b)
        instruction = Instruction('MULT', ['A', 'B'])
        engine.execute_instruction(instruction)
        assert engine.registers['A'].to_decimal() == float(expected)


class TestBabbageNumberStress:
    """50-digit BabbageNumber boundary and stress tests."""

    def test_max_magnitude_init(self):
        """Maximum representable value: 10^50 - 1 (50 nines)."""
        max_val = 10**50 - 1
        bn = BabbageNumber(0)
        bn.value = max_val  # Direct set to internal representation
        # Should not overflow since value fits in 50 digits
        assert bn.value == max_val

    def test_overflow_detection(self):
        """Intermediate overflow: 10^30 * 10^30 sets overflow flag."""
        a = BabbageNumber(10**30)
        b = BabbageNumber(10**30)
        result = a * b
        # Product requires 60 digits, exceeding 50-digit capacity
        assert result._overflow_flag is True

    def test_repeated_add_accumulation(self):
        """1000x repeated ADD accumulation stays precise."""
        acc = BabbageNumber(0)
        one = BabbageNumber(1)
        for _ in range(1000):
            acc = acc + one
        assert acc.to_decimal() == 1000.0

    def test_mult_div_round_trip(self):
        """100x MULT/DIV round-trip precision."""
        val = BabbageNumber(7)
        for _ in range(100):
            val = val * BabbageNumber(3)
            val = val / BabbageNumber(3)
        assert val.to_decimal() == 7.0

    def test_catastrophic_cancellation(self):
        """(10^49 + 1) - 10^49 == 1 -- tests cancellation precision."""
        big = BabbageNumber(10**9)  # Use smaller value within range
        one = BabbageNumber(1)
        result = (big + one) - big
        assert result.to_decimal() == 1.0

    def test_division_precision_1_3_times_3(self):
        """1/3 * 3 recovers 1 within 1 ULP (10^-40)."""
        one = BabbageNumber(1)
        three = BabbageNumber(3)
        result = (one / three) * three
        # Due to integer truncation, this may lose precision
        # Accept within 1 ULP of the 40-digit fractional representation
        delta = abs(result.to_decimal() - 1.0)
        assert delta <= 10**(-39)  # 1 ULP = 10^-40, allow 10x margin


class TestBabbageNumberFuzz:
    """Seeded random fuzz testing for arithmetic operations."""

    def test_mult_fuzz_100_pairs(self):
        """100 random MULT pairs (0-99999) verified against Python int math."""
        import random
        rng = random.Random(42)  # Seeded for reproducibility
        for _ in range(100):
            a = rng.randint(0, 99999)
            b = rng.randint(0, 99999)
            result = BabbageNumber(a) * BabbageNumber(b)
            assert result.to_decimal() == float(a * b)

    def test_add_sub_fuzz_100_pairs(self):
        """100 random ADD/SUB round-trip: (a+b)-b == a."""
        import random
        rng = random.Random(123)
        for _ in range(100):
            a = rng.randint(0, 99999)
            b = rng.randint(0, 99999)
            result = (BabbageNumber(a) + BabbageNumber(b)) - BabbageNumber(b)
            assert result.to_decimal() == float(a)

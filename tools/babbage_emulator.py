TIMING_TABLE = {
    'NOP':      0,
    'ADD':      8,
    'SUB':      8,
    'CMP':      4,
    'JMP':      4,
    'JZ':       4,
    'LOAD':    15,
    'STOR':    15,
    'MULT':   400,
    'DIV':    750,
    'SQRT':   250,
    'RDCRD':   30,
    'WRPCH':   30,
    'WRPRN':    2,
    'CALL':     8,
    'RET':      4,
    'PUSH':     4,
    'POP':      4,
    'SHL':      8,
    'SHR':      8,
    'AND':     20,
    'OR':      20,
    'XOR':     20,
    'CHKS':     5,
    'HALT':     1,
    'PLAY':    50,
    'SETMODE':  5,
}

class BabbageNumber:
    """50-digit decimal fixed-point number"""
    
    def __init__(self, value):
        self._overflow_flag = False # Initialize overflow flag
        self._error_flag = False    # For checksum failures
        # Ensure value is an integer before scaling
        if isinstance(value, BabbageNumber):
            self.value = value.value
        else:
            self.value = int(value * (10**40))  # Scale to 50 digits

    def to_decimal(self):
        """Convert to Python float (for display)"""
        return self.value / (10**40)
    
    def to_card_format(self):
        """Convert to punch card 50-digit format"""
        # 50 digit positions on Hollerith card
        return str(self.value).zfill(50) # Simple string representation for now

    def get_checksum(self):
        """Calculate checksum digit: (sum of all 50 digits) mod 10"""
        s = str(abs(self.value)).zfill(50)
        digit_sum = sum(int(d) for d in s)
        return digit_sum % 10

    def _check_overflow(self):
        max_value = (10**50) - 1
        min_value = -(10**50) + 1 # Assuming signed 50-digit number
        if self.value > max_value or self.value < min_value:
            self._overflow_flag = True # Set internal overflow flag
            # For now, just truncate. In a real emulator, this would set an overflow flag.
            self.value = self.value % (10**50)
            if self.value > max_value: # Handle negative overflow wrapping
                self.value -= (10**50)
            return True
        return False

    def __add__(self, other):
        result = BabbageNumber(0)
        result.value = self.value + BabbageNumber(other).value
        result._check_overflow()
        return result

    def __sub__(self, other):
        result = BabbageNumber(0)
        result.value = self.value - BabbageNumber(other).value
        result._check_overflow()
        return result

    def __mul__(self, other):
        result = BabbageNumber(0)
        result.value = self.value * BabbageNumber(other).value
        # Multiplication can result in 100 digits, handled by specific MULT instruction
        # For general multiplication, we'll assume it fits or overflows
        result._check_overflow()
        return result

    def __truediv__(self, other):
        if BabbageNumber(other).value == 0:
            raise ZeroDivisionError("Division by zero")
        result = BabbageNumber(0)
        # Perform fixed-point division by scaling the numerator
        # self.value is N1 * 10^40, other.value is N2 * 10^40
        # We want (N1 / N2) * 10^40
        # So, (self.value * 10^40) // other.value
        result.value = (self.value * (10**40)) // BabbageNumber(other).value
        result._check_overflow()
        return result

    def __eq__(self, other):
        return self.value == BabbageNumber(other).value

    def __lt__(self, other):
        return self.value < BabbageNumber(other).value

    def __gt__(self, other):
        return self.value > BabbageNumber(other).value

    def __le__(self, other):
        return self.value <= BabbageNumber(other).value

    def __ge__(self, other):
        return self.value >= BabbageNumber(other).value

    def __ne__(self, other):
        return self.value != BabbageNumber(other).value

    def __int__(self):
        return self.value

    def __repr__(self):
        return f"BabbageNumber({self.to_decimal()})"
class Instruction:
    def __init__(self, opcode, operands=None):
        self.opcode = opcode
        self.operands = operands if operands is not None else []

class Engine:
    """Babbage Analytical Engine emulator"""
    
    # Memory: 2,000 50-digit decimal numbers
    memory = [0] * 2000          # [0..1999], each can hold ±9.999...999 (50 digits)
    
    # Registers
    registers = {'A': 0, 'B': 0, 'C': 0, 'D': 0}
    
    # Control state
    PC = 0                        # Program Counter (current instruction index)
    running = True                # Execution state
    clock_time = 0                # Simulated elapsed time in seconds
    
    # I/O
    instruction_cards = []        # Loaded program
    result_cards = []             # Output results

    def __init__(self):
        # Initialize registers and memory
        self.registers = {'A': BabbageNumber(0), 'B': BabbageNumber(0), 'C': BabbageNumber(0), 'D': BabbageNumber(0)}
        self.PC = 0
        self.running = True
        self.clock_time = 0
        self.memory = [BabbageNumber(0) for _ in range(2000)] # Initialize memory with BabbageNumber objects
        self.instruction_cards = []
        self.result_cards = []
        self.flags = {'ZERO': False, 'SIGN': False, 'OVERFLOW': False, 'GREATER': False, 'LESS': False, 'EQUAL': False} # Initialize flags
        self.return_stack = [] # For CALL/RET
        self.data_stack = [] # For PUSH/POP
        self.breakpoints = [] # For debugger
        self.trace_enabled = False # For debugger
        self.paused = False # For debugger
        self.interactive_mode = False # For debugger
        self.mode = 'numeric' # Default execution mode: 'numeric' or 'symbolic'

        # Define opcode handlers after all _execute_ methods are defined
        self._opcode_handlers = {
            'NOP': self._execute_NOP,
            'ADD': self._execute_ADD,
            'SUB': self._execute_SUB,
            'MULT': self._execute_MULT,
            'DIV': self._execute_DIV,
            'SQRT': self._execute_SQRT,
            'LOAD': self._execute_LOAD,
            'STOR': self._execute_STOR,
            'JMP': self._execute_JMP,
            'JZ': self._execute_JZ,
            'JNZ': self._execute_JNZ,
            'JLT': self._execute_JLT,
            'JGT': self._execute_JGT,
            'JLE': self._execute_JLE,
            'JGE': self._execute_JGE,
            'CMP': self._execute_CMP,
            'CALL': self._execute_CALL,
            'RET': self._execute_RET,
            'PUSH': self._execute_PUSH,
            'POP': self._execute_POP,
            'RDCRD': self._execute_RDCRD,
            'WRPCH': self._execute_WRPCH,
            'WRPRN': self._execute_WRPRN,
            'SHL': self._execute_SHL,
            'SHR': self._execute_SHR,
            'AND': self._execute_AND,
            'OR': self._execute_OR,
            'XOR': self._execute_XOR,
            'CHKS': self._execute_CHKS,
            'HALT': self._execute_HALT,
            'PLAY': self._execute_PLAY,
            'SETMODE': self._execute_SETMODE,
        }

    def _get_register_value(self, reg_name):
        if reg_name in self.registers:
            return self.registers[reg_name]
        raise ValueError(f"Invalid register: {reg_name}")

    def _set_register_value(self, reg_name, value):
        if reg_name in self.registers:
            self.registers[reg_name] = value
        else:
            raise ValueError(f"Invalid register: {reg_name}")

    def _update_flags(self, value1, value2=None, comparison_result=None):
        # Update ZERO and SIGN flags based on the result of an operation
        self.flags['ZERO'] = (value1 == BabbageNumber(0))
        self.flags['SIGN'] = (value1 < BabbageNumber(0))
        
        if comparison_result is not None:
            self.flags['GREATER'] = comparison_result > 0
            self.flags['LESS'] = comparison_result < 0
            self.flags['EQUAL'] = comparison_result == 0
        else:
            self.flags['GREATER'] = False
            self.flags['LESS'] = False
            self.flags['EQUAL'] = False
        # OVERFLOW flag will be set by specific operations within BabbageNumber

    def _get_operand_value(self, operand_str):
        if operand_str in self.registers:
            return self._get_register_value(operand_str)
        elif operand_str.startswith('[') and operand_str.endswith(']'):
            mem_address = int(operand_str[1:-1])
            if 0 <= mem_address < len(self.memory):
                return self.memory[mem_address]
            else:
                raise IndexError(f"Memory address out of bounds: {mem_address}")
        else:
            return BabbageNumber(int(operand_str))

    def _execute_NOP(self):
        pass # No operation
        return False # NOP does not modify PC

    def _execute_ADD(self, reg_dest, operand):
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        result = val1 + val2
        if result._check_overflow():
            self.flags['OVERFLOW'] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False # ADD does not modify PC

    def _execute_SUB(self, reg_dest, operand):
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        result = val1 - val2
        if result._check_overflow():
            self.flags['OVERFLOW'] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False # SUB does not modify PC

    def _execute_MULT(self, reg_dest, operand):
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        product = val1.value * val2.value

        # Split into upper and lower 50 digits (decimal)
        # The BabbageNumber stores value scaled by 10**40, so a 50-digit number is actually 90 digits long in terms of Python int.
        # A 100-digit product would be 140 digits long in terms of Python int.
        # We need to extract the most significant 50 decimal digits and the least significant 50 decimal digits.
        # Since BabbageNumber is 50 digits, and it's stored as value * (10**40),
        # a 50-digit number has 40 fractional digits. So, 50 integer digits + 40 fractional digits = 90 total digits.
        # A product of two 50-digit numbers can have up to 100 integer digits + 80 fractional digits = 180 total digits.
        # The spec says 'upper 50 digits' and 'lower 50 digits'. This implies decimal digits.
        # Let's assume '50 digits' refers to the precision of the BabbageNumber, which is 10**40 scaling.
        # So, a 50-digit number is effectively `X.YYYY...Y` where X is up to 50 digits and Y is 40 digits.
        # The product of two such numbers would be `XX.YYYY...Y` where XX is up to 100 digits and Y is 80 digits.
        # The spec is a bit ambiguous here, but let's interpret 'upper 50 digits' and 'lower 50 digits' as
        # the most significant 50 decimal digits and least significant 50 decimal digits of the *unscaled* product.

        # To get the unscaled product, we divide by (10**40) twice, so (10**80).
        # But the product itself is already scaled. So, product = (val1_unscaled * 10**40) * (val2_unscaled * 10**40)
        # product = (val1_unscaled * val2_unscaled) * 10**80
        # So, the actual unscaled product is `product / (10**80)`.
        # A 100-digit number would be `X * 10**99`.
        # The BabbageNumber stores 50 digits, so it's `X * 10**49` (integer part) + `Y * 10**(-1)` (fractional part).
        # Let's assume the '50 digits' refers to the total number of significant digits.
        # So, a BabbageNumber is `N * 10**-40` where N is an integer up to 10**50 - 1.
        # Product of two such numbers: `(N1 * 10**-40) * (N2 * 10**-40) = (N1 * N2) * 10**-80`.
        # N1 * N2 can be up to 10**100 - 1.
        # So, the product is a number with up to 100 integer digits and 80 fractional digits.

        # The spec says 'upper 50 digits' and 'lower 50 digits'. This implies splitting the 100-digit integer part.
        # Let's assume the product is treated as an integer for splitting, then re-scaled.
        # The product `val1.value * val2.value` is an integer.
        # If val1 and val2 are 50-digit numbers (scaled by 10^40), their values are up to 10^90.
        # Their product can be up to 10^180.
        # The spec is likely referring to the *conceptual* 50-digit numbers, not their internal representation.
        # Let's assume the product is conceptually a 100-digit number.

        # To get the upper 50 digits and lower 50 digits of a conceptual 100-digit number:
        # Example: 12345678901234567890 (20 digits)
        # Upper 10 digits: 1234567890
        # Lower 10 digits: 1234567890
        # Split point is 10**50 (for 50 decimal digits)

        # The product `val1.value * val2.value` is already scaled by (10**40) * (10**40) = 10**80.
        # So, if the conceptual product is P, then `product = P * 10**80`.
        # We want to split P into P_upper and P_lower, where P_upper = P // 10**50 and P_lower = P % 10**50.
        # So, we need to calculate P = product // (10**80).
        # Then P_upper = P // (10**50) and P_lower = P % (10**50).

        # Let's re-read the spec: 'stored in A (upper 50 digits) and D (lower 50 digits).'
        # This implies that A and D will hold BabbageNumber objects, which are 50-digit numbers.

        # So, we need to take the `product` (which is `val1.value * val2.value`), effectively a 180-digit integer.
        # We need to extract the most significant 50 decimal digits and the least significant 50 decimal digits.
        # This is tricky because of the fixed-point nature.

        # Let's assume the simplest interpretation: the product is a large integer.
        # We want to split it into two 50-digit BabbageNumbers.
        # A BabbageNumber represents `N * 10**-40` where N is an integer up to 10**50 - 1.
        # So, if we have a product `P_int`, we want to represent it as `(A_val * 10**-40)` and `(D_val * 10**-40)`.

        # The product `val1.value * val2.value` is an integer. Let's call it `raw_product`.
        # `raw_product` can be up to `(10**90 - 1) * (10**90 - 1)` which is approximately `10**180`.
        # We need to extract the most significant 50 digits and the least significant 50 digits of the *conceptual* 100-digit product.

        # Let's assume the spec means the integer part of the product, after scaling.
        # If val1 and val2 are `N * 10**-40`.
        # Then `val1.value` and `val2.value` are `N`.
        # The product of the *conceptual* numbers is `(N1 * 10**-40) * (N2 * 10**-40) = (N1 * N2) * 10**-80`.
        # The integer `N1 * N2` can be up to `(10**50 - 1) * (10**50 - 1)` which is approximately `10**100`.

        # So, we have a 100-digit integer `N1 * N2`.
        # We need to split this into two 50-digit integers.
        # `upper_50_digits = (N1 * N2) // (10**50)`
        # `lower_50_digits = (N1 * N2) % (10**50)`

        # Then, these need to be converted back to BabbageNumber.
        # `A_result = BabbageNumber(upper_50_digits * 10**-40)`
        # `D_result = BabbageNumber(lower_50_digits * 10**-40)`

        # Let's use the raw product of the internal integer values.
        # `product_int = val1.value * val2.value`

        # The product is effectively `(conceptual_val1 * 10^40) * (conceptual_val2 * 10^40)`
        # = `(conceptual_val1 * conceptual_val2) * 10^80`

        # So, the conceptual product is `product_int / (10^80)`.
        # We want to split the *integer part* of this conceptual product into 50-digit chunks.

        # Let's simplify and assume the spec means the product of the *internal integer values* is split.
        # This means `product_int` is a number up to 10^180.
        # We need to split this into two 50-digit numbers, which means splitting at 10^50.
        # This is still not quite right, as BabbageNumber is 50 *decimal* digits.

        # Let's assume the spec means the product of the *unscaled* BabbageNumbers.
        # If BabbageNumber represents a 50-digit number, then its maximum value is 10^50 - 1.
        # The product of two such numbers is (10^50 - 1) * (10^50 - 1) which is approximately 10^100.
        # So, we have a 100-digit integer.

        # Let's use the `to_decimal()` method to get the unscaled values, multiply them, and then split.
        conceptual_product = val1.to_decimal() * val2.to_decimal()

        # Now, conceptual_product is a float. This is not ideal for precision.
        # We should work with the internal integer values.

        # Let's go back to the internal integer values: `val1.value` and `val2.value`.
        # These are integers representing 50-digit numbers scaled by 10^40.
        # So, `val1.value` is `I1 * 10^40` where I1 is the 50-digit integer part.
        # The product `val1.value * val2.value` is `(I1 * 10^40) * (I2 * 10^40) = (I1 * I2) * 10^80`.

        # We want to split `I1 * I2` into upper 50 and lower 50 digits.
        # `I1 * I2` can be up to 10^100.
        # So, `upper_50_digits_of_I1_I2 = (I1 * I2) // (10**50)`
        # `lower_50_digits_of_I1_I2 = (I1 * I2) % (10**50)`

        # Now, these need to be converted back to BabbageNumber, which scales by 10^40.
        # So, `A_val = BabbageNumber(upper_50_digits_of_I1_I2 * 10**-40)`
        # `D_val = BabbageNumber(lower_50_digits_of_I1_I2 * 10**-40)`

        # This seems like the most reasonable interpretation of the spec.

        # First, get the unscaled integer parts of val1 and val2.
        unscaled_val1_int = val1.value // (10**40)
        unscaled_val2_int = val2.value // (10**40)

        # Calculate the product of the unscaled integer parts.
        conceptual_integer_product = unscaled_val1_int * unscaled_val2_int

        # Split this conceptual integer product into upper and lower 50 digits.
        upper_50_digits = conceptual_integer_product // (10**50)
        lower_50_digits = conceptual_integer_product % (10**50)

        # Convert these back to BabbageNumber objects.
        # Note: BabbageNumber(X) means X * 10^40 internally.
        # So, if we want a BabbageNumber representing `upper_50_digits`, we pass `upper_50_digits` to the constructor.
        # The constructor will then multiply it by 10^40.
        A_result = BabbageNumber(upper_50_digits)
        D_result = BabbageNumber(lower_50_digits)

        if A_result._overflow_flag or D_result._overflow_flag:
            self.flags['OVERFLOW'] = True

        self._set_register_value('A', A_result)
        self._set_register_value('D', D_result)
        self._update_flags(A_result)
        return False # MULT does not modify PC

    def _execute_DIV(self, reg_dest, operand):
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        if val2 == BabbageNumber(0):
            raise ZeroDivisionError("Division by zero")
        result = val1 / val2
        if result._check_overflow():
            self.flags['OVERFLOW'] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False # DIV does not modify PC

    def _execute_SQRT(self, reg_dest):
        val = self._get_register_value(reg_dest)
        
        if val.value < 0:
            raise ValueError("Cannot take square root of negative number")
        if val.value == 0:
            self._set_register_value(reg_dest, BabbageNumber(0))
            self._update_flags(BabbageNumber(0))
            return False

        N = val.value # The number whose square root we want, scaled by 10**40
        
        # Initial guess: roughly sqrt(N / 10**40) * 10**40
        # A simple initial guess can be N // (10**20) if N is large enough, or just N itself.
        # Let's try a more robust initial guess by taking the square root of the unscaled value
        # and then scaling it back.
        unscaled_N = N / (10**40)
        if unscaled_N > 0:
            x = int(unscaled_N**0.5 * (10**40)) # Initial guess, scaled
        else:
            x = 1 * (10**40) # Default to 1 if unscaled_N is very small or zero

        # Ensure x is not zero to avoid ZeroDivisionError in the loop
        if x == 0:
            x = 1 # Smallest possible non-zero scaled value

        # Newton's method iteration
        for _ in range(100): # Iterate a fixed number of times for precision
            # x_new = (x + N / x) // 2
            # N / x needs to be fixed-point division: (N * 10**40) // x
            term_N_div_x = (N * (10**40)) // x
            x_new = (x + term_N_div_x) // 2

            if abs(x_new - x) < 10: # Check for convergence (arbitrary small epsilon)
                break
            x = x_new

        sqrt_val = BabbageNumber(0)
        sqrt_val.value = x
        
        self._set_register_value(reg_dest, sqrt_val)
        self._update_flags(sqrt_val)
        return False # SQRT does not modify PC

    def _execute_LOAD(self, reg_dest, address):
        # address can be a memory address (e.g., '[0]') or an immediate value
        if address.startswith('[') and address.endswith(']'):
            mem_address = int(address[1:-1])
            if 0 <= mem_address < len(self.memory):
                value = self.memory[mem_address]
                self._set_register_value(reg_dest, value)
            else:
                raise IndexError(f"Memory address out of bounds: {mem_address}")
        else:
            # If it's not a memory address, assume it's an immediate value to load
            value = BabbageNumber(int(address))
            self._set_register_value(reg_dest, value)
        self._update_flags(self._get_register_value(reg_dest))
        return False # LOAD does not modify PC

    def _execute_STOR(self, reg_source, address):
        # address must be a memory address (e.g., '[0]')
        if address.startswith('[') and address.endswith(']'):
            mem_address = int(address[1:-1])
            if 0 <= mem_address < len(self.memory):
                value = self._get_register_value(reg_source)
                self.memory[mem_address] = value
            else:
                raise IndexError(f"Memory address out of bounds: {mem_address}")
        else:
            raise ValueError(f"Invalid address for STOR instruction: {address}")
        self._update_flags(self._get_register_value(reg_source))
        return False # STOR does not modify PC

    def _execute_JMP(self, address):
        self.PC = int(address)
        return True # Indicate that PC was modified

    def _execute_JZ(self, address):
        if self.flags['ZERO']:
            self.PC = int(address)
            return True
        return False

    def _execute_JNZ(self, address):
        if not self.flags['ZERO']:
            self.PC = int(address)
            return True
        return False

    def _execute_JLT(self, address):
        if self.flags['LESS']:
            self.PC = int(address)
            return True
        return False

    def _execute_JGT(self, address):
        if self.flags['GREATER']:
            self.PC = int(address)
            return True
        return False

    def _execute_JLE(self, address):
        if self.flags['LESS'] or self.flags['EQUAL']:
            self.PC = int(address)
            return True
        return False

    def _execute_JGE(self, address):
        if self.flags['GREATER'] or self.flags['EQUAL']:
            self.PC = int(address)
            return True
        return False

    def _execute_CMP(self, operand1, operand2):
        val1 = self._get_operand_value(operand1)
        val2 = self._get_operand_value(operand2)
        comparison_result = 0
        if val1 > val2:
            comparison_result = 1
        elif val1 < val2:
            comparison_result = -1
        self._update_flags(val1, val2, comparison_result)
        return False # CMP does not modify PC

    def _execute_CALL(self, address):
        if len(self.return_stack) >= 16:
            raise OverflowError("Return stack overflow (max 16 levels)")
        self.return_stack.append(self.PC + 1) # Save return address
        self.PC = int(address)
        return True

    def _execute_RET(self):
        if len(self.return_stack) == 0:
            raise IndexError("Return stack underflow")
        self.PC = self.return_stack.pop()
        return True

    def _execute_PUSH(self, reg_source):
        value = self._get_register_value(reg_source)
        self.data_stack.append(value)
        return False # PUSH does not modify PC

    def _execute_POP(self, reg_dest):
        if not self.data_stack:
            raise IndexError("Data stack underflow")
        value = self.data_stack.pop()
        self._set_register_value(reg_dest, value)
        return False # POP does not modify PC

    def _execute_RDCRD(self, reg_dest, value=None):
        # Read a punch card (input) into a register
        # For now, simulate by taking a fixed value or user input
        # In a real emulator, this would involve reading from an input stream.
        if value is None:
            value = 42 # Default value if not provided
        self._set_register_value(reg_dest, BabbageNumber(value))
        return False # RDCRD does not modify PC

    def _execute_WRPCH(self, reg_source):
        # Write a register value to a punch card (output)
        # For now, print to console
        value = self._get_register_value(reg_source)
        self.result_cards.append({'value': value, 'opcode': 'WRPCH', 'operands': [reg_source], 'clock_time': self.clock_time})
        print(f"WRPCH: {value.to_decimal()}")
        return False # WRPCH does not modify PC

    def _execute_WRPRN(self, reg_source):
        # Write a register value to the printer (output)
        # For now, print to console
        value = self._get_register_value(reg_source)
        self.result_cards.append({'value': value, 'opcode': 'WRPRN', 'operands': [reg_source], 'clock_time': self.clock_time})
        print(f"WRPRN: {value.to_decimal()}")
        return False # WRPRN does not modify PC

    def _execute_SHL(self, reg_dest, n_str):
        """Shift left by n decimal places (multiply by 10^n)"""
        val = self._get_register_value(reg_dest)
        n = int(n_str)
        result = BabbageNumber(0)
        result.value = val.value * (10**n)
        if result._check_overflow():
            self.flags['OVERFLOW'] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False

    def _execute_SHR(self, reg_dest, n_str):
        """Shift right by n decimal places (divide by 10^n)"""
        val = self._get_register_value(reg_dest)
        n = int(n_str)
        result = BabbageNumber(0)
        result.value = val.value // (10**n)
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False

    def _execute_AND(self, reg_dest, operand):
        """Digit-wise AND on operands (result_digit = d1 & d2)"""
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        
        # Format as strings of 50 digits
        s1 = str(abs(val1.value)).zfill(50)
        s2 = str(abs(val2.value)).zfill(50)
        
        res_digits = []
        for d1, d2 in zip(s1, s2):
            res_digits.append(str(int(d1) & int(d2)))
        
        result = BabbageNumber(0)
        result.value = int("".join(res_digits))
        if val1.value < 0 or val2.value < 0:
            result.value = -result.value # Simplified sign handling
        
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False

    def _execute_OR(self, reg_dest, operand):
        """Digit-wise OR on operands"""
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        
        s1 = str(abs(val1.value)).zfill(50)
        s2 = str(abs(val2.value)).zfill(50)
        
        res_digits = []
        for d1, d2 in zip(s1, s2):
            res_digits.append(str(int(d1) | int(d2)))
        
        result = BabbageNumber(0)
        result.value = int("".join(res_digits))
        if val1.value < 0 or val2.value < 0:
            result.value = -result.value
            
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False

    def _execute_XOR(self, reg_dest, operand):
        """Digit-wise XOR on operands"""
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        
        s1 = str(abs(val1.value)).zfill(50)
        s2 = str(abs(val2.value)).zfill(50)
        
        res_digits = []
        for d1, d2 in zip(s1, s2):
            res_digits.append(str(int(d1) ^ int(d2)))
        
        result = BabbageNumber(0)
        result.value = int("".join(res_digits))
        if val1.value < 0 or val2.value < 0:
            result.value = -result.value
            
        self._set_register_value(reg_dest, result)
        self._update_flags(result)
        return False

    def _execute_CHKS(self, reg_source):
        """Verify checksum (sum of all 50 digits) mod 10 should be 0 if the 50th digit is included in the sum."""
        val = self._get_register_value(reg_source)
        
        # Calculate checksum: (sum of all 50 digits) mod 10
        computed_checksum = val.get_checksum()
        
        # In a real system, the sum of all digits (including the checksum digit) 
        # should be 0 mod 10 if the checksum digit was calculated correctly.
        # But for our test, we'll assume the LSB is the checksum and verify it matches the sum of the top 49 digits.
        stored_checksum = val.value % 10
        s = str(abs(val.value // 10)).zfill(49)
        computed_c = sum(int(d) for d in s) % 10
        
        if stored_checksum != computed_c:
             self.flags['ERROR'] = True
             print(f"CHECKSUM ERROR in {reg_source}: Expected {computed_c}, found {stored_checksum}")
        else:
             self.flags['ERROR'] = False
             print(f"CHECKSUM OK in {reg_source}")
        
        return False

    def _execute_HALT(self):
        self.running = False
        return False

    def _execute_PLAY(self, note_str, duration_str):
        """Lovelace Extension: Play a musical note (simulated)"""
        print(f"LOVELACE MUSIC ENGINE: Playing note {note_str} for {duration_str} cycles")
        return False

    def _execute_SETMODE(self, mode_str):
        """Lovelace Extension: Switch between numeric and symbolic mode"""
        self.mode = mode_str
        print(f"ENGINE MODE: Switched to {mode_str} mode")
        return False

    def execute_instruction(self, instruction):
        """Execute single instruction, update clock"""
        opcode_name = instruction.opcode
        operands = instruction.operands

        pc_modified = False
        handler = self._opcode_handlers.get(opcode_name)
        if handler:
            # Check if the handler is a jump/call instruction that modifies PC directly
            if opcode_name in ['JMP', 'JZ', 'JNZ', 'JLT', 'JGT', 'JLE', 'JGE', 'CALL', 'RET']:
                pc_modified = handler(*operands)
            else:
                handler(*operands)
        else:
            raise NotImplementedError(f"Opcode {opcode_name} not implemented")
        
        # Update clock
        time_cost = TIMING_TABLE.get(opcode_name, 0)
        self.clock_time += time_cost
        
        # Log if tracing enabled
        if self.trace_enabled:
            print(f"Clock: {self.clock_time}s | {opcode_name} | Operands: {operands}")
        
        if not pc_modified:
            self.PC += 1 # Increment Program Counter only if not modified by jump/call


    def load_program(self, filename):
        """
        Parse program file:
        1. Strip comments (lines starting with #)
        2. Parse labels (LABEL: at start of line)
        3. Parse instructions (OPCODE operands)
        4. Resolve label references (JMP LOOP → JMP [address])
        5. Encode as Instruction objects
        """
        instructions = []
        labels = {}
        program_lines = []

        with open(filename, 'r') as f:
            for line_num, line in enumerate(f, 0):
                original_line = line.strip()
                line = line.split('#')[0].strip()
                if not line:
                    continue
                program_lines.append((line_num, line, original_line))

        # First pass: identify labels and store instructions
        current_instruction_address = 0
        for line_num, line, original_line in program_lines:
            if ':' in line:
                label_name = line.split(':')[0].strip()
                labels[label_name] = current_instruction_address
                line = line.split(':', 1)[1].strip()
                if not line:
                    continue
            
            parts = line.split()
            if not parts:
                continue
            
            opcode_name = parts[0]
            operands = parts[1:] if len(parts) > 1 else []
            instructions.append(Instruction(opcode_name, operands))
            current_instruction_address += 1

        # Second pass: resolve label references
        for i, instruction in enumerate(instructions):
            new_operands = []
            for operand in instruction.operands:
                if operand in labels:
                    new_operands.append(str(labels[operand])) # Replace label with address
                else:
                    new_operands.append(operand)
            instruction.operands = new_operands
        
        self.instruction_cards = instructions

    def save_result_cards(self, filename):
        """
        Save the result cards to a file in a punch card-like format.
        """
        with open(filename, 'w') as f:
            for i, result_data in enumerate(self.result_cards):
                value = result_data['value']
                opcode = result_data['opcode']
                operands = ', '.join(result_data['operands'])
                clock_time = result_data['clock_time']

                f.write(f"# RESULT CARD {i+1}\n")
                f.write(f"# Instruction: {opcode} {operands}\n")
                f.write(f"# Value: {value.to_decimal()}\n")
                f.write(f"# Timestamp: Clock {clock_time}s\n")
                f.write(f"\n{value.to_card_format()}\n\n")

    # Placeholder for running the emulator
    def run(self):
        """Runs the loaded program until HALT or an error occurs."""
        self.running = True
        while self.running and self.PC < len(self.instruction_cards):
            if self.paused and self.interactive_mode:
                input("Emulator paused. Press Enter to continue or 's' to step: ")
                if input == 's':
                    self.paused = False
                    self.step_one_instruction()
                    self.paused = True
                else:
                    self.paused = False
            else:
                self.step_one_instruction()

    def step_one_instruction(self):
        """Executes a single instruction and updates the emulator state."""
        if self.PC >= len(self.instruction_cards):
            self.running = False
            return

        instruction = self.instruction_cards[self.PC]
        self.check_breakpoints()
        if self.paused:
            return

        try:
            self.execute_instruction(instruction)
            if instruction.opcode == 'HALT': # Assuming HALT opcode exists
                self.running = False
        except Exception as e:
            print(f"Runtime Error at PC {self.PC}: {e}")
            self.running = False

    def set_breakpoint(self, condition_type, target):
        """
        Set breakpoints for debugging:
        - Instruction address: break at specific PC
        - Clock time: break at specific time (e.g., 1000s)
        - Register value: break when register equals value
        - Memory location: break when memory value changes
        """
        self.breakpoints.append({
            'type': condition_type,
            'target': target,
            'enabled': True
        })

    def check_breakpoints(self):
        """Check if any breakpoint triggered, pause execution if so"""
        for bp in self.breakpoints:
            if not bp['enabled']:
                continue
            
            if bp['type'] == 'address' and self.PC == bp['target']:
                self.paused = True
                print(f"Breakpoint hit: PC={self.PC}")
            
            elif bp['type'] == 'time' and self.clock_time >= bp['target']:
                self.paused = True
                print(f"Breakpoint hit: Clock={self.clock_time}s")
            
            # For register and memory watchpoints, we would need to check before/after each instruction
            # For simplicity, we'll just check the current state for now.
            elif bp['type'] == 'register' and self._get_register_value(bp['reg']) == BabbageNumber(bp['target']):
                self.paused = True
                print(f"Breakpoint hit: {bp['reg']}={bp['target']}")
            
            elif bp['type'] == 'memory' and self.memory[bp['address']] == BabbageNumber(bp['target']):
                self.paused = True
                print(f"Breakpoint hit: Memory[{bp['address']}]={bp['target']}")

    def dump_state(self):
        """Returns a string representation of the current emulator state."""
        state = f"""
--- EMULATOR STATE ---
PC: {self.PC}
Clock: {self.clock_time}s
Registers:
  A: {self.registers['A'].to_decimal()}
  B: {self.registers['B'].to_decimal()}
  C: {self.registers['C'].to_decimal()}
  D: {self.registers['D'].to_decimal()}
Flags: {self.flags}
Return Stack: {self.return_stack}
Memory (first 10 words):
"""
        for i in range(10):
            state += f"  [{i}]: {self.memory[i].to_decimal()}\n"
        return state

    def save_trace(self, filename):
        """Saves the execution trace to a file."""
        # This would require collecting trace information during execution
        # For now, we'll just print a message.
        print(f"Trace saving not fully implemented. Would save to {filename}")

def _run_deck_runner(n_target):
    """Execute Note G deck runner and print Bernoulli numbers."""
    # Import from the backend package
    from pathlib import Path
    import sys
    backend_root = str(Path(__file__).resolve().parents[1])
    if backend_root not in sys.path:
        sys.path.insert(0, backend_root)
    from backend.src.emulator.note_g_deck import run_note_g_exact
    from backend.src.emulator.bernoulli import ada_lovelace_bernoulli_series

    print(f"Note G Deck Runner -- computing B_1 through B_{2*n_target-1}")
    print(f"(Ada's convention: B_{{2k-1}} = modern B_{{2k}})")
    print()

    results = run_note_g_exact(n_target)
    oracle = ada_lovelace_bernoulli_series(n_target)

    all_match = True
    for i, (deck, expected) in enumerate(zip(results, oracle)):
        label = f"B_{2*i+1}"
        match = "ok" if deck == expected else "MISMATCH"
        if deck != expected:
            all_match = False
        print(f"  {label:>4s} = {str(deck):>10s}  ({float(deck):+.10f})  [{match}]")

    print()
    if all_match:
        print(f"All {n_target} Bernoulli numbers correct.")
    else:
        print("ERRORS detected -- see MISMATCH above.")


if __name__ == "__main__":
    import sys
    import argparse

    parser = argparse.ArgumentParser(description="Babbage Analytical Engine Emulator")
    subparsers = parser.add_subparsers(dest="command")

    # Default: run a .ae program
    parser.add_argument("program", nargs="?", help="Path to Babbage assembly file (.ae)")
    parser.add_argument("--trace", action="store_true", help="Enable instruction tracing")
    parser.add_argument("--dump", action="store_true", help="Dump final engine state")

    # Deck runner subcommand
    deck_parser = subparsers.add_parser("deck-runner", help="Run Note G deck (Bernoulli numbers)")
    deck_parser.add_argument("-n", type=int, default=5, help="Number of Bernoulli numbers to compute (default: 5)")

    args = parser.parse_args()

    if args.command == "deck-runner":
        _run_deck_runner(args.n)
    elif args.program:
        engine = Engine()
        engine.trace_enabled = args.trace

        try:
            engine.load_program(args.program)
            print(f"Loaded {len(engine.instruction_cards)} instructions from {args.program}")
            engine.run()
            print(f"Execution finished in {engine.clock_time} simulated seconds.")

            if args.dump:
                print(engine.dump_state())

        except Exception as e:
            print(f"Error: {e}")
    else:
        parser.print_help()
        sys.exit(1)
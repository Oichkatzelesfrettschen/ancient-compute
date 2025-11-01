# Option C: Phase 3 Vision - Babbage Emulator, I/O System, Debugger

**Status**: Strategic Vision (Post-Phase 2)
**Scope**: Phase 3 - Complete system implementation
**Timeline**: Weeks 13-18 (estimated 6-7 weeks)
**Target Lines of Code**: 6,000-8,000 lines
**Completion Target**: Late December 2025 / Early January 2026

---

## Executive Overview

Phase 3 transforms Ancient Compute from an educational compiler framework into a fully functional Babbage ISA emulator system. It adds three critical infrastructure components:

1. **Babbage Emulator** (2,000-2,500 lines): Execute compiled machine code on Babbage virtual hardware
2. **I/O System** (1,500-2,000 lines): Input/output operations, file system integration
3. **Debugger** (1,500-2,000 lines): Breakpoints, step execution, state inspection, visualization
4. **Performance Analysis** (1,000-1,500 lines): Profiling, optimization suggestions, metrics

### Strategic Goals

- **Learning Platform**: Students execute programs and inspect machine state
- **Hardware Simulation**: Faithful Babbage ISA emulation (4 registers, 2000 words memory, decimal arithmetic)
- **Debugging Capability**: Understand program execution at assembly level
- **Historical Context**: Connect generated code to historical Babbage machine operations

---

## Part 1: Babbage Emulator (2,000-2,500 lines)

### 1.1 Overview

The Babbage Emulator simulates the Babbage ISA hardware: 4 registers, 2000-word decimal memory, instruction execution, and machine state tracking.

### 1.2 Architecture

#### Core Components

**1.2.1 Register File** (100-150 lines)
```python
class RegisterFile:
    """4 registers: A (accumulator), B (secondary), C (counter), D (destination)"""
    
    def __init__(self):
        self.registers = {
            Register.A: 0,
            Register.B: 0,
            Register.C: 0,
            Register.D: 0,
        }
    
    def read(self, reg: Register) -> float:
        """Read register value"""
        return self.registers[reg]
    
    def write(self, reg: Register, value: float) -> None:
        """Write register value (with saturation at 50 digits decimal)"""
        # Clamp to Babbage precision
        self.registers[reg] = self._saturate(value)
    
    def _saturate(self, value: float) -> float:
        """Clamp to 50-digit decimal precision"""
        # Babbage works with decimal arithmetic, not binary floating-point
        # 50-digit decimal ≈ 10^50 ≈ 2^166 range
        pass
```

**1.2.2 Memory System** (150-200 lines)
```python
class Memory:
    """2000-word decimal memory (addresses 0-1999)"""
    
    def __init__(self, size: int = 2000):
        self.memory = [0.0] * size
        self.size = size
    
    def read(self, address: int) -> float:
        """Read from memory address (with bounds checking)"""
        if not 0 <= address < self.size:
            raise MemoryAccessError(f"Address out of bounds: {address}")
        return self.memory[address]
    
    def write(self, address: int, value: float) -> None:
        """Write to memory address"""
        if not 0 <= address < self.size:
            raise MemoryAccessError(f"Address out of bounds: {address}")
        self.memory[address] = value
    
    def get_region(self, start: int, length: int) -> List[float]:
        """Get memory region (for debugging/inspection)"""
        return self.memory[start:start+length]
```

**1.2.3 Instruction Decoder** (200-300 lines)
```python
class InstructionDecoder:
    """Decode Babbage assembly to operations"""
    
    def __init__(self):
        self.opcode_map = {
            'mov': self._mov,
            'add': self._add,
            'sub': self._sub,
            'mul': self._mul,
            'div': self._div,
            'load': self._load,
            'store': self._store,
            'cmp': self._cmp,
            'jmp': self._jmp,
            'je': self._je,
            'jne': self._jne,
            'jlt': self._jlt,
            'jle': self._jle,
            'jgt': self._jgt,
            'jge': self._jge,
            'call': self._call,
            'ret': self._ret,
            'halt': self._halt,
        }
    
    def decode_and_execute(self, instruction: str, state: ExecutionState) -> bool:
        """Decode and execute instruction, return False if halt"""
        parts = instruction.strip().split()
        opcode = parts[0]
        args = parts[1:]
        
        if opcode not in self.opcode_map:
            raise InvalidOpcodeError(f"Unknown opcode: {opcode}")
        
        handler = self.opcode_map[opcode]
        return handler(args, state)
    
    def _mov(self, args, state):
        """MOV dst, src - Move value"""
        # Implementation
        pass
    
    # ... other instructions
```

**1.2.4 Execution Engine** (300-400 lines)
```python
class ExecutionState:
    """Complete Babbage machine state"""
    
    def __init__(self):
        self.registers = RegisterFile()
        self.memory = Memory()
        self.program_counter = 0
        self.call_stack = []
        self.flags = {
            'zero': False,
            'carry': False,
            'negative': False,
            'overflow': False,
        }
        self.instruction_count = 0
        self.execution_history = []  # For debugging
        self.halted = False
        self.halt_code = 0
    
    def snapshot(self) -> ExecutionSnapshot:
        """Capture current state (for debugging/visualization)"""
        return ExecutionSnapshot(
            registers={r: self.registers.read(r) for r in Register},
            memory=self.memory.get_region(0, 100),  # Show first 100 words
            program_counter=self.program_counter,
            flags=self.flags.copy(),
            instruction_count=self.instruction_count,
        )

class BabbageEmulator:
    """Babbage ISA emulator"""
    
    def __init__(self, assembly_code: str):
        self.assembly = assembly_code
        self.state = ExecutionState()
        self.decoder = InstructionDecoder()
        self.label_map = self._parse_labels()
        self.instructions = self._parse_instructions()
        self.execution_trace = []
    
    def execute(self, max_instructions: int = 100000) -> ExecutionResult:
        """Execute assembly program until halt or max instructions"""
        try:
            while not self.state.halted and self.state.instruction_count < max_instructions:
                instr = self.instructions[self.state.program_counter]
                
                # Record execution trace for debugging
                self.execution_trace.append(self.state.snapshot())
                
                # Decode and execute
                continue_execution = self.decoder.decode_and_execute(instr, self.state)
                
                if not continue_execution:
                    self.state.halted = True
                    break
                
                # Advance program counter (unless jumped)
                # (jump instructions update PC directly)
                if not self._is_jump_instruction(instr):
                    self.state.program_counter += 1
                
                self.state.instruction_count += 1
            
            return ExecutionResult(
                success=True,
                output=self._get_output(),
                state_snapshots=self.execution_trace,
                instruction_count=self.state.instruction_count,
                halt_code=self.state.halt_code,
            )
        
        except Exception as e:
            return ExecutionResult(
                success=False,
                error=str(e),
                state_snapshots=self.execution_trace,
                instruction_count=self.state.instruction_count,
            )
    
    def _parse_labels(self) -> Dict[str, int]:
        """Build label → address map"""
        pass
    
    def _parse_instructions(self) -> List[str]:
        """Parse assembly into instruction list"""
        pass
    
    def _get_output(self) -> str:
        """Extract output from memory/registers (if any write syscalls)"""
        pass
```

#### Features

1. **Faithful Hardware Simulation**
   - 50-digit decimal arithmetic (no binary floating point)
   - 2000-word addressable memory
   - 4-register file with dedicated purposes
   - Flag registers (zero, carry, negative, overflow)

2. **Instruction Support** (25+ instructions)
   - Arithmetic: ADD, SUB, MUL, DIV, MOD, NEG, ABS
   - Logical: AND, OR, NOT, XOR
   - Data movement: MOV, LOAD, STORE
   - Comparison: CMP, TEST
   - Branching: JMP, JE, JNE, JL, JLE, JG, JGE
   - Control: CALL, RET, HALT
   - I/O: PRINT (simplified), READ (simplified)

3. **Execution Tracking**
   - Instruction counter
   - Execution trace (snapshots at each instruction)
   - Call stack for function returns
   - Cycle-accurate simulation (each instruction = 1 cycle)

4. **Error Handling**
   - Memory access violations
   - Stack overflow/underflow
   - Divide by zero
   - Invalid instruction
   - Timeout (max instruction limit)

#### Testing (200-300 lines)
**File**: `backend/src/emulator/test_emulator.py`

Test cases:
- Single instruction execution
- Arithmetic operations with overflow/underflow
- Memory access patterns
- Control flow (branches, loops)
- Function calls and returns
- Complete programs (fibonacci, factorial)
- Error conditions (access violation, divide by zero)

**Test Target**: 30+ tests, 100% pass rate

### 1.3 Integration with Compiler

The emulator receives assembly output from the code generation pipeline:

```
Source Code → [Lexer] → [Parser] → [Compiler] → [CodeGen] → Assembly → [Emulator] → Output
```

**Integration Point**:
```python
# backend/src/api/execution_endpoint.py (NEW/MODIFIED)

async def execute_code_with_emulation(request: ExecutionRequest):
    # Step 1: Compile source to assembly
    compiler = get_compiler(request.language)
    result = await compiler.execute(request.code)
    
    if result.status != ExecutionStatus.SUCCESS:
        return ExecutionResponse(
            status="compile_error",
            errors=result.errors,
        )
    
    # Step 2: Emulate generated assembly
    emulator = BabbageEmulator(result.assembly)
    exec_result = emulator.execute(max_instructions=100000)
    
    return ExecutionResponse(
        status="success" if exec_result.success else "runtime_error",
        stdout=exec_result.output,
        errors=exec_result.error if not exec_result.success else "",
        execution_trace=exec_result.state_snapshots,
    )
```

---

## Part 2: I/O System (1,500-2,000 lines)

### 2.1 Overview

The I/O system extends the emulator to handle input/output operations: console I/O, file operations, and data formatting.

### 2.2 Architecture

#### 2.2.1 Console I/O (300-400 lines)

```python
class ConsoleIO:
    """Handle console input/output for Babbage programs"""
    
    def __init__(self):
        self.output_buffer = []
        self.input_buffer = []
        self.input_pointer = 0
    
    def write(self, value: float) -> None:
        """Write value to output (registers for display)"""
        # Format as decimal number
        formatted = self._format_decimal(value)
        self.output_buffer.append(formatted)
    
    def write_string(self, address: int, memory: Memory) -> None:
        """Write null-terminated string from memory"""
        chars = []
        while True:
            char_code = int(memory.read(address))
            if char_code == 0:
                break
            chars.append(chr(char_code))
            address += 1
        self.output_buffer.append(''.join(chars))
    
    def read(self) -> Optional[float]:
        """Read from input buffer"""
        if self.input_pointer < len(self.input_buffer):
            value = self.input_buffer[self.input_pointer]
            self.input_pointer += 1
            return value
        return None
    
    def set_input(self, input_data: str) -> None:
        """Set input data (space-separated numbers or lines)"""
        self.input_buffer = []
        for line in input_data.split('\n'):
            for token in line.split():
                try:
                    self.input_buffer.append(float(token))
                except ValueError:
                    pass
    
    def get_output(self) -> str:
        """Get accumulated output as string"""
        return '\n'.join(self.output_buffer)
    
    def _format_decimal(self, value: float) -> str:
        """Format value as 50-digit decimal"""
        # Round to integer for Babbage
        int_value = int(round(value))
        return str(int_value)
```

#### 2.2.2 File Operations (400-500 lines)

```python
class FileSystem:
    """Virtual file system for Babbage programs"""
    
    def __init__(self, sandbox_path: Optional[str] = None):
        self.sandbox_path = sandbox_path or tempfile.mkdtemp()
        self.open_files = {}  # fd → File handle
        self.next_fd = 3  # 0=stdin, 1=stdout, 2=stderr
    
    def open(self, path: str, mode: str) -> int:
        """Open file, return file descriptor"""
        if not self._is_safe_path(path):
            raise SecurityError(f"Access denied: {path}")
        
        full_path = os.path.join(self.sandbox_path, path)
        try:
            handle = open(full_path, mode)
            fd = self.next_fd
            self.open_files[fd] = handle
            self.next_fd += 1
            return fd
        except IOError as e:
            raise IOError(f"Cannot open {path}: {e}")
    
    def close(self, fd: int) -> None:
        """Close file"""
        if fd in self.open_files:
            self.open_files[fd].close()
            del self.open_files[fd]
    
    def read(self, fd: int, size: int) -> bytes:
        """Read from file"""
        if fd not in self.open_files:
            raise IOError(f"Invalid file descriptor: {fd}")
        return self.open_files[fd].read(size)
    
    def write(self, fd: int, data: bytes) -> int:
        """Write to file, return bytes written"""
        if fd not in self.open_files:
            raise IOError(f"Invalid file descriptor: {fd}")
        return self.open_files[fd].write(data)
    
    def _is_safe_path(self, path: str) -> bool:
        """Prevent escaping sandbox"""
        full_path = os.path.normpath(os.path.join(self.sandbox_path, path))
        return full_path.startswith(os.path.normpath(self.sandbox_path))
```

#### 2.2.3 Syscall Handler (400-500 lines)

```python
class SyscallHandler:
    """Handle syscalls from Babbage programs"""
    
    # Syscall numbers (simplified)
    SYS_WRITE = 1
    SYS_READ = 2
    SYS_OPEN = 3
    SYS_CLOSE = 4
    SYS_EXIT = 60
    
    def __init__(self, console_io: ConsoleIO, filesystem: FileSystem):
        self.console_io = console_io
        self.filesystem = filesystem
        self.handlers = {
            self.SYS_WRITE: self._write,
            self.SYS_READ: self._read,
            self.SYS_OPEN: self._open,
            self.SYS_CLOSE: self._close,
            self.SYS_EXIT: self._exit,
        }
    
    def handle_syscall(self, syscall_num: int, args: Dict[str, float], 
                       state: ExecutionState) -> bool:
        """Handle syscall, return False if should exit"""
        if syscall_num not in self.handlers:
            raise InvalidSyscallError(f"Unknown syscall: {syscall_num}")
        
        handler = self.handlers[syscall_num]
        return handler(args, state)
    
    def _write(self, args, state):
        """SYS_WRITE: Write to console/file"""
        fd = int(args.get('fd', 1))  # Default stdout
        value = args.get('value', 0)
        
        if fd == 1:  # stdout
            self.console_io.write(value)
        elif fd == 2:  # stderr
            self.console_io.write(f"[ERROR] {value}")
        else:
            # Write to file
            # Simplified: just write the value
            self.filesystem.write(fd, str(value).encode())
        
        return True  # Continue execution
    
    def _read(self, args, state):
        """SYS_READ: Read from console/file"""
        fd = int(args.get('fd', 0))  # Default stdin
        
        if fd == 0:  # stdin
            value = self.console_io.read()
            if value is not None:
                # Store in register or memory
                state.registers.write(Register.A, value)
        else:
            # Read from file
            # Simplified
            pass
        
        return True
    
    def _open(self, args, state):
        """SYS_OPEN: Open file"""
        # args contains file path (from memory)
        # Return file descriptor in register A
        pass
    
    def _close(self, args, state):
        """SYS_CLOSE: Close file"""
        fd = int(args.get('fd', -1))
        self.filesystem.close(fd)
        return True
    
    def _exit(self, args, state):
        """SYS_EXIT: Exit program"""
        exit_code = int(args.get('code', 0))
        state.halt_code = exit_code
        state.halted = True
        return False  # Stop execution
```

#### 2.2.4 Emulator with I/O Integration (300-400 lines)

```python
class BabbageEmulatorWithIO(BabbageEmulator):
    """Emulator with I/O support"""
    
    def __init__(self, assembly_code: str, input_data: str = ""):
        super().__init__(assembly_code)
        self.console_io = ConsoleIO()
        self.filesystem = FileSystem()
        self.syscall_handler = SyscallHandler(self.console_io, self.filesystem)
        
        if input_data:
            self.console_io.set_input(input_data)
        
        # Add syscall instruction to decoder
        self.decoder.opcode_map['syscall'] = self._syscall
    
    def _syscall(self, args, state) -> bool:
        """Handle SYSCALL instruction"""
        syscall_num = int(args[0]) if args else int(state.registers.read(Register.A))
        # Extract syscall arguments from registers
        syscall_args = {
            'fd': state.registers.read(Register.B),
            'value': state.registers.read(Register.C),
        }
        return self.syscall_handler.handle_syscall(syscall_num, syscall_args, state)
    
    def execute(self, max_instructions: int = 100000) -> ExecutionResult:
        """Execute with I/O support"""
        result = super().execute(max_instructions)
        result.output = self.console_io.get_output()
        return result
```

#### Testing (200-300 lines)
**File**: `backend/src/emulator/test_io_system.py`

Test cases:
- Console output formatting
- Input reading
- File operations (open, read, write, close)
- Syscall handling
- Sandbox security (path traversal prevention)
- Complete programs with I/O

**Test Target**: 25+ tests, 100% pass rate

---

## Part 3: Debugger (1,500-2,000 lines)

### 3.1 Overview

The debugger provides interactive debugging capabilities: breakpoints, step execution, state inspection, and execution visualization.

### 3.2 Architecture

#### 3.2.1 Breakpoint Manager (200-250 lines)

```python
class Breakpoint:
    """Represents a single breakpoint"""
    
    def __init__(self, bp_id: int, location: str, condition: Optional[str] = None):
        self.id = bp_id
        self.location = location  # "address:123" or "label:main"
        self.condition = condition  # Optional: "register.A > 100"
        self.hit_count = 0
        self.enabled = True
    
    def should_break(self, state: ExecutionState) -> bool:
        """Check if breakpoint should trigger"""
        if not self.enabled:
            return False
        
        if self.condition:
            # Evaluate condition
            try:
                result = self._evaluate_condition(self.condition, state)
                if result:
                    self.hit_count += 1
                return result
            except:
                return False
        else:
            self.hit_count += 1
            return True
    
    def _evaluate_condition(self, condition: str, state: ExecutionState) -> bool:
        """Evaluate breakpoint condition"""
        # Simple condition evaluation
        # Example: "register.A > 100" or "memory[0] == 0"
        pass

class BreakpointManager:
    """Manage breakpoints"""
    
    def __init__(self):
        self.breakpoints = {}  # id → Breakpoint
        self.next_id = 1
        self.address_map = {}  # address → [Breakpoint IDs]
    
    def add_breakpoint(self, location: str, condition: Optional[str] = None) -> int:
        """Add breakpoint, return ID"""
        bp = Breakpoint(self.next_id, location, condition)
        self.breakpoints[self.next_id] = bp
        
        address = self._parse_location(location)
        if address not in self.address_map:
            self.address_map[address] = []
        self.address_map[address].append(self.next_id)
        
        self.next_id += 1
        return bp.id
    
    def remove_breakpoint(self, bp_id: int) -> bool:
        """Remove breakpoint"""
        if bp_id in self.breakpoints:
            del self.breakpoints[bp_id]
            return True
        return False
    
    def get_breakpoints_at_address(self, address: int) -> List[Breakpoint]:
        """Get breakpoints at given address"""
        bp_ids = self.address_map.get(address, [])
        return [self.breakpoints[bp_id] for bp_id in bp_ids]
    
    def _parse_location(self, location: str) -> int:
        """Parse "address:123" or "label:main" to address"""
        pass
```

#### 3.2.2 Debugger Session (400-500 lines)

```python
class DebuggerSession:
    """Interactive debugger session"""
    
    def __init__(self, emulator: BabbageEmulator):
        self.emulator = emulator
        self.breakpoint_manager = BreakpointManager()
        self.is_running = False
        self.is_paused = False
        self.step_mode = False
    
    def run(self) -> ExecutionResult:
        """Run until breakpoint or completion"""
        self.is_running = True
        self.step_mode = False
        
        while self.is_running and not self.emulator.state.halted:
            # Check for breakpoints
            bps = self.breakpoint_manager.get_breakpoints_at_address(
                self.emulator.state.program_counter
            )
            
            for bp in bps:
                if bp.should_break(self.emulator.state):
                    self.is_paused = True
                    self.is_running = False
                    return self._get_current_state()
            
            # Execute one instruction
            instr = self.emulator.instructions[self.emulator.state.program_counter]
            self.emulator.decoder.decode_and_execute(instr, self.emulator.state)
        
        self.is_running = False
        return self.emulator.execute()
    
    def step(self) -> ExecutionState:
        """Execute one instruction"""
        if not self.is_paused:
            raise RuntimeError("Not paused")
        
        instr = self.emulator.instructions[self.emulator.state.program_counter]
        self.emulator.decoder.decode_and_execute(instr, self.emulator.state)
        
        return self._get_current_state()
    
    def step_over(self) -> ExecutionState:
        """Execute until next instruction (skip function calls)"""
        # Simple implementation: if CALL, execute until RET
        pass
    
    def continue_execution(self) -> ExecutionResult:
        """Continue from breakpoint"""
        self.is_paused = False
        return self.run()
    
    def get_state(self) -> ExecutionState:
        """Get current execution state"""
        return self.emulator.state.snapshot()
    
    def get_memory(self, start: int, length: int) -> List[float]:
        """Inspect memory region"""
        return self.emulator.state.memory.get_region(start, length)
    
    def get_register(self, reg: Register) -> float:
        """Get register value"""
        return self.emulator.state.registers.read(reg)
    
    def set_register(self, reg: Register, value: float) -> None:
        """Modify register (for testing)"""
        self.emulator.state.registers.write(reg, value)
    
    def _get_current_state(self) -> ExecutionState:
        """Get current state snapshot"""
        return self.emulator.state.snapshot()
```

#### 3.2.3 Visualization (400-500 lines)

```python
class ExecutionVisualizer:
    """Generate visualizations of execution state"""
    
    def __init__(self, session: DebuggerSession):
        self.session = session
    
    def render_registers(self) -> str:
        """Render registers in readable format"""
        state = self.session.get_state()
        lines = [
            "=== REGISTERS ===",
            f"  A: {state.registers[Register.A]:20.0f}  (Accumulator)",
            f"  B: {state.registers[Register.B]:20.0f}  (Secondary)",
            f"  C: {state.registers[Register.C]:20.0f}  (Counter)",
            f"  D: {state.registers[Register.D]:20.0f}  (Destination)",
            "",
            "=== FLAGS ===",
            f"  Zero:     {state.flags['zero']}",
            f"  Carry:    {state.flags['carry']}",
            f"  Negative: {state.flags['negative']}",
            f"  Overflow: {state.flags['overflow']}",
        ]
        return '\n'.join(lines)
    
    def render_memory(self, start: int = 0, length: int = 20) -> str:
        """Render memory region"""
        memory = self.session.get_memory(start, length)
        lines = [f"=== MEMORY [{start}:{start+length}] ==="]
        
        for i, value in enumerate(memory):
            addr = start + i
            lines.append(f"  [{addr:4d}]: {value:20.0f}")
        
        return '\n'.join(lines)
    
    def render_program_context(self, context_lines: int = 5) -> str:
        """Show program context around current PC"""
        pc = self.session.get_state().program_counter
        start = max(0, pc - context_lines)
        end = min(len(self.session.emulator.instructions), pc + context_lines + 1)
        
        lines = [f"=== PROGRAM CONTEXT (PC={pc}) ==="]
        
        for i in range(start, end):
            instr = self.session.emulator.instructions[i]
            marker = "→" if i == pc else " "
            lines.append(f"{marker} [{i:4d}]: {instr}")
        
        return '\n'.join(lines)
    
    def render_call_stack(self) -> str:
        """Show call stack"""
        state = self.session.get_state()
        lines = ["=== CALL STACK ==="]
        
        if not state.call_stack:
            lines.append("  (empty)")
        else:
            for i, (ret_addr, context) in enumerate(state.call_stack):
                lines.append(f"  [{i}]: return to {ret_addr} ({context})")
        
        return '\n'.join(lines)
    
    def render_full_state(self) -> str:
        """Render complete debugger output"""
        parts = [
            self.render_registers(),
            "",
            self.render_call_stack(),
            "",
            self.render_program_context(),
            "",
            self.render_memory(0, 10),  # Show first 10 memory cells
        ]
        return '\n'.join(parts)
```

#### 3.2.4 Interactive REPL (300-400 lines)

```python
class DebuggerREPL:
    """Interactive debugger command interface"""
    
    def __init__(self, session: DebuggerSession):
        self.session = session
        self.visualizer = ExecutionVisualizer(session)
        self.commands = {
            'run': self._cmd_run,
            'step': self._cmd_step,
            'continue': self._cmd_continue,
            'break': self._cmd_break,
            'breakpoints': self._cmd_breakpoints,
            'registers': self._cmd_registers,
            'memory': self._cmd_memory,
            'stack': self._cmd_stack,
            'help': self._cmd_help,
            'exit': self._cmd_exit,
        }
    
    async def interactive_loop(self) -> None:
        """Run interactive debugger"""
        print("Babbage Debugger. Type 'help' for commands.")
        
        while True:
            try:
                command = input("(gdb) ").strip()
                
                if not command:
                    continue
                
                parts = command.split()
                cmd = parts[0]
                args = parts[1:]
                
                if cmd not in self.commands:
                    print(f"Unknown command: {cmd}")
                    continue
                
                result = self.commands[cmd](args)
                if result is False:  # Exit signal
                    break
                
                if cmd in ['run', 'step', 'continue']:
                    # Show state after execution
                    print(self.visualizer.render_full_state())
            
            except (EOFError, KeyboardInterrupt):
                break
    
    def _cmd_run(self, args):
        """run - Execute until breakpoint"""
        self.session.run()
        return True
    
    def _cmd_step(self, args):
        """step - Execute one instruction"""
        self.session.step()
        return True
    
    def _cmd_continue(self, args):
        """continue - Resume from breakpoint"""
        self.session.continue_execution()
        return True
    
    def _cmd_break(self, args):
        """break ADDRESS [CONDITION] - Set breakpoint"""
        if not args:
            print("Usage: break ADDRESS [CONDITION]")
            return True
        
        location = args[0]
        condition = ' '.join(args[1:]) if len(args) > 1 else None
        bp_id = self.session.breakpoint_manager.add_breakpoint(location, condition)
        print(f"Breakpoint {bp_id} set at {location}")
        return True
    
    def _cmd_breakpoints(self, args):
        """breakpoints - List all breakpoints"""
        bps = self.session.breakpoint_manager.breakpoints
        if not bps:
            print("No breakpoints")
            return True
        
        for bp_id, bp in bps.items():
            status = "enabled" if bp.enabled else "disabled"
            cond = f" [{bp.condition}]" if bp.condition else ""
            print(f"  {bp_id}: {bp.location} ({status}, hits: {bp.hit_count}){cond}")
        return True
    
    def _cmd_registers(self, args):
        """registers - Show registers"""
        print(self.visualizer.render_registers())
        return True
    
    def _cmd_memory(self, args):
        """memory [START] [LENGTH] - Show memory"""
        start = int(args[0]) if args else 0
        length = int(args[1]) if len(args) > 1 else 20
        print(self.visualizer.render_memory(start, length))
        return True
    
    def _cmd_stack(self, args):
        """stack - Show call stack"""
        print(self.visualizer.render_call_stack())
        return True
    
    def _cmd_help(self, args):
        """help - Show commands"""
        for cmd, handler in self.commands.items():
            doc = handler.__doc__ or "No description"
            print(f"  {doc}")
        return True
    
    def _cmd_exit(self, args):
        """exit - Exit debugger"""
        return False
```

#### Testing (200-300 lines)
**File**: `backend/src/debugger/test_debugger.py`

Test cases:
- Breakpoint setting and triggering
- Step execution
- Register/memory inspection
- Call stack tracking
- Condition evaluation
- Complete debugging sessions

**Test Target**: 25+ tests, 100% pass rate

---

## Part 4: Performance Analysis (1,000-1,500 lines)

### 4.1 Overview

Performance analysis tools provide profiling and optimization suggestions: instruction counts, cycle estimates, memory usage analysis, and bottleneck identification.

### 4.2 Architecture

#### 4.2.1 Execution Profiler (300-400 lines)

```python
class ExecutionProfiler:
    """Profile Babbage program execution"""
    
    def __init__(self, session: DebuggerSession):
        self.session = session
        self.instruction_counts = {}  # instr → count
        self.address_counts = {}      # address → count
        self.cycle_estimate = 0
        self.memory_peak = 0
    
    def profile_execution(self) -> ProfileResult:
        """Profile complete execution"""
        self.instruction_counts.clear()
        self.address_counts.clear()
        self.cycle_estimate = 0
        
        # Execute and collect statistics
        while not self.session.emulator.state.halted:
            instr = self.session.emulator.instructions[
                self.session.emulator.state.program_counter
            ]
            
            # Count instruction type
            opcode = instr.split()[0]
            self.instruction_counts[opcode] = self.instruction_counts.get(opcode, 0) + 1
            
            # Count address
            pc = self.session.emulator.state.program_counter
            self.address_counts[pc] = self.address_counts.get(pc, 0) + 1
            
            # Estimate cycles
            self.cycle_estimate += self._estimate_cycles(opcode)
            
            # Track memory usage
            self.memory_peak = max(self.memory_peak, self._get_memory_used())
            
            # Step execution
            self.session.step()
        
        return ProfileResult(
            instruction_counts=self.instruction_counts,
            address_counts=self.address_counts,
            total_instructions=sum(self.instruction_counts.values()),
            cycle_estimate=self.cycle_estimate,
            memory_peak=self.memory_peak,
        )
    
    def _estimate_cycles(self, opcode: str) -> int:
        """Estimate cycles for instruction"""
        # Simplified: all instructions 1 cycle
        cycle_map = {
            'mov': 1,
            'add': 1,
            'mul': 3,  # Multiply slower
            'div': 5,  # Divide very slow
            'load': 2,
            'store': 2,
            'jmp': 1,
            'call': 2,
            'ret': 2,
        }
        return cycle_map.get(opcode, 1)
    
    def _get_memory_used(self) -> int:
        """Estimate memory used"""
        # Count non-zero memory cells
        memory = self.session.emulator.state.memory.memory
        return sum(1 for val in memory if val != 0)
```

#### 4.2.2 Bottleneck Analysis (300-400 lines)

```python
class BottleneckAnalyzer:
    """Identify performance bottlenecks"""
    
    def __init__(self, profile_result: ProfileResult):
        self.profile = profile_result
        self.bottlenecks = []
    
    def analyze(self) -> List[Bottleneck]:
        """Find bottlenecks"""
        self.bottlenecks.clear()
        
        # Find hot instructions
        total = self.profile.total_instructions
        for instr, count in self.profile.instruction_counts.items():
            percentage = (count / total) * 100
            if percentage > 20:  # More than 20% of time
                self.bottlenecks.append(Bottleneck(
                    type="hot_instruction",
                    location=instr,
                    percentage=percentage,
                    suggestion=f"Optimize {instr} operation"
                ))
        
        # Find memory hotspots
        for addr, count in self.profile.address_counts.items():
            percentage = (count / total) * 100
            if percentage > 10:  # More than 10% of time
                self.bottlenecks.append(Bottleneck(
                    type="hot_address",
                    location=f"address:{addr}",
                    percentage=percentage,
                    suggestion=f"Address {addr} executed {count} times"
                ))
        
        # Check cycle efficiency
        if self.profile.cycle_estimate > 100000:
            self.bottlenecks.append(Bottleneck(
                type="high_cycles",
                location="overall",
                percentage=(self.profile.cycle_estimate / 1000000) * 100,
                suggestion="Consider algorithmic optimization"
            ))
        
        return sorted(self.bottlenecks, key=lambda b: b.percentage, reverse=True)
```

#### 4.2.3 Optimization Suggestions (200-300 lines)

```python
class OptimizationAdvisor:
    """Provide optimization suggestions"""
    
    def __init__(self, session: DebuggerSession, profile_result: ProfileResult):
        self.session = session
        self.profile = profile_result
    
    def get_suggestions(self) -> List[str]:
        """Get optimization suggestions"""
        suggestions = []
        
        # Check for common inefficiencies
        if 'mul' in self.profile.instruction_counts:
            if self.profile.instruction_counts['mul'] > 100:
                suggestions.append(
                    "High multiplication count - consider bit shifting or lookup tables"
                )
        
        if 'div' in self.profile.instruction_counts:
            if self.profile.instruction_counts['div'] > 50:
                suggestions.append(
                    "High division count - division is expensive, use multiplication"
                )
        
        if 'load' in self.profile.instruction_counts:
            if self.profile.instruction_counts['load'] > 500:
                suggestions.append(
                    "High memory load count - cache frequently used values in registers"
                )
        
        if self.profile.memory_peak > 1500:
            suggestions.append(
                f"High memory usage ({self.profile.memory_peak}/2000) - optimize data structures"
            )
        
        return suggestions
```

#### 4.2.4 Report Generation (200-300 lines)

```python
class ProfileReport:
    """Generate performance report"""
    
    def __init__(self, session: DebuggerSession, profile_result: ProfileResult):
        self.session = session
        self.profile = profile_result
        self.analyzer = BottleneckAnalyzer(profile_result)
        self.advisor = OptimizationAdvisor(session, profile_result)
    
    def generate_report(self) -> str:
        """Generate complete performance report"""
        lines = [
            "=== BABBAGE EXECUTION PROFILE ===",
            "",
            f"Total Instructions Executed: {self.profile.total_instructions}",
            f"Estimated Cycles: {self.profile.cycle_estimate}",
            f"Peak Memory Usage: {self.profile.memory_peak}/2000 words",
            "",
            "=== INSTRUCTION DISTRIBUTION ===",
        ]
        
        total = self.profile.total_instructions
        for instr in sorted(self.profile.instruction_counts, 
                           key=lambda x: self.profile.instruction_counts[x], 
                           reverse=True):
            count = self.profile.instruction_counts[instr]
            percentage = (count / total) * 100
            lines.append(f"  {instr:10s}: {count:6d} ({percentage:5.1f}%)")
        
        bottlenecks = self.analyzer.analyze()
        if bottlenecks:
            lines.extend(["", "=== BOTTLENECKS ==="])
            for bn in bottlenecks[:5]:  # Top 5
                lines.append(f"  {bn.type:20s}: {bn.percentage:5.1f}% - {bn.suggestion}")
        
        suggestions = self.advisor.get_suggestions()
        if suggestions:
            lines.extend(["", "=== OPTIMIZATION SUGGESTIONS ==="])
            for suggestion in suggestions:
                lines.append(f"  • {suggestion}")
        
        return '\n'.join(lines)
```

#### Testing (100-200 lines)
**File**: `backend/src/profiler/test_profiler.py`

Test cases:
- Instruction counting
- Cycle estimation
- Memory tracking
- Bottleneck detection
- Optimization suggestions

**Test Target**: 15+ tests, 100% pass rate

---

## Integration Architecture

### Phase 3 System Architecture

```
┌─────────────────────────────────────────────────────┐
│                  Compiler Pipeline                  │
│  Source → Lexer → Parser → Compiler → Assembly     │
└──────────────────────┬──────────────────────────────┘
                       │
                       ↓
┌──────────────────────────────────────────────────────┐
│              Babbage Emulator System                 │
├──────────────────────────────────────────────────────┤
│                                                      │
│  ┌─────────────────────────────────────────────┐   │
│  │    Execution Engine                         │   │
│  │  • Register File (A, B, C, D)              │   │
│  │  • Memory (2000 words)                     │   │
│  │  • Instruction Decoder (25+ instructions) │   │
│  │  • Flag Management                         │   │
│  └──────────────────┬──────────────────────────┘   │
│                     │                               │
│  ┌──────────────────┴──────────────────────────┐   │
│  │    I/O System                               │   │
│  │  • Console I/O (stdin/stdout)              │   │
│  │  • File System (sandboxed)                 │   │
│  │  • Syscall Handler                        │   │
│  └──────────────────────────────────────────────┘   │
│                                                      │
│  ┌──────────────────────────────────────────────┐   │
│  │    Debugger                                  │   │
│  │  • Breakpoint Manager                       │   │
│  │  • Debugger Session (run/step/continue)    │   │
│  │  • Interactive REPL                         │   │
│  │  • Execution Visualizer                     │   │
│  └──────────────────────────────────────────────┘   │
│                                                      │
│  ┌──────────────────────────────────────────────┐   │
│  │    Performance Analysis                      │   │
│  │  • Execution Profiler                        │   │
│  │  • Bottleneck Analyzer                       │   │
│  │  • Optimization Advisor                      │   │
│  │  • Report Generator                          │   │
│  └──────────────────────────────────────────────┘   │
│                                                      │
└──────────────────────────────────────────────────────┘
                       │
                       ↓
┌──────────────────────────────────────────────────────┐
│                  Output System                       │
│  • stdout/stderr                                     │
│  • Execution trace                                   │
│  • Performance report                                │
│  • Visualization                                     │
└──────────────────────────────────────────────────────┘
```

### API Endpoints (Phase 3)

```python
# Execute with emulation and full debugging
POST /execute/debug
  request: ExecutionRequest + debug_options
  response: ExecutionResponse + execution_trace + state_snapshots

# Get execution trace
GET /executions/{execution_id}/trace
  response: execution trace (instruction-by-instruction)

# Interactive debugging API
WebSocket /debug/{execution_id}
  protocol: JSON-RPC commands (run, step, break, etc.)

# Performance report
GET /executions/{execution_id}/profile
  response: ProfileReport (instructions, bottlenecks, suggestions)

# Emulator state
GET /debug/{execution_id}/state
  response: ExecutionState (registers, memory, program counter, flags)
```

---

## Implementation Timeline

### Week 13: Babbage Emulator (3-4 days)
- Register file and memory system
- Instruction decoder (25+ instructions)
- Execution engine with state tracking
- Basic error handling
- Test suite (30+ tests)

### Week 14: I/O System (2-3 days)
- Console I/O (stdin/stdout)
- File operations (open, read, write, close)
- Syscall handler
- Sandbox security
- Test suite (25+ tests)

### Week 15: Debugger (3-4 days)
- Breakpoint manager
- Debugger session (run, step, continue)
- Interactive REPL
- State visualization
- Test suite (25+ tests)

### Week 16: Performance Analysis (2-3 days)
- Execution profiler
- Bottleneck analyzer
- Optimization advisor
- Report generation
- Test suite (15+ tests)

### Week 17-18: Integration & Polish (3-4 days)
- API endpoint integration
- WebSocket debugging protocol
- Complete test suite (100+ tests total)
- Documentation and examples
- Performance optimization

---

## Success Criteria

✓ All 4 components implemented (Emulator, I/O, Debugger, Profiler)
✓ 100+ tests passing (100% pass rate)
✓ Zero compiler warnings
✓ Complete execution simulation with I/O
✓ Interactive debugging working
✓ Performance analysis tools functional
✓ API endpoints fully integrated
✓ Documentation complete (user guides, API reference)
✓ Example programs demonstrating all features
✓ Phase 3 completion target: 6,000-8,000 lines

---

## References

- Babbage ISA Specification: backend/src/ir_types.py
- Compiler Pipeline: WEEK_8_PHASE_3_COMPLETION_SUMMARY.md
- Phase 2 Architecture: OPTION_B_IMPLEMENTATION_ROADMAP.md
- Testing Patterns: test_haskell_compiler.py

---

*End of Option C Phase 3 Vision*

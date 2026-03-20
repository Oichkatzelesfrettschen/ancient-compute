import sys
import os

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from backend.src.emulator.analytical_engine import Engine, BabbageNumber

def test_micro_mult():
    print("Testing Micro-Programmed Multiplication...")
    engine = Engine()
    
    # 10 * 3
    engine.registers['A'] = BabbageNumber(10)
    # Multiplication instruction
    from backend.src.emulator.analytical_engine import Instruction
    
    # We'll use the internal micro-programmed handler directly to observe steps
    # Or just use the high level op
    engine.execute_instruction(Instruction('MULT', ['A', '3']))
    
    result = engine.registers['A'].to_decimal()
    print(f"Result of 10 * 3: {result}")
    
    if result == 30.0:
        print("SUCCESS: Micro-programmed MULT (units digit) verified.")
    else:
        print(f"FAILURE: Expected 30.0, got {result}")

if __name__ == "__main__":
    test_micro_mult()

import sys
import os
import time

# Add project root to path
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from backend.src.emulator.carry import CarryPropagationUnit

def benchmark():
    print("Benchmarking Carry Propagation...")
    
    # Worst case: All columns generating carries
    inputs = [True] * 8
    
    # Test Sequential
    seq_unit = CarryPropagationUnit("sequential")
    start = time.perf_counter()
    # Run enough times to measure Python overhead diff (though we care about steps)
    for _ in range(100000):
        seq_unit.propagate(inputs)
    end = time.perf_counter()
    seq_time = end - start
    seq_steps = seq_unit.get_step_count()
    
    # Test Lookahead
    look_unit = CarryPropagationUnit("lookahead")
    start = time.perf_counter()
    for _ in range(100000):
        look_unit.propagate(inputs)
    end = time.perf_counter()
    look_time = end - start
    look_steps = look_unit.get_step_count()
    
    print(f"Sequential Mode: {seq_time:.6f}s for 100k ops (Simulated Steps per op: {seq_steps})")
    print(f"Lookahead Mode:  {look_time:.6f}s for 100k ops (Simulated Steps per op: {look_steps})")
    
    speedup = seq_steps / look_steps if look_steps > 0 else 0
    print(f"Theoretical Mechanical Speedup: {speedup}x")

if __name__ == "__main__":
    benchmark()

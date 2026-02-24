import sys
import os

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from backend.src.emulator.analytical_engine import BabbageNumber

def logistic_map_demo():
    print("Logistic Map Precision Test: BabbageNumber vs Float")
    
    r_val = 3.9
    x0_val = 0.51  # Slightly off 0.5 to trigger chaos
    iterations = 30
    
    # Python Float
    r_f = r_val
    x_f = x0_val
    print(f"\n--- Python Float (64-bit IEEE 754) ---")
    print(f"Start: {x_f}")
    for i in range(iterations):
        x_f = r_f * x_f * (1.0 - x_f)
        if i >= 25:
            print(f"Iter {i}: {x_f:.20f}")
            
    # BabbageNumber
    print(f"\n--- BabbageNumber (50-digit fixed point) ---")
    r_b = BabbageNumber(r_val)
    x_b = BabbageNumber(x0_val)
    one_b = BabbageNumber(1)
    
    print(f"Start: {x_b.to_decimal()}")
    
    for i in range(iterations):
        # x_{n+1} = r * x * (1 - x)
        term = one_b - x_b
        x_b = r_b * x_b * term
        
        if i >= 25:
            print(f"Iter {i}: {x_b.to_decimal()}")

if __name__ == "__main__":
    logistic_map_demo()

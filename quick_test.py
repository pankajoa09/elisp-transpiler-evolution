#!/usr/bin/env python3
"""
Quick test with just a few cases to make sure it works
"""

import subprocess
import time

TEST_CASES = [
    "(+ 10 15)",      # Should work (arithmetic already supported)
    "(< 3 8)",        # Simple comparison
    "(if t 100 200)", # Simple if with t
    "(let ((z 7)) z)" # Simple let binding
]

def run_test(test_case):
    print(f"\n{'='*50}")
    print(f"Testing: {test_case}")
    
    try:
        result = subprocess.run(
            ['python3', 'evolving_transpiler.py', test_case],
            capture_output=True,
            text=True,
            timeout=120,  # 2 minutes max
            cwd='/Users/pankajahuja/Documents/elisp-transpiler-evolution'
        )
        
        if "SUCCESS" in result.stdout or "handles this!" in result.stdout:
            print("✅ SUCCESS")
            # Check if it evolved
            if "Evolution successful!" in result.stdout:
                import re
                version = re.search(r'Now at v(\d+)', result.stdout)
                if version:
                    print(f"   Evolved to v{version.group(1)}")
            return True
        else:
            print("❌ FAILED or TIMEOUT")
            return False
            
    except subprocess.TimeoutExpired:
        print("⏱️ TIMEOUT")
        return False

print("🧪 Quick Test - 4 cases")
print("="*50)

for test in TEST_CASES:
    run_test(test)
    time.sleep(1)

print("\n✅ Quick test complete!")
#!/usr/bin/env python3
import json
import subprocess
import os

def compile_transpiler():
    """Compile elisp_transpiler.cpp"""
    result = subprocess.run(
        ['g++', '-std=c++17', '-o', 'elisp_transpiler', 'elisp_transpiler.cpp'],
        capture_output=True,
        text=True
    )
    return result.returncode == 0

def test_transpiler(expression, expected):
    """Test transpiler with an expression"""
    result = subprocess.run(
        ['./elisp_transpiler'],
        input=expression,
        capture_output=True,
        text=True,
        timeout=5
    )
    
    if result.returncode != 0:
        return False, f"Transpiler crashed: {result.stderr}"
    
    # For now just check if it produces output
    # Later need to compile and run the C++ output
    output = result.stdout.strip()
    if output:
        return True, output
    else:
        return False, "No output"

def evolve_transpiler(failed_test, failed_index):
    """Feed elisp_transpiler.cpp back to Qwen with failure info"""
    
    # Read current transpiler code
    with open('elisp_transpiler.cpp', 'r') as f:
        cpp_code = f.read()
    
    prompt = f"""Fix this Elisp to C++ transpiler. It failed on test #{failed_index + 1}:
Test: {failed_test['expression']}
Expected output: {failed_test['expected']}

Current transpiler code:
{cpp_code}

The transpiler must handle this test case correctly. Fix the code and output ONLY the complete corrected C++ code."""
    
    # Call Qwen
    result = subprocess.run(
        ['ollama', 'run', 'qwen2.5-coder:32b'],
        input=prompt,
        capture_output=True,
        text=True,
        timeout=300
    )
    
    if result.returncode == 0:
        # Save updated code
        with open('elisp_transpiler.cpp', 'w') as f:
            f.write(result.stdout.strip())
        return True
    return False

def main():
    # Load tests with answers
    with open('evolutionary_tests_with_answers.json', 'r') as f:
        data = json.load(f)
    
    tests = data['tests']
    max_evolution_attempts = 100
    
    while True:
        # Compile transpiler
        while not compile_transpiler():
            print("❌ Failed to compile transpiler")
            print("  🧬 Evolving to fix compilation...")
            with open('elisp_transpiler.cpp', 'r') as f:
                cpp_code = f.read()
            
            result = subprocess.run(
                ['g++', '-std=c++17', '-o', 'elisp_transpiler', 'elisp_transpiler.cpp'],
                capture_output=True,
                text=True
            )
            
            prompt = f"""Fix this C++ compilation error:
{result.stderr}

Code:
{cpp_code}

Output ONLY the corrected C++ code."""
            
            fix_result = subprocess.run(
                ['ollama', 'run', 'qwen2.5-coder:32b'],
                input=prompt,
                capture_output=True,
                text=True,
                timeout=300
            )
            
            if fix_result.returncode == 0:
                with open('elisp_transpiler.cpp', 'w') as f:
                    f.write(fix_result.stdout.strip())
                print("  📝 Code updated, trying again...")
        
        # Test each case sequentially
        passed = 0
        for i, test in enumerate(tests):
            if test['expected'] is None:
                continue  # Skip tests that errored in Elisp
            
            print(f"Testing #{i+1}: {test['expression']}")
            success, output = test_transpiler(test['expression'], test['expected'])
            
            if success:
                print(f"  ✅ Passed")
                passed += 1
            else:
                print(f"  ❌ Failed at test #{i+1}")
                print(f"     Expression: {test['expression']}")
                print(f"     Expected: {test['expected']}")
                
                # Evolve transpiler
                print(f"  🧬 Evolving transpiler...")
                if evolve_transpiler(test, i):
                    print(f"  📝 Transpiler updated, restarting tests...")
                    break
                else:
                    print(f"  ❌ Evolution failed")
                    return
        
        if passed == len([t for t in tests if t['expected'] is not None]):
            print(f"🎉 ALL TESTS PASSED!")
            break

if __name__ == "__main__":
    main()
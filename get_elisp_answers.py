#!/usr/bin/env python3
import json
import subprocess

# Load tests
with open('evolutionary_tests_fixed.json', 'r') as f:
    data = json.load(f)

tests_with_answers = []

for i, test in enumerate(data['tests'], 1):
    print(f"Test {i}: {test}")
    
    # Run through Emacs Elisp interpreter
    try:
        result = subprocess.run(
            ['emacs', '--batch', '--eval', f'(princ {test})'],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        if result.returncode == 0:
            answer = result.stdout.strip()
            print(f"  Answer: {answer}")
            tests_with_answers.append({
                "expression": test,
                "expected": answer
            })
        else:
            print(f"  Error: {result.stderr}")
            tests_with_answers.append({
                "expression": test,
                "expected": None,
                "error": result.stderr
            })
    except subprocess.TimeoutExpired:
        print("  Timeout!")
        tests_with_answers.append({
            "expression": test,
            "expected": None,
            "error": "timeout"
        })
    except Exception as e:
        print(f"  Exception: {e}")
        tests_with_answers.append({
            "expression": test,
            "expected": None,
            "error": str(e)
        })

# Save results
with open('evolutionary_tests_with_answers.json', 'w') as f:
    json.dump({"tests": tests_with_answers}, f, indent=2)

print(f"\n✅ Processed {len(tests_with_answers)} tests")
print("Saved to evolutionary_tests_with_answers.json")
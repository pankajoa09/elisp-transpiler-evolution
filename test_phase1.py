#!/usr/bin/env python3
import json
import subprocess

with open('phase1_tests.json', 'r') as f:
    data = json.load(f)

tests = data['tests']
passed = 0
failed = 0
errors = []

for i, test in enumerate(tests, 1):
    name = test['name']
    desc = test.get('description', '')
    expr = test['expression']
    expected = test['expected']

    try:
        # Run transpiler
        transpiler_proc = subprocess.run(
            ['./elisp_transpiler'],
            input=expr,
            capture_output=True,
            text=True,
            timeout=2
        )

        if transpiler_proc.returncode != 0:
            print(f"{i}. FAIL [{name}]: Transpiler error")
            errors.append((name, "transpiler_error", transpiler_proc.stderr))
            failed += 1
            continue

        # Compile C++
        with open('test_output.cpp', 'w') as f:
            f.write(transpiler_proc.stdout)

        compile_proc = subprocess.run(
            ['g++', '-std=c++17', 'test_output.cpp', '-o', 'test_output'],
            capture_output=True,
            timeout=5
        )

        if compile_proc.returncode != 0:
            print(f"{i}. FAIL [{name}]: Compile error")
            error_msg = compile_proc.stderr.decode()[:200]
            errors.append((name, "compile_error", error_msg))
            failed += 1
            continue

        # Run
        run_proc = subprocess.run(
            ['./test_output'],
            capture_output=True,
            text=True,
            timeout=2
        )

        result = run_proc.stdout.strip()

        if result == expected:
            print(f"{i}. PASS [{name}] - {desc}")
            passed += 1
        else:
            print(f"{i}. FAIL [{name}]: Expected '{expected}', got '{result}'")
            errors.append((name, "wrong_output", f"Expected: {expected}, Got: {result}"))
            failed += 1

    except Exception as e:
        print(f"{i}. FAIL [{name}]: Exception - {e}")
        errors.append((name, "exception", str(e)))
        failed += 1

print(f"\n{'='*70}")
print(f"Phase 1 Feature Tests")
print(f"Passed: {passed}/{passed+failed}")
print(f"Success rate: {100*passed/(passed+failed) if passed+failed > 0 else 0:.1f}%")

if errors:
    print(f"\n{'='*70}")
    print("ERRORS:")
    for name, error_type, details in errors:
        print(f"\n{name} ({error_type}):")
        print(f"  {details}")

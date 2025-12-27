#!/usr/bin/env python3
import json
import subprocess

with open('additional_tests.json', 'r') as f:
    data = json.load(f)

tests = data['tests']
passed = 0
failed = 0

for i, test in enumerate(tests, 1):
    name = test['name']
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
            print(f"{i}. PASS [{name}]")
            passed += 1
        else:
            print(f"{i}. FAIL [{name}]: Expected {expected}, got {result}")
            failed += 1

    except Exception as e:
        print(f"{i}. FAIL [{name}]: {e}")
        failed += 1

print(f"\n{'='*50}")
print(f"Passed: {passed}/{passed+failed}")
print(f"Success rate: {100*passed/(passed+failed) if passed+failed > 0 else 0:.1f}%")

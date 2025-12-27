#!/usr/bin/env python3
import json
import subprocess
import sys

with open('evolutionary_tests_with_answers.json', 'r') as f:
    data = json.load(f)

tests = data['tests']
passed = 0
failed = 0
skipped = 0
failures = []

for i, test in enumerate(tests, 1):
    expr = test['expression']
    expected = test.get('expected')

    if expected is None:
        print(f"Test {i}: SKIP (expected null): {expr[:50]}...")
        skipped += 1
        continue

    # Run transpiler
    try:
        transpiler_proc = subprocess.run(
            ['./elisp_transpiler'],
            input=expr,
            capture_output=True,
            text=True,
            timeout=2
        )

        if transpiler_proc.returncode != 0:
            print(f"Test {i}: FAIL (transpiler error): {expr[:50]}...")
            failures.append((i, expr, expected, "transpiler_error", transpiler_proc.stderr))
            failed += 1
            continue

        cpp_code = transpiler_proc.stdout

        # Compile C++
        with open('test_output.cpp', 'w') as f:
            f.write(cpp_code)

        compile_proc = subprocess.run(
            ['g++', '-std=c++17', 'test_output.cpp', '-o', 'test_output'],
            capture_output=True,
            timeout=5
        )

        if compile_proc.returncode != 0:
            print(f"Test {i}: FAIL (compile error): {expr[:50]}...")
            failures.append((i, expr, expected, "compile_error", compile_proc.stderr.decode()[:200]))
            failed += 1
            continue

        # Run compiled code
        run_proc = subprocess.run(
            ['./test_output'],
            capture_output=True,
            text=True,
            timeout=2
        )

        result = run_proc.stdout.strip()

        if result == expected:
            print(f"Test {i}: PASS: {expr[:50]}...")
            passed += 1
        else:
            print(f"Test {i}: FAIL (wrong output): {expr[:50]}...")
            failures.append((i, expr, expected, "wrong_output", f"Expected: {expected}, Got: {result}"))
            failed += 1

    except subprocess.TimeoutExpired:
        print(f"Test {i}: FAIL (timeout): {expr[:50]}...")
        failures.append((i, expr, expected, "timeout", ""))
        failed += 1
    except Exception as e:
        print(f"Test {i}: FAIL (exception): {expr[:50]}...")
        failures.append((i, expr, expected, "exception", str(e)))
        failed += 1

print(f"\n{'='*70}")
print(f"Total: {len(tests)} tests")
print(f"Passed: {passed}")
print(f"Failed: {failed}")
print(f"Skipped (expected null): {skipped}")
print(f"Success rate: {passed}/{passed+failed} ({100*passed/(passed+failed) if passed+failed > 0 else 0:.1f}%)")

if failures:
    print(f"\n{'='*70}")
    print("FAILURES:")
    for test_num, expr, expected, error_type, details in failures:
        print(f"\nTest {test_num}: {expr}")
        print(f"  Expected: {expected}")
        print(f"  Error: {error_type}")
        if details:
            print(f"  Details: {details[:150]}")

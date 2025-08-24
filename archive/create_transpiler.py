#!/usr/bin/env python3
"""
Ask Qwen Coder to write a complete Elisp to C++ transpiler
"""

import subprocess
import sys

prompt = """Write a complete C++ program that implements an Elisp to C++ transpiler.

The program should:
1. Parse Elisp S-expressions
2. Convert Elisp constructs to equivalent C++ code
3. Handle core Elisp features:
   - defun (function definitions)
   - setq (variable assignment)
   - arithmetic operators (+, -, *, /)
   - comparison operators (<, >, <=, >=, =)
   - if/else conditionals
   - let bindings
   - recursion
   - basic data types (integers, strings, lists)

The main function should:
- Accept Elisp code as input (from stdin or command line)
- Output equivalent C++ code

Example transformations:
- (defun add (a b) (+ a b)) → int add(int a, int b) { return a + b; }
- (setq x 5) → int x = 5;
- (if (> a b) a b) → (a > b) ? a : b

Write the COMPLETE implementation with all parsing logic, AST representation, and code generation.
Make it production-ready and handle edge cases.
Output ONLY the C++ code, no explanations."""

print("🤖 Asking Qwen Coder to write a complete Elisp→C++ transpiler...")
print("=" * 60)

try:
    result = subprocess.run(
        ["ollama", "run", "qwen2.5-coder:32b"],
        input=prompt,
        capture_output=True,
        text=True,
        timeout=120
    )
    
    if result.returncode == 0:
        # Save the transpiler code
        with open("elisp_transpiler.cpp", "w") as f:
            f.write(result.stdout)
        print("✅ Transpiler code generated and saved to elisp_transpiler.cpp")
        print("\nFirst 50 lines of generated code:")
        print("-" * 40)
        lines = result.stdout.split('\n')[:50]
        for line in lines:
            print(line)
        print("-" * 40)
        print(f"\nTotal lines: {len(result.stdout.split(chr(10)))}")
        
        # Try to compile it
        print("\n🔨 Attempting to compile the transpiler...")
        compile_result = subprocess.run(
            ["g++", "-std=c++17", "-o", "elisp_transpiler", "elisp_transpiler.cpp"],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if compile_result.returncode == 0:
            print("✅ Transpiler compiled successfully!")
            print("You can now use: ./elisp_transpiler")
        else:
            print("❌ Compilation failed:")
            print(compile_result.stderr[:500])
    else:
        print("❌ Failed to generate transpiler")
        print(result.stderr)

except subprocess.TimeoutExpired:
    print("⏱️ Timeout - Qwen is taking too long")
except Exception as e:
    print(f"❌ Error: {e}")
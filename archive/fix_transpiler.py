#!/usr/bin/env python3
"""
Use Qwen to fix compilation errors in the transpiler
"""

import subprocess

# Read the current transpiler code
with open("elisp_transpiler.cpp", "r") as f:
    current_code = f.read()

# Get the compilation errors
compile_result = subprocess.run(
    ["g++", "-std=c++17", "-o", "elisp_transpiler", "elisp_transpiler.cpp"],
    capture_output=True,
    text=True
)

errors = compile_result.stderr

prompt = f"""Fix the following C++ code compilation errors:

ERRORS:
{errors}

CURRENT CODE:
{current_code}

Please output the COMPLETE FIXED C++ code. Do not use markdown formatting, just plain C++ code."""

print("🔧 Asking Qwen to fix the compilation errors...")
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
        # Save the fixed code
        with open("elisp_transpiler_fixed.cpp", "w") as f:
            f.write(result.stdout)
        print("✅ Fixed code saved to elisp_transpiler_fixed.cpp")
        
        # Try to compile the fixed version
        print("\n🔨 Attempting to compile the fixed version...")
        compile_result = subprocess.run(
            ["g++", "-std=c++17", "-o", "elisp_transpiler", "elisp_transpiler_fixed.cpp"],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if compile_result.returncode == 0:
            print("✅ Fixed transpiler compiled successfully!")
            print("\nTesting with simple example...")
            test = subprocess.run(
                ["echo", "(defun add (a b) (+ a b))"],
                capture_output=True,
                text=True
            )
            test_result = subprocess.run(
                ["./elisp_transpiler"],
                input="(defun add (a b) (+ a b))",
                capture_output=True,
                text=True
            )
            print("Input: (defun add (a b) (+ a b))")
            print(f"Output: {test_result.stdout}")
        else:
            print("❌ Still has compilation errors:")
            print(compile_result.stderr[:500])
    else:
        print("❌ Qwen failed to generate fix")

except Exception as e:
    print(f"❌ Error: {e}")
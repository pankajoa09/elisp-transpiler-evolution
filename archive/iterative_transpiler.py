#!/usr/bin/env python3
"""
Iteratively build a transpiler with Qwen until it works
"""

import subprocess
import tempfile
import os

# Start with the simplest possible test case
TEST_ELISP = "(+ 1 2)"
EXPECTED_OUTPUT = "3"

iteration = 0
max_iterations = 10

print(f"🎯 Goal: Create a transpiler that converts '{TEST_ELISP}' to C++ that outputs '{EXPECTED_OUTPUT}'")
print("=" * 70)

def test_transpiler(cpp_code):
    """Compile and test the transpiler"""
    # Save the C++ code
    with open("transpiler.cpp", "w") as f:
        f.write(cpp_code)
    
    # Try to compile
    compile_result = subprocess.run(
        ["g++", "-std=c++17", "-o", "transpiler", "transpiler.cpp"],
        capture_output=True,
        text=True,
        timeout=10
    )
    
    if compile_result.returncode != 0:
        return {"success": False, "error": f"Compilation failed:\n{compile_result.stderr[:500]}"}
    
    # Try to run with test input
    try:
        run_result = subprocess.run(
            ["./transpiler"],
            input=TEST_ELISP,
            capture_output=True,
            text=True,
            timeout=5
        )
        
        # Now compile and run the generated C++ code
        with open("generated.cpp", "w") as f:
            f.write(run_result.stdout)
        
        compile_gen = subprocess.run(
            ["g++", "-std=c++17", "-o", "generated", "generated.cpp"],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if compile_gen.returncode != 0:
            return {"success": False, "error": f"Generated C++ failed to compile:\n{compile_gen.stderr[:500]}"}
        
        # Run the generated code
        final_result = subprocess.run(
            ["./generated"],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        output = final_result.stdout.strip()
        
        if output == EXPECTED_OUTPUT:
            return {"success": True, "output": output}
        else:
            return {"success": False, "error": f"Wrong output: got '{output}', expected '{EXPECTED_OUTPUT}'"}
            
    except Exception as e:
        return {"success": False, "error": str(e)}

# Initial prompt
prompt = f"""Write a C++ program that:
1. Reads the Elisp expression: {TEST_ELISP}
2. Transpiles it to C++ code
3. The generated C++ code when compiled and run should output: {EXPECTED_OUTPUT}

The transpiler should output a complete C++ program with a main() function that prints {EXPECTED_OUTPUT}.

Start simple - just handle the (+ 1 2) case specifically if needed.
Output ONLY the C++ code for the transpiler, no explanations."""

cpp_code = ""
feedback = ""

while iteration < max_iterations:
    iteration += 1
    print(f"\n📝 Iteration {iteration}")
    
    if iteration > 1:
        # Add feedback from previous attempt
        prompt = f"""Fix this C++ transpiler code:

PREVIOUS CODE:
{cpp_code}

PROBLEM:
{feedback}

The transpiler must:
1. Read the Elisp expression: {TEST_ELISP}
2. Output C++ code that prints: {EXPECTED_OUTPUT}

Fix the issue and output the COMPLETE corrected C++ transpiler code."""
    
    # Ask Qwen
    print("   🤖 Asking Qwen...")
    result = subprocess.run(
        ["ollama", "run", "qwen2.5-coder:32b"],
        input=prompt,
        capture_output=True,
        text=True,
        timeout=60
    )
    
    if result.returncode != 0:
        print("   ❌ Qwen failed")
        break
    
    cpp_code = result.stdout.strip()
    # Remove markdown if present
    cpp_code = cpp_code.replace("```cpp\n", "").replace("```c++\n", "").replace("\n```", "").replace("```", "")
    
    # Test it
    print("   🧪 Testing...")
    test_result = test_transpiler(cpp_code)
    
    if test_result["success"]:
        print(f"   ✅ SUCCESS! Output: {test_result['output']}")
        print("\n🎉 Transpiler works!")
        print(f"Took {iteration} iterations")
        
        # Save the working transpiler
        with open("working_transpiler.cpp", "w") as f:
            f.write(cpp_code)
        print("Saved to working_transpiler.cpp")
        break
    else:
        print(f"   ❌ Failed: {test_result['error'][:200]}")
        feedback = test_result["error"]

if iteration >= max_iterations:
    print(f"\n😞 Couldn't get it working after {max_iterations} iterations")
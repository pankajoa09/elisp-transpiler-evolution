#!/usr/bin/env python3
"""
Elisp to C++ Transpiler using Qwen Coder
Executes Elisp code, generates test cases, and produces equivalent C++ code
"""

import os
import sys
import json
import subprocess
import tempfile
import argparse
import re
from typing import Dict, List, Tuple, Optional

class ElispExecutor:
    """Execute Elisp code and capture output"""
    
    def __init__(self):
        self.emacs_path = "/opt/homebrew/bin/emacs"
    
    def run_elisp(self, code: str) -> Dict:
        """Run Elisp code and return results"""
        # Wrap code to capture output and return value
        wrapped_code = f"""
(let ((output "")
      (result nil))
  (setq result (progn {code}))
  (princ (format "RESULT: %S" result)))
"""
        
        try:
            result = subprocess.run(
                [self.emacs_path, "--batch", "--eval", wrapped_code],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            output = result.stdout
            error = result.stderr
            
            # Parse result
            result_match = re.search(r"RESULT: (.+)$", output)
            return_value = result_match.group(1) if result_match else "nil"
            
            # Clean output
            clean_output = re.sub(r"RESULT: .+$", "", output).strip()
            
            return {
                "success": result.returncode == 0,
                "output": clean_output,
                "return": return_value,
                "error": error
            }
        except subprocess.TimeoutExpired:
            return {"success": False, "error": "Execution timeout"}
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    def extract_function_info(self, code: str) -> Optional[Dict]:
        """Extract function name and parameters from defun"""
        defun_match = re.match(r'\(defun\s+(\w+)\s*\(([^)]*)\)', code)
        if defun_match:
            name = defun_match.group(1)
            params = [p.strip() for p in defun_match.group(2).split() if p.strip()]
            return {"name": name, "params": params}
        return None
    
    def generate_test_cases(self, code: str) -> List[Dict]:
        """Generate test cases by running the function with different inputs"""
        func_info = self.extract_function_info(code)
        if not func_info:
            return []
        
        test_cases = []
        test_inputs = []
        
        # Generate test inputs based on number of parameters
        if len(func_info["params"]) == 0:
            test_inputs = [[]]
        elif len(func_info["params"]) == 1:
            test_inputs = [[1], [5], [10], [0], [-5]]
        elif len(func_info["params"]) == 2:
            test_inputs = [[1, 2], [5, 3], [10, 20], [0, 0], [-5, 5]]
        else:
            # Generic test cases for functions with more params
            test_inputs = [[i for i in range(1, len(func_info["params"]) + 1)]]
        
        for inputs in test_inputs:
            # First define the function, then call it
            call = f"({func_info['name']} {' '.join(map(str, inputs))})"
            full_code = f"{code}\n{call}"
            
            result = self.run_elisp(full_code)
            if result["success"]:
                test_cases.append({
                    "inputs": inputs,
                    "expected_output": result["output"],
                    "expected_return": result["return"]
                })
        
        return test_cases


class QwenCoderTranspiler:
    """Use Qwen Coder to transpile Elisp to C++"""
    
    def __init__(self):
        self.model = "qwen2.5-coder:32b"
        self.examples = self.load_examples()
    
    def load_examples(self) -> str:
        """Load example Elisp to C++ mappings"""
        return """
Examples of Elisp to C++ mappings:

1. Simple arithmetic function:
Elisp: (defun add (a b) (+ a b))
C++: int add(int a, int b) { return a + b; }

2. Factorial:
Elisp: (defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
C++: int factorial(int n) { return (n <= 1) ? 1 : n * factorial(n - 1); }

3. List operations:
Elisp: (defun sum-list (lst) (apply '+ lst))
C++: int sum_list(std::vector<int> lst) { return std::accumulate(lst.begin(), lst.end(), 0); }
"""
    
    def create_prompt(self, elisp_code: str, test_cases: List[Dict]) -> str:
        """Create prompt for Qwen Coder"""
        prompt = f"""{self.examples}

Task: Transpile the following Elisp code to C++.

Elisp code:
{elisp_code}

Test cases (the C++ function must produce these exact results):
"""
        for i, tc in enumerate(test_cases, 1):
            inputs_str = ", ".join(map(str, tc["inputs"]))
            prompt += f"\nTest {i}: Input({inputs_str}) → Output: {tc['expected_return']}"
        
        prompt += """

Generate a complete C++ program with:
1. The transpiled function
2. A main() function that runs all test cases
3. #include statements for all needed headers
4. Use modern C++ (C++17 or later)
5. Print test results in format: "Test N: PASS" or "Test N: FAIL"

Output ONLY the C++ code, no explanations."""
        
        return prompt
    
    def transpile(self, elisp_code: str, test_cases: List[Dict]) -> Optional[str]:
        """Send to Qwen Coder and get C++ code"""
        prompt = self.create_prompt(elisp_code, test_cases)
        
        try:
            # Use stdin to send the prompt to ollama
            result = subprocess.run(
                ["ollama", "run", self.model],
                input=prompt,
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                # Extract C++ code from response
                cpp_code = result.stdout.strip()
                # Remove any markdown code blocks if present
                cpp_code = re.sub(r'```c\+\+\n', '', cpp_code, flags=re.MULTILINE)
                cpp_code = re.sub(r'```cpp\n', '', cpp_code, flags=re.MULTILINE)
                cpp_code = re.sub(r'```\n', '', cpp_code, flags=re.MULTILINE)
                cpp_code = re.sub(r'\n```', '', cpp_code, flags=re.MULTILINE)
                return cpp_code
            else:
                print(f"Qwen Coder error: {result.stderr}")
                return None
                
        except subprocess.TimeoutExpired:
            print("Qwen Coder timeout")
            return None
        except Exception as e:
            print(f"Error calling Qwen Coder: {e}")
            return None


class CppTester:
    """Compile and test C++ code"""
    
    def compile_and_run(self, cpp_code: str) -> Dict:
        """Compile C++ code and run it"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
            f.write(cpp_code)
            cpp_file = f.name
        
        exe_file = cpp_file.replace('.cpp', '')
        
        try:
            # Compile
            compile_result = subprocess.run(
                ["g++", "-std=c++17", "-o", exe_file, cpp_file],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if compile_result.returncode != 0:
                os.unlink(cpp_file)
                return {
                    "success": False,
                    "error": f"Compilation failed:\n{compile_result.stderr}"
                }
            
            # Run
            run_result = subprocess.run(
                [exe_file],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            # Cleanup
            os.unlink(cpp_file)
            os.unlink(exe_file)
            
            return {
                "success": run_result.returncode == 0,
                "output": run_result.stdout,
                "error": run_result.stderr if run_result.returncode != 0 else None
            }
            
        except subprocess.TimeoutExpired:
            # Cleanup
            if os.path.exists(cpp_file):
                os.unlink(cpp_file)
            if os.path.exists(exe_file):
                os.unlink(exe_file)
            return {"success": False, "error": "Execution timeout"}
        except Exception as e:
            # Cleanup
            if os.path.exists(cpp_file):
                os.unlink(cpp_file)
            if os.path.exists(exe_file):
                os.unlink(exe_file)
            return {"success": False, "error": str(e)}


def main():
    parser = argparse.ArgumentParser(description="Transpile Elisp to C++ using Qwen Coder")
    parser.add_argument("-c", "--code", help="Elisp code to transpile")
    parser.add_argument("-f", "--file", help="Elisp file to transpile")
    parser.add_argument("-o", "--output", help="Output C++ file")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")
    parser.add_argument("--test-only", action="store_true", help="Only run tests, don't save output")
    
    args = parser.parse_args()
    
    # Get Elisp code
    if args.code:
        elisp_code = args.code
    elif args.file:
        with open(args.file, 'r') as f:
            elisp_code = f.read()
    else:
        print("Error: Provide either -c CODE or -f FILE")
        sys.exit(1)
    
    print("🔧 Elisp to C++ Transpiler")
    print("=" * 50)
    
    # Initialize components
    executor = ElispExecutor()
    transpiler = QwenCoderTranspiler()
    tester = CppTester()
    
    # Step 1: Execute Elisp and generate test cases
    print("\n📝 Analyzing Elisp code...")
    if args.verbose:
        print(f"Code: {elisp_code}")
    
    test_cases = executor.generate_test_cases(elisp_code)
    if not test_cases:
        print("⚠️  Warning: Could not generate test cases")
    else:
        print(f"✅ Generated {len(test_cases)} test cases")
        if args.verbose:
            for i, tc in enumerate(test_cases, 1):
                print(f"  Test {i}: {tc['inputs']} → {tc['expected_return']}")
    
    # Step 2: Transpile to C++
    print("\n🤖 Transpiling with Qwen Coder...")
    cpp_code = transpiler.transpile(elisp_code, test_cases)
    
    if not cpp_code:
        print("❌ Transpilation failed")
        sys.exit(1)
    
    print("✅ C++ code generated")
    
    if args.verbose:
        print("\nGenerated C++ code:")
        print("-" * 40)
        print(cpp_code)
        print("-" * 40)
    
    # Step 3: Test C++ code
    print("\n🔬 Testing C++ code...")
    test_result = tester.compile_and_run(cpp_code)
    
    if test_result["success"]:
        print("✅ C++ code compiled and ran successfully")
        if test_result["output"]:
            print("\nTest results:")
            print(test_result["output"])
    else:
        print("❌ C++ testing failed")
        print(test_result["error"])
    
    # Step 4: Save output if requested
    if not args.test_only and args.output:
        with open(args.output, 'w') as f:
            f.write(cpp_code)
        print(f"\n💾 C++ code saved to: {args.output}")
    
    print("\n✨ Transpilation complete!")


if __name__ == "__main__":
    main()
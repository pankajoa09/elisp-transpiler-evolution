#!/usr/bin/env python3
"""
Simple Elisp to C++ transpiler with verification
No AI, just pattern matching
"""

import re
import subprocess
import sys
import tempfile
import os

def parse_elisp(code):
    """Parse simple Elisp expressions"""
    code = code.strip()
    
    # Match (operator arg1 arg2)
    match = re.match(r'\(([+\-*/])\s+(\d+)\s+(\d+)\)', code)
    if match:
        return {
            'type': 'arithmetic',
            'op': match.group(1),
            'left': int(match.group(2)),
            'right': int(match.group(3))
        }
    
    # Match (defun name (params) body)
    match = re.match(r'\(defun\s+(\w+)\s*\(([^)]*)\)\s*(.+)\)', code)
    if match:
        return {
            'type': 'function',
            'name': match.group(1),
            'params': match.group(2).split(),
            'body': match.group(3)
        }
    
    return None

def transpile_to_cpp(parsed):
    """Convert parsed Elisp to C++"""
    if not parsed:
        return None
    
    if parsed['type'] == 'arithmetic':
        op = parsed['op']
        left = parsed['left']
        right = parsed['right']
        
        cpp_code = f"""#include <iostream>

int main() {{
    int result = {left} {op} {right};
    std::cout << "The result of {left} {op} {right} is: " << result << std::endl;
    return 0;
}}"""
        return cpp_code
    
    elif parsed['type'] == 'function':
        # Simple function transpilation
        name = parsed['name']
        params = parsed['params']
        body_parsed = parse_elisp(parsed['body'])
        
        if body_parsed and body_parsed['type'] == 'arithmetic':
            param_str = ', '.join([f"int {p}" for p in params])
            # Replace parameter names in the body
            left = body_parsed['left'] if not isinstance(body_parsed['left'], int) else body_parsed['left']
            right = body_parsed['right'] if not isinstance(body_parsed['right'], int) else body_parsed['right']
            
            cpp_code = f"""#include <iostream>

int {name}({param_str}) {{
    return {params[0]} {body_parsed['op']} {params[1]};
}}

int main() {{
    int result = {name}(3, 5);
    std::cout << "Result: " << result << std::endl;
    return 0;
}}"""
            return cpp_code
    
    return None

def verify_cpp(cpp_code):
    """Compile and run C++ code to verify it works"""
    # Write to temp file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
        f.write(cpp_code)
        cpp_file = f.name
    
    exe_file = cpp_file.replace('.cpp', '')
    
    try:
        # Compile
        compile_result = subprocess.run(
            ['g++', '-o', exe_file, cpp_file],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        if compile_result.returncode != 0:
            os.unlink(cpp_file)
            return {
                'success': False,
                'error': f"Compilation failed:\n{compile_result.stderr}"
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
            'success': True,
            'output': run_result.stdout,
            'return_code': run_result.returncode
        }
        
    except Exception as e:
        # Cleanup on error
        if os.path.exists(cpp_file):
            os.unlink(cpp_file)
        if os.path.exists(exe_file):
            os.unlink(exe_file)
        return {
            'success': False,
            'error': str(e)
        }

def main():
    if len(sys.argv) < 2:
        print("Usage: simple_elisp2cpp.py '<elisp-code>'")
        print("Example: simple_elisp2cpp.py '(+ 1 2)'")
        sys.exit(1)
    
    elisp_code = sys.argv[1]
    
    print(f"📝 Input Elisp: {elisp_code}")
    print("=" * 50)
    
    # Parse
    parsed = parse_elisp(elisp_code)
    if not parsed:
        print("❌ Failed to parse Elisp code")
        sys.exit(1)
    
    print(f"✅ Parsed: {parsed}")
    
    # Transpile
    cpp_code = transpile_to_cpp(parsed)
    if not cpp_code:
        print("❌ Failed to transpile to C++")
        sys.exit(1)
    
    print("\n🔄 Generated C++ code:")
    print("-" * 40)
    print(cpp_code)
    print("-" * 40)
    
    # Verify
    print("\n🧪 Verifying C++ code...")
    result = verify_cpp(cpp_code)
    
    if result['success']:
        print("✅ C++ code compiled and ran successfully!")
        print(f"Output:\n{result['output']}")
    else:
        print(f"❌ Verification failed:\n{result['error']}")
        sys.exit(1)
    
    # Save if requested
    if len(sys.argv) > 2 and sys.argv[2] == '--save':
        filename = 'output.cpp'
        with open(filename, 'w') as f:
            f.write(cpp_code)
        print(f"\n💾 Saved to {filename}")

if __name__ == "__main__":
    main()
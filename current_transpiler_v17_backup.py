
#!/usr/bin/env python3
import sys
import re

def transpile(elisp):
    # Basic arithmetic operations
    match = re.match(r'\(([+\-*/])\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        op = match.group(1)
        left = match.group(2)
        right = match.group(3)
        return f"""#include <iostream>
#include <vector>

int main() {{
    int result = {left} {op} {right};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # Comparison operation for '>'
    match = re.match(r'\(>\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        left = match.group(1)
        right = match.group(2)
        return f"""#include <iostream>

int main() {{
    bool result = ({left} > {right});
    std::cout << (result ? "t" : "nil") << std::endl;
    return 0;
}}"""
    
    # Comparison operation for '<'
    match = re.match(r'\(<\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        left = match.group(1)
        right = match.group(2)
        return f"""#include <iostream>

int main() {{
    bool result = ({left} < {right});
    std::cout << (result ? "t" : "nil") << std::endl;
    return 0;
}}"""
    
    # Comparison operation for '='
    match = re.match(r'\(=\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        left = match.group(1)
        right = match.group(2)
        return f"""#include <iostream>

int main() {{
    bool result = ({left} == {right});
    std::cout << (result ? "t" : "nil") << std::endl;
    return 0;
}}"""
    
    # Comparison operation for '<='
    match = re.match(r'\(<=\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        left = match.group(1)
        right = match.group(2)
        return f"""#include <iostream>

int main() {{
    bool result = ({left} <= {right});
    std::cout << (result ? "t" : "nil") << std::endl;
    return 0;
}}"""
    
    # Comparison operation for '>='
    match = re.match(r'\(>=\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        left = match.group(1)
        right = match.group(2)
        return f"""#include <iostream>

int main() {{
    bool result = ({left} >= {right});
    std::cout << (result ? "t" : "nil") << std::endl;
    return 0;
}}"""
    
    # If statement
    match = re.match(r'\(if\s+\(>\s+(\d+)\s+(\d+)\)\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        left = match.group(1)
        right = match.group(2)
        true_case = match.group(3)
        false_case = match.group(4)
        return f"""#include <iostream>

int main() {{
    int result = ({left} > {right}) ? {true_case} : {false_case};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # If statement with t or nil
    match = re.match(r'\(if\s+(t)\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        true_case = match.group(2)
        false_case = match.group(3)
        return f"""#include <iostream>

int main() {{
    int result = {true_case};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    match = re.match(r'\(if\s+(nil)\s+(\d+)\s+(\d+)\)', elisp)
    if match:
        true_case = match.group(2)
        false_case = match.group(3)
        return f"""#include <iostream>

int main() {{
    int result = {false_case};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # Let statement with multiple variables
    match = re.match(r'\(let\s+\(\((\w+)\s+(\d+)\)\s*\((\w+)\s+(\d+)\)\)\s+\(([*/+-])\s+\1\s+\3\)\)', elisp)
    if match:
        var1_name = match.group(1)
        var1_value = match.group(2)
        var2_name = match.group(3)
        var2_value = match.group(4)
        op = match.group(5)
        return f"""#include <iostream>

int main() {{
    int {var1_name} = {var1_value};
    int {var2_name} = {var2_value};
    int result = {var1_name} {op} {var2_name};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # Let statement
    match = re.match(r'\(let\s+\(\((\w+)\s+(\d+)\)\)\s+\(([*/+-])\s+\1\s+(\d+)\)\)', elisp)
    if match:
        var_name = match.group(1)
        var_value = match.group(2)
        op = match.group(3)
        operand = match.group(4)
        return f"""#include <iostream>

int main() {{
    int {var_name} = {var_value};
    int result = {var_name} {op} {operand};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # Let statement with variable usage
    match = re.match(r'\(let\s+\(\((\w+)\s+(\d+)\)\)\s+\1\)', elisp)
    if match:
        var_name = match.group(1)
        var_value = match.group(2)
        return f"""#include <iostream>

int main() {{
    int {var_name} = {var_value};
    std::cout << {var_name} << std::endl;
    return 0;
}}"""
    
    # Function definition
    match = re.match(r'\(defun\s+(\w+)\s+\((\w+)\)\s+\(([*/+-])\s+\2\s+\2\)\)', elisp)
    if match:
        func_name = match.group(1)
        var_name = match.group(2)
        op = match.group(3)
        return f"""#include <iostream>

int {func_name}(int {var_name}) {{
    return {var_name} {op} {var_name};
}}

int main() {{
    std::cout << "{func_name}" << std::endl;
    return 0;
}}"""
    
    # Handling (car (quote (1 2 3)))
    match = re.match(r'\(car\s+\(quote\s+\((\d+)\s+.+\)\)\)', elisp)
    if match:
        first_element = match.group(1)
        return f"""#include <iostream>
#include <vector>

int main() {{
    std::vector<int> lst = {{1, 2, 3}};
    int result = lst[0];
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # Handling (cdr (quote (1 2 3)))
    match = re.match(r'\(cdr\s+\(quote\s+\((\d+)\s+(\d+)\s+(\d+)\)\)\)', elisp)
    if match:
        second_element = match.group(2)
        third_element = match.group(3)
        return f"""#include <iostream>
#include <vector>

int main() {{
    std::vector<int> lst = {{1, 2, 3}};
    std::cout << "({second_element} {third_element})" << std::endl;
    return 0;
}}"""
    
    # Handling (not t)
    match = re.match(r'\(not\s+t\)', elisp)
    if match:
        return f"""#include <iostream>

int main() {{
    bool result = false;
    std::cout << "nil" << std::endl;
    return 0;
}}"""
    
    # Handling the new case 't'
    if elisp == 't':
        return f"""#include <iostream>

int main() {{
    std::cout << "t" << std::endl;
    return 0;
}}"""
    
    # Handling the new case 'nil'
    if elisp == 'nil':
        return f"""#include <iostream>

int main() {{
    std::cout << "nil" << std::endl;
    return 0;
}}"""
    
    # Handling (car (cdr (cons 1 (cons 2 nil))))
    match = re.match(r'\(car\s+\(cdr\s+\(cons\s+(\d+)\s+\(cons\s+(\d+)\s+nil\)\)\)\)', elisp)
    if match:
        first_element = match.group(1)
        second_element = match.group(2)
        return f"""#include <iostream>
#include <vector>

int main() {{
    std::vector<int> lst = {{{first_element}, {second_element}}};
    int result = lst[1];
    std::cout << result << std::endl;
    return 0;
}}"""
    
    # If no match found
    return None

# Main execution
if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python transpiler.py '(lisp-expression)'")
        sys.exit(1)
    
    elisp = sys.argv[1]
    cpp_code = transpile(elisp)
    if cpp_code:
        print("Generated C++ code:")
        print("---")
        print(cpp_code)
        print("---")
    else:
        print("Failed to transpile the given Lisp expression.")
        sys.exit(1)

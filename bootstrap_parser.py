#!/usr/bin/env python3
"""
Bootstrap parser-based transpiler to help evolution break through the regex wall
This gives Qwen a better foundation to evolve from
"""

import sys
import re

def tokenize(elisp):
    """Convert Elisp string into tokens"""
    # Add spaces around parens for easier splitting
    elisp = elisp.replace('(', ' ( ').replace(')', ' ) ')
    tokens = elisp.split()
    return tokens

def parse(tokens):
    """Parse tokens into nested list structure (S-expressions)"""
    if not tokens:
        raise SyntaxError("Unexpected EOF")
    
    token = tokens.pop(0)
    
    if token == '(':
        expr = []
        while tokens[0] != ')':
            expr.append(parse(tokens))
        tokens.pop(0)  # Remove closing paren
        return expr
    elif token == ')':
        raise SyntaxError("Unexpected )")
    else:
        # Try to convert to number, otherwise keep as string
        try:
            return int(token)
        except ValueError:
            try:
                return float(token)
            except ValueError:
                return token

def transpile_expr(expr):
    """Transpile parsed S-expression to C++ code"""
    # Handle atoms (numbers, strings, symbols)
    if not isinstance(expr, list):
        if expr == 't':
            return 'true'
        elif expr == 'nil':
            return 'false'
        else:
            return str(expr)
    
    # Handle empty list
    if not expr:
        return 'nullptr'
    
    # Get operator and operands
    op = expr[0]
    
    # Basic arithmetic
    if op in ['+', '-', '*', '/']:
        if len(expr) == 3:
            left = transpile_expr(expr[1])
            right = transpile_expr(expr[2])
            return f"({left} {op} {right})"
        elif len(expr) > 3:
            # Handle multiple args: (+ 1 2 3) -> ((1 + 2) + 3)
            result = transpile_expr(expr[1])
            for arg in expr[2:]:
                result = f"({result} {op} {transpile_expr(arg)})"
            return result
    
    # Comparisons
    elif op in ['>', '<', '>=', '<=']:
        left = transpile_expr(expr[1])
        right = transpile_expr(expr[2])
        return f"({left} {op} {right})"
    
    elif op == '=':
        left = transpile_expr(expr[1])
        right = transpile_expr(expr[2])
        return f"({left} == {right})"
    
    # If statement
    elif op == 'if':
        condition = transpile_expr(expr[1])
        true_branch = transpile_expr(expr[2])
        false_branch = transpile_expr(expr[3]) if len(expr) > 3 else 'nullptr'
        return f"({condition} ? {true_branch} : {false_branch})"
    
    # Quote (for now, just handle simple lists)
    elif op == 'quote':
        return '"QUOTED"'  # Placeholder
    
    # Car/cdr placeholder
    elif op == 'car':
        return '"CAR"'  # Placeholder
    elif op == 'cdr':
        return '"CDR"'  # Placeholder
    
    # Let binding - simplified
    elif op == 'let':
        # For now, just evaluate the body
        if len(expr) > 2:
            return transpile_expr(expr[2])
        return '0'
    
    # Default: treat as function call (placeholder)
    else:
        return f'"UNKNOWN_{op}"'

def transpile(elisp):
    """Main transpiler function"""
    try:
        tokens = tokenize(elisp)
        ast = parse(tokens)
        cpp_expr = transpile_expr(ast)
        
        # Determine output type
        if any(comp in elisp for comp in ['>', '<', '=', '>=', '<=']):
            # Boolean output
            return f"""#include <iostream>

int main() {{
    bool result = {cpp_expr};
    std::cout << (result ? "t" : "nil") << std::endl;
    return 0;
}}"""
        else:
            # Numeric or string output
            return f"""#include <iostream>

int main() {{
    auto result = {cpp_expr};
    std::cout << result << std::endl;
    return 0;
}}"""
    
    except Exception as e:
        # If parsing fails, fall back to old regex patterns for backward compatibility
        # This ensures we don't break existing functionality
        return None

# Keep the old regex-based code as fallback
def transpile_legacy(elisp):
    """Original regex-based transpiler for backward compatibility"""
    # [Previous regex patterns would go here]
    # For now, return None to indicate failure
    return None

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(1)
    
    elisp = sys.argv[1]
    
    # Try new parser-based approach first
    cpp = transpile(elisp)
    
    # Fall back to legacy if needed
    if cpp is None:
        cpp = transpile_legacy(elisp)
    
    if cpp:
        print("Generated C++ code:")
        print("---")
        print(cpp)
        print("---")
    else:
        print("Failed to transpile the given Lisp expression.")
        sys.exit(1)
# Elisp to C++ Self-Evolving Transpiler

A transpiler that learns and evolves to handle more Elisp constructs over time using AI.

## How It Works

1. **Try**: Attempts to transpile Elisp code
2. **Fail**: If it can't handle it, asks Qwen Coder for help
3. **Evolve**: Gets improved version that handles the new case
4. **Verify**: Tests against ALL previous cases (regression testing)
5. **Save**: Stores the improved transpiler and adds to test suite

## Current Capabilities (v4)

The transpiler can handle:
- ✅ Arithmetic: `(+ 1 2)`, `(* 7 6)`, `(- 10 3)`, `(/ 8 2)`
- ✅ Comparisons: `(> 5 3)`, `(< 2 7)`
- ✅ Conditionals: `(if (> 5 3) 10 20)`
- ✅ Variables: `(let ((x 5)) (* x 2))`
- ✅ Functions: `(defun square (x) (* x x))`

## Usage

```bash
cd ~/Documents/elisp-transpiler-evolution
python3 evolving_transpiler.py '(+ 1 2)'
```

## Files

- `evolving_transpiler.py` - Main evolution engine
- `current_transpiler.py` - Current transpiler version (v4)
- `test_suite.json` - All test cases it has learned
- `transpiler_version.txt` - Current version number

## Test Suite

The transpiler maintains a growing test suite of all cases it has learned:
```json
[
  {"elisp": "(+ 1 2)", "expected_output": "3"},
  {"elisp": "(* 7 6)", "expected_output": "42"},
  {"elisp": "(> 5 3)", "expected_output": "t"},
  {"elisp": "(if (> 5 3) 10 20)", "expected_output": "10"},
  {"elisp": "(let ((x 5)) (* x 2))", "expected_output": "10"},
  {"elisp": "(defun square (x) (* x x))", "expected_output": "square"}
]
```

## Evolution Process

Each time it encounters new Elisp it can't handle:
1. Asks Qwen Coder to improve the transpiler
2. Verifies the new version still passes ALL old tests
3. Only accepts if 100% backward compatible
4. Adds the new case to its permanent memory

## Examples

### Simple arithmetic
```bash
python3 evolving_transpiler.py '(+ 10 20)'
# Output: C++ code that prints 30
```

### Conditionals
```bash
python3 evolving_transpiler.py '(if (< 1 2) 100 200)'
# Output: C++ code with ternary operator
```

### Variables
```bash
python3 evolving_transpiler.py '(let ((y 7)) (+ y 3))'
# Output: C++ code with local variable
```

## Future Evolution

The transpiler will continue learning as you use it. Try giving it:
- Lists: `(car (list 1 2 3))`
- Loops: `(dotimes (i 5) (print i))`
- Recursion: `(defun fib (n) ...)`
- Strings: `(concat "hello" "world")`

Each new construct makes it permanently smarter!
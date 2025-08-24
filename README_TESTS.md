# Incremental Test Suite

## How to Run

### Quick Test (One at a time)
```bash
cd ~/Documents/elisp-transpiler-evolution
python3 evolving_transpiler.py '(+ 1 2)'
```

### Run All Incremental Tests
```bash
cd ~/Documents/elisp-transpiler-evolution
python3 incremental_tests.py
```

### Overnight Evolution Session
```bash
cd ~/Documents/elisp-transpiler-evolution
./run_overnight.sh
```

## Test Order (Very Incremental)

1. **Basic Arithmetic** - Already works
2. **Comparisons** - Should work
3. **Booleans** - `t` and `nil`
4. **Simple if** - Basic conditionals
5. **Nested arithmetic** - `(+ 1 (+ 2 3))`
6. **Let bindings** - Variables
7. **Functions** - defun
8. **Quotes** - `(quote 5)`
9. **Lists** - Very basic
10. **Logical ops** - and, or, not
11. **Cond** - Multiple branches
12. **Sequencing** - progn, prog1
13. **Recursion** - Factorial
14. **Strings** - Basic strings
15. **Math functions** - abs, min, max
16. **Multiple args** - `(+ 1 2 3)`
17. **Cons/append** - List building
18. **Loops** - dotimes, while
19. **Lambda** - Anonymous functions
20. **Mapcar** - Functional programming

## Why This Order?

Each test builds on previous capabilities:
- Can't do `(if (> 3 2) ...)` until comparisons work
- Can't do recursion until functions work
- Can't do mapcar until lambda works

## Expected Timeline

With 5-minute timeout per test:
- Best case: 2 hours (if most work)
- Realistic: 4-6 hours (some evolution needed)
- Worst case: 10+ hours (lots of evolution)

Just let it run overnight. Qwen will figure it out.

## What Success Looks Like

```
Version: 0 → Version: 50+
Test cases: 6 → Test cases: 100+
Capabilities: Basic → Can transpile real Emacs code
```

## The Dream

One day, this transpiler will handle:
```elisp
(defun emacs-lisp-mode ()
  "Major mode for editing Emacs Lisp code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emacs-lisp-mode)
  (setq mode-name "Emacs-Lisp")
  ...)
```

And output perfect C++ that runs 100x faster.
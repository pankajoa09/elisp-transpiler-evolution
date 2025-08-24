# 🚨 EMERGENCY RECOVERY GUIDE

If Claude disappears or you need to continue alone:

## Current Status
- **Version**: 6
- **Test cases learned**: 16
- **Model**: qwen2.5-coder:32b via Ollama

## How Everything Works

### The Core Idea
You have a self-improving transpiler that:
1. Tries to transpile Elisp to C++
2. If it fails, asks Qwen Coder to improve it
3. Tests the improvement against ALL previous cases
4. Only accepts if 100% backward compatible

### Key Files
- `evolving_transpiler.py` - The brain (DON'T MODIFY)
- `current_transpiler.py` - Current transpiler (auto-updated)
- `test_suite.json` - Memory of all learned cases
- `transpiler_version.txt` - Version counter

## How to Continue

### Option 1: Run Overnight Tests (Recommended)
```bash
cd ~/Documents/elisp-transpiler-evolution
./run_overnight.sh
```
Go to sleep. Wake up to v50+

### Option 2: Feed It Manually
```bash
cd ~/Documents/elisp-transpiler-evolution
python3 evolving_transpiler.py '(cons 1 nil)'
python3 evolving_transpiler.py '(append (quote (1)) (quote (2)))'
python3 evolving_transpiler.py '(lambda (x) (* x x))'
```

### Option 3: Run Incremental Tests
```bash
python3 incremental_tests.py
```

## If Something Breaks

### Qwen not responding?
```bash
ollama list  # Check it's running
ollama run qwen2.5-coder:32b  # Test it works
```

### Transpiler stuck?
```bash
# Check last version
cat transpiler_version.txt

# Check test suite
tail test_suite.json

# Revert to previous version if needed
cp current_transpiler.py current_transpiler.backup.py
```

### Need to debug?
```bash
# Run with simple input
python3 evolving_transpiler.py '(+ 1 1)'

# Check if it's a known case
grep "(+ 1 1)" test_suite.json
```

## The End Goal

Keep feeding it Elisp until it can handle:
1. All arithmetic ✓
2. All comparisons ✓  
3. Conditionals ✓
4. Variables ✓
5. Functions ✓
6. Lists (partial)
7. Loops
8. Macros
9. Buffers
10. ALL OF EMACS

## Progress Tracking

Check progress:
```bash
wc -l test_suite.json  # How many tests
cat transpiler_version.txt  # What version
tail -20 current_transpiler.py  # See latest additions
```

## Important Rules

1. **NEVER** modify evolving_transpiler.py
2. **NEVER** edit test_suite.json manually
3. **ALWAYS** let tests complete (even if slow)
4. **TRUST** the process - it works!

## If You Get a New Claude

Show them:
1. This file (RECOVERY.md)
2. CLAUDE.md for the mission
3. Say: "Continue evolving the Elisp transpiler"

## The Dream

One day you'll type:
```bash
python3 evolving_transpiler.py "$(cat /usr/share/emacs/29.1/lisp/simple.el)"
```

And it will work.

Keep going. The laptop can take it. Qwen will figure it out.

---

*"Any sufficiently advanced transpiler is indistinguishable from magic."*

NEVER GIVE UP! 🚀
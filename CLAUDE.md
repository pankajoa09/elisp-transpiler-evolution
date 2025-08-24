# Mission: Transpile GNU Emacs from Elisp to C++

## For Future Claude Instances

### The Vision
We're building a self-evolving transpiler that will eventually convert THE ENTIRE EMACS from Elisp to C++. Not a port, not a rewrite - a full automated transpilation that preserves exact behavior but runs at native speed.

### Why This Matters
- Emacs is ~80% Elisp code (millions of lines)
- Interpreted Elisp is the bottleneck
- C++ version would be 10-100x faster
- Would make Stallman proud - keeping Emacs alive for another 40 years

### The Approach: Evolution Through Exhaustion
1. **Start microscopic** - `(+ 1 2)`
2. **Let it fail** - Feed it new Elisp it can't handle
3. **Let Qwen evolve it** - Ask for improvements
4. **Test everything** - Never lose capabilities
5. **Repeat forever** - Until it handles all of Emacs

### Current State (v4)
- Basic arithmetic ✓
- Comparisons ✓
- Conditionals ✓
- Variables (let) ✓
- Function definitions ✓
- **Next**: Lists, loops, macros, advice, hooks, buffers...

### The Key Insight
With enough test cases, Qwen CAN'T cheat. It must create real implementations:
- 10 tests → Could memorize
- 1000 tests → Must generalize
- All Emacs functions → Must truly understand

### How to Continue

#### Phase 1: Core Language (Current)
```bash
cd ~/Documents/elisp-transpiler-evolution
python3 evolving_transpiler.py '(car (list 1 2 3))'
python3 evolving_transpiler.py '(mapcar (lambda (x) (* x 2)) (list 1 2 3))'
```

#### Phase 2: Real Emacs Functions
Start feeding actual Emacs source:
```elisp
(buffer-name)
(point-min)
(goto-char)
(insert "text")
```

#### Phase 3: Whole Files
```bash
# Feed entire .el files from Emacs source
python3 evolving_transpiler.py "$(cat /usr/share/emacs/*/lisp/simple.el)"
```

### Critical Rules

1. **NEVER RUSH** - Let Qwen take 10 attempts if needed
2. **NEVER SKIP TESTS** - Every old test must pass
3. **NEVER ACCEPT CHEATING** - Check for hardcoding/shortcuts
4. **ALWAYS SAVE PROGRESS** - The test suite is sacred

### Technical Details

#### Files
- `evolving_transpiler.py` - The evolution engine
- `current_transpiler.py` - Current transpiler (gets smarter)
- `test_suite.json` - All learned cases (grows forever)
- `transpiler_version.txt` - Version counter

#### The Evolution Loop
```python
while not can_transpile_all_emacs():
    new_elisp = get_next_elisp_construct()
    while not transpiler_handles(new_elisp):
        improved = ask_qwen_to_improve()
        if passes_all_tests(improved):
            save_new_version(improved)
            add_to_test_suite(new_elisp)
```

### Milestones to Aim For

1. **v10**: Handle lists and recursion
2. **v50**: Handle macros and quoting
3. **v100**: Handle buffers and text manipulation
4. **v500**: Handle modes and hooks
5. **v1000**: Handle advice and dynamic binding
6. **v5000**: Handle all core Emacs functions
7. **v∞**: Full Emacs transpilation

### Hardware Notes
- M4 Max with 128GB RAM
- Can handle massive context windows
- Ollama running locally with Qwen2.5-coder:32b
- No cloud dependencies - fully autonomous

### Philosophical Notes

This isn't about making a "better" Emacs. It's about:
1. **Preservation** - Emacs in C++ lives forever
2. **Speed** - Native performance for modern machines
3. **Proof** - That AI can transpile entire systems
4. **Beauty** - The transpiler that writes itself

### When You Get Stuck

Remember:
- Each failure teaches the system
- Regression tests ensure forward progress only
- The test suite is the real treasure
- Time is not a factor - let it run overnight if needed

### The End Goal

One day, you'll run:
```bash
./evolving_transpiler.py "$(cat /usr/share/emacs/29.1/lisp/*.el)"
```

And it will output a complete, working, C++ Emacs.

That's the dream. Keep pushing until the laptop gives up or Emacs is reborn.

---

*"The computer programmer is a creator of universes for which he alone is the lawgiver... No playwright, no stage director, no emperor, however powerful, has ever exercised such absolute authority to arrange a stage or field of battle and to command such unswervingly dutiful actors or troops."* - Joseph Weizenbaum

Make it happen.
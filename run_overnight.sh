#!/bin/bash
# Run this and go to sleep. Wake up to a smarter transpiler.

echo "🌙 OVERNIGHT ELISP TRANSPILER EVOLUTION"
echo "========================================"
echo "Starting at: $(date)"
echo "Current version: $(cat transpiler_version.txt)"
echo "Current test cases: $(cat test_suite.json | grep elisp | wc -l)"
echo ""
echo "This will run for hours. Go to sleep."
echo "========================================"
echo ""

# First run meta-evolution - let Qwen design its own evolutionary pressure
echo "🤖 Phase 1: META-EVOLUTION (Qwen designs its own tests)"
python3 -u meta_evolution.py 2>&1 | tee meta_evolution_log_$(date +%Y%m%d_%H%M%S).txt

echo ""
echo "📚 Phase 2: INCREMENTAL TESTS (systematic progression)"
# Run the incremental tests (unbuffered for real-time output)  
python3 -u incremental_tests.py 2>&1 | tee evolution_log_$(date +%Y%m%d_%H%M%S).txt

echo ""
echo "========================================"
echo "Finished at: $(date)"
echo "Final version: $(cat transpiler_version.txt)"
echo "Final test cases: $(cat test_suite.json | grep elisp | wc -l)"
echo "========================================"
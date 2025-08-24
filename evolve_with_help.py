#!/usr/bin/env python3
"""
Help the transpiler evolve by giving it a parser foundation
Instead of evolving from regex, evolve from a basic parser
"""

import shutil
import subprocess
import sys

print("🚀 BOOTSTRAP EVOLUTION HELPER")
print("="*50)
print("Giving the transpiler a parser foundation to evolve from...")
print()

# Backup current transpiler
print("📦 Backing up current transpiler (v17)...")
shutil.copy('current_transpiler.py', 'current_transpiler_v17_backup.py')

# Merge bootstrap parser with existing patterns
print("🔧 Creating hybrid transpiler with parser + legacy patterns...")

# Read the bootstrap parser
with open('bootstrap_parser.py', 'r') as f:
    bootstrap_code = f.read()

# Read current transpiler's regex patterns (to preserve learned patterns)
with open('current_transpiler.py', 'r') as f:
    current_code = f.read()

# Extract just the regex patterns from current transpiler
import re
pattern_section = []
in_function = False
for line in current_code.split('\n'):
    if 'def transpile(' in line:
        in_function = True
    elif in_function and ('def ' in line or line.strip().startswith('if __name__')):
        break
    elif in_function and ('re.match' in line or 'if elisp ==' in line):
        # Capture the pattern and its handling
        pattern_section.append(line)

# Create hybrid transpiler
hybrid_code = bootstrap_code.replace(
    'def transpile_legacy(elisp):',
    f'''def transpile_legacy(elisp):
    """Original regex-based transpiler for backward compatibility"""
    import re
    
    # Preserved regex patterns from v17
{chr(10).join('    ' + line if line.strip() else '' for line in current_code.split(chr(10))[6:248])}
    
    return None'''
)

# Save as new current transpiler
with open('current_transpiler.py', 'w') as f:
    f.write(hybrid_code)

print("✅ Hybrid transpiler created (parser + v17 patterns)")
print()

# Update version
with open('transpiler_version.txt', 'w') as f:
    f.write('18')

print("📈 Version bumped to v18 (bootstrap edition)")
print()

# Test the new hybrid
print("🧪 Testing hybrid transpiler...")
test_cases = [
    "(+ 1 2)",  # Should work with both
    "(+ 1 (+ 2 3))",  # Parser handles nesting better
    "(> 5 3)",  # Regex fallback
    "(car (cdr (cons 1 (cons 2 nil))))"  # v17's achievement
]

for test in test_cases:
    result = subprocess.run(
        ['python3', 'current_transpiler.py', test],
        capture_output=True,
        text=True,
        timeout=5
    )
    status = "✅" if result.returncode == 0 else "❌"
    print(f"  {status} {test}")

print()
print("🎯 Now run meta_evolution.py again!")
print("The parser foundation should help it evolve past the regex wall.")
print()
print("Key improvements:")
print("  • Real tokenization and parsing")
print("  • Handles arbitrary nesting")
print("  • Preserves v17's learned patterns as fallback")
print("  • Clean structure for Qwen to evolve")
#!/usr/bin/env python3
"""
Meta-Evolution: Qwen designs its own test cases to evolve the transpiler
This is recursive self-improvement - the AI creates the selection pressure for its own evolution
"""

import json
import subprocess
import sys
import requests
import time
from pathlib import Path

OLLAMA_URL = "http://localhost:11434/api/generate"
MODEL = "qwen2.5-coder:32b"

def call_qwen(prompt):
    """Ask Qwen to generate test cases"""
    print("🤖 Asking Qwen to design evolutionary test cases...")
    print("   (This may take a few minutes - Qwen is thinking)")
    
    payload = {
        "model": MODEL,
        "prompt": prompt,
        "stream": False,
        "options": {
            "temperature": 0.7,  # Some creativity for test design
            "num_predict": 2000
        }
    }
    
    try:
        response = requests.post(OLLAMA_URL, json=payload, timeout=300)
        if response.status_code == 200:
            return response.json()['response'].strip()
        else:
            print(f"❌ Ollama error: {response.status_code}")
            return None
    except Exception as e:
        print(f"❌ Failed to reach Qwen: {e}")
        return None

def analyze_current_transpiler():
    """Read and analyze current transpiler to understand its limitations"""
    with open('current_transpiler.py', 'r') as f:
        code = f.read()
    
    # Extract patterns it can handle
    patterns = []
    for line in code.split('\n'):
        if 're.match(' in line and 'r\'' in line:
            start = line.find("r'") + 2
            end = line.find("'", start)
            if end > start:
                patterns.append(line[start:end])
    
    return code, patterns

def generate_evolution_tests():
    """Ask Qwen to design tests that will force architectural evolution"""
    
    current_code, patterns = analyze_current_transpiler()
    version = get_version()
    
    prompt = f"""You are designing test cases to evolve a regex-based Elisp to C++ transpiler. 

CURRENT TRANSPILER (v{version}):
The transpiler uses regex pattern matching with these patterns:
{chr(10).join('- ' + p for p in patterns[:10])}
... and {len(patterns)-10} more similar regex patterns.

PROBLEM: This regex approach is a local maximum - it can't handle:
1. Nested expressions beyond 2 levels
2. Variable scoping
3. Recursive data structures  
4. Function composition
5. Dynamic evaluation

YOUR TASK: Design exactly 5 Elisp test cases that will FORCE this transpiler to evolve beyond regex matching. Each test should:

1. Be impossible to handle with simple regex patterns
2. Require building an actual S-expression parser
3. Build on previous capabilities (don't break working features)
4. Gradually increase architectural pressure

Focus on:
- Nested expressions that break regex: (+ 1 (+ 2 (+ 3 4)))
- Compositions that require parsing: (car (cdr (cons 1 (cons 2 nil))))
- Variable scoping: (let ((x 1)) (let ((x 2)) x))

Return ONLY the 5 Elisp expressions, one per line, no explanation:"""

    response = call_qwen(prompt)
    if not response:
        return None
        
    # Extract test cases from response
    lines = [line.strip() for line in response.split('\n') if line.strip() and line.strip().startswith('(')]
    return lines[:5]  # Take first 5 valid expressions

def get_version():
    """Get current transpiler version"""
    try:
        with open('transpiler_version.txt', 'r') as f:
            return int(f.read().strip())
    except:
        return 0

def run_evolution_on_test(elisp_test):
    """Run the evolution process on a single test case"""
    print(f"\n🧬 Evolving with: {elisp_test}")
    
    try:
        result = subprocess.run(
            ['python3', 'evolving_transpiler.py', elisp_test],
            capture_output=True,
            text=True,
            timeout=7200,  # 2 hours max
            cwd='/Users/pankajahuja/Documents/elisp-transpiler-evolution'
        )
        
        if "Evolution successful!" in result.stdout:
            print("✅ Successfully evolved!")
            return True
        elif "Already learned this case!" in result.stdout:
            print("✅ Already knew this case")
            return True
        else:
            print("❌ Evolution failed")
            print(result.stdout[-200:] if result.stdout else "No output")
            return False
            
    except subprocess.TimeoutExpired:
        print("⏱️ Evolution timed out (2 hours)")
        return False
    except Exception as e:
        print(f"❌ Evolution error: {e}")
        return False

def meta_evolution_cycle():
    """Run one cycle of meta-evolution"""
    print("\n" + "="*60)
    print("🌟 META-EVOLUTION: Qwen designs its own evolutionary pressure")
    print("="*60)
    
    start_version = get_version()
    print(f"Starting version: v{start_version}")
    
    # Generate tests designed to break current architecture
    test_cases = generate_evolution_tests()
    if not test_cases:
        print("❌ Failed to generate test cases")
        return False
    
    print(f"\n🎯 Qwen designed {len(test_cases)} architectural pressure tests:")
    for i, test in enumerate(test_cases, 1):
        print(f"   {i}. {test}")
    
    # Run evolution on each test
    successes = 0
    for test in test_cases:
        if run_evolution_on_test(test):
            successes += 1
    
    end_version = get_version()
    print(f"\n📊 Meta-evolution results:")
    print(f"   Start: v{start_version} → End: v{end_version}")
    print(f"   Successful tests: {successes}/{len(test_cases)}")
    print(f"   Architectural pressure applied: {'🔥' * min(successes, 5)}")
    
    return end_version > start_version

if __name__ == "__main__":
    print("🚀 STARTING META-EVOLUTION")
    print("Qwen will design test cases to evolve itself!")
    print("This is recursive self-improvement in action.")
    print()
    
    max_cycles = 10
    cycle = 1
    
    while cycle <= max_cycles:
        print(f"\n🔄 META-EVOLUTION CYCLE {cycle}/{max_cycles}")
        
        if meta_evolution_cycle():
            print(f"✅ Cycle {cycle} successful - transpiler evolved!")
        else:
            print(f"⚠️ Cycle {cycle} - no evolution detected")
            
        cycle += 1
        
        # Brief pause between cycles
        print("\n⏸️ Pausing 10 seconds between cycles...")
        time.sleep(10)
    
    final_version = get_version()
    print(f"\n🎉 META-EVOLUTION COMPLETE")
    print(f"Final transpiler version: v{final_version}")
    print("The AI has designed its own evolutionary path!")
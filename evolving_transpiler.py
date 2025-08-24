#!/usr/bin/env python3
"""
Self-improving Elisp to C++ transpiler
Learns from failures and builds up capability over time
"""

import json
import os
import subprocess
import sys
import tempfile
import re
import time
from pathlib import Path

try:
    from googlesearch import search as google_search
    import requests
    from bs4 import BeautifulSoup
    GOOGLE_AVAILABLE = True
except ImportError:
    GOOGLE_AVAILABLE = False
    print("⚠️ Google search not available. Install with: pip3 install googlesearch-python requests beautifulsoup4")


# Files for persistence
TEST_SUITE_FILE = "test_suite.json"
TRANSPILER_FILE = "current_transpiler.py"
VERSION_FILE = "transpiler_version.txt"
ATTEMPT_HISTORY_FILE = "attempt_history.json"

def web_search(query, num_results=3):
    """Search Google and return summarized results"""
    if not GOOGLE_AVAILABLE:
        return "❌ Google search not available"
    
    print(f"      🔍 Searching Google for: {query}")
    results = []
    
    try:
        search_results = list(google_search(query, num_results=num_results, stop=num_results))
        
        for i, url in enumerate(search_results):
            try:
                print(f"         Fetching result {i+1}: {url[:60]}...")
                response = requests.get(url, timeout=10, headers={
                    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
                })
                
                soup = BeautifulSoup(response.text, 'html.parser')
                
                # Remove script and style elements
                for script in soup(["script", "style"]):
                    script.decompose()
                
                # Get text and clean it
                text = soup.get_text()
                lines = (line.strip() for line in text.splitlines())
                chunks = (phrase.strip() for line in lines for phrase in line.split("  "))
                text = ' '.join(chunk for chunk in chunks if chunk)
                
                # Take first 800 characters
                text = text[:800]
                if len(text) == 800:
                    text += "..."
                
                results.append(f"Result {i+1}: {url}\n{text}\n{'='*50}")
                
            except Exception as e:
                print(f"         Error fetching {url}: {e}")
                continue
        
        if results:
            search_summary = f"\n🔍 Google Search Results for: '{query}'\n" + "\n".join(results)
            print(f"      ✅ Found {len(results)} results")
            return search_summary
        else:
            return f"❌ No accessible results found for: {query}"
            
    except Exception as e:
        print(f"      ❌ Search error: {e}")
        return f"❌ Search failed: {e}"

def load_attempt_history():
    """Load previous attempt failures"""
    if not os.path.exists(ATTEMPT_HISTORY_FILE):
        return []
    try:
        with open(ATTEMPT_HISTORY_FILE, 'r') as f:
            return json.load(f)
    except:
        return []

def save_attempt_history(history):
    """Save attempt failures"""
    with open(ATTEMPT_HISTORY_FILE, 'w') as f:
        json.dump(history, f, indent=2)

def load_test_suite():
    """Load all test cases from file"""
    if not os.path.exists(TEST_SUITE_FILE):
        return []
    with open(TEST_SUITE_FILE, 'r') as f:
        return json.load(f)

def save_test_suite(test_suite):
    """Save test cases to file"""
    with open(TEST_SUITE_FILE, 'w') as f:
        json.dump(test_suite, f, indent=2)

def get_current_version():
    """Get current transpiler version"""
    if not os.path.exists(VERSION_FILE):
        return 0
    with open(VERSION_FILE, 'r') as f:
        return int(f.read().strip())

def save_version(version):
    """Save transpiler version"""
    with open(VERSION_FILE, 'w') as f:
        f.write(str(version))

def run_elisp(code):
    """Run Elisp code in Emacs to get expected output"""
    wrapped = f'(princ {code})'
    try:
        result = subprocess.run(
            ['/opt/homebrew/bin/emacs', '--batch', '--eval', wrapped],
            capture_output=True,
            text=True,
            timeout=5
        )
        return result.stdout.strip()
    except:
        return None

def test_transpiler(transpiler_code, elisp_input):
    """Test if transpiler can handle the input"""
    # Save transpiler code to temp file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
        f.write(transpiler_code)
        transpiler_file = f.name
    
    try:
        # Run transpiler
        result = subprocess.run(
            [sys.executable, transpiler_file, elisp_input],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        os.unlink(transpiler_file)
        
        if result.returncode == 0:
            # Extract C++ code from output
            lines = result.stdout.split('\n')
            in_code = False
            cpp_code = []
            for line in lines:
                if '---' in line and not in_code:
                    in_code = True
                    continue
                elif '---' in line and in_code:
                    break
                elif in_code:
                    cpp_code.append(line)
            
            if cpp_code:
                return {'success': True, 'cpp': '\n'.join(cpp_code)}
        
        return {'success': False, 'error': result.stderr or result.stdout}
    
    except Exception as e:
        if os.path.exists(transpiler_file):
            os.unlink(transpiler_file)
        return {'success': False, 'error': str(e)}

def verify_cpp_output(cpp_code, expected_output):
    """Compile and run C++ to verify output"""
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
            return False
        
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
        
        # Check if output contains expected value
        return expected_output in run_result.stdout
        
    except:
        if os.path.exists(cpp_file):
            os.unlink(cpp_file)
        if os.path.exists(exe_file):
            os.unlink(exe_file)
        return False

def load_current_transpiler():
    """Load the current transpiler code"""
    if not os.path.exists(TRANSPILER_FILE):
        # Create initial basic transpiler
        return '''#!/usr/bin/env python3
import sys
import re

def transpile(elisp):
    # Basic arithmetic operations
    match = re.match(r'\\(([+\\-*/])\\s+(\\d+)\\s+(\\d+)\\)', elisp)
    if match:
        op = match.group(1)
        left = match.group(2)
        right = match.group(3)
        return f"""#include <iostream>

int main() {{
    int result = {left} {op} {right};
    std::cout << result << std::endl;
    return 0;
}}"""
    return None

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(1)
    elisp = sys.argv[1]
    cpp = transpile(elisp)
    if cpp:
        print("Generated C++ code:")
        print("---")
        print(cpp)
        print("---")
    else:
        sys.exit(1)
'''
    
    with open(TRANSPILER_FILE, 'r') as f:
        return f.read()

def improve_transpiler(current_code, test_case, error, attempt_history=None, previous_attempt=None):
    """Enhanced transpiler improvement with history, previous attempts, and Google search"""
    
    # Load attempt history if not provided
    if attempt_history is None:
        attempt_history = load_attempt_history()
    
    # Start building enhanced prompt
    prompt_parts = []
    
    prompt_parts.append(f"""ELISP TO C++ TRANSPILER IMPROVEMENT TASK

Target: Make transpiler handle '{test_case['elisp']}' → '{test_case['expected_output']}'

CURRENT TRANSPILER CODE ({len(current_code)} characters):
{current_code}

ERROR FROM CURRENT TRANSPILER:
{error}""")
    
    # Add previous attempt if available
    if previous_attempt:
        # Handle both string and dict formats
        if isinstance(previous_attempt, str):
            code_preview = previous_attempt[:1000]
            error_info = "Unknown error"
            timestamp = "unknown"
        else:
            code_preview = previous_attempt.get('code', str(previous_attempt))[:1000]
            error_info = previous_attempt.get('error', 'Unknown error')[:200]
            timestamp = previous_attempt.get('timestamp', 'unknown')
        
        prompt_parts.append(f"""
PREVIOUS FAILED ATTEMPT:
{code_preview}...
ERROR: {error_info}
TIME: {timestamp}""")
    
    # Add failure history
    if attempt_history:
        recent_failures = attempt_history[-5:]  # Last 5 failures
        failure_summary = []
        for failure in recent_failures:
            failure_summary.append(f"- {failure['elisp']}: {failure['error'][:100]}")
        
        prompt_parts.append(f"""
RECENT FAILURE PATTERNS:
{chr(10).join(failure_summary)}""")
    
    prompt_parts.append(f"""
INSTRUCTIONS:
1. Fix the transpiler to handle the new case: {test_case['elisp']}
2. Keep ALL existing functionality working
3. If you need more information about Elisp functions, include: # SEARCH: <your query>
4. Output COMPLETE Python transpiler code

Available resources:
- Google search (use # SEARCH: comments)
- Previous failure patterns shown above
- Your training knowledge of Elisp and C++

OUTPUT THE COMPLETE FIXED TRANSPILER CODE:""")
    
    prompt = "\n".join(prompt_parts)
    
    print("   🤖 Enhanced Qwen consultation starting...")
    print(f"      Current transpiler: {len(current_code)} chars")
    print(f"      Learning: {test_case['elisp']} → {test_case['expected_output']}")
    print(f"      Previous failures: {len(attempt_history)}")
    print(f"      Previous attempt available: {'Yes' if previous_attempt else 'No'}")
    
    # Try up to 3 iterations with potential Google searches
    for iteration in range(3):
        print(f"      Iteration {iteration + 1}/3...")
        
        start_time = time.time()
        
        # Send to Qwen
        payload = {
            "model": "qwen2.5-coder:32b", 
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": 0.3,
                "num_predict": 25000
            }
        }
        
        try:
            response = requests.post(
                "http://localhost:11434/api/generate",
                json=payload,
                timeout=7200  # 2 hours
            )
            
            elapsed = time.time() - start_time
            
            if response.status_code != 200:
                print(f"      ❌ Ollama error: {response.status_code}")
                continue
                
            code = response.json()['response'].strip()
            
            # Extract code from markdown if present
            if '```python' in code:
                code = code.split('```python')[1].split('```')[0]
            elif '```py' in code:
                code = code.split('```py')[1].split('```')[0]
            elif '```' in code:
                parts = code.split('```')
                if len(parts) >= 2:
                    code = parts[1]
            
            code = code.strip()
            
            # Check for search requests
            search_requests = re.findall(r'#\s*SEARCH:\s*(.+)', code)
            
            if search_requests:
                print(f"      🔍 Qwen requested {len(search_requests)} searches:")
                search_results = []
                
                for query in search_requests:
                    print(f"         - {query}")
                    result = web_search(query.strip())
                    search_results.append(f"Query: {query}\n{result}")
                
                # Remove search comments from code
                code = re.sub(r'#\s*SEARCH:.*\n?', '', code)
                
                # Add search results to prompt for next iteration
                search_summary = "\n\n".join(search_results)
                prompt += f"\n\nSEARCH RESULTS FROM YOUR REQUESTS:\n{search_summary}\n\nNow use this information to complete the transpiler:"
                
                print(f"      🔄 Feeding search results back to Qwen...")
                continue  # Next iteration with search results
            
            # Validate it's actual Python code
            if 'def transpile' in code and ('import' in code or len(code) > 500):
                print(f"      ✅ Qwen responded in {elapsed:.1f}s ({elapsed/60:.1f} min)")
                print(f"      New transpiler: {len(code)} chars")
                return code
            else:
                print(f"      ❌ Invalid/incomplete transpiler from Qwen")
                print(f"      Code preview: {code[:200]}...")
                if iteration < 2:
                    prompt += f"\n\nPREVIOUS ATTEMPT WAS INVALID:\n{code[:500]}\n\nTry again with COMPLETE Python transpiler code:"
                    continue
                return None
                
        except requests.Timeout:
            print(f"      ⏱️ Qwen timed out after 2 hours")
            return None
        except Exception as e:
            print(f"      ❌ API error: {e}")
            return None
    
    print(f"   ❌ All {iteration+1} attempts failed")
    return None

def regression_test(transpiler_code, test_suite):
    """Test transpiler against all previous cases"""
    print(f"   🧪 Testing against {len(test_suite)} previous cases...")
    import time
    start_time = time.time()
    
    for i, test in enumerate(test_suite, 1):
        print(f"      Test {i}/{len(test_suite)}: {test['elisp'][:30]}...", end='')
        result = test_transpiler(transpiler_code, test['elisp'])
        if not result['success']:
            print(" ❌")
            return False, f"Failed on: {test['elisp']}"
        
        if not verify_cpp_output(result['cpp'], test['expected_output']):
            print(" ❌")
            return False, f"Wrong output for: {test['elisp']}"
        print(" ✓")
    
    elapsed = time.time() - start_time
    return True, f"All {len(test_suite)} tests passed in {elapsed:.1f}s"

def main():
    if len(sys.argv) < 2:
        print("Usage: evolving_transpiler.py '<elisp-code>'")
        sys.exit(1)
    
    import time
    total_start = time.time()
    
    elisp_input = sys.argv[1]
    print(f"🧬 Evolving Transpiler v{get_current_version()}")
    print(f"📝 Input: {elisp_input}")
    print(f"⏰ Started: {time.strftime('%H:%M:%S')}")
    print("=" * 50)
    
    # Load test suite and current transpiler
    test_suite = load_test_suite()
    current_transpiler = load_current_transpiler()
    
    # Check if we've seen this before
    for test in test_suite:
        if test['elisp'] == elisp_input:
            print(f"✅ Already learned this case!")
            result = test_transpiler(current_transpiler, elisp_input)
            if result['success']:
                print("\n" + result['cpp'])
            return
    
    # Get expected output by running in Emacs
    expected_output = run_elisp(elisp_input)
    if expected_output is None:
        print("❌ Failed to run Elisp in Emacs")
        sys.exit(1)
    
    print(f"📊 Expected output: {expected_output}")
    
    # Try current transpiler
    result = test_transpiler(current_transpiler, elisp_input)
    
    if result['success'] and verify_cpp_output(result['cpp'], expected_output):
        print("✅ Current transpiler handles this!")
        print("\n" + result['cpp'])
        # Add to test suite
        test_suite.append({'elisp': elisp_input, 'expected_output': expected_output})
        save_test_suite(test_suite)
        return
    
    # Need to improve
    print("🔧 Current transpiler can't handle this. Evolving...")
    print(f"   Current capabilities: v{get_current_version()} with {len(test_suite)} tests")
    print(f"   Need to learn: {elisp_input[:50]}...")
    
    # First verify current transpiler still works on all old cases
    print("\n🧪 Pre-evolution check: Verifying current transpiler...")
    pre_check, pre_msg = regression_test(current_transpiler, test_suite)
    if not pre_check:
        print(f"   ❌ Current transpiler is broken! {pre_msg}")
        print("   Cannot evolve from broken state. Exiting.")
        sys.exit(1)
    print(f"   ✅ {pre_msg}")
    
    new_test = {'elisp': elisp_input, 'expected_output': expected_output}
    error = result.get('error', 'Cannot transpile')
    
    # Load attempt history for this session
    attempt_history = load_attempt_history()
    failed_attempts = []
    previous_attempt = None
    
    max_attempts = 3
    for attempt in range(max_attempts):
        print(f"\n🤖 Improvement attempt {attempt + 1}/{max_attempts}")
        
        # Get improved version from Qwen with enhanced context
        improved = improve_transpiler(
            current_transpiler, 
            new_test, 
            error, 
            attempt_history=attempt_history,
            previous_attempt=previous_attempt
        )
        
        if not improved:
            print("   ❌ Qwen couldn't generate improvement")
            # Record this failure
            failure_record = {
                'elisp': elisp_input,
                'error': 'Qwen failed to generate code',
                'timestamp': time.strftime('%Y-%m-%d %H:%M:%S'),
                'attempt': attempt + 1
            }
            failed_attempts.append(failure_record)
            continue
        
        # Test the new case
        result = test_transpiler(improved, elisp_input)
        if not result['success']:
            print(f"   ❌ Still fails: {result.get('error', 'Unknown')[:100]}")
            error = result.get('error', error)
            # Record test failure
            failure_record = {
                'elisp': elisp_input,
                'error': f"Test failed: {error}",
                'timestamp': time.strftime('%Y-%m-%d %H:%M:%S'),
                'attempt': attempt + 1
            }
            failed_attempts.append(failure_record)
            previous_attempt = improved  # Remember this attempt for next iteration
            continue
        
        if not verify_cpp_output(result['cpp'], expected_output):
            print("   ❌ Wrong output")
            error = "Output doesn't match expected"
            # Record wrong output failure
            failure_record = {
                'elisp': elisp_input,
                'error': f"Wrong output: got {result['cpp'][:100]}, expected {expected_output[:100]}",
                'timestamp': time.strftime('%Y-%m-%d %H:%M:%S'),
                'attempt': attempt + 1
            }
            failed_attempts.append(failure_record)
            previous_attempt = improved  # Remember this attempt for next iteration
            continue
        
        print("   ✅ Handles new case!")
        
        # Regression test
        print("\n   🧪 Post-evolution check: Testing all cases with new transpiler...")
        passes, msg = regression_test(improved, test_suite)
        
        if passes:
            print(f"   ✅ {msg}")
            
            # Save improved version
            version = get_current_version() + 1
            with open(TRANSPILER_FILE, 'w') as f:
                f.write(improved)
            save_version(version)
            
            # Add to test suite
            test_suite.append(new_test)
            save_test_suite(test_suite)
            
            total_elapsed = time.time() - total_start
            print(f"\n🎉 Evolution successful! Now at v{version}")
            print(f"📈 Can handle {len(test_suite)} different cases")
            print(f"⏱️ Total time: {total_elapsed:.1f}s ({total_elapsed/60:.1f} min)")
            print("\nGenerated C++:")
            print(result['cpp'])
            
            # Save attempt history
            save_attempt_history(failed_attempts)
            return
        else:
            print(f"   ❌ Regression failed: {msg}")
            error = f"Regression: {msg}"
            # Record regression failure
            failure_record = {
                'elisp': elisp_input,
                'error': f"Regression failure: {msg}",
                'timestamp': time.strftime('%Y-%m-%d %H:%M:%S'),
                'attempt': attempt + 1
            }
            failed_attempts.append(failure_record)
            previous_attempt = improved  # Remember this attempt for next iteration
    
    print("\n😞 Evolution failed after all attempts")
    # Save attempt history even on failure
    save_attempt_history(failed_attempts)

if __name__ == "__main__":
    main()
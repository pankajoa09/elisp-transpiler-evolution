#!/usr/bin/env python3
"""
Enhanced evolving transpiler with Google search capability
Qwen can now ask to search for information!
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

# Copy all the existing functions from evolving_transpiler.py
# But modify improve_transpiler to handle search requests

def web_search(query, num_results=3):
    """Search Google and return summaries"""
    if not GOOGLE_AVAILABLE:
        return "Search not available"
    
    print(f"      🔍 Searching Google for: {query}")
    results = []
    
    try:
        for url in google_search(query, num_results=num_results):
            try:
                # Get page content
                response = requests.get(url, timeout=5)
                soup = BeautifulSoup(response.text, 'html.parser')
                
                # Extract text (first 500 chars)
                text = soup.get_text()[:500].replace('\n', ' ').strip()
                results.append(f"URL: {url}\nContent: {text}...\n")
                print(f"         Found: {url[:50]}...")
            except:
                continue
                
        return "\n".join(results) if results else "No results found"
    except Exception as e:
        return f"Search error: {e}"

def improve_transpiler_with_search(current_code, test_case, error, max_searches=3):
    """Ask Qwen to improve transpiler, allowing it to request searches"""
    
    search_count = 0
    search_history = []
    
    while search_count <= max_searches:
        # Build prompt with search history
        prompt = f"""Improve this Python transpiler to handle a new Elisp case.

Current transpiler code:
{current_code}

New test case that fails:
Input: {test_case['elisp']}
Expected output: {test_case['expected_output']}
Error: {error}

You can request web searches by including lines like:
# SEARCH: elisp mapcar function documentation
# SEARCH: how does elisp destructuring-bind work

Previous searches this session:
{chr(10).join(search_history) if search_history else "None yet"}

The transpiler must:
1. Handle the new case
2. Still handle all previous cases
3. Output C++ that produces the expected output

Output the complete improved Python transpiler code. Include SEARCH comments if you need more information."""

        print("   🤖 Asking Qwen to improve transpiler...")
        print(f"      Current transpiler: {len(current_code)} chars")
        print(f"      Teaching it: {test_case['elisp']}")
        print(f"      Searches available: {max_searches - search_count}")
        
        start_time = time.time()
        
        result = subprocess.run(
            ['ollama', 'run', 'qwen2.5-coder:32b'],
            input=prompt,
            capture_output=True,
            text=True,
            timeout=300  # 5 minutes
        )
        
        elapsed = time.time() - start_time
        
        if result.returncode != 0:
            print(f"   ❌ Qwen failed after {elapsed:.1f}s")
            return None
            
        code = result.stdout.strip()
        code = code.replace('```python', '').replace('```py', '').replace('```', '')
        
        # Check for search requests
        search_requests = re.findall(r'#\s*SEARCH:\s*(.+)', code)
        
        if search_requests and search_count < max_searches:
            print(f"   🔎 Qwen requested {len(search_requests)} searches")
            for query in search_requests:
                search_count += 1
                results = web_search(query)
                search_history.append(f"Search: {query}\nResults: {results[:500]}...")
            
            # Remove search comments from code
            code = re.sub(r'#\s*SEARCH:.*\n', '', code)
            
            # Continue loop to let Qwen use the search results
            print(f"   🔄 Giving search results back to Qwen...")
            current_code = code  # Use partially improved code
            continue
        else:
            # No more searches requested or limit reached
            print(f"   ✅ Qwen responded in {elapsed:.1f}s with {search_count} searches")
            print(f"      New transpiler: {len(code)} chars")
            return code
    
    print(f"   ⚠️ Search limit reached ({max_searches} searches)")
    return code

# For testing, create a wrapper script that uses this enhanced version
if __name__ == "__main__":
    print("🧬 Enhanced Evolving Transpiler with Google Search")
    print("Qwen can now search the web for Elisp documentation!")
    
    # Import the rest of the functions from original file
    import sys
    sys.path.insert(0, os.path.dirname(__file__))
    
    # You would copy the rest of the evolving_transpiler.py code here
    # But replace improve_transpiler with improve_transpiler_with_search
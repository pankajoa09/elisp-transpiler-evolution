#!/usr/bin/env python3
"""
Incremental test cases for the evolving transpiler
Very gradual difficulty increase to ensure success
"""

# Start SUPER basic and gradually increase complexity
TEST_CASES = [
    # Basic arithmetic - stuff we know works
    "(+ 2 3)",
    "(- 7 2)", 
    "(* 3 3)",
    "(/ 8 2)",
    
    # Simple comparisons
    "(< 2 5)",
    "(> 7 3)",
    "(= 5 5)",
    "(<= 3 3)",
    "(>= 4 2)",
    
    # Boolean values
    "t",
    "nil",
    
    # Simple if statements
    "(if t 1 2)",
    "(if nil 1 2)",
    "(if (> 3 2) 10 20)",
    "(if (< 5 2) 10 20)",
    
    # Nested arithmetic
    "(+ 1 (+ 2 3))",
    "(* 2 (+ 3 4))",
    "(- (+ 5 5) 3)",
    
    # Simple let bindings
    "(let ((x 10)) x)",
    "(let ((x 5)) (+ x 1))",
    "(let ((x 3) (y 4)) (+ x y))",
    "(let ((a 2)) (* a a))",
    
    # Nested let
    "(let ((x 5)) (let ((y 2)) (+ x y)))",
    
    # Simple functions
    "(defun identity (x) x)",
    "(defun add1 (n) (+ n 1))",
    "(defun double (x) (* x 2))",
    "(defun add (a b) (+ a b))",
    
    # Quoted numbers
    "(quote 5)",
    "(quote 10)",
    
    # Very simple lists
    "(quote (1))",
    "(quote (1 2))",
    "(car (quote (5)))",
    "(car (quote (10 20)))",
    
    # Logical operations - start simple
    "(and t t)",
    "(and t nil)",
    "(and nil t)",
    "(or t nil)",
    "(or nil nil)",
    "(or nil t)",
    "(not t)",
    "(not nil)",
    
    # Cond - like if but multiple branches
    "(cond (t 5))",
    "(cond (nil 5) (t 10))",
    "(cond ((> 3 2) 100))",
    "(cond ((< 5 2) 10) (t 20))",
    
    # Prog1 and progn - sequencing
    "(progn 1 2 3)",
    "(prog1 10 20 30)",
    
    # Simple recursion - factorial
    "(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))",
    
    # Simple strings
    "\"hello\"",
    "\"world\"",
    
    # String length
    "(length \"hello\")",
    "(length \"\")",
    
    # Math functions
    "(abs -5)",
    "(abs 5)",
    "(min 3 5)",
    "(max 3 5)",
    "(mod 10 3)",
    
    # Multiple arg arithmetic
    "(+ 1 2 3)",
    "(* 2 3 4)",
    
    # Nested conditionals
    "(if (if (> 3 2) t nil) 100 200)",
    
    # Variables in conditions
    "(let ((x 5)) (if (> x 3) 100 200))",
    
    # Function calls
    "(defun triple (x) (* x 3))",
    
    # Comments (should be ignored)
    "(+ 1 2) ; this is a comment",
    
    # Cons and list building
    "(cons 1 nil)",
    "(cons 1 (quote (2)))",
    
    # Length of lists
    "(length (quote (1 2 3)))",
    "(length (quote ()))",
    
    # Nth element
    "(nth 0 (quote (10 20 30)))",
    "(nth 1 (quote (10 20 30)))",
    
    # Append
    "(append (quote (1 2)) (quote (3 4)))",
    
    # Member
    "(member 2 (quote (1 2 3)))",
    "(member 5 (quote (1 2 3)))",
    
    # Simple loop - dotimes
    "(dotimes (i 3) i)",
    
    # While loop
    "(let ((x 0)) (while (< x 3) (setq x (+ x 1))) x)",
    
    # Setq
    "(setq x 10)",
    "(progn (setq x 5) x)",
    
    # Lambda
    "((lambda (x) (* x 2)) 5)",
    "((lambda (x y) (+ x y)) 3 4)",
    
    # Mapcar
    "(mapcar (lambda (x) (* x 2)) (quote (1 2 3)))",
    
    # Equal
    "(equal 5 5)",
    "(equal 5 6)",
    "(equal \"hello\" \"hello\")",
    
    # When and unless
    "(when t 5)",
    "(when nil 5)",
    "(unless t 5)",
    "(unless nil 5)",
]

def run_test(test_case):
    """Run a single test through the evolving transpiler"""
    import subprocess
    import time
    
    print(f"\n{'='*60}")
    print(f"Testing: {test_case}")
    print(f"{'='*60}")
    
    try:
        result = subprocess.run(
            ['python3', 'evolving_transpiler.py', test_case],
            capture_output=True,
            text=True,
            timeout=7200,  # 2 hours max per test
            cwd='/Users/pankajahuja/Documents/elisp-transpiler-evolution'
        )
        
        # Check if it succeeded
        if "Evolution successful!" in result.stdout or "Current transpiler handles this!" in result.stdout or "Already learned this case!" in result.stdout:
            print("✅ SUCCESS")
            # Extract version
            import re
            version_match = re.search(r'Now at v(\d+)', result.stdout)
            if version_match:
                print(f"   Evolved to v{version_match.group(1)}")
            return True
        else:
            print("❌ FAILED")
            print(result.stdout[-500:] if result.stdout else "No output")
            return False
            
    except subprocess.TimeoutExpired:
        print("⏱️ TIMEOUT (5 minutes)")
        return False
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return False

def main():
    print("🧬 INCREMENTAL ELISP TRANSPILER EVOLUTION")
    print("=" * 60)
    print(f"Running {len(TEST_CASES)} test cases")
    print("This will take a while. Go make coffee. Or sleep.")
    print("=" * 60)
    
    success_count = 0
    fail_count = 0
    timeout_count = 0
    
    for i, test in enumerate(TEST_CASES, 1):
        print(f"\n📊 Test {i}/{len(TEST_CASES)}")
        result = run_test(test)
        
        if result:
            success_count += 1
        elif "TIMEOUT" in str(result):
            timeout_count += 1
            print("Skipping to next test...")
        else:
            fail_count += 1
        
        # Small delay between tests
        import time
        time.sleep(2)
    
    print("\n" + "=" * 60)
    print("📈 FINAL RESULTS")
    print(f"✅ Success: {success_count}")
    print(f"❌ Failed: {fail_count}")
    print(f"⏱️ Timeout: {timeout_count}")
    print("=" * 60)

if __name__ == "__main__":
    main()
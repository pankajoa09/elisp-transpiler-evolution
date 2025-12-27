#!/bin/bash

test_expr() {
    local expr="$1"
    local expected="$2"
    echo "$expr" | ./elisp_transpiler > test_output.cpp 2>&1
    if [ $? -ne 0 ]; then
        echo "FAIL (transpiler error): $expr"
        return
    fi
    g++ -std=c++17 test_output.cpp -o test_output 2>&1 > /dev/null
    if [ $? -ne 0 ]; then
        echo "FAIL (compile error): $expr"
        return
    fi
    local result=$(./test_output 2>&1)
    if [ "$result" = "$expected" ]; then
        echo "PASS: $expr"
    else
        echo "FAIL: $expr (expected: $expected, got: $result)"
    fi
}

# Basic arithmetic
test_expr "(+ 1 2)" "3"
test_expr "(- 5 3)" "2"
test_expr "(* 2 4)" "8"
test_expr "(/ 10 2)" "5"

# Nested expressions
test_expr "(* (+ 2 3) (- 10 5))" "25"
test_expr "(+ 1 (+ 2 (+ 3 (+ 4 5))))" "15"

# Variables
test_expr "(setq x 10)" "10"
test_expr "(setq y (+ 5 3))" "8"

# Let bindings
test_expr "(let ((x 5)) x)" "5"
test_expr "(let ((x 1) (y 2)) (+ x y))" "3"

# Functions
test_expr "(defun add (a b) (+ a b))" "add"
test_expr "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))" "factorial"

# Conditionals
test_expr "(if (> 5 3) 10 20)" "10"
test_expr "(if (< (+ 1 2) (* 2 3)) 'true 'false)" "true"

# Lists
test_expr "(cons 1 2)" "(1 . 2)"
test_expr "(cons 1 (cons 2 nil))" "(1 2)"
test_expr "(car (cons 1 2))" "1"
test_expr "(cdr (cons 1 (cons 2 nil)))" "(2)"
test_expr "(list 1 2 3 4)" "(1 2 3 4)"

# Advanced
test_expr "(defcustom my-var 42 \"A custom variable\" :type 'integer)" "my-var"
test_expr "(when (boundp 'some-var) (setq some-var 'new-value))" "nil"

echo "Tests completed"

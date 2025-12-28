#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <sstream>
#include <cctype>
#include <stdexcept>
#include <algorithm>
#include <cstring>
#include <cmath>
#include <cstdlib>
#include <numeric>

// AST Node Types
enum class NodeType {
    Integer,
    Symbol,
    List,
    String
};

struct ASTNode {
    NodeType type;
    int int_value = 0;
    std::string str_value;
    std::vector<std::shared_ptr<ASTNode>> children;

    ASTNode(NodeType t) : type(t) {}

    static std::shared_ptr<ASTNode> makeInt(int val) {
        auto node = std::make_shared<ASTNode>(NodeType::Integer);
        node->int_value = val;
        return node;
    }

    static std::shared_ptr<ASTNode> makeSymbol(const std::string& sym) {
        auto node = std::make_shared<ASTNode>(NodeType::Symbol);
        node->str_value = sym;
        return node;
    }

    static std::shared_ptr<ASTNode> makeList(const std::vector<std::shared_ptr<ASTNode>>& children) {
        auto node = std::make_shared<ASTNode>(NodeType::List);
        node->children = children;
        return node;
    }

    static std::shared_ptr<ASTNode> makeString(const std::string& str) {
        auto node = std::make_shared<ASTNode>(NodeType::String);
        node->str_value = str;
        return node;
    }
};

// Tokenizer
class Tokenizer {
    std::string input;
    size_t pos = 0;

public:
    Tokenizer(const std::string& inp) : input(inp) {}

    std::string nextToken() {
        // Skip whitespace
        while (pos < input.size() && std::isspace(input[pos])) pos++;

        if (pos >= input.size()) return "";

        // Handle parentheses and quote
        if (input[pos] == '(' || input[pos] == ')' || input[pos] == '\'') {
            return std::string(1, input[pos++]);
        }

        // Handle strings
        if (input[pos] == '"') {
            size_t start = pos++;
            while (pos < input.size() && input[pos] != '"') {
                if (input[pos] == '\\') pos++; // Handle escape sequences
                pos++;
            }
            if (pos < input.size()) pos++; // Skip closing quote
            return input.substr(start, pos - start);
        }

        // Handle symbols and numbers
        size_t start = pos;
        while (pos < input.size() && !std::isspace(input[pos]) &&
               input[pos] != '(' && input[pos] != ')' && input[pos] != '\'') {
            pos++;
        }
        return input.substr(start, pos - start);
    }
};

// Parser
class Parser {
    Tokenizer& tokenizer;
    std::string current_token;

    void advance() {
        current_token = tokenizer.nextToken();
    }

public:
    Parser(Tokenizer& tok) : tokenizer(tok) {
        advance();
    }

    std::shared_ptr<ASTNode> parse() {
        if (current_token.empty()) {
            throw std::runtime_error("Unexpected end of input");
        }

        // Handle quote
        if (current_token == "'") {
            advance();
            auto quoted = parse();
            // Return quoted symbol as string
            if (quoted->type == NodeType::Symbol) {
                return ASTNode::makeString(quoted->str_value);
            }
            return quoted;
        }

        if (current_token == "(") {
            return parseList();
        } else if (current_token[0] == '"') {
            std::string str = current_token.substr(1, current_token.size() - 2);
            advance();
            return ASTNode::makeString(str);
        } else if (std::isdigit(current_token[0]) ||
                   (current_token[0] == '-' && current_token.size() > 1 && std::isdigit(current_token[1]))) {
            // Check if it's a valid integer (all digits, or - followed by digits)
            bool is_number = true;
            size_t start_idx = (current_token[0] == '-') ? 1 : 0;
            for (size_t i = start_idx; i < current_token.size(); i++) {
                if (!std::isdigit(current_token[i])) {
                    is_number = false;
                    break;
                }
            }

            if (is_number) {
                int val = std::stoi(current_token);
                advance();
                return ASTNode::makeInt(val);
            } else {
                // Not a pure number, treat as symbol (e.g., "1+", "1-")
                std::string sym = current_token;
                advance();
                return ASTNode::makeSymbol(sym);
            }
        } else {
            std::string sym = current_token;
            advance();
            return ASTNode::makeSymbol(sym);
        }
    }

    std::shared_ptr<ASTNode> parseList() {
        advance(); // Skip '('
        std::vector<std::shared_ptr<ASTNode>> children;

        while (!current_token.empty() && current_token != ")") {
            children.push_back(parse());
        }

        if (current_token != ")") {
            throw std::runtime_error("Expected ')'");
        }
        advance(); // Skip ')'

        return ASTNode::makeList(children);
    }
};

// Code Generator
class CodeGenerator {
    std::ostringstream code;
    std::ostringstream declarations;
    std::ostringstream functions;
    std::map<std::string, std::string> variables;
    std::map<std::string, std::pair<std::vector<std::string>, std::shared_ptr<ASTNode>>> user_functions;
    int temp_var_counter = 0;
    int scope_depth = 0;
    bool in_function = false;

    std::string getTempVar() {
        return "temp_" + std::to_string(temp_var_counter++);
    }

    std::string sanitizeIdentifier(const std::string& name) {
        std::string result = name;
        for (char& c : result) {
            if (c == '-') c = '_';
        }
        return result;
    }

    void collectSetqVars(std::shared_ptr<ASTNode> node, std::vector<std::string>& vars) {
        if (!node || node->type != NodeType::List || node->children.empty()) return;

        auto& op = node->children[0];
        if (op->type == NodeType::Symbol && op->str_value == "setq") {
            for (size_t i = 1; i + 1 < node->children.size(); i += 2) {
                vars.push_back(node->children[i]->str_value);
                // Also check the value expression for nested setqs
                collectSetqVars(node->children[i + 1], vars);
            }
        } else {
            // Recursively check all children
            for (auto& child : node->children) {
                collectSetqVars(child, vars);
            }
        }
    }

    std::string generateExpr(std::shared_ptr<ASTNode> node) {
        if (node->type == NodeType::Integer) {
            return std::to_string(node->int_value);
        } else if (node->type == NodeType::Symbol) {
            // Handle special symbols
            if (node->str_value == "nil") {
                return "0";
            }
            return sanitizeIdentifier(node->str_value);
        } else if (node->type == NodeType::String) {
            return "\"" + node->str_value + "\"";
        } else if (node->type == NodeType::List && !node->children.empty()) {
            auto& op = node->children[0];

            if (op->type == NodeType::Symbol) {
                std::string op_name = op->str_value;

                // Arithmetic operators
                if (op_name == "+" || op_name == "-" || op_name == "*" || op_name == "/") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[1]) + " " +
                               op_name + " " + generateExpr(node->children[2]) + ")";
                    } else if (node->children.size() > 3) {
                        std::string result = generateExpr(node->children[1]);
                        for (size_t i = 2; i < node->children.size(); i++) {
                            result = "(" + result + " " + op_name + " " +
                                    generateExpr(node->children[i]) + ")";
                        }
                        return result;
                    }
                }

                // Math functions
                if (op_name == "abs") {
                    if (node->children.size() == 2) {
                        return "abs(" + generateExpr(node->children[1]) + ")";
                    }
                }

                if (op_name == "min") {
                    if (node->children.size() == 3) {
                        return "std::min(" + generateExpr(node->children[1]) + ", " +
                               generateExpr(node->children[2]) + ")";
                    } else if (node->children.size() > 3) {
                        std::string result = "std::min(" + generateExpr(node->children[1]) + ", " +
                                           generateExpr(node->children[2]) + ")";
                        for (size_t i = 3; i < node->children.size(); i++) {
                            result = "std::min(" + result + ", " + generateExpr(node->children[i]) + ")";
                        }
                        return result;
                    }
                }

                if (op_name == "max") {
                    if (node->children.size() == 3) {
                        return "std::max(" + generateExpr(node->children[1]) + ", " +
                               generateExpr(node->children[2]) + ")";
                    } else if (node->children.size() > 3) {
                        std::string result = "std::max(" + generateExpr(node->children[1]) + ", " +
                                           generateExpr(node->children[2]) + ")";
                        for (size_t i = 3; i < node->children.size(); i++) {
                            result = "std::max(" + result + ", " + generateExpr(node->children[i]) + ")";
                        }
                        return result;
                    }
                }

                if (op_name == "mod" || op_name == "rem" || op_name == "%") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[1]) + " % " +
                               generateExpr(node->children[2]) + ")";
                    }
                }

                if (op_name == "expt") {
                    if (node->children.size() == 3) {
                        return "((int)pow(" + generateExpr(node->children[1]) + ", " +
                               generateExpr(node->children[2]) + "))";
                    }
                }

                if (op_name == "sqrt") {
                    if (node->children.size() == 2) {
                        return "((int)sqrt(" + generateExpr(node->children[1]) + "))";
                    }
                }

                if (op_name == "floor") {
                    if (node->children.size() == 2) {
                        return "((int)floor(" + generateExpr(node->children[1]) + "))";
                    }
                }

                if (op_name == "ceiling") {
                    if (node->children.size() == 2) {
                        return "((int)ceil(" + generateExpr(node->children[1]) + "))";
                    }
                }

                if (op_name == "round") {
                    if (node->children.size() == 2) {
                        return "((int)round(" + generateExpr(node->children[1]) + "))";
                    }
                }

                if (op_name == "sin") {
                    if (node->children.size() == 2) {
                        return "sin(" + generateExpr(node->children[1]) + ")";
                    }
                }

                if (op_name == "cos") {
                    if (node->children.size() == 2) {
                        return "cos(" + generateExpr(node->children[1]) + ")";
                    }
                }

                if (op_name == "tan") {
                    if (node->children.size() == 2) {
                        return "tan(" + generateExpr(node->children[1]) + ")";
                    }
                }

                if (op_name == "log") {
                    if (node->children.size() == 2) {
                        return "log(" + generateExpr(node->children[1]) + ")";
                    }
                }

                if (op_name == "exp") {
                    if (node->children.size() == 2) {
                        return "exp(" + generateExpr(node->children[1]) + ")";
                    }
                }

                if (op_name == "random") {
                    if (node->children.size() == 2) {
                        return "(rand() % " + generateExpr(node->children[1]) + ")";
                    } else {
                        return "rand()";
                    }
                }

                if (op_name == "1+" || op_name == "add1") {
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + " + 1)";
                    }
                }

                if (op_name == "1-" || op_name == "sub1") {
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + " - 1)";
                    }
                }

                if (op_name == "gcd") {
                    if (node->children.size() == 3) {
                        std::string a = generateExpr(node->children[1]);
                        std::string b = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_a = " << a << ";\n";
                        code << "    auto " << temp << "_b = " << b << ";\n";
                        code << "    while (" << temp << "_b != 0) {\n";
                        code << "        auto t = " << temp << "_b;\n";
                        code << "        " << temp << "_b = " << temp << "_a % " << temp << "_b;\n";
                        code << "        " << temp << "_a = t;\n";
                        code << "    }\n";
                        return temp + "_a";
                    }
                }

                if (op_name == "lcm") {
                    if (node->children.size() == 3) {
                        std::string a = generateExpr(node->children[1]);
                        std::string b = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        // LCM(a,b) = abs(a*b) / GCD(a,b)
                        code << "    auto " << temp << "_a = " << a << ";\n";
                        code << "    auto " << temp << "_b = " << b << ";\n";
                        code << "    auto " << temp << "_gcd_a = " << temp << "_a;\n";
                        code << "    auto " << temp << "_gcd_b = " << temp << "_b;\n";
                        code << "    while (" << temp << "_gcd_b != 0) {\n";
                        code << "        auto t = " << temp << "_gcd_b;\n";
                        code << "        " << temp << "_gcd_b = " << temp << "_gcd_a % " << temp << "_gcd_b;\n";
                        code << "        " << temp << "_gcd_a = t;\n";
                        code << "    }\n";
                        code << "    auto " << temp << " = abs(" << temp << "_a * " << temp << "_b) / " << temp << "_gcd_a;\n";
                        return temp;
                    }
                }

                // Comparison operators
                if (op_name == ">" || op_name == "<" || op_name == "=" ||
                    op_name == ">=" || op_name == "<=") {
                    if (node->children.size() == 3) {
                        std::string cpp_op = op_name;
                        if (op_name == "=") cpp_op = "==";
                        return "(" + generateExpr(node->children[1]) + " " +
                               cpp_op + " " + generateExpr(node->children[2]) + ")";
                    }
                }

                // Logical operators
                if (op_name == "and") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[1]) + " && " +
                               generateExpr(node->children[2]) + ")";
                    } else if (node->children.size() > 3) {
                        std::string result = generateExpr(node->children[1]);
                        for (size_t i = 2; i < node->children.size(); i++) {
                            result = "(" + result + " && " +
                                    generateExpr(node->children[i]) + ")";
                        }
                        return result;
                    }
                }

                if (op_name == "or") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[1]) + " || " +
                               generateExpr(node->children[2]) + ")";
                    } else if (node->children.size() > 3) {
                        std::string result = generateExpr(node->children[1]);
                        for (size_t i = 2; i < node->children.size(); i++) {
                            result = "(" + result + " || " +
                                    generateExpr(node->children[i]) + ")";
                        }
                        return result;
                    }
                }

                // Function call
                if (user_functions.count(op_name)) {
                    std::string call = sanitizeIdentifier(op_name) + "(";
                    for (size_t i = 1; i < node->children.size(); i++) {
                        if (i > 1) call += ", ";
                        call += generateExpr(node->children[i]);
                    }
                    call += ")";
                    return call;
                }

                // Built-in functions
                if (op_name == "cons") {
                    // For simplicity, represent cons as creating a pair
                    return "std::make_pair(" + generateExpr(node->children[1]) + ", " +
                           generateExpr(node->children[2]) + ")";
                }

                if (op_name == "car") {
                    // car returns first element of cons cell
                    return "(" + generateExpr(node->children[1]) + ").first";
                }

                if (op_name == "cdr") {
                    // cdr returns second element of cons cell
                    return "(" + generateExpr(node->children[1]) + ").second";
                }

                if (op_name == "list") {
                    std::string list_code = "std::vector<int>{";
                    for (size_t i = 1; i < node->children.size(); i++) {
                        if (i > 1) list_code += ", ";
                        list_code += generateExpr(node->children[i]);
                    }
                    list_code += "}";
                    return list_code;
                }

                // length - get list/string length
                if (op_name == "length") {
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + ").size()";
                    }
                }

                // nth - get nth element
                if (op_name == "nth") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[2]) + ")[" +
                               generateExpr(node->children[1]) + "]";
                    }
                }

                // append - concatenate lists
                if (op_name == "append") {
                    if (node->children.size() == 3) {
                        // Simple 2-list append
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << generateExpr(node->children[1]) << ";\n";
                        code << "    " << temp << ".insert(" << temp << ".end(), "
                             << generateExpr(node->children[2]) << ".begin(), "
                             << generateExpr(node->children[2]) << ".end());\n";
                        return temp;
                    }
                }

                // reverse - reverse a list
                if (op_name == "reverse") {
                    if (node->children.size() == 2) {
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << generateExpr(node->children[1]) << ";\n";
                        code << "    std::reverse(" << temp << ".begin(), " << temp << ".end());\n";
                        return temp;
                    }
                }

                // member - check if element is in list
                if (op_name == "member" || op_name == "memq") {
                    if (node->children.size() == 3) {
                        std::string elem = generateExpr(node->children[1]);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list << ";\n";
                        code << "    auto it_" << temp << " = std::find(" << temp << ".begin(), "
                             << temp << ".end(), " << elem << ");\n";
                        return "(it_" + temp + " != " + temp + ".end() ? 1 : 0)";
                    }
                }

                // remove - remove element from list
                if (op_name == "remove" || op_name == "delete") {
                    if (node->children.size() == 3) {
                        std::string elem = generateExpr(node->children[1]);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list << ";\n";
                        code << "    " << temp << ".erase(std::remove(" << temp << ".begin(), "
                             << temp << ".end(), " << elem << "), " << temp << ".end());\n";
                        return temp;
                    }
                }

                // push - add element to front of list (modifies list)
                if (op_name == "push") {
                    if (node->children.size() == 3) {
                        std::string elem = generateExpr(node->children[1]);
                        std::string list_var = node->children[2]->str_value;
                        std::string sanitized_list = sanitizeIdentifier(list_var);
                        code << "    " << sanitized_list << ".insert(" << sanitized_list
                             << ".begin(), " << elem << ");\n";
                        return elem;
                    }
                }

                // pop - remove and return first element (modifies list)
                if (op_name == "pop") {
                    if (node->children.size() == 2) {
                        std::string list_var = node->children[1]->str_value;
                        std::string sanitized_list = sanitizeIdentifier(list_var);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << sanitized_list << "[0];\n";
                        code << "    " << sanitized_list << ".erase(" << sanitized_list << ".begin());\n";
                        return temp;
                    }
                }

                // nthcdr - get list starting from nth element
                if (op_name == "nthcdr") {
                    if (node->children.size() == 3) {
                        std::string n = generateExpr(node->children[1]);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list << ";\n";
                        code << "    std::vector<int> " << temp << "_result(" << temp << ".begin() + "
                             << n << ", " << temp << ".end());\n";
                        return temp + "_result";
                    }
                }

                // butlast - return list without last element
                if (op_name == "butlast") {
                    if (node->children.size() == 2) {
                        std::string list = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list << ";\n";
                        code << "    if (" << temp << ".size() > 0) " << temp << ".pop_back();\n";
                        return temp;
                    }
                }

                // last - get last element
                if (op_name == "last") {
                    if (node->children.size() == 2) {
                        std::string list = generateExpr(node->children[1]);
                        return "(" + list + ".back())";
                    }
                }

                // Vector operations
                if (op_name == "vector") {
                    // Same as list for now
                    std::string vec_code = "std::vector<int>{";
                    for (size_t i = 1; i < node->children.size(); i++) {
                        if (i > 1) vec_code += ", ";
                        vec_code += generateExpr(node->children[i]);
                    }
                    vec_code += "}";
                    return vec_code;
                }

                if (op_name == "make-vector") {
                    if (node->children.size() >= 2) {
                        std::string size = generateExpr(node->children[1]);
                        std::string init = (node->children.size() >= 3) ?
                                          generateExpr(node->children[2]) : "0";
                        return "std::vector<int>(" + size + ", " + init + ")";
                    }
                }

                if (op_name == "aref" || op_name == "elt") {
                    if (node->children.size() == 3) {
                        std::string arr = generateExpr(node->children[1]);
                        std::string idx = generateExpr(node->children[2]);
                        return "(" + arr + ")[" + idx + "]";
                    }
                }

                if (op_name == "aset") {
                    if (node->children.size() == 4) {
                        std::string arr = node->children[1]->str_value;
                        std::string sanitized_arr = sanitizeIdentifier(arr);
                        std::string idx = generateExpr(node->children[2]);
                        std::string val = generateExpr(node->children[3]);
                        code << "    " << sanitized_arr << "[" << idx << "] = " << val << ";\n";
                        return val;
                    }
                }

                // Hash table operations
                if (op_name == "make-hash-table") {
                    // Simplified: ignore all keyword arguments
                    std::string temp = getTempVar();
                    code << "    std::map<int, int> " << temp << ";\n";
                    return temp;
                }

                if (op_name == "gethash") {
                    if (node->children.size() >= 3) {
                        std::string key = generateExpr(node->children[1]);
                        std::string table_var = node->children[2]->str_value;
                        std::string sanitized_table = sanitizeIdentifier(table_var);
                        std::string default_val = (node->children.size() >= 4) ?
                                                 generateExpr(node->children[3]) : "0";
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_it = " << sanitized_table << ".find(" << key << ");\n";
                        code << "    int " << temp << " = (" << temp << "_it != " << sanitized_table << ".end()) ? "
                             << temp << "_it->second : " << default_val << ";\n";
                        return temp;
                    }
                }

                if (op_name == "puthash") {
                    if (node->children.size() == 4) {
                        std::string key = generateExpr(node->children[1]);
                        std::string val = generateExpr(node->children[2]);
                        std::string table_var = node->children[3]->str_value;
                        std::string sanitized_table = sanitizeIdentifier(table_var);
                        code << "    " << sanitized_table << "[" << key << "] = " << val << ";\n";
                        return val;
                    }
                }

                if (op_name == "remhash") {
                    if (node->children.size() == 3) {
                        std::string key = generateExpr(node->children[1]);
                        std::string table_var = node->children[2]->str_value;
                        std::string sanitized_table = sanitizeIdentifier(table_var);
                        code << "    " << sanitized_table << ".erase(" << key << ");\n";
                        return "0";
                    }
                }

                if (op_name == "clrhash") {
                    if (node->children.size() == 2) {
                        std::string table_var = node->children[1]->str_value;
                        std::string sanitized_table = sanitizeIdentifier(table_var);
                        code << "    " << sanitized_table << ".clear();\n";
                        return "0";
                    }
                }

                if (op_name == "hash-table-count" || op_name == "hash-table-size") {
                    if (node->children.size() == 2) {
                        std::string table = generateExpr(node->children[1]);
                        return "(int)" + table + ".size()";
                    }
                }

                if (op_name == "hash-table-p") {
                    // Simplified: just return 1 (true) for now
                    return "1";
                }

                // Sequence functions
                if (op_name == "sort") {
                    if (node->children.size() >= 2) {
                        std::string seq = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << seq << ";\n";
                        code << "    std::sort(" << temp << ".begin(), " << temp << ".end());\n";
                        return temp;
                    }
                }

                if (op_name == "find") {
                    if (node->children.size() == 3) {
                        std::string item = generateExpr(node->children[1]);
                        std::string seq = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << seq << ";\n";
                        code << "    auto it_" << temp << " = std::find(" << temp << ".begin(), "
                             << temp << ".end(), " << item << ");\n";
                        return "(it_" + temp + " != " + temp + ".end() ? *it_" + temp + " : 0)";
                    }
                }

                if (op_name == "position") {
                    if (node->children.size() == 3) {
                        std::string item = generateExpr(node->children[1]);
                        std::string seq = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << seq << ";\n";
                        code << "    auto it_" << temp << " = std::find(" << temp << ".begin(), "
                             << temp << ".end(), " << item << ");\n";
                        return "(it_" + temp + " != " + temp + ".end() ? "
                               "(int)(it_" + temp + " - " + temp + ".begin()) : -1)";
                    }
                }

                // count - count occurrences
                if (op_name == "count") {
                    if (node->children.size() == 3) {
                        std::string item = generateExpr(node->children[1]);
                        std::string seq = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << seq << ";\n";
                        return "(int)std::count(" + temp + ".begin(), " + temp + ".end(), " + item + ")";
                    }
                }

                // make-list - create list with repeated element
                if (op_name == "make-list") {
                    if (node->children.size() >= 2) {
                        std::string size = generateExpr(node->children[1]);
                        std::string init = (node->children.size() >= 3) ?
                                          generateExpr(node->children[2]) : "0";
                        return "std::vector<int>(" + size + ", " + init + ")";
                    }
                }

                // copy-list - copy a list
                if (op_name == "copy-list" || op_name == "copy-sequence") {
                    if (node->children.size() == 2) {
                        return generateExpr(node->children[1]); // Vectors copy by value anyway
                    }
                }

                // nreverse - destructive reverse
                if (op_name == "nreverse") {
                    if (node->children.size() == 2) {
                        std::string list_var = node->children[1]->str_value;
                        std::string sanitized = sanitizeIdentifier(list_var);
                        code << "    std::reverse(" << sanitized << ".begin(), " << sanitized << ".end());\n";
                        return sanitized;
                    }
                }

                // fill - fill sequence with value
                if (op_name == "fill") {
                    if (node->children.size() == 3) {
                        std::string seq_var = node->children[1]->str_value;
                        std::string sanitized = sanitizeIdentifier(seq_var);
                        std::string val = generateExpr(node->children[2]);
                        code << "    std::fill(" << sanitized << ".begin(), " << sanitized << ".end(), " << val << ");\n";
                        return sanitized;
                    }
                }

                // subseq - subsequence
                if (op_name == "subseq") {
                    if (node->children.size() >= 3) {
                        std::string seq = generateExpr(node->children[1]);
                        std::string start = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_full = " << seq << ";\n";
                        if (node->children.size() >= 4) {
                            std::string end = generateExpr(node->children[3]);
                            code << "    std::vector<int> " << temp << "(" << temp << "_full.begin() + "
                                 << start << ", " << temp << "_full.begin() + " << end << ");\n";
                        } else {
                            code << "    std::vector<int> " << temp << "(" << temp << "_full.begin() + "
                                 << start << ", " << temp << "_full.end());\n";
                        }
                        return temp;
                    }
                }

                // mapcar - map function over list (simplified: uses lambda)
                if (op_name == "mapcar") {
                    if (node->children.size() == 3) {
                        // For user functions, generate a loop
                        std::string func_name = node->children[1]->str_value;
                        std::string sanitized_func = sanitizeIdentifier(func_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_in = " << list << ";\n";
                        code << "    std::vector<int> " << temp << ";\n";
                        code << "    for (auto item : " << temp << "_in) {\n";
                        code << "        " << temp << ".push_back(" << sanitized_func << "(item));\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // mapc - like mapcar but for side effects, returns original list
                if (op_name == "mapc") {
                    if (node->children.size() == 3) {
                        std::string func_name = node->children[1]->str_value;
                        std::string sanitized_func = sanitizeIdentifier(func_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list << ";\n";
                        code << "    for (auto item : " << temp << ") {\n";
                        code << "        " << sanitized_func << "(item);\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // mapconcat - map and concatenate with separator
                if (op_name == "mapconcat") {
                    if (node->children.size() == 4) {
                        std::string func_name = node->children[1]->str_value;
                        std::string sanitized_func = sanitizeIdentifier(func_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string sep = generateExpr(node->children[3]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_in = " << list << ";\n";
                        code << "    std::string " << temp << ";\n";
                        code << "    bool first = true;\n";
                        code << "    for (auto item : " << temp << "_in) {\n";
                        code << "        if (!first) " << temp << " += " << sep << ";\n";
                        code << "        first = false;\n";
                        code << "        " << temp << " += std::to_string(" << sanitized_func << "(item));\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // remove-if - remove elements matching predicate
                if (op_name == "remove-if") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_in = " << list << ";\n";
                        code << "    std::vector<int> " << temp << ";\n";
                        code << "    for (auto item : " << temp << "_in) {\n";
                        code << "        if (!" << sanitized_pred << "(item)) " << temp << ".push_back(item);\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // remove-if-not (keep-if) - keep elements matching predicate
                if (op_name == "remove-if-not" || op_name == "keep-if") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_in = " << list << ";\n";
                        code << "    std::vector<int> " << temp << ";\n";
                        code << "    for (auto item : " << temp << "_in) {\n";
                        code << "        if (" << sanitized_pred << "(item)) " << temp << ".push_back(item);\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // find-if - find first element matching predicate
                if (op_name == "find-if") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_in = " << list << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    for (auto item : " << temp << "_in) {\n";
                        code << "        if (" << sanitized_pred << "(item)) { " << temp << " = item; break; }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // Association lists (alists) - using vectors of pairs
                if (op_name == "assoc" || op_name == "assq") {
                    if (node->children.size() == 3) {
                        std::string key = generateExpr(node->children[1]);
                        std::string alist = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << alist << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    // Simplified: returns value, not pair\n";
                        code << "    for (size_t i = 0; i + 1 < " << temp << "_list.size(); i += 2) {\n";
                        code << "        if (" << temp << "_list[i] == " << key << ") {\n";
                        code << "            " << temp << " = " << temp << "_list[i + 1];\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                if (op_name == "rassoc" || op_name == "rassq") {
                    if (node->children.size() == 3) {
                        std::string val = generateExpr(node->children[1]);
                        std::string alist = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << alist << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    // Reverse assoc: find by value, return key\n";
                        code << "    for (size_t i = 0; i + 1 < " << temp << "_list.size(); i += 2) {\n";
                        code << "        if (" << temp << "_list[i + 1] == " << val << ") {\n";
                        code << "            " << temp << " = " << temp << "_list[i];\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // Property lists (plists) - flat key-value pairs
                if (op_name == "plist-get") {
                    if (node->children.size() == 3) {
                        std::string plist = generateExpr(node->children[1]);
                        std::string key = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_plist = " << plist << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    for (size_t i = 0; i + 1 < " << temp << "_plist.size(); i += 2) {\n";
                        code << "        if (" << temp << "_plist[i] == " << key << ") {\n";
                        code << "            " << temp << " = " << temp << "_plist[i + 1];\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                if (op_name == "plist-put") {
                    if (node->children.size() == 4) {
                        std::string plist_var = node->children[1]->str_value;
                        std::string sanitized_plist = sanitizeIdentifier(plist_var);
                        std::string key = generateExpr(node->children[2]);
                        std::string val = generateExpr(node->children[3]);
                        code << "    // plist-put: update or append\n";
                        code << "    bool found = false;\n";
                        code << "    for (size_t i = 0; i + 1 < " << sanitized_plist << ".size(); i += 2) {\n";
                        code << "        if (" << sanitized_plist << "[i] == " << key << ") {\n";
                        code << "            " << sanitized_plist << "[i + 1] = " << val << ";\n";
                        code << "            found = true;\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        code << "    if (!found) {\n";
                        code << "        " << sanitized_plist << ".push_back(" << key << ");\n";
                        code << "        " << sanitized_plist << ".push_back(" << val << ");\n";
                        code << "    }\n";
                        return sanitized_plist;
                    }
                }

                if (op_name == "plist-member") {
                    if (node->children.size() == 3) {
                        std::string plist = generateExpr(node->children[1]);
                        std::string key = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_plist = " << plist << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    for (size_t i = 0; i < " << temp << "_plist.size(); i += 2) {\n";
                        code << "        if (" << temp << "_plist[i] == " << key << ") {\n";
                        code << "            " << temp << " = 1;\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // reduce - fold a list with a binary function
                if (op_name == "reduce") {
                    if (node->children.size() >= 3) {
                        std::string func_name = node->children[1]->str_value;
                        std::string sanitized_func = sanitizeIdentifier(func_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << list << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    if (!" << temp << "_list.empty()) {\n";
                        code << "        " << temp << " = " << temp << "_list[0];\n";
                        code << "        for (size_t i = 1; i < " << temp << "_list.size(); i++) {\n";
                        code << "            " << temp << " = " << sanitized_func << "(" << temp << ", " << temp << "_list[i]);\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // every - test if all elements satisfy predicate
                if (op_name == "every") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << list << ";\n";
                        code << "    int " << temp << " = 1;\n";
                        code << "    for (auto item : " << temp << "_list) {\n";
                        code << "        if (!" << sanitized_pred << "(item)) {\n";
                        code << "            " << temp << " = 0;\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // some - test if any element satisfies predicate
                if (op_name == "some") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << list << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    for (auto item : " << temp << "_list) {\n";
                        code << "        if (" << sanitized_pred << "(item)) {\n";
                        code << "            " << temp << " = 1;\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // notany - test if no element satisfies predicate
                if (op_name == "notany") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << list << ";\n";
                        code << "    int " << temp << " = 1;\n";
                        code << "    for (auto item : " << temp << "_list) {\n";
                        code << "        if (" << sanitized_pred << "(item)) {\n";
                        code << "            " << temp << " = 0;\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // notevery - test if not all elements satisfy predicate
                if (op_name == "notevery") {
                    if (node->children.size() == 3) {
                        std::string pred_name = node->children[1]->str_value;
                        std::string sanitized_pred = sanitizeIdentifier(pred_name);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << list << ";\n";
                        code << "    int " << temp << " = 0;\n";
                        code << "    for (auto item : " << temp << "_list) {\n";
                        code << "        if (!" << sanitized_pred << "(item)) {\n";
                        code << "            " << temp << " = 1;\n";
                        code << "            break;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // adjoin - add element to list if not present
                if (op_name == "adjoin") {
                    if (node->children.size() == 3) {
                        std::string item = generateExpr(node->children[1]);
                        std::string list = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list << ";\n";
                        code << "    if (std::find(" << temp << ".begin(), " << temp << ".end(), " << item << ") == " << temp << ".end()) {\n";
                        code << "        " << temp << ".push_back(" << item << ");\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // union - set union of two lists
                if (op_name == "union") {
                    if (node->children.size() == 3) {
                        std::string list1 = generateExpr(node->children[1]);
                        std::string list2 = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << list1 << ";\n";
                        code << "    auto " << temp << "_list2 = " << list2 << ";\n";
                        code << "    for (auto item : " << temp << "_list2) {\n";
                        code << "        if (std::find(" << temp << ".begin(), " << temp << ".end(), item) == " << temp << ".end()) {\n";
                        code << "            " << temp << ".push_back(item);\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // intersection - set intersection of two lists
                if (op_name == "intersection") {
                    if (node->children.size() == 3) {
                        std::string list1 = generateExpr(node->children[1]);
                        std::string list2 = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list1 = " << list1 << ";\n";
                        code << "    auto " << temp << "_list2 = " << list2 << ";\n";
                        code << "    std::vector<int> " << temp << ";\n";
                        code << "    for (auto item : " << temp << "_list1) {\n";
                        code << "        if (std::find(" << temp << "_list2.begin(), " << temp << "_list2.end(), item) != " << temp << "_list2.end()) {\n";
                        code << "            if (std::find(" << temp << ".begin(), " << temp << ".end(), item) == " << temp << ".end()) {\n";
                        code << "                " << temp << ".push_back(item);\n";
                        code << "            }\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // set-difference - elements in first list but not in second
                if (op_name == "set-difference") {
                    if (node->children.size() == 3) {
                        std::string list1 = generateExpr(node->children[1]);
                        std::string list2 = generateExpr(node->children[2]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list1 = " << list1 << ";\n";
                        code << "    auto " << temp << "_list2 = " << list2 << ";\n";
                        code << "    std::vector<int> " << temp << ";\n";
                        code << "    for (auto item : " << temp << "_list1) {\n";
                        code << "        if (std::find(" << temp << "_list2.begin(), " << temp << "_list2.end(), item) == " << temp << "_list2.end()) {\n";
                        code << "            " << temp << ".push_back(item);\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // not - logical negation
                if (op_name == "not" || op_name == "null") {
                    if (node->children.size() == 2) {
                        return "(!" + generateExpr(node->children[1]) + ")";
                    }
                }

                // boundp - always return false for simplicity
                if (op_name == "boundp") {
                    return "false";
                }

                // Type predicates
                if (op_name == "numberp" || op_name == "integerp") {
                    // In our simplified model, we'll use a heuristic
                    // For now, always return true for simplicity
                    return "true";
                }

                if (op_name == "stringp") {
                    // For now, return false (most things are ints in our system)
                    return "false";
                }

                if (op_name == "symbolp") {
                    return "false";
                }

                if (op_name == "listp") {
                    // Check if it's a vector/list type
                    return "false";  // Simplified for now
                }

                if (op_name == "atom") {
                    // Opposite of cons cell - in our model, most things are atoms
                    return "true";
                }

                if (op_name == "consp") {
                    // Check if it's a cons cell
                    return "false";  // Simplified for now
                }

                if (op_name == "endp") {
                    // Check if list is empty
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + ".empty() ? 1 : 0)";
                    }
                }

                // Equality predicates
                if (op_name == "eq" || op_name == "eql") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[1]) + " == " +
                               generateExpr(node->children[2]) + ")";
                    }
                }

                if (op_name == "equal") {
                    if (node->children.size() == 3) {
                        return "(" + generateExpr(node->children[1]) + " == " +
                               generateExpr(node->children[2]) + ")";
                    }
                }

                // zerop - test if zero
                if (op_name == "zerop") {
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + " == 0)";
                    }
                }

                // plusp/minusp - positive/negative tests
                if (op_name == "plusp") {
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + " > 0)";
                    }
                }

                if (op_name == "minusp") {
                    if (node->children.size() == 2) {
                        return "(" + generateExpr(node->children[1]) + " < 0)";
                    }
                }

                // evenp/oddp
                if (op_name == "evenp") {
                    if (node->children.size() == 2) {
                        return "((" + generateExpr(node->children[1]) + " % 2) == 0)";
                    }
                }

                if (op_name == "oddp") {
                    if (node->children.size() == 2) {
                        return "((" + generateExpr(node->children[1]) + " % 2) != 0)";
                    }
                }

                // message - just return the first string argument
                if (op_name == "message") {
                    if (node->children.size() > 1) {
                        return generateExpr(node->children[1]);
                    }
                    return "\"\"";
                }

                // concat - concatenate strings
                if (op_name == "concat") {
                    if (node->children.size() >= 2) {
                        std::string result = "std::string(\"\")";
                        for (size_t i = 1; i < node->children.size(); i++) {
                            result = "(" + result + " + " + generateExpr(node->children[i]) + ")";
                        }
                        return result;
                    }
                    return "std::string(\"\")";
                }

                // substring - extract substring
                if (op_name == "substring") {
                    if (node->children.size() >= 3) {
                        std::string str = generateExpr(node->children[1]);
                        std::string start = generateExpr(node->children[2]);
                        // Wrap in std::string to support string literals
                        std::string str_wrapped = "std::string(" + str + ")";
                        if (node->children.size() >= 4) {
                            std::string end = generateExpr(node->children[3]);
                            return str_wrapped + ".substr(" + start + ", " + end + " - " + start + ")";
                        } else {
                            return str_wrapped + ".substr(" + start + ")";
                        }
                    }
                    return "\"\"";
                }

                // string= - string equality
                if (op_name == "string=" || op_name == "string-equal") {
                    if (node->children.size() == 3) {
                        return "(std::string(" + generateExpr(node->children[1]) + ") == std::string(" +
                               generateExpr(node->children[2]) + ") ? 1 : 0)";
                    }
                }

                // string< - string less than
                if (op_name == "string<" || op_name == "string-lessp") {
                    if (node->children.size() == 3) {
                        return "(std::string(" + generateExpr(node->children[1]) + ") < std::string(" +
                               generateExpr(node->children[2]) + ") ? 1 : 0)";
                    }
                }

                // upcase - convert to uppercase
                if (op_name == "upcase") {
                    if (node->children.size() == 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = std::string(" << str << ");\n";
                        code << "    std::transform(" << temp << ".begin(), " << temp << ".end(), "
                             << temp << ".begin(), ::toupper);\n";
                        return temp;
                    }
                }

                // downcase - convert to lowercase
                if (op_name == "downcase") {
                    if (node->children.size() == 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = std::string(" << str << ");\n";
                        code << "    std::transform(" << temp << ".begin(), " << temp << ".end(), "
                             << temp << ".begin(), ::tolower);\n";
                        return temp;
                    }
                }

                // capitalize - capitalize first character
                if (op_name == "capitalize") {
                    if (node->children.size() == 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = std::string(" << str << ");\n";
                        code << "    if (!" << temp << ".empty()) {\n";
                        code << "        " << temp << "[0] = std::toupper(" << temp << "[0]);\n";
                        code << "        std::transform(" << temp << ".begin() + 1, " << temp << ".end(), "
                             << temp << ".begin() + 1, ::tolower);\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // upcase-initials - capitalize first letter of each word
                if (op_name == "upcase-initials") {
                    if (node->children.size() == 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = std::string(" << str << ");\n";
                        code << "    bool new_word = true;\n";
                        code << "    for (size_t i = 0; i < " << temp << ".length(); i++) {\n";
                        code << "        if (std::isspace(" << temp << "[i])) {\n";
                        code << "            new_word = true;\n";
                        code << "        } else if (new_word) {\n";
                        code << "            " << temp << "[i] = std::toupper(" << temp << "[i]);\n";
                        code << "            new_word = false;\n";
                        code << "        }\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // string-length - get string length
                if (op_name == "string-length") {
                    if (node->children.size() == 2) {
                        return "((int)std::string(" + generateExpr(node->children[1]) + ").length())";
                    }
                }

                // char-to-string - convert character code to string
                if (op_name == "char-to-string") {
                    if (node->children.size() == 2) {
                        std::string ch = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    std::string " << temp << "(1, (char)" << ch << ");\n";
                        return temp;
                    }
                }

                // string-to-char - get first character code from string
                if (op_name == "string-to-char") {
                    if (node->children.size() == 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_s = std::string(" << str << ");\n";
                        code << "    int " << temp << " = " << temp << "_s.empty() ? 0 : (int)" << temp << "_s[0];\n";
                        return temp;
                    }
                }

                // aref for strings - get character at index
                if (op_name == "aref" && node->children.size() == 3) {
                    // Already handled by vector aref, but add string support
                }

                // string-trim functions
                if (op_name == "string-trim" || op_name == "string-trim-left" || op_name == "string-trim-right") {
                    if (node->children.size() == 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = std::string(" << str << ");\n";
                        if (op_name == "string-trim" || op_name == "string-trim-left") {
                            code << "    " << temp << ".erase(" << temp << ".begin(), "
                                 << "std::find_if(" << temp << ".begin(), " << temp << ".end(), "
                                 << "[](unsigned char ch) { return !std::isspace(ch); }));\n";
                        }
                        if (op_name == "string-trim" || op_name == "string-trim-right") {
                            code << "    " << temp << ".erase(std::find_if(" << temp << ".rbegin(), "
                                 << temp << ".rend(), [](unsigned char ch) { return !std::isspace(ch); }).base(), "
                                 << temp << ".end());\n";
                        }
                        return temp;
                    }
                }

                // split-string
                if (op_name == "split-string") {
                    if (node->children.size() >= 2) {
                        std::string str = generateExpr(node->children[1]);
                        std::string sep = (node->children.size() >= 3) ? generateExpr(node->children[2]) : "\" \"";
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_str = std::string(" << str << ");\n";
                        code << "    auto " << temp << "_sep = std::string(" << sep << ");\n";
                        code << "    std::vector<int> " << temp << ";\n";
                        code << "    // Simplified: count words only\n";
                        code << "    size_t pos = 0, count = 0;\n";
                        code << "    while ((pos = " << temp << "_str.find(" << temp << "_sep, pos)) != std::string::npos) {\n";
                        code << "        count++; pos += " << temp << "_sep.length();\n";
                        code << "    }\n";
                        code << "    " << temp << ".push_back(count + 1);\n";
                        return temp;
                    }
                }

                // string-join
                if (op_name == "string-join") {
                    if (node->children.size() >= 2) {
                        std::string list = generateExpr(node->children[1]);
                        std::string sep = (node->children.size() >= 3) ? generateExpr(node->children[2]) : "\"\"";
                        std::string temp = getTempVar();
                        code << "    auto " << temp << "_list = " << list << ";\n";
                        code << "    std::string " << temp << ";\n";
                        code << "    for (size_t i = 0; i < " << temp << "_list.size(); i++) {\n";
                        code << "        if (i > 0) " << temp << " += " << sep << ";\n";
                        code << "        " << temp << " += std::to_string(" << temp << "_list[i]);\n";
                        code << "    }\n";
                        return temp;
                    }
                }

                // symbol functions (simplified)
                if (op_name == "symbol-name") {
                    if (node->children.size() == 2) {
                        return "std::string(\"symbol\")";  // Simplified
                    }
                }

                if (op_name == "symbol-value") {
                    if (node->children.size() == 2) {
                        return generateExpr(node->children[1]);  // Simplified
                    }
                }

                if (op_name == "intern") {
                    if (node->children.size() == 2) {
                        return generateExpr(node->children[1]);  // Simplified
                    }
                }

                // print, prin1, princ - output functions
                if (op_name == "print" || op_name == "prin1") {
                    if (node->children.size() == 2) {
                        std::string val = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << val << ";\n";
                        code << "    std::cout << " << temp << " << std::endl;\n";
                        return temp;
                    }
                }

                if (op_name == "princ") {
                    if (node->children.size() == 2) {
                        std::string val = generateExpr(node->children[1]);
                        std::string temp = getTempVar();
                        code << "    auto " << temp << " = " << val << ";\n";
                        code << "    std::cout << " << temp << ";\n";
                        return temp;
                    }
                }

                // terpri - print newline
                if (op_name == "terpri") {
                    code << "    std::cout << std::endl;\n";
                    return "0";
                }

                // format - simple formatting (simplified)
                if (op_name == "format") {
                    if (node->children.size() >= 2) {
                        // Very basic: just concatenate args
                        std::string result = "std::string(\"\")";
                        for (size_t i = 2; i < node->children.size(); i++) {
                            result = "(" + result + " + std::to_string(" + generateExpr(node->children[i]) + "))";
                        }
                        return result;
                    }
                }

                // identity - return argument unchanged
                if (op_name == "identity") {
                    if (node->children.size() == 2) {
                        return generateExpr(node->children[1]);
                    }
                }

                // constantly - return constant function (simplified)
                if (op_name == "constantly") {
                    if (node->children.size() == 2) {
                        return generateExpr(node->children[1]);
                    }
                }

                // if - as an expression (ternary operator)
                if (op_name == "if") {
                    if (node->children.size() >= 3) {
                        std::string condition = generateExpr(node->children[1]);
                        std::string then_expr = generateExpr(node->children[2]);
                        std::string else_expr = node->children.size() > 3 ?
                                               generateExpr(node->children[3]) : "0";
                        return "(" + condition + " ? " + then_expr + " : " + else_expr + ")";
                    }
                }

                // cond - as an expression (nested ternaries)
                if (op_name == "cond") {
                    if (node->children.size() >= 2) {
                        std::string result;
                        for (size_t i = node->children.size() - 1; i >= 1; i--) {
                            auto clause = node->children[i];
                            if (clause->type != NodeType::List || clause->children.empty()) continue;

                            // Get the condition and result
                            std::string condition = generateExpr(clause->children[0]);
                            std::string value = clause->children.size() > 1 ?
                                               generateExpr(clause->children[clause->children.size() - 1]) : "0";

                            // Check for 't' (always true, default case)
                            if (clause->children[0]->type == NodeType::Symbol &&
                                clause->children[0]->str_value == "t") {
                                result = value;
                            } else {
                                if (result.empty()) {
                                    result = "(" + condition + " ? " + value + " : 0)";
                                } else {
                                    result = "(" + condition + " ? " + value + " : " + result + ")";
                                }
                            }
                        }
                        return result.empty() ? "0" : result;
                    }
                }

                // setq - as an expression (returns the assigned value)
                if (op_name == "setq") {
                    if (node->children.size() == 3) {
                        // Single setq: (setq var value)
                        std::string var_name = node->children[1]->str_value;
                        std::string sanitized_var_name = sanitizeIdentifier(var_name);
                        std::string value = generateExpr(node->children[2]);

                        // Assignment expression returns the value
                        return "(" + sanitized_var_name + " = " + value + ")";
                    }
                }

                // setf - generalized assignment (like setq for now)
                if (op_name == "setf") {
                    if (node->children.size() == 3) {
                        std::string var_name = node->children[1]->str_value;
                        std::string sanitized_var_name = sanitizeIdentifier(var_name);
                        std::string value = generateExpr(node->children[2]);
                        return "(" + sanitized_var_name + " = " + value + ")";
                    }
                }

                // incf - increment in place
                if (op_name == "incf") {
                    if (node->children.size() >= 2) {
                        std::string var_name = node->children[1]->str_value;
                        std::string sanitized_var_name = sanitizeIdentifier(var_name);
                        if (node->children.size() >= 3) {
                            // incf with delta: (incf x 5)
                            std::string delta = generateExpr(node->children[2]);
                            return "(" + sanitized_var_name + " += " + delta + ")";
                        } else {
                            // incf by 1: (incf x)
                            return "(++" + sanitized_var_name + ")";
                        }
                    }
                }

                // decf - decrement in place
                if (op_name == "decf") {
                    if (node->children.size() >= 2) {
                        std::string var_name = node->children[1]->str_value;
                        std::string sanitized_var_name = sanitizeIdentifier(var_name);
                        if (node->children.size() >= 3) {
                            // decf with delta: (decf x 5)
                            std::string delta = generateExpr(node->children[2]);
                            return "(" + sanitized_var_name + " -= " + delta + ")";
                        } else {
                            // decf by 1: (decf x)
                            return "(--" + sanitized_var_name + ")";
                        }
                    }
                }

                // let/let* - as an expression (for nested let)
                if (op_name == "let" || op_name == "let*") {
                    if (node->children.size() >= 3) {
                        // Generate an inline lambda that executes the let bindings
                        std::ostringstream lambda;
                        auto bindings = node->children[1];

                        lambda << "[&]() { ";

                        if (bindings->type == NodeType::List) {
                            for (auto& binding : bindings->children) {
                                if (binding->type == NodeType::List && binding->children.size() >= 2) {
                                    std::string var_name = binding->children[0]->str_value;
                                    std::string sanitized_var_name = sanitizeIdentifier(var_name);
                                    std::string value = generateExpr(binding->children[1]);
                                    lambda << "int " << sanitized_var_name << " = " << value << "; ";
                                }
                            }
                        }

                        // Return the last expression
                        std::string result = generateExpr(node->children[node->children.size() - 1]);
                        lambda << "return " << result << "; }()";

                        return lambda.str();
                    }
                }
            }
        }

        return "0";
    }

    void generateStatement(std::shared_ptr<ASTNode> node, bool is_last = false) {
        if (node->type != NodeType::List || node->children.empty()) {
            return;
        }

        auto& op = node->children[0];
        if (op->type != NodeType::Symbol) return;

        std::string op_name = op->str_value;

        // setq - variable assignment
        if (op_name == "setq") {
            // Collect all setq variables (including nested ones)
            std::vector<std::string> all_setq_vars;
            collectSetqVars(node, all_setq_vars);

            // Declare all variables that haven't been declared yet
            for (const std::string& var : all_setq_vars) {
                if (!variables.count(var)) {
                    variables[var] = "auto";
                    code << "    auto " << sanitizeIdentifier(var) << " = 0;\n";
                }
            }

            std::string last_var;
            for (size_t i = 1; i + 1 < node->children.size(); i += 2) {
                std::string var_name = node->children[i]->str_value;
                std::string sanitized_var_name = sanitizeIdentifier(var_name);
                std::string value = generateExpr(node->children[i + 1]);

                code << "    " << sanitized_var_name << " = " << value << ";\n";
                last_var = sanitized_var_name;
            }

            if (is_last && !last_var.empty()) {
                code << "    std::cout << " << last_var << " << std::endl;\n";
            }
            return;
        }

        // let/let* - local bindings
        if (op_name == "let" || op_name == "let*") {
            if (node->children.size() < 3) return;

            auto bindings = node->children[1];

            if (is_last) {
                std::string temp = getTempVar();
                code << "    int " << temp << ";\n";
                code << "    {\n";

                if (bindings->type == NodeType::List) {
                    for (auto& binding : bindings->children) {
                        if (binding->type == NodeType::List && binding->children.size() >= 2) {
                            std::string var_name = binding->children[0]->str_value;
                            std::string sanitized_var_name = sanitizeIdentifier(var_name);
                            std::string value = generateExpr(binding->children[1]);
                            code << "        int " << sanitized_var_name << " = " << value << ";\n";
                            variables[var_name] = "int";
                        }
                    }
                }

                // Body - last expression sets the temp variable
                for (size_t i = 2; i < node->children.size(); i++) {
                    bool is_last_expr = (i == node->children.size() - 1);
                    if (is_last_expr) {
                        std::string result = generateExpr(node->children[i]);
                        code << "        " << temp << " = " << result << ";\n";
                    } else {
                        std::string expr = generateExpr(node->children[i]);
                        code << "        " << expr << ";\n";
                    }
                }

                code << "    }\n";
                code << "    std::cout << " << temp << " << std::endl;\n";
            } else {
                code << "    {\n";

                if (bindings->type == NodeType::List) {
                    for (auto& binding : bindings->children) {
                        if (binding->type == NodeType::List && binding->children.size() >= 2) {
                            std::string var_name = binding->children[0]->str_value;
                            std::string sanitized_var_name = sanitizeIdentifier(var_name);
                            std::string value = generateExpr(binding->children[1]);
                            code << "        int " << sanitized_var_name << " = " << value << ";\n";
                            variables[var_name] = "int";
                        }
                    }
                }

                // Body
                for (size_t i = 2; i < node->children.size(); i++) {
                    std::string expr = generateExpr(node->children[i]);
                    code << "        " << expr << ";\n";
                }

                code << "    }\n";
            }
            return;
        }

        // defun - function definition
        if (op_name == "defun") {
            if (node->children.size() < 4) return;

            std::string func_name = node->children[1]->str_value;
            std::string sanitized_func_name = sanitizeIdentifier(func_name);
            auto params = node->children[2];

            std::vector<std::string> param_names;
            if (params->type == NodeType::List) {
                for (auto& param : params->children) {
                    param_names.push_back(param->str_value);
                }
            }

            user_functions[func_name] = {param_names, node->children[3]};

            // Generate function
            functions << "int " << sanitized_func_name << "(";
            for (size_t i = 0; i < param_names.size(); i++) {
                if (i > 0) functions << ", ";
                functions << "int " << sanitizeIdentifier(param_names[i]);
            }
            functions << ") {\n";

            // Function body
            std::string body_expr = generateExpr(node->children[3]);
            functions << "    return " << body_expr << ";\n";
            functions << "}\n\n";

            if (is_last) {
                code << "    std::cout << \"" << func_name << "\" << std::endl;\n";
            }
            return;
        }

        // if - conditional
        if (op_name == "if") {
            if (node->children.size() < 3) return;

            std::string condition = generateExpr(node->children[1]);
            std::string temp = getTempVar();
            std::string then_expr = generateExpr(node->children[2]);
            std::string else_expr = node->children.size() > 3 ? generateExpr(node->children[3]) : "0";

            code << "    auto " << temp << " = (" << condition << ") ? " << then_expr << " : " << else_expr << ";\n";

            if (is_last) {
                code << "    std::cout << " << temp << " << std::endl;\n";
            }
            return;
        }

        // cond - multi-branch conditional
        if (op_name == "cond") {
            if (node->children.size() < 2) return;

            bool first = true;
            for (size_t i = 1; i < node->children.size(); i++) {
                auto clause = node->children[i];
                if (clause->type != NodeType::List || clause->children.empty()) continue;

                std::string condition = generateExpr(clause->children[0]);

                if (first) {
                    code << "    ";
                    first = false;
                } else {
                    code << " else ";
                }

                // Check for 't' (default case)
                if (clause->children[0]->type == NodeType::Symbol &&
                    clause->children[0]->str_value == "t") {
                    code << "{\n";
                } else {
                    code << "if (" << condition << ") {\n";
                }

                // Execute clause body
                for (size_t j = 1; j < clause->children.size(); j++) {
                    bool is_last_in_clause = (j == clause->children.size() - 1);
                    if (is_last_in_clause && is_last) {
                        // Print the last expression of any matched clause when cond is final statement
                        code << "        std::cout << " << generateExpr(clause->children[j]) << " << std::endl;\n";
                    } else if (is_last_in_clause) {
                        code << "        " << generateExpr(clause->children[j]) << ";\n";
                    } else {
                        code << "        " << generateExpr(clause->children[j]) << ";\n";
                    }
                }

                code << "    }";
            }
            code << "\n";
            return;
        }

        // case - pattern matching (like switch)
        if (op_name == "case") {
            if (node->children.size() < 3) return;

            std::string test_expr = generateExpr(node->children[1]);
            std::string temp = getTempVar();
            code << "    auto " << temp << " = " << test_expr << ";\n";

            bool first = true;
            for (size_t i = 2; i < node->children.size(); i++) {
                auto clause = node->children[i];
                if (clause->type != NodeType::List || clause->children.empty()) continue;

                if (first) {
                    code << "    ";
                    first = false;
                } else {
                    code << " else ";
                }

                // Check for 'otherwise' or 't' (default case)
                if (clause->children[0]->type == NodeType::Symbol &&
                    (clause->children[0]->str_value == "otherwise" ||
                     clause->children[0]->str_value == "t")) {
                    code << "{\n";
                } else {
                    // Handle single value or list of values
                    std::string condition;
                    if (clause->children[0]->type == NodeType::List) {
                        // Multiple values: (case x ((1 2 3) result))
                        condition = "(";
                        for (size_t j = 0; j < clause->children[0]->children.size(); j++) {
                            if (j > 0) condition += " || ";
                            condition += temp + " == " + generateExpr(clause->children[0]->children[j]);
                        }
                        condition += ")";
                    } else {
                        // Single value: (case x (1 result))
                        condition = "(" + temp + " == " + generateExpr(clause->children[0]) + ")";
                    }
                    code << "if " << condition << " {\n";
                }

                // Execute clause body
                for (size_t j = 1; j < clause->children.size(); j++) {
                    bool is_last_in_clause = (j == clause->children.size() - 1);
                    if (is_last_in_clause && is_last) {
                        code << "        std::cout << " << generateExpr(clause->children[j]) << " << std::endl;\n";
                    } else {
                        code << "        " << generateExpr(clause->children[j]) << ";\n";
                    }
                }

                code << "    }";
            }
            code << "\n";
            return;
        }

        // unless - opposite of when
        if (op_name == "unless") {
            if (node->children.size() < 3) return;

            std::string condition = generateExpr(node->children[1]);
            code << "    if (!(" << condition << ")) {\n";

            for (size_t i = 2; i < node->children.size(); i++) {
                bool is_last_expr = (i == node->children.size() - 1);
                if (is_last_expr && is_last) {
                    code << "        std::cout << " << generateExpr(node->children[i]) << " << std::endl;\n";
                } else {
                    code << "        " << generateExpr(node->children[i]) << ";\n";
                }
            }

            code << "    }\n";
            return;
        }

        // when - conditional without else
        if (op_name == "when") {
            if (node->children.size() < 3) return;

            // Collect all setq variables in the when body
            std::vector<std::string> all_setq_vars;
            for (size_t i = 2; i < node->children.size(); i++) {
                collectSetqVars(node->children[i], all_setq_vars);
            }

            // Declare all variables that haven't been declared yet
            for (const std::string& var : all_setq_vars) {
                if (!variables.count(var)) {
                    variables[var] = "auto";
                    code << "    auto " << sanitizeIdentifier(var) << " = 0;\n";
                }
            }

            std::string condition = generateExpr(node->children[1]);

            if (is_last) {
                std::string temp = getTempVar();
                code << "    auto " << temp << " = \"nil\";\n";
                code << "    if (" << condition << ") {\n";

                for (size_t i = 2; i < node->children.size(); i++) {
                    bool is_last_in_when = (i == node->children.size() - 1);
                    if (is_last_in_when) {
                        code << "        " << temp << " = " << generateExpr(node->children[i]) << ";\n";
                    } else {
                        code << "        " << generateExpr(node->children[i]) << ";\n";
                    }
                }

                code << "    }\n";
                code << "    std::cout << " << temp << " << std::endl;\n";
            } else {
                code << "    if (" << condition << ") {\n";
                for (size_t i = 2; i < node->children.size(); i++) {
                    code << "        " << generateExpr(node->children[i]) << ";\n";
                }
                code << "    }\n";
            }
            return;
        }

        // progn - execute multiple expressions, return last
        if (op_name == "progn") {
            if (node->children.size() < 2) return;

            for (size_t i = 1; i < node->children.size(); i++) {
                bool is_last_expr = (i == node->children.size() - 1);
                if (is_last_expr && is_last) {
                    std::string result = generateExpr(node->children[i]);
                    code << "    std::cout << " << result << " << std::endl;\n";
                } else if (is_last_expr) {
                    // Just evaluate, don't print
                    std::string expr = generateExpr(node->children[i]);
                    code << "    " << expr << ";\n";
                } else {
                    // Execute for side effects
                    std::string expr = generateExpr(node->children[i]);
                    code << "    " << expr << ";\n";
                }
            }
            return;
        }

        // while - loop
        if (op_name == "while") {
            if (node->children.size() < 2) return;

            std::string condition = generateExpr(node->children[1]);
            code << "    while (" << condition << ") {\n";

            for (size_t i = 2; i < node->children.size(); i++) {
                std::string expr = generateExpr(node->children[i]);
                code << "        " << expr << ";\n";
            }

            code << "    }\n";

            if (is_last) {
                code << "    std::cout << \"nil\" << std::endl;\n";
            }
            return;
        }

        // dolist - iterate over list
        if (op_name == "dolist") {
            if (node->children.size() < 3) return;

            auto spec = node->children[1];
            if (spec->type == NodeType::List && spec->children.size() >= 2) {
                std::string var = spec->children[0]->str_value;
                std::string sanitized_var = sanitizeIdentifier(var);
                std::string list_expr = generateExpr(spec->children[1]);

                code << "    for (auto " << sanitized_var << " : " << list_expr << ") {\n";

                for (size_t i = 2; i < node->children.size(); i++) {
                    std::string expr = generateExpr(node->children[i]);
                    code << "        " << expr << ";\n";
                }

                code << "    }\n";

                if (is_last) {
                    code << "    std::cout << \"nil\" << std::endl;\n";
                }
            }
            return;
        }

        // dotimes - iterate N times
        if (op_name == "dotimes") {
            if (node->children.size() < 2) return;

            auto spec = node->children[1];
            if (spec->type == NodeType::List && spec->children.size() >= 2) {
                std::string var = spec->children[0]->str_value;
                std::string sanitized_var = sanitizeIdentifier(var);
                std::string count_expr = generateExpr(spec->children[1]);

                // Optional result form
                std::string result_expr = "0";
                if (spec->children.size() >= 3) {
                    result_expr = generateExpr(spec->children[2]);
                }

                code << "    for (int " << sanitized_var << " = 0; " << sanitized_var
                     << " < " << count_expr << "; " << sanitized_var << "++) {\n";

                for (size_t i = 2; i < node->children.size(); i++) {
                    std::string expr = generateExpr(node->children[i]);
                    code << "        " << expr << ";\n";
                }

                code << "    }\n";

                if (is_last) {
                    code << "    std::cout << " << result_expr << " << std::endl;\n";
                }
            }
            return;
        }

        // defcustom, add-hook, use-package, interactive - just acknowledge them
        if (op_name == "defcustom" || op_name == "add-hook" ||
            op_name == "use-package" || op_name == "interactive") {
            if (is_last) {
                // For defcustom, return the variable name
                if (op_name == "defcustom" && node->children.size() > 1) {
                    code << "    std::cout << \"" << node->children[1]->str_value << "\" << std::endl;\n";
                } else {
                    code << "    std::cout << \"" << op_name << "\" << std::endl;\n";
                }
            }
            return;
        }

        // lambda
        if (op_name == "lambda") {
            if (is_last) {
                code << "    std::cout << \"lambda\" << std::endl;\n";
            }
            return;
        }

        // Default: evaluate expression
        if (is_last) {
            // Check if this is a cons, car, cdr, or list operation for special formatting
            if (node->type == NodeType::List && !node->children.empty() &&
                node->children[0]->type == NodeType::Symbol) {
                std::string op_name = node->children[0]->str_value;

                if (op_name == "cons") {
                    // Check if second element is nil or another cons (proper list)
                    bool is_proper_list = false;
                    std::vector<std::string> list_elements;
                    list_elements.push_back(generateExpr(node->children[1]));

                    auto second = node->children[2];
                    if (second->type == NodeType::Symbol && second->str_value == "nil") {
                        is_proper_list = true;
                    } else if (second->type == NodeType::List && !second->children.empty() &&
                               second->children[0]->type == NodeType::Symbol &&
                               second->children[0]->str_value == "cons") {
                        // It's a nested cons - check if it forms a proper list
                        is_proper_list = true;
                        auto current = second;
                        while (current->type == NodeType::List && !current->children.empty() &&
                               current->children[0]->type == NodeType::Symbol &&
                               current->children[0]->str_value == "cons") {
                            list_elements.push_back(generateExpr(current->children[1]));
                            current = current->children[2];
                            if (current->type == NodeType::Symbol && current->str_value == "nil") {
                                break;
                            }
                            if (!(current->type == NodeType::List && !current->children.empty() &&
                                  current->children[0]->type == NodeType::Symbol &&
                                  current->children[0]->str_value == "cons")) {
                                is_proper_list = false;
                                break;
                            }
                        }
                    }

                    if (is_proper_list) {
                        code << "    std::cout << \"(\"";
                        for (size_t i = 0; i < list_elements.size(); i++) {
                            if (i > 0) code << " << \" \"";
                            code << " << " << list_elements[i];
                        }
                        code << " << \")\" << std::endl;\n";
                    } else {
                        std::string first = generateExpr(node->children[1]);
                        std::string second_expr = generateExpr(node->children[2]);
                        code << "    std::cout << \"(\" << " << first;
                        code << " << \" . \" << " << second_expr;
                        code << " << \")\" << std::endl;\n";
                    }
                    return;
                }

                if (op_name == "list") {
                    code << "    std::cout << \"(\"";
                    for (size_t i = 1; i < node->children.size(); i++) {
                        if (i > 1) code << " << \" \"";
                        code << " << " << generateExpr(node->children[i]);
                    }
                    code << " << \")\" << std::endl;\n";
                    return;
                }

                if (op_name == "cdr") {
                    // Check if argument is a cons expression
                    auto arg = node->children[1];
                    if (arg->type == NodeType::List && !arg->children.empty() &&
                        arg->children[0]->type == NodeType::Symbol &&
                        arg->children[0]->str_value == "cons") {
                        // Get the cdr part (second element of cons)
                        auto cdr_part = arg->children[2];

                        // Check if it's a proper list
                        std::vector<std::string> list_elements;
                        bool is_proper_list = false;

                        if (cdr_part->type == NodeType::Symbol && cdr_part->str_value == "nil") {
                            // cdr of (cons X nil) is nil, print as ()
                            code << "    std::cout << \"nil\" << std::endl;\n";
                            return;
                        } else if (cdr_part->type == NodeType::List && !cdr_part->children.empty() &&
                                   cdr_part->children[0]->type == NodeType::Symbol &&
                                   cdr_part->children[0]->str_value == "cons") {
                            // It's a cons cell - extract elements
                            is_proper_list = true;
                            auto current = cdr_part;
                            while (current->type == NodeType::List && !current->children.empty() &&
                                   current->children[0]->type == NodeType::Symbol &&
                                   current->children[0]->str_value == "cons") {
                                list_elements.push_back(generateExpr(current->children[1]));
                                current = current->children[2];
                                if (current->type == NodeType::Symbol && current->str_value == "nil") {
                                    break;
                                }
                            }

                            if (is_proper_list && !list_elements.empty()) {
                                code << "    std::cout << \"(\"";
                                for (size_t i = 0; i < list_elements.size(); i++) {
                                    if (i > 0) code << " << \" \"";
                                    code << " << " << list_elements[i];
                                }
                                code << " << \")\" << std::endl;\n";
                                return;
                            }
                        }
                    }

                    // Default cdr handling
                    std::string expr = generateExpr(node);
                    code << "    std::cout << " << expr << " << std::endl;\n";
                    return;
                }
            }

            std::string expr = generateExpr(node);
            code << "    std::cout << " << expr << " << std::endl;\n";
        } else {
            std::string expr = generateExpr(node);
            code << "    " << expr << ";\n";
        }
    }

public:
    std::string generate(std::shared_ptr<ASTNode> ast) {
        std::ostringstream output;

        output << "#include <iostream>\n";
        output << "#include <utility>\n";
        output << "#include <vector>\n";
        output << "#include <string>\n\n";

        // Generate code
        generateStatement(ast, true);

        // Assemble final output
        output << functions.str();
        output << "int main() {\n";
        output << code.str();
        output << "    return 0;\n";
        output << "}\n";

        return output.str();
    }
};

int main() {
    try {
        // Read input
        std::string input;
        std::string line;
        while (std::getline(std::cin, line)) {
            input += line + "\n";
        }

        // Parse
        Tokenizer tokenizer(input);
        Parser parser(tokenizer);
        auto ast = parser.parse();

        // Generate C++ code
        CodeGenerator generator;
        std::string cpp_code = generator.generate(ast);

        // Output C++ code
        std::cout << cpp_code;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}

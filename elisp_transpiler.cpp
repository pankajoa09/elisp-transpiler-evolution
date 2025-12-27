#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <sstream>
#include <cctype>
#include <stdexcept>
#include <algorithm>

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
            int val = std::stoi(current_token);
            advance();
            return ASTNode::makeInt(val);
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

    std::string generateExpr(std::shared_ptr<ASTNode> node) {
        if (node->type == NodeType::Integer) {
            return std::to_string(node->int_value);
        } else if (node->type == NodeType::Symbol) {
            // Handle special symbols
            if (node->str_value == "nil") {
                return "0";
            }
            return node->str_value;
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
                    std::string call = op_name + "(";
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
                    std::string list_code = "{";
                    for (size_t i = 1; i < node->children.size(); i++) {
                        if (i > 1) list_code += ", ";
                        list_code += generateExpr(node->children[i]);
                    }
                    list_code += "}";
                    return list_code;
                }

                // boundp - always return false for simplicity
                if (op_name == "boundp") {
                    return "false";
                }

                // message - just return the first string argument
                if (op_name == "message") {
                    if (node->children.size() > 1) {
                        return generateExpr(node->children[1]);
                    }
                    return "\"\"";
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
            for (size_t i = 1; i + 1 < node->children.size(); i += 2) {
                std::string var_name = node->children[i]->str_value;
                std::string value = generateExpr(node->children[i + 1]);

                if (!variables.count(var_name)) {
                    variables[var_name] = "int";
                    code << "    int " << var_name << " = " << value << ";\n";
                } else {
                    code << "    " << var_name << " = " << value << ";\n";
                }

                if (is_last && i + 2 >= node->children.size()) {
                    code << "    std::cout << " << var_name << " << std::endl;\n";
                }
            }
            return;
        }

        // let - local bindings
        if (op_name == "let") {
            if (node->children.size() < 3) return;

            auto bindings = node->children[1];
            code << "    {\n";

            if (bindings->type == NodeType::List) {
                for (auto& binding : bindings->children) {
                    if (binding->type == NodeType::List && binding->children.size() >= 2) {
                        std::string var_name = binding->children[0]->str_value;
                        std::string value = generateExpr(binding->children[1]);
                        code << "        int " << var_name << " = " << value << ";\n";
                        variables[var_name] = "int";
                    }
                }
            }

            // Body
            for (size_t i = 2; i < node->children.size(); i++) {
                bool is_last_expr = (i == node->children.size() - 1);
                if (is_last_expr && is_last) {
                    std::string result = generateExpr(node->children[i]);
                    code << "        std::cout << " << result << " << std::endl;\n";
                } else {
                    generateStatement(node->children[i], is_last_expr && is_last);
                }
            }

            code << "    }\n";
            return;
        }

        // defun - function definition
        if (op_name == "defun") {
            if (node->children.size() < 4) return;

            std::string func_name = node->children[1]->str_value;
            auto params = node->children[2];

            std::vector<std::string> param_names;
            if (params->type == NodeType::List) {
                for (auto& param : params->children) {
                    param_names.push_back(param->str_value);
                }
            }

            user_functions[func_name] = {param_names, node->children[3]};

            // Generate function
            functions << "int " << func_name << "(";
            for (size_t i = 0; i < param_names.size(); i++) {
                if (i > 0) functions << ", ";
                functions << "int " << param_names[i];
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

        // when - conditional without else
        if (op_name == "when") {
            if (node->children.size() < 3) return;

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

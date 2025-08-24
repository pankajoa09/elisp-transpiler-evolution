#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <map>
#include <variant>
#include <memory>
#include <numeric>
#include <cctype>

using namespace std;

struct SExpr;
using Expr = variant<int, string, shared_ptr<SExpr>>;

struct SExpr {
    string type;
    vector<Expr> args;
};

enum class VarType { INT, STRING };

struct Variable {
    string name;
    VarType type;
};

map<string, Variable> variables;

VarType get_type(const Expr& expr) {
    if (holds_alternative<int>(expr)) return VarType::INT;
    if (holds_alternative<string>(expr)) {
        const auto& str = get<string>(expr);
        if (str.front() == '"' && str.back() == '"') return VarType::STRING;
    }
    throw runtime_error("Unknown type");
}

string var_type_to_string(VarType type) {
    switch (type) {
        case VarType::INT: return "int";
        case VarType::STRING: return "string";
        default: throw runtime_error("Invalid variable type");
    }
}

shared_ptr<SExpr> parse_expr(istream& is);
void parse_list(istream& is, vector<Expr>& list);

shared_ptr<SExpr> parse_expr(istream& is) {
    char ch;
    is >> ch;

    if (ch == '(') {
        auto sexpr = make_shared<SExpr>();
        is >> sexpr->type;
        parse_list(is, sexpr->args);
        return sexpr;
    } else if (isdigit(ch)) {
        is.putback(ch);
        int val;
        is >> val;
        return make_shared<SExpr>(SExpr{"int", {val}});
    } else if (ch == '"') {
        string str;
        getline(is, str, '"');
        return make_shared<SExpr>(SExpr{"string", {string{'"'} + str + string{'"'}}});
    } else {
        is.putback(ch);
        string sym;
        is >> sym;
        return make_shared<SExpr>(SExpr{"symbol", {sym}});
    }
}

void parse_list(istream& is, vector<Expr>& list) {
    char ch;
    while (is.get(ch)) {
        if (ch == ')') break;
        else {
            is.putback(ch);
            list.push_back(parse_expr(is));
        }
    }
}

string generate_code(const SExpr& expr);

string generate_code(const Expr& expr) {
    if (holds_alternative<int>(expr)) return to_string(get<int>(expr));
    if (holds_alternative<string>(expr)) return get<string>(expr);
    const auto& sexpr = get<shared_ptr<SExpr>>(expr);
    if (sexpr->type == "defun") {
        string func_name = get<string>(get<shared_ptr<SExpr>>(sexpr->args[0])->args[0]);
        vector<string> params;
        for (const auto& arg : get<shared_ptr<SExpr>>(sexpr->args[0])->args) {
            if (holds_alternative<shared_ptr<SExpr>>(arg)) {
                params.push_back(var_type_to_string(get_type(sexpr->args.back())) + " " + get<string>(get<shared_ptr<SExpr>>(arg)->args[0]));
            }
        }
        string param_str = "(" + (params.empty() ? "" : (accumulate(params.begin(), params.end(), string{}, [](string a, const string& b) { return a + ", " + b; }).substr(2))) + ")";
        return var_type_to_string(get_type(sexpr->args.back())) + " " + func_name + param_str + " { return " + generate_code(*sexpr->args.back()) + "; }";
    } else if (sexpr->type == "setq") {
        string var_name = get<string>(get<shared_ptr<SExpr>>(sexpr->args[0])->args[0]);
        VarType var_type = get_type(sexpr->args[1]);
        variables[var_name] = {var_name, var_type};
        return var_type_to_string(var_type) + " " + var_name + " = " + generate_code(sexpr->args[1]) + ";";
    } else if (sexpr->type == "+" || sexpr->type == "-" || sexpr->type == "*" || sexpr->type == "/") {
        return "(" + generate_code(sexpr->args[0]) + " " + sexpr->type + " " + generate_code(sexpr->args[1]) + ")";
    } else if (sexpr->type == "<" || sexpr->type == ">" || sexpr->type == "<=" || sexpr->type == ">=" || sexpr->type == "=") {
        return "(" + generate_code(sexpr->args[0]) + " " + sexpr->type + " " + generate_code(sexpr->args[1]) + ")";
    } else if (sexpr->type == "if") {
        return "(" + generate_code(sexpr->args[0]) + ") ? " + generate_code(sexpr->args[1]) + " : " + generate_code(sexpr->args[2]);
    } else if (sexpr->type == "let") {
        string let_body;
        for (const auto& binding : get<shared_ptr<SExpr>>(sexpr->args[0])->args) {
            const auto& binding_expr = get<shared_ptr<SExpr>>(binding);
            let_body += var_type_to_string(get_type(binding_expr->args[1])) + " " + get<string>(get<shared_ptr<SExpr>>(binding_expr->args[0])->args[0]) + " = " + generate_code(binding_expr->args[1]) + "; ";
        }
        let_body += generate_code(sexpr->args[1]);
        return "{ " + let_body + " }";
    } else {
        string func_call;
        for (const auto& arg : sexpr->args) {
            func_call += ", " + generate_code(arg);
        }
        return sexpr->type + "(" + func_call.substr(2) + ")";
    }
}

int main() {
    string input((istreambuf_iterator<char>(cin)), istreambuf_iterator<char>());
    istringstream iss(input);
    vector<Expr> program;
    parse_list(iss, program);

    for (const auto& expr : program) {
        cout << generate_code(expr) << endl;
    }

    return 0;
}

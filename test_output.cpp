#include <iostream>
#include <utility>
#include <vector>
#include <string>

int grade(int score) {
    return ((score >= 90) ? "A" : ((score >= 80) ? "B" : ((score >= 70) ? "C" : "F")));
}

int main() {
    std::cout << "grade" << std::endl;
    return 0;
}

#include <iostream>
#include <utility>
#include <vector>
#include <string>

int main() {
    int nested;
    int a;
    int b;
    nested = ((a = 5) + (b = 3));
    std::cout << nested << std::endl;
    return 0;
}

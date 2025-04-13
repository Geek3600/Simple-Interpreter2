#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include "Token.h"
#include "Lexer.h"
#include "Lox.h"



Lox::Lox() = default;
Lox::~Lox() = default;


void Lox::run(int argc, char *argv[]) {

    if (argc > 2) {
        std::cerr << "Usage: lox [script]\n";
        exit(0);
    }
    else if (argc == 2) {
        this->runFile(argv[1]); // 以文件形式运行代码
    }
    else {
        this->runPrompt(); // 以交互形式运行代码
    }

}


void Lox::runFile(const std::string &path) {
    std::ifstream file(path); // 创建文件流对象
    if (!file.is_open()) { // 打开文件
        std::cerr << "Error opening file: " << path << std::endl;
        exit(0);
    }

    std::stringstream buffer; // 创建字符串流对象
    buffer << file.rdbuf(); // 将文件流中的内容读入字符串流中
    this->interprete(buffer.str()); // 将字符串流中的内容作为字符串传入run函数中

    file.close(); // 关闭文件
    if (this->hasError)  {
        exit(0);
    }
}



void Lox::runPrompt() {
    std::string line = "";
    while (true) {
        std::cout << ">";
        if (!std::getline(std::cin, line)) {
            break;
        }
        if (line.empty()) continue;
        this->interprete(line);
        this->hasError = false;
    }
}



void Lox::interprete(const std::string &str) {
    std::cout << "Running Code: " << str << std::endl;
    Lexer lexer(str);
    std::vector<Token> tokens = lexer.scanTokens();
}


void Lox::error(int line, std::string &message) {
    this->report(line, "", message);
}


void Lox::report(int line, std::string where, std::string &errorMessage) {
    std::cerr << "[line " << line << "] Error" << where << ": " << errorMessage << std::endl;
    this->hasError = true;
}


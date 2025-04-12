#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <Token.h>

class Lox {
public: 
    bool hasError = false;
    Lox();
    ~Lox();

    void run(int argc, char *argv[]) {
        if (argc > 1) {
            std::cerr << "Usage: lox [script]\n";
            exit(0);
        }
        else if (argc == 1) {
            this->runFile(argv[1]); // 以文件形式运行代码
        }
        else {
            this->runPrompt(); // 以交互形式运行代码
        }
    }

private:
    void runFile(const std::string &path) {
        std::ifstream file(path); // 创建文件流对象
        if (!file.is_open()) { // 打开文件
            std:cerr << "Error opening file: " << path << std::endl;
            exit(74);
        }

        std::stringstream buffer; // 创建字符串流对象
        buffer << file.rdbuf(); // 将文件流中的内容读入字符串流中
        this->run(buffer.str()); // 将字符串流中的内容作为字符串传入run函数中

        file.close(); // 关闭文件
        if (this->hasError)  {
            exit(65);
        }
    }
    void runPrompt() {
        std::string line;
        while (true) {
            std::cout << ">";
            if (!std::getline(std::cin, line)) {
                break;
            }
            if (line.empty()) continue;
            this->interprete(line);
        }
    }

    void interprete(const std::string &str) {
        std::cout << "Running Code: " << str << std::endl;
        Lexer lexer(str);
        std::vector<Token> tokens = lexer.lexicalAnalysis();
    }

    void error(int line, std::string &message) {
        this->report(line, "", message);
    }

    void report(int line, std::string where, std::string &errorMessage) {
        std::cerr << "[line " << line << "] Error" << where << ": " << errorMessage << std::endl;
        this->hasError = true;
    }

};

int main(int argc, char *argv[])
{
    Lox lox;
    lox.run(argc, argv);
    return 0;
}
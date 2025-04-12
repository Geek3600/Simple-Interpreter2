#include "Token.h"
#include <iostream>
#include <any>

class Token {
public:
    TokenType type; // 存储token类型
    std::string lexeme;  // 存储原始文本
    std::string literal; // 存储数字、字符串等字面量
    int line; // 存储行号
    Token(TokenType type, const std::string &lexeme, const std::string &literal, int line)
        : type(type), lexeme(lexeme), literal(literal), line(line) {}

    friend std::ostream &operator<<(std::ostream &os, const Token &token) {
        os << "Token(" << token.type << ", " << token.lexeme << ", " << token.literal << ", " << token.line << ")";
        return os;
    }
}
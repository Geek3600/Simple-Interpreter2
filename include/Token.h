
#ifndef TOKEN_H
#define TOKEN_H
#include <string>
#include "TokenType.h"

class Token {
public:
    TokenType type = TokenType::UNKOWN;      // 存储token类型
    std::string lexeme = "";  // 存储原始文本
    std::string literal = ""; // 存储数字、字符串等字面量
    int lineNum = 0;            // 存储行号

    Token(TokenType tokenType, const std::string &lexeme, const std::string &literal, int lineNum) ;
    ~Token();
    // friend std::ostream &operator<<(std::ostream &os, const Token &token);
};

#endif
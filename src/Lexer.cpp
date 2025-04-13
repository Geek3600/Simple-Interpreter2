#include <iostream>
#include <string>
#include <vector>
#include "Token.h"
#include "Lexer.h"


Lexer::Lexer(const std::string &str): text(str) {}
Lexer::~Lexer() = default;

// 主函数，扫描所有token
std::vector<Token> Lexer::scanTokens() {
    while (!isAtEnd()) {
        start = current;
        scanToken();
    }
    // tokens.push_back(Token(TokenType::EOFC, "", "", text));
    return tokens;
}

void Lexer::scanToken() {
    
}


// 是否扫描到文本末尾
bool Lexer::isAtEnd() {
    return current >= static_cast<int>(text.size());
}

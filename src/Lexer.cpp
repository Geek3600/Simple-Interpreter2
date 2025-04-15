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
    tokens.push_back(Token(TokenType::EOFC, "", "", current));
    return tokens;
}

char Lexer::getNextChar() {
    this->current ++;
    return this->text[current - 1];  // 返回当前扫描到的字符    
}
void Lexer::addToken(TokenType type) {
    this->addToken(type, ""); // 添加一个为非数值型token，没有字面量
}

void Lexer::addToken(TokenType type, std::string literal) {
    std::string word = this->text.substr(this->start, this->current - this->start);
    this->tokens.push_back(Token(type, word, literal, 1));
}

void Lexer::scanToken() {
    char c = getNextChar();
    switch (c) {
        case '(': addToken(TokenType::LEFT_PAREN);  break;
        case ')': addToken(TokenType::RIGHT_PAREN); break;
        case '{': addToken(TokenType::LEFT_BRACE);  break;
        case '}': addToken(TokenType::RIGHT_BRACE); break;
        case ',': addToken(TokenType::DOT        ); break;
        case '.': addToken(TokenType::RIGHT_PAREN); break;
        case '-': addToken(TokenType::RIGHT_PAREN); break;
        case '+': addToken(TokenType::RIGHT_PAREN); break;
        case ';': addToken(TokenType::RIGHT_PAREN); break;
        case '*': addToken(TokenType::RIGHT_PAREN); break;
    }
}


// 是否扫描到文本末尾
bool Lexer::isAtEnd() {
    return current >= static_cast<int>(text.size());
}

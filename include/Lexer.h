#ifndef LEXER_H
#define LEXER_H
#include <vector>
#include <string>
#include "Token.h"

class Lexer {
public: 
    std::string text; // 接收的文本
    std::vector<Token> tokens; // tokens序列
    int start = 0;    // 一个token的第一个字符的偏移量
    int current = 0;  // 当前的字符的偏移量 
    int lineNum = 1;
    Lexer(const std::string &str);
    ~Lexer();

    std::vector<Token> scanTokens();
    void scanToken();
    bool isAtEnd();
    void addToken(TokenType type);
    void addToken(TokenType type, std::string literal);
    char getNextChar();

};


#endif
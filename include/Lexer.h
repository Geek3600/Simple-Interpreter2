#ifndef LEXER_H
#define LEXER_H
#include <vector>
#include <string>
#include "Token.h"

class Lexer {
public: 
    std::string text;
    std::vector<Token> tokens;
    int start = 0;
    int current = 0;
    Lexer(const std::string &str);
    ~Lexer();

    std::vector<Token> scanTokens();
    void scanToken();
    bool isAtEnd();
};


#endif
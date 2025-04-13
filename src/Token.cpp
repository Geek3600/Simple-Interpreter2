#include <iostream>
#include "Token.h"

Token::Token(TokenType tokenType, const std::string &lexeme, const std::string &literal, int lineNum) :
    type(tokenType), lexeme(lexeme), literal(literal), lineNum(lineNum) {}
Token::~Token() = default;


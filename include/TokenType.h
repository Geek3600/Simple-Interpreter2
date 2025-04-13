
#ifndef TOKEN_TYPE_H
#define TOKEN_TYPE_H

#include <unordered_map>

// token类型枚举
enum class TokenType {
    // Single-character tokens.
  UNKOWN,
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH, 
  STAR,
   // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
   // Literals.
  IDENTIFIER,
  STRING,
  NUMBER,
  // Keywords.
  AND,
  CLASS,
  ELSE,
  FALSE,
  FUN,
  FOR,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,
  EOFC,
};

// token类型对应的字符串
// inline关键字告诉编译器只保存一份实例，防止出现重复定义
inline std::unordered_map<TokenType, std::string> tokenName = {
    {TokenType::UNKOWN,       "UNKOWN"},
    {TokenType::LEFT_PAREN,   "LEFT_PAREN"},
    {TokenType::RIGHT_PAREN,  "RIGHT_PAREN"},
    {TokenType::LEFT_BRACE,   "LEFT_BRACE"},
    {TokenType::RIGHT_BRACE,  "RIGHT_BRACE"},
    {TokenType::COMMA,        "COMMA"},
    {TokenType::DOT,          "DOT"},
    {TokenType::MINUS,        "MINUS"},
    {TokenType::PLUS,         "PLUS"},
    {TokenType::SEMICOLON,    "SEMICOLON"},
    {TokenType::SLASH,        "SLASH"},
    {TokenType::STAR,         "STAR"},
    {TokenType::BANG,         "BANG"},
    {TokenType::BANG_EQUAL,   "BANG_EQUAL"},
    {TokenType::EQUAL,        "EQUAL"},
    {TokenType::EQUAL_EQUAL,  "EQUAL_EQUAL"},
    {TokenType::GREATER,      "GREATER"},
    {TokenType::GREATER_EQUAL,"GREATER_EQUAL"},
    {TokenType::LESS,         "LESS"},
    {TokenType::LESS_EQUAL,   "LESS_EQUAL"},
    {TokenType::IDENTIFIER,   "IDENTIFIER"},
    {TokenType::STRING,       "STRING"},
    {TokenType::NUMBER,       "NUMBER"},
    {TokenType::AND,          "AND"},
    {TokenType::CLASS,        "CLASS"},
    {TokenType::ELSE,         "ELSE"},
    {TokenType::FALSE,        "FALSE"},
    {TokenType::FUN,          "FUN"},
    {TokenType::FOR,          "FOR"},
    {TokenType::IF,           "IF"},
    {TokenType::NIL,          "NIL"},
    {TokenType::OR,           "OR"},
    {TokenType::PRINT,        "PRINT"},
    {TokenType::RETURN,       "RETURN"},
    {TokenType::SUPER,        "SUPER"},
    {TokenType::THIS,         "THIS"},
    {TokenType::TRUE,         "TRUE"},
    {TokenType::VAR,          "VAR"},
    {TokenType::WHILE,        "WHILE"},
    {TokenType::EOFC,         "EOFC"},
};
#endif


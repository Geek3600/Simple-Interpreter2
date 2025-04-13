#include <iostream>
#include <string>
#include "Lexer.h"
#include "Token.h"
#include "TokenType.h"
#include "Lox.h"

int main(int argc, char *argv[]) {
    Lox lox;
    lox.run(argc, argv);
    return 0;
}
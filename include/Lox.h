#ifndef LOX_H
#define LOX_H

#include <string>

class Lox {
public: 
    bool hasError = false;

    Lox();
    
    ~Lox();

    void run(int argc, char *argv[]);

private:
    void runFile(const std::string &path);

    void runPrompt();

    void interprete(const std::string &str);

    void error(int line, std::string &message);

    void report(int line, std::string where, std::string &errorMessage);
};

#endif
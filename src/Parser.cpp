#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <memory>
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "KaleidoscopeJIT.h"
using namespace llvm;
using namespace llvm::orc;
//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
enum Token {
  TOK_EOF = -1,
  TOK_DEF = -2,
  TOK_EXTERN = -3,
  TOK_IDENTIFIER = -4,
  TOK_NUMBER = -5,
  TOK_IF = -6,
  TOK_THEN = -7,
  TOK_ELSE = -8,
  TOK_FOR = -9,
  TOK_IN = -10,
  TOK_BINARY = -11,
  TOK_UNARY = -12
};

inline std::string identifierStr; // 如果当前的token是标识符类型，则将标识符保存到这个变量中
inline double numVal; // 如果当前的token是数字字面量，则保存在这

// 从输入的字符中获取token，其实只是获取token的种类
// 如果是标识符，就把标识符更新到全局变量identifierStr中，再判断标识符是不是def或者extern，则返回TOK_EXTERN/TOK_DEF
// 如果不是则返回TOK_IDENTIFIER，表示识别到了一个一般的标识符
// 如果是数字，则把字符字面量转成数字字面量，存到numVal中，同时返回TOK_NUMBER，表示识别到了一个数字
// 如果是其他的，比如运算符，由于其只是一个单字符，可以用ASCII码表示，所以直接返回其自身
int getToken() {
    static int lastChar = ' ';
    
    // printf("-----1");
    // 如果是空格就跳过
    while (isspace(lastChar)) lastChar = getchar();
    // printf("-----2");

    // 识别到标识符
    if (isalpha(lastChar)) {
        identifierStr = lastChar;
        while (isalnum((lastChar = getchar()))) {
            identifierStr += lastChar;
        }

        // 识别两种特殊的标识符
        if (identifierStr == "def") return TOK_DEF;
        if (identifierStr == "extern") return TOK_EXTERN;
        if (identifierStr == "if") return TOK_IF;
        if (identifierStr == "then") return TOK_THEN;
        if (identifierStr == "else") return TOK_ELSE;
        if (identifierStr == "for") return TOK_FOR;
        if (identifierStr == "in") return TOK_IN;
        if (identifierStr == "binary") return TOK_BINARY;
        if (identifierStr == "unary") return TOK_UNARY;
        return TOK_IDENTIFIER;
    }

    // 识别到数字
    //TODO add error check
    if (isdigit(lastChar) || lastChar == '.') {
        std::string numStr;
        do {
            numStr += lastChar; //把刚才识别到的数字字符拼接起来
            lastChar = getchar(); // 获取下一个字符
        } while (isdigit(lastChar) || lastChar == '.'); //判断当前获取的新字符是不是数字
        numVal = strtod(numStr.c_str(), nullptr); // 将字符串转成浮点数
        return TOK_NUMBER;
    }

    // 识别到注释
    if (lastChar == '#') {
        do lastChar = getchar();
        while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

        if (lastChar != EOF) return getToken(); 
    }

    if (lastChar == EOF) return TOK_EOF;

    // 如果不是以上的所有字符，就是+ - 那些，直接用他们本身作为字面量
    int thisChar = lastChar;
    lastChar = getchar();
    return thisChar;
}






//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
// AST基类节点
class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual Value *codeGen() = 0;
};


// 数值AST节点
class NumberExprAST: public ExprAST {
    double val;
public:
    NumberExprAST(double val): val(val) {}
    Value *codeGen() override;
};

// 变量节点
class VariableExprAST: public ExprAST {
    std::string name;
public:
    VariableExprAST(const std::string &name): name(name) {}
    Value *codeGen() override;
};

// 二元运算符节点
class BinaryOperatorExprAST: public ExprAST {
    char op;
    std::unique_ptr<ExprAST> LHS;
    std::unique_ptr<ExprAST> RHS;
public:
    BinaryOperatorExprAST(char op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS):
        op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}    
    Value *codeGen() override;
};


// 过程调用节点
class CallExprAST: public ExprAST {
    std::string callee;
    std::vector<std::unique_ptr<ExprAST>> args;
public:
    CallExprAST(const std::string &callee, std::vector<std::unique_ptr<ExprAST>> args): callee(callee), args(std::move(args)) {}
    Value *codeGen() override;
};

class ProtoTypeAST {
    std::string name;
    std::vector<std::string> args;
    bool isOperator;
    unsigned precedence;
public:
    ProtoTypeAST(const std::string &name, std::vector<std::string> args, bool isOperator = false, unsigned prec = 0): name(name), args(std::move(args)),
     isOperator(isOperator), precedence(prec) {} 
    Function *codeGen();
    const std::string &getName() const {return this->name;}

    bool isUnaryOp() const {return isOperator && args.size() == 1;}
    bool isBinaryOp() const {return isOperator && args.size() == 2;}

    char getOperatorName() const {
        assert(isUnaryOp() || isBinaryOp());
        return name[name.size() - 1];
    }

    unsigned getBinaryPrecedence() const {return precedence;}
};


class FunctionAST {
    std::unique_ptr<ProtoTypeAST> proto;
    std::unique_ptr<ExprAST> body;

public:
    FunctionAST(std::unique_ptr<ProtoTypeAST> proto, std::unique_ptr<ExprAST> body): 
        proto(std::move(proto)), body(std::move(body)) {}
    Function *codeGen();
};

class IfExprAST: public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, std::unique_ptr<ExprAST> Else): Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}  
    Value* codeGen() override;
};

class ForExprAST: public ExprAST {
    std::string VarName;
    std::unique_ptr<ExprAST> Start, End, Step, Body;
public:
    ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start, std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step
    , std::unique_ptr<ExprAST> Body)  : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
      Step(std::move(Step)), Body(std::move(Body)) {};

    Value* codeGen() override;  
};

class UnaryExprAST: public ExprAST {
    char Opcode;
    std::unique_ptr<ExprAST> Operand;
public:
    UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand):Opcode(Opcode), Operand(std::move(Operand)) {}
    Value* codeGen() override;
};

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//
std::unique_ptr<ExprAST> parsePrimary();
std::unique_ptr<ExprAST> parseBinaryOpRHS(int minimalPrec, std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ProtoTypeAST> parseProtoType();
std::unique_ptr<FunctionAST> parseDefinition();
std::unique_ptr<ProtoTypeAST> parseExtern();
std::unique_ptr<FunctionAST> parseTopLevelExpr();
std::unique_ptr<ExprAST> parseExpression();
std::unique_ptr<ExprAST> logError(const char *str);
std::unique_ptr<ProtoTypeAST> logErrorP(const char *str);
std::unique_ptr<ExprAST> parseNumberExpr();
std::unique_ptr<ExprAST> parseParenExpr();
std::unique_ptr<ExprAST> parseIdentifierExpr();
std::unique_ptr<ExprAST> parsePrimary();
std::unique_ptr<ExprAST> parseIfExpr();
std::unique_ptr<ExprAST> parseForExpr();
std::unique_ptr<ExprAST> parseUnary();

int currentToken;
int getNextToken() {
    return currentToken = getToken();
}


/// expression
///   ::= primary binop pieces
std::unique_ptr<ExprAST> parseExpression() {
    auto LHS = parseUnary();
    if (!LHS) return nullptr;
    return parseBinaryOpRHS(0, std::move(LHS));
}

std::unique_ptr<ExprAST> logError(const char *str) {
    fprintf(stderr, "Error: %s\n", str);
    return nullptr;
}

std::unique_ptr<ProtoTypeAST> logErrorP(const char *str) {
    logError(str);
    return nullptr;
}

// number
std::unique_ptr<ExprAST> parseNumberExpr() {
    auto result = std::make_unique<NumberExprAST>(numVal);
    getNextToken(); //eat number
    return std::move(result);
}

// (expression)
std::unique_ptr<ExprAST> parseParenExpr() {
    getNextToken(); // eat ( 
    auto v = parseExpression();
    if (!v) return nullptr;
    if (currentToken != ')') return logError("expected ')'");
    getNextToken(); // eat )
    return v;
}


// identifier | identifier(expression*)
std:: unique_ptr<ExprAST> parseIdentifierExpr() {
    std::string idName = identifierStr;
    getNextToken(); // eat identifier

    // 如果没有括号，说明是一个变量，而不是一个过程调用
    if (currentToken != '(') return std::make_unique<VariableExprAST>(idName);
    getNextToken(); //eat (

    std::vector<std::unique_ptr<ExprAST>> args;
    if (currentToken != ')') {
        while (true) {
            if (auto arg = parseExpression())
                args.push_back(std::move(arg));
            else 
                return nullptr;
            if (currentToken == ')') break;
            if (currentToken !=  ',') return logError("expected ')' or ',', in argument list");
            getNextToken(); // eat ','
        }

    }
    getNextToken(); //eat ')'
    return std::make_unique<CallExprAST>(idName, std::move(args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr 
std::unique_ptr<ExprAST> parsePrimary() {
    switch (currentToken) {
        default: 
            return logError("unknown token when expecting an expression");
        case TOK_IDENTIFIER:
            return parseIdentifierExpr();
        case TOK_NUMBER:
            return parseNumberExpr();
        case '(':
            return parseParenExpr();
        case TOK_IF:
            return parseIfExpr();
        case TOK_FOR:
            return parseForExpr();
    }
}


std::map<char, int> BinaryOpPrecedence;

// 查询当前运算符的优先级
int getTokenPrecedence() {
    if (!isascii(currentToken)) return -1;

    // 确保它是一个已经被声明的二元运算符
    int tokenPrec = BinaryOpPrecedence[currentToken];
    if (tokenPrec <= 0) return -1;
    return tokenPrec;
}



// minimalPrec是允许处理的最小的优先级，如果传入的二元运算符的优先级小于minimalPrec，则无法处理，直接返回LHS
std::unique_ptr<ExprAST> parseBinaryOpRHS(int minimalPrec, std::unique_ptr<ExprAST> LHS) {
    
    // auto RHS = parseUnary();
    // if (!RHS) return nullptr;
    
    
    while (true) {
        int tokPrec = getTokenPrecedence();
        if (tokPrec < minimalPrec) return LHS;
        int binOperator = currentToken;
        getNextToken(); // eat binop
        auto RHS = parseUnary();
        if (!RHS) return nullptr;

        int nextPrec = getTokenPrecedence();
        if (tokPrec < nextPrec) {
            RHS = parseBinaryOpRHS(tokPrec + 1, std::move(RHS));
            if (!RHS) return nullptr;
        }
        LHS = std::make_unique<BinaryOperatorExprAST>(binOperator, std::move(LHS), std::move(RHS));
    }

}

/// prototype
///   ::= id '(' id* ')'
///   ::= binary LETTER number? (id, id)
std::unique_ptr<ProtoTypeAST> parseProtoType() {
    std::string FnName ;
    unsigned Kind = 0;
    unsigned BinaryPrecedence = 30;
    switch (currentToken) {
        default:
            return logErrorP("Expected function name in prototype");
        case TOK_IDENTIFIER:
            FnName = identifierStr;
            Kind = 0;
            getNextToken();
            break;
        case TOK_UNARY:
            getNextToken();
            if (!isascii(currentToken)) return logErrorP("Expected unary operator");
            FnName = "unary";
            FnName += (char)currentToken;
            Kind = 1;
            getNextToken();
            break;
        case TOK_BINARY:
            getNextToken();
            if (!isascii(currentToken))
                return logErrorP("Expected binary operator");
            FnName = "binary";
            FnName += (char) currentToken;
            Kind = 2;
            getNextToken();

            if (currentToken == TOK_NUMBER) {
                if (numVal < 1 || numVal > 100) return logErrorP("Invalid precedence: must be 1..100");
                BinaryPrecedence = (unsigned) numVal;
                getNextToken();
            }
            break;
    }

    // if (currentToken != TOK_IDENTIFIER) return logErrorP("expected function name in prototype");
    // std::string funcName = identifierStr;
    // getNextToken(); // eat function name

    if (currentToken != '(') return logErrorP("expect '(' in prototype");

    std::vector<std::string> argsNames;
    while (getNextToken() == TOK_IDENTIFIER) 
        argsNames.push_back(identifierStr);
    if (currentToken != ')') 
        return logErrorP("expected ')' in prototype");

    getNextToken(); // eat ')'
    if (Kind && argsNames.size() != Kind) return logErrorP("Invalid number of operands for operator");
    return std::make_unique<ProtoTypeAST>(FnName, argsNames, Kind != 0, BinaryPrecedence);
}

std::unique_ptr<FunctionAST> parseDefinition() {
    getNextToken(); // eat def
    auto proto = parseProtoType(); // 解析函数声明
    if (!proto) return nullptr; 
    if (auto expr = parseExpression())
        return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
    return nullptr;
}

std::unique_ptr<ProtoTypeAST> parseExtern() {
    getNextToken(); // eat extern
    return parseProtoType();
}

std::unique_ptr<FunctionAST> parseTopLevelExpr() {
    if (auto expr = parseExpression()) {
        auto proto = std::make_unique<ProtoTypeAST>("__anon_expr", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
    }
    return nullptr;
}

std::unique_ptr<ExprAST> parseIfExpr() {
    getNextToken(); // eat if

    auto Cond = parseExpression(); // 解析if后的条件表达式
    if (!Cond) return nullptr;

    if (currentToken != TOK_THEN) return logError("expected then");
    getNextToken(); // eat then

    auto Then = parseExpression(); // 解析then后的语句块
    if (!Then) return nullptr;

    if (currentToken != TOK_ELSE) return logError("expected else");
    getNextToken(); // eat else
    auto Else = parseExpression(); // 解析else后的语句块
    if (!Else) return nullptr;

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else)); //构造ifelse的语法树节点
}


std::unique_ptr<ExprAST> parseForExpr() {
    getNextToken(); // eat for
    if (currentToken != TOK_IDENTIFIER) return logError("expected identifier after for");
    std::string IdName = identifierStr; // 保存函数名
    getNextToken(); // eat identifier
    if (currentToken != '=') return logError("expected '=' after for");
    getNextToken(); // eat =
    auto Start = parseExpression();
    if (!Start) return nullptr;
    
    if (currentToken != ',') return logError("expected ',' after for start value");
    getNextToken(); // eat ,

    auto End = parseExpression();
    if (!End) return nullptr;

    std::unique_ptr<ExprAST> Step;
    if (currentToken == ',') {
        getNextToken();
        Step = parseExpression();
        if (!Step) return nullptr;
    }

    if (currentToken != TOK_IN) return logError("expected 'in' after for");
    getNextToken(); // eat in 

    auto Body = parseExpression();
    if (!Body) return nullptr;
    return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End), std::move(Step), std::move(Body));
}

std::unique_ptr<ExprAST> parseUnary() {
    if (!isascii(currentToken) || currentToken == '(' || currentToken == ',') return parsePrimary();
    int Opc = currentToken;
    getNextToken(); // eat unary op
    if (auto Operand = parseUnary()) return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

std::unique_ptr<LLVMContext> TheContext; // llvm的核心，包含大量数据结构和函数
std::unique_ptr<IRBuilder<>> Builder; // 构建IR, 用于生成LLVM IR
std::unique_ptr<Module> TheModule; // llvmIR顶层结构，包含所有生成的IR
std::map<std::string, Value *> NamedValues; // 类似符号表
std::unique_ptr<KaleidoscopeJIT> TheJIT;
std::unique_ptr<FunctionPassManager> TheFPM;
std::unique_ptr<LoopAnalysisManager> TheLAM;
std::unique_ptr<FunctionAnalysisManager> TheFAM;
std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
std::unique_ptr<ModuleAnalysisManager> TheMAM;
std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
std::unique_ptr<StandardInstrumentations> TheSI;
std::map<std::string, std::unique_ptr<ProtoTypeAST>> FunctionProtos;
ExitOnError ExitOnErr;
Function *getFunction(std::string name) {

    // 首先我们查找函数是否已经存在于当前的模块中
    if (auto *F = TheModule->getFunction(name)) return F;

    // 如果没有，查看我们是否可以从现存的原型中，重新生成这个函数的声明
    auto FI = FunctionProtos.find(name);
    if (FI != FunctionProtos.end()) return FI->second->codeGen();

    // 如果现存的原型不存在，只能返回null
    return nullptr;
}
Value *logErrorV(const char *str) {
    logError(str);
    return nullptr;
}
Value *NumberExprAST::codeGen() {
    return ConstantFP::get(*TheContext, APFloat(val));
}

Value *VariableExprAST::codeGen() {
    Value* V = NamedValues[name];
    if (!V) return logErrorV("unknown variable name");
    return V;
}
Value *BinaryOperatorExprAST::codeGen() {
    Value *L = LHS->codeGen();
    Value *R = RHS->codeGen();
    if (!L || !R) return nullptr;

    switch (op) {
        case '+': return Builder->CreateFAdd(L, R, "addtmp");
        case '-': return Builder->CreateFSub(L, R, "subtmp");
        case '*': return Builder->CreateFMul(L, R, "multmp");
        case '<':
            L = Builder->CreateFCmpULT(L, R, "cmptmp");
            return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
        default:
            break;
    }

    // 如果它不是一个内建的二元运算符，那么一定是用户自定义的运算符
    Function *F = getFunction(std::string("binary") + op);
    assert(F && "bianry operator not found!");

    Value *Ops[] = {L, R};
    return Builder->CreateCall(F, Ops, "binop");
} 

Value *CallExprAST::codeGen() {
    
    // 在全局模块表中查找名字
    Function *CalleeF = getFunction(callee);
    if (!CalleeF) 
        return logErrorV("unknow function referenced");

    if (CalleeF->arg_size() != args.size()) 
        return logErrorV("incorrect # arguments passed");

    std::vector<Value *> argsV;
    for (unsigned i = 0, e = args.size(); i != e; ++i) {
        argsV.push_back(args[i]->codeGen());
        if (!argsV.back()) 
            return nullptr;
    }
    return Builder->CreateCall(CalleeF, argsV, "calltmp");
}


Function * ProtoTypeAST::codeGen() {
    std::vector<Type*> Doubles(args.size(), Type::getDoubleTy(*TheContext));
    FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
    
    Function *F = Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());
    
    unsigned idx = 0;
    for (auto &arg : F->args())
        arg.setName(args[idx++]);
    return F;
}


Function *FunctionAST::codeGen() {

    auto &P = *proto;
    FunctionProtos[proto->getName()] = std::move(proto);
    Function *TheFunction = getFunction(P.getName());
    if (!TheFunction) 
        return nullptr;

    if (P.isBinaryOp()) 
        BinaryOpPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    // 创造一个基本块去开始插入
    BasicBlock * BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);
    
    // 将函数参数记录在NamedValues中
    NamedValues.clear();
    for (auto &arg : TheFunction->args())
        NamedValues[std::string(arg.getName())] = &arg;
    
    if (Value *RetVal = body->codeGen()) {
        // 函数完成
        Builder->CreateRet(RetVal);
        // 验证生成的代码，检查其一致性
        verifyFunction(*TheFunction);

        // 运行优化
        TheFPM->run(*TheFunction, *TheFAM);
        return TheFunction;
    }

    TheFunction->eraseFromParent();

    if (P.isBinaryOp()) 
        BinaryOpPrecedence.erase(P.getOperatorName());
    return nullptr;
}

Value* IfExprAST::codeGen() {
    Value * CondV = Cond->codeGen();
    if (!CondV) 
        return nullptr;

    CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Emit then value
    Builder->SetInsertPoint(ThenBB);
    Value *ThenV = Then->codeGen();
    if (!ThenV) return nullptr;

    Builder->CreateBr(MergeBB);
    
    // codegen of then can change the current block update thenbb for the phi
    ThenBB = Builder->GetInsertBlock();

    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);

    Value *ElseV = Else->codeGen();
    if (!ElseV) 
        return nullptr;

    Builder->CreateBr(MergeBB);
    ElseBB = Builder->GetInsertBlock();

    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);

    return PN;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
Value *ForExprAST::codeGen() {
    Value *StartVal = Start->codeGen();

    if (!StartVal) 
        return nullptr;
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder->GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    Builder->CreateBr(LoopBB);

    Builder->SetInsertPoint(LoopBB);
    PHINode *Variable = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
    Variable->addIncoming(StartVal, PreheaderBB);

    Value* OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;

    if (!Body->codeGen()) 
        return nullptr;

    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codeGen();
        if (!StepVal) 
            return nullptr;
    }
    else {
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    Value* NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");
    Value *EndCond = End->codeGen();
    if (!EndCond) 
        return nullptr;

    EndCond = Builder->CreateFCmpONE(EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

    BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);
    Builder->SetInsertPoint(AfterBB);

    Variable->addIncoming(NextVar, LoopEndBB);
    if (OldVal) 
        NamedValues[VarName] = OldVal;
    else 
        NamedValues.erase(VarName);

    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Value* UnaryExprAST::codeGen() {
    Value *OperandV = Operand->codeGen();
    if (!OperandV) 
        return nullptr;
    
    Function *F = getFunction(std::string("unary") + Opcode);
    if (!F) 
        return logErrorV("Unknown unary operator");

    return Builder->CreateCall(F, OperandV, "unop");
}

void InitializeModuleAndPassManager() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    TheFPM = std::make_unique<FunctionPassManager>();
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                    /*DebugLogging*/ true);
    TheSI->registerCallbacks(*ThePIC, TheMAM.get());


    TheFPM->addPass(InstCombinePass());
    TheFPM->addPass(ReassociatePass());
    TheFPM->addPass(GVNPass());
    TheFPM->addPass(SimplifyCFGPass());

    // Register analysis passes used in these transform passes.
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}
void handleDefinition() {
    if (auto fnAST = parseDefinition()) {
        if (auto *fnIR = fnAST->codeGen()) {
            fprintf(stderr, "\nRead function definition:\n");
            fnIR->print(errs());
            fprintf(stderr, "\n");
            ExitOnErr(TheJIT->addModule(ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndPassManager();
        }
    }
    else {
        getNextToken();
    }
}


void handleExtern() {
    if (auto protoAST = parseExtern()) {
        if (auto *fnIR = protoAST->codeGen()) {
            fprintf(stderr, "Read extern: ");
            fnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[protoAST->getName()] = std::move(protoAST);
        }
    }
    else {
        getNextToken();
    }
}

void handleTopLevelExpression() {
    if (auto fnAST = parseTopLevelExpr()) {
        if (auto *fnIR = fnAST->codeGen()) {

            auto RT = TheJIT->getMainJITDylib().createResourceTracker();
            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndPassManager();
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
            // assert(ExprSymbol && "Function not found");

            double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
            fprintf(stderr, "Evaluated to %f\n", FP());

            // fprintf(stderr, "\nRead top-level expression:\n");
            // fnIR->print(errs());
            // fprintf(stderr, "\n");
            // fnIR->eraseFromParent();
            ExitOnErr(RT->remove());
        }
    }
    else {
        getNextToken();
    }
}
/// top ::= definition | external | expression | ';'
void mainLoop() {
    while (true) {
        fprintf(stderr, "ready> ");
        switch (currentToken) {
            case TOK_EOF:
                return;
            case ';':
                getNextToken();
                break;
            case TOK_DEF:
                handleDefinition();
                break;
            case TOK_EXTERN:
                handleExtern();
                break;
            default:
                handleTopLevelExpression(); // 一开始肯定是进来这
                break;
        }
    }
}


#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

int main() {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    BinaryOpPrecedence['<'] = 10;
    BinaryOpPrecedence['+'] = 20;
    BinaryOpPrecedence['-'] = 20;
    BinaryOpPrecedence['*'] = 40;
    // BinaryOpPrecedence['/'] = 40;

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    // 使用JIT动态加载和执行生成的LLVM IR
    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
    InitializeModuleAndPassManager();
    // Run the main "interpreter loop" now.
    mainLoop();
    return 0;
}


// #include "KaleidoscopeJIT.h"
// #include "llvm/ADT/APFloat.h"
// #include "llvm/ADT/STLExtras.h"
// #include "llvm/IR/BasicBlock.h"
// #include "llvm/IR/Constants.h"
// #include "llvm/IR/DerivedTypes.h"
// #include "llvm/IR/Function.h"
// #include "llvm/IR/IRBuilder.h"
// #include "llvm/IR/Instructions.h"
// #include "llvm/IR/LLVMContext.h"
// #include "llvm/IR/Module.h"
// #include "llvm/IR/PassManager.h"
// #include "llvm/IR/Type.h"
// #include "llvm/IR/Verifier.h"
// #include "llvm/Passes/PassBuilder.h"
// #include "llvm/Passes/StandardInstrumentations.h"
// #include "llvm/Support/TargetSelect.h"
// #include "llvm/Target/TargetMachine.h"
// #include "llvm/Transforms/InstCombine/InstCombine.h"
// #include "llvm/Transforms/Scalar.h"
// #include "llvm/Transforms/Scalar/GVN.h"
// #include "llvm/Transforms/Scalar/Reassociate.h"
// #include "llvm/Transforms/Scalar/SimplifyCFG.h"
// #include <algorithm>
// #include <cassert>
// #include <cctype>
// #include <cstdint>
// #include <cstdio>
// #include <cstdlib>
// #include <map>
// #include <memory>
// #include <string>
// #include <vector>

// using namespace llvm;
// using namespace llvm::orc;

// //===----------------------------------------------------------------------===//
// // Lexer
// //===----------------------------------------------------------------------===//

// // The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// // of these for known things.
// enum Token {
//   tok_eof = -1,

//   // commands
//   tok_def = -2,
//   tok_extern = -3,

//   // primary
//   tok_identifier = -4,
//   tok_number = -5,

//   // control
//   tok_if = -6,
//   tok_then = -7,
//   tok_else = -8,
//   tok_for = -9,
//   tok_in = -10,

//   // operators
//   tok_binary = -11,
//   tok_unary = -12
// };

// static std::string IdentifierStr; // Filled in if tok_identifier
// static double NumVal;             // Filled in if tok_number

// /// gettok - Return the next token from standard input.
// static int gettok() {
//   static int LastChar = ' ';

//   // Skip any whitespace.
//   while (isspace(LastChar))
//     LastChar = getchar();

//   if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
//     IdentifierStr = LastChar;
//     while (isalnum((LastChar = getchar())))
//       IdentifierStr += LastChar;

//     if (IdentifierStr == "def")
//       return tok_def;
//     if (IdentifierStr == "extern")
//       return tok_extern;
//     if (IdentifierStr == "if")
//       return tok_if;
//     if (IdentifierStr == "then")
//       return tok_then;
//     if (IdentifierStr == "else")
//       return tok_else;
//     if (IdentifierStr == "for")
//       return tok_for;
//     if (IdentifierStr == "in")
//       return tok_in;
//     if (IdentifierStr == "binary")
//       return tok_binary;
//     if (IdentifierStr == "unary")
//       return tok_unary;
//     return tok_identifier;
//   }

//   if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
//     std::string NumStr;
//     do {
//       NumStr += LastChar;
//       LastChar = getchar();
//     } while (isdigit(LastChar) || LastChar == '.');

//     NumVal = strtod(NumStr.c_str(), nullptr);
//     return tok_number;
//   }

//   if (LastChar == '#') {
//     // Comment until end of line.
//     do
//       LastChar = getchar();
//     while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

//     if (LastChar != EOF)
//       return gettok();
//   }

//   // Check for end of file.  Don't eat the EOF.
//   if (LastChar == EOF)
//     return tok_eof;

//   // Otherwise, just return the character as its ascii value.
//   int ThisChar = LastChar;
//   LastChar = getchar();
//   return ThisChar;
// }

// //===----------------------------------------------------------------------===//
// // Abstract Syntax Tree (aka Parse Tree)
// //===----------------------------------------------------------------------===//

// namespace {

// /// ExprAST - Base class for all expression nodes.
// class ExprAST {
// public:
//   virtual ~ExprAST() = default;

//   virtual Value *codegen() = 0;
// };

// /// NumberExprAST - Expression class for numeric literals like "1.0".
// class NumberExprAST : public ExprAST {
//   double Val;

// public:
//   NumberExprAST(double Val) : Val(Val) {}

//   Value *codegen() override;
// };

// /// VariableExprAST - Expression class for referencing a variable, like "a".
// class VariableExprAST : public ExprAST {
//   std::string Name;

// public:
//   VariableExprAST(const std::string &Name) : Name(Name) {}

//   Value *codegen() override;
// };

// /// UnaryExprAST - Expression class for a unary operator.
// class UnaryExprAST : public ExprAST {
//   char Opcode;
//   std::unique_ptr<ExprAST> Operand;

// public:
//   UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
//       : Opcode(Opcode), Operand(std::move(Operand)) {}

//   Value *codegen() override;
// };

// /// BinaryExprAST - Expression class for a binary operator.
// class BinaryExprAST : public ExprAST {
//   char Op;
//   std::unique_ptr<ExprAST> LHS, RHS;

// public:
//   BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
//                 std::unique_ptr<ExprAST> RHS)
//       : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

//   Value *codegen() override;
// };

// /// CallExprAST - Expression class for function calls.
// class CallExprAST : public ExprAST {
//   std::string Callee;
//   std::vector<std::unique_ptr<ExprAST>> Args;

// public:
//   CallExprAST(const std::string &Callee,
//               std::vector<std::unique_ptr<ExprAST>> Args)
//       : Callee(Callee), Args(std::move(Args)) {}

//   Value *codegen() override;
// };

// /// IfExprAST - Expression class for if/then/else.
// class IfExprAST : public ExprAST {
//   std::unique_ptr<ExprAST> Cond, Then, Else;

// public:
//   IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
//             std::unique_ptr<ExprAST> Else)
//       : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

//   Value *codegen() override;
// };

// /// ForExprAST - Expression class for for/in.
// class ForExprAST : public ExprAST {
//   std::string VarName;
//   std::unique_ptr<ExprAST> Start, End, Step, Body;

// public:
//   ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
//              std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
//              std::unique_ptr<ExprAST> Body)
//       : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
//         Step(std::move(Step)), Body(std::move(Body)) {}

//   Value *codegen() override;
// };

// /// PrototypeAST - This class represents the "prototype" for a function,
// /// which captures its name, and its argument names (thus implicitly the number
// /// of arguments the function takes), as well as if it is an operator.
// class PrototypeAST {
//   std::string Name;
//   std::vector<std::string> Args;
//   bool IsOperator;
//   unsigned Precedence; // Precedence if a binary op.

// public:
//   PrototypeAST(const std::string &Name, std::vector<std::string> Args,
//                bool IsOperator = false, unsigned Prec = 0)
//       : Name(Name), Args(std::move(Args)), IsOperator(IsOperator),
//         Precedence(Prec) {}

//   Function *codegen();
//   const std::string &getName() const { return Name; }

//   bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
//   bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

//   char getOperatorName() const {
//     assert(isUnaryOp() || isBinaryOp());
//     return Name[Name.size() - 1];
//   }

//   unsigned getBinaryPrecedence() const { return Precedence; }
// };

// /// FunctionAST - This class represents a function definition itself.
// class FunctionAST {
//   std::unique_ptr<PrototypeAST> Proto;
//   std::unique_ptr<ExprAST> Body;

// public:
//   FunctionAST(std::unique_ptr<PrototypeAST> Proto,
//               std::unique_ptr<ExprAST> Body)
//       : Proto(std::move(Proto)), Body(std::move(Body)) {}

//   Function *codegen();
// };

// } // end anonymous namespace

// //===----------------------------------------------------------------------===//
// // Parser
// //===----------------------------------------------------------------------===//

// /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
// /// token the parser is looking at.  getNextToken reads another token from the
// /// lexer and updates CurTok with its results.
// static int CurTok;
// static int getNextToken() { return CurTok = gettok(); }

// /// BinopPrecedence - This holds the precedence for each binary operator that is
// /// defined.
// static std::map<char, int> BinopPrecedence;

// /// GetTokPrecedence - Get the precedence of the pending binary operator token.
// static int GetTokPrecedence() {
//   if (!isascii(CurTok))
//     return -1;

//   // Make sure it's a declared binop.
//   int TokPrec = BinopPrecedence[CurTok];
//   if (TokPrec <= 0)
//     return -1;
//   return TokPrec;
// }

// /// Error* - These are little helper functions for error handling.
// std::unique_ptr<ExprAST> LogError(const char *Str) {
//   fprintf(stderr, "Error: %s\n", Str);
//   return nullptr;
// }

// std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
//   LogError(Str);
//   return nullptr;
// }

// static std::unique_ptr<ExprAST> ParseExpression();

// /// numberexpr ::= number
// static std::unique_ptr<ExprAST> ParseNumberExpr() {
//   auto Result = std::make_unique<NumberExprAST>(NumVal);
//   getNextToken(); // consume the number
//   return std::move(Result);
// }

// /// parenexpr ::= '(' expression ')'
// static std::unique_ptr<ExprAST> ParseParenExpr() {
//   getNextToken(); // eat (.
//   auto V = ParseExpression();
//   if (!V)
//     return nullptr;

//   if (CurTok != ')')
//     return LogError("expected ')'");
//   getNextToken(); // eat ).
//   return V;
// }

// /// identifierexpr
// ///   ::= identifier
// ///   ::= identifier '(' expression* ')'
// static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
//   std::string IdName = IdentifierStr;

//   getNextToken(); // eat identifier.

//   if (CurTok != '(') // Simple variable ref.
//     return std::make_unique<VariableExprAST>(IdName);

//   // Call.
//   getNextToken(); // eat (
//   std::vector<std::unique_ptr<ExprAST>> Args;
//   if (CurTok != ')') {
//     while (true) {
//       if (auto Arg = ParseExpression())
//         Args.push_back(std::move(Arg));
//       else
//         return nullptr;

//       if (CurTok == ')')
//         break;

//       if (CurTok != ',')
//         return LogError("Expected ')' or ',' in argument list");
//       getNextToken();
//     }
//   }

//   // Eat the ')'.
//   getNextToken();

//   return std::make_unique<CallExprAST>(IdName, std::move(Args));
// }

// /// ifexpr ::= 'if' expression 'then' expression 'else' expression
// static std::unique_ptr<ExprAST> ParseIfExpr() {
//   getNextToken(); // eat the if.

//   // condition.
//   auto Cond = ParseExpression();
//   if (!Cond)
//     return nullptr;

//   if (CurTok != tok_then)
//     return LogError("expected then");
//   getNextToken(); // eat the then

//   auto Then = ParseExpression();
//   if (!Then)
//     return nullptr;

//   if (CurTok != tok_else)
//     return LogError("expected else");

//   getNextToken();

//   auto Else = ParseExpression();
//   if (!Else)
//     return nullptr;

//   return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
//                                       std::move(Else));
// }

// /// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
// static std::unique_ptr<ExprAST> ParseForExpr() {
//   getNextToken(); // eat the for.

//   if (CurTok != tok_identifier)
//     return LogError("expected identifier after for");

//   std::string IdName = IdentifierStr;
//   getNextToken(); // eat identifier.

//   if (CurTok != '=')
//     return LogError("expected '=' after for");
//   getNextToken(); // eat '='.

//   auto Start = ParseExpression();
//   if (!Start)
//     return nullptr;
//   if (CurTok != ',')
//     return LogError("expected ',' after for start value");
//   getNextToken();

//   auto End = ParseExpression();
//   if (!End)
//     return nullptr;

//   // The step value is optional.
//   std::unique_ptr<ExprAST> Step;
//   if (CurTok == ',') {
//     getNextToken();
//     Step = ParseExpression();
//     if (!Step)
//       return nullptr;
//   }

//   if (CurTok != tok_in)
//     return LogError("expected 'in' after for");
//   getNextToken(); // eat 'in'.

//   auto Body = ParseExpression();
//   if (!Body)
//     return nullptr;

//   return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
//                                        std::move(Step), std::move(Body));
// }

// /// primary
// ///   ::= identifierexpr
// ///   ::= numberexpr
// ///   ::= parenexpr
// ///   ::= ifexpr
// ///   ::= forexpr
// static std::unique_ptr<ExprAST> ParsePrimary() {
//   switch (CurTok) {
//   default:
//     return LogError("unknown token when expecting an expression");
//   case tok_identifier:
//     return ParseIdentifierExpr();
//   case tok_number:
//     return ParseNumberExpr();
//   case '(':
//     return ParseParenExpr();
//   case tok_if:
//     return ParseIfExpr();
//   case tok_for:
//     return ParseForExpr();
//   }
// }

// /// unary
// ///   ::= primary
// ///   ::= '!' unary
// static std::unique_ptr<ExprAST> ParseUnary() {
//   // If the current token is not an operator, it must be a primary expr.
//   if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
//     return ParsePrimary();

//   // If this is a unary operator, read it.
//   int Opc = CurTok;
//   getNextToken();
//   if (auto Operand = ParseUnary())
//     return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
//   return nullptr;
// }

// /// binoprhs
// ///   ::= ('+' unary)*
// static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
//                                               std::unique_ptr<ExprAST> LHS) {
//   // If this is a binop, find its precedence.
//   while (true) {
//     int TokPrec = GetTokPrecedence();

//     // If this is a binop that binds at least as tightly as the current binop,
//     // consume it, otherwise we are done.
//     if (TokPrec < ExprPrec)
//       return LHS;

//     // Okay, we know this is a binop.
//     int BinOp = CurTok;
//     getNextToken(); // eat binop

//     // Parse the unary expression after the binary operator.
//     auto RHS = ParseUnary();
//     if (!RHS)
//       return nullptr;

//     // If BinOp binds less tightly with RHS than the operator after RHS, let
//     // the pending operator take RHS as its LHS.
//     int NextPrec = GetTokPrecedence();
//     if (TokPrec < NextPrec) {
//       RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
//       if (!RHS)
//         return nullptr;
//     }

//     // Merge LHS/RHS.
//     LHS =
//         std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
//   }
// }

// /// expression
// ///   ::= unary binoprhs
// ///
// static std::unique_ptr<ExprAST> ParseExpression() {
//   auto LHS = ParseUnary();
//   if (!LHS)
//     return nullptr;

//   return ParseBinOpRHS(0, std::move(LHS));
// }

// /// prototype
// ///   ::= id '(' id* ')'
// ///   ::= binary LETTER number? (id, id)
// ///   ::= unary LETTER (id)
// static std::unique_ptr<PrototypeAST> ParsePrototype() {
//   std::string FnName;

//   unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
//   unsigned BinaryPrecedence = 30;

//   switch (CurTok) {
//   default:
//     return LogErrorP("Expected function name in prototype");
//   case tok_identifier:
//     FnName = IdentifierStr;
//     Kind = 0;
//     getNextToken();
//     break;
//   case tok_unary:
//     getNextToken();
//     if (!isascii(CurTok))
//       return LogErrorP("Expected unary operator");
//     FnName = "unary";
//     FnName += (char)CurTok;
//     Kind = 1;
//     getNextToken();
//     break;
//   case tok_binary:
//     getNextToken();
//     if (!isascii(CurTok))
//       return LogErrorP("Expected binary operator");
//     FnName = "binary";
//     FnName += (char)CurTok;
//     Kind = 2;
//     getNextToken();

//     // Read the precedence if present.
//     if (CurTok == tok_number) {
//       if (NumVal < 1 || NumVal > 100)
//         return LogErrorP("Invalid precedence: must be 1..100");
//       BinaryPrecedence = (unsigned)NumVal;
//       getNextToken();
//     }
//     break;
//   }

//   if (CurTok != '(')
//     return LogErrorP("Expected '(' in prototype");

//   std::vector<std::string> ArgNames;
//   while (getNextToken() == tok_identifier)
//     ArgNames.push_back(IdentifierStr);
//   if (CurTok != ')')
//     return LogErrorP("Expected ')' in prototype");

//   // success.
//   getNextToken(); // eat ')'.

//   // Verify right number of names for operator.
//   if (Kind && ArgNames.size() != Kind)
//     return LogErrorP("Invalid number of operands for operator");

//   return std::make_unique<PrototypeAST>(FnName, ArgNames, Kind != 0,
//                                          BinaryPrecedence);
// }

// /// definition ::= 'def' prototype expression
// static std::unique_ptr<FunctionAST> ParseDefinition() {
//   getNextToken(); // eat def.
//   auto Proto = ParsePrototype();
//   if (!Proto)
//     return nullptr;

//   if (auto E = ParseExpression())
//     return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
//   return nullptr;
// }

// /// toplevelexpr ::= expression
// static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
//   if (auto E = ParseExpression()) {
//     // Make an anonymous proto.
//     auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
//                                                  std::vector<std::string>());
//     return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
//   }
//   return nullptr;
// }

// /// external ::= 'extern' prototype
// static std::unique_ptr<PrototypeAST> ParseExtern() {
//   getNextToken(); // eat extern.
//   return ParsePrototype();
// }

// //===----------------------------------------------------------------------===//
// // Code Generation
// //===----------------------------------------------------------------------===//

// static std::unique_ptr<LLVMContext> TheContext;
// static std::unique_ptr<Module> TheModule;
// static std::unique_ptr<IRBuilder<>> Builder;
// static std::map<std::string, Value *> NamedValues;
// static std::unique_ptr<KaleidoscopeJIT> TheJIT;
// static std::unique_ptr<FunctionPassManager> TheFPM;
// static std::unique_ptr<LoopAnalysisManager> TheLAM;
// static std::unique_ptr<FunctionAnalysisManager> TheFAM;
// static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
// static std::unique_ptr<ModuleAnalysisManager> TheMAM;
// static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
// static std::unique_ptr<StandardInstrumentations> TheSI;
// static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
// static ExitOnError ExitOnErr;

// Value *LogErrorV(const char *Str) {
//   LogError(Str);
//   return nullptr;
// }

// Function *getFunction(std::string Name) {
//   // First, see if the function has already been added to the current module.
//   if (auto *F = TheModule->getFunction(Name))
//     return F;

//   // If not, check whether we can codegen the declaration from some existing
//   // prototype.
//   auto FI = FunctionProtos.find(Name);
//   if (FI != FunctionProtos.end())
//     return FI->second->codegen();

//   // If no existing prototype exists, return null.
//   return nullptr;
// }

// Value *NumberExprAST::codegen() {
//   return ConstantFP::get(*TheContext, APFloat(Val));
// }

// Value *VariableExprAST::codegen() {
//   // Look this variable up in the function.
//   Value *V = NamedValues[Name];
//   if (!V)
//     return LogErrorV("Unknown variable name");
//   return V;
// }

// Value *UnaryExprAST::codegen() {
//   Value *OperandV = Operand->codegen();
//   if (!OperandV)
//     return nullptr;

//   Function *F = getFunction(std::string("unary") + Opcode);
//   if (!F)
//     return LogErrorV("Unknown unary operator");

//   return Builder->CreateCall(F, OperandV, "unop");
// }

// Value *BinaryExprAST::codegen() {
//   Value *L = LHS->codegen();
//   Value *R = RHS->codegen();
//   if (!L || !R)
//     return nullptr;

//   switch (Op) {
//   case '+':
//     return Builder->CreateFAdd(L, R, "addtmp");
//   case '-':
//     return Builder->CreateFSub(L, R, "subtmp");
//   case '*':
//     return Builder->CreateFMul(L, R, "multmp");
//   case '<':
//     L = Builder->CreateFCmpULT(L, R, "cmptmp");
//     // Convert bool 0/1 to double 0.0 or 1.0
//     return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
//   default:
//     break;
//   }

//   // If it wasn't a builtin binary operator, it must be a user defined one. Emit
//   // a call to it.
//   Function *F = getFunction(std::string("binary") + Op);
//   assert(F && "binary operator not found!");

//   Value *Ops[] = {L, R};
//   return Builder->CreateCall(F, Ops, "binop");
// }

// Value *CallExprAST::codegen() {
//   // Look up the name in the global module table.
//   Function *CalleeF = getFunction(Callee);
//   if (!CalleeF)
//     return LogErrorV("Unknown function referenced");

//   // If argument mismatch error.
//   if (CalleeF->arg_size() != Args.size())
//     return LogErrorV("Incorrect # arguments passed");

//   std::vector<Value *> ArgsV;
//   for (unsigned i = 0, e = Args.size(); i != e; ++i) {
//     ArgsV.push_back(Args[i]->codegen());
//     if (!ArgsV.back())
//       return nullptr;
//   }

//   return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
// }

// Value *IfExprAST::codegen() {
//   Value *CondV = Cond->codegen();
//   if (!CondV)
//     return nullptr;

//   // Convert condition to a bool by comparing non-equal to 0.0.
//   CondV = Builder->CreateFCmpONE(
//       CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

//   Function *TheFunction = Builder->GetInsertBlock()->getParent();

//   // Create blocks for the then and else cases.  Insert the 'then' block at the
//   // end of the function.
//   BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
//   BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
//   BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

//   Builder->CreateCondBr(CondV, ThenBB, ElseBB);

//   // Emit then value.
//   Builder->SetInsertPoint(ThenBB);

//   Value *ThenV = Then->codegen();
//   if (!ThenV)
//     return nullptr;

//   Builder->CreateBr(MergeBB);
//   // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
//   ThenBB = Builder->GetInsertBlock();

//   // Emit else block.
//   TheFunction->insert(TheFunction->end(), ElseBB);
//   Builder->SetInsertPoint(ElseBB);

//   Value *ElseV = Else->codegen();
//   if (!ElseV)
//     return nullptr;

//   Builder->CreateBr(MergeBB);
//   // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
//   ElseBB = Builder->GetInsertBlock();

//   // Emit merge block.
//   TheFunction->insert(TheFunction->end(), MergeBB);
//   Builder->SetInsertPoint(MergeBB);
//   PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

//   PN->addIncoming(ThenV, ThenBB);
//   PN->addIncoming(ElseV, ElseBB);
//   return PN;
// }

// // Output for-loop as:
// //   ...
// //   start = startexpr
// //   goto loop
// // loop:
// //   variable = phi [start, loopheader], [nextvariable, loopend]
// //   ...
// //   bodyexpr
// //   ...
// // loopend:
// //   step = stepexpr
// //   nextvariable = variable + step
// //   endcond = endexpr
// //   br endcond, loop, endloop
// // outloop:
// Value *ForExprAST::codegen() {
//   // Emit the start code first, without 'variable' in scope.
//   Value *StartVal = Start->codegen();
//   if (!StartVal)
//     return nullptr;

//   // Make the new basic block for the loop header, inserting after current
//   // block.
//   Function *TheFunction = Builder->GetInsertBlock()->getParent();
//   BasicBlock *PreheaderBB = Builder->GetInsertBlock();
//   BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

//   // Insert an explicit fall through from the current block to the LoopBB.
//   Builder->CreateBr(LoopBB);

//   // Start insertion in LoopBB.
//   Builder->SetInsertPoint(LoopBB);

//   // Start the PHI node with an entry for Start.
//   PHINode *Variable =
//       Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
//   Variable->addIncoming(StartVal, PreheaderBB);

//   // Within the loop, the variable is defined equal to the PHI node.  If it
//   // shadows an existing variable, we have to restore it, so save it now.
//   Value *OldVal = NamedValues[VarName];
//   NamedValues[VarName] = Variable;

//   // Emit the body of the loop.  This, like any other expr, can change the
//   // current BB.  Note that we ignore the value computed by the body, but don't
//   // allow an error.
//   if (!Body->codegen())
//     return nullptr;

//   // Emit the step value.
//   Value *StepVal = nullptr;
//   if (Step) {
//     StepVal = Step->codegen();
//     if (!StepVal)
//       return nullptr;
//   } else {
//     // If not specified, use 1.0.
//     StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
//   }

//   Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

//   // Compute the end condition.
//   Value *EndCond = End->codegen();
//   if (!EndCond)
//     return nullptr;

//   // Convert condition to a bool by comparing non-equal to 0.0.
//   EndCond = Builder->CreateFCmpONE(
//       EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

//   // Create the "after loop" block and insert it.
//   BasicBlock *LoopEndBB = Builder->GetInsertBlock();
//   BasicBlock *AfterBB =
//       BasicBlock::Create(*TheContext, "afterloop", TheFunction);

//   // Insert the conditional branch into the end of LoopEndBB.
//   Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

//   // Any new code will be inserted in AfterBB.
//   Builder->SetInsertPoint(AfterBB);

//   // Add a new entry to the PHI node for the backedge.
//   Variable->addIncoming(NextVar, LoopEndBB);

//   // Restore the unshadowed variable.
//   if (OldVal)
//     NamedValues[VarName] = OldVal;
//   else
//     NamedValues.erase(VarName);

//   // for expr always returns 0.0.
//   return Constant::getNullValue(Type::getDoubleTy(*TheContext));
// }

// Function *PrototypeAST::codegen() {
//   // Make the function type:  double(double,double) etc.
//   std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
//   FunctionType *FT =
//       FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

//   Function *F =
//       Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

//   // Set names for all arguments.
//   unsigned Idx = 0;
//   for (auto &Arg : F->args())
//     Arg.setName(Args[Idx++]);

//   return F;
// }

// Function *FunctionAST::codegen() {
//   // Transfer ownership of the prototype to the FunctionProtos map, but keep a
//   // reference to it for use below.
//   auto &P = *Proto;
//   FunctionProtos[Proto->getName()] = std::move(Proto);
//   Function *TheFunction = getFunction(P.getName());
//   if (!TheFunction)
//     return nullptr;

//   // If this is an operator, install it.
//   if (P.isBinaryOp())
//     BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

//   // Create a new basic block to start insertion into.
//   BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
//   Builder->SetInsertPoint(BB);

//   // Record the function arguments in the NamedValues map.
//   NamedValues.clear();
//   for (auto &Arg : TheFunction->args())
//     NamedValues[std::string(Arg.getName())] = &Arg;

//   if (Value *RetVal = Body->codegen()) {
//     // Finish off the function.
//     Builder->CreateRet(RetVal);

//     // Validate the generated code, checking for consistency.
//     verifyFunction(*TheFunction);

//     // Run the optimizer on the function.
//     TheFPM->run(*TheFunction, *TheFAM);

//     return TheFunction;
//   }

//   // Error reading body, remove function.
//   TheFunction->eraseFromParent();

//   if (P.isBinaryOp())
//     BinopPrecedence.erase(P.getOperatorName());
//   return nullptr;
// }

// //===----------------------------------------------------------------------===//
// // Top-Level parsing and JIT Driver
// //===----------------------------------------------------------------------===//

// static void InitializeModuleAndManagers() {
//   // Open a new context and module.
//   TheContext = std::make_unique<LLVMContext>();
//   TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
//   TheModule->setDataLayout(TheJIT->getDataLayout());

//   // Create a new builder for the module.
//   Builder = std::make_unique<IRBuilder<>>(*TheContext);

//   // Create new pass and analysis managers.
//   TheFPM = std::make_unique<FunctionPassManager>();
//   TheLAM = std::make_unique<LoopAnalysisManager>();
//   TheFAM = std::make_unique<FunctionAnalysisManager>();
//   TheCGAM = std::make_unique<CGSCCAnalysisManager>();
//   TheMAM = std::make_unique<ModuleAnalysisManager>();
//   ThePIC = std::make_unique<PassInstrumentationCallbacks>();
//   TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
//                                                      /*DebugLogging*/ true);
//   TheSI->registerCallbacks(*ThePIC, TheMAM.get());

//   // Add transform passes.
//   // Do simple "peephole" optimizations and bit-twiddling optzns.
//   TheFPM->addPass(InstCombinePass());
//   // Reassociate expressions.
//   TheFPM->addPass(ReassociatePass());
//   // Eliminate Common SubExpressions.
//   TheFPM->addPass(GVNPass());
//   // Simplify the control flow graph (deleting unreachable blocks, etc).
//   TheFPM->addPass(SimplifyCFGPass());

//   // Register analysis passes used in these transform passes.
//   PassBuilder PB;
//   PB.registerModuleAnalyses(*TheMAM);
//   PB.registerFunctionAnalyses(*TheFAM);
//   PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
// }

// static void HandleDefinition() {
//   if (auto FnAST = ParseDefinition()) {
//     if (auto *FnIR = FnAST->codegen()) {
//       fprintf(stderr, "Read function definition:");
//       FnIR->print(errs());
//       fprintf(stderr, "\n");
//       ExitOnErr(TheJIT->addModule(
//           ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
//       InitializeModuleAndManagers();
//     }
//   } else {
//     // Skip token for error recovery.
//     getNextToken();
//   }
// }

// static void HandleExtern() {
//   if (auto ProtoAST = ParseExtern()) {
//     if (auto *FnIR = ProtoAST->codegen()) {
//       fprintf(stderr, "Read extern: ");
//       FnIR->print(errs());
//       fprintf(stderr, "\n");
//       FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
//     }
//   } else {
//     // Skip token for error recovery.
//     getNextToken();
//   }
// }

// static void HandleTopLevelExpression() {
//   // Evaluate a top-level expression into an anonymous function.
//   if (auto FnAST = ParseTopLevelExpr()) {
//     if (FnAST->codegen()) {
//       // Create a ResourceTracker to track JIT'd memory allocated to our
//       // anonymous expression -- that way we can free it after executing.
//       auto RT = TheJIT->getMainJITDylib().createResourceTracker();

//       auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
//       ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
//       InitializeModuleAndManagers();

//       // Search the JIT for the __anon_expr symbol.
//       auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

//       // Get the symbol's address and cast it to the right type (takes no
//       // arguments, returns a double) so we can call it as a native function.
//       double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
//       fprintf(stderr, "Evaluated to %f\n", FP());

//       // Delete the anonymous expression module from the JIT.
//       ExitOnErr(RT->remove());
//     }
//   } else {
//     // Skip token for error recovery.
//     getNextToken();
//   }
// }

// /// top ::= definition | external | expression | ';'
// static void MainLoop() {
//   while (true) {
//     fprintf(stderr, "ready> ");
//     switch (CurTok) {
//     case tok_eof:
//       return;
//     case ';': // ignore top-level semicolons.
//       getNextToken();
//       break;
//     case tok_def:
//       HandleDefinition();
//       break;
//     case tok_extern:
//       HandleExtern();
//       break;
//     default:
//       HandleTopLevelExpression();
//       break;
//     }
//   }
// }

// //===----------------------------------------------------------------------===//
// // "Library" functions that can be "extern'd" from user code.
// //===----------------------------------------------------------------------===//

// #ifdef _WIN32
// #define DLLEXPORT __declspec(dllexport)
// #else
// #define DLLEXPORT
// #endif

// /// putchard - putchar that takes a double and returns 0.
// extern "C" DLLEXPORT double putchard(double X) {
//   fputc((char)X, stderr);
//   return 0;
// }

// /// printd - printf that takes a double prints it as "%f\n", returning 0.
// extern "C" DLLEXPORT double printd(double X) {
//   fprintf(stderr, "%f\n", X);
//   return 0;
// }

// //===----------------------------------------------------------------------===//
// // Main driver code.
// //===----------------------------------------------------------------------===//

// int main() {
//   InitializeNativeTarget();
//   InitializeNativeTargetAsmPrinter();
//   InitializeNativeTargetAsmParser();

//   // Install standard binary operators.
//   // 1 is lowest precedence.
//   BinopPrecedence['<'] = 10;
//   BinopPrecedence['+'] = 20;
//   BinopPrecedence['-'] = 20;
//   BinopPrecedence['*'] = 40; // highest.

//   // Prime the first token.
//   fprintf(stderr, "ready> ");
//   getNextToken();

//   TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

//   InitializeModuleAndManagers();

//   // Run the main "interpreter loop" now.
//   MainLoop();

//   return 0;
// }
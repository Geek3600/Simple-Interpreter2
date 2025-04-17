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
  TOK_IN = -10
};

inline std::string identifierStr;
inline double numVal;

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

    // 识别标识符
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
        return TOK_IDENTIFIER;
    }

    // 识别数字
    //TODO add error check
    if (isdigit(lastChar) || lastChar == '.') {
        std::string numStr;
        do {
            numStr += lastChar;
            lastChar = getchar();
        } while (isdigit(lastChar) || lastChar == '.');
        numVal = strtod(numStr.c_str(), nullptr);
        return TOK_NUMBER;
    }

    // 识别注释
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
public:
    ProtoTypeAST(const std::string &name, std::vector<std::string> args): name(name), args(std::move(args)) {} 
    Function *codeGen();
    const std::string &getName() const {return this->name;}
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

int currentToken;
int getNextToken() {
    return currentToken = getToken();
}


/// expression
///   ::= primary binop pieces
std::unique_ptr<ExprAST> parseExpression() {
    auto LHS = parsePrimary();
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
    return result;
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
    while (true) {
        int tokPrec = getTokenPrecedence();
        if (tokPrec < minimalPrec) return LHS;
        int binOperator = currentToken;
        getNextToken(); // eat binop
        auto RHS = parsePrimary();
        if (!RHS) return nullptr;

        int nextPrec = getTokenPrecedence();
        if (tokPrec < nextPrec) {
            RHS = parseBinaryOpRHS(tokPrec + 1, std::move(RHS));
            if (!RHS) return nullptr;
        }
        LHS = std::make_unique<BinaryOperatorExprAST>(binOperator, std::move(LHS), std::move(RHS));
    }

}

std::unique_ptr<ProtoTypeAST> parseProtoType() {
    if (currentToken != TOK_IDENTIFIER) return logErrorP("expected function name in prototype");
    std::string funcName = identifierStr;
    getNextToken(); // eat function name

    if (currentToken != '(') return logErrorP("expect '(' in prototype");
    std::vector<std::string> argsNames;
    while (getNextToken() == TOK_IDENTIFIER) 
        argsNames.push_back(identifierStr);
    if (currentToken != ')') 
        return logErrorP("expected ')' in prototype");

    getNextToken(); // eat ')'
    return std::make_unique<ProtoTypeAST>(funcName, std::move(argsNames));
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
};


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
            return logErrorV("invalid binary operator");
    }
} 

Value *CallExprAST::codeGen() {
    
    // 在全局模块表中查找名字
    Function *CalleeF = getFunction(callee);
    if (!CalleeF) return logErrorV("unknow function referenced");

    if (CalleeF->arg_size() != args.size()) return logErrorV("incorrect # arguments passed");

    std::vector<Value *> argsV;
    for (unsigned i = 0, e = args.size(); i != e; ++i) {
        argsV.push_back(args[i]->codeGen());
        if (!argsV.back()) return nullptr;
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
    // Function *TheFunction = TheModule->getFunction(proto->getName());
    if (!TheFunction)
        TheFunction = proto->codeGen();
    if (!TheFunction) return nullptr;
    // if (!TheFunction->empty()) return (Function *)logErrorV("function cannot ne redefined.");

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
    return nullptr;
}

Value* IfExprAST::codeGen() {
    Value * CondV = Cond->codeGen();
    if (!CondV) return nullptr;

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
    if (!ElseV) return nullptr;

    Builder->CreateBr(MergeBB);
    ElseBB = Builder->GetInsertBlock();

    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);

    return PN;
}

Value *ForExprAST::codeGen() {
    Value *StartVal = Start->codeGen();

    if (!StartVal) return nullptr;
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder->GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    Builder->CreateBr(LoopBB);

    Builder->SetInsertPoint(LoopBB);
    PHINode *Variable = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
    Variable->addIncoming(StartVal, PreheaderBB);

    Value* OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;

    if (!Body->codeGen()) return nullptr;

    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codeGen();
        if (!StepVal) return nullptr;
    }
    else {
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    Value* NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");
    Value *EndCond = End->codeGen();
    if (!EndCond) return nullptr;

    EndCond = Builder->CreateFCmpONE(EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

    BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);
    Builder->SetInsertPoint(AfterBB);

    Variable->addIncoming(NextVar, LoopEndBB);
    if (OldVal) NamedValues[VarName] = OldVal;
    else NamedValues.erase(VarName);

    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
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
            fprintf(stderr, "\nRead extern:\n");
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

            fprintf(stderr, "\nRead top-level expression:\n");
            fnIR->print(errs());
            fprintf(stderr, "\n");
            fnIR->eraseFromParent();
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
    BinaryOpPrecedence['/'] = 40;

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


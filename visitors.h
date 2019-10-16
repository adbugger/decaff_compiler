#ifndef __VISITORS_H__
#define __VISITORS_H__

#include "ast.h"
#include "types.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"

#include <map>
#include <stack>

class DebugVisitor: public ASTVisitor {
public:
    llvm::Value* visit(ProgramNode&);
    llvm::Value* visit(Field&);
    llvm::Function* visit(Method&);
    llvm::Value* visit(Identifier&);
    llvm::Value* visit(Argument&);
    llvm::Value* visit(Block&);
    llvm::Value* visit(Variable&);

    llvm::Value* visit(AssignStatement&);
    llvm::Value* visit(IfStatement&);
    llvm::Value* visit(ForStatement&);
    llvm::Value* visit(MethodCallStatement&);
    llvm::Value* visit(ReturnStatement&);
    llvm::Value* visit(BlockStatement&);
    llvm::Value* visit(BreakStatement&);
    llvm::Value* visit(ContinueStatement&);

    llvm::Value* visit(Location&);
    llvm::Value* visit(MethodCall&);
    llvm::Value* visit(Callout&);

    llvm::Value* visit(LocationExpr&);
    llvm::Value* visit(LiteralExpr&);
    llvm::Value* visit(BinaryExpr&);
    llvm::Value* visit(UnaryExpr&);
    llvm::Value* visit(MethodCallExpr&);

    llvm::Value* visit(IntegerLiteral&);
    llvm::Value* visit(StringLiteral&);
    llvm::Value* visit(BooleanLiteral&);

    virtual ~DebugVisitor() {};
};

class LoopInfo {
public:
    llvm::BasicBlock *after, *body;
    llvm::Value* condition;
    std::string loop_var;
    llvm::PHINode* phi_var;
    LoopInfo(llvm::BasicBlock *bb1, llvm::BasicBlock *bb2,
                llvm::Value* v, std::string s, llvm::PHINode* p)
             : after(bb1), body(bb2), condition(v), loop_var(s), phi_var(p) {}
};

class CodegenVisitor: public ASTVisitor {
public:
    llvm::LLVMContext Context;
    llvm::IRBuilder<> *Builder;
    llvm::Module *Module;
    llvm::Value* reportError(std::string);
    std::map<std::string, llvm::AllocaInst*> NamedValues;
    std::stack<LoopInfo*> loops;
    llvm::AllocaInst* CreateEntryBlockAllocation(
        llvm::Function*, std::string*, Type);

    CodegenVisitor();
    void generateCode();

    llvm::Value* visit(ProgramNode&);
    llvm::Value* visit(Field&);
    llvm::Function* visit(Method&);
    /* identifiers and arguments have been handled in methods itself */
    llvm::Value* visit(Identifier&);
    llvm::Value* visit(Argument&);

    llvm::Value* visit(Block&);
    llvm::Value* visit(Variable&);

    llvm::Value* visit(AssignStatement&);
    llvm::Value* visit(IfStatement&);
    llvm::Value* visit(ForStatement&);
    llvm::Value* visit(MethodCallStatement&);
    llvm::Value* visit(ReturnStatement&);
    llvm::Value* visit(BlockStatement&);
    llvm::Value* visit(BreakStatement&);
    llvm::Value* visit(ContinueStatement&);

    llvm::Value* visit(Location&);
    llvm::Value* visit(MethodCall&);
    llvm::Value* visit(Callout&);

    llvm::Value* visit(LocationExpr&);
    llvm::Value* visit(LiteralExpr&);
    llvm::Value* visit(BinaryExpr&);
    llvm::Value* visit(UnaryExpr&);
    llvm::Value* visit(MethodCallExpr&);

    llvm::Value* visit(IntegerLiteral&);
    llvm::Value* visit(StringLiteral&);
    llvm::Value* visit(BooleanLiteral&);

    virtual ~CodegenVisitor() {};
};

#endif /* __VISITORS_H__ */

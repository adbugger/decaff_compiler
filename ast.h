#ifndef __AST_H__
#define __AST_H__

#include <vector>
#include <string>
#include "types.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Value.h"

using namespace std;

class ASTNode;
class ProgramNode;
class Field;
class Method;
class Identifier;
class Argument;
class Block;
class Variable;

class Statement;
class AssignStatement;
class IfStatement;
class ForStatement;
class MethodCallStatement;
class ReturnStatement;
class BlockStatement;
class BreakStatement;
class ContinueStatement;

class Location;
class MethodCall;
class Callout;

class Expression;
class LocationExpr;
class LiteralExpr;
class BinaryExpr;
class UnaryExpr;
class MethodCallExpr;

class Literal;
class IntegerLiteral;
class StringLiteral;
class BooleanLiteral;

class ASTVisitor {
public:
    virtual llvm::Value* visit(ProgramNode&) = 0;
    virtual llvm::Value* visit(Field&) = 0;
    virtual llvm::Function* visit(Method&) = 0;
    virtual llvm::Value* visit(Identifier&) = 0;
    virtual llvm::Value* visit(Argument&) = 0;
    virtual llvm::Value* visit(Block&) = 0;
    virtual llvm::Value* visit(Variable&) = 0;

    virtual llvm::Value* visit(AssignStatement&) = 0;
    virtual llvm::Value* visit(IfStatement&) = 0;
    virtual llvm::Value* visit(ForStatement&) = 0;
    virtual llvm::Value* visit(MethodCallStatement&) = 0;
    virtual llvm::Value* visit(ReturnStatement&) = 0;
    virtual llvm::Value* visit(BlockStatement&) = 0;
    virtual llvm::Value* visit(BreakStatement&) = 0;
    virtual llvm::Value* visit(ContinueStatement&) = 0;

    virtual llvm::Value* visit(Location&) = 0;
    virtual llvm::Value* visit(MethodCall&) = 0;
    virtual llvm::Value* visit(Callout&) = 0;

    virtual llvm::Value* visit(LocationExpr&) = 0;
    virtual llvm::Value* visit(LiteralExpr&) = 0;
    virtual llvm::Value* visit(BinaryExpr&) = 0;
    virtual llvm::Value* visit(UnaryExpr&) = 0;
    virtual llvm::Value* visit(MethodCallExpr&) = 0;

    virtual llvm::Value* visit(IntegerLiteral&) = 0;
    virtual llvm::Value* visit(StringLiteral&) = 0;
    virtual llvm::Value* visit(BooleanLiteral&) = 0;

    virtual ~ASTVisitor() {};
};

class ASTNode {
public:
    virtual ~ASTNode() {};
    virtual llvm::Value* accept(ASTVisitor& visitor) = 0;
};

class ProgramNode : public ASTNode {
public:
    std::vector<Field*>* fields;
    std::vector<Method*>* methods;
    ProgramNode();
    ProgramNode(std::vector<Field*>* _fieldList);
    ProgramNode(std::vector<Field*>* _fieldList, std::vector<Method*>* _methodList);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Field : public ASTNode {
public:
    Type type;
    std::vector<Identifier*>* identifiers;
    Field(Type t, std::vector<Identifier*>* ids);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Method : public ASTNode {
public:
    Type return_type;
    std::string* name;
    std::vector<Argument*>* args;
    Block* block;
    Method();
    Method(Type type, std::string* name, Block* block);
    Method(Type type, std::string* name, std::vector<Argument*>* args, Block* block);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Identifier : public ASTNode {
public:
    std::string* name;
    bool array;
    int size;
    Identifier(std::string* name);
    Identifier(std::string* name, int size);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Argument : public ASTNode {
public:
    Type type;
    std::string* name;
    Argument(Type t, std::string* n);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Block : public ASTNode {
public:
    std::vector<Variable*>* variables;
    std::vector<Statement*>* statements;
    bool has_return();
    Block();
    Block(std::vector<Variable*>* vars);
    Block(std::vector<Variable*>* vars, std::vector<Statement*>* stmts);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Variable : public ASTNode {
public:
    Type type;
    std::vector<std::string*>* vars;
    Variable();
    Variable(Type t, std::vector<std::string*>* v);
    llvm::Value* accept(ASTVisitor& visitor);
};

class Statement : public ASTNode {
public:
    virtual bool is_return() { return false; };
};

class AssignStatement : public Statement {
public:
    Location* lvalue;
    AssignOp assOp;
    Expression* rvalue;
    AssignStatement(Location*, AssignOp, Expression*);
    llvm::Value* accept(ASTVisitor&);
};

class IfStatement : public Statement {
public:
    Expression* expr;
    Block* main_block;
    Block* else_block;
    IfStatement(Expression*, Block*, Block*);
    IfStatement(Expression*, Block*);
    llvm::Value* accept(ASTVisitor&);
    bool is_return();
};

class ForStatement : public Statement {
public:
    std::string* loop_var;
    Expression* initial_expr;
    Expression* true_condition;
    Block* loop_body;
    ForStatement(std::string*, Expression*, Expression*, Block*);
    llvm::Value* accept(ASTVisitor&);
    bool is_return();
};

class MethodCallStatement : public Statement {
public:
    MethodCall* method_call;
    MethodCallStatement(MethodCall*);
    llvm::Value* accept(ASTVisitor&);
};

class ReturnStatement : public Statement {
public:
    Expression* return_expr;
    ReturnStatement();
    ReturnStatement(Expression*);
    llvm::Value* accept(ASTVisitor&);
    bool is_return();
};

class BlockStatement : public Statement {
public:
    Block* block;
    BlockStatement(Block*);
    llvm::Value* accept(ASTVisitor&);
    bool is_return();
};

class BreakStatement : public Statement {
public:
    llvm::Value* accept(ASTVisitor&);
};

class ContinueStatement : public Statement {
public:
    llvm::Value* accept(ASTVisitor&);
};

class Location : public ASTNode {
public:
    std::string* id;
    std::vector<Expression*>* addrs;
    Location(std::string*);
    Location(std::string*, std::vector<Expression*>*);
    llvm::Value* accept(ASTVisitor&);
};

class MethodCall : public ASTNode {
public:
    std::string* method_name;
    std::vector<Expression*>* expr_args;
    MethodCall(std::string*, std::vector<Expression*>*);
    llvm::Value* accept(ASTVisitor&);
};
class Callout : public MethodCall {
public:
    Callout(std::string*, std::vector<Expression*>*);
    llvm::Value* accept(ASTVisitor&);
};

class Expression : public ASTNode {};

class LocationExpr : public Expression {
public:
    Location* location;
    LocationExpr(Location*);
    llvm::Value* accept(ASTVisitor&);
};

class LiteralExpr : public Expression {
public:
    Literal* literal;
    LiteralExpr(Literal*);
    llvm::Value* accept(ASTVisitor&);
};

class BinaryExpr : public Expression {
public:
    Expression* left;
    BinaryOp binOp;
    Expression* right;
    BinaryExpr(Expression*, BinaryOp, Expression*);
    llvm::Value* accept(ASTVisitor&);
};

class UnaryExpr : public Expression {
public:
    UnaryOp unOp;
    Expression* expr;
    UnaryExpr(UnaryOp, Expression*);
    llvm::Value* accept(ASTVisitor&);
};

class MethodCallExpr : public Expression {
public:
    MethodCall* call;
    MethodCallExpr(MethodCall*);
    llvm::Value* accept(ASTVisitor&);
};

class Literal : public ASTNode {};

class IntegerLiteral : public Literal {
public:
    int intval;
    IntegerLiteral(int);
    llvm::Value* accept(ASTVisitor&);
};

class StringLiteral : public Literal {
public:
    std::string* strval;
    StringLiteral(std::string*);
    llvm::Value* accept(ASTVisitor&);
};

class BooleanLiteral : public Literal {
public:
    bool boolval;
    BooleanLiteral(bool);
    llvm::Value* accept(ASTVisitor&);
};

class ASTContext
{
  public:
    ASTNode* pRoot;
    ~ASTContext()
    {
	    clearAST();
    }

    // free all saved expression trees
    void clearAST()
    {
        if(pRoot != NULL and pRoot != nullptr) delete pRoot;
    }
};

#endif /* End of __AST_H__ */

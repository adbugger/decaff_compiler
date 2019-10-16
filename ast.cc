#include <vector>
#include <string>
#include <iostream>

#include "ast.h"
#include "types.h"

using namespace std;

ProgramNode::ProgramNode(
    vector<Field*>* _fieldList, vector<Method*>* _methodList)
    : fields(_fieldList), methods(_methodList) {}
ProgramNode::ProgramNode(vector<Field*>* _fieldList)
    : ProgramNode(_fieldList, new vector<Method*>()) {}
ProgramNode::ProgramNode()
    : ProgramNode(new vector<Field*>(), new vector<Method*>()) {}
llvm::Value* ProgramNode::accept(ASTVisitor& visitor){ return visitor.visit(*this); }

Field::Field(Type _s, vector<Identifier*>* _ids)
    : type(_s), identifiers(_ids) {}
llvm::Value* Field::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

Method::Method(Type t, string* _s, vector<Argument*>* _args, Block* _blk)
    : return_type(t), name(_s), args(_args), block(_blk) {}
Method::Method(Type t, string* _s, Block* _blk)
    : Method(t, _s, new vector<Argument*>(), _blk) {}
Method::Method()
    : Method(
        Type::VOID,
        new string("random method"),
        new vector<Argument*>(),
        new Block()
    ) {}
llvm::Value* Method::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

Identifier::Identifier(string* _name)
    : name(_name), array(false), size(0) {}
Identifier::Identifier(string* _name, int _size)
    : name(_name), array(true), size(_size) {}
llvm::Value* Identifier::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

Argument::Argument(Type t, string* s) : type(t), name(s) {}
llvm::Value* Argument::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

Block::Block(vector<Variable*>* vars, vector<Statement*>* stmts)
    : variables(vars), statements(stmts) {}
Block::Block(vector<Variable*>* vars)
    : Block(vars, new vector<Statement*>()) {}
Block::Block()
    : Block(
        new vector<Variable*>(),
        new vector<Statement*>()
    ) {}
llvm::Value* Block::accept(ASTVisitor& visitor) { return visitor.visit(*this); }
bool Block::has_return() {
    for (Statement* stmt : *(statements)) {
        if (stmt->is_return()) return true;
    }
    return false;
}

Variable::Variable(Type _type, vector<string*>* _vars)
    : type(_type), vars(_vars) {}
Variable::Variable(): Variable(Type::VOID, new vector<string*>()) {}
llvm::Value* Variable::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

AssignStatement::AssignStatement(Location* l, AssignOp op, Expression* r)
    : lvalue(l), assOp(op), rvalue(r) {}
llvm::Value* AssignStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

IfStatement::IfStatement(Expression* e, Block* main, Block* other)
    : expr(e), main_block(main), else_block(other) {}
IfStatement::IfStatement(Expression* e, Block* main)
    : IfStatement(e, main, nullptr) {}
llvm::Value* IfStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }
bool IfStatement::is_return() {
    if (main_block->has_return() or else_block->has_return()) return true;
    return false;
}

ForStatement::ForStatement(
    string* _var, Expression* _init, Expression* _cond, Block* _body)
    : loop_var(_var),
      initial_expr(_init),
      true_condition(_cond),
      loop_body(_body) {}
llvm::Value* ForStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }
bool ForStatement::is_return() {
    if (loop_body->has_return()) return true;
    return false;
}

MethodCallStatement::MethodCallStatement(MethodCall* _call)
    : method_call(_call) {}
llvm::Value* MethodCallStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

ReturnStatement::ReturnStatement(Expression* e) : return_expr(e) {}
ReturnStatement::ReturnStatement() : ReturnStatement(nullptr) {}
llvm::Value* ReturnStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }
bool ReturnStatement::is_return() { return true; }

BlockStatement::BlockStatement(Block* b) : block(b) {}
llvm::Value* BlockStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }
bool BlockStatement::is_return() { return block->has_return(); }

llvm::Value* BreakStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

llvm::Value* ContinueStatement::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

Location::Location(string* _id, vector<Expression*>* exp_addrs)
    : id(_id), addrs(exp_addrs) {}
Location::Location(string* _id)
    : Location(_id, new vector<Expression*>()) {}
llvm::Value* Location::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

MethodCall::MethodCall(string* _method, vector<Expression*>* _args)
    : method_name(_method), expr_args(_args) {}
llvm::Value* MethodCall::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

Callout::Callout(string* _name, vector<Expression*>* _args)
    : MethodCall(_name, _args) {}
llvm::Value* Callout::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

LocationExpr::LocationExpr(Location* l) : location(l) {}
llvm::Value* LocationExpr::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

LiteralExpr::LiteralExpr(Literal* l): literal(l) {}
llvm::Value* LiteralExpr::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

BinaryExpr::BinaryExpr(Expression* l, BinaryOp op, Expression* r)
    : left(l), binOp(op), right(r) {}
llvm::Value* BinaryExpr::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

UnaryExpr::UnaryExpr(UnaryOp op, Expression* _expr)
    : unOp(op), expr(_expr) {}
llvm::Value* UnaryExpr::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

MethodCallExpr::MethodCallExpr(MethodCall* c) : call(c) {}
llvm::Value* MethodCallExpr::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

IntegerLiteral::IntegerLiteral(int x) : intval(x) {}
llvm::Value* IntegerLiteral::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

StringLiteral::StringLiteral(string* s) : strval(s) {}
llvm::Value* StringLiteral::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

BooleanLiteral::BooleanLiteral(bool v) : boolval(v) {}
llvm::Value* BooleanLiteral::accept(ASTVisitor& visitor) { return visitor.visit(*this); }

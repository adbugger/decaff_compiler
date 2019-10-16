#include "ast.h"
#include "visitors.h"
#include <iostream>

#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"

llvm::Value* DebugVisitor::visit(ProgramNode& node) {
    std::cout << "Program Node with " << node.fields->size() << " fields and "
         << node.methods->size() << " methods\n";
    for (Field* f : *(node.fields))     f->accept(*this);
    for (Method* m : *(node.methods))   m->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(Field& field) {
    std::cout << "Field of type " << field.type << " with "
         << field.identifiers->size() << " identifiers: ";
    for (Identifier* id : *(field.identifiers)) id->accept(*this);
    std::cout << '\n';
	return nullptr;
}
llvm::Value* DebugVisitor::visit(Identifier& id) {
    std::cout << "Identifier " << *(id.name) << " ";
    if(id.array) std::cout << " is array of size " << id.size << " and ";
	return nullptr;
}
llvm::Function* DebugVisitor::visit(Method& method) {
    std::cout << "Method " << *(method.name) << " of type " << method.return_type
         << " with " << method.args->size() << " arguments:";
    for (Argument* a : *(method.args)) std::cout << " " << *(a->name);
    std::cout << '\n';
    for (Argument* a : *(method.args)) a->accept(*this);
    std::cout << "and the following block:\n";
    method.block->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(Argument& arg) {
    std::cout << "Argument " << *(arg.name) << " of type " << arg.type << '\n';
	return nullptr;
}
llvm::Value* DebugVisitor::visit(Block& block) {
    std::cout << "Block with " << block.variables->size() << " variables and "
         << block.statements->size() << " statements\nVariables:\n";
    for (Variable* v : *(block.variables)) v->accept(*this);
    std::cout << "Statements:\n";
    for (Statement* s: *(block.statements)) s->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(Variable& var) {
    std::cout << "Variable of type " << var.type << " with identifiers:";
    for (string* s : *(var.vars)) std::cout << " " << *s;
    std::cout << '\n';
	return nullptr;
}

llvm::Value* DebugVisitor::visit(AssignStatement& assign_stmt) {
    std::cout << "Assignment Statement which has the following lvalue: ";
    assign_stmt.lvalue->accept(*this);
    std::cout << " with operator " << assign_stmt.assOp << " which has the "
         << "following rvalue: ";
    assign_stmt.rvalue->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(IfStatement& if_stmt) {
    std::cout << "If Statement with main expression "; if_stmt.expr->accept(*this);
    std::cout << " and main block "; if_stmt.main_block->accept(*this);

    if(if_stmt.else_block){
        std::cout << " and else block ";
        if_stmt.else_block->accept(*this);
    }
	return nullptr;
}
llvm::Value* DebugVisitor::visit(ForStatement& for_stmt) {
    std::cout << "For Statement with loop variable " << *(for_stmt.loop_var)
         << " and initial expression "; for_stmt.initial_expr->accept(*this);
    std::cout << " and true condition "; for_stmt.true_condition->accept(*this);
    std::cout << " and the following loop body "; for_stmt.loop_body->accept(*this);
    std::cout << '\n';
	return nullptr;
}
llvm::Value* DebugVisitor::visit(MethodCallStatement& method_stmt) {
    std::cout << "Got a Method Call Statement: ";
    method_stmt.method_call->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(ReturnStatement& ret_stmt) {
    std::cout << "Got a return statement ";
    if(ret_stmt.return_expr) {
        std::cout << "with the following return expression ";
        ret_stmt.return_expr->accept(*this);
    }
    else std::cout << "with no return expression\n";
	return nullptr;
}
llvm::Value* DebugVisitor::visit(BlockStatement& block_stmt) {
    std::cout << "Got a Block Statement: ";
    block_stmt.block->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(BreakStatement&) {
    std::cout << "Got a Break Statement\n";
	return nullptr;
}
llvm::Value* DebugVisitor::visit(ContinueStatement&) {
    std::cout << "Got a Continue Statement\n";
	return nullptr;
}

llvm::Value* DebugVisitor::visit(Location& loc) {
    std::cout << "Got a Location with name " << *(loc.id) << " and "
         << loc.addrs->size() << " address expressions:\n";
    for(Expression* expr : *(loc.addrs)) expr->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(MethodCall& call) {
    std::cout << "Calling method " << *(call.method_name) << " and "
         << call.expr_args->size() << " argument expressions\n";
    for (Expression* expr : *(call.expr_args)) expr->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(Callout& call) {
    std::cout << "CALLOUT with EXTERNAL method " << *(call.method_name) << " and "
         << call.expr_args->size() << " argument expression\n";
    for (Expression* expr : *(call.expr_args)) expr->accept(*this);
	return nullptr;
}

llvm::Value* DebugVisitor::visit(LiteralExpr& lit) {
    std::cout << "Got Literal Expression: ";
    lit.literal->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(LocationExpr& lExpr){
    std::cout << "Got Location Expression: ";
    lExpr.location->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(BinaryExpr& binExpr){
    std::cout << "Got Binary Expression with binary operation " << binExpr.binOp
         << " and left expression "; binExpr.left->accept(*this);
    std::cout << " and right expression "; binExpr.right->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(UnaryExpr& unExpr) {
    std::cout << "Got Unary Expression with operator " << unExpr.unOp
         << " and expression "; unExpr.expr->accept(*this);
	return nullptr;
}
llvm::Value* DebugVisitor::visit(MethodCallExpr& callExpr) {
    std::cout << "Got Method Call Expression: ";
    callExpr.call->accept(*this);
	return nullptr;
}

llvm::Value* DebugVisitor::visit(IntegerLiteral& intLit) {
    std::cout << "Integer Literal with value " << intLit.intval << '\n';
	return nullptr;
}
llvm::Value* DebugVisitor::visit(StringLiteral& strLit) {
    std::cout << "String Literal with value |" << *(strLit.strval) << "|\n";
	return nullptr;
}
llvm::Value* DebugVisitor::visit(BooleanLiteral& booLit) {
    std::cout << "Boolean Literal with value " << booLit.boolval << '\n';
	return nullptr;
}

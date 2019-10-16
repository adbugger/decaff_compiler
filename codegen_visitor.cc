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
#include "llvm/Support/raw_ostream.h"

#include "ast.h"
#include "visitors.h"
#include "types.h"

#include <iostream>
#include <memory>
#include <string>
#include <set>

CodegenVisitor::CodegenVisitor() {
    Builder = new llvm::IRBuilder<>(Context);
    Module = new llvm::Module("Decaf Compiler", Context);
}

llvm::AllocaInst* CodegenVisitor::CreateEntryBlockAllocation(
    llvm::Function* function, std::string* v, Type t
) {
    std::string varName = *v;
    // Builder for current context
    llvm::IRBuilder<> temp(&function->getEntryBlock(),
                            function->getEntryBlock().begin());
    llvm::AllocaInst* allocate_instruction = nullptr;
    if(t == Type::INT) {
        allocate_instruction = temp.CreateAlloca(
            llvm::Type::getInt32Ty(Context), 0, varName);
    } else if (t == Type::BOOL) {
        allocate_instruction = temp.CreateAlloca(
            llvm::Type::getInt1Ty(Context), 0, varName);
    }
    return allocate_instruction;
}

llvm::Value* CodegenVisitor::reportError(std::string err_str){
    std::cerr << "\033[1;31m" << err_str << "\033[0m\n";
    return nullptr;
}

void CodegenVisitor::generateCode() {
    // std::cout << "\n\033[32m" << "Generating llvm IR below" << "\033[0m\n\n";
    Module->print(llvm::outs(), nullptr);
}

llvm::Value* CodegenVisitor::visit(Argument& arg) { return nullptr; };
llvm::Value* CodegenVisitor::visit(Identifier& id) { return nullptr; };

llvm::Value* CodegenVisitor::visit(ProgramNode& node) {
    // std::cout << "Codegen visiting program node\n";

    llvm::Value* v;
    bool fields_valid = true;
    for(Field* f : *(node.fields)) {
        v = f->accept(*this);
        if(!v) {
            reportError("Found invalid field declarations");
            fields_valid = false;
        }
    }
    if(!fields_valid){
        std::cerr << "Codegen exiting with invalid field declarations\n";
        return nullptr;
    }

    bool methods_valid = true;
    for(Method* m : *(node.methods)) {
        v = m->accept(*this);
        if(!v) {
            reportError("Found invalid method declarations");
            methods_valid = false;
        }
    }
    if(!methods_valid){
        std::cerr << "Codegen exiting with invalid method declarations\n";
        return nullptr;
    }
    // std::cout << "Codegen out of program node\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(Field& field) {
    // std::cout << "Codegen visiting field declaration\n";
    llvm::Type *type = nullptr;
    if (field.type == Type::INT) type = llvm::Type::getInt32Ty(Context);
    else if (field.type == Type::BOOL) type = llvm::Type::getInt1Ty(Context);

    for(Identifier* id : *(field.identifiers)){
        if(Module->getNamedGlobal(*(id->name)) != nullptr) {
            reportError("Global variable " + *(id->name) + " redeclared. " +
                        "Ignoring redeclaration");
            continue;
        }
        if(id->array) {
            if(id->size < 0) {
                reportError("Found array declaration " + *(id->name) +
                            " with negative size");
                return nullptr;
            }
            llvm::ArrayType * arrType = llvm::ArrayType::get(type, id->size);
            llvm::GlobalVariable *gvar = new llvm::GlobalVariable(
                *Module, arrType, false, llvm::GlobalValue::ExternalLinkage,
                nullptr, *(id->name)
            );
            gvar->setInitializer(llvm::ConstantAggregateZero::get(arrType));
        } else {
            llvm::GlobalVariable *gvar = new llvm::GlobalVariable(
                *Module, type, false, llvm::GlobalValue::ExternalLinkage,
                nullptr, *(id->name)
            );
            gvar->setInitializer(llvm::Constant::getNullValue(type));
        }
    }
    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));

    // std::cout << "Codegen out of field declaration\n";
    return v;
}

llvm::Function* CodegenVisitor::visit(Method& method) {
    // std::cout << "Codegen visiting method declaration\n";
    // Get llvm type of each argument
    std::vector<llvm::Type*> argument_types;
    for (Argument* arg : *(method.args)) {
        if (arg->type == Type::INT) {
            argument_types.push_back(llvm::Type::getInt32Ty(Context));
        } else if (arg->type == Type::BOOL) {
            argument_types.push_back(llvm::Type::getInt1Ty(Context));
        } else {
            std::cerr << "Codegen out of method declaration due to invalid "
                      << "argument type\n";
            reportError("Arguments can only be int or boolean");
            return nullptr;
        }
    }

    // Get return type of function
    llvm::Type* returnType;
    if(method.return_type == Type::INT) {
        returnType = llvm::Type::getInt32Ty(Context);
    } else if (method.return_type == Type::BOOL) {
        returnType = llvm::Type::getInt1Ty(Context);
    } else if (method.return_type == Type::VOID) {
        returnType = llvm::Type::getVoidTy(Context);
    } else {
        std::cerr << "Codegen out of method declaration due to invalid return "
                  << "type\n";
        reportError("Invalid return type for function " + *(method.name) +
                    ". Expected int or bool or void");
        return nullptr;
    }

    // Create the llvm function
    llvm::FunctionType* FT = llvm::FunctionType::get(
        returnType, argument_types, false);
    llvm::Function* F = llvm::Function::Create(
        FT, llvm::Function::ExternalLinkage, *(method.name), Module);

    // Set names for arguments
    std::vector<Argument*>::iterator my_arg_it = method.args->begin();
    llvm::Function::arg_iterator llvm_arg_it = F->arg_begin();
    while(my_arg_it != method.args->end() and llvm_arg_it != F->arg_end()) {
        Argument* my_arg = *my_arg_it;
        llvm_arg_it->setName(*(my_arg->name));
        my_arg_it++; llvm_arg_it++;
    }

    // Create block for function
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(Context, "entry", F);
    Builder->SetInsertPoint(BB);

    // Save old scope
    std::map<std::string, llvm::AllocaInst*> oldNamedValues = NamedValues;

    // Allocate stack memory
    my_arg_it = method.args->begin();
    llvm_arg_it = F->arg_begin();
    while(my_arg_it != method.args->end() and llvm_arg_it != F->arg_end()){
        Argument* my_arg_ptr = *my_arg_it;
        llvm::AllocaInst* alloca = CreateEntryBlockAllocation(
            F, my_arg_ptr->name, my_arg_ptr->type);
        Builder->CreateStore(llvm_arg_it, alloca);
        // Adding argument names to named values
        NamedValues[*(my_arg_ptr->name)] = alloca;
        my_arg_it++; llvm_arg_it++;
    }

    // Generate code for body with current named values
    llvm::Value* retval = method.block->accept(*this);

    // Reset old named values
    NamedValues = oldNamedValues;

    // Match and store return type
    if(!retval) {
        std::cerr << "Codegen exiting method declaration due to invalid return"
                  << " type from function block\n";
        F->eraseFromParent();
        return nullptr;
    }

    // if(method.return_type == Type::VOID) Builder->CreateRetVoid();
    // else Builder->CreateRet(retval);

    llvm::verifyFunction(*F);
    // std::cout << "Codegen out of method declaration\n";
    return F;
}

llvm::Value* CodegenVisitor::visit(Block& block) {
    // std::cout << "Codegen visiting block\n";
    std::set<std::string> scopeDec;
    for (Variable* var : *(block.variables)) {
        for (std::string* var_name : *(var->vars)) {
            if(scopeDec.find(*var_name) != scopeDec.end()){
                reportError("Local Variable " + *(var_name) + " redeclared " +
                            "in same scope");
                return nullptr;
            }
            scopeDec.insert(*var_name);
        }
    }

    llvm::Value *v = nullptr;
    for (Variable* var : *(block.variables)) {
        v = var->accept(*this);
        if(!v){
            std::cerr << "Codegen exiting block due to "
                      << "invalid variable declaration\n";
            return nullptr;
        }
    }

    for (Statement* stmt : *(block.statements)) {
        v = stmt->accept(*this);
        if(!v){
            std::cerr << "Codegen exiting block due to "
                      << "invalid statement declaration\n";
            return nullptr;
        }
    }

    // std::cout << "Codegen out of block\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(Variable& variable) {
    // std::cout << "Codegen visiting variable declaration\n";
    llvm::Function *F = Builder->GetInsertBlock()->getParent();
    for (std::string* ptr_var_name : *(variable.vars)) {
        llvm::Value* initial_val = nullptr;
        llvm::AllocaInst* alloca = nullptr;

        if(variable.type == Type::INT) {
            initial_val = llvm::ConstantInt::get(Context, llvm::APInt(32, 0));
        } else if (variable.type == Type::BOOL) {
            initial_val = llvm::ConstantInt::get(Context, llvm::APInt(1, 0));
        }
        alloca = CreateEntryBlockAllocation(F, ptr_var_name, variable.type);
        Builder->CreateStore(initial_val, alloca);
        /* At this point we can safely modify the NamedValues map because
         * we have stored the old one in the ancestor block code gen pass.
         */
        NamedValues[*ptr_var_name] = alloca;
    }

    // std::cout << "Codegen out of variable declaration\n";

    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));
    return v;
}

llvm::Value* CodegenVisitor::visit(AssignStatement& assign_stmt) {

    // std::cout << "Codegen visiting Assignment Statement\n";

    // TODO: implement visitor for Location class
    llvm::Value* lhs_location = assign_stmt.lvalue->accept(*this);
    if(!lhs_location){

        std::cerr << "Codegen exiting Assignment Statement due to error "
                  << "in lvalue location\n";

        return nullptr;
    }
    llvm::Value* lhs_value = Builder->CreateLoad(lhs_location);

    // TODO: implement visitor for Expression class
    llvm::Value* rhs = assign_stmt.rvalue->accept(*this);
    if(!rhs){

        std::cerr << "Codegen exiting Assignment Statement due to error "
                  << "in rvalue expression\n";

        return nullptr;
    }

    if (assign_stmt.assOp == AssignOp::ASS_INC) {
        rhs = Builder->CreateAdd(lhs_value, rhs, "addEqualToTmp");
    } else if (assign_stmt.assOp == AssignOp::ASS_DEC) {
        rhs = Builder->CreateSub(lhs_value, rhs, "subEqualToTmp");
    }

    // std::cout << "Codegen exiting Assignment Statement\n";

    return Builder->CreateStore(rhs, lhs_location);
}

llvm::Value* CodegenVisitor::visit(IfStatement& if_stmt) {

    // std::cout << "Codegen visiting if statement\n";

    llvm::Value* condition = if_stmt.expr->accept(*this);
    if(!condition){

        std::cerr << "Codegen exiting if statment due to error in condition\n";

        return nullptr;
    }

    // Basic blocks for if then else
    llvm::Function* F = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *if_block = llvm::BasicBlock::Create(Context, "if", F);
    llvm::BasicBlock *else_block = llvm::BasicBlock::Create(Context, "else");
    llvm::BasicBlock *next_block = llvm::BasicBlock::Create(Context, "next");
    llvm::BasicBlock *other_block = else_block;
    bool ret_if = if_stmt.main_block->has_return(), ret_else = false;

    // There may not be an else block
    if (if_stmt.else_block == nullptr) other_block = next_block;
    Builder->CreateCondBr(condition, if_block, other_block);

    // Generate code for the if block
    Builder->SetInsertPoint(if_block);
    llvm::Value* if_val = if_stmt.main_block->accept(*this);
    if(!if_val) {
        std::cerr << "Codegen exiting if statment due to error in main block\n";
        return nullptr;
    }
    if(!ret_if) Builder->CreateBr(next_block);

    if_block = Builder->GetInsertBlock();

    if (if_stmt.else_block != nullptr) {
        F->getBasicBlockList().push_back(else_block);
        Builder->SetInsertPoint(else_block);

        llvm::Value* else_val = if_stmt.else_block->accept(*this);
        if(!else_val) {

            std::cerr << "Codegen exiting if statment due to error in else block\n";

            return nullptr;
        }
        ret_else = if_stmt.else_block->has_return();
        if(!ret_else) Builder->CreateBr(next_block);
    }

    // Create break for next part of code
    F->getBasicBlockList().push_back(next_block);
    Builder->SetInsertPoint(next_block);
    if(ret_else and ret_if){
        llvm::Type* ret_type = Builder->GetInsertBlock()->getParent()->getReturnType();
        if(ret_type == llvm::Type::getVoidTy(Context)){
            Builder->CreateRetVoid();
        } else {
            Builder->CreateRet(llvm::ConstantInt::get(Context, llvm::APInt(32, 0)));
        }
    }
    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 0));

    // std::cout << "Codegen exiting If Statement\n";

    return v;
}

llvm::Value* CodegenVisitor::visit(ForStatement& for_stmt) {
    // std::cout << "Codegen visiting for statement\n";

    llvm::Value* start_val = for_stmt.initial_expr->accept(*this);
    if(!start_val){
        std::cerr << "Codegen exiting for statement due to error in initial_expr\n";
        return nullptr;
    }

    // Get parent method for loop
    llvm::Function* F = Builder->GetInsertBlock()->getParent();
    // Allocate memory for loop variable
    llvm::AllocaInst* Alloca = CreateEntryBlockAllocation(F, for_stmt.loop_var, Type::INT);
    Builder->CreateStore(start_val, Alloca);

    llvm::Value* increment = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));
    llvm::BasicBlock *before_header = Builder->GetInsertBlock();

    llvm::BasicBlock *loop_head = llvm::BasicBlock::Create(Context, "header_condition", F);
    llvm::BasicBlock *loop_body = llvm::BasicBlock::Create(Context, "loop_body", F);
    llvm::BasicBlock *loop_after = llvm::BasicBlock::Create(Context, "loop_after", F);

    Builder->CreateBr(loop_head);
    Builder->SetInsertPoint(loop_head);

    llvm::PHINode* phi_variable = Builder->CreatePHI(
        llvm::Type::getInt32Ty(Context), 2, *(for_stmt.loop_var));
    phi_variable->addIncoming(start_val, before_header);

    llvm::Value* condition = for_stmt.true_condition->accept(*this);
    if(!condition){
        std::cerr << "Codegen exiting for statment due to error in final expression\n";
        return nullptr;
    }

    llvm::Value* cur1 = phi_variable;
    llvm::Value* cond1 = Builder->CreateICmpSLT(
        cur1, condition, "loop_conditional_start");
    Builder->CreateCondBr(cond1, loop_body, loop_after);

    Builder->SetInsertPoint(loop_body);
    // std::cout << "loop variable " << *(for_stmt.loop_var) << '\n';
    loops.push(new LoopInfo(loop_after, loop_body, condition,
                *(for_stmt.loop_var), phi_variable));
    // Save old scope
    std::map<std::string, llvm::AllocaInst*> oldNamedValues = NamedValues;
    NamedValues[*(for_stmt.loop_var)] = Alloca;
    // Generate code for body
    llvm::Value *body_val = for_stmt.loop_body->accept(*this);
    if(!body_val){
        std::cerr << "Codegen exiting for statement due to error in body\n";
        return nullptr;
    }

    llvm::Value* cur2 = phi_variable;
    llvm::Value* next_val = Builder->CreateAdd(cur2, increment, "next_val");
    Builder->CreateStore(next_val, Alloca);

    Builder->CreateBr(loop_head);
    llvm::BasicBlock* loopEndBlock = Builder->GetInsertBlock();
    phi_variable->addIncoming(next_val, loopEndBlock);

    Builder->SetInsertPoint(loop_after);

    NamedValues = oldNamedValues;
    loops.pop();

    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));

    // std::cout << "Codegen coming out of for statement\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(MethodCallStatement& mc_stmt) {
    // std::cout << "Codegen visiting method call statement\n";
    llvm::Value* v = mc_stmt.method_call->accept(*this);
    // std::cout << "Codegen exiting method call statement\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(ReturnStatement& ret_stmt) {
    // std::cout << "Codegen visiting return statement\n";
    llvm::Function* F = Builder->GetInsertBlock()->getParent();
    llvm::Type* Ty = F->getReturnType();
    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));

    if(ret_stmt.return_expr == nullptr){
        if(!Ty->isVoidTy()) {
            reportError("Void return statement in non-void function");
            return nullptr;
        }
        Builder->CreateRetVoid();
        // std::cout << "Codegen coming out of void return statement\n";
        return v;
    }

    if(Ty->isVoidTy()) {
        reportError("Non-void return statement in void function");
        return nullptr;
    }
    v = ret_stmt.return_expr->accept(*this);

    unsigned int v_size = v->getType()->getPrimitiveSizeInBits();
    if (v_size == 0)
        v_size = v->getType()->getContainedType(0)->getPrimitiveSizeInBits();

    if(Ty->getPrimitiveSizeInBits() != v_size) {
        std::cout << "somethign went wrong\n";
        return nullptr;
    }

    if(!v){
        std::cerr << "Codegen exiting return statement due to error in "
                  << "return expression\n";
        return nullptr;
    }

    Builder->CreateRet(v);
    // std::cout << "Codegen coming out of non-void return statement\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(BlockStatement& blk_stmt) {
    // std::cout << "Codegen visiting block statement\n";
    std::map<std::string, llvm::AllocaInst*> oldNamedValues = NamedValues;

    llvm::Value* v = blk_stmt.block->accept(*this);
    if(!v){
        std::cerr << "Codegen exiting block statement due to error in block\n";
        return nullptr;
    }

    NamedValues = oldNamedValues;
    // std::cout << "Codegen coming out of block statement\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(BreakStatement& brk_stmt) {
    // std::cout << "Codegen visiting break statement\n";
    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));
    if(loops.empty()) {
        reportError("Found break outside loop");
        return nullptr;
    }
    LoopInfo *currentLoop = loops.top();
    Builder->CreateBr(currentLoop->after);
    // llvm::Function* F = Builder->GetInsertBlock()->getParent();
    // llvm::BasicBlock* newBlock = llvm::BasicBlock::Create(
    //     Context, "new_block", F);
    // Builder->SetInsertPoint(newBlock);
    // std::cout << "Codegen exiting break statement\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(ContinueStatement& cont_stmt) {
    // std::cout << "Codegen visiting continue statement\n";

    llvm::Value* v = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));
    if(loops.empty()) {
        reportError("Found continue outside loop");
        return nullptr;
    }
    LoopInfo* currentLoop = loops.top();

    std::string var = currentLoop->loop_var;
    // std::cout << "loop variable" << var << '\n';
    llvm::AllocaInst* alloca = NamedValues[var];

    llvm::Value* step_val = llvm::ConstantInt::get(Context, llvm::APInt(32, 1));
    llvm::Value* current = Builder->CreateLoad(alloca, var);
    llvm::Value* next_val = Builder->CreateAdd(current, step_val, "next_val");
    Builder->CreateStore(next_val, alloca);

    llvm::Value* condition = Builder->CreateICmpULE(
        next_val, currentLoop->condition, "loop_conditional_start");
    // llvm::BasicBlock* loopEndBlock = Builder->GetInsertBlock();
    Builder->CreateCondBr(condition, currentLoop->body, currentLoop->after);

    // std::cout << "Codegen exiting continue statement\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(Location& loc) {
    // std::cout << "Codegen visiting location\n";

    llvm::Value* v = NamedValues[*(loc.id)];
    if(!v) v = Module->getNamedGlobal(*(loc.id));
    if(!v) {
        reportError("Could not find variable name: " + *(loc.id));
        std::cerr << "Codegen exiting location due to unknown variable name\n";
        return nullptr;
    }

    if(loc.addrs->size() == 0) {
        // std::cout << "Codegen coming out of a non-array variable location\n";
        return v;
    }

    std::vector<llvm::Value*> idx_list({Builder->getInt32(0)});
    for(Expression* id_expr : *(loc.addrs)) {
        llvm::Value* id_val = id_expr->accept(*this);

        if(!id_val){
            std::cerr << "Codegen exiting location with error "
                      << "due to an invalid index";
            return nullptr;
        }

        idx_list.push_back(id_val);
    }
    v = Builder->CreateGEP(v, idx_list, *(loc.id) + "_index");
    // std::cout << "Codegen exiting location\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(MethodCall& mcall) {
    // std::cout << "Codegen visiting method call\n";
    // Ger reference to function
    llvm::Function* callee = Module->getFunction(*(mcall.method_name));
    // std::cout << *(mcall.method_name);
    if(!callee) {
        reportError("Unknown function name: " + *(mcall.method_name));
        std::cerr << "Codegen exiting method call with error due to "
                  << "function name\n";
        return nullptr;
    }
    // Check if the correct number of parameters have been passed
    if(callee->arg_size() != mcall.expr_args->size()) {
        reportError(
            "Incorrect number of parameters for method call " +
            *(mcall.method_name)+ ". Expected " +
            std::to_string(callee->arg_size())+ ", but got " +
            std::to_string(mcall.expr_args->size())
        );
        std::cerr << "Codegen exiting method call due to "
                  << "incorrect number of parameters\n";
        return nullptr;
    }

    std::vector<llvm::Value*> call_arguments;
    for(Expression* expr_arg : *(mcall.expr_args)) {
        llvm::Value* val = expr_arg->accept(*this);
        if(!val){
            std::cerr << "Codegen exiting method call with invalid argument\n";
            return nullptr;
        }
        call_arguments.push_back(val);
    }
    llvm::Value* v = Builder->CreateCall(callee, call_arguments);
    return v;
}

llvm::Value* CodegenVisitor::visit(Callout& callout) {
    // std::cout << "Codegen visiting callout with" + *(callout.method_name) +"\n";

    std::vector<llvm::Type*> arg_types;
    std::vector<llvm::Value*> args;

    // Generate code for each argument
    for (Expression* arg_exp : *(callout.expr_args)) {
        llvm::Value* arg_val = arg_exp->accept(*this);
        if(!arg_val) {
            std::cerr << "Codgen exiting due to error with callout argument\n";
            return nullptr;
        }
        args.push_back(arg_val);
        arg_types.push_back(arg_val->getType());
    }

    // Generate code for function execution
    llvm::ArrayRef<llvm::Type*> ref_types(arg_types);
    llvm::ArrayRef<llvm::Value*> ref_args(args);
    llvm::FunctionType* FType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(Context), ref_types, false);
    llvm::Constant* func = Module->getOrInsertFunction(
        *(callout.method_name), FType);
    if(!func) {
        reportError("Could not find inbuilt function with name " + *(callout.method_name));
        std::cerr << "Codegen exiting because of unknown inbuilt function\n";
        return nullptr;
    }
    llvm::Value* v = Builder->CreateCall(func, ref_args);
    // std::cout << "Codegen exiting callout noramlly\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(LocationExpr& loc_expr) {
    // std::cout << "Codegen visiting location expression\n";
    llvm::Value* v = loc_expr.location->accept(*this);
    if(!v) {
        std::cerr << "Codegen exiting with error due to invalid location expression\n";
        return nullptr;
    }
    // if(loc_expr.location->addrs->size() == 0) {
    //     // std::cout << "Codegen exiting simple location expression normally\n";
    //     return v;
    // }
    v = Builder->CreateLoad(v);
    // std::cout << "Codegen exiting array location normally\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(LiteralExpr& lit_expr) {
    // std::cout << "Codegen visiting literal expression\n";
    Literal* lit = lit_expr.literal;

    llvm::Value* v = lit->accept(*this);
    if(!v) {
        reportError("Error with literal in literal expression\n");
        std::cerr << "Codegen exiting literal expression due to error "
                  << "with the underlying literal\n";
        return nullptr;
    }
    return v;
}

llvm::Value* CodegenVisitor::visit(BinaryExpr& binexp) {
    // std::cout << "Codegen visiting binary expression\n";
    llvm::Value* left = binexp.left->accept(*this);
    if(!left) {
        std::cerr << "Codegen exiting with error in left expression\n";
        return nullptr;
    }
    llvm::Value* right = binexp.right->accept(*this);
    if(!right) {
        std::cerr << "Codegen exiting with error in right expression\n";
        return nullptr;
    }

    llvm::Value* v = nullptr;
    if (binexp.binOp == BinaryOp::ADD) {
        v = Builder->CreateAdd(left, right, "addtmp");
    } else if (binexp.binOp == BinaryOp::SUB) {
        v = Builder->CreateSub(left, right, "subtmp");
    } else if (binexp.binOp == BinaryOp::MUL) {
        v = Builder->CreateMul(left, right, "multmp");
    } else if (binexp.binOp == BinaryOp::DIV) {
        v = Builder->CreateSDiv(left, right, "divtmp");
    } else if (binexp.binOp == BinaryOp::MOD) {
        v = Builder->CreateSRem(left, right, "modtmp");
    } else if (binexp.binOp == BinaryOp::LT) {
        v = Builder->CreateICmpSLT(left, right, "ltcmptmp");
    } else if (binexp.binOp == BinaryOp::GT) {
        v = Builder->CreateICmpSGT(left, right, "gtcmptmp");
    } else if (binexp.binOp == BinaryOp::LE) {
        v = Builder->CreateICmpSLE(left, right, "lecmptmp");
    } else if (binexp.binOp == BinaryOp::GE) {
        v = Builder->CreateICmpSGE(left, right, "gecmptmp");
    } else if (binexp.binOp == BinaryOp::ISEQ) {
        v = Builder->CreateICmpEQ(left, right, "equalcmptmp");
    } else if (binexp.binOp == BinaryOp::NEQ) {
        v = Builder->CreateICmpNE(left, right, "notequalcmptmp");
    } else if (binexp.binOp == BinaryOp::AND) {
        v = Builder->CreateAnd(left, right, "andtmp");
    } else if (binexp.binOp == BinaryOp::OR) {
        v = Builder->CreateOr(left, right, "ortmp");
    }
    // std::cout << "Codegen exiting binary expression\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(UnaryExpr& unexp) {
    // std::cout << "Codegen visiting unary expression\n";
    llvm::Value* v = unexp.expr->accept(*this);
    if(!v) {
        std::cerr << "Codegen exiting due to error in unary expression\n";
        return v;
    }

    if (unexp.unOp == UnaryOp::SUB) {
        v = Builder->CreateNeg(v, "negtmp");
    } else if (unexp.unOp == UnaryOp::EXCL) {
        v = Builder->CreateNot(v, "nottmp");
    }

    // std::cout << "Codegen exiting unary expression\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(MethodCallExpr& mcexpr) {
    // std::cout << "Codegen visiting method call expression\n";
    llvm::Function* callee = Module->getFunction(*(mcexpr.call->method_name));
    llvm::Type* callee_ty = callee->getReturnType();
    if(callee_ty->isVoidTy()) {
        reportError("Called function " + *(mcexpr.call->method_name) +
                    " in method call expression does not return " +
                    "a non-void value");
        return nullptr;
    }
    llvm::Value* v = mcexpr.call->accept(*this);
    if(!v) {
        std::cerr << "Codegen exiting method call expression due "
                  << "to invalid method call\n";
        return nullptr;
    }
    // std::cout << "Codegen exiting call expression normally\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(IntegerLiteral& intlit) {
    // std::cout << "Codegen visiting integer literal\n";
    llvm::Value* v = llvm::ConstantInt::get(
        Context, llvm::APInt(32, static_cast<uint64_t>(intlit.intval), true)
    );
    // std::cout << "Codegen exiting integer literal\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(BooleanLiteral& boolit) {
    // std::cout << "Codegen visiting boolean literal\n";
    llvm::Value* v = llvm::ConstantInt::get(
        Context, llvm::APInt(1, boolit.boolval));
    // std::cout << "Codegen exiting boolean literal\n";
    return v;
}

llvm::Value* CodegenVisitor::visit(StringLiteral& strlit) {
    // std::cout << "Codegen visiting string literal\n";
    llvm::Value* v = Builder->CreateGlobalStringPtr(*(strlit.strval));
    // std::cout << "Codegen exiting string literal\n";
    return v;
}

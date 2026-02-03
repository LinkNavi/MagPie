#ifndef CODEGEN_H
#define CODEGEN_H

// ------------------------------------------------------------
// codegen.h - LLVM IR code generator for ScriptLang
//
// Takes a fully-typed, analyzed AST and emits LLVM IR.
// Supports both JIT (for hot-reload) and AOT (for shipping).
// ------------------------------------------------------------

#include "ast.h"
#include "types.h"
#include "symbol_table.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Constants.h>

#include <memory>
#include <unordered_map>
#include <string>

namespace scriptlang {

// ============================================================
// LLVMCodeGen - main code generator
// ============================================================
class LLVMCodeGen {
public:
    LLVMCodeGen(TypeContext& types, SymbolTable& symbols);

    /// Generate LLVM IR for an entire program.
    std::unique_ptr<llvm::Module> generate(const Program& program);

    /// Access the LLVM context
    llvm::LLVMContext& context() { return context_; }

    /// Check if errors occurred during generation
    bool hasErrors() const { return hasError_; }

private:
    // --------------------------------------------------------
    // Top-level generation
    // --------------------------------------------------------
    void generateStatement(const Statement* stmt);
    void generateClassDecl(const ClassDecl* cls);
    void generateStructDecl(const StructDecl* strct);
    void generateFunctionDecl(const FunctionDecl* func);
    void generateVarDecl(const VarDecl* var);
    void generateConstDecl(const ConstDecl* constDecl);
    void generateEnumDecl(const EnumDecl* enumDecl);
    void generateIncludeDecl(const IncludeDecl* inc);

    // --------------------------------------------------------
    // Function body generation
    // --------------------------------------------------------
    void generateFunctionBody(const FunctionDecl* func, llvm::Function* llvmFunc);
    void generateBlockStatement(const BlockStatement* block);
    void generateIfStatement(const IfStatement* stmt);
    void generateWhileStatement(const WhileStatement* stmt);
    void generateForStatement(const ForStatement* stmt);
    void generateForInStatement(const ForInStatement* stmt);
    void generateReturnStatement(const ReturnStatement* stmt);
    void generateExprStatement(const ExprStatement* stmt);
    void generateBreakStatement(const BreakStatement* stmt);
    void generateContinueStatement(const ContinueStatement* stmt);
    void generateSwitchStatement(const SwitchStatement* stmt);

    // --------------------------------------------------------
    // Expression generation (returns llvm::Value*)
    // --------------------------------------------------------
    llvm::Value* generateExpression(const Expression* expr);
    llvm::Value* generateBinaryExpr(const BinaryExpr* op);
    llvm::Value* generateUnaryExpr(const UnaryExpr* op);
    llvm::Value* generateAssignExpr(const AssignExpr* assign);
    llvm::Value* generateCallExpr(const CallExpr* call);
    llvm::Value* generateMemberExpr(const MemberExpr* member);
    llvm::Value* generateOptionalMemberExpr(const OptionalMemberExpr* member);
    llvm::Value* generateIndexExpr(const IndexExpr* index);
    llvm::Value* generateIdentifierExpr(const IdentifierExpr* id);
    llvm::Value* generateLiteralExpr(const LiteralExpr* lit);
    llvm::Value* generateNewExpr(const NewExpr* newExpr);
    llvm::Value* generateArrayLiteralExpr(const ArrayLiteralExpr* arr);
    llvm::Value* generateNullCoalesceExpr(const NullCoalesceExpr* nc);
    llvm::Value* generateTernaryExpr(const TernaryExpr* tern);
    llvm::Value* generateLambdaExpr(const LambdaExpr* lambda);
    llvm::Value* generateThisExpr(const ThisExpr* thisExpr);
    llvm::Value* generateSuperExpr(const SuperExpr* superExpr);
    llvm::Value* generateSpreadExpr(const SpreadExpr* spread);
    llvm::Value* generateRangeExpr(const RangeExpr* range);
    llvm::Value* generateInterpolatedStringExpr(const InterpolatedStringExpr* str);

    // --------------------------------------------------------
    // Type mapping (ScriptLang Type -> LLVM Type)
    // --------------------------------------------------------
    llvm::Type* toLLVMType(TypePtr type);

    // --------------------------------------------------------
    // Helpers
    // --------------------------------------------------------
    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* func,
                                             const std::string& varName,
                                             llvm::Type* type);
    llvm::Value* loadIfNeeded(llvm::Value* val);
    void declareEngineFunctions();
    llvm::Function* getOrDeclareEngineFunction(const std::string& name);
    void error(const std::string& msg);

    // --------------------------------------------------------
    // State
    // --------------------------------------------------------
    TypeContext&     types_;
    SymbolTable&     symbols_;
    
    llvm::LLVMContext              context_;
    std::unique_ptr<llvm::Module>  module_;
    llvm::IRBuilder<>              builder_;

    std::unordered_map<std::string, llvm::Function*> functions_;
    std::unordered_map<std::string, llvm::AllocaInst*> namedValues_;

    llvm::Function* currentFunction_ = nullptr;
    TypePtr currentReturnType_;

    llvm::BasicBlock* currentLoopExit_ = nullptr;
    llvm::BasicBlock* currentLoopContinue_ = nullptr;

    bool hasError_ = false;
};

// ============================================================
// Implementation
// ============================================================

inline LLVMCodeGen::LLVMCodeGen(TypeContext& types, SymbolTable& symbols)
    : types_(types), symbols_(symbols), builder_(context_) {}

inline std::unique_ptr<llvm::Module> LLVMCodeGen::generate(const Program& program) {
    module_ = std::make_unique<llvm::Module>("scriptlang_module", context_);
    
    // Declare engine functions (Print, Log, etc.)
    declareEngineFunctions();
    
    // Generate all statements
    for (const auto& stmt : program.statements) {
        generateStatement(stmt.get());
    }
    
    // Verify module
    std::string error;
    llvm::raw_string_ostream errorStream(error);
    if (llvm::verifyModule(*module_, &errorStream)) {
        llvm::errs() << "Module verification failed:\n" << error << "\n";
        hasError_ = true;
        return nullptr;
    }
    
    return hasError_ ? nullptr : std::move(module_);
}

inline void LLVMCodeGen::generateStatement(const Statement* stmt) {
    if (auto* fn = dynamic_cast<const FunctionDecl*>(stmt)) {
        generateFunctionDecl(fn);
    } else if (auto* cls = dynamic_cast<const ClassDecl*>(stmt)) {
        generateClassDecl(cls);
    } else if (auto* strct = dynamic_cast<const StructDecl*>(stmt)) {
        generateStructDecl(strct);
    } else if (auto* var = dynamic_cast<const VarDecl*>(stmt)) {
        generateVarDecl(var);
    } else if (auto* constDecl = dynamic_cast<const ConstDecl*>(stmt)) {
        generateConstDecl(constDecl);
    } else if (auto* enumDecl = dynamic_cast<const EnumDecl*>(stmt)) {
        generateEnumDecl(enumDecl);
    } else if (auto* inc = dynamic_cast<const IncludeDecl*>(stmt)) {
        generateIncludeDecl(inc);
    } else if (auto* ret = dynamic_cast<const ReturnStatement*>(stmt)) {
        generateReturnStatement(ret);
    } else if (auto* ifStmt = dynamic_cast<const IfStatement*>(stmt)) {
        generateIfStatement(ifStmt);
    } else if (auto* whileStmt = dynamic_cast<const WhileStatement*>(stmt)) {
        generateWhileStatement(whileStmt);
    } else if (auto* forStmt = dynamic_cast<const ForStatement*>(stmt)) {
        generateForStatement(forStmt);
    } else if (auto* forIn = dynamic_cast<const ForInStatement*>(stmt)) {
        generateForInStatement(forIn);
    } else if (auto* block = dynamic_cast<const BlockStatement*>(stmt)) {
        generateBlockStatement(block);
    } else if (auto* exprStmt = dynamic_cast<const ExprStatement*>(stmt)) {
        generateExprStatement(exprStmt);
    } else if (auto* breakStmt = dynamic_cast<const BreakStatement*>(stmt)) {
        generateBreakStatement(breakStmt);
    } else if (auto* continueStmt = dynamic_cast<const ContinueStatement*>(stmt)) {
        generateContinueStatement(continueStmt);
    } else if (auto* switchStmt = dynamic_cast<const SwitchStatement*>(stmt)) {
        generateSwitchStatement(switchStmt);
    }
}

inline void LLVMCodeGen::generateFunctionDecl(const FunctionDecl* func) {
    // Build function type
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : func->params) {
        // Look up type from symbol table or use type annotation
        TypePtr paramType = types_.Int32(); // TODO: resolve from param.typeAnnot
        paramTypes.push_back(toLLVMType(paramType));
    }
    
    llvm::Type* retType = func->returnType 
        ? toLLVMType(types_.Int32()) // TODO: resolve from func->returnType
        : builder_.getVoidTy();
    
    llvm::FunctionType* funcType = llvm::FunctionType::get(retType, paramTypes, false);
    
    // Create function
    llvm::Function* llvmFunc = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        func->name,
        module_.get()
    );
    
    functions_[func->name] = llvmFunc;
    
    // Generate body
    if (func->body) {
        generateFunctionBody(func, llvmFunc);
    }
}

inline void LLVMCodeGen::generateFunctionBody(const FunctionDecl* func, llvm::Function* llvmFunc) {
    // Create entry block
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context_, "entry", llvmFunc);
    builder_.SetInsertPoint(entry);
    
    // Store old state
    auto oldFunction = currentFunction_;
    auto oldNamedValues = namedValues_;
    
    currentFunction_ = llvmFunc;
    namedValues_.clear();
    
    // Allocate parameters
    auto argIt = llvmFunc->arg_begin();
    for (size_t i = 0; i < func->params.size(); ++i, ++argIt) {
        const auto& param = func->params[i];
        argIt->setName(param.name);
        
        llvm::AllocaInst* alloca = createEntryBlockAlloca(
            llvmFunc, param.name, argIt->getType()
        );
        builder_.CreateStore(argIt, alloca);
        namedValues_[param.name] = alloca;
    }
    
    // Generate body
    if (auto* block = dynamic_cast<const BlockStatement*>(func->body.get())) {
        generateBlockStatement(block);
    } else {
        generateStatement(func->body.get());
    }
    
    // Add return if missing (only if block doesn't already have terminator)
    if (!builder_.GetInsertBlock()->getTerminator()) {
        if (llvmFunc->getReturnType()->isVoidTy()) {
            builder_.CreateRetVoid();
        } else {
            // For non-void functions, this is an error but we'll return zero to keep IR valid
            error("Function '" + func->name + "' missing return statement on some path");
            builder_.CreateRet(llvm::Constant::getNullValue(llvmFunc->getReturnType()));
        }
    }
    
    // Verify function
    std::string errMsg;
    llvm::raw_string_ostream errStream(errMsg);
    if (llvm::verifyFunction(*llvmFunc, &errStream)) {
        llvm::errs() << "Function verification failed for '" << func->name << "':\n" 
                     << errMsg << "\n";
        hasError_ = true;
    }
    
    // Restore state
    currentFunction_ = oldFunction;
    namedValues_ = std::move(oldNamedValues);
}

inline void LLVMCodeGen::generateBlockStatement(const BlockStatement* block) {
    for (const auto& stmt : block->statements) {
        generateStatement(stmt.get());
        
        // Stop if we hit a terminator
        if (builder_.GetInsertBlock()->getTerminator()) {
            break;
        }
    }
}

inline void LLVMCodeGen::generateReturnStatement(const ReturnStatement* stmt) {
    if (stmt->value) {
        llvm::Value* retVal = generateExpression(stmt->value.get());
        builder_.CreateRet(retVal);
    } else {
        builder_.CreateRetVoid();
    }
}

inline void LLVMCodeGen::generateIfStatement(const IfStatement* stmt) {
    llvm::Value* cond = generateExpression(stmt->condition.get());
    
    llvm::Function* func = builder_.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context_, "if.then", func);
    llvm::BasicBlock* elseBB = stmt->elseBranch 
        ? llvm::BasicBlock::Create(context_, "if.else", func) 
        : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "if.merge", func);
    
    builder_.CreateCondBr(cond, thenBB, elseBB ? elseBB : mergeBB);
    
    // Then branch
    builder_.SetInsertPoint(thenBB);
    generateStatement(stmt->thenBranch.get());
    bool thenHasTerminator = builder_.GetInsertBlock()->getTerminator() != nullptr;
    if (!thenHasTerminator) {
        builder_.CreateBr(mergeBB);
    }
    
    // Else branch
    bool elseHasTerminator = false;
    if (elseBB) {
        builder_.SetInsertPoint(elseBB);
        generateStatement(stmt->elseBranch.get());
        elseHasTerminator = builder_.GetInsertBlock()->getTerminator() != nullptr;
        if (!elseHasTerminator) {
            builder_.CreateBr(mergeBB);
        }
    }
    
    // Merge block - only add if at least one branch can reach it
    if (!thenHasTerminator || !elseHasTerminator || !elseBB) {
        builder_.SetInsertPoint(mergeBB);
    } else {
        // Both branches have terminators - merge is unreachable
        // Don't set insert point, just delete the unused merge block
        mergeBB->eraseFromParent();
    }
}

inline void LLVMCodeGen::generateWhileStatement(const WhileStatement* stmt) {
    llvm::Function* func = builder_.GetInsertBlock()->getParent();
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context_, "while.cond", func);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context_, "while.body", func);
    llvm::BasicBlock* exitBB = llvm::BasicBlock::Create(context_, "while.exit", func);
    
    // Save loop context
    auto oldExit = currentLoopExit_;
    auto oldContinue = currentLoopContinue_;
    currentLoopExit_ = exitBB;
    currentLoopContinue_ = condBB;
    
    builder_.CreateBr(condBB);
    
    // Condition
    builder_.SetInsertPoint(condBB);
    llvm::Value* cond = generateExpression(stmt->condition.get());
    builder_.CreateCondBr(cond, bodyBB, exitBB);
    
    // Body
    builder_.SetInsertPoint(bodyBB);
    generateStatement(stmt->body.get());
    if (!builder_.GetInsertBlock()->getTerminator()) {
        builder_.CreateBr(condBB);
    }
    
    // Exit
    builder_.SetInsertPoint(exitBB);
    
    // Restore loop context
    currentLoopExit_ = oldExit;
    currentLoopContinue_ = oldContinue;
}

inline void LLVMCodeGen::generateExprStatement(const ExprStatement* stmt) {
    generateExpression(stmt->expr.get());
}

inline void LLVMCodeGen::generateBreakStatement(const BreakStatement* stmt) {
    if (currentLoopExit_) {
        builder_.CreateBr(currentLoopExit_);
    } else {
        error("break outside loop");
    }
}

inline void LLVMCodeGen::generateContinueStatement(const ContinueStatement* stmt) {
    if (currentLoopContinue_) {
        builder_.CreateBr(currentLoopContinue_);
    } else {
        error("continue outside loop");
    }
}

inline llvm::Value* LLVMCodeGen::generateExpression(const Expression* expr) {
    if (auto* lit = dynamic_cast<const LiteralExpr*>(expr)) {
        return generateLiteralExpr(lit);
    } else if (auto* id = dynamic_cast<const IdentifierExpr*>(expr)) {
        return generateIdentifierExpr(id);
    } else if (auto* bin = dynamic_cast<const BinaryExpr*>(expr)) {
        return generateBinaryExpr(bin);
    } else if (auto* unary = dynamic_cast<const UnaryExpr*>(expr)) {
        return generateUnaryExpr(unary);
    } else if (auto* assign = dynamic_cast<const AssignExpr*>(expr)) {
        return generateAssignExpr(assign);
    } else if (auto* call = dynamic_cast<const CallExpr*>(expr)) {
        return generateCallExpr(call);
    } else if (auto* member = dynamic_cast<const MemberExpr*>(expr)) {
        return generateMemberExpr(member);
    } else if (auto* index = dynamic_cast<const IndexExpr*>(expr)) {
        return generateIndexExpr(index);
    }
    
    error("Unsupported expression type");
    return nullptr;
}

inline llvm::Value* LLVMCodeGen::generateLiteralExpr(const LiteralExpr* lit) {
    switch (lit->kind) {
        case LiteralKind::Integer:
        case LiteralKind::HexInteger:
        case LiteralKind::BinaryInteger: {
            int64_t val = std::stoll(lit->value, nullptr, 0);
            return llvm::ConstantInt::get(builder_.getInt32Ty(), val);
        }
        case LiteralKind::Float:
        case LiteralKind::Scientific: {
            double val = std::stod(lit->value);
            return llvm::ConstantFP::get(builder_.getDoubleTy(), val);
        }
        case LiteralKind::True:
            return llvm::ConstantInt::getTrue(context_);
        case LiteralKind::False:
            return llvm::ConstantInt::getFalse(context_);
        case LiteralKind::Null:
            return llvm::ConstantPointerNull::get(builder_.getPtrTy());
        default:
            error("Unsupported literal kind");
            return nullptr;
    }
}

inline llvm::Value* LLVMCodeGen::generateIdentifierExpr(const IdentifierExpr* id) {
    auto it = namedValues_.find(id->name);
    if (it != namedValues_.end()) {
        return builder_.CreateLoad(it->second->getAllocatedType(), it->second, id->name);
    }
    
    error("Undefined variable: " + id->name);
    return nullptr;
}

inline llvm::Value* LLVMCodeGen::generateBinaryExpr(const BinaryExpr* expr) {
    llvm::Value* left = generateExpression(expr->left.get());
    llvm::Value* right = generateExpression(expr->right.get());
    
    if (!left || !right) return nullptr;
    
    switch (expr->op) {
        case BinaryOp::Add:
            return builder_.CreateAdd(left, right, "add");
        case BinaryOp::Sub:
            return builder_.CreateSub(left, right, "sub");
        case BinaryOp::Mul:
            return builder_.CreateMul(left, right, "mul");
        case BinaryOp::Div:
            return builder_.CreateSDiv(left, right, "div");
        case BinaryOp::Mod:
            return builder_.CreateSRem(left, right, "mod");
        case BinaryOp::Equal:
            return builder_.CreateICmpEQ(left, right, "eq");
        case BinaryOp::NotEqual:
            return builder_.CreateICmpNE(left, right, "ne");
        case BinaryOp::Less:
            return builder_.CreateICmpSLT(left, right, "lt");
        case BinaryOp::LessEqual:
            return builder_.CreateICmpSLE(left, right, "le");
        case BinaryOp::Greater:
            return builder_.CreateICmpSGT(left, right, "gt");
        case BinaryOp::GreaterEqual:
            return builder_.CreateICmpSGE(left, right, "ge");
        case BinaryOp::And:
            return builder_.CreateAnd(left, right, "and");
        case BinaryOp::Or:
            return builder_.CreateOr(left, right, "or");
        default:
            error("Unsupported binary operator");
            return nullptr;
    }
}

inline llvm::Value* LLVMCodeGen::generateUnaryExpr(const UnaryExpr* expr) {
    llvm::Value* operand = generateExpression(expr->operand.get());
    if (!operand) return nullptr;
    
    switch (expr->op) {
        case UnaryOp::Negate:
            return builder_.CreateNeg(operand, "neg");
        case UnaryOp::Not:
            return builder_.CreateNot(operand, "not");
        default:
            error("Unsupported unary operator");
            return nullptr;
    }
}

inline llvm::Value* LLVMCodeGen::generateAssignExpr(const AssignExpr* expr) {
    // Get target (must be lvalue)
    auto* id = dynamic_cast<const IdentifierExpr*>(expr->target.get());
    if (!id) {
        error("Assignment target must be identifier");
        return nullptr;
    }
    
    auto it = namedValues_.find(id->name);
    if (it == namedValues_.end()) {
        error("Undefined variable: " + id->name);
        return nullptr;
    }
    
    llvm::Value* value = generateExpression(expr->value.get());
    if (!value) return nullptr;
    
    // Handle compound assignment
    if (expr->op != AssignOp::Assign) {
        llvm::Value* current = builder_.CreateLoad(
            it->second->getAllocatedType(), it->second, id->name
        );
        
        switch (expr->op) {
            case AssignOp::PlusAssign:
                value = builder_.CreateAdd(current, value, "add");
                break;
            case AssignOp::MinusAssign:
                value = builder_.CreateSub(current, value, "sub");
                break;
            case AssignOp::StarAssign:
                value = builder_.CreateMul(current, value, "mul");
                break;
            case AssignOp::SlashAssign:
                value = builder_.CreateSDiv(current, value, "div");
                break;
            default:
                break;
        }
    }
    
    builder_.CreateStore(value, it->second);
    return value;
}

inline llvm::Value* LLVMCodeGen::generateCallExpr(const CallExpr* call) {
    // Get function
    auto* calleeId = dynamic_cast<const IdentifierExpr*>(call->callee.get());
    if (!calleeId) {
        error("Only direct function calls supported for now");
        return nullptr;
    }
    
    llvm::Function* func = module_->getFunction(calleeId->name);
    if (!func) {
        // Try engine function
        func = getOrDeclareEngineFunction(calleeId->name);
    }
    
    if (!func) {
        error("Undefined function: " + calleeId->name);
        return nullptr;
    }
    
    // Generate arguments
    std::vector<llvm::Value*> args;
    for (const auto& arg : call->args) {
        llvm::Value* argVal = generateExpression(arg.get());
        if (!argVal) return nullptr;
        args.push_back(argVal);
    }
    
    return builder_.CreateCall(func, args, "call");
}

inline llvm::Type* LLVMCodeGen::toLLVMType(TypePtr type) {
    if (!type) return builder_.getVoidTy();
    
    switch (type->kind) {
        case TypeKind::Int8:    return builder_.getInt8Ty();
        case TypeKind::Int16:   return builder_.getInt16Ty();
        case TypeKind::Int32:   return builder_.getInt32Ty();
        case TypeKind::Int64:   return builder_.getInt64Ty();
        case TypeKind::UInt8:   return builder_.getInt8Ty();
        case TypeKind::UInt16:  return builder_.getInt16Ty();
        case TypeKind::UInt32:  return builder_.getInt32Ty();
        case TypeKind::UInt64:  return builder_.getInt64Ty();
        case TypeKind::Float32: return builder_.getFloatTy();
        case TypeKind::Float64: return builder_.getDoubleTy();
        case TypeKind::Bool:    return builder_.getInt1Ty();
        case TypeKind::Void:    return builder_.getVoidTy();
        case TypeKind::String:  return builder_.getPtrTy(); // char*
        default:
            return builder_.getPtrTy(); // Pointer for complex types
    }
}

inline llvm::AllocaInst* LLVMCodeGen::createEntryBlockAlloca(
    llvm::Function* func, const std::string& varName, llvm::Type* type
) {
    llvm::IRBuilder<> tmpBuilder(&func->getEntryBlock(), func->getEntryBlock().begin());
    return tmpBuilder.CreateAlloca(type, nullptr, varName);
}

inline void LLVMCodeGen::declareEngineFunctions() {
    // Print(string) -> void
    llvm::FunctionType* printType = llvm::FunctionType::get(
        builder_.getVoidTy(),
        {builder_.getPtrTy()},
        false
    );
    llvm::Function::Create(
        printType,
        llvm::Function::ExternalLinkage,
        "Print",
        module_.get()
    );
}

inline llvm::Function* LLVMCodeGen::getOrDeclareEngineFunction(const std::string& name) {
    return module_->getFunction(name);
}

inline void LLVMCodeGen::error(const std::string& msg) {
    llvm::errs() << "CodeGen error: " << msg << "\n";
    hasError_ = true;
}

// Stub implementations for unimplemented functions
inline void LLVMCodeGen::generateClassDecl(const ClassDecl*) { /* TODO */ }
inline void LLVMCodeGen::generateStructDecl(const StructDecl*) { /* TODO */ }
inline void LLVMCodeGen::generateVarDecl(const VarDecl* var) {
    if (!currentFunction_) {
        // Global variable - TODO
        error("Global variables not yet supported");
        return;
    }
    
    // Local variable
    llvm::Type* varType = builder_.getInt32Ty(); // TODO: resolve from var->typeAnnot
    
    // Create alloca
    llvm::AllocaInst* alloca = createEntryBlockAlloca(
        currentFunction_, var->name, varType
    );
    
    // Store initial value if provided
    if (var->initializer) {
        llvm::Value* initVal = generateExpression(var->initializer.get());
        if (initVal) {
            builder_.CreateStore(initVal, alloca);
        }
    } else {
        // Initialize to zero
        builder_.CreateStore(llvm::Constant::getNullValue(varType), alloca);
    }
    
    // Register in symbol table
    namedValues_[var->name] = alloca;
}
inline void LLVMCodeGen::generateConstDecl(const ConstDecl*) { /* TODO */ }
inline void LLVMCodeGen::generateEnumDecl(const EnumDecl*) { /* TODO */ }
inline void LLVMCodeGen::generateIncludeDecl(const IncludeDecl*) { /* Already handled */ }
inline void LLVMCodeGen::generateForStatement(const ForStatement*) { /* TODO */ }
inline void LLVMCodeGen::generateForInStatement(const ForInStatement*) { /* TODO */ }
inline void LLVMCodeGen::generateSwitchStatement(const SwitchStatement*) { /* TODO */ }
inline llvm::Value* LLVMCodeGen::generateMemberExpr(const MemberExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateOptionalMemberExpr(const OptionalMemberExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateIndexExpr(const IndexExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateNewExpr(const NewExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateArrayLiteralExpr(const ArrayLiteralExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateNullCoalesceExpr(const NullCoalesceExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateTernaryExpr(const TernaryExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateLambdaExpr(const LambdaExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateThisExpr(const ThisExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateSuperExpr(const SuperExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateSpreadExpr(const SpreadExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateRangeExpr(const RangeExpr*) { return nullptr; }
inline llvm::Value* LLVMCodeGen::generateInterpolatedStringExpr(const InterpolatedStringExpr*) { return nullptr; }

} // namespace scriptlang

#endif // CODEGEN_H

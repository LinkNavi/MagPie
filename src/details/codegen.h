#ifndef CODEGEN_H
#define CODEGEN_H

#include "ast.h"
#include "symbol_table.h"
#include "types.h"
#include "arena.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace scriptlang {

struct ClassInfo {
  llvm::StructType *structType = nullptr;
  std::unordered_map<std::string, int> fieldIndices;
  std::unordered_map<std::string, llvm::Function *> methods;
  std::vector<std::pair<std::string, llvm::Type *>> fields;
  TypePtr scriptType;
};

class LLVMCodeGen {
public:
  LLVMCodeGen(TypeContext &types, SymbolTable &symbols);
  std::unique_ptr<llvm::Module> generate(const Program &program);
  llvm::LLVMContext &context() { return context_; }
  bool hasErrors() const { return hasError_; }
  const std::vector<std::string> &errors() const { return errors_; }

private:
  void generateStatement(const Statement *stmt);
  void generateClassDecl(const ClassDecl *cls);
  void generateStructDecl(const StructDecl *strct);
  void generateFunctionDecl(const FunctionDecl *func);
llvm::GlobalVariable *frameArena_ = nullptr;
  void generateVarDecl(const VarDecl *var);
  void generateConstDecl(const ConstDecl *constDecl);
  void generateEnumDecl(const EnumDecl *enumDecl);
  void generateIncludeDecl(const IncludeDecl *inc);
  void generateFunctionBody(const FunctionDecl *func, llvm::Function *llvmFunc);
  void generateBlockStatement(const BlockStatement *block);
  void generateIfStatement(const IfStatement *stmt);
  void generateWhileStatement(const WhileStatement *stmt);
  void generateForStatement(const ForStatement *stmt);
  void generateForInStatement(const ForInStatement *stmt);
  void generateReturnStatement(const ReturnStatement *stmt);
  void generateExprStatement(const ExprStatement *stmt);
  void generateBreakStatement(const BreakStatement *stmt);
  void generateContinueStatement(const ContinueStatement *stmt);
  void generateSwitchStatement(const SwitchStatement *stmt);
llvm::Value *intToString(llvm::Value *val);
llvm::Value *generateStringConcat(llvm::Value *left, llvm::Value *right);
  llvm::Value *generateExpression(const Expression *expr);
  llvm::Value *generateBinaryExpr(const BinaryExpr *op);
  llvm::Value *generateUnaryExpr(const UnaryExpr *op);
  llvm::Value *generateAssignExpr(const AssignExpr *assign);
  llvm::Value *generateCallExpr(const CallExpr *call);
  llvm::Value *generateMemberExpr(const MemberExpr *member);
  llvm::Value *generateOptionalMemberExpr(const OptionalMemberExpr *member);
  llvm::Value *generateIndexExpr(const IndexExpr *index);
  llvm::Value *generateIdentifierExpr(const IdentifierExpr *id);
  llvm::Value *generateLiteralExpr(const LiteralExpr *lit);
  llvm::Value *generateNewExpr(const NewExpr *newExpr);
  llvm::Value *generateArrayLiteralExpr(const ArrayLiteralExpr *arr);
  llvm::Value *generateNullCoalesceExpr(const NullCoalesceExpr *nc);
  llvm::Value *generateTernaryExpr(const TernaryExpr *tern);
  llvm::Value *generateLambdaExpr(const LambdaExpr *lambda);
  llvm::Value *generateThisExpr(const ThisExpr *thisExpr);
  llvm::Value *generateSuperExpr(const SuperExpr *superExpr);
  llvm::Value *generateSpreadExpr(const SpreadExpr *spread);
  llvm::Value *generateRangeExpr(const RangeExpr *range);
  llvm::Value *
  generateInterpolatedStringExpr(const InterpolatedStringExpr *str);

  llvm::Type *toLLVMType(TypePtr type);
  llvm::Type *resolveTypeFromAnnotation(const TypeAnnotation *typeAnnot);
  llvm::Type *resolveVarType(const std::string &name,
                             const TypeAnnotation *typeAnnot,
                             const Expression *initializer);

  void generateClassMethod(const ClassDecl *cls, const FunctionDecl *method,
                           ClassInfo &info);
  void generateMethodBody(const FunctionDecl *method,
                          llvm::Function *methodFunc, ClassInfo &info);
  void generateClassConstructor(const ClassDecl *cls, ClassInfo &info);
  void generateGlobalVar(const VarDecl *var);
  void generateGlobalConst(const ConstDecl *constDecl);

  llvm::Function *getOrDeclareMalloc();
  llvm::Function *getOrDeclareFree();
  llvm::Function *getOrDeclareMemcpy();
  llvm::Function *getOrDeclarePrintf();
  llvm::Function *getOrDeclareSnprintf();
  llvm::Function *getOrDeclareStrlen();
  llvm::Function *getOrDeclareStrcat();
  void declareEngineFunctions();
  llvm::Function *getOrDeclareEngineFunction(const std::string &name);

  llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func,
                                           const std::string &varName,
                                           llvm::Type *type);
  llvm::Value *castIfNeeded(llvm::Value *val, llvm::Type *targetType);
  llvm::Value *createStringConstant(const std::string &str);
  void error(const std::string &msg);

  TypeContext &types_;
  SymbolTable &symbols_;
  llvm::LLVMContext context_;
  std::unique_ptr<llvm::Module> module_;
  llvm::IRBuilder<> builder_;

  std::unordered_map<std::string, llvm::Function *> functions_;
  std::unordered_map<std::string, llvm::AllocaInst *> namedValues_;
  std::unordered_map<std::string, llvm::GlobalVariable *> globalValues_;
  std::unordered_map<std::string, ClassInfo> classInfos_;
  std::unordered_map<std::string, int64_t> enumValues_;

  llvm::Function *currentFunction_ = nullptr;
  TypePtr currentReturnType_;
  llvm::Value *currentThisPtr_ = nullptr;
  std::string currentClassName_;
  llvm::BasicBlock *currentLoopExit_ = nullptr;
  llvm::BasicBlock *currentLoopContinue_ = nullptr;
  bool hasError_ = false;
  std::vector<std::string> errors_;
  int lambdaCounter_ = 0;
  int stringConstCounter_ = 0;
};

// ============================================================
// Implementation
// ============================================================

inline LLVMCodeGen::LLVMCodeGen(TypeContext &types, SymbolTable &symbols)
    : types_(types), symbols_(symbols), builder_(context_) {}

inline std::unique_ptr<llvm::Module>
LLVMCodeGen::generate(const Program &program) {
 module_ = std::make_unique<llvm::Module>("scriptlang_module", context_);
  module_->setDataLayout(
      "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128");
  module_->setTargetTriple(llvm::Triple("x86_64-pc-linux-gnu"));
  
  // Arena memory management setup
  frameArena_ = new llvm::GlobalVariable(
      *module_, builder_.getPtrTy(), false,
      llvm::GlobalValue::ExternalLinkage, nullptr, "g_frame_arena");
  
  llvm::FunctionType *arenaAllocTy = llvm::FunctionType::get(
      builder_.getPtrTy(),
      {builder_.getPtrTy(), builder_.getInt64Ty()},
      false);
  llvm::Function::Create(arenaAllocTy, llvm::Function::ExternalLinkage,
      "sl_arena_alloc", module_.get());
  
  declareEngineFunctions();

  // Pass 1: Register classes/structs/enums
  for (const auto &stmt : program.statements) {
    if (auto *cls = dynamic_cast<const ClassDecl *>(stmt.get())) {
      std::vector<llvm::Type *> fieldTypes;
      ClassInfo info;
      info.scriptType = types_.lookupByName(cls->name);
      int idx = 0;
      for (const auto &member : cls->members) {
        if (auto *varDecl = dynamic_cast<const VarDecl *>(member.decl.get())) {
          llvm::Type *ft =
              resolveVarType(varDecl->name, varDecl->typeAnnot.get(),
                             varDecl->initializer.get());
          fieldTypes.push_back(ft);
          info.fieldIndices[varDecl->name] = idx++;
          info.fields.push_back({varDecl->name, ft});
        }
      }
      info.structType =
          llvm::StructType::create(context_, fieldTypes, cls->name);
      classInfos_[cls->name] = std::move(info);
    } else if (auto *strct = dynamic_cast<const StructDecl *>(stmt.get())) {
      std::vector<llvm::Type *> fieldTypes;
      ClassInfo info;
      info.scriptType = types_.lookupByName(strct->name);
      int idx = 0;
      for (const auto &member : strct->members) {
        if (auto *varDecl = dynamic_cast<const VarDecl *>(member.decl.get())) {
          llvm::Type *ft =
              resolveVarType(varDecl->name, varDecl->typeAnnot.get(),
                             varDecl->initializer.get());
          fieldTypes.push_back(ft);
          info.fieldIndices[varDecl->name] = idx++;
          info.fields.push_back({varDecl->name, ft});
        }
      }
      info.structType =
          llvm::StructType::create(context_, fieldTypes, strct->name);
      classInfos_[strct->name] = std::move(info);
    } else if (auto *enumDecl = dynamic_cast<const EnumDecl *>(stmt.get())) {
      int64_t val = 0;
      for (const auto &entry : enumDecl->entries) {
        if (entry.value) {
          if (auto *lit =
                  dynamic_cast<const LiteralExpr *>(entry.value.get())) {
            val = std::stoll(lit->value, nullptr, 0);
          }
        }
        enumValues_[enumDecl->name + "::" + entry.name] = val++;
      }
    }
  }

  // Pass 2: Declare functions
  for (const auto &stmt : program.statements) {
    if (auto *fn = dynamic_cast<const FunctionDecl *>(stmt.get())) {
      std::vector<llvm::Type *> paramTypes;
      for (const auto &param : fn->params) {
        paramTypes.push_back(resolveTypeFromAnnotation(param.typeAnnot.get()));
      }
      llvm::Type *retType =
          fn->returnType ? resolveTypeFromAnnotation(fn->returnType.get())
                         : builder_.getVoidTy();
      llvm::FunctionType *funcType =
          llvm::FunctionType::get(retType, paramTypes, false);
      llvm::Function *llvmFunc = llvm::Function::Create(
          funcType, llvm::Function::ExternalLinkage, fn->name, module_.get());
      functions_[fn->name] = llvmFunc;
    }
  }
  for (const auto &stmt : program.statements) {
    if (auto *var = dynamic_cast<const VarDecl *>(stmt.get())) {
      // Top-level var is a global
      generateGlobalVar(var);
    } else if (auto *constDecl = dynamic_cast<const ConstDecl *>(stmt.get())) {
      // Top-level const is a global
      generateGlobalConst(constDecl);
    }
  }
  // Pass 3: Generate code
  for (const auto &stmt : program.statements) {
    generateStatement(stmt.get());
  }

  std::string errStr;
  llvm::raw_string_ostream errStream(errStr);
  if (llvm::verifyModule(*module_, &errStream)) {
    error("Module verification failed: " + errStr);
    return nullptr;
  }
  return hasError_ ? nullptr : std::move(module_);
}

inline void LLVMCodeGen::generateStatement(const Statement *stmt) {
  if (auto *fn = dynamic_cast<const FunctionDecl *>(stmt))
    generateFunctionDecl(fn);
  else if (auto *cls = dynamic_cast<const ClassDecl *>(stmt))
    generateClassDecl(cls);
  else if (auto *strct = dynamic_cast<const StructDecl *>(stmt))
    generateStructDecl(strct);
  else if (auto *var = dynamic_cast<const VarDecl *>(stmt))
    generateVarDecl(var);
  else if (auto *constDecl = dynamic_cast<const ConstDecl *>(stmt))
    generateConstDecl(constDecl);
  else if (auto *enumDecl = dynamic_cast<const EnumDecl *>(stmt))
    generateEnumDecl(enumDecl);
  else if (auto *inc = dynamic_cast<const IncludeDecl *>(stmt))
    generateIncludeDecl(inc);
  else if (auto *ret = dynamic_cast<const ReturnStatement *>(stmt))
    generateReturnStatement(ret);
  else if (auto *ifStmt = dynamic_cast<const IfStatement *>(stmt))
    generateIfStatement(ifStmt);
  else if (auto *whileStmt = dynamic_cast<const WhileStatement *>(stmt))
    generateWhileStatement(whileStmt);
  else if (auto *forStmt = dynamic_cast<const ForStatement *>(stmt))
    generateForStatement(forStmt);
  else if (auto *forIn = dynamic_cast<const ForInStatement *>(stmt))
    generateForInStatement(forIn);
  else if (auto *block = dynamic_cast<const BlockStatement *>(stmt))
    generateBlockStatement(block);
  else if (auto *exprStmt = dynamic_cast<const ExprStatement *>(stmt))
    generateExprStatement(exprStmt);
  else if (auto *breakStmt = dynamic_cast<const BreakStatement *>(stmt))
    generateBreakStatement(breakStmt);
  else if (auto *continueStmt = dynamic_cast<const ContinueStatement *>(stmt))
    generateContinueStatement(continueStmt);
  else if (auto *switchStmt = dynamic_cast<const SwitchStatement *>(stmt))
    generateSwitchStatement(switchStmt);
}

inline void LLVMCodeGen::generateFunctionDecl(const FunctionDecl *func) {
  llvm::Function *llvmFunc = functions_[func->name];
  if (!llvmFunc) {
    std::vector<llvm::Type *> paramTypes;
    for (const auto &param : func->params)
      paramTypes.push_back(resolveTypeFromAnnotation(param.typeAnnot.get()));
    llvm::Type *retType =
        func->returnType ? resolveTypeFromAnnotation(func->returnType.get())
                         : builder_.getVoidTy();
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(retType, paramTypes, false);
    llvmFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                      func->name, module_.get());
    functions_[func->name] = llvmFunc;
  }
  if (func->body)
    generateFunctionBody(func, llvmFunc);
}

inline void LLVMCodeGen::generateFunctionBody(const FunctionDecl *func,
                                              llvm::Function *llvmFunc) {
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(context_, "entry", llvmFunc);
  builder_.SetInsertPoint(entry);
  auto oldFunction = currentFunction_;
  auto oldNamedValues = namedValues_;
  auto oldReturnType = currentReturnType_;
  currentFunction_ = llvmFunc;
  namedValues_.clear();

  if (func->returnType) {
    currentReturnType_ = types_.lookupByName(func->returnType->name);
    if (!currentReturnType_) {
      if (func->returnType->name == "int32")
        currentReturnType_ = types_.Int32();
      else if (func->returnType->name == "int64")
        currentReturnType_ = types_.Int64();
      else if (func->returnType->name == "float32")
        currentReturnType_ = types_.Float32();
      else if (func->returnType->name == "float64")
        currentReturnType_ = types_.Float64();
      else if (func->returnType->name == "bool")
        currentReturnType_ = types_.Bool();
      else if (func->returnType->name == "string")
        currentReturnType_ = types_.String();
      else
        currentReturnType_ = types_.Void();
    }
  } else
    currentReturnType_ = types_.Void();

  auto argIt = llvmFunc->arg_begin();
  for (size_t i = 0; i < func->params.size(); ++i, ++argIt) {
    argIt->setName(func->params[i].name);
    llvm::AllocaInst *alloca = createEntryBlockAlloca(
        llvmFunc, func->params[i].name, argIt->getType());
    builder_.CreateStore(&*argIt, alloca);
    namedValues_[func->params[i].name] = alloca;
  }

  if (auto *block = dynamic_cast<const BlockStatement *>(func->body.get()))
    generateBlockStatement(block);
  else
    generateStatement(func->body.get());

  if (!builder_.GetInsertBlock()->getTerminator()) {
    if (llvmFunc->getReturnType()->isVoidTy())
      builder_.CreateRetVoid();
    else
      builder_.CreateRet(
          llvm::Constant::getNullValue(llvmFunc->getReturnType()));
  }

  std::string errMsg;
  llvm::raw_string_ostream errStream(errMsg);
  if (llvm::verifyFunction(*llvmFunc, &errStream))
    error("Function '" + func->name + "' verification failed: " + errMsg);

  currentFunction_ = oldFunction;
  namedValues_ = std::move(oldNamedValues);
  currentReturnType_ = oldReturnType;
}

inline void LLVMCodeGen::generateBlockStatement(const BlockStatement *block) {
  for (const auto &stmt : block->statements) {
    generateStatement(stmt.get());
    if (builder_.GetInsertBlock()->getTerminator())
      break;
  }
}

inline void LLVMCodeGen::generateReturnStatement(const ReturnStatement *stmt) {
  if (stmt->value) {
    llvm::Value *retVal = generateExpression(stmt->value.get());
    if (retVal) {
      retVal = castIfNeeded(retVal, currentFunction_->getReturnType());
      builder_.CreateRet(retVal);
    } else
      builder_.CreateRetVoid();
  } else
    builder_.CreateRetVoid();
}

inline void LLVMCodeGen::generateIfStatement(const IfStatement *stmt) {
  llvm::Value *cond = generateExpression(stmt->condition.get());
  if (!cond)
    return;
  if (!cond->getType()->isIntegerTy(1))
    cond = builder_.CreateICmpNE(
        cond, llvm::Constant::getNullValue(cond->getType()), "ifcond");

  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context_, "if.then", func);
  llvm::BasicBlock *elseBB =
      stmt->elseBranch ? llvm::BasicBlock::Create(context_, "if.else", func)
                       : nullptr;
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(context_, "if.merge", func);

  builder_.CreateCondBr(cond, thenBB, elseBB ? elseBB : mergeBB);

  builder_.SetInsertPoint(thenBB);
  generateStatement(stmt->thenBranch.get());
  bool thenTerminated = builder_.GetInsertBlock()->getTerminator() != nullptr;
  if (!thenTerminated)
    builder_.CreateBr(mergeBB);

  bool elseTerminated = false;
  if (elseBB) {
    builder_.SetInsertPoint(elseBB);
    generateStatement(stmt->elseBranch.get());
    elseTerminated = builder_.GetInsertBlock()->getTerminator() != nullptr;
    if (!elseTerminated)
      builder_.CreateBr(mergeBB);
  }

  if (!thenTerminated || !elseTerminated || !elseBB)
    builder_.SetInsertPoint(mergeBB);
  else
    mergeBB->eraseFromParent();
}

inline void LLVMCodeGen::generateWhileStatement(const WhileStatement *stmt) {
  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context_, "while.cond", func);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(context_, "while.body", func);
  llvm::BasicBlock *exitBB =
      llvm::BasicBlock::Create(context_, "while.exit", func);

  auto oldExit = currentLoopExit_;
  auto oldContinue = currentLoopContinue_;
  currentLoopExit_ = exitBB;
  currentLoopContinue_ = condBB;

  builder_.CreateBr(condBB);
  builder_.SetInsertPoint(condBB);
  llvm::Value *cond = generateExpression(stmt->condition.get());
  if (cond && !cond->getType()->isIntegerTy(1))
    cond = builder_.CreateICmpNE(
        cond, llvm::Constant::getNullValue(cond->getType()), "whilecond");
  if (cond)
    builder_.CreateCondBr(cond, bodyBB, exitBB);
  else
    builder_.CreateBr(exitBB);

  builder_.SetInsertPoint(bodyBB);
  generateStatement(stmt->body.get());
  if (!builder_.GetInsertBlock()->getTerminator())
    builder_.CreateBr(condBB);

  builder_.SetInsertPoint(exitBB);
  currentLoopExit_ = oldExit;
  currentLoopContinue_ = oldContinue;
}

inline void LLVMCodeGen::generateForStatement(const ForStatement *stmt) {
  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  if (stmt->init)
    generateStatement(stmt->init.get());

  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context_, "for.cond", func);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(context_, "for.body", func);
  llvm::BasicBlock *incBB = llvm::BasicBlock::Create(context_, "for.inc", func);
  llvm::BasicBlock *exitBB =
      llvm::BasicBlock::Create(context_, "for.exit", func);

  auto oldExit = currentLoopExit_;
  auto oldContinue = currentLoopContinue_;
  currentLoopExit_ = exitBB;
  currentLoopContinue_ = incBB;

  builder_.CreateBr(condBB);
  builder_.SetInsertPoint(condBB);
  if (stmt->condition) {
    llvm::Value *cond = generateExpression(stmt->condition.get());
    if (cond && !cond->getType()->isIntegerTy(1))
      cond = builder_.CreateICmpNE(
          cond, llvm::Constant::getNullValue(cond->getType()), "forcond");
    if (cond)
      builder_.CreateCondBr(cond, bodyBB, exitBB);
    else
      builder_.CreateBr(exitBB);
  } else
    builder_.CreateBr(bodyBB);

  builder_.SetInsertPoint(bodyBB);
  generateStatement(stmt->body.get());
  if (!builder_.GetInsertBlock()->getTerminator())
    builder_.CreateBr(incBB);

  builder_.SetInsertPoint(incBB);
  if (stmt->update)
    generateExpression(stmt->update.get());
  builder_.CreateBr(condBB);

  builder_.SetInsertPoint(exitBB);
  currentLoopExit_ = oldExit;
  currentLoopContinue_ = oldContinue;
}

inline void LLVMCodeGen::generateForInStatement(const ForInStatement *stmt) {
  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::Value *iterable = generateExpression(stmt->iterable.get());
  if (!iterable)
    return;

  llvm::Value *arrPtr = builder_.CreateExtractValue(iterable, 0, "arr.ptr");
  llvm::Value *arrLen = builder_.CreateExtractValue(iterable, 1, "arr.len");

  llvm::AllocaInst *idxAlloca =
      createEntryBlockAlloca(func, stmt->name + ".idx", builder_.getInt64Ty());
  builder_.CreateStore(builder_.getInt64(0), idxAlloca);

  llvm::Type *elemType = builder_.getInt32Ty();
  llvm::AllocaInst *elemAlloca =
      createEntryBlockAlloca(func, stmt->name, elemType);
  namedValues_[stmt->name] = elemAlloca;

  llvm::BasicBlock *condBB =
      llvm::BasicBlock::Create(context_, "forin.cond", func);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(context_, "forin.body", func);
  llvm::BasicBlock *incBB =
      llvm::BasicBlock::Create(context_, "forin.inc", func);
  llvm::BasicBlock *exitBB =
      llvm::BasicBlock::Create(context_, "forin.exit", func);

  auto oldExit = currentLoopExit_;
  auto oldContinue = currentLoopContinue_;
  currentLoopExit_ = exitBB;
  currentLoopContinue_ = incBB;

  builder_.CreateBr(condBB);
  builder_.SetInsertPoint(condBB);
  llvm::Value *idx =
      builder_.CreateLoad(builder_.getInt64Ty(), idxAlloca, "idx");
  llvm::Value *cond = builder_.CreateICmpSLT(idx, arrLen, "forin.cond");
  builder_.CreateCondBr(cond, bodyBB, exitBB);

  builder_.SetInsertPoint(bodyBB);
  llvm::Value *elemPtr = builder_.CreateGEP(elemType, arrPtr, idx, "elem.ptr");
  llvm::Value *elem = builder_.CreateLoad(elemType, elemPtr, "elem");
  builder_.CreateStore(elem, elemAlloca);
  generateStatement(stmt->body.get());
  if (!builder_.GetInsertBlock()->getTerminator())
    builder_.CreateBr(incBB);

  builder_.SetInsertPoint(incBB);
  llvm::Value *nextIdx =
      builder_.CreateAdd(idx, builder_.getInt64(1), "idx.next");
  builder_.CreateStore(nextIdx, idxAlloca);
  builder_.CreateBr(condBB);

  builder_.SetInsertPoint(exitBB);
  currentLoopExit_ = oldExit;
  currentLoopContinue_ = oldContinue;
}

inline void LLVMCodeGen::generateExprStatement(const ExprStatement *stmt) {
  generateExpression(stmt->expr.get());
}
inline void LLVMCodeGen::generateBreakStatement(const BreakStatement *) {
  if (currentLoopExit_)
    builder_.CreateBr(currentLoopExit_);
  else
    error("break outside loop");
}
inline void LLVMCodeGen::generateContinueStatement(const ContinueStatement *) {
  if (currentLoopContinue_)
    builder_.CreateBr(currentLoopContinue_);
  else
    error("continue outside loop");
}

inline void LLVMCodeGen::generateSwitchStatement(const SwitchStatement *stmt) {
  llvm::Value *disc = generateExpression(stmt->discriminant.get());
  if (!disc)
    return;

  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::BasicBlock *exitBB =
      llvm::BasicBlock::Create(context_, "switch.exit", func);
  llvm::BasicBlock *defaultBB = exitBB;

  std::vector<std::pair<llvm::ConstantInt *, llvm::BasicBlock *>> casePairs;
  for (const auto &c : stmt->cases) {
    llvm::BasicBlock *caseBB =
        llvm::BasicBlock::Create(context_, "switch.case", func);
    if (c.value) {
      llvm::Value *caseVal = generateExpression(c.value.get());
      if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(caseVal))
        casePairs.push_back({ci, caseBB});
    } else
      defaultBB = caseBB;
  }

  llvm::SwitchInst *sw =
      builder_.CreateSwitch(disc, defaultBB, casePairs.size());
  for (auto &[val, bb] : casePairs)
    sw->addCase(val, bb);

  auto oldExit = currentLoopExit_;
  currentLoopExit_ = exitBB;

  size_t caseIdx = 0;
  for (const auto &c : stmt->cases) {
    llvm::BasicBlock *caseBB =
        c.value ? casePairs[caseIdx++].second : defaultBB;
    builder_.SetInsertPoint(caseBB);
    for (const auto &s : c.body) {
      generateStatement(s.get());
      if (builder_.GetInsertBlock()->getTerminator())
        break;
    }
    if (!builder_.GetInsertBlock()->getTerminator())
      builder_.CreateBr(exitBB);
  }

  builder_.SetInsertPoint(exitBB);
  currentLoopExit_ = oldExit;
}

inline llvm::Value *LLVMCodeGen::generateExpression(const Expression *expr) {
  if (!expr)
    return nullptr;
  if (auto *lit = dynamic_cast<const LiteralExpr *>(expr))
    return generateLiteralExpr(lit);
  if (auto *id = dynamic_cast<const IdentifierExpr *>(expr))
    return generateIdentifierExpr(id);
  if (auto *bin = dynamic_cast<const BinaryExpr *>(expr))
    return generateBinaryExpr(bin);
  if (auto *unary = dynamic_cast<const UnaryExpr *>(expr))
    return generateUnaryExpr(unary);
  if (auto *assign = dynamic_cast<const AssignExpr *>(expr))
    return generateAssignExpr(assign);
  if (auto *call = dynamic_cast<const CallExpr *>(expr))
    return generateCallExpr(call);
  if (auto *member = dynamic_cast<const MemberExpr *>(expr))
    return generateMemberExpr(member);
  if (auto *optMember = dynamic_cast<const OptionalMemberExpr *>(expr))
    return generateOptionalMemberExpr(optMember);
  if (auto *index = dynamic_cast<const IndexExpr *>(expr))
    return generateIndexExpr(index);
  if (auto *newExpr = dynamic_cast<const NewExpr *>(expr))
    return generateNewExpr(newExpr);
  if (auto *arr = dynamic_cast<const ArrayLiteralExpr *>(expr))
    return generateArrayLiteralExpr(arr);
  if (auto *nc = dynamic_cast<const NullCoalesceExpr *>(expr))
    return generateNullCoalesceExpr(nc);
  if (auto *tern = dynamic_cast<const TernaryExpr *>(expr))
    return generateTernaryExpr(tern);
  if (auto *lambda = dynamic_cast<const LambdaExpr *>(expr))
    return generateLambdaExpr(lambda);
  if (auto *thisE = dynamic_cast<const ThisExpr *>(expr))
    return generateThisExpr(thisE);
  if (auto *superE = dynamic_cast<const SuperExpr *>(expr))
    return generateSuperExpr(superE);
  if (auto *spread = dynamic_cast<const SpreadExpr *>(expr))
    return generateSpreadExpr(spread);
  if (auto *range = dynamic_cast<const RangeExpr *>(expr))
    return generateRangeExpr(range);
  if (auto *interp = dynamic_cast<const InterpolatedStringExpr *>(expr))
    return generateInterpolatedStringExpr(interp);
  error("Unknown expression type");
  return nullptr;
}

inline llvm::Value *LLVMCodeGen::generateLiteralExpr(const LiteralExpr *lit) {
  switch (lit->kind) {
  case LiteralKind::Integer:
    return llvm::ConstantInt::get(builder_.getInt32Ty(),
                                  std::stoll(lit->value, nullptr, 10));
  case LiteralKind::HexInteger:
    return llvm::ConstantInt::get(builder_.getInt32Ty(),
                                  std::stoll(lit->value, nullptr, 16));
  case LiteralKind::BinaryInteger:
    return llvm::ConstantInt::get(builder_.getInt32Ty(),
                                  std::stoll(lit->value.substr(2), nullptr, 2));
  case LiteralKind::Float:
  case LiteralKind::Scientific:
    return llvm::ConstantFP::get(builder_.getDoubleTy(), std::stod(lit->value));
  case LiteralKind::True:
    return llvm::ConstantInt::getTrue(context_);
  case LiteralKind::False:
    return llvm::ConstantInt::getFalse(context_);
  case LiteralKind::Null:
    return llvm::ConstantPointerNull::get(builder_.getPtrTy());
  case LiteralKind::String:
  case LiteralKind::RawString:
    return createStringConstant(lit->value);
  case LiteralKind::Char:
    return llvm::ConstantInt::get(builder_.getInt8Ty(),
                                  lit->value.empty() ? 0 : lit->value[0]);
  }
  return nullptr;
}

inline llvm::Value *
LLVMCodeGen::generateIdentifierExpr(const IdentifierExpr *id) {
  auto it = namedValues_.find(id->name);
  if (it != namedValues_.end())
    return builder_.CreateLoad(it->second->getAllocatedType(), it->second,
                               id->name);

  auto git = globalValues_.find(id->name);
  if (git != globalValues_.end())
    return builder_.CreateLoad(git->second->getValueType(), git->second,
                               id->name);

  auto eit = enumValues_.find(id->name);
  if (eit != enumValues_.end())
    return llvm::ConstantInt::get(builder_.getInt32Ty(), eit->second);

  size_t colonPos = id->name.find("::");
  if (colonPos != std::string::npos) {
    auto qit = enumValues_.find(id->name);
    if (qit != enumValues_.end())
      return llvm::ConstantInt::get(builder_.getInt32Ty(), qit->second);
  }

  if (currentThisPtr_ && !currentClassName_.empty()) {
    auto cit = classInfos_.find(currentClassName_);
    if (cit != classInfos_.end()) {
      auto fit = cit->second.fieldIndices.find(id->name);
      if (fit != cit->second.fieldIndices.end()) {
        llvm::Value *fieldPtr =
            builder_.CreateStructGEP(cit->second.structType, currentThisPtr_,
                                     fit->second, id->name + ".ptr");
        return builder_.CreateLoad(cit->second.fields[fit->second].second,
                                   fieldPtr, id->name);
      }
    }
  }

  error("Undefined variable: " + id->name);
  return nullptr;
}

inline llvm::Value *LLVMCodeGen::generateStringConcat(llvm::Value *left, llvm::Value *right) {
  llvm::Function *strlenFn = getOrDeclareStrlen();
  llvm::Function *mallocFn = getOrDeclareMalloc();
  llvm::Function *memcpyFn = getOrDeclareMemcpy();
  
  // Get lengths
  llvm::Value *len1 = builder_.CreateCall(strlenFn, {left}, "len1");
  llvm::Value *len2 = builder_.CreateCall(strlenFn, {right}, "len2");
  llvm::Value *totalLen = builder_.CreateAdd(len1, len2, "totallen");
  llvm::Value *allocSize = builder_.CreateAdd(totalLen, builder_.getInt64(1), "allocsize"); // +1 for null
  
  // Allocate new buffer
llvm::Function *arenaAllocFn = module_->getFunction("sl_arena_alloc");
if (!arenaAllocFn) {
  llvm::FunctionType *arenaAllocTy = llvm::FunctionType::get(
      builder_.getPtrTy(),
      {builder_.getPtrTy(), builder_.getInt64Ty()},
      false);
  arenaAllocFn = llvm::Function::Create(arenaAllocTy, 
      llvm::Function::ExternalLinkage, "sl_arena_alloc", module_.get());
}
llvm::Value *newStr = builder_.CreateCall(arenaAllocFn, 
    {frameArena_, allocSize});
  // Copy first string
  builder_.CreateCall(memcpyFn, {newStr, left, len1, builder_.getFalse()});
  
  // Copy second string (including null terminator)
  llvm::Value *dst2 = builder_.CreateGEP(builder_.getInt8Ty(), newStr, len1, "dst2");
  llvm::Value *len2Plus1 = builder_.CreateAdd(len2, builder_.getInt64(1));
  builder_.CreateCall(memcpyFn, {dst2, right, len2Plus1, builder_.getFalse()});
  
  return newStr;
}
inline llvm::Value *LLVMCodeGen::generateBinaryExpr(const BinaryExpr *expr) {

 // String concatenation
  if (expr->op == BinaryOp::Add) {
    // Check if either operand is a string
    bool isStringConcat = false;
    if (auto *litL = dynamic_cast<const LiteralExpr*>(expr->left.get())) {
      if (litL->kind == LiteralKind::String || litL->kind == LiteralKind::RawString)
        isStringConcat = true;
    }
    if (auto *litR = dynamic_cast<const LiteralExpr*>(expr->right.get())) {
      if (litR->kind == LiteralKind::String || litR->kind == LiteralKind::RawString)
        isStringConcat = true;
    }
    // Also check identifier types
    if (auto *idL = dynamic_cast<const IdentifierExpr*>(expr->left.get())) {
      const Symbol *sym = symbols_.lookup(idL->name);
      if (sym && sym->type && sym->type->kind == TypeKind::String)
        isStringConcat = true;
    }
    if (auto *idR = dynamic_cast<const IdentifierExpr*>(expr->right.get())) {
      const Symbol *sym = symbols_.lookup(idR->name);
      if (sym && sym->type && sym->type->kind == TypeKind::String)
        isStringConcat = true;
    }
    
    if (isStringConcat) {
      llvm::Value *left = generateExpression(expr->left.get());
      llvm::Value *right = generateExpression(expr->right.get());
      if (!left || !right) return nullptr;
      return generateStringConcat(left, right);
    }
  }

  // Handle short-circuit operators separately
  if (expr->op == BinaryOp::And) {
    llvm::Function *func = builder_.GetInsertBlock()->getParent();
    llvm::BasicBlock *rhsBB =
        llvm::BasicBlock::Create(context_, "and.rhs", func);
    llvm::BasicBlock *mergeBB =
        llvm::BasicBlock::Create(context_, "and.merge", func);

    llvm::Value *left = generateExpression(expr->left.get());
    if (!left)
      return nullptr;
    if (!left->getType()->isIntegerTy(1))
      left = builder_.CreateICmpNE(
          left, llvm::Constant::getNullValue(left->getType()), "tobool");

    llvm::BasicBlock *entryBB = builder_.GetInsertBlock();
    builder_.CreateCondBr(left, rhsBB, mergeBB);

    builder_.SetInsertPoint(rhsBB);
    llvm::Value *right = generateExpression(expr->right.get());
    if (!right)
      return nullptr;
    if (!right->getType()->isIntegerTy(1))
      right = builder_.CreateICmpNE(
          right, llvm::Constant::getNullValue(right->getType()), "tobool");
    llvm::BasicBlock *rhsEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi =
        builder_.CreatePHI(builder_.getInt1Ty(), 2, "and.result");
    phi->addIncoming(builder_.getFalse(), entryBB);
    phi->addIncoming(right, rhsEndBB);
    return phi;
  }

  if (expr->op == BinaryOp::Or) {
    llvm::Function *func = builder_.GetInsertBlock()->getParent();
    llvm::BasicBlock *rhsBB =
        llvm::BasicBlock::Create(context_, "or.rhs", func);
    llvm::BasicBlock *mergeBB =
        llvm::BasicBlock::Create(context_, "or.merge", func);

    llvm::Value *left = generateExpression(expr->left.get());
    if (!left)
      return nullptr;
    if (!left->getType()->isIntegerTy(1))
      left = builder_.CreateICmpNE(
          left, llvm::Constant::getNullValue(left->getType()), "tobool");

    llvm::BasicBlock *entryBB = builder_.GetInsertBlock();
    builder_.CreateCondBr(left, mergeBB, rhsBB);

    builder_.SetInsertPoint(rhsBB);
    llvm::Value *right = generateExpression(expr->right.get());
    if (!right)
      return nullptr;
    if (!right->getType()->isIntegerTy(1))
      right = builder_.CreateICmpNE(
          right, llvm::Constant::getNullValue(right->getType()), "tobool");
    llvm::BasicBlock *rhsEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi =
        builder_.CreatePHI(builder_.getInt1Ty(), 2, "or.result");
    phi->addIncoming(builder_.getTrue(), entryBB);
    phi->addIncoming(right, rhsEndBB);
    return phi;
  }

  // Rest of binary expr handling (non-short-circuit)
  llvm::Value *left = generateExpression(expr->left.get());
  llvm::Value *right = generateExpression(expr->right.get());

  if (!left || !right)
    return nullptr;

  bool leftFloat = left->getType()->isFloatingPointTy();
  bool rightFloat = right->getType()->isFloatingPointTy();
  if (leftFloat && !rightFloat)
    right = builder_.CreateSIToFP(right, left->getType(), "conv");
  else if (!leftFloat && rightFloat)
    left = builder_.CreateSIToFP(left, right->getType(), "conv");
  bool isFloat = left->getType()->isFloatingPointTy();

  switch (expr->op) {
  case BinaryOp::Add:
    return isFloat ? builder_.CreateFAdd(left, right, "fadd")
                   : builder_.CreateAdd(left, right, "add");
  case BinaryOp::Sub:
    return isFloat ? builder_.CreateFSub(left, right, "fsub")
                   : builder_.CreateSub(left, right, "sub");
  case BinaryOp::Mul:
    return isFloat ? builder_.CreateFMul(left, right, "fmul")
                   : builder_.CreateMul(left, right, "mul");
  case BinaryOp::Div:
    return isFloat ? builder_.CreateFDiv(left, right, "fdiv")
                   : builder_.CreateSDiv(left, right, "div");
  case BinaryOp::Mod:
    return isFloat ? builder_.CreateFRem(left, right, "fmod")
                   : builder_.CreateSRem(left, right, "mod");
  case BinaryOp::Equal:
    return isFloat ? builder_.CreateFCmpOEQ(left, right, "feq")
                   : builder_.CreateICmpEQ(left, right, "eq");
  case BinaryOp::NotEqual:
    return isFloat ? builder_.CreateFCmpONE(left, right, "fne")
                   : builder_.CreateICmpNE(left, right, "ne");
  case BinaryOp::Less:
    return isFloat ? builder_.CreateFCmpOLT(left, right, "flt")
                   : builder_.CreateICmpSLT(left, right, "lt");
  case BinaryOp::LessEqual:
    return isFloat ? builder_.CreateFCmpOLE(left, right, "fle")
                   : builder_.CreateICmpSLE(left, right, "le");
  case BinaryOp::Greater:
    return isFloat ? builder_.CreateFCmpOGT(left, right, "fgt")
                   : builder_.CreateICmpSGT(left, right, "gt");
  case BinaryOp::GreaterEqual:
    return isFloat ? builder_.CreateFCmpOGE(left, right, "fge")
                   : builder_.CreateICmpSGE(left, right, "ge");
  case BinaryOp::And:
  case BinaryOp::Or:
    // Already handled above
    return nullptr;
  default:
    error("Operator not supported");
    return nullptr;
  }
}

inline llvm::Value *LLVMCodeGen::generateUnaryExpr(const UnaryExpr *expr) {
  llvm::Value *operand = generateExpression(expr->operand.get());
  if (!operand)
    return nullptr;
  switch (expr->op) {
  case UnaryOp::Negate:
    return operand->getType()->isFloatingPointTy()
               ? builder_.CreateFNeg(operand, "fneg")
               : builder_.CreateNeg(operand, "neg");
  case UnaryOp::Not:
    return builder_.CreateNot(operand, "not");
  }
  return nullptr;
}

inline llvm::Value *LLVMCodeGen::generateAssignExpr(const AssignExpr *expr) {
  llvm::Value *value = generateExpression(expr->value.get());
  if (!value)
    return nullptr;

  // 1. Identifier assignment
  if (auto *id = dynamic_cast<const IdentifierExpr *>(expr->target.get())) {
    auto it = namedValues_.find(id->name);
    if (it != namedValues_.end()) {
      llvm::Value *finalVal = value;
      if (expr->op != AssignOp::Assign) {
        llvm::Value *current =
            builder_.CreateLoad(it->second->getAllocatedType(), it->second);
        bool isFloat = current->getType()->isFloatingPointTy();
        switch (expr->op) {
        case AssignOp::PlusAssign:
          finalVal = isFloat ? builder_.CreateFAdd(current, value)
                             : builder_.CreateAdd(current, value);
          break;
        case AssignOp::MinusAssign:
          finalVal = isFloat ? builder_.CreateFSub(current, value)
                             : builder_.CreateSub(current, value);
          break;
        case AssignOp::StarAssign:
          finalVal = isFloat ? builder_.CreateFMul(current, value)
                             : builder_.CreateMul(current, value);
          break;
        case AssignOp::SlashAssign:
          finalVal = isFloat ? builder_.CreateFDiv(current, value)
                             : builder_.CreateSDiv(current, value);
          break;
        default:
          break;
        }
      }
      builder_.CreateStore(finalVal, it->second);
      return finalVal;
    }

    // Check global
    auto git = globalValues_.find(id->name);
    if (git != globalValues_.end()) {
      builder_.CreateStore(value, git->second);
      return value;
    }

    // Check class field
    if (currentThisPtr_ && !currentClassName_.empty()) {
      auto cit = classInfos_.find(currentClassName_);
      if (cit != classInfos_.end()) {
        auto fit = cit->second.fieldIndices.find(id->name);
        if (fit != cit->second.fieldIndices.end()) {
          llvm::Value *fieldPtr = builder_.CreateStructGEP(
              cit->second.structType, currentThisPtr_, fit->second);
          llvm::Value *finalVal = value;
          if (expr->op != AssignOp::Assign) {
            llvm::Value *current = builder_.CreateLoad(
                cit->second.fields[fit->second].second, fieldPtr);
            bool isFloat = current->getType()->isFloatingPointTy();
            switch (expr->op) {
            case AssignOp::PlusAssign:
              finalVal = isFloat ? builder_.CreateFAdd(current, value)
                                 : builder_.CreateAdd(current, value);
              break;
            case AssignOp::MinusAssign:
              finalVal = isFloat ? builder_.CreateFSub(current, value)
                                 : builder_.CreateSub(current, value);
              break;
            case AssignOp::StarAssign:
              finalVal = isFloat ? builder_.CreateFMul(current, value)
                                 : builder_.CreateMul(current, value);
              break;
            case AssignOp::SlashAssign:
              finalVal = isFloat ? builder_.CreateFDiv(current, value)
                                 : builder_.CreateSDiv(current, value);
              break;
            default:
              break;
            }
          }
          builder_.CreateStore(finalVal, fieldPtr);
          return finalVal;
        }
      }
    }
    error("Undefined variable: " + id->name);
    return nullptr;
  }

  // 2. Index assignment: arr[i] = val (MOVED BEFORE MemberExpr)
  if (auto *indexExpr = dynamic_cast<const IndexExpr *>(expr->target.get())) {
    llvm::Value *arr = generateExpression(indexExpr->object.get());
    llvm::Value *idx = generateExpression(indexExpr->index.get());
    if (!arr || !idx)
      return nullptr;

    if (arr->getType()->isStructTy()) {
      llvm::Value *arrPtr = builder_.CreateExtractValue(arr, 0, "arr.ptr");
      llvm::Type *elemType = value->getType();
      idx = castIfNeeded(idx, builder_.getInt64Ty());
      llvm::Value *elemPtr =
          builder_.CreateGEP(elemType, arrPtr, idx, "elem.ptr");
      builder_.CreateStore(value, elemPtr);
      return value;
    }

    if (arr->getType()->isPointerTy()) {
      llvm::Type *elemType = value->getType();
      idx = castIfNeeded(idx, builder_.getInt64Ty());
      llvm::Value *elemPtr =
          builder_.CreateGEP(elemType, arr, idx, "ptr.elem");
      builder_.CreateStore(value, elemPtr);
      return value;
    }
  }

  // 3. Member assignment: obj.field = val
  if (auto *member = dynamic_cast<const MemberExpr *>(expr->target.get())) {
    llvm::Value *obj = generateExpression(member->object.get());
    if (!obj)
      return nullptr;

    std::string className;
    if (auto *idObj =
            dynamic_cast<const IdentifierExpr *>(member->object.get())) {
      if (idObj->name == "this") {
        className = currentClassName_;
        obj = currentThisPtr_;
      } else {
        const Symbol *sym = symbols_.lookup(idObj->name);
        if (sym && sym->type && sym->type->isUserDefined())
          className = sym->type->name;
      }
    }

    if (!className.empty()) {
      auto cit = classInfos_.find(className);
      if (cit != classInfos_.end()) {
        auto fit = cit->second.fieldIndices.find(member->member);
        if (fit != cit->second.fieldIndices.end()) {
          llvm::Value *fieldPtr = builder_.CreateStructGEP(
              cit->second.structType, obj, fit->second);
          builder_.CreateStore(value, fieldPtr);
          return value;
        }
      }
    }
  }

  error("Invalid assignment target");
  return nullptr;
}

inline llvm::Value *LLVMCodeGen::generateCallExpr(const CallExpr *call) {
  if (auto *id = dynamic_cast<const IdentifierExpr *>(call->callee.get())) {
    llvm::Function *func = module_->getFunction(id->name);
    if (!func)
      func = getOrDeclareEngineFunction(id->name);
    if (!func) {
      error("Undefined function: " + id->name);
      return nullptr;
    }

    std::vector<llvm::Value *> args;
    for (const auto &arg : call->args) {
      llvm::Value *argVal = generateExpression(arg.get());
      if (!argVal)
        return nullptr;
      args.push_back(argVal);
    }
    if (func->getReturnType()->isVoidTy()) {
      builder_.CreateCall(func, args);
      return nullptr;
    }
    return builder_.CreateCall(func, args, "call");
  }

  if (auto *member = dynamic_cast<const MemberExpr *>(call->callee.get())) {
    llvm::Value *obj = generateExpression(member->object.get());
    if (!obj)
      return nullptr;

    std::string className;
    if (auto *idObj =
            dynamic_cast<const IdentifierExpr *>(member->object.get())) {
      if (idObj->name == "this") {
        className = currentClassName_;
        obj = currentThisPtr_;
      } else {
        const Symbol *sym = symbols_.lookup(idObj->name);
        if (sym && sym->type && sym->type->isUserDefined())
          className = sym->type->name;
      }
    }

    if (!className.empty()) {
      std::string mangledName = className + "_" + member->member;
      llvm::Function *method = module_->getFunction(mangledName);
      if (method) {
        std::vector<llvm::Value *> args;
        args.push_back(obj);
        for (const auto &arg : call->args) {
          llvm::Value *argVal = generateExpression(arg.get());
          if (!argVal)
            return nullptr;
          args.push_back(argVal);
        }
        if (method->getReturnType()->isVoidTy()) {
          builder_.CreateCall(method, args);
          return nullptr;
        }
        return builder_.CreateCall(method, args, "mcall");
      }
    }
    error("Unknown method: " + member->member);
    return nullptr;
  }

  error("Unsupported call expression");
  return nullptr;
}

inline llvm::Value *LLVMCodeGen::generateMemberExpr(const MemberExpr *member) {
  llvm::Value *obj = generateExpression(member->object.get());
  if (!obj)
    return nullptr;

  std::string className;
  if (auto *id = dynamic_cast<const IdentifierExpr *>(member->object.get())) {
    if (id->name == "this") {
      className = currentClassName_;
      obj = currentThisPtr_;
    } else {
      const Symbol *sym = symbols_.lookup(id->name);
      if (sym && sym->type && sym->type->isUserDefined())
        className = sym->type->name;
    }
  }

  if (!className.empty()) {
    auto cit = classInfos_.find(className);
    if (cit != classInfos_.end()) {
      auto fit = cit->second.fieldIndices.find(member->member);
      if (fit != cit->second.fieldIndices.end()) {
        llvm::Value *fieldPtr =
            builder_.CreateStructGEP(cit->second.structType, obj, fit->second);
        return builder_.CreateLoad(cit->second.fields[fit->second].second,
                                   fieldPtr, member->member);
      }
    }
  }
  error("Unknown member: " + member->member);
  return nullptr;
}

inline llvm::Value *
LLVMCodeGen::generateOptionalMemberExpr(const OptionalMemberExpr *member) {
  llvm::Value *obj = generateExpression(member->object.get());
  if (!obj)
    return nullptr;

  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::BasicBlock *notNullBB =
      llvm::BasicBlock::Create(context_, "opt.notnull", func);
  llvm::BasicBlock *nullBB =
      llvm::BasicBlock::Create(context_, "opt.null", func);
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(context_, "opt.merge", func);

  llvm::Value *isNull = builder_.CreateIsNull(obj, "isnull");
  builder_.CreateCondBr(isNull, nullBB, notNullBB);

  builder_.SetInsertPoint(notNullBB);
  llvm::Type *resultType = builder_.getPtrTy();
  llvm::Value *result = llvm::Constant::getNullValue(resultType);
  builder_.CreateBr(mergeBB);
  llvm::BasicBlock *notNullEnd = builder_.GetInsertBlock();

  builder_.SetInsertPoint(nullBB);
  llvm::Value *nullResult = llvm::Constant::getNullValue(resultType);
  builder_.CreateBr(mergeBB);

  builder_.SetInsertPoint(mergeBB);
  llvm::PHINode *phi = builder_.CreatePHI(resultType, 2, "opt.result");
  phi->addIncoming(result, notNullEnd);
  phi->addIncoming(nullResult, nullBB);
  return phi;
}

inline llvm::Value *LLVMCodeGen::generateIndexExpr(const IndexExpr *index) {
  llvm::Value *arr = generateExpression(index->object.get());
  llvm::Value *idx = generateExpression(index->index.get());
  if (!arr || !idx)
    return nullptr;

  // If arr is array struct {ptr, len}
  if (arr->getType()->isStructTy()) {
    llvm::Value *arrPtr = builder_.CreateExtractValue(arr, 0, "arr.ptr");
    llvm::Value *arrLen = builder_.CreateExtractValue(arr, 1, "arr.len");

    // TODO: Optional bounds check
    // llvm::Value *inBounds = builder_.CreateICmpSLT(idx, arrLen);

    // Default to int32 element type (improve with type info if available)
    llvm::Type *elemType = builder_.getInt32Ty();

    // Check symbol table for actual element type
    if (auto *idExpr =
            dynamic_cast<const IdentifierExpr *>(index->object.get())) {
      const Symbol *sym = symbols_.lookup(idExpr->name);
      if (sym && sym->type && sym->type->kind == TypeKind::Array &&
          sym->type->elementType) {
        elemType = toLLVMType(sym->type->elementType);
      }
    }

    idx = castIfNeeded(idx, builder_.getInt64Ty());
    llvm::Value *elemPtr =
        builder_.CreateGEP(elemType, arrPtr, idx, "elem.ptr");
    return builder_.CreateLoad(elemType, elemPtr, "elem");
  }

  // Pointer indexing
  if (arr->getType()->isPointerTy()) {
    llvm::Type *elemType = builder_.getInt32Ty();
    idx = castIfNeeded(idx, builder_.getInt64Ty());
    llvm::Value *elemPtr = builder_.CreateGEP(elemType, arr, idx, "ptr.elem");
    return builder_.CreateLoad(elemType, elemPtr, "elem");
  }

  error("Cannot index non-array type");
  return nullptr;
}

inline llvm::Value *LLVMCodeGen::generateNewExpr(const NewExpr *newExpr) {
  auto cit = classInfos_.find(newExpr->className);
  if (cit == classInfos_.end()) {
    error("Unknown class: " + newExpr->className);
    return nullptr;
  }

  std::string ctorName = newExpr->className + "_new";
  llvm::Function *ctor = module_->getFunction(ctorName);
  if (!ctor) {
    error("No constructor for: " + newExpr->className);
    return nullptr;
  }

  std::vector<llvm::Value *> args;
  for (const auto &arg : newExpr->args) {
    llvm::Value *argVal = generateExpression(arg.get());
    if (argVal)
      args.push_back(argVal);
  }
  return builder_.CreateCall(ctor, args, "new");
}

inline llvm::Value *
LLVMCodeGen::generateArrayLiteralExpr(const ArrayLiteralExpr *arr) {
  if (arr->elements.empty()) {
    // Empty array: {nullptr, 0}
    llvm::Type *arrStructTy = llvm::StructType::get(
        context_, {builder_.getPtrTy(), builder_.getInt64Ty()});
    llvm::Value *result = llvm::UndefValue::get(arrStructTy);
    result = builder_.CreateInsertValue(
        result, llvm::ConstantPointerNull::get(builder_.getPtrTy()), 0);
    result = builder_.CreateInsertValue(result, builder_.getInt64(0), 1);
    return result;
  }

  // Determine element type from first element
  llvm::Value *firstElem = generateExpression(arr->elements[0].get());
  if (!firstElem)
    return nullptr;
  llvm::Type *elemType = firstElem->getType();

  size_t count = arr->elements.size();

  // Allocate array on heap
  llvm::Function *mallocFn = getOrDeclareMalloc();
  llvm::Value *elemSize =
      builder_.getInt64(module_->getDataLayout().getTypeAllocSize(elemType));
  llvm::Value *totalSize =
      builder_.CreateMul(elemSize, builder_.getInt64(count));

llvm::Function *arenaAllocFn = module_->getFunction("sl_arena_alloc");
if (!arenaAllocFn) {
  llvm::FunctionType *arenaAllocTy = llvm::FunctionType::get(
      builder_.getPtrTy(),
      {builder_.getPtrTy(), builder_.getInt64Ty()},
      false);
  arenaAllocFn = llvm::Function::Create(arenaAllocTy, 
      llvm::Function::ExternalLinkage, "sl_arena_alloc", module_.get());
}
llvm::Value *rawPtr = builder_.CreateCall(arenaAllocFn, {frameArena_, totalSize});

  // Store first element
  builder_.CreateStore(firstElem, rawPtr);

  // Store remaining elements
  for (size_t i = 1; i < count; ++i) {
    llvm::Value *elemVal = generateExpression(arr->elements[i].get());
    if (!elemVal)
      return nullptr;
    elemVal = castIfNeeded(elemVal, elemType);
    llvm::Value *elemPtr =
        builder_.CreateGEP(elemType, rawPtr, builder_.getInt64(i), "arr.elem");
    builder_.CreateStore(elemVal, elemPtr);
  }

  // Return {ptr, len} struct
  llvm::Type *arrStructTy = llvm::StructType::get(
      context_, {builder_.getPtrTy(), builder_.getInt64Ty()});
  llvm::Value *result = llvm::UndefValue::get(arrStructTy);
  result = builder_.CreateInsertValue(result, rawPtr, 0);
  result = builder_.CreateInsertValue(result, builder_.getInt64(count), 1);
  return result;
}

inline llvm::Value *
LLVMCodeGen::generateNullCoalesceExpr(const NullCoalesceExpr *nc) {
  llvm::Value *left = generateExpression(nc->left.get());
  if (!left)
    return nullptr;

  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::BasicBlock *notNullBB =
      llvm::BasicBlock::Create(context_, "nc.notnull", func);
  llvm::BasicBlock *nullBB =
      llvm::BasicBlock::Create(context_, "nc.null", func);
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(context_, "nc.merge", func);

  llvm::Value *isNull = builder_.CreateIsNull(left, "isnull");
  builder_.CreateCondBr(isNull, nullBB, notNullBB);

  builder_.SetInsertPoint(notNullBB);
  builder_.CreateBr(mergeBB);

  builder_.SetInsertPoint(nullBB);
  llvm::Value *right = generateExpression(nc->right.get());
  builder_.CreateBr(mergeBB);
  llvm::BasicBlock *nullEnd = builder_.GetInsertBlock();

  builder_.SetInsertPoint(mergeBB);
  llvm::PHINode *phi = builder_.CreatePHI(left->getType(), 2, "nc.result");
  phi->addIncoming(left, notNullBB);
  phi->addIncoming(
      right ? right : llvm::Constant::getNullValue(left->getType()), nullEnd);
  return phi;
}

inline llvm::Value *LLVMCodeGen::generateTernaryExpr(const TernaryExpr *tern) {
  llvm::Value *cond = generateExpression(tern->condition.get());
  if (!cond)
    return nullptr;
  if (!cond->getType()->isIntegerTy(1))
    cond = builder_.CreateICmpNE(cond,
                                 llvm::Constant::getNullValue(cond->getType()));

  llvm::Function *func = builder_.GetInsertBlock()->getParent();
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context_, "tern.then", func);
  llvm::BasicBlock *elseBB =
      llvm::BasicBlock::Create(context_, "tern.else", func);
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(context_, "tern.merge", func);

  builder_.CreateCondBr(cond, thenBB, elseBB);

  builder_.SetInsertPoint(thenBB);
  llvm::Value *thenVal = generateExpression(tern->thenExpr.get());
  builder_.CreateBr(mergeBB);
  llvm::BasicBlock *thenEnd = builder_.GetInsertBlock();

  builder_.SetInsertPoint(elseBB);
  llvm::Value *elseVal = generateExpression(tern->elseExpr.get());
  builder_.CreateBr(mergeBB);
  llvm::BasicBlock *elseEnd = builder_.GetInsertBlock();

  builder_.SetInsertPoint(mergeBB);
  if (!thenVal || !elseVal)
    return nullptr;

  llvm::Type *resultType = thenVal->getType();
  if (elseVal->getType() != resultType)
    elseVal = castIfNeeded(elseVal, resultType);

  llvm::PHINode *phi = builder_.CreatePHI(resultType, 2, "tern.result");
  phi->addIncoming(thenVal, thenEnd);
  phi->addIncoming(elseVal, elseEnd);
  return phi;
}

inline llvm::Value *LLVMCodeGen::generateLambdaExpr(const LambdaExpr *lambda) {
  std::string lambdaName = "__lambda_" + std::to_string(lambdaCounter_++);

  std::vector<llvm::Type *> paramTypes;
  for (const auto &param : lambda->params)
    paramTypes.push_back(resolveTypeFromAnnotation(param.typeAnnot.get()));

  llvm::Type *retType = builder_.getInt32Ty();
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(retType, paramTypes, false);
  llvm::Function *lambdaFunc = llvm::Function::Create(
      funcType, llvm::Function::InternalLinkage, lambdaName, module_.get());

  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(context_, "entry", lambdaFunc);
  auto oldBuilder = builder_.GetInsertBlock();
  auto oldFunction = currentFunction_;
  auto oldNamedValues = namedValues_;

  builder_.SetInsertPoint(entry);
  currentFunction_ = lambdaFunc;
  namedValues_.clear();

  auto argIt = lambdaFunc->arg_begin();
  for (size_t i = 0; i < lambda->params.size(); ++i, ++argIt) {
    argIt->setName(lambda->params[i].name);
    llvm::AllocaInst *alloca = createEntryBlockAlloca(
        lambdaFunc, lambda->params[i].name, argIt->getType());
    builder_.CreateStore(&*argIt, alloca);
    namedValues_[lambda->params[i].name] = alloca;
  }

  if (lambda->bodyExpr) {
    llvm::Value *result = generateExpression(lambda->bodyExpr.get());
    if (result)
      builder_.CreateRet(result);
    else
      builder_.CreateRetVoid();
  } else if (lambda->bodyBlock) {
    generateStatement(lambda->bodyBlock.get());
    if (!builder_.GetInsertBlock()->getTerminator())
      builder_.CreateRetVoid();
  }

  namedValues_ = std::move(oldNamedValues);
  currentFunction_ = oldFunction;
  if (oldBuilder)
    builder_.SetInsertPoint(oldBuilder);
  return lambdaFunc;
}

inline llvm::Value *LLVMCodeGen::generateThisExpr(const ThisExpr *) {
  if (!currentThisPtr_) {
    error("'this' used outside class method");
    return nullptr;
  }
  return currentThisPtr_;
}

inline llvm::Value *LLVMCodeGen::generateSuperExpr(const SuperExpr *) {
  error("'super' not yet implemented");
  return nullptr;
}
inline llvm::Value *LLVMCodeGen::generateSpreadExpr(const SpreadExpr *spread) {
  return generateExpression(spread->operand.get());
}

inline llvm::Value *LLVMCodeGen::generateRangeExpr(const RangeExpr *range) {
  llvm::Value *start = generateExpression(range->start.get());
  llvm::Value *end = generateExpression(range->end.get());
  if (!start || !end)
    return nullptr;

  llvm::Type *rangeType = llvm::StructType::get(
      context_, {start->getType(), end->getType(), builder_.getInt1Ty()});
  llvm::Value *result = llvm::UndefValue::get(rangeType);
  result = builder_.CreateInsertValue(result, start, 0);
  result = builder_.CreateInsertValue(result, end, 1);
  result =
      builder_.CreateInsertValue(result, builder_.getInt1(range->inclusive), 2);
  return result;
}

inline llvm::Value *LLVMCodeGen::generateInterpolatedStringExpr(const InterpolatedStringExpr *str) {
  if (str->segments.empty() && str->parts.empty()) {
    return createStringConstant("");
  }
  
  // Start with first segment
  llvm::Value *result = nullptr;
  size_t partIdx = 0;
  
  for (size_t i = 0; i < str->segments.size(); ++i) {
    llvm::Value *segVal = createStringConstant(str->segments[i]);
    
    if (!result) {
      result = segVal;
    } else {
      result = generateStringConcat(result, segVal);
    }
    
    // If there's a part after this segment, concat it
    if (partIdx < str->parts.size()) {
      llvm::Value *partVal = generateExpression(str->parts[partIdx].get());
      if (partVal) {
        // Convert to string if not already
        if (!partVal->getType()->isPointerTy()) {
          partVal = intToString(partVal);
        }
        result = generateStringConcat(result, partVal);
      }
      ++partIdx;
    }
  }
  
  return result ? result : createStringConstant("");
}// ============================================================
// Class generation
// ============================================================

inline void LLVMCodeGen::generateClassDecl(const ClassDecl *cls) {
  auto &info = classInfos_[cls->name];
  for (const auto &member : cls->members) {
    if (auto *funcDecl =
            dynamic_cast<const FunctionDecl *>(member.decl.get())) {
      generateClassMethod(cls, funcDecl, info);
    }
  }
  generateClassConstructor(cls, info);
}

inline void LLVMCodeGen::generateClassMethod(const ClassDecl *cls,
                                             const FunctionDecl *method,
                                             ClassInfo &info) {
  std::vector<llvm::Type *> paramTypes;
  paramTypes.push_back(builder_.getPtrTy());
  for (const auto &param : method->params)
    paramTypes.push_back(resolveTypeFromAnnotation(param.typeAnnot.get()));

  llvm::Type *retType =
      method->returnType ? resolveTypeFromAnnotation(method->returnType.get())
                         : builder_.getVoidTy();
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(retType, paramTypes, false);

  std::string mangledName = cls->name + "_" + method->name;
  llvm::Function *methodFunc = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, mangledName, module_.get());

  info.methods[method->name] = methodFunc;
  functions_[mangledName] = methodFunc;

  auto argIt = methodFunc->arg_begin();
  (argIt++)->setName("this");
  for (size_t i = 0; i < method->params.size(); ++i, ++argIt)
    argIt->setName(method->params[i].name);

  if (method->body)
    generateMethodBody(method, methodFunc, info);
}

inline void LLVMCodeGen::generateMethodBody(const FunctionDecl *method,
                                            llvm::Function *methodFunc,
                                            ClassInfo &info) {
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(context_, "entry", methodFunc);
  builder_.SetInsertPoint(entry);

  auto oldFunction = currentFunction_;
  auto oldNamedValues = namedValues_;
  auto oldThisPtr = currentThisPtr_;
  auto oldClassName = currentClassName_;

  currentFunction_ = methodFunc;
  namedValues_.clear();
  currentClassName_ = info.structType->getName().str();

  auto argIt = methodFunc->arg_begin();
  currentThisPtr_ = &*argIt++;

  for (size_t i = 0; i < method->params.size(); ++i, ++argIt) {
    llvm::AllocaInst *alloca = createEntryBlockAlloca(
        methodFunc, method->params[i].name, argIt->getType());
    builder_.CreateStore(&*argIt, alloca);
    namedValues_[method->params[i].name] = alloca;
  }

  if (auto *block = dynamic_cast<const BlockStatement *>(method->body.get()))
    generateBlockStatement(block);
  else
    generateStatement(method->body.get());

  if (!builder_.GetInsertBlock()->getTerminator()) {
    if (methodFunc->getReturnType()->isVoidTy())
      builder_.CreateRetVoid();
    else
      builder_.CreateRet(
          llvm::Constant::getNullValue(methodFunc->getReturnType()));
  }

  currentFunction_ = oldFunction;
  namedValues_ = std::move(oldNamedValues);
  currentThisPtr_ = oldThisPtr;
  currentClassName_ = oldClassName;
}

inline void LLVMCodeGen::generateClassConstructor(const ClassDecl *cls,
                                                  ClassInfo &info) {
  llvm::FunctionType *ctorType =
      llvm::FunctionType::get(builder_.getPtrTy(), {}, false);
  std::string ctorName = cls->name + "_new";
  llvm::Function *ctorFunc = llvm::Function::Create(
      ctorType, llvm::Function::ExternalLinkage, ctorName, module_.get());

  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(context_, "entry", ctorFunc);
  builder_.SetInsertPoint(entry);

  llvm::Function *mallocFn = getOrDeclareMalloc();
  llvm::Value *size = llvm::ConstantInt::get(
      builder_.getInt64Ty(),
      module_->getDataLayout().getTypeAllocSize(info.structType));
 llvm::Function *arenaAllocFn = module_->getFunction("sl_arena_alloc");
if (!arenaAllocFn) {
  llvm::FunctionType *arenaAllocTy = llvm::FunctionType::get(
      builder_.getPtrTy(),
      {builder_.getPtrTy(), builder_.getInt64Ty()},
      false);
  arenaAllocFn = llvm::Function::Create(arenaAllocTy, 
      llvm::Function::ExternalLinkage, "sl_arena_alloc", module_.get());
}
llvm::Value *ptr = builder_.CreateCall(arenaAllocFn, {frameArena_, size});

  auto oldThisPtr = currentThisPtr_;
  auto oldClassName = currentClassName_;
  currentThisPtr_ = ptr;
  currentClassName_ = cls->name;

  int fieldIndex = 0;
  for (const auto &member : cls->members) {
    if (auto *varDecl = dynamic_cast<const VarDecl *>(member.decl.get())) {
      llvm::Value *fieldPtr =
          builder_.CreateStructGEP(info.structType, ptr, fieldIndex);
      if (varDecl->initializer) {
        llvm::Value *initVal = generateExpression(varDecl->initializer.get());
        if (initVal)
          builder_.CreateStore(initVal, fieldPtr);
        else
          builder_.CreateStore(
              llvm::Constant::getNullValue(info.fields[fieldIndex].second),
              fieldPtr);
      } else {
        builder_.CreateStore(
            llvm::Constant::getNullValue(info.fields[fieldIndex].second),
            fieldPtr);
      }
      fieldIndex++;
    }
  }

  currentThisPtr_ = oldThisPtr;
  currentClassName_ = oldClassName;

  builder_.CreateRet(ptr);
  functions_[ctorName] = ctorFunc;
}

inline void LLVMCodeGen::generateStructDecl(const StructDecl *strct) {
  auto &info = classInfos_[strct->name];
  for (const auto &member : strct->members) {
    if (auto *funcDecl =
            dynamic_cast<const FunctionDecl *>(member.decl.get())) {
      std::vector<llvm::Type *> paramTypes;
      paramTypes.push_back(builder_.getPtrTy());
      for (const auto &param : funcDecl->params)
        paramTypes.push_back(resolveTypeFromAnnotation(param.typeAnnot.get()));
      llvm::Type *retType =
          funcDecl->returnType
              ? resolveTypeFromAnnotation(funcDecl->returnType.get())
              : builder_.getVoidTy();
      llvm::FunctionType *funcType =
          llvm::FunctionType::get(retType, paramTypes, false);

      std::string mangledName = strct->name + "_" + funcDecl->name;
      llvm::Function *methodFunc =
          llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                 mangledName, module_.get());
      if (funcDecl->body)
        generateMethodBody(funcDecl, methodFunc, info);
    }
  }
}

inline void LLVMCodeGen::generateVarDecl(const VarDecl *var) {
  // If no current function, this is a global (already handled in generate())
  if (!currentFunction_)
    return;

  llvm::Type *varType =
      resolveVarType(var->name, var->typeAnnot.get(), var->initializer.get());
  llvm::AllocaInst *alloca =
      createEntryBlockAlloca(currentFunction_, var->name, varType);

  if (var->initializer) {
    llvm::Value *initVal = generateExpression(var->initializer.get());
    if (initVal) {
      initVal = castIfNeeded(initVal, varType);
      builder_.CreateStore(initVal, alloca);
    }
  }

  namedValues_[var->name] = alloca;
}

inline void LLVMCodeGen::generateGlobalVar(const VarDecl *var) {
  llvm::Type *varType =
      resolveVarType(var->name, var->typeAnnot.get(), var->initializer.get());
  llvm::Constant *init = llvm::Constant::getNullValue(varType);
  if (var->initializer) {
    if (auto *lit = dynamic_cast<const LiteralExpr *>(var->initializer.get())) {
      if (lit->kind == LiteralKind::Integer)
        init = llvm::ConstantInt::get(varType,
                                      std::stoll(lit->value, nullptr, 10));
      else if (lit->kind == LiteralKind::Float)
        init = llvm::ConstantFP::get(varType, std::stod(lit->value));
      else if (lit->kind == LiteralKind::True)
        init = llvm::ConstantInt::getTrue(context_);
      else if (lit->kind == LiteralKind::False)
        init = llvm::ConstantInt::getFalse(context_);
    }
  }
  llvm::GlobalVariable *gv = new llvm::GlobalVariable(
      *module_, varType, false, llvm::GlobalValue::ExternalLinkage, init,
      var->name);
  globalValues_[var->name] = gv;
}

inline void LLVMCodeGen::generateConstDecl(const ConstDecl *constDecl) {
  // If no current function, this is a global (already handled in generate())
  if (!currentFunction_)
    return;

  llvm::Type *varType =
      resolveVarType(constDecl->name, constDecl->typeAnnot.get(),
                     constDecl->initializer.get());
  llvm::AllocaInst *alloca =
      createEntryBlockAlloca(currentFunction_, constDecl->name, varType);

  if (constDecl->initializer) {
    llvm::Value *initVal = generateExpression(constDecl->initializer.get());
    if (initVal) {
      initVal = castIfNeeded(initVal, varType);
      builder_.CreateStore(initVal, alloca);
    }
  }

  namedValues_[constDecl->name] = alloca;
}

inline void LLVMCodeGen::generateGlobalConst(const ConstDecl *constDecl) {
  llvm::Type *varType =
      resolveVarType(constDecl->name, constDecl->typeAnnot.get(),
                     constDecl->initializer.get());
  llvm::Constant *init = llvm::Constant::getNullValue(varType);
  if (constDecl->initializer) {
    if (auto *lit =
            dynamic_cast<const LiteralExpr *>(constDecl->initializer.get())) {
      if (lit->kind == LiteralKind::Integer)
        init = llvm::ConstantInt::get(varType,
                                      std::stoll(lit->value, nullptr, 10));
      else if (lit->kind == LiteralKind::Float)
        init = llvm::ConstantFP::get(varType, std::stod(lit->value));
      else if (lit->kind == LiteralKind::True)
        init = llvm::ConstantInt::getTrue(context_);
      else if (lit->kind == LiteralKind::False)
        init = llvm::ConstantInt::getFalse(context_);
    }
  }
  llvm::GlobalVariable *gv = new llvm::GlobalVariable(
      *module_, varType, true, llvm::GlobalValue::ExternalLinkage, init,
      constDecl->name);
  globalValues_[constDecl->name] = gv;
}

inline void LLVMCodeGen::generateEnumDecl(const EnumDecl *) {}
inline void LLVMCodeGen::generateIncludeDecl(const IncludeDecl *) {}

// ============================================================
// Type helpers
// ============================================================

inline llvm::Type *LLVMCodeGen::toLLVMType(TypePtr type) {
  if (!type)
    return builder_.getVoidTy();
  switch (type->kind) {
  case TypeKind::Int8:
    return builder_.getInt8Ty();
  case TypeKind::Int16:
    return builder_.getInt16Ty();
  case TypeKind::Int32:
    return builder_.getInt32Ty();
  case TypeKind::Int64:
    return builder_.getInt64Ty();
  case TypeKind::UInt8:
    return builder_.getInt8Ty();
  case TypeKind::UInt16:
    return builder_.getInt16Ty();
  case TypeKind::UInt32:
    return builder_.getInt32Ty();
  case TypeKind::UInt64:
    return builder_.getInt64Ty();
  case TypeKind::Float32:
    return builder_.getFloatTy();
  case TypeKind::Float64:
    return builder_.getDoubleTy();
  case TypeKind::Bool:
    return builder_.getInt1Ty();
  case TypeKind::Void:
    return builder_.getVoidTy();
  case TypeKind::String:
    return builder_.getPtrTy();
  case TypeKind::Null:
    return builder_.getPtrTy();
  case TypeKind::Auto:
    return builder_.getInt32Ty();
  case TypeKind::Array:
    return llvm::StructType::get(context_,
                                 {builder_.getPtrTy(), builder_.getInt64Ty()});
  case TypeKind::Optional:
    return builder_.getPtrTy();
  case TypeKind::Range:
    return llvm::StructType::get(
        context_,
        {builder_.getInt32Ty(), builder_.getInt32Ty(), builder_.getInt1Ty()});
  case TypeKind::Function:
    return builder_.getPtrTy();
  case TypeKind::Class:
  case TypeKind::Struct:
    return builder_.getPtrTy();
  case TypeKind::Enum:
    return builder_.getInt32Ty();
  case TypeKind::Error:
    return builder_.getInt32Ty();
  }
  return builder_.getInt32Ty();
}

inline llvm::Type *
LLVMCodeGen::resolveTypeFromAnnotation(const TypeAnnotation *typeAnnot) {
  if (!typeAnnot)
    return builder_.getInt32Ty();
  const std::string &name = typeAnnot->name;
  if (name == "int8")
    return builder_.getInt8Ty();
  if (name == "int16")
    return builder_.getInt16Ty();
  if (name == "int32")
    return builder_.getInt32Ty();
  if (name == "int64")
    return builder_.getInt64Ty();
  if (name == "uint8")
    return builder_.getInt8Ty();
  if (name == "uint16")
    return builder_.getInt16Ty();
  if (name == "uint32")
    return builder_.getInt32Ty();
  if (name == "uint64")
    return builder_.getInt64Ty();
  if (name == "float32")
    return builder_.getFloatTy();
  if (name == "float64")
    return builder_.getDoubleTy();
  if (name == "bool")
    return builder_.getInt1Ty();
  if (name == "void")
    return builder_.getVoidTy();
  if (name == "string")
    return builder_.getPtrTy();
  if (name == "auto")
    return builder_.getInt32Ty();
  TypePtr scriptType = types_.lookupByName(name);
  if (scriptType)
    return toLLVMType(scriptType);
  auto it = classInfos_.find(name);
  if (it != classInfos_.end())
    return builder_.getPtrTy();
  return builder_.getInt32Ty();
}

inline llvm::Type *LLVMCodeGen::resolveVarType(const std::string &name,
                                               const TypeAnnotation *typeAnnot,
                                               const Expression *initializer) {
  const Symbol *sym = symbols_.lookup(name);
  if (sym && sym->type && sym->type->kind != TypeKind::Auto)
    return toLLVMType(sym->type);
    
  if (typeAnnot)
    return resolveTypeFromAnnotation(typeAnnot);
    
  if (initializer) {
    if (auto *lit = dynamic_cast<const LiteralExpr *>(initializer)) {
      switch (lit->kind) {
      case LiteralKind::Integer:
      case LiteralKind::HexInteger:
      case LiteralKind::BinaryInteger:
        return builder_.getInt32Ty();
      case LiteralKind::Float:
      case LiteralKind::Scientific:
        return builder_.getDoubleTy();
      case LiteralKind::True:
      case LiteralKind::False:
        return builder_.getInt1Ty();
      case LiteralKind::String:
      case LiteralKind::RawString:
      case LiteralKind::Null:
        return builder_.getPtrTy();
      default:
        break;
      }
    }
    // Unary NOT produces bool
    if (auto *unaryExpr = dynamic_cast<const UnaryExpr *>(initializer)) {
      if (unaryExpr->op == UnaryOp::Not)
        return builder_.getInt1Ty();
      // Negate preserves type
      return resolveVarType("", nullptr, unaryExpr->operand.get());
    }
    // Binary comparisons produce bool
    if (auto *binExpr = dynamic_cast<const BinaryExpr *>(initializer)) {
      switch (binExpr->op) {
      case BinaryOp::Equal:
      case BinaryOp::NotEqual:
      case BinaryOp::Less:
      case BinaryOp::LessEqual:
      case BinaryOp::Greater:
      case BinaryOp::GreaterEqual:
      case BinaryOp::And:
      case BinaryOp::Or:
        return builder_.getInt1Ty();
      default:
        break;
      }
      // Arithmetic - check operand types
      llvm::Type *leftType = resolveVarType("", nullptr, binExpr->left.get());
      llvm::Type *rightType = resolveVarType("", nullptr, binExpr->right.get());
      if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy())
        return builder_.getDoubleTy();
      return leftType;
    }
    // Identifier - look up type
    if (auto *idExpr = dynamic_cast<const IdentifierExpr *>(initializer)) {
      auto it = namedValues_.find(idExpr->name);
      if (it != namedValues_.end())
        return it->second->getAllocatedType();
      const Symbol *idSym = symbols_.lookup(idExpr->name);
      if (idSym && idSym->type)
        return toLLVMType(idSym->type);
    }
    if (dynamic_cast<const NewExpr *>(initializer))
      return builder_.getPtrTy();
    if (dynamic_cast<const ArrayLiteralExpr *>(initializer))
      return llvm::StructType::get(context_, {builder_.getPtrTy(), builder_.getInt64Ty()});
  }

  return builder_.getInt32Ty();
}inline void LLVMCodeGen::declareEngineFunctions() {
  // Declare external engine functions here
}

inline void LLVMCodeGen::error(const std::string &msg) {
  hasError_ = true;
  errors_.push_back(msg);
}

inline llvm::AllocaInst *LLVMCodeGen::createEntryBlockAlloca(
    llvm::Function *func, const std::string &varName, llvm::Type *type) {
  llvm::IRBuilder<> tmpBuilder(&func->getEntryBlock(),
                               func->getEntryBlock().begin());
  return tmpBuilder.CreateAlloca(type, nullptr, varName);
}

inline llvm::Value *LLVMCodeGen::castIfNeeded(llvm::Value *val,
                                              llvm::Type *targetType) {
  if (!val || val->getType() == targetType)
    return val;
  if (val->getType()->isIntegerTy() && targetType->isIntegerTy())
    return builder_.CreateIntCast(val, targetType, true);
  if (val->getType()->isFloatingPointTy() && targetType->isFloatingPointTy())
    return builder_.CreateFPCast(val, targetType);
  if (val->getType()->isIntegerTy() && targetType->isFloatingPointTy())
    return builder_.CreateSIToFP(val, targetType);
  if (val->getType()->isFloatingPointTy() && targetType->isIntegerTy())
    return builder_.CreateFPToSI(val, targetType);
  if (val->getType()->isPointerTy() && targetType->isPointerTy())
    return val;
  return val;
}

inline llvm::Value *LLVMCodeGen::createStringConstant(const std::string &str) {
  return builder_.CreateGlobalStringPtr(
      str, ".str." + std::to_string(stringConstCounter_++));
}
inline llvm::Function* LLVMCodeGen::getOrDeclareStrlen() {
  if (auto* f = module_->getFunction("strlen")) return f;
  auto* funcType = llvm::FunctionType::get(builder_.getInt64Ty(), {builder_.getPtrTy()}, false);
  return llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "strlen", module_.get());
}

inline llvm::Function* LLVMCodeGen::getOrDeclareMemcpy() {
  if (auto* f = module_->getFunction("llvm.memcpy.p0.p0.i64")) return f;
  auto* funcType = llvm::FunctionType::get(builder_.getVoidTy(), 
      {builder_.getPtrTy(), builder_.getPtrTy(), builder_.getInt64Ty(), builder_.getInt1Ty()}, false);
  return llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, 
      "llvm.memcpy.p0.p0.i64", module_.get());
}
inline llvm::Function *LLVMCodeGen::getOrDeclareMalloc() {
  if (auto *f = module_->getFunction("malloc"))
    return f;
  auto *funcType = llvm::FunctionType::get(builder_.getPtrTy(),
                                           {builder_.getInt64Ty()}, false);
  return llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                "malloc", module_.get());
}
inline llvm::Function* LLVMCodeGen::getOrDeclareSnprintf() {
  if (auto* f = module_->getFunction("snprintf")) return f;
  auto* funcType = llvm::FunctionType::get(builder_.getInt32Ty(), 
      {builder_.getPtrTy(), builder_.getInt64Ty(), builder_.getPtrTy()}, true); // variadic
  return llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "snprintf", module_.get());
}
inline llvm::Value *LLVMCodeGen::intToString(llvm::Value *val) {
  llvm::Function *snprintfFn = getOrDeclareSnprintf();
  llvm::Function *mallocFn = getOrDeclareMalloc();
  
  // Allocate buffer (32 bytes is enough for any 64-bit int)
 llvm::Function *arenaAllocFn = module_->getFunction("sl_arena_alloc");
if (!arenaAllocFn) {
  llvm::FunctionType *arenaAllocTy = llvm::FunctionType::get(
      builder_.getPtrTy(),
      {builder_.getPtrTy(), builder_.getInt64Ty()},
      false);
  arenaAllocFn = llvm::Function::Create(arenaAllocTy, 
      llvm::Function::ExternalLinkage, "sl_arena_alloc", module_.get());
}
llvm::Value *buf = builder_.CreateCall(arenaAllocFn, {frameArena_, builder_.getInt64(32)});
  
  // Format string for integer
  llvm::Value *fmt = createStringConstant("%d");
  
  // snprintf(buf, 32, "%d", val)
  val = castIfNeeded(val, builder_.getInt32Ty());
  builder_.CreateCall(snprintfFn, {buf, builder_.getInt64(32), fmt, val});
  
  return buf;
}
inline llvm::Function *LLVMCodeGen::getOrDeclareStrcat() {
  if (auto *f = module_->getFunction("strcat"))
    return f;
  auto *funcType = llvm::FunctionType::get(
      builder_.getPtrTy(), {builder_.getPtrTy(), builder_.getPtrTy()}, false);
  return llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                "strcat", module_.get());
}

inline llvm::Function *
LLVMCodeGen::getOrDeclareEngineFunction(const std::string &name) {
  if (auto *f = module_->getFunction(name))
    return f;
  return nullptr;
}

} // namespace scriptlang

# LLVM API Reference Guide

This is a practical guide to the LLVM C++ API for building compilers.

## Table of Contents
1. [Core Concepts](#core-concepts)
2. [Context & Module](#context--module)
3. [Types](#types)
4. [Values & Instructions](#values--instructions)
5. [Functions](#functions)
6. [Basic Blocks & Control Flow](#basic-blocks--control-flow)
7. [IRBuilder](#irbuilder)
8. [Memory Operations](#memory-operations)
9. [Common Patterns](#common-patterns)
10. [Debugging & Verification](#debugging--verification)

---

## Core Concepts

### The LLVM Type Hierarchy

```
Value (base class for all IR values)
├── Constant (compile-time constants)
│   ├── ConstantInt (42, true, false)
│   ├── ConstantFP (3.14)
│   ├── ConstantPointerNull (nullptr)
│   └── GlobalValue (globals, functions)
├── Instruction (runtime operations)
│   ├── BinaryOperator (add, mul, etc.)
│   ├── CallInst (function calls)
│   ├── LoadInst (load from memory)
│   ├── StoreInst (store to memory)
│   ├── AllocaInst (stack allocation)
│   └── ...
├── Argument (function parameters)
└── BasicBlock (sequence of instructions)
```

### SSA Form (Static Single Assignment)

Every value is assigned exactly once:
```llvm
; BAD (not valid LLVM IR):
%x = add i32 1, 2
%x = add i32 %x, 3    ; ERROR: reassignment

; GOOD (use phi for control flow merges):
entry:
  br i1 %cond, label %then, label %else
then:
  %x.then = add i32 1, 2
  br label %merge
else:
  %x.else = add i32 3, 4
  br label %merge
merge:
  %x = phi i32 [ %x.then, %then ], [ %x.else, %else ]
```

---

## Context & Module

### LLVMContext

The context owns global state like type uniquing. **Always create one context per thread.**

```cpp
#include <llvm/IR/LLVMContext.h>

llvm::LLVMContext context;
```

- Manages type uniquing (e.g., only one `i32` type exists)
- Not thread-safe - one per thread
- Destroyed automatically when out of scope

### Module

A module is a compilation unit - contains functions, globals, metadata.

```cpp
#include <llvm/IR/Module.h>

auto module = std::make_unique<llvm::Module>("my_module", context);

// Set target triple (what architecture we're compiling for)
module->setTargetTriple("x86_64-pc-linux-gnu");

// Set data layout (how types are laid out in memory)
module->setDataLayout("e-m:e-i64:64-f80:128-n8:16:32:64-S128");

// Print module to stdout
module->print(llvm::outs(), nullptr);

// Get a function by name
llvm::Function* func = module->getFunction("myFunction");

// Get a global variable by name
llvm::GlobalVariable* global = module->getNamedGlobal("myGlobal");
```

---

## Types

### Primitive Types

```cpp
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

llvm::LLVMContext context;

// Integer types
llvm::Type* i1  = llvm::Type::getInt1Ty(context);   // bool
llvm::Type* i8  = llvm::Type::getInt8Ty(context);   // byte/char
llvm::Type* i16 = llvm::Type::getInt16Ty(context);  // short
llvm::Type* i32 = llvm::Type::getInt32Ty(context);  // int
llvm::Type* i64 = llvm::Type::getInt64Ty(context);  // long

// Floating-point types
llvm::Type* f16  = llvm::Type::getHalfTy(context);     // half precision
llvm::Type* f32  = llvm::Type::getFloatTy(context);    // float
llvm::Type* f64  = llvm::Type::getDoubleTy(context);   // double
llvm::Type* f128 = llvm::Type::getFP128Ty(context);    // quad precision

// Void (for function returns)
llvm::Type* voidTy = llvm::Type::getVoidTy(context);

// Pointer (opaque pointers in LLVM 15+)
llvm::Type* ptrTy = llvm::PointerType::get(context, 0);  // ptr
```

### Aggregate Types

```cpp
// Array: [10 x i32]
llvm::Type* arrayTy = llvm::ArrayType::get(
    llvm::Type::getInt32Ty(context),
    10  // number of elements
);

// Struct: { i32, float, i8* }
std::vector<llvm::Type*> fields = {
    llvm::Type::getInt32Ty(context),
    llvm::Type::getFloatTy(context),
    llvm::PointerType::get(context, 0)
};
llvm::StructType* structTy = llvm::StructType::create(context, fields, "MyStruct");

// Or anonymous struct:
llvm::StructType* anonStruct = llvm::StructType::get(context, fields);

// Vector: <4 x float> (SIMD)
llvm::Type* vecTy = llvm::VectorType::get(
    llvm::Type::getFloatTy(context),
    llvm::ElementCount::getFixed(4),
    false  // not scalable
);
```

### Function Types

```cpp
// int32 add(int32 a, int32 b)
std::vector<llvm::Type*> paramTypes = {
    llvm::Type::getInt32Ty(context),
    llvm::Type::getInt32Ty(context)
};

llvm::FunctionType* funcTy = llvm::FunctionType::get(
    llvm::Type::getInt32Ty(context),  // return type
    paramTypes,                       // parameter types
    false                             // is variadic?
);

// Variadic: int printf(char* fmt, ...)
llvm::FunctionType* printfTy = llvm::FunctionType::get(
    llvm::Type::getInt32Ty(context),
    {llvm::PointerType::get(context, 0)},
    true  // variadic
);
```

---

## Values & Instructions

### Constants

```cpp
#include <llvm/IR/Constants.h>

// Integer constants
llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
llvm::Value* fortytwo = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 42);
llvm::Value* trueBool = llvm::ConstantInt::getTrue(context);
llvm::Value* falseBool = llvm::ConstantInt::getFalse(context);

// Floating-point constants
llvm::Value* pi = llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), 3.14159);

// Null pointer
llvm::Value* nullPtr = llvm::ConstantPointerNull::get(
    llvm::PointerType::get(context, 0)
);

// Struct constant: { 42, 3.14 }
std::vector<llvm::Constant*> structFields = {
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 42),
    llvm::ConstantFP::get(llvm::Type::getFloatTy(context), 3.14)
};
llvm::Constant* structConst = llvm::ConstantStruct::get(structTy, structFields);

// Array constant: [1, 2, 3]
std::vector<llvm::Constant*> arrayElements = {
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1),
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 2),
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 3)
};
llvm::Constant* arrayConst = llvm::ConstantArray::get(arrayTy, arrayElements);

// Zero initializer (for any type)
llvm::Constant* zeroInit = llvm::Constant::getNullValue(myType);
```

### Working with Values

```cpp
llvm::Value* val = /* ... */;

// Get the type
llvm::Type* type = val->getType();

// Set the name (for debugging)
val->setName("myValue");

// Check what kind of value it is
if (llvm::isa<llvm::Constant>(val)) {
    // It's a constant
}
if (auto* inst = llvm::dyn_cast<llvm::Instruction>(val)) {
    // It's an instruction
    llvm::BasicBlock* bb = inst->getParent();
}
if (auto* func = llvm::dyn_cast<llvm::Function>(val)) {
    // It's a function
}

// Replace all uses of one value with another
oldVal->replaceAllUsesWith(newVal);
```

---

## Functions

### Creating Functions

```cpp
#include <llvm/IR/Function.h>

// Create function type: int32 add(int32, int32)
llvm::FunctionType* funcTy = llvm::FunctionType::get(
    llvm::Type::getInt32Ty(context),
    {llvm::Type::getInt32Ty(context), llvm::Type::getInt32Ty(context)},
    false
);

// Create the function
llvm::Function* func = llvm::Function::Create(
    funcTy,
    llvm::Function::ExternalLinkage,  // linkage type
    "add",                            // name
    module.get()                      // parent module
);

// Name the parameters
auto args = func->arg_begin();
llvm::Argument* a = args++;
a->setName("a");
llvm::Argument* b = args++;
b->setName("b");
```

### Linkage Types

```cpp
// ExternalLinkage - visible outside module, can be called from other modules
llvm::Function::ExternalLinkage

// InternalLinkage - only visible within module (static in C)
llvm::Function::InternalLinkage

// PrivateLinkage - like internal, but may be optimized more aggressively
llvm::Function::PrivateLinkage

// WeakAnyLinkage - weak symbol (linker picks one if multiple definitions)
llvm::Function::WeakAnyLinkage

// ExternalWeakLinkage - weak external symbol
llvm::Function::ExternalWeakLinkage
```

### Function Attributes

```cpp
#include <llvm/IR/Attributes.h>

// Mark function as not returning (like [[noreturn]])
func->addFnAttr(llvm::Attribute::NoReturn);

// Mark function as readonly (doesn't modify memory)
func->addFnAttr(llvm::Attribute::ReadOnly);

// Mark function as not throwing exceptions
func->addFnAttr(llvm::Attribute::NoUnwind);

// Mark function for inlining
func->addFnAttr(llvm::Attribute::AlwaysInline);

// Mark parameter attributes
func->addParamAttr(0, llvm::Attribute::NoAlias);  // first param is noalias
func->addParamAttr(1, llvm::Attribute::NonNull);  // second param is nonnull
```

### Working with Functions

```cpp
// Iterate over basic blocks
for (llvm::BasicBlock& bb : *func) {
    // Process basic block
}

// Iterate over instructions
for (llvm::BasicBlock& bb : *func) {
    for (llvm::Instruction& inst : bb) {
        // Process instruction
    }
}

// Get entry block
llvm::BasicBlock* entry = &func->getEntryBlock();

// Check if function is declaration (no body)
if (func->isDeclaration()) {
    // It's just a declaration
}

// Delete function
func->eraseFromParent();
```

---

## Basic Blocks & Control Flow

### Creating Basic Blocks

```cpp
#include <llvm/IR/BasicBlock.h>

llvm::Function* func = /* ... */;

// Create basic blocks
llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", func);
llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "then", func);
llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "else", func);
llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "merge", func);
```

### Terminators (Control Flow)

Every basic block must end with exactly one terminator instruction.

```cpp
#include <llvm/IR/IRBuilder.h>

llvm::IRBuilder<> builder(context);
builder.SetInsertPoint(someBB);

// Return statement
builder.CreateRet(returnValue);      // return value
builder.CreateRetVoid();             // return (void)

// Unconditional branch
builder.CreateBr(targetBB);          // goto targetBB

// Conditional branch
builder.CreateCondBr(
    condition,   // i1 value
    thenBB,      // if true
    elseBB       // if false
);

// Switch statement
llvm::SwitchInst* switchInst = builder.CreateSwitch(
    value,      // value to switch on
    defaultBB,  // default case
    3           // number of cases (hint)
);
switchInst->addCase(
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1),
    case1BB
);
switchInst->addCase(
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 2),
    case2BB
);

// Unreachable (for impossible cases)
builder.CreateUnreachable();
```

### Phi Nodes (for SSA)

Phi nodes merge values from different predecessors.

```cpp
// Example: x = cond ? 10 : 20
builder.SetInsertPoint(mergeBB);

llvm::PHINode* phi = builder.CreatePHI(
    llvm::Type::getInt32Ty(context),
    2,          // number of incoming values
    "x"
);

phi->addIncoming(
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 10),
    thenBB
);
phi->addIncoming(
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 20),
    elseBB
);
```

---

## IRBuilder

IRBuilder is the main API for creating instructions. It tracks the current insertion point.

```cpp
#include <llvm/IR/IRBuilder.h>

llvm::IRBuilder<> builder(context);

// Set where to insert instructions
builder.SetInsertPoint(basicBlock);

// Or insert at specific position
builder.SetInsertPoint(basicBlock, iterator);
```

### Arithmetic Operations

```cpp
// Integer arithmetic
llvm::Value* add = builder.CreateAdd(lhs, rhs, "add");
llvm::Value* sub = builder.CreateSub(lhs, rhs, "sub");
llvm::Value* mul = builder.CreateMul(lhs, rhs, "mul");
llvm::Value* sdiv = builder.CreateSDiv(lhs, rhs, "div");  // signed
llvm::Value* udiv = builder.CreateUDiv(lhs, rhs, "div");  // unsigned
llvm::Value* srem = builder.CreateSRem(lhs, rhs, "rem");  // signed modulo
llvm::Value* urem = builder.CreateURem(lhs, rhs, "rem");  // unsigned modulo

// Floating-point arithmetic
llvm::Value* fadd = builder.CreateFAdd(lhs, rhs, "add");
llvm::Value* fsub = builder.CreateFSub(lhs, rhs, "sub");
llvm::Value* fmul = builder.CreateFMul(lhs, rhs, "mul");
llvm::Value* fdiv = builder.CreateFDiv(lhs, rhs, "div");
llvm::Value* frem = builder.CreateFRem(lhs, rhs, "rem");

// Negation
llvm::Value* neg = builder.CreateNeg(val, "neg");
llvm::Value* fneg = builder.CreateFNeg(val, "neg");

// Bitwise operations
llvm::Value* andVal = builder.CreateAnd(lhs, rhs, "and");
llvm::Value* orVal = builder.CreateOr(lhs, rhs, "or");
llvm::Value* xorVal = builder.CreateXor(lhs, rhs, "xor");
llvm::Value* notVal = builder.CreateNot(val, "not");

// Shifts
llvm::Value* shl = builder.CreateShl(val, amount, "shl");   // <<
llvm::Value* lshr = builder.CreateLShr(val, amount, "lshr"); // >> (logical)
llvm::Value* ashr = builder.CreateAShr(val, amount, "ashr"); // >> (arithmetic)
```

### Comparisons

```cpp
// Integer comparisons (returns i1)
llvm::Value* eq = builder.CreateICmpEQ(lhs, rhs, "eq");    // ==
llvm::Value* ne = builder.CreateICmpNE(lhs, rhs, "ne");    // !=
llvm::Value* slt = builder.CreateICmpSLT(lhs, rhs, "lt");  // < (signed)
llvm::Value* ult = builder.CreateICmpULT(lhs, rhs, "lt");  // < (unsigned)
llvm::Value* sle = builder.CreateICmpSLE(lhs, rhs, "le");  // <= (signed)
llvm::Value* ule = builder.CreateICmpULE(lhs, rhs, "le");  // <= (unsigned)
llvm::Value* sgt = builder.CreateICmpSGT(lhs, rhs, "gt");  // > (signed)
llvm::Value* ugt = builder.CreateICmpUGT(lhs, rhs, "gt");  // > (unsigned)
llvm::Value* sge = builder.CreateICmpSGE(lhs, rhs, "ge");  // >= (signed)
llvm::Value* uge = builder.CreateICmpUGE(lhs, rhs, "ge");  // >= (unsigned)

// Floating-point comparisons
llvm::Value* oeq = builder.CreateFCmpOEQ(lhs, rhs, "eq");  // ordered ==
llvm::Value* olt = builder.CreateFCmpOLT(lhs, rhs, "lt");  // ordered <
llvm::Value* ole = builder.CreateFCmpOLE(lhs, rhs, "le");  // ordered <=
llvm::Value* ogt = builder.CreateFCmpOGT(lhs, rhs, "gt");  // ordered >
llvm::Value* oge = builder.CreateFCmpOGE(lhs, rhs, "ge");  // ordered >=
llvm::Value* one = builder.CreateFCmpONE(lhs, rhs, "ne");  // ordered !=

// Unordered versions (true if either operand is NaN)
llvm::Value* ueq = builder.CreateFCmpUEQ(lhs, rhs, "eq");
```

### Type Conversions

```cpp
// Integer conversions
llvm::Value* trunc = builder.CreateTrunc(val, targetType, "trunc");     // i64 -> i32
llvm::Value* zext = builder.CreateZExt(val, targetType, "zext");        // i32 -> i64 (zero extend)
llvm::Value* sext = builder.CreateSExt(val, targetType, "sext");        // i32 -> i64 (sign extend)

// Float conversions
llvm::Value* fptrunc = builder.CreateFPTrunc(val, targetType, "trunc"); // double -> float
llvm::Value* fpext = builder.CreateFPExt(val, targetType, "ext");       // float -> double

// Int <-> Float
llvm::Value* sitofp = builder.CreateSIToFP(val, targetType, "conv");    // signed int -> float
llvm::Value* uitofp = builder.CreateUIToFP(val, targetType, "conv");    // unsigned int -> float
llvm::Value* fptosi = builder.CreateFPToSI(val, targetType, "conv");    // float -> signed int
llvm::Value* fptoui = builder.CreateFPToUI(val, targetType, "conv");    // float -> unsigned int

// Pointer conversions
llvm::Value* ptrtoint = builder.CreatePtrToInt(val, intType, "conv");   // ptr -> int
llvm::Value* inttoptr = builder.CreateIntToPtr(val, ptrType, "conv");   // int -> ptr

// Bitcast (reinterpret bits)
llvm::Value* bitcast = builder.CreateBitCast(val, targetType, "cast");
```

---

## Memory Operations

### Stack Allocation (alloca)

Allocates memory on the stack (like C `int x;`).

```cpp
// Allocate one i32 on the stack
llvm::AllocaInst* alloca = builder.CreateAlloca(
    llvm::Type::getInt32Ty(context),
    nullptr,    // array size (nullptr = single value)
    "x"
);

// Allocate array of 10 i32s
llvm::Value* arraySize = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 10);
llvm::AllocaInst* arrayAlloca = builder.CreateAlloca(
    llvm::Type::getInt32Ty(context),
    arraySize,
    "arr"
);

// Best practice: Create allocas at the start of function entry block
llvm::IRBuilder<> tmpBuilder(&func->getEntryBlock(), func->getEntryBlock().begin());
llvm::AllocaInst* alloca = tmpBuilder.CreateAlloca(type, nullptr, name);
```

### Load & Store

```cpp
// Store value to memory
builder.CreateStore(value, pointerToMemory);

// Load value from memory
llvm::Value* loaded = builder.CreateLoad(
    elementType,          // type of value being loaded
    pointerToMemory,
    "loaded"
);

// Volatile load/store (for memory-mapped I/O)
builder.CreateStore(value, ptr, /*isVolatile=*/true);
llvm::Value* vol = builder.CreateLoad(type, ptr, /*isVolatile=*/true);

// Aligned load/store
llvm::LoadInst* load = builder.CreateLoad(type, ptr);
load->setAlignment(llvm::Align(4));  // 4-byte aligned

llvm::StoreInst* store = builder.CreateStore(val, ptr);
store->setAlignment(llvm::Align(4));
```

### GetElementPtr (GEP)

GEP calculates addresses into aggregates (arrays, structs).

```cpp
// Array indexing: arr[i]
llvm::Value* elemPtr = builder.CreateGEP(
    elementType,        // type of array element
    arrayPtr,          // pointer to array
    {index},           // indices
    "elemPtr"
);

// Struct field access: struct.field1
llvm::Value* fieldPtr = builder.CreateStructGEP(
    structType,        // struct type
    structPtr,         // pointer to struct
    1,                 // field index
    "fieldPtr"
);

// Multi-dimensional: arr[i][j]
llvm::Value* elemPtr = builder.CreateGEP(
    arrayType,
    arrayPtr,
    {indexI, indexJ}
);

// Convenience: In-bounds GEP (enables more optimizations)
llvm::Value* ptr = builder.CreateInBoundsGEP(type, basePtr, {indices});
```

---

## Common Patterns

### If-Then-Else

```cpp
void generateIf(llvm::Value* cond, std::function<void()> thenCode,
                std::function<void()> elseCode) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "else", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "merge", func);
    
    builder.CreateCondBr(cond, thenBB, elseBB);
    
    // Then branch
    builder.SetInsertPoint(thenBB);
    thenCode();
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(mergeBB);
    }
    
    // Else branch
    builder.SetInsertPoint(elseBB);
    elseCode();
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(mergeBB);
    }
    
    // Merge
    builder.SetInsertPoint(mergeBB);
}
```

### While Loop

```cpp
void generateWhile(std::function<llvm::Value*()> condCode,
                   std::function<void()> bodyCode) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context, "while.cond", func);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context, "while.body", func);
    llvm::BasicBlock* exitBB = llvm::BasicBlock::Create(context, "while.exit", func);
    
    builder.CreateBr(condBB);
    
    // Condition
    builder.SetInsertPoint(condBB);
    llvm::Value* cond = condCode();
    builder.CreateCondBr(cond, bodyBB, exitBB);
    
    // Body
    builder.SetInsertPoint(bodyBB);
    bodyCode();
    builder.CreateBr(condBB);
    
    // Exit
    builder.SetInsertPoint(exitBB);
}
```

### Function Call

```cpp
// Direct call
llvm::Function* callee = module->getFunction("myFunction");
std::vector<llvm::Value*> args = {arg1, arg2, arg3};
llvm::Value* result = builder.CreateCall(callee, args, "call");

// Indirect call (function pointer)
llvm::FunctionType* funcType = /* ... */;
llvm::Value* result = builder.CreateCall(funcType, funcPtr, args);
```

### String Constant

```cpp
// Create global string constant
llvm::Constant* strConstant = builder.CreateGlobalStringPtr("Hello, World!", "str");

// Use with printf
llvm::Function* printf = module->getFunction("printf");
builder.CreateCall(printf, {strConstant});
```

### Select (Ternary Operator)

```cpp
// result = cond ? trueVal : falseVal
llvm::Value* result = builder.CreateSelect(cond, trueVal, falseVal, "select");
```

---

## Debugging & Verification

### Verify Functions and Modules

```cpp
#include <llvm/IR/Verifier.h>

// Verify a function
std::string error;
llvm::raw_string_ostream errorStream(error);
if (llvm::verifyFunction(*func, &errorStream)) {
    llvm::errs() << "Function verification failed:\n" << error << "\n";
}

// Verify entire module
if (llvm::verifyModule(*module, &errorStream)) {
    llvm::errs() << "Module verification failed:\n" << error << "\n";
}

// Verify and abort on error (for debugging)
llvm::verifyFunction(*func, &llvm::errs());
llvm::verifyModule(*module, &llvm::errs());
```

### Print IR

```cpp
// Print to stdout
module->print(llvm::outs(), nullptr);
func->print(llvm::outs());
bb->print(llvm::outs());
inst->print(llvm::outs());

// Print to string
std::string str;
llvm::raw_string_ostream stream(str);
module->print(stream, nullptr);
llvm::outs() << str;

// Print to file
std::error_code EC;
llvm::raw_fd_ostream file("output.ll", EC);
module->print(file, nullptr);
```

### Dump Methods

```cpp
// Dump to stderr (for debugging)
module->dump();
func->dump();
bb->dump();
inst->dump();
value->dump();
type->dump();
```

### View CFG

```cpp
#include <llvm/Analysis/CFGPrinter.h>

// View control flow graph in graphviz
func->viewCFG();

// Or write to .dot file
func->viewCFGOnly();  // simplified view
```

---

## Quick Reference Cheat Sheet

```cpp
// Create context & module
llvm::LLVMContext context;
auto module = std::make_unique<llvm::Module>("name", context);
llvm::IRBuilder<> builder(context);

// Types
auto i32 = builder.getInt32Ty();
auto f64 = builder.getDoubleTy();
auto ptr = builder.getPtrTy();
auto voidTy = builder.getVoidTy();

// Constants
auto zero = builder.getInt32(0);
auto pi = llvm::ConstantFP::get(f64, 3.14);
auto nullPtr = llvm::ConstantPointerNull::get(ptr);

// Function
auto funcTy = llvm::FunctionType::get(returnType, {paramTypes}, false);
auto func = llvm::Function::Create(funcTy, llvm::Function::ExternalLinkage, "name", module.get());

// Basic blocks
auto bb = llvm::BasicBlock::Create(context, "name", func);
builder.SetInsertPoint(bb);

// Instructions
auto add = builder.CreateAdd(lhs, rhs);
auto cmp = builder.CreateICmpEQ(lhs, rhs);
auto call = builder.CreateCall(callee, {args});
auto alloca = builder.CreateAlloca(type);
auto load = builder.CreateLoad(type, ptr);
builder.CreateStore(val, ptr);
builder.CreateRet(val);
builder.CreateBr(targetBB);
builder.CreateCondBr(cond, thenBB, elseBB);

// Verify
llvm::verifyFunction(*func);
llvm::verifyModule(*module);

// Print
module->print(llvm::outs(), nullptr);
```

---

## Further Reading

- **Official LLVM Programmer's Manual**: https://llvm.org/docs/ProgrammersManual.html
- **LLVM Language Reference**: https://llvm.org/docs/LangRef.html
- **Kaleidoscope Tutorial**: https://llvm.org/docs/tutorial/
- **LLVM Doxygen**: https://llvm.org/doxygen/

This guide covers 90% of what you need for building a compiler. Experiment, read the generated IR, and iterate!

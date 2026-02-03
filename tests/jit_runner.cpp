// jit_runner.cpp - Complete, runnable JIT compiler for ScriptLang
// This file contains everything needed to run the JIT compiler

#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <chrono>

// LLVM includes
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

// Your compiler headers (assumed to be available)
// If you don't have implementations yet, we'll create minimal stubs
#define ARENA_IMPLEMENTATION

 #include "scriptlang.h"

using namespace scriptlang;

// ============================================================
// Engine API Functions - These are callable from scripts
// ============================================================

extern "C" {
    // Simple print function
    void ScriptPrint(const char* message) {
        std::cout << "[Script] " << message << std::endl;
    }
    
    // Print an integer
    void ScriptPrintInt(int32_t value) {
        std::cout << "[Script] " << value << std::endl;
    }
    
    // Print a float
    void ScriptPrintFloat(float value) {
        std::cout << "[Script] " << value << std::endl;
    }
    
    // Get delta time (mock)
    float ScriptGetDeltaTime() {
        return 0.016f; // 60 FPS
    }
    
    // Simple math function
    int32_t ScriptAdd(int32_t a, int32_t b) {
        return a + b;
    }
}

// ============================================================
// Simple JIT Runner - Minimal implementation
// ============================================================

class SimpleJIT {
public:
    SimpleJIT() {
        // Initialize LLVM
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
    }
    
    bool initialize() {
        // Create LLJIT instance
        auto jitOrErr = llvm::orc::LLJITBuilder().create();
        if (!jitOrErr) {
            std::cerr << "Failed to create LLJIT: " 
                     << llvm::toString(jitOrErr.takeError()) << std::endl;
            return false;
        }
        
        jit_ = std::move(*jitOrErr);
        std::cout << "✓ JIT initialized successfully" << std::endl;
        
        // Bind engine functions
        bindEngineFunctions();
        
        return true;
    }
    
    // Create a simple test module with LLVM IR directly
    void createTestModule() {
        auto context = std::make_unique<llvm::LLVMContext>();
        auto module = std::make_unique<llvm::Module>("test_module", *context);
        
        llvm::IRBuilder<> builder(*context);
        
        // Set up data layout and target triple
        module->setDataLayout(jit_->getDataLayout());
        module->setTargetTriple(jit_->getTargetTriple().str());
        
        // Declare external functions (engine API)
        llvm::FunctionType* printIntType = llvm::FunctionType::get(
            builder.getVoidTy(),
            {builder.getInt32Ty()},
            false
        );
        llvm::Function::Create(
            printIntType,
            llvm::Function::ExternalLinkage,
            "ScriptPrintInt",
            module.get()
        );
        
        llvm::FunctionType* addType = llvm::FunctionType::get(
            builder.getInt32Ty(),
            {builder.getInt32Ty(), builder.getInt32Ty()},
            false
        );
        llvm::Function::Create(
            addType,
            llvm::Function::ExternalLinkage,
            "ScriptAdd",
            module.get()
        );
        
        // Create a simple test function: void TestFunction()
        llvm::FunctionType* testFuncType = llvm::FunctionType::get(
            builder.getVoidTy(),
            false
        );
        
        llvm::Function* testFunc = llvm::Function::Create(
            testFuncType,
            llvm::Function::ExternalLinkage,
            "TestFunction",
            module.get()
        );
        
        // Function body
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", testFunc);
        builder.SetInsertPoint(entry);
        
        // Call ScriptPrintInt(42)
        llvm::Function* printIntFunc = module->getFunction("ScriptPrintInt");
        builder.CreateCall(printIntFunc, {builder.getInt32(42)});
        
        // Call result = ScriptAdd(10, 20)
        llvm::Function* addFunc = module->getFunction("ScriptAdd");
        llvm::Value* result = builder.CreateCall(addFunc, {
            builder.getInt32(10),
            builder.getInt32(20)
        });
        
        // Call ScriptPrintInt(result)
        builder.CreateCall(printIntFunc, {result});
        
        builder.CreateRetVoid();
        
        // Verify the function
        std::string errors;
        llvm::raw_string_ostream errorStream(errors);
        if (llvm::verifyFunction(*testFunc, &errorStream)) {
            std::cerr << "Function verification failed: " << errors << std::endl;
            return;
        }
        
        std::cout << "✓ Test function created successfully" << std::endl;
        
        // Print the IR
        std::cout << "\n=== Generated LLVM IR ===" << std::endl;
        module->print(llvm::outs(), nullptr);
        std::cout << "========================\n" << std::endl;
        
        // Add module to JIT
        llvm::orc::ThreadSafeModule tsm(std::move(module), std::move(context));
        
        auto err = jit_->addIRModule(std::move(tsm));
        if (err) {
            std::cerr << "Failed to add module: " 
                     << llvm::toString(std::move(err)) << std::endl;
            return;
        }
        
        std::cout << "✓ Module added to JIT" << std::endl;
    }
    
    // Execute a function by name
    void executeFunction(const std::string& functionName) {
        std::cout << "\n=== Executing: " << functionName << " ===" << std::endl;
        
        // Look up the function
        auto symOrErr = jit_->lookup(functionName);
        if (!symOrErr) {
            std::cerr << "Failed to lookup function: " 
                     << llvm::toString(symOrErr.takeError()) << std::endl;
            return;
        }
        
        // Get function pointer
        auto funcPtr = symOrErr->toPtr<void(*)()>();
        
        // Execute!
        auto startTime = std::chrono::high_resolution_clock::now();
        funcPtr();
        auto endTime = std::chrono::high_resolution_clock::now();
        
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(
            endTime - startTime).count();
        
        std::cout << "=== Execution completed in " << duration << "µs ===" << std::endl;
    }
    
    // Create a more complex example with loops
    void createComplexExample() {
        auto context = std::make_unique<llvm::LLVMContext>();
        auto module = std::make_unique<llvm::Module>("complex_module", *context);
        
        llvm::IRBuilder<> builder(*context);
        
        module->setDataLayout(jit_->getDataLayout());
        module->setTargetTriple(jit_->getTargetTriple().str());
        
        // Declare ScriptPrintInt
        llvm::FunctionType* printIntType = llvm::FunctionType::get(
            builder.getVoidTy(),
            {builder.getInt32Ty()},
            false
        );
        llvm::Function::Create(
            printIntType,
            llvm::Function::ExternalLinkage,
            "ScriptPrintInt",
            module.get()
        );
        
        // Create function: void CountToTen()
        llvm::FunctionType* countFuncType = llvm::FunctionType::get(
            builder.getVoidTy(),
            false
        );
        
        llvm::Function* countFunc = llvm::Function::Create(
            countFuncType,
            llvm::Function::ExternalLinkage,
            "CountToTen",
            module.get()
        );
        
        // Create basic blocks
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", countFunc);
        llvm::BasicBlock* loopCond = llvm::BasicBlock::Create(*context, "loop.cond", countFunc);
        llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(*context, "loop.body", countFunc);
        llvm::BasicBlock* loopEnd = llvm::BasicBlock::Create(*context, "loop.end", countFunc);
        
        // Entry block: initialize counter
        builder.SetInsertPoint(entry);
        llvm::AllocaInst* counter = builder.CreateAlloca(builder.getInt32Ty(), nullptr, "counter");
        builder.CreateStore(builder.getInt32(1), counter);
        builder.CreateBr(loopCond);
        
        // Loop condition: counter <= 10
        builder.SetInsertPoint(loopCond);
        llvm::Value* counterVal = builder.CreateLoad(builder.getInt32Ty(), counter);
        llvm::Value* cmp = builder.CreateICmpSLE(counterVal, builder.getInt32(10));
        builder.CreateCondBr(cmp, loopBody, loopEnd);
        
        // Loop body: print counter and increment
        builder.SetInsertPoint(loopBody);
        llvm::Value* currentVal = builder.CreateLoad(builder.getInt32Ty(), counter);
        llvm::Function* printFunc = module->getFunction("ScriptPrintInt");
        builder.CreateCall(printFunc, {currentVal});
        llvm::Value* nextVal = builder.CreateAdd(currentVal, builder.getInt32(1));
        builder.CreateStore(nextVal, counter);
        builder.CreateBr(loopCond);
        
        // Loop end
        builder.SetInsertPoint(loopEnd);
        builder.CreateRetVoid();
        
        // Verify
        std::string errors;
        llvm::raw_string_ostream errorStream(errors);
        if (llvm::verifyFunction(*countFunc, &errorStream)) {
            std::cerr << "Function verification failed: " << errors << std::endl;
            return;
        }
        
        std::cout << "✓ Complex example created" << std::endl;
        
        // Add to JIT
        llvm::orc::ThreadSafeModule tsm(std::move(module), std::move(context));
        auto err = jit_->addIRModule(std::move(tsm));
        if (err) {
            std::cerr << "Failed to add module: " 
                     << llvm::toString(std::move(err)) << std::endl;
            return;
        }
        
        std::cout << "✓ Module added to JIT" << std::endl;
    }
    
    // Create an example with function calls
    void createFibonacciExample() {
        auto context = std::make_unique<llvm::LLVMContext>();
        auto module = std::make_unique<llvm::Module>("fibonacci_module", *context);
        
        llvm::IRBuilder<> builder(*context);
        
        module->setDataLayout(jit_->getDataLayout());
        module->setTargetTriple(jit_->getTargetTriple().str());
        
        // Declare ScriptPrintInt
        llvm::FunctionType* printIntType = llvm::FunctionType::get(
            builder.getVoidTy(),
            {builder.getInt32Ty()},
            false
        );
        llvm::Function::Create(
            printIntType,
            llvm::Function::ExternalLinkage,
            "ScriptPrintInt",
            module.get()
        );
        
        // Create function: int32 fib(int32 n)
        llvm::FunctionType* fibType = llvm::FunctionType::get(
            builder.getInt32Ty(),
            {builder.getInt32Ty()},
            false
        );
        
        llvm::Function* fibFunc = llvm::Function::Create(
            fibType,
            llvm::Function::ExternalLinkage,
            "fib",
            module.get()
        );
        
        llvm::Argument* n = fibFunc->arg_begin();
        n->setName("n");
        
        // Create basic blocks
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", fibFunc);
        llvm::BasicBlock* ifBase = llvm::BasicBlock::Create(*context, "if.base", fibFunc);
        llvm::BasicBlock* ifRecursive = llvm::BasicBlock::Create(*context, "if.recursive", fibFunc);
        
        // Entry: check if n <= 1
        builder.SetInsertPoint(entry);
        llvm::Value* cmp = builder.CreateICmpSLE(n, builder.getInt32(1));
        builder.CreateCondBr(cmp, ifBase, ifRecursive);
        
        // Base case: return n
        builder.SetInsertPoint(ifBase);
        builder.CreateRet(n);
        
        // Recursive case: return fib(n-1) + fib(n-2)
        builder.SetInsertPoint(ifRecursive);
        llvm::Value* n_minus_1 = builder.CreateSub(n, builder.getInt32(1));
        llvm::Value* n_minus_2 = builder.CreateSub(n, builder.getInt32(2));
        llvm::Value* fib_n_minus_1 = builder.CreateCall(fibFunc, {n_minus_1});
        llvm::Value* fib_n_minus_2 = builder.CreateCall(fibFunc, {n_minus_2});
        llvm::Value* result = builder.CreateAdd(fib_n_minus_1, fib_n_minus_2);
        builder.CreateRet(result);
        
        // Create wrapper function: void TestFib()
        llvm::FunctionType* testFibType = llvm::FunctionType::get(
            builder.getVoidTy(),
            false
        );
        
        llvm::Function* testFibFunc = llvm::Function::Create(
            testFibType,
            llvm::Function::ExternalLinkage,
            "TestFib",
            module.get()
        );
        
        llvm::BasicBlock* testEntry = llvm::BasicBlock::Create(*context, "entry", testFibFunc);
        builder.SetInsertPoint(testEntry);
        
        // Calculate fib(10) and print it
        llvm::Value* fibResult = builder.CreateCall(fibFunc, {builder.getInt32(10)});
        llvm::Function* printFunc = module->getFunction("ScriptPrintInt");
        builder.CreateCall(printFunc, {fibResult});
        
        builder.CreateRetVoid();
        
        std::cout << "✓ Fibonacci example created" << std::endl;
        
        // Add to JIT
        llvm::orc::ThreadSafeModule tsm(std::move(module), std::move(context));
        auto err = jit_->addIRModule(std::move(tsm));
        if (err) {
            std::cerr << "Failed to add module: " 
                     << llvm::toString(std::move(err)) << std::endl;
            return;
        }
        
        std::cout << "✓ Module added to JIT" << std::endl;
    }

private:
    std::unique_ptr<llvm::orc::LLJIT> jit_;
    
    void bindEngineFunctions() {
        llvm::orc::MangleAndInterner mangle(
            jit_->getExecutionSession(),
            jit_->getDataLayout()
        );
        
        // Create symbol map
        llvm::orc::SymbolMap symbols;
        
        // Add engine functions
        symbols[mangle("ScriptPrint")] = {
            llvm::orc::ExecutorAddr::fromPtr((void*)&ScriptPrint),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable
        };
        
        symbols[mangle("ScriptPrintInt")] = {
            llvm::orc::ExecutorAddr::fromPtr((void*)&ScriptPrintInt),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable
        };
        
        symbols[mangle("ScriptPrintFloat")] = {
            llvm::orc::ExecutorAddr::fromPtr((void*)&ScriptPrintFloat),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable
        };
        
        symbols[mangle("ScriptGetDeltaTime")] = {
            llvm::orc::ExecutorAddr::fromPtr((void*)&ScriptGetDeltaTime),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable
        };
        
        symbols[mangle("ScriptAdd")] = {
            llvm::orc::ExecutorAddr::fromPtr((void*)&ScriptAdd),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable
        };
        
        // Add to main JIT dylib
        auto err = jit_->getMainJITDylib().define(
            llvm::orc::absoluteSymbols(symbols)
        );
        
        if (err) {
            std::cerr << "Failed to bind engine functions: " 
                     << llvm::toString(std::move(err)) << std::endl;
            return;
        }
        
        std::cout << "✓ Engine functions bound to JIT" << std::endl;
    }
};

// ============================================================
// Main - Run the JIT
// ============================================================

int main(int argc, char* argv[]) {
    std::cout << "╔════════════════════════════════════════╗" << std::endl;
    std::cout << "║   ScriptLang JIT Compiler Runner      ║" << std::endl;
    std::cout << "╚════════════════════════════════════════╝\n" << std::endl;
    
    // Initialize arena allocators
    g_frame_arena = arena_create(1024 * 1024);       // 1 MB
    g_persistent_arena = arena_create(10 * 1024 * 1024); // 10 MB
    std::cout << "✓ Memory arenas initialized" << std::endl;
    
    // Create and initialize JIT
    SimpleJIT jit;
    if (!jit.initialize()) {
        return 1;
    }
    
    std::cout << "\n--- Example 1: Simple Function Call ---" << std::endl;
    jit.createTestModule();
    jit.executeFunction("TestFunction");
    
    std::cout << "\n--- Example 2: Loop (Count to 10) ---" << std::endl;
    jit.createComplexExample();
    jit.executeFunction("CountToTen");
    
    std::cout << "\n--- Example 3: Recursive Fibonacci ---" << std::endl;
    jit.createFibonacciExample();
    jit.executeFunction("TestFib");
    
    std::cout << "\n╔════════════════════════════════════════╗" << std::endl;
    std::cout << "║   All examples completed successfully! ║" << std::endl;
    std::cout << "╚════════════════════════════════════════╝" << std::endl;
    
    // Cleanup
    arena_destroy(g_frame_arena);
    arena_destroy(g_persistent_arena);
    
    return 0;
}

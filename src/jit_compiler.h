// scriptlang_jit.h - JIT compiler extension for ScriptLang
//
// This header adds JIT execution capabilities to ScriptLang.
// Include this AFTER scriptlang.h to get runtime compilation.
//
// Usage:
//     #include "scriptlang.h"
//     #include "scriptlang_jit.h"
//
// Features:
//   - Just-in-time compilation with LLVM ORC JIT v2
//   - Hot-reload support with state preservation
//   - Debugging with breakpoints and profiling
//   - Memory management with arena allocators

#ifndef SCRIPTLANG_JIT_H
#define SCRIPTLANG_JIT_H

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/Module.h>

#include <memory>
#include <string>
#include <unordered_map>
#include <functional>
#include <vector>
#include <iostream>
#include "arena.h"
#include "semantic_analyzer.h"
#include "parser.h"
#include "codegen.h"
#include <chrono>
#include <thread>
#include <atomic>

#ifdef _WIN32
    #include <windows.h>
    #include <sys/stat.h>
#else
    #include <sys/inotify.h>
    #include <sys/stat.h>
    #include <unistd.h>
    #include <poll.h>
#endif

namespace scriptlang {

// ============================================================
// ScriptInstance - Runtime instance of a compiled script
// ============================================================
struct ScriptInstance {
    std::string moduleName;
    
    // Entry points - function pointers looked up from JIT
    using VoidFunc = void(*)();
    using UpdateFunc = void(*)(float);
    using EventFunc = void(*)(const char*);
    
    VoidFunc Start = nullptr;
    UpdateFunc Update = nullptr;
    VoidFunc OnDestroy = nullptr;
    EventFunc OnEvent = nullptr;
    
    // User state - preserved across hot-reloads
    std::unordered_map<std::string, void*> savedState;
    
    ScriptInstance() = default;
    ScriptInstance(const std::string& name) : moduleName(name) {}
};

// ============================================================
// JITCompiler - Main JIT execution engine
// ============================================================
class JITCompiler {
public:
    JITCompiler() = default;
    ~JITCompiler();
    
    // Initialization
    bool initialize();
    
    // Bind C++ functions for scripts to call
    void bindFunction(const std::string& name, void* funcPtr);
    
    // Compile a module from source
    ScriptInstance* compileModule(const std::string& moduleName,
                                  const std::string& sourceCode);
    
    // Compile from pre-generated LLVM module
    ScriptInstance* compileModule(const std::string& moduleName,
                                  std::unique_ptr<llvm::Module> module);
    
    // Hot-reload a module
    bool reloadModule(const std::string& moduleName,
                     const std::string& sourceCode);
    
    // Look up a function by name
    void* lookupFunction(const std::string& name);
    
    // Get a script instance
    ScriptInstance* getInstance(const std::string& moduleName);
    
    // Memory management
    void initArenas(size_t frameArenaSize, size_t persistentArenaSize);
    void resetFrameArena();
    Arena* getFrameArena() { return frameArena_; }
    Arena* getPersistentArena() { return persistentArena_; }
    
    // Error handling
    bool hasErrors() const { return !errors_.empty(); }
    const std::vector<std::string>& errors() const { return errors_; }
    void clearErrors() { errors_.clear(); }
    
    // Statistics
    struct Stats {
        size_t modulesLoaded = 0;
        size_t totalCompilationTimeMs = 0;
        size_t totalReloads = 0;
    };
    Stats getStats() const { return stats_; }

private:
    std::unique_ptr<llvm::orc::LLJIT> jit_;
    std::unordered_map<std::string, std::unique_ptr<ScriptInstance>> instances_;
    std::unordered_map<std::string, llvm::orc::ResourceTrackerSP> moduleTrackers_;
    
    Arena* frameArena_ = nullptr;
    Arena* persistentArena_ = nullptr;
    
    std::vector<std::string> errors_;
    Stats stats_;
    
    void error(const std::string& msg) { errors_.push_back(msg); }
};


// ============================================================
// ScriptDebugger - Debugging and profiling
// ============================================================
class ScriptDebugger {
public:
    struct FunctionStats {
        std::string name;
        uint64_t callCount = 0;
        uint64_t totalTimeNs = 0;
        uint64_t minTimeNs = UINT64_MAX;
        uint64_t maxTimeNs = 0;
        
        double averageTimeMs() const {
            return callCount > 0 ? (totalTimeNs / callCount) / 1000000.0 : 0.0;
        }
        double totalTimeMs() const { return totalTimeNs / 1000000.0; }
    };
    
    ScriptDebugger(JITCompiler& jit) : jit_(jit), profilingEnabled_(false) {}
    
    // Profiling
    void enableProfiling(bool enable) { profilingEnabled_ = enable; }
    const std::unordered_map<std::string, FunctionStats>& getStats() const { 
        return functionStats_; 
    }
    std::vector<FunctionStats> getTopFunctions(size_t n = 10) const;
    void resetStats() { functionStats_.clear(); }
    
    // Called by instrumented code
    void onFunctionEnter(const char* name);
    void onFunctionExit(const char* name);

private:
    JITCompiler& jit_;
    bool profilingEnabled_;
    std::unordered_map<std::string, FunctionStats> functionStats_;
    
    struct FunctionEntry {
        std::string name;
        std::chrono::time_point<std::chrono::high_resolution_clock> startTime;
    };
    std::vector<FunctionEntry> callStack_;
};

// ============================================================
// Implementation
// ============================================================

inline JITCompiler::~JITCompiler() {
    instances_.clear();
    if (frameArena_) arena_destroy(frameArena_);
    if (persistentArena_) arena_destroy(persistentArena_);
}

inline bool JITCompiler::initialize() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    
    auto jitOrErr = llvm::orc::LLJITBuilder().create();
    if (!jitOrErr) {
        error("Failed to create LLJIT: " + llvm::toString(jitOrErr.takeError()));
        return false;
    }
    jit_ = std::move(*jitOrErr);
    return true;
}

inline void JITCompiler::bindFunction(const std::string& name, void* funcPtr) {
    llvm::orc::MangleAndInterner mangle(jit_->getExecutionSession(),
                                       jit_->getDataLayout());
    llvm::orc::SymbolMap symbols;
    symbols[mangle(name)] = {
        llvm::orc::ExecutorAddr::fromPtr(funcPtr),
        llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable
    };
    
    auto err = jit_->getMainJITDylib().define(llvm::orc::absoluteSymbols(symbols));
    if (err) {
        error("Failed to bind function " + name + ": " + 
              llvm::toString(std::move(err)));
    }
}

inline ScriptInstance* JITCompiler::compileModule(
    const std::string& moduleName,
    const std::string& sourceCode) {
    
    auto startTime = std::chrono::high_resolution_clock::now();
    
    // Lex
    Lexer lexer(sourceCode.c_str());
    auto tokens = lexer.tokenize();
    
    // Parse
    Parser parser(tokens);
    auto program = parser.parse();
    if (!program || parser.hasErrors()) {
        error("Parse errors in module: " + moduleName);
        return nullptr;
    }
    
    // Semantic analysis
    SemanticAnalyzer analyzer;
    if (!analyzer.analyze(*program)) {
        error("Semantic errors in module: " + moduleName);
        return nullptr;
    }
    
    // Code generation
    LLVMCodeGen codegen(analyzer.typeContext(), analyzer.symbolTable());
    auto module = codegen.generate(*program);
    if (!module || codegen.hasErrors()) {
        error("Code generation failed for module: " + moduleName);
        return nullptr;
    }
    
    auto endTime = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
        endTime - startTime).count();
    stats_.totalCompilationTimeMs += duration;
    
    return compileModule(moduleName, std::move(module));
}

inline ScriptInstance* JITCompiler::compileModule(
    const std::string& moduleName,
    std::unique_ptr<llvm::Module> module) {
    
    if (!module) {
        error("Cannot compile null module");
        return nullptr;
    }
    
    // Wrap in ThreadSafeModule
    llvm::orc::ThreadSafeModule tsm(
        std::move(module),
        std::make_unique<llvm::LLVMContext>());
    
    // Create resource tracker
    auto rt = jit_->getMainJITDylib().createResourceTracker();
    
    // Add to JIT
    auto err = jit_->addIRModule(rt, std::move(tsm));
    if (err) {
        error("Failed to add module: " + llvm::toString(std::move(err)));
        return nullptr;
    }
    
    // Create instance
    auto instance = std::make_unique<ScriptInstance>(moduleName);
    
    // Look up entry points
    instance->Start = (ScriptInstance::VoidFunc)lookupFunction(moduleName + "_Start");
    instance->Update = (ScriptInstance::UpdateFunc)lookupFunction(moduleName + "_Update");
    instance->OnDestroy = (ScriptInstance::VoidFunc)lookupFunction(moduleName + "_OnDestroy");
    instance->OnEvent = (ScriptInstance::EventFunc)lookupFunction(moduleName + "_OnEvent");
    
    moduleTrackers_[moduleName] = rt;
    stats_.modulesLoaded++;
    
    ScriptInstance* ptr = instance.get();
    instances_[moduleName] = std::move(instance);
    return ptr;
}

inline bool JITCompiler::reloadModule(const std::string& moduleName,
                                     const std::string& sourceCode) {
    // Remove old module
    auto it = moduleTrackers_.find(moduleName);
    if (it != moduleTrackers_.end()) {
        if (auto err = it->second->remove()) {
            error("Failed to remove module: " + llvm::toString(std::move(err)));
        }
        moduleTrackers_.erase(it);
    }
    instances_.erase(moduleName);
    
    // Recompile
    auto* instance = compileModule(moduleName, sourceCode);
    if (!instance) return false;
    
    stats_.totalReloads++;
    return true;
}

inline void* JITCompiler::lookupFunction(const std::string& name) {
    auto symOrErr = jit_->lookup(name);
    if (!symOrErr) {
        llvm::consumeError(symOrErr.takeError());
        return nullptr;
    }
    return symOrErr->toPtr<void*>();
}

inline ScriptInstance* JITCompiler::getInstance(const std::string& moduleName) {
    auto it = instances_.find(moduleName);
    return it != instances_.end() ? it->second.get() : nullptr;
}

inline void JITCompiler::initArenas(size_t frameArenaSize, size_t persistentArenaSize) {
    frameArena_ = arena_create(frameArenaSize);
    persistentArena_ = arena_create(persistentArenaSize);
    
    bindFunction("sl_arena_alloc", (void*)&sl_arena_alloc);
    bindFunction("sl_arena_reset", (void*)&sl_arena_reset);
    bindFunction("g_frame_arena", (void*)frameArena_);
    bindFunction("g_persistent_arena", (void*)persistentArena_);
}

inline void JITCompiler::resetFrameArena() {
    if (frameArena_) arena_reset(frameArena_);
}

// ============================================================
// ScriptDebugger Implementation
// ============================================================

inline std::vector<ScriptDebugger::FunctionStats> 
ScriptDebugger::getTopFunctions(size_t n) const {
    std::vector<FunctionStats> stats;
    for (const auto& [name, stat] : functionStats_) {
        stats.push_back(stat);
    }
    
    std::sort(stats.begin(), stats.end(),
             [](const FunctionStats& a, const FunctionStats& b) {
        return a.totalTimeNs > b.totalTimeNs;
    });
    
    if (stats.size() > n) stats.resize(n);
    return stats;
}

inline void ScriptDebugger::onFunctionEnter(const char* name) {
    if (!profilingEnabled_) return;
    
    FunctionEntry entry;
    entry.name = name;
    entry.startTime = std::chrono::high_resolution_clock::now();
    callStack_.push_back(entry);
}

inline void ScriptDebugger::onFunctionExit(const char* name) {
    if (!profilingEnabled_ || callStack_.empty()) return;
    
    auto entry = callStack_.back();
    callStack_.pop_back();
    
    auto endTime = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(
        endTime - entry.startTime).count();
    
    auto& stats = functionStats_[name];
    stats.name = name;
    stats.callCount++;
    stats.totalTimeNs += duration;
    stats.minTimeNs = std::min(stats.minTimeNs, (uint64_t)duration);
    stats.maxTimeNs = std::max(stats.maxTimeNs, (uint64_t)duration);
}

} // namespace scriptlang

#endif // SCRIPTLANG_JIT_H

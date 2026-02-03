// ============================================================
// ScriptLang Public API
// ============================================================
// This is the ONLY header users need to include.
// 
// Example usage:
//     #include "scriptlang_api.h"
//     
//     ScriptLang::Runtime runtime;
//     runtime.initialize();
//     runtime.compileAndRun("var x = 10; Print(x);");
//
// ============================================================

#ifndef SCRIPTLANG_API_H
#define SCRIPTLANG_API_H

#include <string>
#include <vector>
#include <functional>
#include <memory>
#include <cstdint>

namespace ScriptLang {

// ============================================================
// Forward Declarations (hide implementation details)
// ============================================================
namespace Internal {
    class RuntimeImpl;
    class ModuleImpl;
}

// ============================================================
// Error/Diagnostic Information
// ============================================================
enum class DiagnosticLevel {
    Error,
    Warning,
    Note
};

struct Diagnostic {
    DiagnosticLevel level;
    std::string message;
    int line;
    int column;
    
    Diagnostic(DiagnosticLevel lvl, std::string msg, int l, int c)
        : level(lvl), message(std::move(msg)), line(l), column(c) {}
};

// ============================================================
// Compiled Module Handle
// ============================================================
class Module {
public:
    Module();
    ~Module();
    
    // Query module state
    bool isValid() const;
    const std::vector<Diagnostic>& getDiagnostics() const;
    bool hasErrors() const;
    
    // Get list of exported functions
    std::vector<std::string> getExportedFunctions() const;
    
    // Print LLVM IR (for debugging)
    std::string getIR() const;
    
private:
    friend class Runtime;
    std::unique_ptr<Internal::ModuleImpl> impl_;
};

// ============================================================
// Script Value - Type-safe wrapper for script values
// ============================================================
class Value {
public:
    enum class Type {
        Void,
        Int32,
        Int64,
        Float32,
        Float64,
        Bool,
        String,
        Null
    };
    
    Value();
    explicit Value(int32_t v);
    explicit Value(int64_t v);
    explicit Value(float v);
    explicit Value(double v);
    explicit Value(bool v);
    explicit Value(const char* v);
    explicit Value(const std::string& v);
    
    Type getType() const { return type_; }
    
    // Type checking
    bool isInt32() const { return type_ == Type::Int32; }
    bool isInt64() const { return type_ == Type::Int64; }
    bool isFloat32() const { return type_ == Type::Float32; }
    bool isFloat64() const { return type_ == Type::Float64; }
    bool isBool() const { return type_ == Type::Bool; }
    bool isString() const { return type_ == Type::String; }
    bool isVoid() const { return type_ == Type::Void; }
    bool isNull() const { return type_ == Type::Null; }
    
    // Value extraction (throws if wrong type)
    int32_t asInt32() const;
    int64_t asInt64() const;
    float asFloat32() const;
    double asFloat64() const;
    bool asBool() const;
    std::string asString() const;
    
private:
    Type type_;
    union {
        int32_t i32;
        int64_t i64;
        float f32;
        double f64;
        bool b;
    } data_;
    std::string stringData_;
};

// ============================================================
// Function Binding - Call C++ from scripts
// ============================================================
using NativeFunction = std::function<Value(const std::vector<Value>&)>;

// Helper macros for easier function binding
#define SCRIPTLANG_BIND_FUNC_0(name, func) \
    runtime.bindFunction(name, [](const std::vector<ScriptLang::Value>&) { \
        func(); return ScriptLang::Value(); \
    })

#define SCRIPTLANG_BIND_FUNC_1(name, func) \
    runtime.bindFunction(name, [](const std::vector<ScriptLang::Value>& args) { \
        return ScriptLang::Value(func(args[0])); \
    })

#define SCRIPTLANG_BIND_FUNC_2(name, func) \
    runtime.bindFunction(name, [](const std::vector<ScriptLang::Value>& args) { \
        return ScriptLang::Value(func(args[0], args[1])); \
    })

// ============================================================
// Main Runtime Interface
// ============================================================
class Runtime {
public:
    Runtime();
    ~Runtime();
    
    // --------------------------------------------------------
    // Initialization
    // --------------------------------------------------------
    
    /// Initialize the runtime (must be called first)
    bool initialize();
    
    /// Check if initialized
    bool isInitialized() const;
    
    // --------------------------------------------------------
    // Compilation
    // --------------------------------------------------------
    
    /// Compile source code to a module
    Module compile(const std::string& sourceCode);
    
    /// Compile from file
    Module compileFile(const std::string& filepath);
    
    /// Quick compile and execute (for REPL/testing)
    bool compileAndRun(const std::string& sourceCode);
    
    // --------------------------------------------------------
    // Module Management
    // --------------------------------------------------------
    
    /// Load a compiled module into the runtime
    bool loadModule(const Module& module, const std::string& moduleName = "main");
    
    /// Unload a module
    void unloadModule(const std::string& moduleName);
    
    /// Check if module is loaded
    bool hasModule(const std::string& moduleName) const;
    
    // --------------------------------------------------------
    // Function Calls (C++ → Script)
    // --------------------------------------------------------
    
    /// Call a script function by name
    Value callFunction(const std::string& functionName);
    Value callFunction(const std::string& functionName, const std::vector<Value>& args);
    
    /// Call a function from a specific module
    Value callFunction(const std::string& moduleName, 
                      const std::string& functionName,
                      const std::vector<Value>& args = {});
    
    // --------------------------------------------------------
    // Function Binding (Script → C++)
    // --------------------------------------------------------
    
    /// Bind a C++ function to be callable from scripts
    void bindFunction(const std::string& name, NativeFunction func);
    
    /// Bind a simple void function
    void bindFunction(const std::string& name, std::function<void()> func);
    
    /// Bind common C functions
    void bindPrintf();  // Binds printf-style Print() function
    
    // --------------------------------------------------------
    // Variable Access (C++ ↔ Script)
    // --------------------------------------------------------
    
    /// Get a global variable value
    Value getGlobal(const std::string& name) const;
    
    /// Set a global variable value
    void setGlobal(const std::string& name, const Value& value);
    
    /// Check if global exists
    bool hasGlobal(const std::string& name) const;
    
    // --------------------------------------------------------
    // Memory Management
    // --------------------------------------------------------
    
    /// Set arena sizes (must be called before initialize)
    void setFrameArenaSize(size_t bytes);       // Default: 1 MB
    void setPersistentArenaSize(size_t bytes);  // Default: 10 MB
    
    /// Reset frame arena (call once per frame in game loop)
    void resetFrameArena();
    
    /// Get memory usage statistics
    struct MemoryStats {
        size_t frameArenaUsed;
        size_t frameArenaCapacity;
        size_t persistentArenaUsed;
        size_t persistentArenaCapacity;
    };
    MemoryStats getMemoryStats() const;
    
    // --------------------------------------------------------
    // Hot Reload Support
    // --------------------------------------------------------
    
    /// Enable hot reload for a module (watches file for changes)
    void enableHotReload(const std::string& moduleName, 
                        const std::string& filepath);
    
    /// Disable hot reload
    void disableHotReload(const std::string& moduleName);
    
    /// Set callback for reload events
    using ReloadCallback = std::function<void(const std::string& moduleName, bool success)>;
    void setReloadCallback(ReloadCallback callback);
    
    /// Manually trigger a reload
    bool reloadModule(const std::string& moduleName);
    
    // --------------------------------------------------------
    // Debugging & Introspection
    // --------------------------------------------------------
    
    /// Dump LLVM IR for debugging
    std::string dumpModuleIR(const std::string& moduleName) const;
    
    /// Get list of loaded modules
    std::vector<std::string> getLoadedModules() const;
    
    /// Get list of functions in a module
    std::vector<std::string> getFunctions(const std::string& moduleName) const;
    
    /// Get list of global variables in a module
    std::vector<std::string> getGlobals(const std::string& moduleName) const;
    
    /// Enable/disable profiling
    void enableProfiling(bool enable);
    
    /// Get profiling statistics
    struct ProfilingStats {
        std::string functionName;
        uint64_t callCount;
        double averageTimeMs;
        double totalTimeMs;
    };
    std::vector<ProfilingStats> getProfilingStats() const;
    void resetProfilingStats();
    
    // --------------------------------------------------------
    // Error Handling
    // --------------------------------------------------------
    
    /// Get last error message
    std::string getLastError() const;
    
    /// Check if there were errors in last operation
    bool hasErrors() const;
    
    /// Get all diagnostics from last operation
    const std::vector<Diagnostic>& getDiagnostics() const;
    
    /// Clear error state
    void clearErrors();
    
    // --------------------------------------------------------
    // Configuration
    // --------------------------------------------------------
    
    /// Set optimization level (0-3, default: 2)
    void setOptimizationLevel(int level);
    
    /// Enable/disable JIT (vs AOT compilation)
    void setJITEnabled(bool enabled);
    
    /// Set maximum execution time for scripts (milliseconds, 0 = unlimited)
    void setExecutionTimeout(uint64_t timeoutMs);
    
private:
    std::unique_ptr<Internal::RuntimeImpl> impl_;
};

// ============================================================
// Convenience Functions (for simple use cases)
// ============================================================

/// Quick execute - compile and run in one call
inline bool execute(const std::string& sourceCode) {
    Runtime runtime;
    runtime.initialize();
    return runtime.compileAndRun(sourceCode);
}

/// Quick execute from file
inline bool executeFile(const std::string& filepath) {
    Runtime runtime;
    runtime.initialize();
    auto module = runtime.compileFile(filepath);
    if (!module.isValid()) return false;
    return runtime.loadModule(module);
}

// ============================================================
// Version Information
// ============================================================

struct Version {
    int major;
    int minor;
    int patch;
    
    std::string toString() const {
        return std::to_string(major) + "." + 
               std::to_string(minor) + "." + 
               std::to_string(patch);
    }
};

/// Get ScriptLang version
Version getVersion();

/// Get LLVM version used
std::string getLLVMVersion();

} // namespace ScriptLang

// ============================================================
// Example Usage
// ============================================================
#if 0

// Example 1: Simple execution
ScriptLang::execute(R"(
    var x = 10;
    var y = 20;
    Print("Sum:", x + y);
)");

// Example 2: Full control
ScriptLang::Runtime runtime;
runtime.initialize();

// Bind C++ functions
runtime.bindFunction("GetPlayerHealth", []() { return 100; });
runtime.bindFunction("SetPlayerHealth", [](int health) { 
    playerHealth = health; 
});

// Compile and load
auto module = runtime.compile(R"(
    var health = GetPlayerHealth();
    Print("Health:", health);
    SetPlayerHealth(50);
)");

if (module.isValid()) {
    runtime.loadModule(module);
}

// Call script functions
auto result = runtime.callFunction("calculateDamage", {
    ScriptLang::Value(10),  // base damage
    ScriptLang::Value(1.5)  // multiplier
});
std::cout << "Damage: " << result.asInt32() << std::endl;

// Example 3: Hot reload
runtime.enableHotReload("gameplay", "scripts/gameplay.sl");
runtime.setReloadCallback([](const std::string& name, bool success) {
    if (success) {
        std::cout << "Reloaded: " << name << std::endl;
    }
});

// Game loop
while (running) {
    runtime.resetFrameArena();  // Clear frame allocations
    
    // Call update function
    runtime.callFunction("gameplay", "Update", {
        ScriptLang::Value(deltaTime)
    });
    
    render();
}

#endif

#endif // SCRIPTLANG_API_H

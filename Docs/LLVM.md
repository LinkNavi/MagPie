# ScriptLang LLVM Compilation Roadmap

## Goal: Maximum Performance Game Scripting Language

This roadmap will take you from your current AST parser to a fully optimizing, 
native-code-generating compiler suitable for AAA game engines.

---

## Architecture Overview

```
Source Code (.mp)
    ↓
Lexer → Tokens
    ↓
Parser → AST (✅ YOU ARE HERE)
    ↓
Semantic Analysis → Typed AST
    ↓
LLVM IR Generation
    ↓
LLVM Optimization Passes
    ↓
Machine Code (x86/ARM/etc.)
```

---

## Phase 1: Semantic Analysis & Type System (2-4 weeks)

Before generating code, you need to:

### 1.1 Type Checking
```cpp
class TypeChecker {
    // Visit AST and assign types to all expressions
    TypePtr inferType(const Expression* expr);
    void checkType(const Expression* expr, TypePtr expected);
    
    // Build symbol tables
    SymbolTable globals;
    SymbolTable currentScope;
};
```

**Key types to support:**
- Primitives: int8, int16, int32, int64, float32, float64, bool
- Compound: string, arrays, classes
- Functions: (int32, int32) -> int32

### 1.2 Symbol Resolution
```cpp
class SymbolResolver {
    // Resolve all identifiers to their declarations
    void resolveProgram(Program* ast);
    
    // Bind each Identifier node to its VarDecl/FunctionDecl
    std::unordered_map<IdentifierExpr*, Declaration*> bindings;
};
```

### 1.3 Optimization-Friendly IR (Optional but recommended)
```cpp
// Convert AST to a simpler IR before LLVM
// This makes optimizations easier to write
class IRBuilder {
    std::vector<BasicBlock> blocks;
    
    // SSA form - each variable assigned exactly once
    void buildSSA(const Statement* stmt);
};
```

**Files to create:**
- `src/types.h` - Type system
- `src/symbol_table.h` - Scoped symbol tables
- `src/type_checker.cpp` - Type inference & checking
- `src/semantic_analyzer.cpp` - Full semantic pass

---

## Phase 2: LLVM Integration (2-3 weeks)

### 2.1 Setup LLVM (1 day)

**Install LLVM 18 (latest stable):**
```bash
# Ubuntu/Debian
sudo apt install llvm-18 llvm-18-dev

# macOS
brew install llvm@18

# Or build from source for maximum control
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
mkdir build && cd build
cmake -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release ../llvm
make -j$(nproc)
```

**Update meson.build:**
```meson
llvm_dep = dependency('llvm', version: '>=18.0')

executable('scriptlang_compiler',
    sources: ['src/main.cpp', 'src/codegen.cpp', ...],
    dependencies: [scriptlang_dep, llvm_dep]
)
```

### 2.2 Basic LLVM IR Generator (1-2 weeks)

```cpp
// src/llvm_codegen.h
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

class LLVMCodeGen {
public:
    LLVMCodeGen();
    
    // Generate LLVM IR from AST
    std::unique_ptr<llvm::Module> compile(const Program& ast);
    
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    
    // Symbol table mapping variables to LLVM values
    std::unordered_map<std::string, llvm::Value*> namedValues;
    
    // Code generation for different node types
    llvm::Value* codegen(const Expression* expr);
    llvm::Value* codegen(const Statement* stmt);
    llvm::Function* codegen(const FunctionDecl* func);
    
    // Type conversion
    llvm::Type* getLLVMType(const TypeAnnotation* type);
};
```

**Example implementation:**
```cpp
llvm::Value* LLVMCodeGen::codegen(const Expression* expr) {
    // Literal
    if (auto* lit = dynamic_cast<const LiteralExpr*>(expr)) {
        if (lit->kind == LiteralKind::Integer) {
            return llvm::ConstantInt::get(context, 
                llvm::APInt(32, std::stoi(lit->value)));
        }
        // ... other literals
    }
    
    // Binary operation
    if (auto* bin = dynamic_cast<const BinaryExpr*>(expr)) {
        llvm::Value* L = codegen(bin->left.get());
        llvm::Value* R = codegen(bin->right.get());
        
        switch (bin->op) {
            case BinaryOp::Add:
                return builder.CreateAdd(L, R, "addtmp");
            case BinaryOp::Mul:
                return builder.CreateMul(L, R, "multmp");
            // ... other ops
        }
    }
    
    // Variable reference
    if (auto* id = dynamic_cast<const IdentifierExpr*>(expr)) {
        llvm::Value* V = namedValues[id->name];
        return builder.CreateLoad(V->getType(), V, id->name);
    }
    
    // ... other expressions
}
```

### 2.3 Generate Object Files (2 days)

```cpp
// src/compiler.cpp
#include <llvm/Target/TargetMachine.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>

class Compiler {
public:
    void compileToObjectFile(const std::string& inputFile,
                           const std::string& outputFile) {
        // 1. Parse
        auto ast = parseFile(inputFile);
        
        // 2. Type check
        TypeChecker checker;
        checker.check(*ast);
        
        // 3. Generate LLVM IR
        LLVMCodeGen codegen;
        auto module = codegen.compile(*ast);
        
        // 4. Optimize
        optimizeModule(*module);
        
        // 5. Generate machine code
        emitObjectFile(*module, outputFile);
    }
    
private:
    void optimizeModule(llvm::Module& module) {
        using namespace llvm;
        
        // Create optimization pipeline
        PassBuilder PB;
        LoopAnalysisManager LAM;
        FunctionAnalysisManager FAM;
        CGSCCAnalysisManager CGAM;
        ModuleAnalysisManager MAM;
        
        // Register all analyses
        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
        
        // O3 optimization level
        ModulePassManager MPM = 
            PB.buildPerModuleDefaultPipeline(OptimizationLevel::O3);
        
        // Run optimizations
        MPM.run(module, MAM);
    }
    
    void emitObjectFile(llvm::Module& module, 
                       const std::string& filename) {
        using namespace llvm;
        
        // Initialize targets
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmParsers();
        InitializeAllAsmPrinters();
        
        // Get target triple
        auto TargetTriple = sys::getDefaultTargetTriple();
        module.setTargetTriple(TargetTriple);
        
        // Get target
        std::string Error;
        auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
        
        // Create target machine
        auto CPU = "generic";
        auto Features = "";
        TargetOptions opt;
        auto RM = Optional<Reloc::Model>();
        auto TheTargetMachine = Target->createTargetMachine(
            TargetTriple, CPU, Features, opt, RM);
        
        module.setDataLayout(TheTargetMachine->createDataLayout());
        
        // Emit object file
        std::error_code EC;
        raw_fd_ostream dest(filename, EC, sys::fs::OF_None);
        
        legacy::PassManager pass;
        auto FileType = CGFT_ObjectFile;
        
        TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType);
        pass.run(module);
        dest.flush();
    }
};
```

**Files to create:**
- `src/llvm_codegen.h/cpp` - LLVM IR generation
- `src/compiler.h/cpp` - Main compilation driver
- `src/optimizer.cpp` - Optimization passes

---

## Phase 3: Advanced Optimizations (4-8 weeks)

### 3.1 Game-Specific Optimizations

**Hot Loop Detection & Vectorization:**
```cpp
// Detect tight loops and vectorize them
class LoopVectorizer {
    void vectorizeLoop(llvm::Loop* L);
};

// Example: Vector operations on arrays
for (int i = 0; i < positions.length; i++) {
    positions[i].x += velocities[i].x * dt;
    positions[i].y += velocities[i].y * dt;
    positions[i].z += velocities[i].z * dt;
}
// → LLVM generates SIMD instructions (SSE/AVX/NEON)
```

**Devirtualization:**
```cpp
// Turn virtual calls into direct calls when type is known
class Devirtualizer {
    void devirtualize(CallExpr* call);
};

// Example:
// virtual function call → direct function call
enemy.takeDamage(10);  // If enemy type is known at compile-time
```

**Inlining Annotations:**
```cpp
// Use your @expose annotations to guide optimization
class AnnotationOptimizer {
    void processAnnotations(const ClassDecl* cls) {
        for (auto& member : cls->members) {
            for (auto& ann : member.annotations) {
                if (ann.name == "inline_always") {
                    // Force LLVM to inline this function
                    setAlwaysInline(member);
                }
            }
        }
    }
};
```

### 3.2 Escape Analysis & Stack Allocation
```cpp
// Allocate objects on stack instead of heap when possible
class EscapeAnalyzer {
    bool objectEscapes(const NewExpr* newExpr);
    void promoteToStack(const NewExpr* newExpr);
};

// Example:
void foo() {
    Player p = new Player();  // Doesn't escape
    p.health = 100;
}
// → Allocate Player on stack, not heap
```

### 3.3 Profile-Guided Optimization (PGO)
```cpp
// Instrument code to collect runtime profiles
class ProfileInstrumenter {
    void instrumentFunction(llvm::Function* F);
};

// Then recompile with profile data for better optimization
```

---

## Phase 4: JIT for Hot-Reloading (2-3 weeks)

For development iteration speed, add JIT compilation:

```cpp
#include <llvm/ExecutionEngine/Orc/LLJIT.h>

class JITCompiler {
public:
    JITCompiler() {
        auto JIT = llvm::orc::LLJITBuilder().create();
        jit = std::move(*JIT);
    }
    
    void* compileAndLoad(const std::string& source) {
        // Parse & generate IR
        auto module = compileToIR(source);
        
        // JIT compile
        auto err = jit->addIRModule(
            llvm::orc::ThreadSafeModule(
                std::move(module), 
                std::make_unique<llvm::LLVMContext>()
            )
        );
        
        // Get function pointer
        auto symbol = jit->lookup("main");
        return (void*)symbol->getAddress();
    }
    
private:
    std::unique_ptr<llvm::orc::LLJIT> jit;
};
```

**Use cases:**
- Live scripting in-editor
- Hot-reload scripts without restarting game
- Quick iteration during development

---

## Phase 5: Runtime System (2-4 weeks)

### 5.1 Garbage Collection (Optional)
```cpp
// Use LLVM's GC support
class GarbageCollector {
    void* allocate(size_t size);
    void collect();
    void markRoots();
};
```

**Or use reference counting for deterministic cleanup:**
```cpp
class RefCounted {
    std::atomic<int> refCount{0};
public:
    void addRef() { refCount++; }
    void release() { 
        if (--refCount == 0) delete this; 
    }
};
```

### 5.2 Native Interop
```cpp
// Call C++ functions from script
class NativeBindings {
    void registerFunction(const std::string& name, void* funcPtr) {
        // Expose to LLVM IR generator
        nativeFunctions[name] = funcPtr;
    }
};

// Usage:
bindings.registerFunction("GetDeltaTime", (void*)&Engine::getDeltaTime);
bindings.registerFunction("SpawnEntity", (void*)&World::spawnEntity);
```

### 5.3 Multithreading Support
```cpp
// Generate thread-safe code
class ThreadSafetyAnalyzer {
    void checkDataRaces(const Program* ast);
    void insertSynchronization(const Statement* stmt);
};
```

---

## Phase 6: Platform-Specific Optimizations (Ongoing)

### 6.1 SIMD Intrinsics
```cpp
// Expose SIMD operations to scripts
@simd
void processPositions(Vector3[] positions, Vector3[] velocities, float dt) {
    for (int i = 0; i < positions.length; i += 4) {
        // LLVM auto-vectorizes to SSE/AVX
        positions[i]   += velocities[i]   * dt;
        positions[i+1] += velocities[i+1] * dt;
        positions[i+2] += velocities[i+2] * dt;
        positions[i+3] += velocities[i+3] * dt;
    }
}
```

### 6.2 Console-Specific Optimizations
```cpp
// PS5/Xbox-specific code generation
if (targetPlatform == Platform::PS5) {
    // Use PS5-specific SIMD instructions
    usePS5SIMD();
}
```

---

## Benchmarking & Performance Targets

### Realistic Performance Goals:
- **Simple arithmetic:** 0.9-1.0x native C++ speed
- **Function calls:** 0.8-0.95x native (with inlining)
- **Object allocation:** 0.7-0.9x native (with escape analysis)
- **Hot loops:** 0.95-1.0x native (with vectorization)

### Comparison to other languages:
- **Lua (interpreted):** ~50-100x slower than native
- **Lua (LuaJIT):** ~2-5x slower than native
- **C# (Unity Burst):** ~0.9-1.0x native (similar to your approach)
- **Rust/C++:** 1.0x native (baseline)

---

## Development Timeline

### Assuming full-time work:

**Month 1-2: Semantic Analysis**
- Type system
- Symbol resolution
- Basic error reporting

**Month 3-4: LLVM IR Generation**
- Basic IR generation
- Function compilation
- Object file emission

**Month 5-6: Optimizations**
- LLVM optimization passes
- Inlining
- Dead code elimination

**Month 7-8: Runtime System**
- Memory management
- Native interop
- Standard library

**Month 9-10: Advanced Features**
- JIT compilation
- Profile-guided optimization
- Platform-specific optimizations

**Month 11-12: Polish & Testing**
- Benchmarking
- Bug fixes
- Documentation

---

## Recommended Learning Resources

### Books:
1. **"Engineering a Compiler"** by Cooper & Torczon
   - Best book on optimizing compilers
   
2. **"Modern Compiler Implementation in C"** by Andrew Appel
   - Practical compiler construction

3. **"Crafting Interpreters"** by Bob Nystrom
   - Great for understanding the basics

### Online:
1. **LLVM Tutorial (Kaleidoscope)**
   - https://llvm.org/docs/tutorial/
   - Build a JIT compiler step-by-step
   
2. **"Writing an LLVM Backend"**
   - https://llvm.org/docs/WritingAnLLVMBackend.html
   
3. **Compiler Explorer**
   - https://godbolt.org/
   - See what optimized code looks like

### Example Projects to Study:
1. **Swift Compiler** - https://github.com/apple/swift
   - LLVM-based, game-focused optimizations
   
2. **Julia** - https://github.com/JuliaLang/julia
   - JIT compilation with LLVM, scientific computing
   
3. **Rust Compiler** - https://github.com/rust-lang/rust
   - Advanced optimizations, LLVM backend
   
4. **Luau** (Roblox) - https://github.com/Roblox/luau
   - Bytecode + JIT for game scripting

---

## Next Immediate Steps

1. **Install LLVM 18**
2. **Create type system** (`src/types.h`)
3. **Build symbol table** (`src/symbol_table.h`)
4. **Write type checker** (`src/type_checker.cpp`)
5. **Start LLVM codegen** (`src/llvm_codegen.cpp`)

Then I can help you implement each phase in detail!

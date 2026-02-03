#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H

// ------------------------------------------------------------
// semantic_analyzer.h — The orchestrator pass that runs before
// (and wraps) the type checker.
//
// Responsibilities (in order):
//   1. Process #include directives: resolve engine headers and
//      populate the symbol table with the names they export.
//   2. Pre-register all top-level declarations (classes, structs,
//      enums, functions) so that forward references resolve.
//      This is a "name-only" pass — no bodies are checked yet.
//   3. Run the TypeChecker (which does the full DFS and fills in
//      exprTypes / stmtTypes).
//   4. Post-checks: unused symbols, annotation validation,
//      lifecycle hook validation.
//
// Usage:
//     SemanticAnalyzer analyzer;
//     auto result = analyzer.analyze(program);
//     if (result.hasErrors()) { … print diagnostics … }
//
// The analyzer owns its TypeContext, SymbolTable, and
// Diagnostics.  They are accessible after analysis for
// downstream passes (IR codegen).
// ------------------------------------------------------------

#include "ast.h"
#include "types.h"
#include "symbol_table.h"
#include "type_checker.h"

#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <functional>

namespace scriptlang {

// ============================================================
// EngineHeaderRegistry — stub database of what each engine
// header exports.
//
// In a real engine this would be populated from actual C++ header
// files or a schema.  For now it's a hand-written table that
// matches the headers referenced in the test .mp files.
// ============================================================
class EngineHeaderRegistry {
public:
    struct ExportedSymbol {
        std::string name;
        SymbolKind  kind;
        TypePtr     type;   // may be nullptr — filled in by the analyzer
    };

    struct Header {
        std::string                path;    // e.g. "engine/console"
        std::vector<ExportedSymbol> exports;
    };

    EngineHeaderRegistry(TypeContext& types) : types_(types) {
        initHeaders();
    }

    /// Look up a header by path.  Returns nullptr if not found.
    const Header* lookup(const std::string& path) const {
        auto it = headers_.find(path);
        return (it != headers_.end()) ? &it->second : nullptr;
    }

    /// All known header paths (for "did you mean?" on bad includes).
    std::vector<std::string> allPaths() const {
        std::vector<std::string> paths;
        for (auto& [p, _] : headers_) paths.push_back(p);
        return paths;
    }

private:
    TypeContext& types_;
    std::unordered_map<std::string, Header> headers_;

    void initHeaders() {
        // --------------------------------------------------------
        // engine/console  — Print, Log, Warn, Error
        // --------------------------------------------------------
        {
            // Print and Log are variadic: (...auto) -> void
            std::vector<TypePtr> variadicParams = { types_.Auto() };
            TypePtr printType = types_.makeFunction(variadicParams, types_.Void(), /*variadic=*/true);

            headers_["engine/console"] = Header{
                "engine/console",
                {
                    { "Print", SymbolKind::BuiltinFunc, printType },
                    { "Log",   SymbolKind::BuiltinFunc, printType },
                    { "Warn",  SymbolKind::BuiltinFunc, printType },
                    { "Error", SymbolKind::BuiltinFunc, printType },
                }
            };
        }

        // --------------------------------------------------------
        // engine/time  — DeltaTime, FixedDeltaTime, Time
        // --------------------------------------------------------
        {
            headers_["engine/time"] = Header{
                "engine/time",
                {
                    { "DeltaTime",      SymbolKind::Variable, types_.Float64() },
                    { "FixedDeltaTime", SymbolKind::Variable, types_.Float64() },
                    { "Time",           SymbolKind::Variable, types_.Float64() },
                }
            };
        }

        // --------------------------------------------------------
        // engine/physics  — stub exports
        // --------------------------------------------------------
        {
            headers_["engine/physics"] = Header{
                "engine/physics",
                {
                    // Gravity constant
                    { "Gravity", SymbolKind::Constant, types_.Float64() },
                }
            };
        }

        // --------------------------------------------------------
        // engine/math  — common math functions
        // --------------------------------------------------------
        {
            std::vector<TypePtr> f64x1 = { types_.Float64() };
            std::vector<TypePtr> f64x2 = { types_.Float64(), types_.Float64() };
            TypePtr f64_to_f64  = types_.makeFunction(f64x1, types_.Float64());
            TypePtr f64x2_to_f64 = types_.makeFunction(f64x2, types_.Float64());

            headers_["engine/math"] = Header{
                "engine/math",
                {
                    { "Abs",   SymbolKind::BuiltinFunc, f64_to_f64 },
                    { "Sqrt",  SymbolKind::BuiltinFunc, f64_to_f64 },
                    { "Sin",   SymbolKind::BuiltinFunc, f64_to_f64 },
                    { "Cos",   SymbolKind::BuiltinFunc, f64_to_f64 },
                    { "Max",   SymbolKind::BuiltinFunc, f64x2_to_f64 },
                    { "Min",   SymbolKind::BuiltinFunc, f64x2_to_f64 },
                }
            };
        }
    }
};

// ============================================================
// Valid lifecycle phases (for @lifecycle annotation validation)
// ============================================================
static const std::unordered_set<std::string> VALID_LIFECYCLE_PHASES = {
    "init",
    "late_init",
    "update",
    "fixed_update",
    "destroy",
};

// Valid compile modes
static const std::unordered_set<std::string> VALID_COMPILE_MODES = {
    "aot",
    "jit",
};

// ============================================================
// SemanticAnalyzer
// ============================================================
class SemanticAnalyzer {
public:
    SemanticAnalyzer() : registry_(types_) {}

    /// Run the full semantic analysis pipeline on a program.
    /// Returns true if analysis completed without errors.
    bool analyze(const Program& program);

    // --------------------------------------------------------
    // Access the products of analysis (for downstream passes)
    // --------------------------------------------------------
    TypeContext&  typeContext()   { return types_; }
    SymbolTable&  symbolTable()  { return symbols_; }
    Diagnostics&  diagnostics()  { return diags_; }
    TypeChecker&  typeChecker()  { return *checker_; }

private:
    TypeContext          types_;
    SymbolTable          symbols_;
    Diagnostics          diags_;
    EngineHeaderRegistry registry_;
    std::unique_ptr<TypeChecker> checker_;

    // --------------------------------------------------------
    // Phase 1: include resolution
    // --------------------------------------------------------
    void resolveIncludes(const Program& program);
    void processInclude(const IncludeDecl* inc);

    // --------------------------------------------------------
    // Phase 2: pre-registration of top-level declarations
    // (name + type stub, no body checking)
    // --------------------------------------------------------
    void preRegisterDeclarations(const Program& program);
    void preRegisterClass(const ClassDecl* cls);
    void preRegisterStruct(const StructDecl* strct);
    void preRegisterEnum(const EnumDecl* enm);
    void preRegisterFunction(const FunctionDecl* func);

    // --------------------------------------------------------
    // Phase 4: post-checks (annotations, unused symbols, etc.)
    // --------------------------------------------------------
    void postCheck(const Program& program);
    void validateAnnotations(const Program& program);
    void validateClassAnnotations(const ClassDecl* cls);
    void validateFunctionAnnotations(const FunctionDecl* func,
                                     const std::vector<Annotation>& annotations);
    void validateMemberAnnotations(const ClassDecl* cls,
                                   const ClassMember& member);
    void warnUnusedSymbols();
};

// ============================================================
// SemanticAnalyzer implementations (inline — header-only)
// ============================================================

inline bool SemanticAnalyzer::analyze(const Program& program) {
    // --- Phase 1: resolve includes ---
    resolveIncludes(program);

    // --- Phase 2: pre-register all top-level declarations ---
    // Push a file scope for top-level declarations
    symbols_.pushScope(Scope::Kind::File);
    preRegisterDeclarations(program);

    // --- Phase 3: run the type checker (full DFS) ---
    checker_ = std::make_unique<TypeChecker>(types_, symbols_, diags_);
    checker_->check(program);

    // --- Phase 4: post-checks ---
    postCheck(program);

    // Pop file scope
    symbols_.popScope();

    return !diags_.hasErrors();
}

// ------------------------------------------------------------
// Phase 1: include resolution
// ------------------------------------------------------------
inline void SemanticAnalyzer::resolveIncludes(const Program& program) {
    for (auto& stmt : program.statements) {
        if (auto* inc = dynamic_cast<const IncludeDecl*>(stmt.get())) {
            processInclude(inc);
        }
    }
}

inline void SemanticAnalyzer::processInclude(const IncludeDecl* inc) {
    const auto* header = registry_.lookup(inc->path);
    if (!header) {
        diags_.error("Unknown engine header '<" + inc->path + ">'", inc->loc);

        // Suggest similar paths
        auto allPaths = registry_.allPaths();
        for (auto& p : allPaths) {
            // Simple prefix match for suggestions
            if (p.size() >= 4 && inc->path.size() >= 4) {
                size_t shared = 0;
                size_t minLen = std::min(p.size(), inc->path.size());
                while (shared < minLen && p[shared] == inc->path[shared]) ++shared;
                if (shared >= 4) {
                    diags_.note("Did you mean '<" + p + ">'?", inc->loc);
                    break;
                }
            }
        }
        return;
    }

    // If selective import { Name1, Name2 } is specified, only import those
    if (!inc->selectedNames.empty()) {
        std::unordered_set<std::string> wanted(inc->selectedNames.begin(),
                                                inc->selectedNames.end());

        for (auto& export_ : header->exports) {
            if (wanted.count(export_.name)) {
                Symbol sym;
                sym.name = export_.name;
                sym.kind = export_.kind;
                sym.type = export_.type;
                sym.loc  = inc->loc;
                sym.isConst = (export_.kind == SymbolKind::Constant);
                sym.isMutable = (export_.kind != SymbolKind::Constant);
                symbols_.addImport(sym, inc->path);
                wanted.erase(export_.name);
            }
        }

        // Warn about names that weren't found in the header
        for (auto& missing : wanted) {
            diags_.error("Header '<" + inc->path + ">' does not export '" + missing + "'",
                         inc->loc);
        }
    } else {
        // Import everything
        for (auto& export_ : header->exports) {
            Symbol sym;
            sym.name = export_.name;
            sym.kind = export_.kind;
            sym.type = export_.type;
            sym.loc  = inc->loc;
            sym.isConst = (export_.kind == SymbolKind::Constant);
            sym.isMutable = (export_.kind != SymbolKind::Constant);
            symbols_.addImport(sym, inc->path);
        }
    }
}

// ------------------------------------------------------------
// Phase 2: pre-registration
// ------------------------------------------------------------
inline void SemanticAnalyzer::preRegisterDeclarations(const Program& program) {
    for (auto& stmt : program.statements) {
        if (auto* cls   = dynamic_cast<const ClassDecl*>(stmt.get()))    { preRegisterClass(cls);    continue; }
        if (auto* strct = dynamic_cast<const StructDecl*>(stmt.get()))   { preRegisterStruct(strct); continue; }
        if (auto* enm   = dynamic_cast<const EnumDecl*>(stmt.get()))     { preRegisterEnum(enm);     continue; }
        if (auto* func  = dynamic_cast<const FunctionDecl*>(stmt.get())) { preRegisterFunction(func); continue; }
        // IncludeDecl already handled in phase 1; other statements
        // (var/const at top-level) are handled by the type checker.
    }
}

inline void SemanticAnalyzer::preRegisterClass(const ClassDecl* cls) {
    // Create the type stub — members will be filled in by the type checker
    TypePtr classType = types_.makeClass(cls->name);

    // Register in symbol table
    Symbol sym;
    sym.name     = cls->name;
    sym.kind     = SymbolKind::Class;
    sym.type     = classType;
    sym.declNode = cls;
    sym.loc      = cls->loc;
    symbols_.declare(sym);
}

inline void SemanticAnalyzer::preRegisterStruct(const StructDecl* strct) {
    TypePtr structType = types_.makeStruct(strct->name);

    Symbol sym;
    sym.name     = strct->name;
    sym.kind     = SymbolKind::Struct;
    sym.type     = structType;
    sym.declNode = strct;
    sym.loc      = strct->loc;
    symbols_.declare(sym);
}

inline void SemanticAnalyzer::preRegisterEnum(const EnumDecl* enm) {
    TypePtr enumType = types_.makeEnum(enm->name);

    Symbol sym;
    sym.name     = enm->name;
    sym.kind     = SymbolKind::Enum;
    sym.type     = enumType;
    sym.declNode = enm;
    sym.loc      = enm->loc;
    symbols_.declare(sym);
}

inline void SemanticAnalyzer::preRegisterFunction(const FunctionDecl* func) {
    // Build a preliminary function type (params may be Auto if
    // no annotations — that's fine, the type checker will refine)
    std::vector<TypePtr> paramTypes;
    bool isVariadic = false;
    for (auto& p : func->params) {
        if (p.isSpread) isVariadic = true;
        // We can't call resolveTypeAnnotation here (that's the
        // type checker's job), so we do a quick primitive lookup
        TypePtr pType = types_.Auto();
        if (p.typeAnnot) {
            const std::string& n = p.typeAnnot->name;
            if      (n == "int8")    pType = types_.Int8();
            else if (n == "int16")   pType = types_.Int16();
            else if (n == "int32")   pType = types_.Int32();
            else if (n == "int64")   pType = types_.Int64();
            else if (n == "uint8")   pType = types_.UInt8();
            else if (n == "uint16")  pType = types_.UInt16();
            else if (n == "uint32")  pType = types_.UInt32();
            else if (n == "uint64")  pType = types_.UInt64();
            else if (n == "float32") pType = types_.Float32();
            else if (n == "float64") pType = types_.Float64();
            else if (n == "bool")    pType = types_.Bool();
            else if (n == "string")  pType = types_.String();
            else {
                // Could be a user-defined type — look up
                TypePtr ut = types_.lookupByName(n);
                if (ut) pType = ut;
            }
        }
        paramTypes.push_back(pType);
    }

    TypePtr retType = types_.Void();
    if (func->returnType) {
        const std::string& n = func->returnType->name;
        if      (n == "int8")    retType = types_.Int8();
        else if (n == "int16")   retType = types_.Int16();
        else if (n == "int32")   retType = types_.Int32();
        else if (n == "int64")   retType = types_.Int64();
        else if (n == "uint8")   retType = types_.UInt8();
        else if (n == "uint16")  retType = types_.UInt16();
        else if (n == "uint32")  retType = types_.UInt32();
        else if (n == "uint64")  retType = types_.UInt64();
        else if (n == "float32") retType = types_.Float32();
        else if (n == "float64") retType = types_.Float64();
        else if (n == "bool")    retType = types_.Bool();
        else if (n == "string")  retType = types_.String();
        else if (n == "void")    retType = types_.Void();
        else {
            TypePtr ut = types_.lookupByName(n);
            if (ut) retType = ut;
        }
    }

    TypePtr funcType = types_.makeFunction(paramTypes, retType, isVariadic);

    Symbol sym;
    sym.name     = func->name;
    sym.kind     = SymbolKind::Function;
    sym.type     = funcType;
    sym.declNode = func;
    sym.loc      = func->loc;
    sym.isPublic = func->isPublic;
    sym.isStatic = func->isStatic;
    symbols_.declare(sym);   // best-effort; type checker will re-declare in its own scope
}

// ------------------------------------------------------------
// Phase 4: post-checks
// ------------------------------------------------------------
inline void SemanticAnalyzer::postCheck(const Program& program) {
    validateAnnotations(program);
    warnUnusedSymbols();
}

inline void SemanticAnalyzer::validateAnnotations(const Program& program) {
    for (auto& stmt : program.statements) {
        if (auto* cls = dynamic_cast<const ClassDecl*>(stmt.get())) {
            validateClassAnnotations(cls);
        }
        if (auto* func = dynamic_cast<const FunctionDecl*>(stmt.get())) {
            validateFunctionAnnotations(func, func->annotations);
        }
    }
}

inline void SemanticAnalyzer::validateClassAnnotations(const ClassDecl* cls) {
    // Validate class-level annotations
    for (auto& ann : cls->annotations) {
        if (ann.name == "compile") {
            // @compile(mode=aot) or @compile(mode=jit) or just @compile(aot)
            for (auto& [key, val] : ann.args) {
                if (key == "mode") {
                    if (!VALID_COMPILE_MODES.count(val)) {
                        diags_.error("Invalid compile mode '" + val +
                                     "' in @compile on class '" + cls->name +
                                     "'. Valid modes: aot, jit", ann.loc);
                    }
                } else {
                    // Single positional arg (no key) — value is in key, val is empty
                    // This handles @compile(aot) style
                }
            }
            // Also handle the case where the first arg has no '=' — parser stores
            // it with key=value where value might be the mode directly
            if (ann.args.size() == 1 && ann.args[0].second.empty()) {
                // Single-arg form: @compile(aot) — key holds the mode value
                if (!VALID_COMPILE_MODES.count(ann.args[0].first)) {
                    diags_.error("Invalid compile mode '" + ann.args[0].first +
                                 "' in @compile. Valid modes: aot, jit", ann.loc);
                }
            }
        }
        else if (ann.name == "permissions") {
            // @permissions(restrict=[...]) — just validate structure
            for (auto& [key, val] : ann.args) {
                if (key != "restrict") {
                    diags_.warning("Unknown @permissions argument '" + key +
                                   "' (expected 'restrict')", ann.loc);
                }
            }
        }
        else if (ann.name == "lifecycle") {
            diags_.error("@lifecycle cannot be applied to a class — apply it to methods",
                         ann.loc);
        }
    }

    // Validate member annotations
    for (auto& member : cls->members) {
        validateMemberAnnotations(cls, member);
    }
}

inline void SemanticAnalyzer::validateFunctionAnnotations(const FunctionDecl* func,
                                                           const std::vector<Annotation>& annotations) {
    for (auto& ann : annotations) {
        if (ann.name == "lifecycle") {
            // Must have phase=<validPhase>
            bool foundPhase = false;
            for (auto& [key, val] : ann.args) {
                if (key == "phase") {
                    foundPhase = true;
                    if (!VALID_LIFECYCLE_PHASES.count(val)) {
                        diags_.error("Invalid lifecycle phase '" + val +
                                     "' in @lifecycle on '" + func->name +
                                     "'. Valid phases: init, late_init, update, fixed_update, destroy",
                                     ann.loc);
                    }

                    // Check that the function signature matches the phase
                    // All lifecycle hooks must be: void name()
                    if (func->returnType && func->returnType->name != "void") {
                        diags_.error("Lifecycle hook '" + func->name +
                                     "' must return void", func->loc);
                    }
                    if (!func->params.empty()) {
                        diags_.error("Lifecycle hook '" + func->name +
                                     "' must take no parameters", func->loc);
                    }
                }
            }
            if (!foundPhase) {
                diags_.error("@lifecycle requires a 'phase' argument", ann.loc);
            }
        }
        else if (ann.name == "expose") {
            // @expose is only valid on class fields, not standalone functions
            // (The member-level check handles the details)
            diags_.error("@expose can only be applied to class member fields",
                         ann.loc);
        }
    }
}

inline void SemanticAnalyzer::validateMemberAnnotations(const ClassDecl* cls,
                                                         const ClassMember& member) {
    auto* varDecl  = dynamic_cast<const VarDecl*>(member.decl.get());
    auto* funcDecl = dynamic_cast<const FunctionDecl*>(member.decl.get());

    for (auto& ann : member.annotations) {
        if (ann.name == "expose") {
            // @expose is only valid on fields (VarDecl), not methods
            if (!varDecl) {
                diags_.error("@expose can only be applied to fields, not methods (in class '" +
                             cls->name + "')", ann.loc);
                continue;
            }

            // Validate @expose arguments
            for (auto& [key, val] : ann.args) {
                if (key == "min" || key == "max" || key == "step" || key == "maxLength") {
                    // These should be numeric — just check non-empty
                    if (val.empty()) {
                        diags_.error("@expose argument '" + key + "' requires a value", ann.loc);
                    }
                } else if (key == "tooltip" || key == "group") {
                    // String values — fine as-is
                } else {
                    diags_.warning("Unknown @expose argument '" + key + "'", ann.loc);
                }
            }

            // If min/max are present, check that the field type is numeric
            bool hasMin = false, hasMax = false;
            for (auto& [key, _] : ann.args) {
                if (key == "min") hasMin = true;
                if (key == "max") hasMax = true;
            }
            if ((hasMin || hasMax) && varDecl->typeAnnot) {
                const std::string& typeName = varDecl->typeAnnot->name;
                bool isNumericType = (typeName == "int8"  || typeName == "int16" ||
                                      typeName == "int32" || typeName == "int64" ||
                                      typeName == "uint8" || typeName == "uint16" ||
                                      typeName == "uint32"|| typeName == "uint64" ||
                                      typeName == "float32"|| typeName == "float64");
                if (!isNumericType) {
                    diags_.error("@expose min/max only applies to numeric fields, but '" +
                                 varDecl->name + "' is '" + typeName + "'", ann.loc);
                }
            }

            // maxLength only makes sense on string fields
            bool hasMaxLength = false;
            for (auto& [key, _] : ann.args) {
                if (key == "maxLength") hasMaxLength = true;
            }
            if (hasMaxLength && varDecl->typeAnnot && varDecl->typeAnnot->name != "string") {
                diags_.error("@expose maxLength only applies to string fields, but '" +
                             varDecl->name + "' is '" + varDecl->typeAnnot->name + "'", ann.loc);
            }
        }
        else if (ann.name == "lifecycle") {
            // Must be on a method
            if (!funcDecl) {
                diags_.error("@lifecycle can only be applied to methods, not fields (in class '" +
                             cls->name + "')", ann.loc);
                continue;
            }
            // Delegate to function annotation validation
            validateFunctionAnnotations(funcDecl, {ann});
        }
    }

    // If the member is a FunctionDecl, also validate its own annotation list
    if (funcDecl && !funcDecl->annotations.empty()) {
        validateFunctionAnnotations(funcDecl, funcDecl->annotations);
    }
}

inline void SemanticAnalyzer::warnUnusedSymbols() {
    // Collect unused symbols and emit warnings
    // (We skip this for now to avoid noise during development;
    //  uncomment when the language is more mature)
    //
    // auto unused = symbols_.unusedSymbols();
    // for (auto* sym : unused) {
    //     diags_.warning("Symbol '" + sym->name + "' is declared but never used", sym->loc);
    // }
}

} // namespace scriptlang

#endif // SEMANTIC_ANALYZER_H

#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

// ------------------------------------------------------------
// symbol_table.h — Scoped symbol tables for ScriptLang.
//
// A SymbolTable is a stack of Scope frames.  Each Scope is a
// flat hash map.  Looking up a name walks the stack from the
// innermost (current) scope outward — standard lexical scoping.
//
// Design notes:
//   • Symbols are cheap value structs stored directly in the map
//     (no heap allocation per symbol beyond the map bucket).
//   • The AST node pointer in Symbol is raw — it points into the
//     AST tree that must outlive the symbol table.  That's fine
//     because both live for the lifetime of one compilation.
//   • "Class scope" is a special named scope that the type checker
//     pushes when it enters a class/struct body.  Member lookup
//     and `this` resolution consult it.
//   • Includes are modelled as a flat list of "imported names"
//     that are visible at file scope.  The semantic analyzer
//     populates this when it processes IncludeDecl nodes.
// ------------------------------------------------------------

#include "types.h"
#include "ast.h"

#include <string>
#include <vector>
#include <unordered_map>
#include <optional>

namespace scriptlang {

// ============================================================
// SymbolKind — what the symbol refers to
// ============================================================
enum class SymbolKind {
    Variable,       // var x = …
    Constant,       // const X = …
    Function,       // function or method declaration
    Parameter,      // function parameter
    Class,          // class declaration
    Struct,         // struct declaration
    Enum,           // enum declaration
    EnumEntry,      // a single entry inside an enum
    Import,         // brought in by #include
    BuiltinFunc,    // core-provided function (e.g. Print)
};

inline std::ostream& operator<<(std::ostream& os, SymbolKind kind) {
    switch (kind) {
        case SymbolKind::Variable: return os << "Variable";
        case SymbolKind::Constant: return os << "Constant";
        case SymbolKind::Function: return os << "Function";
        case SymbolKind::Parameter: return os << "Parameter";
        case SymbolKind::Class: return os << "Class";
        case SymbolKind::Struct: return os << "Struct";
        case SymbolKind::Enum: return os << "Enum";
        case SymbolKind::EnumEntry: return os << "EnumEntry";
        case SymbolKind::Import: return os << "Import";
        case SymbolKind::BuiltinFunc: return os << "BuiltinFunc";
    }
    return os << "<Unknown>";
}

// ============================================================
// Symbol — one entry in a scope frame
// ============================================================
struct Symbol {
    std::string  name;
    SymbolKind   kind;
    TypePtr      type;            // resolved type (may be Auto initially)

    // The AST node that declared this symbol.  Raw pointer — the
    // AST owns the lifetime.  nullptr for builtins / imports.
    const Statement* declNode = nullptr;

    // Source location (copied from declNode for convenience)
    SourceLoc    loc;

    // --- Flags ---
    bool isConst    = false;      // declared with `const`
    bool isStatic   = false;      // declared with `static`
    bool isPublic   = true;       // visibility
    bool isMutable  = true;       // false for const or enum entries
    bool isUsed     = false;      // set by semantic pass — unused-symbol warnings

    // For EnumEntry: the owning enum type
    TypePtr ownerEnumType;

    // Default constructor
    Symbol() : kind(SymbolKind::Variable) {}
};

// ============================================================
// Scope — a single frame in the scope stack
// ============================================================
struct Scope {
    enum class Kind {
        Block,          // { … }  — generic block
        Function,       // function body
        Class,          // class / struct body
        File,           // top-level file scope
        Builtin,        // the outermost scope containing builtins
    };

    Kind kind = Kind::Block;

    /// For Class scopes: the type of the class being defined.
    TypePtr classType;

    /// For Class scopes: the name, so we can print it in errors.
    std::string className;

    /// The symbols declared in THIS scope (not parents).
    std::unordered_map<std::string, Symbol> symbols;

    /// Insert a symbol.  Returns false if the name already exists
    /// in THIS scope (redeclaration error).
    bool insert(const Symbol& sym) {
        auto [it, inserted] = symbols.emplace(sym.name, sym);
        return inserted;
    }

    /// Look up in THIS scope only.
    const Symbol* find(const std::string& name) const {
        auto it = symbols.find(name);
        return (it != symbols.end()) ? &it->second : nullptr;
    }

    /// Mutable find — needed so the type checker can fill in types
    /// after initial insertion.
    Symbol* findMut(const std::string& name) {
        auto it = symbols.find(name);
        return (it != symbols.end()) ? &it->second : nullptr;
    }
};

// ============================================================
// SymbolTable — the full scoped lookup structure
// ============================================================
class SymbolTable {
public:
    SymbolTable();

    // --------------------------------------------------------
    // Scope management
    // --------------------------------------------------------

    /// Push a new scope frame.  Call before entering a block,
    /// function body, class body, etc.
    void pushScope(Scope::Kind kind = Scope::Kind::Block);

    /// Push a class/struct scope (sets classType + className).
    void pushClassScope(const std::string& name, TypePtr classType);

    /// Pop the current (innermost) scope.  Panics if you try to
    /// pop the Builtin scope.
    void popScope();

    /// How many scope frames are on the stack right now.
    size_t depth() const { return scopes_.size(); }

    // --------------------------------------------------------
    // Symbol insertion
    // --------------------------------------------------------

    /// Declare a symbol in the current (innermost) scope.
    /// Returns true on success, false if the name already exists
    /// in the current scope (caller should emit a redeclaration error).
    bool declare(const Symbol& sym);

    // --------------------------------------------------------
    // Symbol lookup — walks scopes from inner to outer
    // --------------------------------------------------------

    /// Look up a name.  Returns nullptr if not found anywhere.
    const Symbol* lookup(const std::string& name) const;

    /// Mutable lookup — needed so the type checker can mark
    /// symbols as "used" or fill in inferred types.
    Symbol* lookupMut(const std::string& name);

    /// Look up in the current scope ONLY (for redeclaration checks).
    const Symbol* lookupCurrentScope(const std::string& name) const;

    // --------------------------------------------------------
    // Class / `this` helpers
    // --------------------------------------------------------

    /// Walk the scope stack and return the innermost Class scope,
    /// or nullptr if we're not inside a class.
    const Scope* currentClassScope() const;

    /// The type of `this` in the current class scope, or nullptr.
    TypePtr currentThisType() const;

    /// Look up a member of the current class scope by name.
    /// Useful for resolving bare identifiers that might be fields
    /// or methods of the enclosing class.
    const Symbol* lookupClassMember(const std::string& name) const;

    // --------------------------------------------------------
    // Imports (populated when processing #include)
    // --------------------------------------------------------

    /// Register an imported symbol at file scope.
    /// `source` is the include path for error messages.
    void addImport(const Symbol& sym, const std::string& source);

    /// Get all imports (for diagnostics / "did you mean?" suggestions).
    const std::vector<Symbol>& imports() const { return imports_; }

    // --------------------------------------------------------
    // Diagnostics helpers
    // --------------------------------------------------------

    /// Collect all symbols that are declared but never used.
    /// Skips builtins and imports.
    std::vector<const Symbol*> unusedSymbols() const;

    /// Find the closest name to `name` using simple edit-distance
    /// heuristic (for "did you mean X?" suggestions).
    std::optional<std::string> suggestSimilar(const std::string& name) const;

private:
    /// The scope stack.  Index 0 = Builtin (outermost).
    std::vector<Scope> scopes_;

    /// Flat list of imported symbols (file-scope visibility).
    std::vector<Symbol> imports_;

    /// Populate the Builtin scope with core-provided names.
    void initBuiltins();
};

// ============================================================
// SymbolTable implementations (inline — header-only)
// ============================================================

inline SymbolTable::SymbolTable() {
    // Start with the builtin scope
    scopes_.push_back(Scope{Scope::Kind::Builtin, nullptr, "", {}});
    initBuiltins();
}

inline void SymbolTable::initBuiltins() {
    // These are the functions that #include <core/console> etc.
    // will expose.  We pre-declare them here so that if the user
    // forgets the #include we can still give a good error ("did
    // you mean Print? (requires #include <core/console>)").
    //
    // For now we just register the names; the actual types will be
    // filled in properly once we have the core header stubs.
    // Mark them as BuiltinFunc so the checker knows not to warn
    // about them being "unused".

    // We do NOT insert them yet — they live in the imports list
    // and get promoted to visible only when an #include references
    // them.  But we keep a record for suggestion purposes.
}

inline void SymbolTable::pushScope(Scope::Kind kind) {
    scopes_.push_back(Scope{kind, nullptr, "", {}});
}

inline void SymbolTable::pushClassScope(const std::string& name, TypePtr classType) {
    Scope s;
    s.kind      = Scope::Kind::Class;
    s.classType = classType;
    s.className = name;
    scopes_.push_back(std::move(s));
}

inline void SymbolTable::popScope() {
    // Never pop the Builtin scope
    if (scopes_.size() <= 1) return;   // safety guard
    scopes_.pop_back();
}

inline bool SymbolTable::declare(const Symbol& sym) {
    if (scopes_.empty()) return false;
    return scopes_.back().insert(sym);
}

inline const Symbol* SymbolTable::lookup(const std::string& name) const {
    // Walk from innermost to outermost
    for (int i = static_cast<int>(scopes_.size()) - 1; i >= 0; --i) {
        const Symbol* s = scopes_[i].find(name);
        if (s) return s;
    }
    // Also check imports
    for (auto& imp : imports_) {
        if (imp.name == name) return &imp;
    }
    return nullptr;
}

inline Symbol* SymbolTable::lookupMut(const std::string& name) {
    for (int i = static_cast<int>(scopes_.size()) - 1; i >= 0; --i) {
        Symbol* s = scopes_[i].findMut(name);
        if (s) return s;
    }
    for (auto& imp : imports_) {
        if (imp.name == name) return &imp;
    }
    return nullptr;
}

inline const Symbol* SymbolTable::lookupCurrentScope(const std::string& name) const {
    if (scopes_.empty()) return nullptr;
    return scopes_.back().find(name);
}

inline const Scope* SymbolTable::currentClassScope() const {
    for (int i = static_cast<int>(scopes_.size()) - 1; i >= 0; --i) {
        if (scopes_[i].kind == Scope::Kind::Class) return &scopes_[i];
    }
    return nullptr;
}

inline TypePtr SymbolTable::currentThisType() const {
    const Scope* cs = currentClassScope();
    return cs ? cs->classType : nullptr;
}

inline const Symbol* SymbolTable::lookupClassMember(const std::string& name) const {
    const Scope* cs = currentClassScope();
    if (!cs) return nullptr;
    return cs->find(name);
}

inline void SymbolTable::addImport(const Symbol& sym, const std::string& /*source*/) {
    // Check for duplicate import
    for (auto& existing : imports_) {
        if (existing.name == sym.name) return;   // already imported
    }
    imports_.push_back(sym);
}

inline std::vector<const Symbol*> SymbolTable::unusedSymbols() const {
    std::vector<const Symbol*> result;
    // Skip scope 0 (Builtin) — start at 1
    for (size_t i = 1; i < scopes_.size(); ++i) {
        for (auto& [name, sym] : scopes_[i].symbols) {
            if (!sym.isUsed &&
                sym.kind != SymbolKind::BuiltinFunc &&
                sym.kind != SymbolKind::Import) {
                result.push_back(&sym);
            }
        }
    }
    return result;
}

// Simple Levenshtein-based suggestion (capped at short strings
// to keep it fast — this runs at error time, not hot path).
inline std::optional<std::string> SymbolTable::suggestSimilar(const std::string& name) const {
    // Collect all visible names
    std::vector<std::string> candidates;
    for (auto& scope : scopes_) {
        for (auto& [n, _] : scope.symbols) {
            candidates.push_back(n);
        }
    }
    for (auto& imp : imports_) {
        candidates.push_back(imp.name);
    }

    // Simple heuristic: find the candidate with the most shared
    // prefix, then check it's "close enough" (within 2 edits for
    // short names).  Full Levenshtein is overkill here.
    std::string best;
    size_t bestScore = 0;   // higher = more similar

    for (auto& c : candidates) {
        if (c == name) continue;   // exact match shouldn't happen here
        if (c.empty())  continue;

        // Shared prefix length
        size_t prefix = 0;
        size_t minLen = std::min(c.size(), name.size());
        while (prefix < minLen && c[prefix] == name[prefix]) ++prefix;

        // Score = 2 * shared_prefix - length_diff_penalty
        size_t lenDiff = (c.size() > name.size()) ? c.size() - name.size()
                                                  : name.size() - c.size();
        size_t score = (prefix >= 2) ? (2 * prefix) - lenDiff : 0;

        if (score > bestScore && lenDiff <= 3 && prefix >= 2) {
            bestScore = score;
            best      = c;
        }
    }

    if (best.empty()) return std::nullopt;
    return best;
}

} // namespace scriptlang

#endif // SYMBOL_TABLE_H

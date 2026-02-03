#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

// ------------------------------------------------------------
// type_checker.h — Type inference and checking pass.
//
// Responsibilities:
//   1. Infer the type of every Expression node in the AST.
//   2. Check that operators, assignments, calls, and returns
//      are type-compatible.
//   3. Populate the exprTypes_ map so that later passes (IR
//      codegen) can look up "what type does this expression have?"
//      in O(1).
//   4. Emit diagnostics (errors / warnings) into the shared
//      Diagnostic list.
//
// Usage:
//     TypeContext  types;
//     SymbolTable  symbols;
//     Diagnostics  diags;
//     TypeChecker  checker(types, symbols, diags);
//     checker.check(program);
//     // diags now contains any errors; exprTypes() is populated.
//
// The checker does a single DFS over the AST.  It does NOT modify
// the tree — it only reads it and writes into its own maps.
// ------------------------------------------------------------

#include "ast.h"
#include "types.h"
#include "symbol_table.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <optional>

namespace scriptlang {

// ============================================================
// Diagnostic — a single error or warning
// ============================================================
enum class DiagSeverity { Error, Warning, Note };

struct Diagnostic {
    DiagSeverity severity;
    std::string  message;
    SourceLoc    loc;
    // Optional "note" chain for multi-line diagnostics
    std::vector<Diagnostic> notes;
};

// ============================================================
// Diagnostics — the error accumulator
// ============================================================
class Diagnostics {
public:
    void error(const std::string& msg, SourceLoc loc) {
        diags_.push_back({DiagSeverity::Error, msg, loc, {}});
    }
    void warning(const std::string& msg, SourceLoc loc) {
        diags_.push_back({DiagSeverity::Warning, msg, loc, {}});
    }
    void note(const std::string& msg, SourceLoc loc) {
        if (!diags_.empty()) {
            diags_.back().notes.push_back({DiagSeverity::Note, msg, loc, {}});
        }
    }

    bool hasErrors() const {
        for (auto& d : diags_) {
            if (d.severity == DiagSeverity::Error) return true;
        }
        return false;
    }

    const std::vector<Diagnostic>& all() const { return diags_; }
    size_t errorCount() const {
        size_t n = 0;
        for (auto& d : diags_) if (d.severity == DiagSeverity::Error) ++n;
        return n;
    }

private:
    std::vector<Diagnostic> diags_;
};

// ============================================================
// TypeChecker
// ============================================================
class TypeChecker {
public:
    TypeChecker(TypeContext& types, SymbolTable& symbols, Diagnostics& diags)
        : types_(types), symbols_(symbols), diags_(diags) {}

    /// Main entry point.  Walks the entire program.
    void check(const Program& program);

    /// Query the inferred type of any expression node.
    /// Returns Error type if the node was never visited or errored.
    TypePtr typeOf(const Expression* expr) const;

    /// Query the declared/inferred type of any statement node
    /// (meaningful for VarDecl, ConstDecl, FunctionDecl).
    TypePtr typeOf(const Statement* stmt) const;

    // --------------------------------------------------------
    // Public accessors for downstream passes
    // --------------------------------------------------------
    const std::unordered_map<const Expression*, TypePtr>& exprTypes() const { return exprTypes_; }
    const std::unordered_map<const Statement*,  TypePtr>& stmtTypes() const { return stmtTypes_; }

private:
    TypeContext&  types_;
    SymbolTable&  symbols_;
    Diagnostics&  diags_;

    // --- Type maps (the main output of this pass) ---
    std::unordered_map<const Expression*, TypePtr> exprTypes_;
    std::unordered_map<const Statement*,  TypePtr> stmtTypes_;

    // --- Current function return type (for checking return stmts) ---
    TypePtr currentReturnType_;

    // --------------------------------------------------------
    // Statement visitors
    // --------------------------------------------------------
    void checkStatement(const Statement* stmt);
    void checkBlock(const BlockStatement* block);
    void checkVarDecl(const VarDecl* decl);
    void checkConstDecl(const ConstDecl* decl);
    void checkFunctionDecl(const FunctionDecl* func);
    void checkClassDecl(const ClassDecl* cls);
    void checkStructDecl(const StructDecl* strct);
    void checkEnumDecl(const EnumDecl* enm);
    void checkIfStatement(const IfStatement* ifStmt);
    void checkWhileStatement(const WhileStatement* whileStmt);
    void checkForStatement(const ForStatement* forStmt);
    void checkForInStatement(const ForInStatement* forIn);
    void checkSwitchStatement(const SwitchStatement* sw);
    void checkReturnStatement(const ReturnStatement* ret);
    void checkIncludeDecl(const IncludeDecl* inc);
    void checkExprStatement(const ExprStatement* exprStmt);

    // --------------------------------------------------------
    // Expression type inference — returns the inferred type AND
    // caches it in exprTypes_.
    // --------------------------------------------------------
    TypePtr inferType(const Expression* expr);
    TypePtr inferLiteral(const LiteralExpr* lit);
    TypePtr inferIdentifier(const IdentifierExpr* id);
    TypePtr inferBinary(const BinaryExpr* bin);
    TypePtr inferUnary(const UnaryExpr* un);
    TypePtr inferAssign(const AssignExpr* assign);
    TypePtr inferCall(const CallExpr* call);
    TypePtr inferIndex(const IndexExpr* idx);
    TypePtr inferMember(const MemberExpr* mem);
    TypePtr inferOptionalMember(const OptionalMemberExpr* mem);
    TypePtr inferNullCoalesce(const NullCoalesceExpr* nc);
    TypePtr inferArrayLiteral(const ArrayLiteralExpr* arr);
    TypePtr inferLambda(const LambdaExpr* lam);
    TypePtr inferNew(const NewExpr* newExpr);
    TypePtr inferThis(const ThisExpr* thisExpr);
    TypePtr inferSpread(const SpreadExpr* spread);
    TypePtr inferRange(const RangeExpr* range);
    TypePtr inferInterpolatedString(const InterpolatedStringExpr* interp);
    TypePtr inferTernary(const TernaryExpr* tern);

    // --------------------------------------------------------
    // Helpers
    // --------------------------------------------------------

    /// Resolve a TypeAnnotation string (e.g. "int32", "Player")
    /// into a TypePtr.  Returns Error type + emits diagnostic on
    /// failure.
    TypePtr resolveTypeAnnotation(const TypeAnnotation* annot);

    /// Check that `actual` is assignable to `expected`.  If not,
    /// emits an error at `loc` and returns false.
    bool checkAssignable(TypePtr expected, TypePtr actual, SourceLoc loc,
                         const std::string& context = "");

    /// Check that an expression is a valid l-value (something you
    /// can assign to).  Returns false + emits error if not.
    bool checkLValue(const Expression* expr);

    /// Look up a member on a type.  Returns the member's type, or
    /// nullptr if not found.  Also sets `isMutable` and `isStatic`.
    TypePtr lookupMember(TypePtr objType, const std::string& memberName,
                         bool& isMutable, bool& isStatic) const;

    /// Determine the result type of a binary operator on two types.
    /// Returns Error type if the combination is invalid.
    TypePtr binaryResultType(BinaryOp op, TypePtr left, TypePtr right, SourceLoc loc);

    /// For numeric literals that could be multiple integer widths,
    /// determine the narrowest type that fits the value.
    TypePtr narrowIntegerLiteral(const std::string& value, bool isHex, bool isBinary) const;

    /// Cache helper: store the type for an expression and return it.
    TypePtr cache(const Expression* expr, TypePtr type) {
        exprTypes_[expr] = type;
        return type;
    }
};

// ============================================================
// TypeChecker implementations (inline — header-only)
// ============================================================

// ------------------------------------------------------------
// Main entry
// ------------------------------------------------------------
inline void TypeChecker::check(const Program& program) {
    // File scope is already on the symbol table stack (pushed by
    // the semantic analyzer before calling us).  We just walk.
    for (auto& stmt : program.statements) {
        checkStatement(stmt.get());
    }
}

inline TypePtr TypeChecker::typeOf(const Expression* expr) const {
    auto it = exprTypes_.find(expr);
    return (it != exprTypes_.end()) ? it->second : types_.Error();
}

inline TypePtr TypeChecker::typeOf(const Statement* stmt) const {
    auto it = stmtTypes_.find(stmt);
    return (it != stmtTypes_.end()) ? it->second : types_.Error();
}

// ------------------------------------------------------------
// Statement dispatch
// ------------------------------------------------------------
inline void TypeChecker::checkStatement(const Statement* stmt) {
    if (!stmt) return;

    if (auto* s = dynamic_cast<const BlockStatement*>(stmt))        { checkBlock(s); return; }
    if (auto* s = dynamic_cast<const VarDecl*>(stmt))               { checkVarDecl(s); return; }
    if (auto* s = dynamic_cast<const ConstDecl*>(stmt))             { checkConstDecl(s); return; }
    if (auto* s = dynamic_cast<const FunctionDecl*>(stmt))          { checkFunctionDecl(s); return; }
    if (auto* s = dynamic_cast<const ClassDecl*>(stmt))             { checkClassDecl(s); return; }
    if (auto* s = dynamic_cast<const StructDecl*>(stmt))            { checkStructDecl(s); return; }
    if (auto* s = dynamic_cast<const EnumDecl*>(stmt))              { checkEnumDecl(s); return; }
    if (auto* s = dynamic_cast<const IfStatement*>(stmt))           { checkIfStatement(s); return; }
    if (auto* s = dynamic_cast<const WhileStatement*>(stmt))        { checkWhileStatement(s); return; }
    if (auto* s = dynamic_cast<const ForStatement*>(stmt))          { checkForStatement(s); return; }
    if (auto* s = dynamic_cast<const ForInStatement*>(stmt))        { checkForInStatement(s); return; }
    if (auto* s = dynamic_cast<const SwitchStatement*>(stmt))       { checkSwitchStatement(s); return; }
    if (auto* s = dynamic_cast<const ReturnStatement*>(stmt))       { checkReturnStatement(s); return; }
    if (auto* s = dynamic_cast<const IncludeDecl*>(stmt))           { checkIncludeDecl(s); return; }
    if (auto* s = dynamic_cast<const ExprStatement*>(stmt))         { checkExprStatement(s); return; }
    if (dynamic_cast<const BreakStatement*>(stmt))                  { return; } // no type work needed
    if (dynamic_cast<const ContinueStatement*>(stmt))               { return; }
}

inline void TypeChecker::checkBlock(const BlockStatement* block) {
    symbols_.pushScope(Scope::Kind::Block);
    for (auto& s : block->statements) {
        checkStatement(s.get());
    }
    symbols_.popScope();
}

// ------------------------------------------------------------
// Variable / Constant declarations
// ------------------------------------------------------------
inline void TypeChecker::checkVarDecl(const VarDecl* decl) {
    TypePtr declaredType;
    TypePtr initType;

    // Resolve explicit type annotation if present
    if (decl->typeAnnot) {
        declaredType = resolveTypeAnnotation(decl->typeAnnot.get());
    }

    // Infer initializer type if present
    if (decl->initializer) {
        initType = inferType(decl->initializer.get());
    }

    // Determine the final type
    TypePtr finalType;
    if (declaredType && initType) {
        // Both present: check compatibility
        if (!checkAssignable(declaredType, initType, decl->loc,
                             "variable '" + decl->name + "'")) {
            finalType = declaredType;   // use declared type anyway to suppress cascading
        } else {
            finalType = declaredType;
        }
    } else if (declaredType) {
        finalType = declaredType;       // no initializer — type is the annotation
    } else if (initType) {
        finalType = initType;           // type inference from initializer
    } else {
        diags_.error("Variable '" + decl->name + "' has no type annotation and no initializer", decl->loc);
        finalType = types_.Error();
    }

    // Register in symbol table
    Symbol sym;
    sym.name     = decl->name;
    sym.kind     = SymbolKind::Variable;
    sym.type     = finalType;
    sym.declNode = decl;
    sym.loc      = decl->loc;
    sym.isConst  = false;
    sym.isMutable = true;

    if (!symbols_.declare(sym)) {
        diags_.error("Redeclaration of '" + decl->name + "' in this scope", decl->loc);
        // Still record the type so downstream doesn't cascade
    }

    stmtTypes_[decl] = finalType;
}

inline void TypeChecker::checkConstDecl(const ConstDecl* decl) {
    TypePtr declaredType;
    TypePtr initType;

    if (decl->typeAnnot) {
        declaredType = resolveTypeAnnotation(decl->typeAnnot.get());
    }
    if (decl->initializer) {
        initType = inferType(decl->initializer.get());
    } else {
        diags_.error("Constant '" + decl->name + "' must have an initializer", decl->loc);
    }

    TypePtr finalType;
    if (declaredType && initType) {
        checkAssignable(declaredType, initType, decl->loc,
                        "constant '" + decl->name + "'");
        finalType = declaredType;
    } else if (declaredType) {
        finalType = declaredType;
    } else if (initType) {
        finalType = initType;
    } else {
        finalType = types_.Error();
    }

    Symbol sym;
    sym.name      = decl->name;
    sym.kind      = SymbolKind::Constant;
    sym.type      = finalType;
    sym.declNode  = decl;
    sym.loc       = decl->loc;
    sym.isConst   = true;
    sym.isMutable = false;

    if (!symbols_.declare(sym)) {
        diags_.error("Redeclaration of '" + decl->name + "' in this scope", decl->loc);
    }

    stmtTypes_[decl] = finalType;
}

// ------------------------------------------------------------
// Function declaration
// ------------------------------------------------------------
inline void TypeChecker::checkFunctionDecl(const FunctionDecl* func) {
    // Build the function type
    std::vector<TypePtr> paramTypes;
    bool isVariadic = false;

    for (auto& p : func->params) {
        if (p.isSpread) isVariadic = true;
        TypePtr pType = p.typeAnnot ? resolveTypeAnnotation(p.typeAnnot.get())
                                    : types_.Auto();
        paramTypes.push_back(pType);
    }

    TypePtr retType = func->returnType ? resolveTypeAnnotation(func->returnType.get())
                                       : types_.Void();

    TypePtr funcType = types_.makeFunction(paramTypes, retType, isVariadic);

    // Declare the function name in the CURRENT (enclosing) scope
    Symbol sym;
    sym.name     = func->name;
    sym.kind     = SymbolKind::Function;
    sym.type     = funcType;
    sym.declNode = func;
    sym.loc      = func->loc;
    sym.isPublic = func->isPublic;
    sym.isStatic = func->isStatic;

    if (!symbols_.declare(sym)) {
        // Overloading not supported yet — report redeclaration
        diags_.error("Redeclaration of function '" + func->name + "'", func->loc);
    }

    stmtTypes_[func] = funcType;

    // --- Check the body in a new Function scope ---
    symbols_.pushScope(Scope::Kind::Function);

    // Declare parameters in the function scope
    for (size_t i = 0; i < func->params.size(); ++i) {
        Symbol pSym;
        pSym.name     = func->params[i].name;
        pSym.kind     = SymbolKind::Parameter;
        pSym.type     = paramTypes[i];
        pSym.loc      = func->params[i].loc;
        pSym.isMutable = true;

        if (!symbols_.declare(pSym)) {
            diags_.error("Duplicate parameter name '" + func->params[i].name + "'",
                         func->params[i].loc);
        }
    }

    // Set current return type context and check body
    TypePtr savedRetType = currentReturnType_;
    currentReturnType_   = retType;

    if (func->body) {
        // Body is a BlockStatement — but we already pushed a scope,
        // so we inline its contents rather than calling checkBlock
        // (which would push another scope).
        auto* block = dynamic_cast<const BlockStatement*>(func->body.get());
        if (block) {
            for (auto& s : block->statements) {
                checkStatement(s.get());
            }
        }
    }

    currentReturnType_ = savedRetType;
    symbols_.popScope();
}

// ------------------------------------------------------------
// Class declaration
// ------------------------------------------------------------
inline void TypeChecker::checkClassDecl(const ClassDecl* cls) {
    // Create or retrieve the class type
    TypePtr classType = types_.makeClass(cls->name);

    // Resolve parent type if present
    if (!cls->parent.empty()) {
        TypePtr parentType = types_.lookupByName(cls->parent);
        if (!parentType) {
            diags_.error("Unknown parent class '" + cls->parent + "'", cls->loc);
            diags_.note("Check that '" + cls->parent + "' is declared before '" + cls->name + "'", cls->loc);
        } else {
            classType->parentType = parentType;
        }
    }

    // Declare the class name in the enclosing scope
    Symbol classSym;
    classSym.name     = cls->name;
    classSym.kind     = SymbolKind::Class;
    classSym.type     = classType;
    classSym.declNode = cls;
    classSym.loc      = cls->loc;

    if (!symbols_.declare(classSym)) {
        diags_.error("Redeclaration of class '" + cls->name + "'", cls->loc);
    }

    stmtTypes_[cls] = classType;

    // --- First pass: register all member names and types ---
    // (so methods can call each other regardless of order)
    classType->members.clear();
    for (auto& member : cls->members) {
        if (auto* varDecl = dynamic_cast<const VarDecl*>(member.decl.get())) {
            TypePtr mType = varDecl->typeAnnot ? resolveTypeAnnotation(varDecl->typeAnnot.get())
                                               : types_.Auto();
            Type::MemberInfo mi;
            mi.name     = varDecl->name;
            mi.type     = mType;
            mi.isPublic = member.isPublic;
            mi.isStatic = member.isStatic;
            mi.isMethod = false;
            classType->members.push_back(mi);
        } else if (auto* funcDecl = dynamic_cast<const FunctionDecl*>(member.decl.get())) {
            // Build function type for this method
            std::vector<TypePtr> pTypes;
            bool isVariadic = false;
            for (auto& p : funcDecl->params) {
                if (p.isSpread) isVariadic = true;
                pTypes.push_back(p.typeAnnot ? resolveTypeAnnotation(p.typeAnnot.get())
                                             : types_.Auto());
            }
            TypePtr rType = funcDecl->returnType ? resolveTypeAnnotation(funcDecl->returnType.get())
                                                 : types_.Void();

            Type::MemberInfo mi;
            mi.name     = funcDecl->name;
            mi.type     = types_.makeFunction(pTypes, rType, isVariadic);
            mi.isPublic = member.isPublic;
            mi.isStatic = member.isStatic;
            mi.isMethod = true;
            classType->members.push_back(mi);
        }
    }

    // --- Second pass: push class scope and check bodies ---
    symbols_.pushClassScope(cls->name, classType);

    // Declare all members in the class scope so they're visible
    // to each other
    for (auto& mi : classType->members) {
        Symbol mSym;
        mSym.name     = mi.name;
        mSym.kind     = mi.isMethod ? SymbolKind::Function : SymbolKind::Variable;
        mSym.type     = mi.type;
        mSym.isPublic = mi.isPublic;
        mSym.isStatic = mi.isStatic;
        symbols_.declare(mSym);   // ignore redecl here — already validated
    }

    // Now check each member's body / initializer
    for (auto& member : cls->members) {
        checkStatement(member.decl.get());
    }

    symbols_.popScope();
}

// ------------------------------------------------------------
// Struct declaration
// ------------------------------------------------------------
inline void TypeChecker::checkStructDecl(const StructDecl* strct) {
    TypePtr structType = types_.makeStruct(strct->name);

    Symbol structSym;
    structSym.name     = strct->name;
    structSym.kind     = SymbolKind::Struct;
    structSym.type     = structType;
    structSym.declNode = strct;
    structSym.loc      = strct->loc;

    if (!symbols_.declare(structSym)) {
        diags_.error("Redeclaration of struct '" + strct->name + "'", strct->loc);
    }

    stmtTypes_[strct] = structType;

    // Register members
    structType->members.clear();
    for (auto& member : strct->members) {
        if (auto* varDecl = dynamic_cast<const VarDecl*>(member.decl.get())) {
            TypePtr mType = varDecl->typeAnnot ? resolveTypeAnnotation(varDecl->typeAnnot.get())
                                               : types_.Auto();
            Type::MemberInfo mi;
            mi.name     = varDecl->name;
            mi.type     = mType;
            mi.isPublic = member.isPublic;
            mi.isStatic = member.isStatic;
            mi.isMethod = false;
            structType->members.push_back(mi);
        } else if (auto* funcDecl = dynamic_cast<const FunctionDecl*>(member.decl.get())) {
            std::vector<TypePtr> pTypes;
            bool isVariadic = false;
            for (auto& p : funcDecl->params) {
                if (p.isSpread) isVariadic = true;
                pTypes.push_back(p.typeAnnot ? resolveTypeAnnotation(p.typeAnnot.get())
                                             : types_.Auto());
            }
            TypePtr rType = funcDecl->returnType ? resolveTypeAnnotation(funcDecl->returnType.get())
                                                 : types_.Void();
            Type::MemberInfo mi;
            mi.name     = funcDecl->name;
            mi.type     = types_.makeFunction(pTypes, rType, isVariadic);
            mi.isPublic = member.isPublic;
            mi.isStatic = member.isStatic;
            mi.isMethod = true;
            structType->members.push_back(mi);
        }
    }

    // Check member initializers / method bodies
    symbols_.pushClassScope(strct->name, structType);
    for (auto& mi : structType->members) {
        Symbol mSym;
        mSym.name     = mi.name;
        mSym.kind     = mi.isMethod ? SymbolKind::Function : SymbolKind::Variable;
        mSym.type     = mi.type;
        mSym.isPublic = mi.isPublic;
        mSym.isStatic = mi.isStatic;
        symbols_.declare(mSym);
    }
    for (auto& member : strct->members) {
        checkStatement(member.decl.get());
    }
    symbols_.popScope();
}

// ------------------------------------------------------------
// Enum declaration
// ------------------------------------------------------------
inline void TypeChecker::checkEnumDecl(const EnumDecl* enm) {
    TypePtr enumType = types_.makeEnum(enm->name);

    Symbol enumSym;
    enumSym.name     = enm->name;
    enumSym.kind     = SymbolKind::Enum;
    enumSym.type     = enumType;
    enumSym.declNode = enm;
    enumSym.loc      = enm->loc;

    if (!symbols_.declare(enumSym)) {
        diags_.error("Redeclaration of enum '" + enm->name + "'", enm->loc);
    }

    stmtTypes_[enm] = enumType;

    // Register entries
    int64_t nextAutoValue = 0;
    enumType->enumEntries.clear();

    for (auto& entry : enm->entries) {
        Type::EnumEntryInfo ei;
        ei.name = entry.name;

        if (entry.value) {
            TypePtr valType = inferType(entry.value.get());
            if (valType && valType->isInteger()) {
                // Try to extract the literal value for auto-increment
                if (auto* lit = dynamic_cast<const LiteralExpr*>(entry.value.get())) {
                    try { nextAutoValue = std::stoll(lit->value); } catch (...) {}
                }
                ei.value = nextAutoValue;
                nextAutoValue++;
            } else {
                diags_.error("Enum entry '" + entry.name + "' value must be an integer", entry.loc);
                ei.value = nextAutoValue++;
            }
        } else {
            ei.value = nextAutoValue++;
        }

        enumType->enumEntries.push_back(ei);

        // Declare each entry as a symbol in the enclosing scope
        // (accessible as EnumName.EntryName, but also bare for now)
        Symbol entrySym;
        entrySym.name          = entry.name;
        entrySym.kind          = SymbolKind::EnumEntry;
        entrySym.type          = enumType;
        entrySym.loc           = entry.loc;
        entrySym.isConst       = true;
        entrySym.isMutable     = false;
        entrySym.ownerEnumType = enumType;
        symbols_.declare(entrySym);   // best-effort; may collide
    }
}

// ------------------------------------------------------------
// Control flow
// ------------------------------------------------------------
inline void TypeChecker::checkIfStatement(const IfStatement* ifStmt) {
    TypePtr condType = inferType(ifStmt->condition.get());
    if (condType && condType->kind != TypeKind::Bool && !condType->isError()) {
        diags_.error("if condition must be bool, got '" + condType->displayName() + "'",
                     ifStmt->condition->loc);
    }
    checkStatement(ifStmt->thenBranch.get());
    if (ifStmt->elseBranch) {
        checkStatement(ifStmt->elseBranch.get());
    }
}

inline void TypeChecker::checkWhileStatement(const WhileStatement* whileStmt) {
    TypePtr condType = inferType(whileStmt->condition.get());
    if (condType && condType->kind != TypeKind::Bool && !condType->isError()) {
        diags_.error("while condition must be bool, got '" + condType->displayName() + "'",
                     whileStmt->condition->loc);
    }
    checkStatement(whileStmt->body.get());
}

inline void TypeChecker::checkForStatement(const ForStatement* forStmt) {
    symbols_.pushScope(Scope::Kind::Block);   // for-init scope

    if (forStmt->init)      checkStatement(forStmt->init.get());
    if (forStmt->condition) {
        TypePtr condType = inferType(forStmt->condition.get());
        if (condType && condType->kind != TypeKind::Bool && !condType->isError()) {
            diags_.error("for condition must be bool, got '" + condType->displayName() + "'",
                         forStmt->condition->loc);
        }
    }
    if (forStmt->update)    inferType(forStmt->update.get());
    checkStatement(forStmt->body.get());

    symbols_.popScope();
}

inline void TypeChecker::checkForInStatement(const ForInStatement* forIn) {
    TypePtr iterType = inferType(forIn->iterable.get());

    // The iteration variable type depends on what we're iterating:
    //   Array<T>      → T
    //   Range<T>      → T
    //   string        → char (int8)
    TypePtr elemType;
    if (iterType) {
        if (iterType->kind == TypeKind::Array && iterType->elementType) {
            elemType = iterType->elementType;
        } else if (iterType->kind == TypeKind::Range && iterType->elementType) {
            elemType = iterType->elementType;
        } else if (iterType->kind == TypeKind::String) {
            elemType = types_.Int8();   // char
        } else if (!iterType->isError()) {
            diags_.error("Cannot iterate over type '" + iterType->displayName() + "'",
                         forIn->iterable->loc);
            elemType = types_.Error();
        }
    }
    if (!elemType) elemType = types_.Error();

    symbols_.pushScope(Scope::Kind::Block);

    // Declare the loop variable
    Symbol loopVar;
    loopVar.name     = forIn->name;
    loopVar.kind     = SymbolKind::Variable;
    loopVar.type     = elemType;
    loopVar.loc      = forIn->loc;
    loopVar.isMutable = true;
    symbols_.declare(loopVar);

    checkStatement(forIn->body.get());
    symbols_.popScope();
}

inline void TypeChecker::checkSwitchStatement(const SwitchStatement* sw) {
    TypePtr discType = inferType(sw->discriminant.get());

    for (auto& c : sw->cases) {
        if (c.value) {
            TypePtr caseType = inferType(c.value.get());
            // Case value must be comparable to discriminant
            if (discType && caseType && !discType->isError() && !caseType->isError()) {
                if (discType != caseType &&
                    !types_.isImplicitlyConvertible(caseType, discType)) {
                    diags_.error("case value type '" + caseType->displayName() +
                                 "' does not match switch type '" + discType->displayName() + "'",
                                 c.loc);
                }
            }
        }
        for (auto& s : c.body) {
            checkStatement(s.get());
        }
    }
}

inline void TypeChecker::checkReturnStatement(const ReturnStatement* ret) {
    TypePtr retType = ret->value ? inferType(ret->value.get()) : types_.Void();

    if (currentReturnType_) {
        if (currentReturnType_->isVoid() && ret->value) {
            diags_.error("Cannot return a value from a void function", ret->loc);
        } else if (!currentReturnType_->isVoid() && !ret->value) {
            diags_.error("Expected return value of type '" + currentReturnType_->displayName() + "'",
                         ret->loc);
        } else if (ret->value && !currentReturnType_->isVoid()) {
            checkAssignable(currentReturnType_, retType, ret->loc, "return statement");
        }
    }
}

// ------------------------------------------------------------
// Include (stub — real resolution is in semantic_analyzer)
// ------------------------------------------------------------
inline void TypeChecker::checkIncludeDecl(const IncludeDecl* /*inc*/) {
    // Handled by the semantic analyzer's include resolution pass.
    // Nothing to type-check here.
}

inline void TypeChecker::checkExprStatement(const ExprStatement* exprStmt) {
    inferType(exprStmt->expr.get());
}

// ------------------------------------------------------------
// Expression type inference — main dispatch
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferType(const Expression* expr) {
    if (!expr) return types_.Error();

    // Check if already cached
    auto it = exprTypes_.find(expr);
    if (it != exprTypes_.end()) return it->second;

    if (auto* e = dynamic_cast<const LiteralExpr*>(expr))              return inferLiteral(e);
    if (auto* e = dynamic_cast<const IdentifierExpr*>(expr))           return inferIdentifier(e);
    if (auto* e = dynamic_cast<const BinaryExpr*>(expr))               return inferBinary(e);
    if (auto* e = dynamic_cast<const UnaryExpr*>(expr))                return inferUnary(e);
    if (auto* e = dynamic_cast<const AssignExpr*>(expr))               return inferAssign(e);
    if (auto* e = dynamic_cast<const CallExpr*>(expr))                 return inferCall(e);
    if (auto* e = dynamic_cast<const IndexExpr*>(expr))                return inferIndex(e);
    if (auto* e = dynamic_cast<const MemberExpr*>(expr))               return inferMember(e);
    if (auto* e = dynamic_cast<const OptionalMemberExpr*>(expr))       return inferOptionalMember(e);
    if (auto* e = dynamic_cast<const NullCoalesceExpr*>(expr))         return inferNullCoalesce(e);
    if (auto* e = dynamic_cast<const ArrayLiteralExpr*>(expr))         return inferArrayLiteral(e);
    if (auto* e = dynamic_cast<const LambdaExpr*>(expr))               return inferLambda(e);
    if (auto* e = dynamic_cast<const NewExpr*>(expr))                  return inferNew(e);
    if (auto* e = dynamic_cast<const ThisExpr*>(expr))                 return inferThis(e);
    if (auto* e = dynamic_cast<const SpreadExpr*>(expr))               return inferSpread(e);
    if (auto* e = dynamic_cast<const RangeExpr*>(expr))                return inferRange(e);
    if (auto* e = dynamic_cast<const InterpolatedStringExpr*>(expr))   return inferInterpolatedString(e);
    if (auto* e = dynamic_cast<const TernaryExpr*>(expr))              return inferTernary(e);

    // SuperExpr is handled via MemberExpr on super — bare super is not an expression
    if (dynamic_cast<const SuperExpr*>(expr)) {
        diags_.error("'super' must be used as 'super.method(…)'", expr->loc);
        return cache(expr, types_.Error());
    }

    diags_.error("Unknown expression type", expr->loc);
    return cache(expr, types_.Error());
}

// ------------------------------------------------------------
// Literal type inference
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferLiteral(const LiteralExpr* lit) {
    switch (lit->kind) {
        case LiteralKind::Integer:
            return cache(lit, narrowIntegerLiteral(lit->value, false, false));
        case LiteralKind::HexInteger:
            return cache(lit, narrowIntegerLiteral(lit->value, true, false));
        case LiteralKind::BinaryInteger:
            return cache(lit, narrowIntegerLiteral(lit->value, false, true));
        case LiteralKind::Float:
        case LiteralKind::Scientific:
            return cache(lit, types_.Float64());
        case LiteralKind::String:
        case LiteralKind::RawString:
            return cache(lit, types_.String());
        case LiteralKind::Char:
            return cache(lit, types_.Int8());
        case LiteralKind::True:
        case LiteralKind::False:
            return cache(lit, types_.Bool());
        case LiteralKind::Null:
            return cache(lit, types_.Null());
    }
    return cache(lit, types_.Error());
}

// ------------------------------------------------------------
// Identifier lookup
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferIdentifier(const IdentifierExpr* id) {
    const Symbol* sym = symbols_.lookup(id->name);
    if (!sym) {
        diags_.error("Undeclared identifier '" + id->name + "'", id->loc);
        auto suggestion = symbols_.suggestSimilar(id->name);
        if (suggestion) {
            diags_.note("Did you mean '" + *suggestion + "'?", id->loc);
        }
        return cache(id, types_.Error());
    }

    // Mark as used
    Symbol* mutableSym = symbols_.lookupMut(id->name);
    if (mutableSym) mutableSym->isUsed = true;

    return cache(id, sym->type ? sym->type : types_.Error());
}

// ------------------------------------------------------------
// Binary expression
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferBinary(const BinaryExpr* bin) {
    TypePtr left  = inferType(bin->left.get());
    TypePtr right = inferType(bin->right.get());

    if (!left || left->isError() || !right || right->isError()) {
        return cache(bin, types_.Error());   // don't cascade
    }

    TypePtr result = binaryResultType(bin->op, left, right, bin->loc);
    return cache(bin, result);
}

inline TypePtr TypeChecker::binaryResultType(BinaryOp op, TypePtr left, TypePtr right, SourceLoc loc) {
    switch (op) {
        // Arithmetic — both must be numeric, result is promoted type
        case BinaryOp::Add:
        case BinaryOp::Sub:
        case BinaryOp::Mul:
        case BinaryOp::Div:
        case BinaryOp::Mod: {
            // Special case: string + anything = string (concatenation)
            if (op == BinaryOp::Add && (left->kind == TypeKind::String || right->kind == TypeKind::String)) {
                return types_.String();
            }
            if (!left->isNumeric() || !right->isNumeric()) {
                diags_.error("Operator '" + std::string(
                    op == BinaryOp::Add ? "+" : op == BinaryOp::Sub ? "-" :
                    op == BinaryOp::Mul ? "*" : op == BinaryOp::Div ? "/" : "%") +
                    "' requires numeric operands, got '" + left->displayName() +
                    "' and '" + right->displayName() + "'", loc);
                return types_.Error();
            }
            TypePtr promoted = types_.promoteNumeric(left, right);
            return promoted ? promoted : types_.Error();
        }

        // Comparison — numeric or string; result is always bool
        case BinaryOp::Less:
        case BinaryOp::LessEqual:
        case BinaryOp::Greater:
        case BinaryOp::GreaterEqual: {
            if (left->isNumeric() && right->isNumeric()) return types_.Bool();
            if (left->kind == TypeKind::String && right->kind == TypeKind::String) return types_.Bool();
            diags_.error("Cannot compare '" + left->displayName() +
                         "' and '" + right->displayName() + "'", loc);
            return types_.Error();
        }

        // Equality — any two types that are the same (or one is null)
        case BinaryOp::Equal:
        case BinaryOp::NotEqual: {
            if (left == right) return types_.Bool();
            if (left->kind == TypeKind::Null && right->isNullable()) return types_.Bool();
            if (right->kind == TypeKind::Null && left->isNullable())  return types_.Bool();
            if (left->isNumeric() && right->isNumeric())              return types_.Bool();
            diags_.error("Cannot compare '" + left->displayName() +
                         "' and '" + right->displayName() + "' for equality", loc);
            return types_.Error();
        }

        // Logical — both must be bool
        case BinaryOp::And:
        case BinaryOp::Or: {
            if (left->kind != TypeKind::Bool || right->kind != TypeKind::Bool) {
                diags_.error("Logical operator requires bool operands, got '" +
                             left->displayName() + "' and '" + right->displayName() + "'", loc);
                return types_.Error();
            }
            return types_.Bool();
        }

        // Null coalescing — handled by NullCoalesceExpr, but just in case
        case BinaryOp::NullCoalesce:
            return right;   // T? ?? T → T

        // Range — both must be same integer type
        case BinaryOp::Range:
        case BinaryOp::RangeInclusive: {
            if (!left->isInteger() || !right->isInteger()) {
                diags_.error("Range operator requires integer operands", loc);
                return types_.Error();
            }
            TypePtr promoted = types_.promoteNumeric(left, right);
            return types_.makeRange(promoted ? promoted : types_.Int32());
        }
    }
    return types_.Error();
}

// ------------------------------------------------------------
// Unary expression
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferUnary(const UnaryExpr* un) {
    TypePtr operandType = inferType(un->operand.get());
    if (!operandType || operandType->isError()) return cache(un, types_.Error());

    switch (un->op) {
        case UnaryOp::Negate:
            if (!operandType->isNumeric()) {
                diags_.error("Cannot negate non-numeric type '" + operandType->displayName() + "'",
                             un->loc);
                return cache(un, types_.Error());
            }
            return cache(un, operandType);

        case UnaryOp::Not:
            if (operandType->kind != TypeKind::Bool) {
                diags_.error("'!' requires bool operand, got '" + operandType->displayName() + "'",
                             un->loc);
                return cache(un, types_.Error());
            }
            return cache(un, types_.Bool());
    }
    return cache(un, types_.Error());
}

// ------------------------------------------------------------
// Assignment
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferAssign(const AssignExpr* assign) {
    TypePtr targetType = inferType(assign->target.get());
    TypePtr valueType  = inferType(assign->value.get());

    if (!checkLValue(assign->target.get())) {
        return cache(assign, types_.Error());
    }

    if (targetType && valueType && !targetType->isError() && !valueType->isError()) {
        // For compound assignments (+=, etc.), the RHS must be compatible
        // with the arithmetic result type
        if (assign->op != AssignOp::Assign) {
            if (!targetType->isNumeric() || !valueType->isNumeric()) {
                diags_.error("Compound assignment requires numeric types", assign->loc);
                return cache(assign, types_.Error());
            }
        }
        checkAssignable(targetType, valueType, assign->loc, "assignment");
    }

    return cache(assign, targetType ? targetType : types_.Error());
}

// ------------------------------------------------------------
// Function call
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferCall(const CallExpr* call) {
    TypePtr calleeType = inferType(call->callee.get());

    if (!calleeType || calleeType->isError()) return cache(call, types_.Error());

    if (calleeType->kind != TypeKind::Function) {
        diags_.error("Cannot call non-function type '" + calleeType->displayName() + "'",
                     call->callee->loc);
        return cache(call, types_.Error());
    }

    // Infer argument types
    std::vector<TypePtr> argTypes;
    for (auto& arg : call->args) {
        argTypes.push_back(inferType(arg.get()));
    }

    // Check argument count (if not variadic)
    if (!calleeType->isVariadic) {
        if (argTypes.size() != calleeType->paramTypes.size()) {
            diags_.error("Expected " + std::to_string(calleeType->paramTypes.size()) +
                         " argument(s), got " + std::to_string(argTypes.size()),
                         call->loc);
            return cache(call, calleeType->returnType ? calleeType->returnType : types_.Void());
        }
    } else {
        // Variadic: must have at least (paramCount - 1) arguments
        size_t minArgs = calleeType->paramTypes.size() > 0 ? calleeType->paramTypes.size() - 1 : 0;
        if (argTypes.size() < minArgs) {
            diags_.error("Expected at least " + std::to_string(minArgs) +
                         " argument(s), got " + std::to_string(argTypes.size()),
                         call->loc);
        }
    }

    // Check each argument type
    size_t checkCount = std::min(argTypes.size(), calleeType->paramTypes.size());
    for (size_t i = 0; i < checkCount; ++i) {
        if (argTypes[i] && calleeType->paramTypes[i] &&
            !argTypes[i]->isError() && !calleeType->paramTypes[i]->isError() &&
            !calleeType->paramTypes[i]->isAuto()) {
            checkAssignable(calleeType->paramTypes[i], argTypes[i],
                            call->args[i]->loc,
                            "argument " + std::to_string(i + 1));
        }
    }

    return cache(call, calleeType->returnType ? calleeType->returnType : types_.Void());
}

// ------------------------------------------------------------
// Index expression: arr[idx]
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferIndex(const IndexExpr* idx) {
    TypePtr objType   = inferType(idx->object.get());
    TypePtr indexType = inferType(idx->index.get());

    if (!objType || objType->isError()) return cache(idx, types_.Error());

    if (objType->kind == TypeKind::Array) {
        // Index must be integer
        if (indexType && !indexType->isInteger() && !indexType->isError()) {
            diags_.error("Array index must be an integer, got '" + indexType->displayName() + "'",
                         idx->index->loc);
        }
        return cache(idx, objType->elementType ? objType->elementType : types_.Error());
    }

    if (objType->kind == TypeKind::String) {
        if (indexType && !indexType->isInteger() && !indexType->isError()) {
            diags_.error("String index must be an integer, got '" + indexType->displayName() + "'",
                         idx->index->loc);
        }
        return cache(idx, types_.Int8());   // char
    }

    diags_.error("Cannot index into type '" + objType->displayName() + "'", idx->object->loc);
    return cache(idx, types_.Error());
}

// ------------------------------------------------------------
// Member access: obj.member
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferMember(const MemberExpr* mem) {
    TypePtr objType = inferType(mem->object.get());
    if (!objType || objType->isError()) return cache(mem, types_.Error());

    bool isMutable = true, isStatic = false;
    TypePtr memberType = lookupMember(objType, mem->member, isMutable, isStatic);

    if (!memberType) {
        diags_.error("Type '" + objType->displayName() + "' has no member '" + mem->member + "'",
                     mem->loc);
        return cache(mem, types_.Error());
    }

    return cache(mem, memberType);
}

// ------------------------------------------------------------
// Optional member access: obj.?member
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferOptionalMember(const OptionalMemberExpr* mem) {
    TypePtr objType = inferType(mem->object.get());
    if (!objType || objType->isError()) return cache(mem, types_.Error());

    // Unwrap Optional if present
    TypePtr innerType = objType;
    if (objType->kind == TypeKind::Optional && objType->elementType) {
        innerType = objType->elementType;
    } else if (!objType->isNullable()) {
        diags_.warning("'.?' used on non-nullable type '" + objType->displayName() + "' — use '.' instead",
                       mem->loc);
    }

    bool isMutable = true, isStatic = false;
    TypePtr memberType = lookupMember(innerType, mem->member, isMutable, isStatic);

    if (!memberType) {
        diags_.error("Type '" + innerType->displayName() + "' has no member '" + mem->member + "'",
                     mem->loc);
        return cache(mem, types_.Error());
    }

    // Result is Optional<memberType> because the object might be null
    return cache(mem, types_.makeOptional(memberType));
}

// ------------------------------------------------------------
// Null coalescing: a ?? b
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferNullCoalesce(const NullCoalesceExpr* nc) {
    TypePtr leftType  = inferType(nc->left.get());
    TypePtr rightType = inferType(nc->right.get());

    if (!leftType || leftType->isError()) return cache(nc, rightType ? rightType : types_.Error());

    // Left should be Optional<T> or nullable; result is T (unwrapped)
    if (leftType->kind == TypeKind::Optional && leftType->elementType) {
        // Check that right is assignable to T
        if (rightType && !rightType->isError()) {
            checkAssignable(leftType->elementType, rightType, nc->right->loc,
                            "'??' right-hand side");
        }
        return cache(nc, leftType->elementType);
    }

    // If left is already non-optional, ?? is a no-op but valid
    diags_.warning("'??' used on non-optional type '" + leftType->displayName() + "'", nc->loc);
    return cache(nc, leftType);
}

// ------------------------------------------------------------
// Array literal: [a, b, c]
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferArrayLiteral(const ArrayLiteralExpr* arr) {
    if (arr->elements.empty()) {
        // Empty array — type is Array<auto>, will be resolved by context
        return cache(arr, types_.makeArray(types_.Auto()));
    }

    // Infer element types and find common type
    TypePtr commonType = inferType(arr->elements[0].get());
    for (size_t i = 1; i < arr->elements.size(); ++i) {
        TypePtr elemType = inferType(arr->elements[i].get());
        if (commonType && elemType && commonType->isNumeric() && elemType->isNumeric()) {
            commonType = types_.promoteNumeric(commonType, elemType);
        } else if (commonType != elemType && elemType && !elemType->isError()) {
            // Try implicit conversion
            if (!types_.isImplicitlyConvertible(elemType, commonType)) {
                diags_.error("Array elements must have compatible types: expected '" +
                             commonType->displayName() + "', got '" + elemType->displayName() + "'",
                             arr->elements[i]->loc);
            }
        }
    }

    return cache(arr, types_.makeArray(commonType ? commonType : types_.Error()));
}

// ------------------------------------------------------------
// Lambda: (params) => expr  or  (params) => { block }
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferLambda(const LambdaExpr* lam) {
    // Build parameter types (all Auto if no annotations)
    std::vector<TypePtr> paramTypes;
    bool isVariadic = false;
    for (auto& p : lam->params) {
        if (p.isSpread) isVariadic = true;
        paramTypes.push_back(p.typeAnnot ? resolveTypeAnnotation(p.typeAnnot.get())
                                         : types_.Auto());
    }

    // We can't know the return type until we check the body,
    // so we'll set it to Auto for now and patch after.
    TypePtr retType = types_.Auto();

    // Push a function scope for the lambda body
    symbols_.pushScope(Scope::Kind::Function);

    for (size_t i = 0; i < lam->params.size(); ++i) {
        Symbol pSym;
        pSym.name     = lam->params[i].name;
        pSym.kind     = SymbolKind::Parameter;
        pSym.type     = paramTypes[i];
        pSym.loc      = lam->params[i].loc;
        pSym.isMutable = true;
        symbols_.declare(pSym);
    }

    TypePtr savedRetType = currentReturnType_;

    if (lam->bodyExpr) {
        retType = inferType(lam->bodyExpr.get());
        currentReturnType_ = retType;   // so any nested return is checked
    } else if (lam->bodyBlock) {
        currentReturnType_ = types_.Void();   // will be updated if we see a return
        checkStatement(lam->bodyBlock.get());
        retType = currentReturnType_;
    }

    currentReturnType_ = savedRetType;
    symbols_.popScope();

    TypePtr funcType = types_.makeFunction(paramTypes, retType, isVariadic);
    return cache(lam, funcType);
}

// ------------------------------------------------------------
// new ClassName(args)
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferNew(const NewExpr* newExpr) {
    TypePtr classType = types_.lookupByName(newExpr->className);
    if (!classType) {
        diags_.error("Unknown class '" + newExpr->className + "'", newExpr->loc);
        auto suggestion = symbols_.suggestSimilar(newExpr->className);
        if (suggestion) diags_.note("Did you mean '" + *suggestion + "'?", newExpr->loc);
        return cache(newExpr, types_.Error());
    }

    if (classType->kind != TypeKind::Class) {
        diags_.error("'" + newExpr->className + "' is not a class (cannot use 'new')", newExpr->loc);
        return cache(newExpr, types_.Error());
    }

    // Infer argument types (for constructor checking — constructors not fully modeled yet)
    for (auto& arg : newExpr->args) {
        inferType(arg.get());
    }

    return cache(newExpr, classType);
}

// ------------------------------------------------------------
// this
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferThis(const ThisExpr* thisExpr) {
    TypePtr thisType = symbols_.currentThisType();
    if (!thisType) {
        diags_.error("'this' used outside of a class", thisExpr->loc);
        return cache(thisExpr, types_.Error());
    }
    return cache(thisExpr, thisType);
}

// ------------------------------------------------------------
// Spread: ...expr
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferSpread(const SpreadExpr* spread) {
    TypePtr operandType = inferType(spread->operand.get());
    // Spread of an array yields the array's element type in a
    // variadic context — but the expression itself is still the
    // array type.  Actual spread semantics are checked at the
    // call site.
    return cache(spread, operandType);
}

// ------------------------------------------------------------
// Range: start..end  or  start...end
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferRange(const RangeExpr* range) {
    TypePtr startType = inferType(range->start.get());
    TypePtr endType   = inferType(range->end.get());

    if (startType && endType && startType->isInteger() && endType->isInteger()) {
        TypePtr promoted = types_.promoteNumeric(startType, endType);
        return cache(range, types_.makeRange(promoted ? promoted : types_.Int32()));
    }

    if (startType && !startType->isError()) {
        diags_.error("Range requires integer operands, got '" +
                     (startType ? startType->displayName() : "?") + "' and '" +
                     (endType   ? endType->displayName()   : "?") + "'", range->loc);
    }
    return cache(range, types_.Error());
}

// ------------------------------------------------------------
// Interpolated string: "Hello ${name}"
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferInterpolatedString(const InterpolatedStringExpr* interp) {
    // Each interpolated sub-expression must be convertible to string.
    // For now we just infer them all and warn on anything truly exotic.
    for (auto& part : interp->parts) {
        TypePtr partType = inferType(part.get());
        // Most types can be toString'd; we don't warn for now.
        (void)partType;
    }
    return cache(interp, types_.String());
}

// ------------------------------------------------------------
// Ternary: cond ? a : b
// ------------------------------------------------------------
inline TypePtr TypeChecker::inferTernary(const TernaryExpr* tern) {
    TypePtr condType  = inferType(tern->condition.get());
    TypePtr thenType  = inferType(tern->thenExpr.get());
    TypePtr elseType  = inferType(tern->elseExpr.get());

    if (condType && condType->kind != TypeKind::Bool && !condType->isError()) {
        diags_.error("Ternary condition must be bool, got '" + condType->displayName() + "'",
                     tern->condition->loc);
    }

    // Result type is the common type of then/else branches
    if (thenType && elseType && !thenType->isError() && !elseType->isError()) {
        if (thenType == elseType) return cache(tern, thenType);
        if (thenType->isNumeric() && elseType->isNumeric()) {
            return cache(tern, types_.promoteNumeric(thenType, elseType));
        }
        // One might be null → result is Optional of the other
        if (thenType->kind == TypeKind::Null && elseType->isNullable()) return cache(tern, elseType);
        if (elseType->kind == TypeKind::Null && thenType->isNullable()) return cache(tern, thenType);
        if (thenType->kind == TypeKind::Null) return cache(tern, types_.makeOptional(elseType));
        if (elseType->kind == TypeKind::Null) return cache(tern, types_.makeOptional(thenType));

        diags_.error("Ternary branches have incompatible types: '" + thenType->displayName() +
                     "' and '" + elseType->displayName() + "'", tern->loc);
        return cache(tern, types_.Error());
    }

    return cache(tern, thenType ? thenType : (elseType ? elseType : types_.Error()));
}

// ------------------------------------------------------------
// Helper: resolve type annotation string → TypePtr
// ------------------------------------------------------------
inline TypePtr TypeChecker::resolveTypeAnnotation(const TypeAnnotation* annot) {
    if (!annot) return types_.Auto();

    const std::string& name = annot->name;

    // Check primitives first (fast path)
    if (name == "int8")     return types_.Int8();
    if (name == "int16")    return types_.Int16();
    if (name == "int32")    return types_.Int32();
    if (name == "int64")    return types_.Int64();
    if (name == "uint8")    return types_.UInt8();
    if (name == "uint16")   return types_.UInt16();
    if (name == "uint32")   return types_.UInt32();
    if (name == "uint64")   return types_.UInt64();
    if (name == "float32")  return types_.Float32();
    if (name == "float64")  return types_.Float64();
    if (name == "bool")     return types_.Bool();
    if (name == "string")   return types_.String();
    if (name == "void")     return types_.Void();
    if (name == "auto")     return types_.Auto();

    // Look up user-defined type
    TypePtr userType = types_.lookupByName(name);
    if (userType) return userType;

    diags_.error("Unknown type '" + name + "'", annot->loc);
    auto suggestion = symbols_.suggestSimilar(name);
    if (suggestion) diags_.note("Did you mean '" + *suggestion + "'?", annot->loc);
    return types_.Error();
}

// ------------------------------------------------------------
// Helper: assignability check
// ------------------------------------------------------------
inline bool TypeChecker::checkAssignable(TypePtr expected, TypePtr actual, SourceLoc loc,
                                          const std::string& context) {
    if (!expected || !actual || expected->isError() || actual->isError()) return true; // suppress cascade
    if (expected == actual) return true;
    if (types_.isImplicitlyConvertible(actual, expected)) return true;

    std::string msg = "Cannot assign '" + actual->displayName() + "' to '" +
                      expected->displayName() + "'";
    if (!context.empty()) msg += " in " + context;
    diags_.error(msg, loc);
    return false;
}

// ------------------------------------------------------------
// Helper: l-value check
// ------------------------------------------------------------
inline bool TypeChecker::checkLValue(const Expression* expr) {
    // Identifiers are l-values (unless const — checked separately)
    if (dynamic_cast<const IdentifierExpr*>(expr)) {
        // Check constness
        if (auto* id = dynamic_cast<const IdentifierExpr*>(expr)) {
            const Symbol* sym = symbols_.lookup(id->name);
            if (sym && sym->isConst) {
                diags_.error("Cannot assign to constant '" + id->name + "'", expr->loc);
                return false;
            }
        }
        return true;
    }
    // Member access is an l-value (if the member is mutable)
    if (dynamic_cast<const MemberExpr*>(expr)) return true;
    // Index access is an l-value
    if (dynamic_cast<const IndexExpr*>(expr))  return true;

    diags_.error("Left-hand side of assignment is not an l-value", expr->loc);
    return false;
}

// ------------------------------------------------------------
// Helper: member lookup on a type
// ------------------------------------------------------------
inline TypePtr TypeChecker::lookupMember(TypePtr objType, const std::string& memberName,
                                          bool& isMutable, bool& isStatic) const {
    if (!objType) return nullptr;

    // Search this type's member list
    for (auto& m : objType->members) {
        if (m.name == memberName) {
            isMutable = !m.isMethod;   // methods are not mutable "values"
            isStatic  = m.isStatic;
            return m.type;
        }
    }

    // Walk parent chain
    if (objType->parentType) {
        return lookupMember(objType->parentType, memberName, isMutable, isStatic);
    }

    return nullptr;
}

// ------------------------------------------------------------
// Helper: narrow integer literal to the smallest fitting type
// ------------------------------------------------------------
inline TypePtr TypeChecker::narrowIntegerLiteral(const std::string& value,
                                                  bool isHex, bool isBinary) const {
    // Parse the actual numeric value
    int64_t val = 0;
    try {
        if (isHex) {
            // Strip "0x" or "0X" prefix
            val = std::stoll(value.substr(2), nullptr, 16);
        } else if (isBinary) {
            // Strip "0b" or "0B" prefix
            val = std::stoll(value.substr(2), nullptr, 2);
        } else {
            val = std::stoll(value);
        }
    } catch (...) {
        return types_.Int32();   // fallback
    }

    // For hex and binary literals, default to int32 (they're often
    // used for flags/masks where the width is intentional)
    if (isHex || isBinary) {
        if (val >= 0 && val <= 0x7FFFFFFF)       return types_.Int32();
        if (val >= 0 && val <= 0xFFFFFFFFLL)      return types_.UInt32();
        return types_.Int64();
    }

    // Decimal: pick the narrowest signed type that fits
    // (matches the "default integer" philosophy — int32 unless it doesn't fit)
    if (val >= -2147483648LL && val <= 2147483647LL) return types_.Int32();
    return types_.Int64();
}

} // namespace scriptlang

#endif // TYPE_CHECKER_H

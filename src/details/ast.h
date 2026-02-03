#ifndef AST_H
#define AST_H

// ------------------------------------------------------------
// ast.h — Abstract Syntax Tree node definitions.
// FIXED: Corrected Annotation structure to use string for values
// ------------------------------------------------------------

#include <string>
#include <vector>
#include <memory>
#include <ostream>

namespace scriptlang {

// ============================================================
// Forward declarations
// ============================================================
struct ASTNode;
struct Expression;
struct Statement;

using ExprPtr  = std::unique_ptr<Expression>;
using StmtPtr  = std::unique_ptr<Statement>;
using ExprList = std::vector<ExprPtr>;
using StmtList = std::vector<StmtPtr>;

// ============================================================
// Source location — carried by every node
// ============================================================
struct SourceLoc {
    int line;
    int column;

    SourceLoc() : line(0), column(0) {}
    SourceLoc(int l, int c) : line(l), column(c) {}
};

// ============================================================
// Operator enums — used by BinaryExpr and UnaryExpr.
// ============================================================

enum class BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod,
    // Comparison
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    // Logical
    And, Or,
    // Null-coalescing
    NullCoalesce,
    // Range
    Range,          // ..
    RangeInclusive, // ...
};

enum class UnaryOp {
    Negate,     // -expr
    Not,        // !expr
};

enum class AssignOp {
    Assign,         // =
    PlusAssign,     // +=
    MinusAssign,    // -=
    StarAssign,     // *=
    SlashAssign,    // /=
};

// Stream operators for operator enums
inline std::ostream& operator<<(std::ostream& os, BinaryOp op) {
    switch (op) {
        case BinaryOp::Add:            return os << "BinaryOp::Add";
        case BinaryOp::Sub:            return os << "BinaryOp::Sub";
        case BinaryOp::Mul:            return os << "BinaryOp::Mul";
        case BinaryOp::Div:            return os << "BinaryOp::Div";
        case BinaryOp::Mod:            return os << "BinaryOp::Mod";
        case BinaryOp::Equal:          return os << "BinaryOp::Equal";
        case BinaryOp::NotEqual:       return os << "BinaryOp::NotEqual";
        case BinaryOp::Less:           return os << "BinaryOp::Less";
        case BinaryOp::LessEqual:      return os << "BinaryOp::LessEqual";
        case BinaryOp::Greater:        return os << "BinaryOp::Greater";
        case BinaryOp::GreaterEqual:   return os << "BinaryOp::GreaterEqual";
        case BinaryOp::And:            return os << "BinaryOp::And";
        case BinaryOp::Or:             return os << "BinaryOp::Or";
        case BinaryOp::NullCoalesce:   return os << "BinaryOp::NullCoalesce";
        case BinaryOp::Range:          return os << "BinaryOp::Range";
        case BinaryOp::RangeInclusive: return os << "BinaryOp::RangeInclusive";
    }
    return os << "BinaryOp(<unknown>)";
}

inline std::ostream& operator<<(std::ostream& os, UnaryOp op) {
    switch (op) {
        case UnaryOp::Negate: return os << "UnaryOp::Negate";
        case UnaryOp::Not:    return os << "UnaryOp::Not";
    }
    return os << "UnaryOp(<unknown>)";
}

inline std::ostream& operator<<(std::ostream& os, AssignOp op) {
    switch (op) {
        case AssignOp::Assign:      return os << "AssignOp::Assign";
        case AssignOp::PlusAssign:  return os << "AssignOp::PlusAssign";
        case AssignOp::MinusAssign: return os << "AssignOp::MinusAssign";
        case AssignOp::StarAssign:  return os << "AssignOp::StarAssign";
        case AssignOp::SlashAssign: return os << "AssignOp::SlashAssign";
    }
    return os << "AssignOp(<unknown>)";
}

// ============================================================
// Type annotation
// ============================================================
struct TypeAnnotation {
    std::string name;   // e.g. "int32", "string", "Player"
    SourceLoc   loc;

    TypeAnnotation() = default;
    TypeAnnotation(std::string n, SourceLoc l)
        : name(std::move(n)), loc(l) {}
};

using TypeAnnotPtr = std::unique_ptr<TypeAnnotation>;

// ============================================================
// Parameter — used in function & lambda declarations
// ============================================================
struct Parameter {
    std::string     name;
    TypeAnnotPtr    typeAnnot;  // nullptr if omitted (auto-inferred)
    bool            isSpread;   // true if declared as ...name
    SourceLoc       loc;
};

// ============================================================
// Annotation — @name(args)  e.g. @expose(min=0, max=100)
// FIXED: Using string for values instead of nested Annotation
// ============================================================
struct Annotation {
    std::string                                      name;
    std::vector<std::pair<std::string, std::string>> args;  // FIXED: string values
    SourceLoc                                        loc;
};

// ============================================================
// BASE NODES
// ============================================================

struct ASTNode {
    SourceLoc loc;
    ASTNode() = default;
    ASTNode(SourceLoc l) : loc(l) {}
    virtual ~ASTNode() = default;
};

struct Expression : ASTNode {
    using ASTNode::ASTNode;
};

struct Statement : ASTNode {
    using ASTNode::ASTNode;
};

// ============================================================
//  E X P R E S S I O N   N O D E S
// ============================================================

enum class LiteralKind {
    Integer,        // decimal integer
    HexInteger,     // 0x...
    BinaryInteger,  // 0b...
    Float,          // decimal float
    Scientific,     // 1.5e-3
    String,         // "..."
    RawString,      // `...`
    Char,           // '.'
    True,
    False,
    Null,
};

inline std::ostream& operator<<(std::ostream& os, LiteralKind kind) {
    switch (kind) {
        case LiteralKind::Integer:       return os << "LiteralKind::Integer";
        case LiteralKind::HexInteger:    return os << "LiteralKind::HexInteger";
        case LiteralKind::BinaryInteger: return os << "LiteralKind::BinaryInteger";
        case LiteralKind::Float:         return os << "LiteralKind::Float";
        case LiteralKind::Scientific:    return os << "LiteralKind::Scientific";
        case LiteralKind::String:        return os << "LiteralKind::String";
        case LiteralKind::RawString:     return os << "LiteralKind::RawString";
        case LiteralKind::Char:          return os << "LiteralKind::Char";
        case LiteralKind::True:          return os << "LiteralKind::True";
        case LiteralKind::False:         return os << "LiteralKind::False";
        case LiteralKind::Null:          return os << "LiteralKind::Null";
    }
    return os << "LiteralKind(<unknown>)";
}

struct LiteralExpr : Expression {
    LiteralKind kind;
    std::string value;

    LiteralExpr(LiteralKind k, std::string v, SourceLoc loc)
        : Expression(loc), kind(k), value(std::move(v)) {}
};

struct IdentifierExpr : Expression {
    std::string name;

    IdentifierExpr(std::string n, SourceLoc loc)
        : Expression(loc), name(std::move(n)) {}
};

struct BinaryExpr : Expression {
    BinaryOp op;
    ExprPtr  left;
    ExprPtr  right;

    BinaryExpr(BinaryOp o, ExprPtr l, ExprPtr r, SourceLoc loc)
        : Expression(loc), op(o), left(std::move(l)), right(std::move(r)) {}
};

struct UnaryExpr : Expression {
    UnaryOp op;
    ExprPtr operand;

    UnaryExpr(UnaryOp o, ExprPtr e, SourceLoc loc)
        : Expression(loc), op(o), operand(std::move(e)) {}
};

struct AssignExpr : Expression {
    AssignOp op;
    ExprPtr  target;
    ExprPtr  value;

    AssignExpr(AssignOp o, ExprPtr t, ExprPtr v, SourceLoc loc)
        : Expression(loc), op(o), target(std::move(t)), value(std::move(v)) {}
};

struct CallExpr : Expression {
    ExprPtr  callee;
    ExprList args;

    CallExpr(ExprPtr c, ExprList a, SourceLoc loc)
        : Expression(loc), callee(std::move(c)), args(std::move(a)) {}
};

struct IndexExpr : Expression {
    ExprPtr object;
    ExprPtr index;

    IndexExpr(ExprPtr o, ExprPtr i, SourceLoc loc)
        : Expression(loc), object(std::move(o)), index(std::move(i)) {}
};

struct MemberExpr : Expression {
    ExprPtr     object;
    std::string member;

    MemberExpr(ExprPtr o, std::string m, SourceLoc loc)
        : Expression(loc), object(std::move(o)), member(std::move(m)) {}
};

struct OptionalMemberExpr : Expression {
    ExprPtr     object;
    std::string member;

    OptionalMemberExpr(ExprPtr o, std::string m, SourceLoc loc)
        : Expression(loc), object(std::move(o)), member(std::move(m)) {}
};

struct NullCoalesceExpr : Expression {
    ExprPtr left;
    ExprPtr right;

    NullCoalesceExpr(ExprPtr l, ExprPtr r, SourceLoc loc)
        : Expression(loc), left(std::move(l)), right(std::move(r)) {}
};

struct ArrayLiteralExpr : Expression {
    ExprList elements;

    ArrayLiteralExpr(ExprList e, SourceLoc loc)
        : Expression(loc), elements(std::move(e)) {}
};

struct LambdaExpr : Expression {
    std::vector<Parameter> params;
    ExprPtr  bodyExpr;
    StmtPtr  bodyBlock;

    LambdaExpr(std::vector<Parameter> p, ExprPtr be, StmtPtr bb, SourceLoc loc)
        : Expression(loc), params(std::move(p)),
          bodyExpr(std::move(be)), bodyBlock(std::move(bb)) {}
};

struct NewExpr : Expression {
    std::string  className;
    ExprList     args;

    NewExpr(std::string c, ExprList a, SourceLoc loc)
        : Expression(loc), className(std::move(c)), args(std::move(a)) {}
};

struct ThisExpr : Expression {
    ThisExpr(SourceLoc loc) : Expression(loc) {}
};

struct SuperExpr : Expression {
    SuperExpr(SourceLoc loc) : Expression(loc) {}
};

struct SpreadExpr : Expression {
    ExprPtr operand;

    SpreadExpr(ExprPtr o, SourceLoc loc)
        : Expression(loc), operand(std::move(o)) {}
};

struct RangeExpr : Expression {
    ExprPtr  start;
    ExprPtr  end;
    bool     inclusive;

    RangeExpr(ExprPtr s, ExprPtr e, bool incl, SourceLoc loc)
        : Expression(loc), start(std::move(s)), end(std::move(e)), inclusive(incl) {}
};

struct InterpolatedStringExpr : Expression {
    std::vector<std::string> segments;
    ExprList                 parts;

    InterpolatedStringExpr(std::vector<std::string> seg, ExprList p, SourceLoc loc)
        : Expression(loc), segments(std::move(seg)), parts(std::move(p)) {}
};

struct TernaryExpr : Expression {
    ExprPtr condition;
    ExprPtr thenExpr;
    ExprPtr elseExpr;

    TernaryExpr(ExprPtr c, ExprPtr t, ExprPtr e, SourceLoc loc)
        : Expression(loc), condition(std::move(c)),
          thenExpr(std::move(t)), elseExpr(std::move(e)) {}
};

// ============================================================
//  S T A T E M E N T   N O D E S
// ============================================================

struct ExprStatement : Statement {
    ExprPtr expr;

    ExprStatement(ExprPtr e, SourceLoc loc)
        : Statement(loc), expr(std::move(e)) {}
};

struct BlockStatement : Statement {
    StmtList statements;

    BlockStatement(StmtList s, SourceLoc loc)
        : Statement(loc), statements(std::move(s)) {}
};

struct VarDecl : Statement {
    std::string  name;
    TypeAnnotPtr typeAnnot;
    ExprPtr      initializer;
    SourceLoc    loc;

    VarDecl(std::string n, TypeAnnotPtr t, ExprPtr init, SourceLoc l)
        : Statement(l), name(std::move(n)), typeAnnot(std::move(t)),
          initializer(std::move(init)), loc(l) {}
};

struct ConstDecl : Statement {
    std::string  name;
    TypeAnnotPtr typeAnnot;
    ExprPtr      initializer;

    ConstDecl(std::string n, TypeAnnotPtr t, ExprPtr init, SourceLoc loc)
        : Statement(loc), name(std::move(n)), typeAnnot(std::move(t)),
          initializer(std::move(init)) {}
};

struct ReturnStatement : Statement {
    ExprPtr value;

    ReturnStatement(ExprPtr v, SourceLoc loc)
        : Statement(loc), value(std::move(v)) {}
};

struct IfStatement : Statement {
    ExprPtr condition;
    StmtPtr thenBranch;
    StmtPtr elseBranch;

    IfStatement(ExprPtr c, StmtPtr t, StmtPtr e, SourceLoc loc)
        : Statement(loc), condition(std::move(c)),
          thenBranch(std::move(t)), elseBranch(std::move(e)) {}
};

struct WhileStatement : Statement {
    ExprPtr condition;
    StmtPtr body;

    WhileStatement(ExprPtr c, StmtPtr b, SourceLoc loc)
        : Statement(loc), condition(std::move(c)), body(std::move(b)) {}
};

struct ForStatement : Statement {
    StmtPtr init;
    ExprPtr condition;
    ExprPtr update;
    StmtPtr body;

    ForStatement(StmtPtr i, ExprPtr c, ExprPtr u, StmtPtr b, SourceLoc loc)
        : Statement(loc), init(std::move(i)), condition(std::move(c)),
          update(std::move(u)), body(std::move(b)) {}
};

struct ForInStatement : Statement {
    std::string name;
    ExprPtr     iterable;
    StmtPtr     body;

    ForInStatement(std::string n, ExprPtr iter, StmtPtr b, SourceLoc loc)
        : Statement(loc), name(std::move(n)),
          iterable(std::move(iter)), body(std::move(b)) {}
};

struct BreakStatement : Statement {
    BreakStatement(SourceLoc loc) : Statement(loc) {}
};

struct ContinueStatement : Statement {
    ContinueStatement(SourceLoc loc) : Statement(loc) {}
};

struct SwitchCase {
    ExprPtr     value;
    StmtList    body;
    SourceLoc   loc;
};

struct SwitchStatement : Statement {
    ExprPtr                 discriminant;
    std::vector<SwitchCase> cases;

    SwitchStatement(ExprPtr d, std::vector<SwitchCase> c, SourceLoc loc)
        : Statement(loc), discriminant(std::move(d)), cases(std::move(c)) {}
};

struct FunctionDecl : Statement {
    std::string            name;
    std::vector<Parameter> params;
    TypeAnnotPtr           returnType;
    StmtPtr                body;
    bool                   isStatic;
    bool                   isPublic;
    std::vector<Annotation> annotations;

    FunctionDecl(std::string n, std::vector<Parameter> p,
                 TypeAnnotPtr ret, StmtPtr b,
                 bool stat, bool pub, std::vector<Annotation> ann, SourceLoc loc)
        : Statement(loc), name(std::move(n)), params(std::move(p)),
          returnType(std::move(ret)), body(std::move(b)),
          isStatic(stat), isPublic(pub), annotations(std::move(ann)) {}
};

struct ClassMember {
    bool                    isPublic;
    bool                    isStatic;
    std::vector<Annotation> annotations;
    StmtPtr                 decl;
};

struct ClassDecl : Statement {
    std::string              name;
    std::string              parent;
    std::vector<ClassMember> members;
    std::vector<Annotation>  annotations;

    ClassDecl(std::string n, std::string p,
              std::vector<ClassMember> m, std::vector<Annotation> ann, SourceLoc loc)
        : Statement(loc), name(std::move(n)), parent(std::move(p)),
          members(std::move(m)), annotations(std::move(ann)) {}
};

struct StructDecl : Statement {
    std::string              name;
    std::vector<ClassMember> members;
    std::vector<Annotation>  annotations;

    StructDecl(std::string n, std::vector<ClassMember> m,
               std::vector<Annotation> ann, SourceLoc loc)
        : Statement(loc), name(std::move(n)), members(std::move(m)),
          annotations(std::move(ann)) {}
};

struct EnumEntry {
    std::string name;
    ExprPtr     value;
    SourceLoc   loc;
};

struct EnumDecl : Statement {
    std::string              name;
    std::vector<EnumEntry>   entries;
    std::vector<Annotation>  annotations;

    EnumDecl(std::string n, std::vector<EnumEntry> e,
             std::vector<Annotation> ann, SourceLoc loc)
        : Statement(loc), name(std::move(n)), entries(std::move(e)),
          annotations(std::move(ann)) {}
};

struct IncludeDecl : Statement {
    std::string              path;
    std::vector<std::string> selectedNames;

    IncludeDecl(std::string p, std::vector<std::string> names, SourceLoc loc)
        : Statement(loc), path(std::move(p)), selectedNames(std::move(names)) {}
};

// ============================================================
// Program — the root node
// ============================================================
struct Program {
    StmtList  statements;
    SourceLoc loc;

    Program(StmtList s, SourceLoc l)
        : statements(std::move(s)), loc(l) {}
};

} // namespace scriptlang

#endif // AST_H

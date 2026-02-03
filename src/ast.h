#ifndef AST_H
#define AST_H

// ------------------------------------------------------------
// ast.h — Abstract Syntax Tree node definitions.
//
// DESIGN PHILOSOPHY (bytecode-codegen friendly):
//   - Every Expression node has a clear stack semantic:
//       "evaluate my children left-to-right, then push one result"
//   - Every Statement node has a side-effect semantic:
//       "execute, push nothing onto the value stack"
//   - Declarations are statements that also introduce a name.
//   - Operator identity is stored as a plain enum, not a string,
//       so the bytecode emitter can switch on it in O(1).
//   - All child nodes are owned via unique_ptr so the tree is
//       a straightforward heap structure — no arenas needed yet.
//   - Source location (line, column) is on every node for
//       error messages and debug-info emission.
//
// NODE TAXONOMY
//   Expressions (produce a value):
//     Literal, Identifier, BinaryExpr, UnaryExpr, AssignExpr,
//     CallExpr, IndexExpr, MemberExpr, OptionalMemberExpr,
//     NullCoalesceExpr, ArrayLiteralExpr, LambdaExpr,
//     NewExpr, ThisExpr, SuperExpr, SpreadExpr, RangeExpr,
//     InterpolatedStringExpr, TernaryExpr
//
//   Statements (no value):
//     ExprStatement, BlockStatement, VarDecl, ConstDecl,
//     ReturnStatement, IfStatement, WhileStatement,
//     ForStatement, ForInStatement, BreakStatement,
//     ContinueStatement, SwitchStatement, FunctionDecl,
//     ClassDecl, StructDecl, EnumDecl, IncludeDecl
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
// Mapping to bytecodes is a straight switch in the emitter.
// ============================================================

enum class BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod,
    // Comparison
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    // Logical
    And, Or,
    // Null-coalescing (kept here so codegen sees it as one op)
    NullCoalesce,
    // Range
    Range,          // ..
    RangeInclusive, // ... (spread is separate; this is inclusive-range)
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

// ------------------------------------------------------------
// Stream operators for operator enums — needed so EXPECT_EQ
// can print these types in test failure messages.
// ------------------------------------------------------------

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
// Type annotation — lightweight, no resolution yet.
// Stored as the textual name + optional generic params.
// The resolver/type-checker will turn this into a resolved type.
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
// Stored as raw key=value strings; semantic meaning is
// resolved later by the attribute system.
// ============================================================
struct Annotation {
    std::string                          name;
    
std::vector<std::pair<std::string, Annotation>> args;

    SourceLoc                            loc;
std::vector<Annotation> array;  // for Array
};

// ============================================================
// BASE NODES
// ============================================================

// Every node in the tree.  `kind` is the discriminator for
// the codegen visitor — no virtual dispatch needed, just a switch.
struct ASTNode {
    SourceLoc loc;
    ASTNode() = default;
    ASTNode(SourceLoc l) : loc(l) {}
    virtual ~ASTNode() = default;  // unique_ptr needs this
};

// Expression base.  Expressions push exactly one value.
struct Expression : ASTNode {
    using ASTNode::ASTNode;
};

// Statement base.  Statements have side effects only.
struct Statement : ASTNode {
    using ASTNode::ASTNode;
};

// ============================================================
//  E X P R E S S I O N   N O D E S
// ============================================================

// ------------------------------------------------------------
// Literal values — the leaves of the expression tree.
// `kind` tells the bytecode emitter which constant pool slot type
// to use.  `value` is the raw source text; actual parsing of the
// numeric value (e.g. 0xFF → 255) happens in a later compile pass
// or right here if you prefer.
// ------------------------------------------------------------
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
    std::string value;  // raw text (empty for true/false/null)

    LiteralExpr(LiteralKind k, std::string v, SourceLoc loc)
        : Expression(loc), kind(k), value(std::move(v)) {}
};

// ------------------------------------------------------------
// Identifier reference — a bare name like `playerHealth`.
// The resolver will later attach a binding (local slot, upvalue,
// global, class field, etc.).
// ------------------------------------------------------------
struct IdentifierExpr : Expression {
    std::string name;

    IdentifierExpr(std::string n, SourceLoc loc)
        : Expression(loc), name(std::move(n)) {}
};

// ------------------------------------------------------------
// Binary expression — left OP right.
// Codegen: emit(left), emit(right), emit(BinaryOp).
// ------------------------------------------------------------
struct BinaryExpr : Expression {
    BinaryOp op;
    ExprPtr  left;
    ExprPtr  right;

    BinaryExpr(BinaryOp o, ExprPtr l, ExprPtr r, SourceLoc loc)
        : Expression(loc), op(o), left(std::move(l)), right(std::move(r)) {}
};

// ------------------------------------------------------------
// Unary expression — OP operand (prefix only for now).
// Codegen: emit(operand), emit(UnaryOp).
// ------------------------------------------------------------
struct UnaryExpr : Expression {
    UnaryOp op;
    ExprPtr operand;

    UnaryExpr(UnaryOp o, ExprPtr e, SourceLoc loc)
        : Expression(loc), op(o), operand(std::move(e)) {}
};

// ------------------------------------------------------------
// Assignment — target OP= value.
// `target` must resolve to an l-value (identifier, index, member).
// Codegen: emit(value), emit(store-to-target).
// For compound assignments (+=, etc.), the emitter expands into
// load + binary + store.
// ------------------------------------------------------------
struct AssignExpr : Expression {
    AssignOp op;
    ExprPtr  target;   // IdentifierExpr | IndexExpr | MemberExpr
    ExprPtr  value;

    AssignExpr(AssignOp o, ExprPtr t, ExprPtr v, SourceLoc loc)
        : Expression(loc), op(o), target(std::move(t)), value(std::move(v)) {}
};

// ------------------------------------------------------------
// Function / method call — callee(args).
// Codegen: emit(callee), emit each arg left-to-right,
//          emit(Call, argc).
// ------------------------------------------------------------
struct CallExpr : Expression {
    ExprPtr  callee;
    ExprList args;

    CallExpr(ExprPtr c, ExprList a, SourceLoc loc)
        : Expression(loc), callee(std::move(c)), args(std::move(a)) {}
};

// ------------------------------------------------------------
// Index access — object[index].
// As an l-value it becomes a store target; as an r-value it loads.
// Codegen (r-value): emit(object), emit(index), emit(IndexGet).
// ------------------------------------------------------------
struct IndexExpr : Expression {
    ExprPtr object;
    ExprPtr index;

    IndexExpr(ExprPtr o, ExprPtr i, SourceLoc loc)
        : Expression(loc), object(std::move(o)), index(std::move(i)) {}
};

// ------------------------------------------------------------
// Member access — object.member
// Codegen (r-value): emit(object), emit(FieldGet "member").
// ------------------------------------------------------------
struct MemberExpr : Expression {
    ExprPtr     object;
    std::string member;

    MemberExpr(ExprPtr o, std::string m, SourceLoc loc)
        : Expression(loc), object(std::move(o)), member(std::move(m)) {}
};

// ------------------------------------------------------------
// Optional member access — object?.member
// If object is null, short-circuits to null without crashing.
// Codegen: emit(object), emit(OptionalFieldGet "member").
// ------------------------------------------------------------
struct OptionalMemberExpr : Expression {
    ExprPtr     object;
    std::string member;

    OptionalMemberExpr(ExprPtr o, std::string m, SourceLoc loc)
        : Expression(loc), object(std::move(o)), member(std::move(m)) {}
};

// ------------------------------------------------------------
// Null-coalescing — left ?? right.
// Short-circuit: if left is non-null, return it; else eval right.
// Codegen: emit(left), emit(JumpIfNotNull, skip), pop, emit(right), label(skip).
// ------------------------------------------------------------
struct NullCoalesceExpr : Expression {
    ExprPtr left;
    ExprPtr right;

    NullCoalesceExpr(ExprPtr l, ExprPtr r, SourceLoc loc)
        : Expression(loc), left(std::move(l)), right(std::move(r)) {}
};

// ------------------------------------------------------------
// Array literal — [elem, elem, ...]
// Codegen: emit each element, emit(ArrayNew, count).
// ------------------------------------------------------------
struct ArrayLiteralExpr : Expression {
    ExprList elements;

    ArrayLiteralExpr(ExprList e, SourceLoc loc)
        : Expression(loc), elements(std::move(e)) {}
};

// ------------------------------------------------------------
// Lambda / arrow function — (params) => body
// Body is either a single expression (implicit return) or a block.
// Codegen: emit(Closure, function-prototype-index).
// The prototype is compiled separately and stored in the constant pool.
// ------------------------------------------------------------
struct LambdaExpr : Expression {
    std::vector<Parameter> params;
    // Exactly one of these is set:
    ExprPtr  bodyExpr;   // single-expression body  (params) => expr
    StmtPtr  bodyBlock;  // block body              (params) => { ... }

    LambdaExpr(std::vector<Parameter> p, ExprPtr be, StmtPtr bb, SourceLoc loc)
        : Expression(loc), params(std::move(p)),
          bodyExpr(std::move(be)), bodyBlock(std::move(bb)) {}
};

// ------------------------------------------------------------
// `new` expression — new ClassName(args)
// Codegen: emit(NewInstance "ClassName"), emit args, emit(Call, argc).
// ------------------------------------------------------------
struct NewExpr : Expression {
    std::string  className;
    ExprList     args;

    NewExpr(std::string c, ExprList a, SourceLoc loc)
        : Expression(loc), className(std::move(c)), args(std::move(a)) {}
};

// ------------------------------------------------------------
// `this` — reference to the current instance.
// Codegen: emit(LoadThis).
// ------------------------------------------------------------
struct ThisExpr : Expression {
    ThisExpr(SourceLoc loc) : Expression(loc) {}
};

// ------------------------------------------------------------
// `super` — reference to the parent class (used as super.method()).
// Codegen: emit(LoadSuper).
// ------------------------------------------------------------
struct SuperExpr : Expression {
    SuperExpr(SourceLoc loc) : Expression(loc) {}
};

// ------------------------------------------------------------
// Spread expression — ...expr  (inside call args or array literals)
// Codegen: emit(expr), emit(Spread).
// ------------------------------------------------------------
struct SpreadExpr : Expression {
    ExprPtr operand;

    SpreadExpr(ExprPtr o, SourceLoc loc)
        : Expression(loc), operand(std::move(o)) {}
};

// ------------------------------------------------------------
// Range expression — start..end  or  start...end
// Used in for-in loops and array slicing.
// `inclusive` = true for ..  (0..10 means 0,1,...,9)
//            ... is spread in other contexts; in range context
//            it could mean inclusive upper bound — parser decides.
// Codegen: emit(start), emit(end), emit(MakeRange, inclusive).
// ------------------------------------------------------------
struct RangeExpr : Expression {
    ExprPtr  start;
    ExprPtr  end;
    bool     inclusive;  // true = .., false = ... (exclusive end)

    RangeExpr(ExprPtr s, ExprPtr e, bool incl, SourceLoc loc)
        : Expression(loc), start(std::move(s)), end(std::move(e)), inclusive(incl) {}
};

// ------------------------------------------------------------
// Interpolated string — "Hello ${name}, you are ${age}!"
// Stored as alternating segments: literal parts and expression parts.
// `parts` contains the sub-expressions to evaluate.
// `segments` contains the literal string pieces between them.
// Layout: segments[0] + parts[0] + segments[1] + parts[1] + ... + segments[N]
// segments.size() == parts.size() + 1  (may have empty strings at edges)
// Codegen: emit each part, emit(InterpolatedConcat, part_count).
// ------------------------------------------------------------
struct InterpolatedStringExpr : Expression {
    std::vector<std::string> segments;  // literal pieces
    ExprList                 parts;     // expressions inside ${}

    InterpolatedStringExpr(std::vector<std::string> seg, ExprList p, SourceLoc loc)
        : Expression(loc), segments(std::move(seg)), parts(std::move(p)) {}
};

// ------------------------------------------------------------
// Ternary / conditional — condition ? thenExpr : elseExpr
// (Not in the current syntax.mp but natural to include for
//  completeness since we already have `?` token.)
// Codegen: emit(cond), JumpIfFalse, emit(then), Jump, emit(else).
// ------------------------------------------------------------
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

// ------------------------------------------------------------
// Expression statement — wraps an expression as a statement.
// The value is discarded (Pop in bytecode).
// Codegen: emit(expr), emit(Pop).
// ------------------------------------------------------------
struct ExprStatement : Statement {
    ExprPtr expr;

    ExprStatement(ExprPtr e, SourceLoc loc)
        : Statement(loc), expr(std::move(e)) {}
};

// ------------------------------------------------------------
// Block — { stmt; stmt; ... }
// Codegen: emit each child statement sequentially.
// Also marks a scope boundary for local variable lifetimes.
// ------------------------------------------------------------
struct BlockStatement : Statement {
    StmtList statements;

    BlockStatement(StmtList s, SourceLoc loc)
        : Statement(loc), statements(std::move(s)) {}
};

// ------------------------------------------------------------
// Variable declaration — var name [: type] [= init];
// Codegen: emit(init) or emit(LoadNull), emit(DefineLocal "name").
// ------------------------------------------------------------
struct VarDecl : Statement {
    std::string  name;
    TypeAnnotPtr typeAnnot;   // nullptr if omitted
    ExprPtr      initializer; // nullptr if omitted (defaults to null)
    SourceLoc    loc;

    VarDecl(std::string n, TypeAnnotPtr t, ExprPtr init, SourceLoc l)
        : Statement(l), name(std::move(n)), typeAnnot(std::move(t)),
          initializer(std::move(init)), loc(l) {}
};

// ------------------------------------------------------------
// Const declaration — const name [: type] = init;
// Initializer is required for const.
// Codegen: same as VarDecl but slot is marked immutable.
// ------------------------------------------------------------
struct ConstDecl : Statement {
    std::string  name;
    TypeAnnotPtr typeAnnot;
    ExprPtr      initializer;  // required

    ConstDecl(std::string n, TypeAnnotPtr t, ExprPtr init, SourceLoc loc)
        : Statement(loc), name(std::move(n)), typeAnnot(std::move(t)),
          initializer(std::move(init)) {}
};

// ------------------------------------------------------------
// Return statement — return [expr];
// Codegen: emit(expr) or emit(LoadNull), emit(Return).
// ------------------------------------------------------------
struct ReturnStatement : Statement {
    ExprPtr value;  // nullptr for bare `return;`

    ReturnStatement(ExprPtr v, SourceLoc loc)
        : Statement(loc), value(std::move(v)) {}
};

// ------------------------------------------------------------
// If statement — if (cond) then [else alt]
// `alt` is nullptr when there is no else clause.
// `alt` may itself be another IfStatement (else-if chain).
// Codegen: emit(cond), JumpIfFalse(alt), emit(then), Jump(end), label(alt), emit(alt), label(end).
// ------------------------------------------------------------
struct IfStatement : Statement {
    ExprPtr condition;
    StmtPtr thenBranch;   // always a BlockStatement
    StmtPtr elseBranch;   // nullptr | BlockStatement | IfStatement

    IfStatement(ExprPtr c, StmtPtr t, StmtPtr e, SourceLoc loc)
        : Statement(loc), condition(std::move(c)),
          thenBranch(std::move(t)), elseBranch(std::move(e)) {}
};

// ------------------------------------------------------------
// While loop — while (cond) body
// Codegen: label(top), emit(cond), JumpIfFalse(end),
//          emit(body), Jump(top), label(end).
// ------------------------------------------------------------
struct WhileStatement : Statement {
    ExprPtr condition;
    StmtPtr body;  // BlockStatement

    WhileStatement(ExprPtr c, StmtPtr b, SourceLoc loc)
        : Statement(loc), condition(std::move(c)), body(std::move(b)) {}
};

// ------------------------------------------------------------
// C-style for loop — for (init; cond; update) body
// Any of init/cond/update may be nullptr.
// Codegen: emit(init), label(top), emit(cond)/JumpIfFalse(end),
//          emit(body), emit(update), Jump(top), label(end).
// ------------------------------------------------------------
struct ForStatement : Statement {
    StmtPtr init;       // VarDecl or ExprStatement or nullptr
    ExprPtr condition;  // nullptr means `true`
    ExprPtr update;     // nullptr means no update
    StmtPtr body;       // BlockStatement

    ForStatement(StmtPtr i, ExprPtr c, ExprPtr u, StmtPtr b, SourceLoc loc)
        : Statement(loc), init(std::move(i)), condition(std::move(c)),
          update(std::move(u)), body(std::move(b)) {}
};

// ------------------------------------------------------------
// For-in loop — for name in iterable body
// Codegen: emit(iterable), emit(GetIterator),
//          label(top), emit(IteratorNext)/JumpIfDone(end),
//          DefineLocal(name), emit(body), Jump(top), label(end).
// ------------------------------------------------------------
struct ForInStatement : Statement {
    std::string name;       // loop variable
    ExprPtr     iterable;   // expression that produces the collection
    StmtPtr     body;       // BlockStatement

    ForInStatement(std::string n, ExprPtr iter, StmtPtr b, SourceLoc loc)
        : Statement(loc), name(std::move(n)),
          iterable(std::move(iter)), body(std::move(b)) {}
};

// ------------------------------------------------------------
// Break statement
// Codegen: emit(Jump) to the innermost loop/switch end label.
// ------------------------------------------------------------
struct BreakStatement : Statement {
    BreakStatement(SourceLoc loc) : Statement(loc) {}
};

// ------------------------------------------------------------
// Continue statement
// Codegen: emit(Jump) to the innermost loop's update/top label.
// ------------------------------------------------------------
struct ContinueStatement : Statement {
    ContinueStatement(SourceLoc loc) : Statement(loc) {}
};

// ------------------------------------------------------------
// Switch statement
// Codegen: emit(discriminant), for each case emit a compare+jump,
//          then fall through the matched case's body.
// ------------------------------------------------------------
struct SwitchCase {
    ExprPtr     value;      // nullptr for `default`
    StmtList    body;       // statements in this case
    SourceLoc   loc;
};

struct SwitchStatement : Statement {
    ExprPtr                discriminant;
    std::vector<SwitchCase> cases;   // default case (value==nullptr) can be anywhere

    SwitchStatement(ExprPtr d, std::vector<SwitchCase> c, SourceLoc loc)
        : Statement(loc), discriminant(std::move(d)), cases(std::move(c)) {}
};

// ------------------------------------------------------------
// Function declaration
//   [visibility] [static] returnType? name(params) [-> returnType] { body }
// Codegen: compile body into a separate FunctionPrototype, store
//          in the constant pool, emit(DefineFunction, protoIndex).
// ------------------------------------------------------------
struct FunctionDecl : Statement {
    std::string            name;
    std::vector<Parameter> params;
    TypeAnnotPtr           returnType;    // nullptr if omitted (void or inferred)
    StmtPtr                body;          // BlockStatement
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

// ------------------------------------------------------------
// Class member — a field or method inside a class body.
// Fields are VarDecl/ConstDecl; methods are FunctionDecl.
// We wrap them here to attach visibility + annotations uniformly.
// ------------------------------------------------------------
struct ClassMember {
    bool                   isPublic;
    bool                   isStatic;
    std::vector<Annotation> annotations;
    StmtPtr                decl;  // VarDecl | ConstDecl | FunctionDecl
};

// ------------------------------------------------------------
// Class declaration
// Codegen: emit(DefineClass, name, parentName, memberCount),
//          followed by field/method descriptors.
// ------------------------------------------------------------
struct ClassDecl : Statement {
    std::string             name;
    std::string             parent;          // empty if no inheritance
    std::vector<ClassMember> members;
    std::vector<Annotation> annotations;

    ClassDecl(std::string n, std::string p,
              std::vector<ClassMember> m, std::vector<Annotation> ann, SourceLoc loc)
        : Statement(loc), name(std::move(n)), parent(std::move(p)),
          members(std::move(m)), annotations(std::move(ann)) {}
};

// ------------------------------------------------------------
// Struct declaration — like class but value-type semantics.
// Same AST shape; the codegen flag distinguishes them.
// ------------------------------------------------------------
struct StructDecl : Statement {
    std::string              name;
    std::vector<ClassMember> members;
    std::vector<Annotation>  annotations;

    StructDecl(std::string n, std::vector<ClassMember> m,
               std::vector<Annotation> ann, SourceLoc loc)
        : Statement(loc), name(std::move(n)), members(std::move(m)),
          annotations(std::move(ann)) {}
};

// ------------------------------------------------------------
// Enum declaration — enum Name { A, B, C = 5, ... }
// ------------------------------------------------------------
struct EnumEntry {
    std::string name;
    ExprPtr     value;   // nullptr if not explicitly assigned
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

// ------------------------------------------------------------
// Include directive — #include <path> or #include <path> { names }
// Codegen: emit(Import, path, selectedNames).
// ------------------------------------------------------------
struct IncludeDecl : Statement {
    std::string              path;
    std::vector<std::string> selectedNames;  // empty = import all

    IncludeDecl(std::string p, std::vector<std::string> names, SourceLoc loc)
        : Statement(loc), path(std::move(p)), selectedNames(std::move(names)) {}
};

// ============================================================
// Program — the root node.  A translation unit is just a list
// of top-level statements (declarations + executable code).
// ============================================================
struct Program {
    StmtList statements;
    SourceLoc loc;

    Program(StmtList s, SourceLoc l)
        : statements(std::move(s)), loc(l) {}
};

} // namespace scriptlang

#endif // AST_H

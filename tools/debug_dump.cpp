// ------------------------------------------------------------
// debug_dump.cpp — reads a scriptlang source file and produces
// two diagnostic files in the same directory as the input:
//
//     <name>_tokens.txt   — one token per line
//     <name>_ast.txt      — indented tree dump of the full AST
//
// Usage:
//     ./scriptlang_debug  path/to/script.sl
//
// If the lexer or parser encounters errors they are printed to
// stderr and the corresponding output file still contains
// whatever was produced up to that point.
// ------------------------------------------------------------

#include "scriptlang.h"

#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <filesystem>
#include <iomanip>
#include <algorithm>

using namespace scriptlang;
namespace fs = std::filesystem;

// ============================================================
// Token dumper
// ============================================================

static void dumpTokens(const std::vector<Token>& tokens, std::ostream& out) {
    // Header
    out << "// scriptlang token dump\n"
        << "// " << tokens.size() << " token(s)\n"
        << "//\n"
        << "// FORMAT:  LINE:COL  TYPE  \"value\"\n"
        << "//          Eof and Error tokens are marked with a trailing flag.\n\n";

    // Figure out column widths for alignment
    int maxLine = 0, maxCol = 0;
    size_t maxTypeName = 0;
    for (auto& t : tokens) {
        maxLine     = std::max(maxLine,     t.line);
        maxCol      = std::max(maxCol,      t.column);
        maxTypeName = std::max(maxTypeName, tokenTypeName(t.type).size());
    }
    int lineWidth = std::to_string(maxLine).size();
    int colWidth  = std::to_string(maxCol).size();

    for (auto& t : tokens) {
        // LINE:COL  (right-aligned numbers)
        out << std::setw(lineWidth) << t.line
            << ":"
            << std::setw(colWidth)  << t.column
            << "  ";

        // TYPE  (left-aligned, padded)
        out << std::left << std::setw(maxTypeName + 2) << tokenTypeName(t.type);

        // Value — skip for tokens whose value is redundant or empty
        if (!t.value.empty()) {
            out << "\"";
            for (char c : t.value) {
                switch (c) {
                    case '\n': out << "\\n"; break;
                    case '\t': out << "\\t"; break;
                    case '\r': out << "\\r"; break;
                    case '\\': out << "\\\\"; break;
                    case '"':  out << "\\\""; break;
                    default:   out << c;
                }
            }
            out << "\"";
        }

        // Trailing flags
        if (t.isError()) out << "  <-- ERROR";
        if (t.isEof())   out << "  <-- EOF";

        out << "\n";
    }
}

// ============================================================
// AST dumper — recursive pretty printer
// ============================================================

// Indent helper: writes `depth` copies of two spaces.
static void indent(std::ostream& out, int depth) {
    for (int i = 0; i < depth; i++) out << "  ";
}

// Forward declarations for mutual recursion.
static void printExpr(std::ostream& out, const Expression* node, int depth);
static void printStmt(std::ostream& out, const Statement* node, int depth);

// ------------------------------------------------------------
// Shared helpers for sub-structures
// ------------------------------------------------------------

static void printTypeAnnot(std::ostream& out, const TypeAnnotation* ta, int depth) {
    if (!ta) { indent(out, depth); out << "(no type)\n"; return; }
    indent(out, depth);
    out << "TypeAnnotation: " << ta->name << "\n";
}

static void printParam(std::ostream& out, const Parameter& p, int depth) {
    indent(out, depth);
    out << "Param: " << p.name;
    if (p.isSpread) out << "  [spread]";
    out << "\n";
    if (p.typeAnnot) printTypeAnnot(out, p.typeAnnot.get(), depth + 1);
}

static void printAnnotation(std::ostream& out, const Annotation& a, int depth) {
    indent(out, depth);
    out << "Annotation: @" << a.name;
    if (!a.args.empty()) {
        out << "(";
        for (size_t i = 0; i < a.args.size(); i++) {
            if (i) out << ", ";
            out << a.args[i].first;
            if (!a.args[i].second.empty()) out << "=" << a.args[i].second;
        }
        out << ")";
    }
    out << "\n";
}

static void printAnnotations(std::ostream& out, const std::vector<Annotation>& anns, int depth) {
    for (auto& a : anns) printAnnotation(out, a, depth);
}

// ------------------------------------------------------------
// Expression printer — dispatches on dynamic type
// ------------------------------------------------------------

static void printExpr(std::ostream& out, const Expression* node, int depth) {
    if (!node) {
        indent(out, depth);
        out << "(null expr)\n";
        return;
    }

    // LiteralExpr
    if (auto* n = dynamic_cast<const LiteralExpr*>(node)) {
        indent(out, depth);
        out << "Literal: " << n->kind;
        if (!n->value.empty()) out << " = \"" << n->value << "\"";
        out << "\n";
        return;
    }

    // IdentifierExpr
    if (auto* n = dynamic_cast<const IdentifierExpr*>(node)) {
        indent(out, depth);
        out << "Identifier: " << n->name << "\n";
        return;
    }

    // BinaryExpr
    if (auto* n = dynamic_cast<const BinaryExpr*>(node)) {
        indent(out, depth);
        out << "BinaryExpr: " << n->op << "\n";
        indent(out, depth + 1); out << "left:\n";
        printExpr(out, n->left.get(), depth + 2);
        indent(out, depth + 1); out << "right:\n";
        printExpr(out, n->right.get(), depth + 2);
        return;
    }

    // UnaryExpr
    if (auto* n = dynamic_cast<const UnaryExpr*>(node)) {
        indent(out, depth);
        out << "UnaryExpr: " << n->op << "\n";
        indent(out, depth + 1); out << "operand:\n";
        printExpr(out, n->operand.get(), depth + 2);
        return;
    }

    // AssignExpr
    if (auto* n = dynamic_cast<const AssignExpr*>(node)) {
        indent(out, depth);
        out << "AssignExpr: " << n->op << "\n";
        indent(out, depth + 1); out << "target:\n";
        printExpr(out, n->target.get(), depth + 2);
        indent(out, depth + 1); out << "value:\n";
        printExpr(out, n->value.get(), depth + 2);
        return;
    }

    // CallExpr
    if (auto* n = dynamic_cast<const CallExpr*>(node)) {
        indent(out, depth);
        out << "CallExpr: (" << n->args.size() << " arg(s))\n";
        indent(out, depth + 1); out << "callee:\n";
        printExpr(out, n->callee.get(), depth + 2);
        for (size_t i = 0; i < n->args.size(); i++) {
            indent(out, depth + 1); out << "arg[" << i << "]:\n";
            printExpr(out, n->args[i].get(), depth + 2);
        }
        return;
    }

    // IndexExpr
    if (auto* n = dynamic_cast<const IndexExpr*>(node)) {
        indent(out, depth);
        out << "IndexExpr:\n";
        indent(out, depth + 1); out << "object:\n";
        printExpr(out, n->object.get(), depth + 2);
        indent(out, depth + 1); out << "index:\n";
        printExpr(out, n->index.get(), depth + 2);
        return;
    }

    // MemberExpr
    if (auto* n = dynamic_cast<const MemberExpr*>(node)) {
        indent(out, depth);
        out << "MemberExpr: ." << n->member << "\n";
        indent(out, depth + 1); out << "object:\n";
        printExpr(out, n->object.get(), depth + 2);
        return;
    }

    // OptionalMemberExpr
    if (auto* n = dynamic_cast<const OptionalMemberExpr*>(node)) {
        indent(out, depth);
        out << "OptionalMemberExpr: .?" << n->member << "\n";
        indent(out, depth + 1); out << "object:\n";
        printExpr(out, n->object.get(), depth + 2);
        return;
    }

    // NullCoalesceExpr
    if (auto* n = dynamic_cast<const NullCoalesceExpr*>(node)) {
        indent(out, depth);
        out << "NullCoalesceExpr: ??\n";
        indent(out, depth + 1); out << "left:\n";
        printExpr(out, n->left.get(), depth + 2);
        indent(out, depth + 1); out << "right:\n";
        printExpr(out, n->right.get(), depth + 2);
        return;
    }

    // ArrayLiteralExpr
    if (auto* n = dynamic_cast<const ArrayLiteralExpr*>(node)) {
        indent(out, depth);
        out << "ArrayLiteral: [" << n->elements.size() << " element(s)]\n";
        for (size_t i = 0; i < n->elements.size(); i++) {
            indent(out, depth + 1); out << "[" << i << "]:\n";
            printExpr(out, n->elements[i].get(), depth + 2);
        }
        return;
    }

    // LambdaExpr
    if (auto* n = dynamic_cast<const LambdaExpr*>(node)) {
        indent(out, depth);
        out << "LambdaExpr: (" << n->params.size() << " param(s))\n";
        for (auto& p : n->params) printParam(out, p, depth + 1);
        if (n->bodyExpr) {
            indent(out, depth + 1); out << "body (expr):\n";
            printExpr(out, n->bodyExpr.get(), depth + 2);
        }
        if (n->bodyBlock) {
            indent(out, depth + 1); out << "body (block):\n";
            printStmt(out, n->bodyBlock.get(), depth + 2);
        }
        return;
    }

    // NewExpr
    if (auto* n = dynamic_cast<const NewExpr*>(node)) {
        indent(out, depth);
        out << "NewExpr: " << n->className << " (" << n->args.size() << " arg(s))\n";
        for (size_t i = 0; i < n->args.size(); i++) {
            indent(out, depth + 1); out << "arg[" << i << "]:\n";
            printExpr(out, n->args[i].get(), depth + 2);
        }
        return;
    }

    // ThisExpr
    if (dynamic_cast<const ThisExpr*>(node)) {
        indent(out, depth);
        out << "ThisExpr\n";
        return;
    }

    // SuperExpr
    if (dynamic_cast<const SuperExpr*>(node)) {
        indent(out, depth);
        out << "SuperExpr\n";
        return;
    }

    // SpreadExpr
    if (auto* n = dynamic_cast<const SpreadExpr*>(node)) {
        indent(out, depth);
        out << "SpreadExpr:\n";
        indent(out, depth + 1); out << "operand:\n";
        printExpr(out, n->operand.get(), depth + 2);
        return;
    }

    // RangeExpr
    if (auto* n = dynamic_cast<const RangeExpr*>(node)) {
        indent(out, depth);
        out << "RangeExpr: " << (n->inclusive ? "inclusive (..)" : "exclusive (...)") << "\n";
        indent(out, depth + 1); out << "start:\n";
        printExpr(out, n->start.get(), depth + 2);
        indent(out, depth + 1); out << "end:\n";
        printExpr(out, n->end.get(), depth + 2);
        return;
    }

    // InterpolatedStringExpr
    if (auto* n = dynamic_cast<const InterpolatedStringExpr*>(node)) {
        indent(out, depth);
        out << "InterpolatedString: (" << n->parts.size() << " interpolation(s))\n";
        // Interleave segments and parts:  segments[0] parts[0] segments[1] parts[1] ... segments[N]
        for (size_t i = 0; i < n->segments.size(); i++) {
            if (!n->segments[i].empty()) {
                indent(out, depth + 1);
                out << "segment[" << i << "]: \"" << n->segments[i] << "\"\n";
            }
            if (i < n->parts.size()) {
                indent(out, depth + 1); out << "expr[" << i << "]:\n";
                printExpr(out, n->parts[i].get(), depth + 2);
            }
        }
        return;
    }

    // TernaryExpr
    if (auto* n = dynamic_cast<const TernaryExpr*>(node)) {
        indent(out, depth);
        out << "TernaryExpr:\n";
        indent(out, depth + 1); out << "condition:\n";
        printExpr(out, n->condition.get(), depth + 2);
        indent(out, depth + 1); out << "then:\n";
        printExpr(out, n->thenExpr.get(), depth + 2);
        indent(out, depth + 1); out << "else:\n";
        printExpr(out, n->elseExpr.get(), depth + 2);
        return;
    }

    // Fallback — should never happen if all nodes are covered above
    indent(out, depth);
    out << "<unknown Expression node>\n";
}

// ------------------------------------------------------------
// Statement printer — dispatches on dynamic type
// ------------------------------------------------------------

static void printStmt(std::ostream& out, const Statement* node, int depth) {
    if (!node) {
        indent(out, depth);
        out << "(null stmt)\n";
        return;
    }

    // ExprStatement
    if (auto* n = dynamic_cast<const ExprStatement*>(node)) {
        indent(out, depth);
        out << "ExprStatement:\n";
        printExpr(out, n->expr.get(), depth + 1);
        return;
    }

    // BlockStatement
    if (auto* n = dynamic_cast<const BlockStatement*>(node)) {
        indent(out, depth);
        out << "Block: (" << n->statements.size() << " stmt(s))\n";
        for (auto& s : n->statements) printStmt(out, s.get(), depth + 1);
        return;
    }

    // VarDecl
    if (auto* n = dynamic_cast<const VarDecl*>(node)) {
        indent(out, depth);
        out << "VarDecl: " << n->name << "\n";
        if (n->typeAnnot) printTypeAnnot(out, n->typeAnnot.get(), depth + 1);
        if (n->initializer) {
            indent(out, depth + 1); out << "init:\n";
            printExpr(out, n->initializer.get(), depth + 2);
        }
        return;
    }

    // ConstDecl
    if (auto* n = dynamic_cast<const ConstDecl*>(node)) {
        indent(out, depth);
        out << "ConstDecl: " << n->name << "\n";
        if (n->typeAnnot) printTypeAnnot(out, n->typeAnnot.get(), depth + 1);
        indent(out, depth + 1); out << "init:\n";
        printExpr(out, n->initializer.get(), depth + 2);
        return;
    }

    // ReturnStatement
    if (auto* n = dynamic_cast<const ReturnStatement*>(node)) {
        indent(out, depth);
        out << "Return:\n";
        if (n->value) {
            printExpr(out, n->value.get(), depth + 1);
        } else {
            indent(out, depth + 1); out << "(void)\n";
        }
        return;
    }

    // IfStatement
    if (auto* n = dynamic_cast<const IfStatement*>(node)) {
        indent(out, depth);
        out << "If:\n";
        indent(out, depth + 1); out << "condition:\n";
        printExpr(out, n->condition.get(), depth + 2);
        indent(out, depth + 1); out << "then:\n";
        printStmt(out, n->thenBranch.get(), depth + 2);
        if (n->elseBranch) {
            indent(out, depth + 1); out << "else:\n";
            printStmt(out, n->elseBranch.get(), depth + 2);
        }
        return;
    }

    // WhileStatement
    if (auto* n = dynamic_cast<const WhileStatement*>(node)) {
        indent(out, depth);
        out << "While:\n";
        indent(out, depth + 1); out << "condition:\n";
        printExpr(out, n->condition.get(), depth + 2);
        indent(out, depth + 1); out << "body:\n";
        printStmt(out, n->body.get(), depth + 2);
        return;
    }

    // ForStatement
    if (auto* n = dynamic_cast<const ForStatement*>(node)) {
        indent(out, depth);
        out << "For (C-style):\n";
        indent(out, depth + 1); out << "init:\n";
        if (n->init) printStmt(out, n->init.get(), depth + 2);
        else         { indent(out, depth + 2); out << "(empty)\n"; }
        indent(out, depth + 1); out << "condition:\n";
        if (n->condition) printExpr(out, n->condition.get(), depth + 2);
        else              { indent(out, depth + 2); out << "(empty)\n"; }
        indent(out, depth + 1); out << "update:\n";
        if (n->update) printExpr(out, n->update.get(), depth + 2);
        else           { indent(out, depth + 2); out << "(empty)\n"; }
        indent(out, depth + 1); out << "body:\n";
        printStmt(out, n->body.get(), depth + 2);
        return;
    }

    // ForInStatement
    if (auto* n = dynamic_cast<const ForInStatement*>(node)) {
        indent(out, depth);
        out << "ForIn: " << n->name << "\n";
        indent(out, depth + 1); out << "iterable:\n";
        printExpr(out, n->iterable.get(), depth + 2);
        indent(out, depth + 1); out << "body:\n";
        printStmt(out, n->body.get(), depth + 2);
        return;
    }

    // BreakStatement
    if (dynamic_cast<const BreakStatement*>(node)) {
        indent(out, depth);
        out << "Break\n";
        return;
    }

    // ContinueStatement
    if (dynamic_cast<const ContinueStatement*>(node)) {
        indent(out, depth);
        out << "Continue\n";
        return;
    }

    // SwitchStatement
    if (auto* n = dynamic_cast<const SwitchStatement*>(node)) {
        indent(out, depth);
        out << "Switch:\n";
        indent(out, depth + 1); out << "discriminant:\n";
        printExpr(out, n->discriminant.get(), depth + 2);
        for (size_t i = 0; i < n->cases.size(); i++) {
            auto& c = n->cases[i];
            indent(out, depth + 1);
            if (c.value) {
                out << "case[" << i << "]:\n";
                printExpr(out, c.value.get(), depth + 2);
            } else {
                out << "default:\n";
            }
            for (auto& s : c.body) printStmt(out, s.get(), depth + 2);
        }
        return;
    }

    // FunctionDecl
    if (auto* n = dynamic_cast<const FunctionDecl*>(node)) {
        indent(out, depth);
        out << "FunctionDecl: " << n->name;
        if (n->isStatic) out << "  [static]";
        if (!n->isPublic) out << "  [private]";
        out << "\n";
        printAnnotations(out, n->annotations, depth + 1);
        if (n->returnType) {
            indent(out, depth + 1); out << "returnType:\n";
            printTypeAnnot(out, n->returnType.get(), depth + 2);
        }
        for (auto& p : n->params) printParam(out, p, depth + 1);
        indent(out, depth + 1); out << "body:\n";
        printStmt(out, n->body.get(), depth + 2);
        return;
    }

    // ClassDecl
    if (auto* n = dynamic_cast<const ClassDecl*>(node)) {
        indent(out, depth);
        out << "ClassDecl: " << n->name;
        if (!n->parent.empty()) out << " : " << n->parent;
        out << "\n";
        printAnnotations(out, n->annotations, depth + 1);
        for (size_t i = 0; i < n->members.size(); i++) {
            auto& m = n->members[i];
            indent(out, depth + 1);
            out << "member[" << i << "]:";
            if (m.isStatic) out << " [static]";
            if (!m.isPublic) out << " [private]";
            out << "\n";
            printAnnotations(out, m.annotations, depth + 2);
            printStmt(out, m.decl.get(), depth + 2);
        }
        return;
    }

    // StructDecl
    if (auto* n = dynamic_cast<const StructDecl*>(node)) {
        indent(out, depth);
        out << "StructDecl: " << n->name << "\n";
        printAnnotations(out, n->annotations, depth + 1);
        for (size_t i = 0; i < n->members.size(); i++) {
            auto& m = n->members[i];
            indent(out, depth + 1);
            out << "member[" << i << "]:";
            if (m.isStatic) out << " [static]";
            if (!m.isPublic) out << " [private]";
            out << "\n";
            printAnnotations(out, m.annotations, depth + 2);
            printStmt(out, m.decl.get(), depth + 2);
        }
        return;
    }

    // EnumDecl
    if (auto* n = dynamic_cast<const EnumDecl*>(node)) {
        indent(out, depth);
        out << "EnumDecl: " << n->name << "\n";
        printAnnotations(out, n->annotations, depth + 1);
        for (size_t i = 0; i < n->entries.size(); i++) {
            auto& e = n->entries[i];
            indent(out, depth + 1);
            out << "entry: " << e.name;
            if (e.value) {
                out << "\n";
                indent(out, depth + 2); out << "value:\n";
                printExpr(out, e.value.get(), depth + 3);
            } else {
                out << "\n";
            }
        }
        return;
    }

    // IncludeDecl
    if (auto* n = dynamic_cast<const IncludeDecl*>(node)) {
        indent(out, depth);
        out << "IncludeDecl: <" << n->path << ">";
        if (!n->selectedNames.empty()) {
            out << " {";
            for (size_t i = 0; i < n->selectedNames.size(); i++) {
                if (i) out << ",";
                out << " " << n->selectedNames[i];
            }
            out << " }";
        }
        out << "\n";
        return;
    }

    // Fallback
    indent(out, depth);
    out << "<unknown Statement node>\n";
}

// ------------------------------------------------------------
// Program printer
// ------------------------------------------------------------

static void dumpAST(const Program& program, std::ostream& out) {
    out << "// scriptlang AST dump\n"
        << "// " << program.statements.size() << " top-level statement(s)\n\n";

    out << "Program:\n";
    for (auto& stmt : program.statements) {
        printStmt(out, stmt.get(), 1);
    }
}

// ============================================================
// Main
// ============================================================

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <script.sl>\n"
                  << "Produces output/<script>_tokens.txt and output/<script>_ast.txt\n";
        return 1;
    }

    // --- Read source file ---
    fs::path inputPath(argv[1]);
    if (!fs::exists(inputPath)) {
        std::cerr << "Error: file not found: " << inputPath << "\n";
        return 1;
    }

    std::ifstream srcFile(inputPath);
    if (!srcFile.is_open()) {
        std::cerr << "Error: cannot open: " << inputPath << "\n";
        return 1;
    }
    std::string source((std::istreambuf_iterator<char>(srcFile)),
                        std::istreambuf_iterator<char>());
    srcFile.close();

    // --- Derive output paths: same directory, stem + suffix ---
    fs::path stem     = inputPath.stem();
    fs::path tokPath  = fs::path("output/" + stem.string() + "_tokens.txt");
    fs::path astPath  = fs::path("output/" + stem.string() + "_ast.txt");

    // --- Lex ---
    std::cerr << "Lexing " << inputPath << " ...\n";
    Lexer lexer(source);
    auto tokens = lexer.tokenize();

    // Write tokens
    {
        std::ofstream tokFile(tokPath);
        if (!tokFile.is_open()) {
            std::cerr << "Error: cannot write: " << tokPath << "\n";
            return 1;
        }
        dumpTokens(tokens, tokFile);
        std::cerr << "  Wrote " << tokPath << " (" << tokens.size() << " tokens)\n";
    }

    // Check for lex errors
    bool lexError = false;
    for (auto& t : tokens) {
        if (t.isError()) {
            std::cerr << "  Lexer error at " << t.line << ":" << t.column
                      << " — " << t.value << "\n";
            lexError = true;
        }
    }

    // --- Parse ---
    std::cerr << "Parsing ...\n";
    Parser parser(tokens);
    auto program = parser.parse();

    // Report parse errors to stderr
    if (parser.hasErrors()) {
        for (auto& e : parser.errors()) {
            std::cerr << "  Parse error at " << e.line << ":" << e.column
                      << " — " << e.message << "\n";
        }
    }

    // Write AST (even if there were errors — partial tree is still useful)
    {
        std::ofstream astFile(astPath);
        if (!astFile.is_open()) {
            std::cerr << "Error: cannot write: " << astPath << "\n";
            return 1;
        }
        dumpAST(*program, astFile);
        std::cerr << "  Wrote " << astPath << " (" << program->statements.size()
                  << " top-level statements)\n";
    }

    // Exit code: 0 if clean, 1 if any errors were encountered
    return (lexError || parser.hasErrors()) ? 1 : 0;
}

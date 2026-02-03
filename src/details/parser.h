#ifndef PARSER_H
#define PARSER_H

// ------------------------------------------------------------
// parser.h — Recursive-descent parser (IMPROVED VERSION).
//
// IMPROVEMENTS:
// - Fixed annotation parsing to properly handle nested arrays and complex
// values
// - Better error recovery and synchronization
// - Support for all syntax features in syntax.mp and docs.mp
// - Improved handling of visibility modifiers and annotations
// - Better lookahead for disambiguating constructs
// ------------------------------------------------------------

#include "ast.h"
#include "lexer.h"
#include "token.h"

#include <optional>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace scriptlang {

// ============================================================
// Parse error descriptor
// ============================================================
struct ParseError {
  std::string message;
  int line;
  int column;
};

// ============================================================
// Parser
// ============================================================
class Parser {
public:
  explicit Parser(const std::vector<Token> &tokens)
      : tokens_(tokens), pos_(0) {}

  // Main entry point.  Returns the root Program node.
  std::unique_ptr<Program> parse() {
    SourceLoc start = currentLoc();
    StmtList stmts;

    while (!isAtEnd()) {
      auto stmt = parseStatement();
      if (stmt) {
        stmts.push_back(std::move(stmt));
      }
      // If parseStatement returned nullptr we already
      // synchronized, so just keep going.
    }

    return std::make_unique<Program>(std::move(stmts), start);
  }

  bool hasErrors() const { return !errors_.empty(); }
  const std::vector<ParseError> &errors() const { return errors_; }

  // ============================================================
  // PRIVATE
  // ============================================================
private:
  const std::vector<Token> &tokens_;
  size_t pos_;
  std::vector<ParseError> errors_;

  // --------------------------------------------------------
  // Token access helpers
  // --------------------------------------------------------
  const Token &current() const { return tokens_[pos_]; }

  const Token &peek(int offset = 0) const {
    size_t idx = pos_ + offset;
    if (idx >= tokens_.size())
      return tokens_.back(); // Eof
    return tokens_[idx];
  }

  bool isAtEnd() const { return current().type == TokenType::Eof; }

  SourceLoc currentLoc() const {
    return SourceLoc(current().line, current().column);
  }

  // Advance and return the token we just consumed.
  const Token &advance() {
    const Token &t = current();
    if (!isAtEnd())
      pos_++;
    return t;
  }

  // Check without consuming.
  bool check(TokenType type) const { return current().type == type; }

  // Consume if it matches; return true if consumed.
  bool match(TokenType type) {
    if (check(type)) {
      advance();
      return true;
    }
    return false;
  }

  // Consume and return the token, or record an error.
  const Token &expect(TokenType type, const char *message) {
    if (check(type))
      return advance();
    error(message);
    // Return current so callers don't crash; they should
    // check hasErrors() before using the result.
    return current();
  }

  // --------------------------------------------------------
  // Error reporting & synchronization
  // --------------------------------------------------------
  void error(const char *message) {
    errors_.push_back({message, current().line, current().column});
  }

  void errorAtToken(const Token &tok, const char *message) {
    errors_.push_back({message, tok.line, tok.column});
  }

  // Skip tokens until we hit something that looks like the
  // start of a new statement.  Called after an error to let
  // parsing continue and find more errors.
  void synchronize() {
    while (!isAtEnd()) {
      if (current().type == TokenType::Semicolon) {
        advance(); // eat the semicolon
        return;
      }
      // Statement-starting keywords
      switch (current().type) {
      case TokenType::Var:
      case TokenType::Const:
      case TokenType::Class:
      case TokenType::Struct:
      case TokenType::Enum:
      case TokenType::Return:
      case TokenType::If:
      case TokenType::While:
      case TokenType::For:
      case TokenType::Switch:
      case TokenType::RightBrace:
      case TokenType::Hash:
      case TokenType::At:
        return;
      default:
        advance();
      }
    }
  }

  // --------------------------------------------------------
  // IMPROVED Annotation parsing
  // Supports:
  // - @name
  // - @name(key=value, key2=value2)
  // - @name(key=[array, of, values])
  // - Proper error recovery
  // --------------------------------------------------------

  // Parse a single annotation value (can be literal, identifier, or array)
  std::string parseAnnotationValue() {
    if (check(TokenType::LeftBracket)) {
      // Array value: [item1, item2, ...]
      advance(); // consume [
      std::string result = "[";
      bool first = true;

      while (!check(TokenType::RightBracket) && !isAtEnd()) {
        if (!first)
          result += ",";
        first = false;

        // Each item can be a simple value
        Token t = current();
        if (t.type == TokenType::String || t.type == TokenType::Integer ||
            t.type == TokenType::Float || t.type == TokenType::Identifier ||
            t.type == TokenType::True || t.type == TokenType::False) {
          result += t.value;
          advance();
        } else {
          errorAtToken(t, "Expected literal value in annotation array");
          break;
        }

        if (!check(TokenType::RightBracket)) {
          if (!match(TokenType::Comma)) {
            error("Expected ',' or ']' in annotation array");
            break;
          }
        }
      }

      expect(TokenType::RightBracket, "Expected ']' after annotation array");
      result += "]";
      return result;
    } else {
      // Simple value
      Token t = current();
      if (t.type == TokenType::String || t.type == TokenType::Integer ||
          t.type == TokenType::Float || t.type == TokenType::HexInteger ||
          t.type == TokenType::BinaryInteger ||
          t.type == TokenType::Scientific || t.type == TokenType::Identifier ||
          t.type == TokenType::True || t.type == TokenType::False) {
        advance();
        return t.value;
      } else {
        errorAtToken(t, "Expected literal value in annotation");
        return "";
      }
    }
  }

  std::vector<Annotation> parseAnnotations() {
    std::vector<Annotation> annotations;

    while (check(TokenType::At)) {
      SourceLoc loc = currentLoc();
      advance(); // consume @

      std::string name =
          expect(TokenType::Identifier, "Expected annotation name after '@'")
              .value;

      std::vector<std::pair<std::string, std::string>> args;
      if (match(TokenType::LeftParen)) {
        if (!check(TokenType::RightParen)) {
          do {
            std::string key = expect(TokenType::Identifier,
                                     "Expected annotation argument name")
                                  .value;

            // REQUIRE '='
            if (!match(TokenType::Assign)) {
              error("Expected '=' after annotation argument name");
              // Try to recover by skipping to next comma or closing paren
              while (!check(TokenType::Comma) &&
                     !check(TokenType::RightParen) && !isAtEnd()) {
                advance();
              }
              if (check(TokenType::Comma))
                continue;
              else
                break;
            }

            // Parse value (can be simple or array)
            std::string val = parseAnnotationValue();
            args.push_back({std::move(key), std::move(val)});

          } while (match(TokenType::Comma));
        }
        expect(TokenType::RightParen,
               "Expected ')' after annotation arguments");
      }

      Annotation ann;
      ann.name = std::move(name);
      ann.args = std::move(args);
      ann.loc = loc;
      annotations.push_back(std::move(ann));
    }

    return annotations;
  }

  // --------------------------------------------------------
  // Type annotation parsing — just a name for now.
  // Future: generics, array types (T[]), etc.
  // --------------------------------------------------------
  TypeAnnotPtr parseTypeAnnotation() {
    SourceLoc loc = currentLoc();

    // Type keywords and identifiers are both valid type names
    std::string name;
    switch (current().type) {
    case TokenType::Int8:
    case TokenType::Int16:
    case TokenType::Int32:
    case TokenType::Int64:
    case TokenType::UInt8:
    case TokenType::UInt16:
    case TokenType::UInt32:
    case TokenType::UInt64:
    case TokenType::Float32:
    case TokenType::Float64:
    case TokenType::Bool:
    case TokenType::String_:
    case TokenType::Void:
    case TokenType::Auto:
    case TokenType::Identifier:
      name = current().value;
      advance();
      break;
    default:
      error("Expected type name");
      return nullptr;
    }

    return std::make_unique<TypeAnnotation>(std::move(name), loc);
  }

  // --------------------------------------------------------
  // Parameter list parsing — (name [: type], ...)
  // Also handles ...name for spread/rest parameters.
  // --------------------------------------------------------
  std::vector<Parameter> parseParameterList() {
    std::vector<Parameter> params;

    expect(TokenType::LeftParen, "Expected '(' before parameter list");

    if (!check(TokenType::RightParen)) {
      do {
        Parameter param;
        param.loc = currentLoc();
        param.isSpread = false;

        // Check for spread parameter
        if (match(TokenType::DotDotDot)) {
          param.isSpread = true;
        }

        param.name =
            expect(TokenType::Identifier, "Expected parameter name").value;

        // Optional type annotation
        if (match(TokenType::Colon)) {
          param.typeAnnot = parseTypeAnnotation();
        }

        params.push_back(std::move(param));
      } while (match(TokenType::Comma));
    }

    expect(TokenType::RightParen, "Expected ')' after parameter list");
    return params;
  }

  // --------------------------------------------------------
  // Include directive — #include <path> [{ names }]
  // We've already consumed `#` and `include`.
  // --------------------------------------------------------
  StmtPtr parseInclude(SourceLoc loc) {
    // Path is inside < >
    expect(TokenType::Less, "Expected '<' after 'include'");

    // Collect everything up to > as the path string
    std::string path;
    while (!check(TokenType::Greater) && !isAtEnd()) {
      path += current().value;
      advance();
    }
    expect(TokenType::Greater, "Expected '>' after include path");

    // Optional selective import { Name1, Name2 }
    std::vector<std::string> selectedNames;
    if (match(TokenType::LeftBrace)) {
      if (!check(TokenType::RightBrace)) {
        do {
          selectedNames.push_back(
              expect(TokenType::Identifier, "Expected name in include list")
                  .value);
        } while (match(TokenType::Comma));
      }
      expect(TokenType::RightBrace, "Expected '}' after include name list");
    }

    return std::make_unique<IncludeDecl>(std::move(path),
                                         std::move(selectedNames), loc);
  }

  // ============================================================
  // STATEMENT PARSING
  // ============================================================

  StmtPtr parseStatement() {
    // Annotations can prefix declarations
    auto annotations = parseAnnotations();

    switch (current().type) {
    case TokenType::Hash:
      return parseDirective();
    case TokenType::LeftBrace:
      if (!annotations.empty()) {
        error("Annotations cannot precede block statements");
        annotations.clear();
      }
      return parseBlock();
    case TokenType::Var:
      if (!annotations.empty()) {
        error("Annotations cannot precede var declarations (use @expose on "
              "class members)");
        annotations.clear();
      }
      return parseVarDecl();
    case TokenType::Const:
      if (!annotations.empty()) {
        error("Annotations cannot precede const declarations");
        annotations.clear();
      }
      return parseConstDecl();
    case TokenType::Return:
      if (!annotations.empty()) {
        error("Annotations cannot precede return statements");
        annotations.clear();
      }
      return parseReturn();
    case TokenType::If:
      if (!annotations.empty()) {
        error("Annotations cannot precede if statements");
        annotations.clear();
      }
      return parseIf();
    case TokenType::While:
      if (!annotations.empty()) {
        error("Annotations cannot precede while statements");
        annotations.clear();
      }
      return parseWhile();
    case TokenType::For:
      if (!annotations.empty()) {
        error("Annotations cannot precede for statements");
        annotations.clear();
      }
      return parseFor();
    case TokenType::Switch:
      if (!annotations.empty()) {
        error("Annotations cannot precede switch statements");
        annotations.clear();
      }
      return parseSwitch();
    case TokenType::Break:
      if (!annotations.empty()) {
        error("Annotations cannot precede break statements");
        annotations.clear();
      }
      return parseBreak();
    case TokenType::Continue:
      if (!annotations.empty()) {
        error("Annotations cannot precede continue statements");
        annotations.clear();
      }
      return parseContinue();
    case TokenType::Class:
    case TokenType::Public:
    case TokenType::Private:
      return parseClassOrFunction(std::move(annotations));
    case TokenType::Struct:
      return parseStructDecl(std::move(annotations));
    case TokenType::Enum:
      return parseEnumDecl(std::move(annotations));
    case TokenType::Static: {
      // `static` can prefix a function inside a class — but at
      // top-level it starts a declaration too.
      return parseClassOrFunction(std::move(annotations));
    }
    case TokenType::Void:
    case TokenType::Auto:
    case TokenType::Int8:
    case TokenType::Int16:
    case TokenType::Int32:
    case TokenType::Int64:
    case TokenType::UInt8:
    case TokenType::UInt16:
    case TokenType::UInt32:
    case TokenType::UInt64:
    case TokenType::Float32:
    case TokenType::Float64:
    case TokenType::Bool:
    case TokenType::String_: {
      // Could be a typed function declaration: `int32 foo(...) { }`
      // or a typed variable: `int32 x = 5;`
      // Peek ahead: if name is followed by `(` it's a function.
      if (peek(1).type == TokenType::Identifier &&
          peek(2).type == TokenType::LeftParen) {
        return parseClassOrFunction(std::move(annotations));
      }
      // Otherwise fall through to expression statement
      // (which will fail gracefully if it's truly invalid)
      [[fallthrough]];
    }
    default:
      // A bare identifier followed by (...) { is a function
      // declaration with no type prefix or visibility modifier,
      // e.g.  foo() { ... }   or   add(a, b) { return a+b; }
      if (check(TokenType::Identifier) && isBareFunction()) {
        return parseClassOrFunction(std::move(annotations));
      }
      if (!annotations.empty()) {
        error("Annotations can only precede declarations");
        synchronize();
        return nullptr;
      }
      return parseExprStatement();
    }
  }

  // --------------------------------------------------------
  // Directive — # include ...
  // --------------------------------------------------------
  StmtPtr parseDirective() {
    SourceLoc loc = currentLoc();
    advance(); // consume #

    if (match(TokenType::Include)) {
      return parseInclude(loc);
    }

    error("Unknown directive after '#'");
    synchronize();
    return nullptr;
  }

  // --------------------------------------------------------
  // Block — { statements }
  // --------------------------------------------------------
  StmtPtr parseBlock() {
    SourceLoc loc = currentLoc();
    expect(TokenType::LeftBrace, "Expected '{'");

    StmtList stmts;
    while (!check(TokenType::RightBrace) && !isAtEnd()) {
      auto stmt = parseStatement();
      if (stmt)
        stmts.push_back(std::move(stmt));
      else
        break; // error already recorded
    }

    expect(TokenType::RightBrace, "Expected '}'");
    return std::make_unique<BlockStatement>(std::move(stmts), loc);
  }

  // --------------------------------------------------------
  // var declaration — var name [: type] [= expr] ;
  // --------------------------------------------------------
  StmtPtr parseVarDecl() {
    SourceLoc loc = currentLoc();
    advance(); // consume `var`

    std::string name =
        expect(TokenType::Identifier, "Expected variable name after 'var'")
            .value;

    TypeAnnotPtr typeAnnot;
    if (match(TokenType::Colon)) {
      typeAnnot = parseTypeAnnotation();
    }

    ExprPtr init;
    if (match(TokenType::Assign)) {
      init = parseExpression();
    }

    expect(TokenType::Semicolon, "Expected ';' after variable declaration");
    return std::make_unique<VarDecl>(std::move(name), std::move(typeAnnot),
                                     std::move(init), loc);
  }

  // --------------------------------------------------------
  // const declaration — const name [: type] = expr ;
  // --------------------------------------------------------
  StmtPtr parseConstDecl() {
    SourceLoc loc = currentLoc();
    advance(); // consume `const`

    std::string name =
        expect(TokenType::Identifier, "Expected constant name after 'const'")
            .value;

    TypeAnnotPtr typeAnnot;
    if (match(TokenType::Colon)) {
      typeAnnot = parseTypeAnnotation();
    }

    expect(TokenType::Assign, "Expected '=' in const declaration");
    ExprPtr init = parseExpression();

    expect(TokenType::Semicolon, "Expected ';' after const declaration");
    return std::make_unique<ConstDecl>(std::move(name), std::move(typeAnnot),
                                       std::move(init), loc);
  }

  // --------------------------------------------------------
  // return [expr] ;
  // --------------------------------------------------------
  StmtPtr parseReturn() {
    SourceLoc loc = currentLoc();
    advance(); // consume `return`

    ExprPtr value;
    if (!check(TokenType::Semicolon)) {
      value = parseExpression();
    }

    expect(TokenType::Semicolon, "Expected ';' after return statement");
    return std::make_unique<ReturnStatement>(std::move(value), loc);
  }

  // --------------------------------------------------------
  // if (cond) block [else ...]
  // --------------------------------------------------------
StmtPtr parseIf() {
  SourceLoc loc = currentLoc();
  advance(); // consume `if`

  expect(TokenType::LeftParen, "Expected '(' after 'if'");
  ExprPtr cond = parseExpression();
  expect(TokenType::RightParen, "Expected ')' after if condition");

  // Allow either block or single statement
  StmtPtr thenBranch;
  if (check(TokenType::LeftBrace)) {
    thenBranch = parseBlock();
  } else {
    thenBranch = parseStatement();
  }

  StmtPtr elseBranch;
  if (match(TokenType::Else)) {
    if (check(TokenType::If)) {
      elseBranch = parseIf(); // else-if chain
    } else if (check(TokenType::LeftBrace)) {
      elseBranch = parseBlock();
    } else {
      elseBranch = parseStatement();
    }
  }

  return std::make_unique<IfStatement>(std::move(cond), std::move(thenBranch),
                                       std::move(elseBranch), loc);
}

  // --------------------------------------------------------
  // while (cond) block
  // --------------------------------------------------------
 StmtPtr parseWhile() {
  SourceLoc loc = currentLoc();
  advance(); // consume `while`

  expect(TokenType::LeftParen, "Expected '(' after 'while'");
  ExprPtr cond = parseExpression();
  expect(TokenType::RightParen, "Expected ')' after while condition");

  StmtPtr body;
  if (check(TokenType::LeftBrace)) {
    body = parseBlock();
  } else {
    body = parseStatement();
  }

  return std::make_unique<WhileStatement>(std::move(cond), std::move(body), loc);
}

  // --------------------------------------------------------
  // for — either C-style or for-in.
  //   for (init; cond; update) block
  //   for name in expr block
  // We disambiguate by looking at what follows `for`:
  //   `for (` → C-style
  //   `for name in` → for-in
  // --------------------------------------------------------
  StmtPtr parseFor() {
    SourceLoc loc = currentLoc();
    advance(); // consume `for`

    // Distinguish: for-in vs C-style
    if (check(TokenType::Identifier) && peek(1).type == TokenType::In) {
      return parseForIn(loc);
    }

    return parseCStyleFor(loc);
  }

  StmtPtr parseForIn(SourceLoc loc) {
    std::string name = advance().value; // consume identifier
    advance();                          // consume `in`

    ExprPtr iterable = parseExpression();
    StmtPtr body = parseBlock();

    return std::make_unique<ForInStatement>(
        std::move(name), std::move(iterable), std::move(body), loc);
  }

  StmtPtr parseCStyleFor(SourceLoc loc) {
    expect(TokenType::LeftParen, "Expected '(' after 'for'");

    // init: var decl, expression statement, or empty
    StmtPtr init;
    if (match(TokenType::Semicolon)) {
      // empty init — semicolon already consumed
    } else if (check(TokenType::Var)) {
      init = parseVarDecl(); // includes its own semicolon
    } else {
      ExprPtr e = parseExpression();
      expect(TokenType::Semicolon, "Expected ';' after for init");
      init = std::make_unique<ExprStatement>(std::move(e), loc);
    }

    // condition
    ExprPtr cond;
    if (!check(TokenType::Semicolon)) {
      cond = parseExpression();
    }
    expect(TokenType::Semicolon, "Expected ';' after for condition");

    // update
    ExprPtr update;
    if (!check(TokenType::RightParen)) {
      update = parseExpression();
    }
    expect(TokenType::RightParen, "Expected ')' after for clauses");

    StmtPtr body = parseBlock();

    return std::make_unique<ForStatement>(std::move(init), std::move(cond),
                                          std::move(update), std::move(body),
                                          loc);
  }

  // --------------------------------------------------------
  // switch (expr) { case val: stmts  default: stmts }
  // --------------------------------------------------------
  StmtPtr parseSwitch() {
    SourceLoc loc = currentLoc();
    advance(); // consume `switch`

    expect(TokenType::LeftParen, "Expected '(' after 'switch'");
    ExprPtr disc = parseExpression();
    expect(TokenType::RightParen, "Expected ')' after switch expression");
    expect(TokenType::LeftBrace, "Expected '{' to begin switch body");

    std::vector<SwitchCase> cases;

    while (!check(TokenType::RightBrace) && !isAtEnd()) {
      SwitchCase sc;
      sc.loc = currentLoc();

      if (match(TokenType::Case)) {
        sc.value = parseExpression();
        expect(TokenType::Colon, "Expected ':' after case value");
      } else if (match(TokenType::Default)) {
        expect(TokenType::Colon, "Expected ':' after 'default'");
        sc.value = nullptr; // marks the default case
      } else {
        error("Expected 'case' or 'default' in switch body");
        synchronize();
        break;
      }

      // Parse statements until next case/default/}
      while (!check(TokenType::Case) && !check(TokenType::Default) &&
             !check(TokenType::RightBrace) && !isAtEnd()) {
        auto stmt = parseStatement();
        if (stmt)
          sc.body.push_back(std::move(stmt));
        else
          break;
      }

      cases.push_back(std::move(sc));
    }

    expect(TokenType::RightBrace, "Expected '}' after switch body");
    return std::make_unique<SwitchStatement>(std::move(disc), std::move(cases),
                                             loc);
  }

  // --------------------------------------------------------
  // break ;
  // --------------------------------------------------------
  StmtPtr parseBreak() {
    SourceLoc loc = currentLoc();
    advance(); // consume `break`
    expect(TokenType::Semicolon, "Expected ';' after 'break'");
    return std::make_unique<BreakStatement>(loc);
  }

  // --------------------------------------------------------
  // continue ;
  // --------------------------------------------------------
  StmtPtr parseContinue() {
    SourceLoc loc = currentLoc();
    advance(); // consume `continue`
    expect(TokenType::Semicolon, "Expected ';' after 'continue'");
    return std::make_unique<ContinueStatement>(loc);
  }

  // --------------------------------------------------------
  // Function or class declaration dispatch.
  // Handles: [public|private] [static] [returnType] name(...)
  // Peeks ahead to figure out what we're looking at.
  // --------------------------------------------------------
  StmtPtr parseClassOrFunction(std::vector<Annotation> annotations) {
    SourceLoc loc = currentLoc();
    bool isPublic = true; // default
    bool isStatic = false;

    // Consume visibility modifier if present
    if (match(TokenType::Public)) {
      isPublic = true;
    } else if (match(TokenType::Private)) {
      isPublic = false;
    }

    // Consume static if present
    if (match(TokenType::Static)) {
      isStatic = true;
    }

    // After modifiers, what's next?
    if (check(TokenType::Class)) {
      if (isStatic) {
        error("Classes cannot be declared static");
      }
      return parseClassDecl(std::move(annotations), loc);
    }

    // It's a function declaration (possibly with a return type prefix)
    return parseFunctionDecl(std::move(annotations), isPublic, isStatic, loc);
  }

  // --------------------------------------------------------
  // Function declaration
  //   [returnType] name ( params ) [-> returnType] { body }
  // --------------------------------------------------------
  StmtPtr parseFunctionDecl(std::vector<Annotation> annotations, bool isPublic,
                            bool isStatic, SourceLoc loc) {
    TypeAnnotPtr returnType;

    // If current token is a type keyword and next is an identifier
    // followed by '(', then we have: returnType name(...)
    // Otherwise we just have: name(...)  (void/auto inferred)
    if (isTypeToken(current().type) && peek(1).type == TokenType::Identifier) {
      returnType = parseTypeAnnotation();
    }

    std::string name =
        expect(TokenType::Identifier, "Expected function name").value;

    auto params = parseParameterList();

    // Optional -> returnType (alternative syntax)
    if (match(TokenType::Arrow)) {
      returnType = parseTypeAnnotation();
    }

    StmtPtr body = parseBlock();

    return std::make_unique<FunctionDecl>(
        std::move(name), std::move(params), std::move(returnType),
        std::move(body), isStatic, isPublic, std::move(annotations), loc);
  }

  // --------------------------------------------------------
  // Class declaration
  //   class Name [: Parent] { members }
  // --------------------------------------------------------
  StmtPtr parseClassDecl(std::vector<Annotation> annotations, SourceLoc loc) {
    advance(); // consume `class`

    std::string name =
        expect(TokenType::Identifier, "Expected class name").value;

    std::string parent;
    if (match(TokenType::Colon)) {
      parent =
          expect(TokenType::Identifier, "Expected parent class name").value;
    }

    expect(TokenType::LeftBrace, "Expected '{' after class declaration");

    std::vector<ClassMember> members;
    while (!check(TokenType::RightBrace) && !isAtEnd()) {
      ClassMember member;
      member.annotations = parseAnnotations();
      member.isPublic = true; // default in classes
      member.isStatic = false;

      // Consume visibility
      if (match(TokenType::Public)) {
        member.isPublic = true;
      } else if (match(TokenType::Private)) {
        member.isPublic = false;
      }

      // Consume static
      if (match(TokenType::Static)) {
        member.isStatic = true;
      }

      // Now determine if this is a field (var/const) or a method
      // Check for var or const keywords explicitly
      if (check(TokenType::Var)) {
        member.decl = parseVarDecl();
      } else if (check(TokenType::Const)) {
        member.decl = parseConstDecl();
      } else {
        // Could be a method or a typed field
        // Look ahead to distinguish:
        //   int32 fieldName = value;     <- field
        //   int32 methodName(...) { }    <- method

        // We need to check if this looks like a field declaration
        // Pattern: [type] identifier = expr ;
        //      or: [type] identifier ;

        bool isField = false;

        // Check if current token is a type keyword
        if (isTypeToken(current().type)) {
          // Look at what comes after: identifier (=|;) means field
          if (peek(1).type == TokenType::Identifier) {
            TokenType afterIdent = peek(2).type;
            if (afterIdent == TokenType::Assign ||
                afterIdent == TokenType::Semicolon) {
              isField = true;
            }
          }
        }

        if (isField) {
          // Parse as a typed field declaration
          SourceLoc fieldLoc = currentLoc();
          TypeAnnotPtr typeAnnot = parseTypeAnnotation();
          std::string fieldName =
              expect(TokenType::Identifier, "Expected field name").value;

          ExprPtr init;
          if (match(TokenType::Assign)) {
            init = parseExpression();
          }

          expect(TokenType::Semicolon, "Expected ';' after field declaration");

          member.decl = std::make_unique<VarDecl>(std::move(fieldName),
                                                  std::move(typeAnnot),
                                                  std::move(init), fieldLoc);
        } else {
          // Parse as a method
          member.decl =
              parseFunctionDecl(std::move(member.annotations), member.isPublic,
                                member.isStatic, currentLoc());
          member.annotations.clear(); // moved into FunctionDecl
        }
      }

      members.push_back(std::move(member));
    }

    expect(TokenType::RightBrace, "Expected '}' after class body");
    return std::make_unique<ClassDecl>(std::move(name), std::move(parent),
                                       std::move(members),
                                       std::move(annotations), loc);
  }

  // --------------------------------------------------------
  // Struct declaration — same as class, different AST node
  // --------------------------------------------------------
  StmtPtr parseStructDecl(std::vector<Annotation> annotations) {
    SourceLoc loc = currentLoc();
    advance(); // consume `struct`

    std::string name =
        expect(TokenType::Identifier, "Expected struct name").value;

    expect(TokenType::LeftBrace, "Expected '{' after struct declaration");

    std::vector<ClassMember> members;
    while (!check(TokenType::RightBrace) && !isAtEnd()) {
      ClassMember member;
      member.annotations = parseAnnotations();
      member.isPublic = true;
      member.isStatic = false;

      if (match(TokenType::Public)) {
        member.isPublic = true;
      } else if (match(TokenType::Private)) {
        member.isPublic = false;
      }
      if (match(TokenType::Static)) {
        member.isStatic = true;
      }

      if (check(TokenType::Var)) {
        member.decl = parseVarDecl();
      } else if (check(TokenType::Const)) {
        member.decl = parseConstDecl();
      } else {
        member.decl =
            parseFunctionDecl(std::move(member.annotations), member.isPublic,
                              member.isStatic, currentLoc());
        member.annotations.clear();
      }

      members.push_back(std::move(member));
    }

    expect(TokenType::RightBrace, "Expected '}' after struct body");
    return std::make_unique<StructDecl>(std::move(name), std::move(members),
                                        std::move(annotations), loc);
  }

  // --------------------------------------------------------
  // Enum declaration — enum Name { A, B = expr, ... }
  // --------------------------------------------------------
  StmtPtr parseEnumDecl(std::vector<Annotation> annotations) {
    SourceLoc loc = currentLoc();
    advance(); // consume `enum`

    std::string name =
        expect(TokenType::Identifier, "Expected enum name").value;
    expect(TokenType::LeftBrace, "Expected '{' after enum name");

    std::vector<EnumEntry> entries;
    while (!check(TokenType::RightBrace) && !isAtEnd()) {
      EnumEntry entry;
      entry.loc = currentLoc();
      entry.name =
          expect(TokenType::Identifier, "Expected enum entry name").value;

      if (match(TokenType::Assign)) {
        entry.value = parseExpression();
      }

      entries.push_back(std::move(entry));

      if (!match(TokenType::Comma))
        break; // trailing comma is optional
    }

    expect(TokenType::RightBrace, "Expected '}' after enum body");
    return std::make_unique<EnumDecl>(std::move(name), std::move(entries),
                                      std::move(annotations), loc);
  }

  // --------------------------------------------------------
  // Expression statement — expr ;
  // --------------------------------------------------------
  StmtPtr parseExprStatement() {
    SourceLoc loc = currentLoc();
    ExprPtr expr = parseExpression();
    if (!expr) {
      synchronize();
      return nullptr;
    }
    expect(TokenType::Semicolon, "Expected ';' after expression");
    return std::make_unique<ExprStatement>(std::move(expr), loc);
  }

  // ============================================================
  // EXPRESSION PARSING — Pratt / precedence climbing
  //
  // Precedence levels (low → high):
  //   1  assignment     =  +=  -=  *=  /=        (right-assoc)
  //   2  null-coalesce  ??                        (right-assoc)
  //   3  logical-or     ||
  //   4  logical-and    &&
  //   5  equality       ==  !=
  //   6  comparison     <  <=  >  >=
  //   7  range          ..  ...
  //   8  addition       +  -
  //   9  multiplication *  /  %
  //  10  unary          !  - (prefix)
  //  11  postfix        ()  []  .  .?
  //  12  primary        literals, identifiers, grouping, lambda, new, this,
  //  super
  // ============================================================

  static constexpr int PREC_ASSIGN = 1;
  static constexpr int PREC_NULL_COALESCE = 2;
  static constexpr int PREC_OR = 3;
  static constexpr int PREC_AND = 4;
  static constexpr int PREC_EQUALITY = 5;
  static constexpr int PREC_COMPARISON = 6;
  static constexpr int PREC_RANGE = 7;
  static constexpr int PREC_ADDITION = 8;
  static constexpr int PREC_MULTIPLICATION = 9;
  static constexpr int PREC_UNARY = 10;
  static constexpr int PREC_POSTFIX = 11;
  static constexpr int PREC_PRIMARY = 12;

  // Returns the precedence of a binary/assignment operator,
  // or 0 if the token is not a binary operator.
  static int binaryPrecedence(TokenType type) {
    switch (type) {
    case TokenType::Assign:
    case TokenType::PlusAssign:
    case TokenType::MinusAssign:
    case TokenType::StarAssign:
    case TokenType::SlashAssign:
      return PREC_ASSIGN;

    case TokenType::QuestionQuestion:
      return PREC_NULL_COALESCE;
    case TokenType::Or:
      return PREC_OR;
    case TokenType::And:
      return PREC_AND;
    case TokenType::Equal:
    case TokenType::NotEqual:
      return PREC_EQUALITY;
    case TokenType::Less:
    case TokenType::LessEqual:
    case TokenType::Greater:
    case TokenType::GreaterEqual:
      return PREC_COMPARISON;
    case TokenType::DotDot:
    case TokenType::DotDotDot:
      return PREC_RANGE;
    case TokenType::Plus:
    case TokenType::Minus:
      return PREC_ADDITION;
    case TokenType::Star:
    case TokenType::Slash:
    case TokenType::Percent:
      return PREC_MULTIPLICATION;
    default:
      return 0;
    }
  }

  // Right-associative operators
  static bool isRightAssoc(TokenType type) {
    switch (type) {
    case TokenType::Assign:
    case TokenType::PlusAssign:
    case TokenType::MinusAssign:
    case TokenType::StarAssign:
    case TokenType::SlashAssign:
    case TokenType::QuestionQuestion:
      return true;
    default:
      return false;
    }
  }

  // --------------------------------------------------------
  // Top-level expression entry point
  // --------------------------------------------------------
  ExprPtr parseExpression() { return parseExprPrec(PREC_ASSIGN); }

  // --------------------------------------------------------
  // Precedence-climbing core.
  // Parses an expression where all operators have precedence >= minPrec.
  // --------------------------------------------------------
  ExprPtr parseExprPrec(int minPrec) {
    ExprPtr left = parseUnary();
    if (!left)
      return nullptr;

    while (true) {
      int prec = binaryPrecedence(current().type);
      if (prec < minPrec)
        break;

      TokenType opType = current().type;
      SourceLoc opLoc = currentLoc();
      advance(); // consume operator

      // For right-associative ops, the right side parses at the same
      // precedence. For left-associative ops, we go one higher to enforce left
      // grouping.
      int nextPrec = isRightAssoc(opType) ? prec : prec + 1;

      ExprPtr right = parseExprPrec(nextPrec);
      if (!right)
        return left; // error already recorded

      // Build the appropriate node based on operator type
      left = buildBinaryNode(opType, std::move(left), std::move(right), opLoc);
    }

    return left;
  }

  // --------------------------------------------------------
  // Route a binary operator token to the correct AST node.
  // --------------------------------------------------------
  ExprPtr buildBinaryNode(TokenType opType, ExprPtr left, ExprPtr right,
                          SourceLoc loc) {
    switch (opType) {
    // Assignment operators → AssignExpr
    case TokenType::Assign:
      return std::make_unique<AssignExpr>(AssignOp::Assign, std::move(left),
                                          std::move(right), loc);
    case TokenType::PlusAssign:
      return std::make_unique<AssignExpr>(AssignOp::PlusAssign, std::move(left),
                                          std::move(right), loc);
    case TokenType::MinusAssign:
      return std::make_unique<AssignExpr>(
          AssignOp::MinusAssign, std::move(left), std::move(right), loc);
    case TokenType::StarAssign:
      return std::make_unique<AssignExpr>(AssignOp::StarAssign, std::move(left),
                                          std::move(right), loc);
    case TokenType::SlashAssign:
      return std::make_unique<AssignExpr>(
          AssignOp::SlashAssign, std::move(left), std::move(right), loc);

    // Null coalescing → NullCoalesceExpr
    case TokenType::QuestionQuestion:
      return std::make_unique<NullCoalesceExpr>(std::move(left),
                                                std::move(right), loc);

    // Range → RangeExpr
    case TokenType::DotDot:
      return std::make_unique<RangeExpr>(std::move(left), std::move(right),
                                         true, loc);
    case TokenType::DotDotDot:
      return std::make_unique<RangeExpr>(std::move(left), std::move(right),
                                         false, loc);

    // Everything else → BinaryExpr
    default:
      return std::make_unique<BinaryExpr>(
          tokenToBinaryOp(opType), std::move(left), std::move(right), loc);
    }
  }

  static BinaryOp tokenToBinaryOp(TokenType type) {
    switch (type) {
    case TokenType::Plus:
      return BinaryOp::Add;
    case TokenType::Minus:
      return BinaryOp::Sub;
    case TokenType::Star:
      return BinaryOp::Mul;
    case TokenType::Slash:
      return BinaryOp::Div;
    case TokenType::Percent:
      return BinaryOp::Mod;
    case TokenType::Equal:
      return BinaryOp::Equal;
    case TokenType::NotEqual:
      return BinaryOp::NotEqual;
    case TokenType::Less:
      return BinaryOp::Less;
    case TokenType::LessEqual:
      return BinaryOp::LessEqual;
    case TokenType::Greater:
      return BinaryOp::Greater;
    case TokenType::GreaterEqual:
      return BinaryOp::GreaterEqual;
    case TokenType::And:
      return BinaryOp::And;
    case TokenType::Or:
      return BinaryOp::Or;
    default:
      return BinaryOp::Add; // unreachable
    }
  }

  // --------------------------------------------------------
  // Unary prefix — !expr, -expr
  // --------------------------------------------------------
  ExprPtr parseUnary() {
    if (check(TokenType::Not)) {
      SourceLoc loc = currentLoc();
      advance();
      ExprPtr operand = parseUnary(); // right-recursive for !! chaining
      return std::make_unique<UnaryExpr>(UnaryOp::Not, std::move(operand), loc);
    }
    if (check(TokenType::Minus)) {
      SourceLoc loc = currentLoc();
      advance();
      ExprPtr operand = parseUnary();
      return std::make_unique<UnaryExpr>(UnaryOp::Negate, std::move(operand),
                                         loc);
    }
    // Spread prefix: ...expr  (valid inside call args / array literals)
    if (check(TokenType::DotDotDot)) {
      SourceLoc loc = currentLoc();
      advance();
      ExprPtr operand = parseUnary();
      return std::make_unique<SpreadExpr>(std::move(operand), loc);
    }

    return parsePostfix();
  }

  // --------------------------------------------------------
  // Postfix — call(), index[], .member, ?.member
  // Loops so we can chain: obj.method(arg)[0].field
  // --------------------------------------------------------
  ExprPtr parsePostfix() {
    ExprPtr expr = parsePrimary();
    if (!expr)
      return nullptr;

    while (true) {
      if (check(TokenType::LeftParen)) {
        // Function call
        SourceLoc loc = currentLoc();
        advance(); // consume (
        ExprList args = parseArgumentList();
        expect(TokenType::RightParen, "Expected ')' after arguments");
        expr =
            std::make_unique<CallExpr>(std::move(expr), std::move(args), loc);
      } else if (check(TokenType::LeftBracket)) {
        // Index access
        SourceLoc loc = currentLoc();
        advance(); // consume [
        ExprPtr index = parseExpression();
        expect(TokenType::RightBracket, "Expected ']' after index");
        expr =
            std::make_unique<IndexExpr>(std::move(expr), std::move(index), loc);
      } else if (check(TokenType::Dot)) {
        // Member access
        SourceLoc loc = currentLoc();
        advance(); // consume .
        std::string member =
            expect(TokenType::Identifier, "Expected member name after '.'")
                .value;
        expr = std::make_unique<MemberExpr>(std::move(expr), std::move(member),
                                            loc);
      } else if (check(TokenType::DotQuestion)) {
        // Optional member access ?.
        SourceLoc loc = currentLoc();
        advance(); // consume .?
        std::string member =
            expect(TokenType::Identifier, "Expected member name after '.?'")
                .value;
        expr = std::make_unique<OptionalMemberExpr>(std::move(expr),
                                                    std::move(member), loc);
      } else {
        break;
      }
    }

    return expr;
  }

  // --------------------------------------------------------
  // Argument list — expr, expr, ...  (without parens)
  // Handles spread arguments (...expr).
  // --------------------------------------------------------
  ExprList parseArgumentList() {
    ExprList args;
    if (check(TokenType::RightParen))
      return args; // empty

    do {
      args.push_back(parseExpression());
    } while (match(TokenType::Comma));

    return args;
  }

  // --------------------------------------------------------
  // Primary expressions — the atomic building blocks.
  // --------------------------------------------------------
  ExprPtr parsePrimary() {
    SourceLoc loc = currentLoc();

    switch (current().type) {
    // --- Numeric literals ---
    case TokenType::Integer: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::Integer, std::move(val),
                                           loc);
    }
    case TokenType::HexInteger: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::HexInteger,
                                           std::move(val), loc);
    }
    case TokenType::BinaryInteger: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::BinaryInteger,
                                           std::move(val), loc);
    }
    case TokenType::Float: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::Float, std::move(val),
                                           loc);
    }
    case TokenType::Scientific: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::Scientific,
                                           std::move(val), loc);
    }

    // --- String literals ---
    case TokenType::String: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::String, std::move(val),
                                           loc);
    }
    case TokenType::RawString: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::RawString,
                                           std::move(val), loc);
    }
    case TokenType::InterpolatedString: {
      std::string raw = advance().value;
      return parseInterpolatedString(raw, loc);
    }

    // --- Character literal ---
    case TokenType::Char: {
      std::string val = advance().value;
      return std::make_unique<LiteralExpr>(LiteralKind::Char, std::move(val),
                                           loc);
    }

    // --- Boolean / Null ---
    case TokenType::True:
      advance();
      return std::make_unique<LiteralExpr>(LiteralKind::True, "", loc);
    case TokenType::False:
      advance();
      return std::make_unique<LiteralExpr>(LiteralKind::False, "", loc);
    case TokenType::Null:
      advance();
      return std::make_unique<LiteralExpr>(LiteralKind::Null, "", loc);

    // --- this / super ---
    case TokenType::This:
      advance();
      return std::make_unique<ThisExpr>(loc);
    case TokenType::Super:
      advance();
      return std::make_unique<SuperExpr>(loc);

    // --- new ClassName(args) ---
    case TokenType::New:
      return parseNew();

    // --- Parenthesized expression or lambda ---
    case TokenType::LeftParen:
      return parseParenOrLambda();

    // --- Array literal [elem, ...] ---
    case TokenType::LeftBracket:
      return parseArrayLiteral();

    // --- Identifier ---
    case TokenType::Identifier: {
      std::string name = advance().value;
      return std::make_unique<IdentifierExpr>(std::move(name), loc);
    }

    default:
      error("Unexpected token in expression");
      advance(); // consume the bad token so we don't loop forever
      return nullptr;
    }
  }

  // --------------------------------------------------------
  // `new` expression — new Name(args)
  // --------------------------------------------------------
  ExprPtr parseNew() {
    SourceLoc loc = currentLoc();
    advance(); // consume `new`

    std::string className =
        expect(TokenType::Identifier, "Expected class name after 'new'").value;

    ExprList args;
    if (match(TokenType::LeftParen)) {
      args = parseArgumentList();
      expect(TokenType::RightParen, "Expected ')' after constructor arguments");
    }

    return std::make_unique<NewExpr>(std::move(className), std::move(args),
                                     loc);
  }

  // --------------------------------------------------------
  // Parenthesized expression  OR  lambda.
  //
  // Disambiguation strategy:
  //   We tentatively try to parse as a lambda parameter list.
  //   A lambda is confirmed if we see `=>` after the closing `)`.
  //   If not, we treat the contents as a grouped expression.
  //
  //   To avoid backtracking, we use a lookahead scan:
  //     - If we see `) =>` anywhere at the matching paren level → lambda
  //     - Otherwise → grouped expression
  // --------------------------------------------------------
  ExprPtr parseParenOrLambda() {
    SourceLoc loc = currentLoc();

    if (isLambdaAhead()) {
      return parseLambda(loc);
    }

    // Grouped expression
    advance(); // consume (
    ExprPtr expr = parseExpression();
    expect(TokenType::RightParen, "Expected ')'");
    return expr;
  }

  // Lookahead: current token is an Identifier.  Scan forward past the
  // matching parentheses and check whether `{` follows the closing `)`.
  // If so this is a bare function declaration (no type / modifier prefix).
  //     foo(...)  {   →  true   (function decl)
  //     foo(...)  ;   →  false  (expression statement)
  bool isBareFunction() const {
    // pos_ must be at Identifier and pos_+1 must be '('
    if (pos_ + 1 >= tokens_.size() ||
        tokens_[pos_ + 1].type != TokenType::LeftParen)
      return false;

    int depth = 0;
    size_t i = pos_ + 1; // start at the '('
    while (i < tokens_.size()) {
      if (tokens_[i].type == TokenType::LeftParen)
        depth++;
      if (tokens_[i].type == TokenType::RightParen) {
        depth--;
        if (depth == 0) {
          // Move past the closing ')' to the next token
          size_t j = i + 1;
          if (j >= tokens_.size())
            return false;

          // Skip optional  -> returnType  suffix so that
          //   name() -> int32 { … }
          // is recognised the same as
          //   name() { … }
          if (tokens_[j].type == TokenType::Arrow) {
            j++; // skip ->
            if (j < tokens_.size() &&
                (isTypeToken(tokens_[j].type) ||
                 tokens_[j].type == TokenType::Identifier)) {
              j++; // skip the type name
            }
          }

          return (j < tokens_.size() &&
                  tokens_[j].type == TokenType::LeftBrace);
        }
      }
      i++;
    }
    return false;
  }

  // Lookahead: scan for the matching `)` and check if `=>` follows.
  bool isLambdaAhead() const {
    // pos_ points at `(`
    int depth = 0;
    size_t i = pos_;
    while (i < tokens_.size()) {
      if (tokens_[i].type == TokenType::LeftParen)
        depth++;
      if (tokens_[i].type == TokenType::RightParen) {
        depth--;
        if (depth == 0) {
          // Check if => follows
          return (i + 1 < tokens_.size() &&
                  tokens_[i + 1].type == TokenType::LambdaArrow);
        }
      }
      i++;
    }
    return false;
  }

  // --------------------------------------------------------
  // Lambda — (params) => expr  |  (params) => { block }
  // --------------------------------------------------------
  ExprPtr parseLambda(SourceLoc loc) {
    auto params = parseParameterList(); // consumes ( ... )
    expect(TokenType::LambdaArrow, "Expected '=>' in lambda");

    ExprPtr bodyExpr;
    StmtPtr bodyBlock;

    if (check(TokenType::LeftBrace)) {
      bodyBlock = parseBlock();
    } else {
      bodyExpr = parseExpression();
    }

    return std::make_unique<LambdaExpr>(std::move(params), std::move(bodyExpr),
                                        std::move(bodyBlock), loc);
  }

  // --------------------------------------------------------
  // Array literal — [ elem, elem, ... ]
  // Elements can include spread expressions (...arr).
  // --------------------------------------------------------
  ExprPtr parseArrayLiteral() {
    SourceLoc loc = currentLoc();
    advance(); // consume [

    ExprList elements;
    if (!check(TokenType::RightBracket)) {
      do {
        elements.push_back(parseExpression());
      } while (match(TokenType::Comma));
    }

    expect(TokenType::RightBracket, "Expected ']' after array literal");
    return std::make_unique<ArrayLiteralExpr>(std::move(elements), loc);
  }

  // --------------------------------------------------------
  // Interpolated string parsing.
  // The lexer already gave us the raw content with ${...} markers.
  // We split it into literal segments and sub-expressions here.
  //
  // Input example:  "Hello ${name}, you have ${count} items"
  //   raw = "Hello ${name}, you have ${count} items"
  //
  // Output:
  //   segments = ["Hello ", ", you have ", " items"]
  //   parts    = [IdentifierExpr("name"), IdentifierExpr("count")]
  // --------------------------------------------------------
  ExprPtr parseInterpolatedString(const std::string &raw, SourceLoc loc) {
    std::vector<std::string> segments;
    ExprList parts;

    std::string current_segment;
    size_t i = 0;

    while (i < raw.size()) {
      if (raw[i] == '$' && i + 1 < raw.size() && raw[i + 1] == '{') {
        // Save the literal segment so far
        segments.push_back(std::move(current_segment));
        current_segment.clear();

        // Find the matching }
        i += 2; // skip ${
        size_t start = i;
        int depth = 1;
        while (i < raw.size() && depth > 0) {
          if (raw[i] == '{')
            depth++;
          if (raw[i] == '}')
            depth--;
          if (depth > 0)
            i++;
        }

        std::string exprSource = raw.substr(start, i - start);
        i++; // skip closing }

        // Mini-lex and parse the interpolated expression.
        // We re-use the full Lexer + Parser on the sub-expression.
        Lexer subLexer(exprSource);
        auto subTokens = subLexer.tokenize();
        Parser subParser(subTokens);
        ExprPtr subExpr = subParser.parseExpression();

        if (subParser.hasErrors() || !subExpr) {
          error("Failed to parse interpolated expression");
          parts.push_back(std::make_unique<LiteralExpr>(LiteralKind::String,
                                                        exprSource, loc));
        } else {
          parts.push_back(std::move(subExpr));
        }
      } else {
        current_segment += raw[i];
        i++;
      }
    }

    // Final trailing segment (may be empty)
    segments.push_back(std::move(current_segment));

    return std::make_unique<InterpolatedStringExpr>(std::move(segments),
                                                    std::move(parts), loc);
  }

  // --------------------------------------------------------
  // Utility: is this token a type keyword?
  // --------------------------------------------------------
  static bool isTypeToken(TokenType type) {
    switch (type) {
    case TokenType::Int8:
    case TokenType::Int16:
    case TokenType::Int32:
    case TokenType::Int64:
    case TokenType::UInt8:
    case TokenType::UInt16:
    case TokenType::UInt32:
    case TokenType::UInt64:
    case TokenType::Float32:
    case TokenType::Float64:
    case TokenType::Bool:
    case TokenType::String_:
    case TokenType::Void:
    case TokenType::Auto:
      return true;
    default:
      return false;
    }
  }
};

} // namespace scriptlang

#endif // PARSER_H

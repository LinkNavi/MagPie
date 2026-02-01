#include "test_framework.h"
#include "scriptlang.h"

using namespace scriptlang;

// ============================================================
// Helper: lex + parse a source string, return the Program.
// Also asserts no parse errors (fails the test if there are any).
// ============================================================
static std::unique_ptr<Program> parseSource(const char* source) {
    Lexer  lexer(source);
    auto   tokens = lexer.tokenize();
    Parser parser(tokens);
    auto   program = parser.parse();

    // Surface any parse errors as test failures
    for (const auto& err : parser.errors()) {
        std::string msg = "Parse error at " + std::to_string(err.line) +
                          ":" + std::to_string(err.column) + " — " + err.message;
        // We push failures manually so the test runner reports them
        scriptlang::test::currentFailures().push_back({msg, __FILE__, __LINE__});
    }

    return program;
}

// Helper: get statement N from a program, dynamic_cast to T.
// Returns nullptr if cast fails or index is out of range.
template<typename T>
static T* getStmt(const Program& prog, size_t index = 0) {
    if (index >= prog.statements.size()) return nullptr;
    return dynamic_cast<T*>(prog.statements[index].get());
}

// Helper: get the first expression from an ExprStatement at index N.
static Expression* getExprStmt(const Program& prog, size_t index = 0) {
    auto* es = getStmt<ExprStatement>(prog, index);
    return es ? es->expr.get() : nullptr;
}

// ============================================================
// LITERAL EXPRESSION TESTS
// ============================================================

TEST(parser_integer_literal) {
    auto prog = parseSource("42;");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::Integer);
    EXPECT_EQ(lit->value, std::string("42"));
}

TEST(parser_hex_literal) {
    auto prog = parseSource("0xFF;");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::HexInteger);
    EXPECT_EQ(lit->value, std::string("0xFF"));
}

TEST(parser_binary_literal) {
    auto prog = parseSource("0b1010;");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::BinaryInteger);
}

TEST(parser_float_literal) {
    auto prog = parseSource("3.14;");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::Float);
    EXPECT_EQ(lit->value, std::string("3.14"));
}

TEST(parser_scientific_literal) {
    auto prog = parseSource("1.5e-3;");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::Scientific);
}

TEST(parser_string_literal) {
    auto prog = parseSource("\"hello world\";");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::String);
    EXPECT_EQ(lit->value, std::string("hello world"));
}

TEST(parser_raw_string_literal) {
    auto prog = parseSource("`raw content`; ");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::RawString);
    EXPECT_EQ(lit->value, std::string("raw content"));
}

TEST(parser_char_literal) {
    auto prog = parseSource("'a';");
    auto* lit = dynamic_cast<LiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::Char);
    EXPECT_EQ(lit->value, std::string("a"));
}

TEST(parser_true_false_null) {
    auto prog = parseSource("true; false; null;");
    auto* t = dynamic_cast<LiteralExpr*>(getExprStmt(*prog, 0));
    auto* f = dynamic_cast<LiteralExpr*>(getExprStmt(*prog, 1));
    auto* n = dynamic_cast<LiteralExpr*>(getExprStmt(*prog, 2));
    EXPECT_TRUE(t != nullptr);
    EXPECT_TRUE(f != nullptr);
    EXPECT_TRUE(n != nullptr);
    EXPECT_EQ(t->kind, LiteralKind::True);
    EXPECT_EQ(f->kind, LiteralKind::False);
    EXPECT_EQ(n->kind, LiteralKind::Null);
}

// ============================================================
// IDENTIFIER
// ============================================================

TEST(parser_identifier) {
    auto prog = parseSource("playerHealth;");
    auto* id = dynamic_cast<IdentifierExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(id != nullptr);
    EXPECT_EQ(id->name, std::string("playerHealth"));
}

// ============================================================
// BINARY EXPRESSIONS & PRECEDENCE
// ============================================================

TEST(parser_binary_add) {
    auto prog = parseSource("1 + 2;");
    auto* bin = dynamic_cast<BinaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(bin != nullptr);
    EXPECT_EQ(bin->op, BinaryOp::Add);
}

TEST(parser_binary_all_arithmetic) {
    // 1+2, 3-4, 5*6, 7/8, 9%10
    auto prog = parseSource("1+2; 3-4; 5*6; 7/8; 9%10;");
    auto* add = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 0)); EXPECT_EQ(add->op, BinaryOp::Add);
    auto* sub = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 1)); EXPECT_EQ(sub->op, BinaryOp::Sub);
    auto* mul = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 2)); EXPECT_EQ(mul->op, BinaryOp::Mul);
    auto* div = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 3)); EXPECT_EQ(div->op, BinaryOp::Div);
    auto* mod = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 4)); EXPECT_EQ(mod->op, BinaryOp::Mod);
}

TEST(parser_binary_comparison) {
    auto prog = parseSource("a < b; a <= b; a > b; a >= b; a == b; a != b;");
    auto* lt = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 0)); EXPECT_EQ(lt->op, BinaryOp::Less);
    auto* le = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 1)); EXPECT_EQ(le->op, BinaryOp::LessEqual);
    auto* gt = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 2)); EXPECT_EQ(gt->op, BinaryOp::Greater);
    auto* ge = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 3)); EXPECT_EQ(ge->op, BinaryOp::GreaterEqual);
    auto* eq = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 4)); EXPECT_EQ(eq->op, BinaryOp::Equal);
    auto* ne = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 5)); EXPECT_EQ(ne->op, BinaryOp::NotEqual);
}

TEST(parser_binary_logical) {
    auto prog = parseSource("a && b; a || b;");
    auto* an = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 0)); EXPECT_EQ(an->op, BinaryOp::And);
    auto* or_ = dynamic_cast<BinaryExpr*>(getExprStmt(*prog, 1)); EXPECT_EQ(or_->op, BinaryOp::Or);
}

// Precedence: * binds tighter than +
// 1 + 2 * 3  →  BinaryExpr(+, 1, BinaryExpr(*, 2, 3))
TEST(parser_precedence_mul_over_add) {
    auto prog  = parseSource("1 + 2 * 3;");
    auto* add  = dynamic_cast<BinaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(add != nullptr);
    EXPECT_EQ(add->op, BinaryOp::Add);

    auto* left = dynamic_cast<LiteralExpr*>(add->left.get());
    EXPECT_TRUE(left != nullptr);
    EXPECT_EQ(left->value, std::string("1"));

    auto* mul = dynamic_cast<BinaryExpr*>(add->right.get());
    EXPECT_TRUE(mul != nullptr);
    EXPECT_EQ(mul->op, BinaryOp::Mul);
}

// Precedence: && binds tighter than ||
TEST(parser_precedence_and_over_or) {
    auto prog = parseSource("a || b && c;");
    auto* or_ = dynamic_cast<BinaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(or_ != nullptr);
    EXPECT_EQ(or_->op, BinaryOp::Or);
    // right child should be &&
    auto* and_ = dynamic_cast<BinaryExpr*>(or_->right.get());
    EXPECT_TRUE(and_ != nullptr);
    EXPECT_EQ(and_->op, BinaryOp::And);
}

// Parentheses override precedence
TEST(parser_grouping_parens) {
    auto prog = parseSource("(1 + 2) * 3;");
    auto* mul = dynamic_cast<BinaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(mul != nullptr);
    EXPECT_EQ(mul->op, BinaryOp::Mul);
    // left child should be +
    auto* add = dynamic_cast<BinaryExpr*>(mul->left.get());
    EXPECT_TRUE(add != nullptr);
    EXPECT_EQ(add->op, BinaryOp::Add);
}

// Left associativity: 1 - 2 - 3  →  (1-2)-3
TEST(parser_left_associativity) {
    auto prog  = parseSource("1 - 2 - 3;");
    auto* outer = dynamic_cast<BinaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(outer != nullptr);
    EXPECT_EQ(outer->op, BinaryOp::Sub);
    auto* inner = dynamic_cast<BinaryExpr*>(outer->left.get());
    EXPECT_TRUE(inner != nullptr);
    EXPECT_EQ(inner->op, BinaryOp::Sub);
}

// ============================================================
// UNARY EXPRESSIONS
// ============================================================

TEST(parser_unary_negate) {
    auto prog = parseSource("-x;");
    auto* un = dynamic_cast<UnaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(un != nullptr);
    EXPECT_EQ(un->op, UnaryOp::Negate);
}

TEST(parser_unary_not) {
    auto prog = parseSource("!flag;");
    auto* un = dynamic_cast<UnaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(un != nullptr);
    EXPECT_EQ(un->op, UnaryOp::Not);
}

TEST(parser_double_not) {
    // !!x → UnaryExpr(Not, UnaryExpr(Not, x))
    auto prog  = parseSource("!!x;");
    auto* outer = dynamic_cast<UnaryExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(outer != nullptr);
    auto* inner = dynamic_cast<UnaryExpr*>(outer->operand.get());
    EXPECT_TRUE(inner != nullptr);
    EXPECT_EQ(inner->op, UnaryOp::Not);
}

// ============================================================
// ASSIGNMENT EXPRESSIONS
// ============================================================

TEST(parser_simple_assign) {
    auto prog = parseSource("x = 5;");
    auto* assign = dynamic_cast<AssignExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(assign != nullptr);
    EXPECT_EQ(assign->op, AssignOp::Assign);
}

TEST(parser_compound_assignments) {
    auto prog = parseSource("x += 1; x -= 2; x *= 3; x /= 4;");
    auto* pa = dynamic_cast<AssignExpr*>(getExprStmt(*prog, 0)); EXPECT_EQ(pa->op, AssignOp::PlusAssign);
    auto* ma = dynamic_cast<AssignExpr*>(getExprStmt(*prog, 1)); EXPECT_EQ(ma->op, AssignOp::MinusAssign);
    auto* sa = dynamic_cast<AssignExpr*>(getExprStmt(*prog, 2)); EXPECT_EQ(sa->op, AssignOp::StarAssign);
    auto* da = dynamic_cast<AssignExpr*>(getExprStmt(*prog, 3)); EXPECT_EQ(da->op, AssignOp::SlashAssign);
}

// Right-associativity: a = b = 5  →  AssignExpr(=, a, AssignExpr(=, b, 5))
TEST(parser_assign_right_associativity) {
    auto prog  = parseSource("a = b = 5;");
    auto* outer = dynamic_cast<AssignExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(outer != nullptr);
    auto* inner = dynamic_cast<AssignExpr*>(outer->value.get());
    EXPECT_TRUE(inner != nullptr);
    EXPECT_EQ(inner->op, AssignOp::Assign);
}

// ============================================================
// CALL EXPRESSIONS
// ============================================================

TEST(parser_call_no_args) {
    auto prog = parseSource("foo();");
    auto* call = dynamic_cast<CallExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(call != nullptr);
    EXPECT_SIZE(call->args, 0);
}

TEST(parser_call_with_args) {
    auto prog = parseSource("add(1, 2, 3);");
    auto* call = dynamic_cast<CallExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(call != nullptr);
    EXPECT_SIZE(call->args, 3);
}

TEST(parser_chained_calls) {
    // foo()(1) → CallExpr(CallExpr(foo), [1])
    auto prog  = parseSource("foo()(1);");
    auto* outer = dynamic_cast<CallExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(outer != nullptr);
    EXPECT_SIZE(outer->args, 1);
    auto* inner = dynamic_cast<CallExpr*>(outer->callee.get());
    EXPECT_TRUE(inner != nullptr);
    EXPECT_SIZE(inner->args, 0);
}

// ============================================================
// MEMBER ACCESS
// ============================================================

TEST(parser_member_access) {
    auto prog = parseSource("player.health;");
    auto* mem = dynamic_cast<MemberExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(mem != nullptr);
    EXPECT_EQ(mem->member, std::string("health"));
}

TEST(parser_chained_member_access) {
    // a.b.c → MemberExpr(MemberExpr(a, b), c)
    auto prog  = parseSource("a.b.c;");
    auto* outer = dynamic_cast<MemberExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(outer != nullptr);
    EXPECT_EQ(outer->member, std::string("c"));
    auto* inner = dynamic_cast<MemberExpr*>(outer->object.get());
    EXPECT_TRUE(inner != nullptr);
    EXPECT_EQ(inner->member, std::string("b"));
}

TEST(parser_method_call) {
    // player.heal(10) → CallExpr(MemberExpr(player, heal), [10])
    auto prog = parseSource("player.heal(10);");
    auto* call = dynamic_cast<CallExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(call != nullptr);
    auto* mem = dynamic_cast<MemberExpr*>(call->callee.get());
    EXPECT_TRUE(mem != nullptr);
    EXPECT_EQ(mem->member, std::string("heal"));
}

// ============================================================
// OPTIONAL MEMBER ACCESS
// ============================================================

TEST(parser_optional_member) {
    auto prog = parseSource("player.?name;");
    auto* opt = dynamic_cast<OptionalMemberExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(opt != nullptr);
    EXPECT_EQ(opt->member, std::string("name"));
}

// ============================================================
// INDEX ACCESS
// ============================================================

TEST(parser_index_access) {
    auto prog = parseSource("arr[0];");
    auto* idx = dynamic_cast<IndexExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(idx != nullptr);
}

TEST(parser_chained_index) {
    // matrix[0][1] → IndexExpr(IndexExpr(matrix, 0), 1)
    auto prog  = parseSource("matrix[0][1];");
    auto* outer = dynamic_cast<IndexExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(outer != nullptr);
    auto* inner = dynamic_cast<IndexExpr*>(outer->object.get());
    EXPECT_TRUE(inner != nullptr);
}

// ============================================================
// NULL COALESCING
// ============================================================

TEST(parser_null_coalesce) {
    auto prog = parseSource("a ?? b;");
    auto* nc = dynamic_cast<NullCoalesceExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(nc != nullptr);
}

// ============================================================
// RANGE EXPRESSIONS
// ============================================================

TEST(parser_range_dotdot) {
    auto prog = parseSource("0..10;");
    auto* range = dynamic_cast<RangeExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(range != nullptr);
    EXPECT_TRUE(range->inclusive);
}

// ============================================================
// SPREAD EXPRESSION
// ============================================================

TEST(parser_spread_in_call) {
    auto prog = parseSource("foo(...args);");
    auto* call = dynamic_cast<CallExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(call != nullptr);
    EXPECT_SIZE(call->args, 1);
    auto* spread = dynamic_cast<SpreadExpr*>(call->args[0].get());
    EXPECT_TRUE(spread != nullptr);
}

TEST(parser_spread_in_array) {
    auto prog = parseSource("[...a, ...b];");
    auto* arr = dynamic_cast<ArrayLiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(arr != nullptr);
    EXPECT_SIZE(arr->elements, 2);
    auto* s0 = dynamic_cast<SpreadExpr*>(arr->elements[0].get());
    auto* s1 = dynamic_cast<SpreadExpr*>(arr->elements[1].get());
    EXPECT_TRUE(s0 != nullptr);
    EXPECT_TRUE(s1 != nullptr);
}

// ============================================================
// ARRAY LITERAL
// ============================================================

TEST(parser_array_literal_empty) {
    auto prog = parseSource("[];");
    auto* arr = dynamic_cast<ArrayLiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(arr != nullptr);
    EXPECT_SIZE(arr->elements, 0);
}

TEST(parser_array_literal_elements) {
    auto prog = parseSource("[1, 2, 3];");
    auto* arr = dynamic_cast<ArrayLiteralExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(arr != nullptr);
    EXPECT_SIZE(arr->elements, 3);
}

// ============================================================
// LAMBDA EXPRESSIONS
// ============================================================

TEST(parser_lambda_no_params_block) {
    auto prog = parseSource("() => { return 1; };");
    auto* lam = dynamic_cast<LambdaExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lam != nullptr);
    EXPECT_SIZE(lam->params, 0);
    EXPECT_TRUE(lam->bodyBlock != nullptr);
    EXPECT_TRUE(lam->bodyExpr == nullptr);
}

TEST(parser_lambda_single_param_expr) {
    auto prog = parseSource("(x) => x * 2;");
    auto* lam = dynamic_cast<LambdaExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lam != nullptr);
    EXPECT_SIZE(lam->params, 1);
    EXPECT_EQ(lam->params[0].name, std::string("x"));
    EXPECT_TRUE(lam->bodyExpr != nullptr);
    EXPECT_TRUE(lam->bodyBlock == nullptr);
}

TEST(parser_lambda_multiple_params) {
    auto prog = parseSource("(a, b, c) => a + b + c;");
    auto* lam = dynamic_cast<LambdaExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(lam != nullptr);
    EXPECT_SIZE(lam->params, 3);
}

TEST(parser_lambda_as_argument) {
    auto prog = parseSource("arr.map((x) => x + 1);");
    auto* call = dynamic_cast<CallExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(call != nullptr);
    EXPECT_SIZE(call->args, 1);
    auto* lam = dynamic_cast<LambdaExpr*>(call->args[0].get());
    EXPECT_TRUE(lam != nullptr);
}

// ============================================================
// NEW EXPRESSION
// ============================================================

TEST(parser_new_no_args) {
    auto prog = parseSource("new Player();");
    auto* ne = dynamic_cast<NewExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(ne != nullptr);
    EXPECT_EQ(ne->className, std::string("Player"));
    EXPECT_SIZE(ne->args, 0);
}

TEST(parser_new_with_args) {
    auto prog = parseSource("new Vector3(1, 2, 3);");
    auto* ne = dynamic_cast<NewExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(ne != nullptr);
    EXPECT_EQ(ne->className, std::string("Vector3"));
    EXPECT_SIZE(ne->args, 3);
}

// ============================================================
// THIS / SUPER
// ============================================================

TEST(parser_this_expr) {
    auto prog = parseSource("this;");
    auto* t = dynamic_cast<ThisExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(t != nullptr);
}

TEST(parser_super_expr) {
    auto prog = parseSource("super;");
    auto* s = dynamic_cast<SuperExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(s != nullptr);
}

// ============================================================
// INTERPOLATED STRINGS
// ============================================================

TEST(parser_interpolated_string_simple) {
    auto prog = parseSource("\"Hello ${name}\";");
    auto* interp = dynamic_cast<InterpolatedStringExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(interp != nullptr);
    EXPECT_SIZE(interp->segments, 2); // "Hello " and ""
    EXPECT_SIZE(interp->parts, 1);
}

TEST(parser_interpolated_string_multiple) {
    auto prog = parseSource("\"${a} + ${b} = ${c}\";");
    auto* interp = dynamic_cast<InterpolatedStringExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(interp != nullptr);
    EXPECT_SIZE(interp->parts, 3);
    EXPECT_SIZE(interp->segments, 4); // "", " + ", " = ", ""
}

TEST(parser_interpolated_string_expression) {
    // Expression inside interpolation
    auto prog = parseSource("\"Value: ${x + 1}\";");
    auto* interp = dynamic_cast<InterpolatedStringExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(interp != nullptr);
    EXPECT_SIZE(interp->parts, 1);
    // The part should be a BinaryExpr
    auto* bin = dynamic_cast<BinaryExpr*>(interp->parts[0].get());
    EXPECT_TRUE(bin != nullptr);
    EXPECT_EQ(bin->op, BinaryOp::Add);
}

// ============================================================
// VAR / CONST DECLARATIONS
// ============================================================

TEST(parser_var_decl_simple) {
    auto prog = parseSource("var x = 5;");
    auto* vd = getStmt<VarDecl>(*prog);
    EXPECT_TRUE(vd != nullptr);
    EXPECT_EQ(vd->name, std::string("x"));
    EXPECT_TRUE(vd->initializer != nullptr);
}

TEST(parser_var_decl_no_init) {
    auto prog = parseSource("var x;");
    auto* vd = getStmt<VarDecl>(*prog);
    EXPECT_TRUE(vd != nullptr);
    EXPECT_EQ(vd->name, std::string("x"));
    EXPECT_TRUE(vd->initializer == nullptr);
}

TEST(parser_var_decl_with_type) {
    auto prog = parseSource("var x: int32 = 10;");
    auto* vd = getStmt<VarDecl>(*prog);
    EXPECT_TRUE(vd != nullptr);
    EXPECT_TRUE(vd->typeAnnot != nullptr);
    EXPECT_EQ(vd->typeAnnot->name, std::string("int32"));
}

TEST(parser_const_decl) {
    auto prog = parseSource("const PI = 3.14;");
    auto* cd = getStmt<ConstDecl>(*prog);
    EXPECT_TRUE(cd != nullptr);
    EXPECT_EQ(cd->name, std::string("PI"));
    EXPECT_TRUE(cd->initializer != nullptr);
}

TEST(parser_const_decl_hex) {
    auto prog = parseSource("const COLOR_RED = 0xFF0000;");
    auto* cd = getStmt<ConstDecl>(*prog);
    EXPECT_TRUE(cd != nullptr);
    EXPECT_EQ(cd->name, std::string("COLOR_RED"));
    auto* lit = dynamic_cast<LiteralExpr*>(cd->initializer.get());
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::HexInteger);
}

// ============================================================
// BLOCK STATEMENTS
// ============================================================

TEST(parser_block) {
    auto prog = parseSource("{ var x = 1; var y = 2; }");
    auto* block = getStmt<BlockStatement>(*prog);
    EXPECT_TRUE(block != nullptr);
    EXPECT_SIZE(block->statements, 2);
}

TEST(parser_nested_blocks) {
    auto prog = parseSource("{ { var x = 1; } }");
    auto* outer = getStmt<BlockStatement>(*prog);
    EXPECT_TRUE(outer != nullptr);
    EXPECT_SIZE(outer->statements, 1);
    auto* inner = dynamic_cast<BlockStatement*>(outer->statements[0].get());
    EXPECT_TRUE(inner != nullptr);
    EXPECT_SIZE(inner->statements, 1);
}

// ============================================================
// RETURN STATEMENT
// ============================================================

TEST(parser_return_with_value) {
    // Wrap in a function so semantically valid, but parser doesn't enforce that
    auto prog = parseSource("return 42;");
    auto* ret = getStmt<ReturnStatement>(*prog);
    EXPECT_TRUE(ret != nullptr);
    EXPECT_TRUE(ret->value != nullptr);
}

TEST(parser_return_bare) {
    auto prog = parseSource("return;");
    auto* ret = getStmt<ReturnStatement>(*prog);
    EXPECT_TRUE(ret != nullptr);
    EXPECT_TRUE(ret->value == nullptr);
}

// ============================================================
// IF STATEMENTS
// ============================================================

TEST(parser_if_simple) {
    auto prog = parseSource("if (x > 0) { y = 1; }");
    auto* ifst = getStmt<IfStatement>(*prog);
    EXPECT_TRUE(ifst != nullptr);
    EXPECT_TRUE(ifst->condition != nullptr);
    EXPECT_TRUE(ifst->thenBranch != nullptr);
    EXPECT_TRUE(ifst->elseBranch == nullptr);
}

TEST(parser_if_else) {
    auto prog = parseSource("if (x) { a = 1; } else { a = 2; }");
    auto* ifst = getStmt<IfStatement>(*prog);
    EXPECT_TRUE(ifst != nullptr);
    EXPECT_TRUE(ifst->elseBranch != nullptr);
    // else branch should be a BlockStatement
    auto* elseBlock = dynamic_cast<BlockStatement*>(ifst->elseBranch.get());
    EXPECT_TRUE(elseBlock != nullptr);
}

TEST(parser_if_else_if_chain) {
    auto prog = parseSource("if (a) { } else if (b) { } else { }");
    auto* ifst = getStmt<IfStatement>(*prog);
    EXPECT_TRUE(ifst != nullptr);
    // else branch is another IfStatement
    auto* elseIf = dynamic_cast<IfStatement*>(ifst->elseBranch.get());
    EXPECT_TRUE(elseIf != nullptr);
    // and that one has an else block
    EXPECT_TRUE(elseIf->elseBranch != nullptr);
}

// ============================================================
// WHILE LOOP
// ============================================================

TEST(parser_while_loop) {
    auto prog = parseSource("while (running) { tick(); }");
    auto* wh = getStmt<WhileStatement>(*prog);
    EXPECT_TRUE(wh != nullptr);
    EXPECT_TRUE(wh->condition != nullptr);
    EXPECT_TRUE(wh->body != nullptr);
}

// ============================================================
// FOR LOOPS
// ============================================================

TEST(parser_c_style_for) {
    auto prog = parseSource("for (var i = 0; i < 10; i = i + 1) { }");
    auto* forst = getStmt<ForStatement>(*prog);
    EXPECT_TRUE(forst != nullptr);
    EXPECT_TRUE(forst->init != nullptr);
    EXPECT_TRUE(forst->condition != nullptr);
    EXPECT_TRUE(forst->update != nullptr);
    EXPECT_TRUE(forst->body != nullptr);
}

TEST(parser_c_style_for_empty_parts) {
    auto prog = parseSource("for (;;) { break; }");
    auto* forst = getStmt<ForStatement>(*prog);
    EXPECT_TRUE(forst != nullptr);
    EXPECT_TRUE(forst->init == nullptr);
    EXPECT_TRUE(forst->condition == nullptr);
    EXPECT_TRUE(forst->update == nullptr);
}

TEST(parser_for_in_loop) {
    auto prog = parseSource("for item in items { }");
    auto* forin = getStmt<ForInStatement>(*prog);
    EXPECT_TRUE(forin != nullptr);
    EXPECT_EQ(forin->name, std::string("item"));
    EXPECT_TRUE(forin->iterable != nullptr);
}

TEST(parser_for_in_with_range) {
    auto prog = parseSource("for i in 0..10 { }");
    auto* forin = getStmt<ForInStatement>(*prog);
    EXPECT_TRUE(forin != nullptr);
    EXPECT_EQ(forin->name, std::string("i"));
    // iterable should be a RangeExpr
    auto* range = dynamic_cast<RangeExpr*>(forin->iterable.get());
    EXPECT_TRUE(range != nullptr);
}

// ============================================================
// SWITCH STATEMENT
// ============================================================

TEST(parser_switch_basic) {
    auto prog = parseSource(R"(
        switch (x) {
            case 1: y = 10; break;
            case 2: y = 20; break;
            default: y = 0;
        }
    )");
    auto* sw = getStmt<SwitchStatement>(*prog);
    EXPECT_TRUE(sw != nullptr);
    EXPECT_SIZE(sw->cases, 3);
    // First two have values, third is default
    EXPECT_TRUE(sw->cases[0].value != nullptr);
    EXPECT_TRUE(sw->cases[1].value != nullptr);
    EXPECT_TRUE(sw->cases[2].value == nullptr); // default
}

// ============================================================
// BREAK / CONTINUE
// ============================================================

TEST(parser_break) {
    auto prog = parseSource("break;");
    auto* brk = getStmt<BreakStatement>(*prog);
    EXPECT_TRUE(brk != nullptr);
}

TEST(parser_continue) {
    auto prog = parseSource("continue;");
    auto* cont = getStmt<ContinueStatement>(*prog);
    EXPECT_TRUE(cont != nullptr);
}

// ============================================================
// FUNCTION DECLARATIONS
// ============================================================

TEST(parser_function_no_return_type) {
    auto prog = parseSource("foo() { return 1; }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_EQ(fn->name, std::string("foo"));
    EXPECT_SIZE(fn->params, 0);
    EXPECT_TRUE(fn->body != nullptr);
}

TEST(parser_function_with_params) {
    auto prog = parseSource("add(a, b) { return a + b; }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_SIZE(fn->params, 2);
    EXPECT_EQ(fn->params[0].name, std::string("a"));
    EXPECT_EQ(fn->params[1].name, std::string("b"));
}

TEST(parser_function_with_return_type) {
    auto prog = parseSource("int32 getValue() { return 42; }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_EQ(fn->name, std::string("getValue"));
    EXPECT_TRUE(fn->returnType != nullptr);
    EXPECT_EQ(fn->returnType->name, std::string("int32"));
}

TEST(parser_function_arrow_return_type) {
    auto prog = parseSource("getValue() -> int32 { return 42; }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_TRUE(fn->returnType != nullptr);
    EXPECT_EQ(fn->returnType->name, std::string("int32"));
}

TEST(parser_void_function) {
    auto prog = parseSource("void doSomething() { }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_EQ(fn->name, std::string("doSomething"));
    EXPECT_TRUE(fn->returnType != nullptr);
    EXPECT_EQ(fn->returnType->name, std::string("void"));
}

TEST(parser_function_with_typed_params) {
    auto prog = parseSource("move(x: int32, y: int32) { }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_SIZE(fn->params, 2);
    EXPECT_TRUE(fn->params[0].typeAnnot != nullptr);
    EXPECT_EQ(fn->params[0].typeAnnot->name, std::string("int32"));
}

TEST(parser_function_spread_param) {
    auto prog = parseSource("variadic(...args) { }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_SIZE(fn->params, 1);
    EXPECT_TRUE(fn->params[0].isSpread);
    EXPECT_EQ(fn->params[0].name, std::string("args"));
}

// ============================================================
// CLASS DECLARATIONS
// ============================================================

TEST(parser_class_empty) {
    auto prog = parseSource("class Foo { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_EQ(cls->name, std::string("Foo"));
    EXPECT_SIZE(cls->members, 0);
}

TEST(parser_class_with_parent) {
    auto prog = parseSource("class Dog : Animal { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_EQ(cls->name, std::string("Dog"));
    EXPECT_EQ(cls->parent, std::string("Animal"));
}

TEST(parser_class_with_fields) {
    auto prog = parseSource(R"(
        class Player {
            var health = 100;
            var name = "Hero";
        }
    )");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->members, 2);
}

TEST(parser_class_with_methods) {
    auto prog = parseSource(R"(
        class Player {
            var health = 100;
            void takeDamage(amount: int32) {
                health -= amount;
            }
        }
    )");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->members, 2);
    // Second member should be a FunctionDecl
    auto* method = dynamic_cast<FunctionDecl*>(cls->members[1].decl.get());
    EXPECT_TRUE(method != nullptr);
    EXPECT_EQ(method->name, std::string("takeDamage"));
}

TEST(parser_class_visibility_modifiers) {
    auto prog = parseSource(R"(
        class Foo {
            public var x = 1;
            private var y = 2;
        }
    )");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->members, 2);
    EXPECT_TRUE(cls->members[0].isPublic);
    EXPECT_FALSE(cls->members[1].isPublic);
}

TEST(parser_public_class) {
    auto prog = parseSource("public class Visible { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_EQ(cls->name, std::string("Visible"));
}

// ============================================================
// STRUCT DECLARATIONS
// ============================================================

TEST(parser_struct_basic) {
    auto prog = parseSource(R"(
        struct Vector2 {
            var x = 0.0;
            var y = 0.0;
        }
    )");
    auto* st = getStmt<StructDecl>(*prog);
    EXPECT_TRUE(st != nullptr);
    EXPECT_EQ(st->name, std::string("Vector2"));
    EXPECT_SIZE(st->members, 2);
}

// ============================================================
// ENUM DECLARATIONS
// ============================================================

TEST(parser_enum_basic) {
    auto prog = parseSource(R"(
        enum Color {
            Red,
            Green,
            Blue
        }
    )");
    auto* en = getStmt<EnumDecl>(*prog);
    EXPECT_TRUE(en != nullptr);
    EXPECT_EQ(en->name, std::string("Color"));
    EXPECT_SIZE(en->entries, 3);
    EXPECT_EQ(en->entries[0].name, std::string("Red"));
    EXPECT_EQ(en->entries[1].name, std::string("Green"));
    EXPECT_EQ(en->entries[2].name, std::string("Blue"));
}

TEST(parser_enum_with_values) {
    auto prog = parseSource(R"(
        enum HttpStatus {
            Ok = 200,
            NotFound = 404,
            Error = 500
        }
    )");
    auto* en = getStmt<EnumDecl>(*prog);
    EXPECT_TRUE(en != nullptr);
    EXPECT_SIZE(en->entries, 3);
    EXPECT_TRUE(en->entries[0].value != nullptr);
    EXPECT_TRUE(en->entries[1].value != nullptr);
}

// ============================================================
// INCLUDE DIRECTIVE
// ============================================================

TEST(parser_include_simple) {
    auto prog = parseSource("#include <engine/core>");
    auto* inc = getStmt<IncludeDecl>(*prog);
    EXPECT_TRUE(inc != nullptr);
    EXPECT_EQ(inc->path, std::string("engine/core"));
    EXPECT_SIZE(inc->selectedNames, 0);
}

TEST(parser_include_with_selection) {
    auto prog = parseSource("#include <engine/console> { Print, Log }");
    auto* inc = getStmt<IncludeDecl>(*prog);
    EXPECT_TRUE(inc != nullptr);
    EXPECT_EQ(inc->path, std::string("engine/console"));
    EXPECT_SIZE(inc->selectedNames, 2);
    EXPECT_EQ(inc->selectedNames[0], std::string("Print"));
    EXPECT_EQ(inc->selectedNames[1], std::string("Log"));
}

// ============================================================
// ANNOTATIONS
// ============================================================

TEST(parser_annotation_on_class) {
    auto prog = parseSource("@lifecycle(phase=init) class Game { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    EXPECT_EQ(cls->annotations[0].name, std::string("lifecycle"));
}

TEST(parser_annotation_on_function) {
    auto prog = parseSource("@lifecycle(phase=init) void onStart() { }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_SIZE(fn->annotations, 1);
    EXPECT_EQ(fn->annotations[0].name, std::string("lifecycle"));
}

// ============================================================
// REALISTIC GAME-DEV SNIPPETS (from syntax.mp)
// ============================================================

TEST(parser_color_constants) {
    auto prog = parseSource(R"(
        const COLOR_RED   = 0xFF0000;
        const COLOR_GREEN = 0x00FF00;
        const COLOR_BLUE  = 0x0000FF;
    )");
    EXPECT_SIZE(prog->statements, 3);
    for (size_t i = 0; i < 3; i++) {
        auto* cd = dynamic_cast<ConstDecl*>(prog->statements[i].get());
        EXPECT_TRUE(cd != nullptr);
    }
}

TEST(parser_bit_flags) {
    auto prog = parseSource(R"(
        const FLAG_RENDERABLE  = 0b00000001;
        const FLAG_COLLIDABLE  = 0b00000010;
        const FLAG_INTERACTIVE = 0b00000100;
    )");
    EXPECT_SIZE(prog->statements, 3);
    auto* cd = dynamic_cast<ConstDecl*>(prog->statements[0].get());
    EXPECT_TRUE(cd != nullptr);
    auto* lit = dynamic_cast<LiteralExpr*>(cd->initializer.get());
    EXPECT_TRUE(lit != nullptr);
    EXPECT_EQ(lit->kind, LiteralKind::BinaryInteger);
}

TEST(parser_physics_constants_scientific) {
    auto prog = parseSource(R"(
        const EPSILON     = 1.0e-6;
        const MAX_VEL     = 1.0e4;
    )");
    EXPECT_SIZE(prog->statements, 2);
    auto* cd = dynamic_cast<ConstDecl*>(prog->statements[0].get());
    auto* lit = dynamic_cast<LiteralExpr*>(cd->initializer.get());
    EXPECT_EQ(lit->kind, LiteralKind::Scientific);
}

TEST(parser_lambda_callback_pattern) {
    auto prog = parseSource(R"(
        var button = new Button();
        button.setOnClick(() => {
            Print("clicked");
        });
    )");
    EXPECT_SIZE(prog->statements, 2);
    // Second statement: expression statement with a call
    auto* es = dynamic_cast<ExprStatement*>(prog->statements[1].get());
    EXPECT_TRUE(es != nullptr);
    auto* call = dynamic_cast<CallExpr*>(es->expr.get());
    EXPECT_TRUE(call != nullptr);
    // The argument should be a lambda
    EXPECT_SIZE(call->args, 1);
    auto* lam = dynamic_cast<LambdaExpr*>(call->args[0].get());
    EXPECT_TRUE(lam != nullptr);
    EXPECT_SIZE(lam->params, 0);
    EXPECT_TRUE(lam->bodyBlock != nullptr);
}

TEST(parser_for_in_range_loop) {
    auto prog = parseSource(R"(
        for i in 0..10 {
            Print("hello");
        }
    )");
    auto* forin = getStmt<ForInStatement>(*prog);
    EXPECT_TRUE(forin != nullptr);
    auto* range = dynamic_cast<RangeExpr*>(forin->iterable.get());
    EXPECT_TRUE(range != nullptr);
    EXPECT_TRUE(range->inclusive);
}

TEST(parser_array_map_with_lambda) {
    auto prog = parseSource("var doubled = numbers.map((x) => x * 2);");
    auto* vd = getStmt<VarDecl>(*prog);
    EXPECT_TRUE(vd != nullptr);
    EXPECT_EQ(vd->name, std::string("doubled"));
    // initializer is a call to numbers.map(...)
    auto* call = dynamic_cast<CallExpr*>(vd->initializer.get());
    EXPECT_TRUE(call != nullptr);
    auto* mem = dynamic_cast<MemberExpr*>(call->callee.get());
    EXPECT_TRUE(mem != nullptr);
    EXPECT_EQ(mem->member, std::string("map"));
}

TEST(parser_null_coalesce_optional_chain) {
    auto prog = parseSource("var name = player.?name ?? \"Unknown\";");
    auto* vd = getStmt<VarDecl>(*prog);
    EXPECT_TRUE(vd != nullptr);
    auto* nc = dynamic_cast<NullCoalesceExpr*>(vd->initializer.get());
    EXPECT_TRUE(nc != nullptr);
    auto* opt = dynamic_cast<OptionalMemberExpr*>(nc->left.get());
    EXPECT_TRUE(opt != nullptr);
    EXPECT_EQ(opt->member, std::string("name"));
}

TEST(parser_full_class_with_methods) {
    auto prog = parseSource(R"(
        public class Entity {
            private var health = 100;

            void takeDamage(amount: int32) {
                health -= amount;
                if (health <= 0) {
                    onDeath();
                }
            }

            void onDeath() {
                return;
            }
        }
    )");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_EQ(cls->name, std::string("Entity"));
    EXPECT_SIZE(cls->members, 3); // health, takeDamage, onDeath

    // Verify the method has proper structure
    auto* method = dynamic_cast<FunctionDecl*>(cls->members[1].decl.get());
    EXPECT_TRUE(method != nullptr);
    EXPECT_EQ(method->name, std::string("takeDamage"));
    EXPECT_SIZE(method->params, 1);
    EXPECT_EQ(method->params[0].name, std::string("amount"));
}

TEST(parser_switch_with_hex_cases) {
    auto prog = parseSource(R"(
        switch (key) {
            case 'w': moveForward(); break;
            case 's': moveBack(); break;
            default: idle();
        }
    )");
    auto* sw = getStmt<SwitchStatement>(*prog);
    EXPECT_TRUE(sw != nullptr);
    EXPECT_SIZE(sw->cases, 3);
}

TEST(parser_complex_expression) {
    // a.b(c + d * e, ...f)[0]
    auto prog = parseSource("a.b(c + d * e, ...f)[0];");
    auto* idx = dynamic_cast<IndexExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(idx != nullptr);
    auto* call = dynamic_cast<CallExpr*>(idx->object.get());
    EXPECT_TRUE(call != nullptr);
    EXPECT_SIZE(call->args, 2);
    // Second arg is a spread
    auto* spread = dynamic_cast<SpreadExpr*>(call->args[1].get());
    EXPECT_TRUE(spread != nullptr);
}

TEST(parser_nested_lambda_calls) {
    // Events.on('attack', (a, t) => { dealDamage(a, t); });
    auto prog = parseSource(R"(
        Events.on("attack", (a, t) => {
            dealDamage(a, t);
        });
    )");
    auto* es = dynamic_cast<ExprStatement*>(prog->statements[0].get());
    EXPECT_TRUE(es != nullptr);
    auto* call = dynamic_cast<CallExpr*>(es->expr.get());
    EXPECT_TRUE(call != nullptr);
    EXPECT_SIZE(call->args, 2);
    auto* lam = dynamic_cast<LambdaExpr*>(call->args[1].get());
    EXPECT_TRUE(lam != nullptr);
    EXPECT_SIZE(lam->params, 2);
}

TEST(parser_member_assign) {
    auto prog = parseSource("player.health = 50;");
    auto* assign = dynamic_cast<AssignExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(assign != nullptr);
    auto* mem = dynamic_cast<MemberExpr*>(assign->target.get());
    EXPECT_TRUE(mem != nullptr);
    EXPECT_EQ(mem->member, std::string("health"));
}

TEST(parser_index_assign) {
    auto prog = parseSource("arr[0] = 42;");
    auto* assign = dynamic_cast<AssignExpr*>(getExprStmt(*prog));
    EXPECT_TRUE(assign != nullptr);
    auto* idx = dynamic_cast<IndexExpr*>(assign->target.get());
    EXPECT_TRUE(idx != nullptr);
}

// ============================================================
// ERROR HANDLING — parser should record errors, not crash
// ============================================================

TEST(parser_error_missing_semicolon) {
    Lexer  lexer("var x = 5");
    auto   tokens = lexer.tokenize();
    Parser parser(tokens);
    parser.parse();
    EXPECT_TRUE(parser.hasErrors());
}

TEST(parser_error_unexpected_token) {
    Lexer  lexer("var = ;");
    auto   tokens = lexer.tokenize();
    Parser parser(tokens);
    parser.parse();
    EXPECT_TRUE(parser.hasErrors());
}

TEST(parser_error_unclosed_paren) {
    Lexer  lexer("foo(1, 2;");
    auto   tokens = lexer.tokenize();
    Parser parser(tokens);
    parser.parse();
    EXPECT_TRUE(parser.hasErrors());
}

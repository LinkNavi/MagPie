#include "test_framework.h"
#include "scriptlang.h"

using namespace scriptlang;

// ============================================================
// Helper: full compilation pipeline
// ============================================================
struct CompileResult {
    std::unique_ptr<Program> program;
    SemanticAnalyzer analyzer;
    bool success;
    
    CompileResult() : success(false) {}
};

static CompileResult compile(const char* source) {
    CompileResult result;
    
    // Lex
    Lexer lexer(source);
    auto tokens = lexer.tokenize();
    
    // Parse
    Parser parser(tokens);
    result.program = parser.parse();
    
    if (parser.hasErrors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << "Parse error: " << err.message << "\n";
        }
        return result;
    }
    
    // Semantic analysis
    result.success = result.analyzer.analyze(*result.program);
    
    if (!result.success) {
        for (const auto& diag : result.analyzer.diagnostics().all()) {
            std::cerr << "Semantic error: " << diag.message << "\n";
        }
    }
    
    return result;
}

// ============================================================
// INCLUDE RESOLUTION TESTS
// ============================================================

TEST(semantic_include_console_print) {
    auto result = compile(
        "#include <engine/console> { Print }\n"
        "void test() { Print(\"hello\"); }\n"
    );
    
    EXPECT_TRUE(result.success);
    
    // Verify Print is in symbol table
    auto* printSym = result.analyzer.symbolTable().lookup("Print");
    EXPECT_TRUE(printSym != nullptr);
    EXPECT_EQ(printSym->kind, SymbolKind::BuiltinFunc);
}

TEST(semantic_include_all_exports) {
    auto result = compile(
        "#include <engine/console>\n"
        "void test() { Log(\"test\"); }\n"
    );
    
    EXPECT_TRUE(result.success);
    
    // Should import all: Print, Log, Warn, Error
    EXPECT_TRUE(result.analyzer.symbolTable().lookup("Print") != nullptr);
    EXPECT_TRUE(result.analyzer.symbolTable().lookup("Log") != nullptr);
    EXPECT_TRUE(result.analyzer.symbolTable().lookup("Warn") != nullptr);
    EXPECT_TRUE(result.analyzer.symbolTable().lookup("Error") != nullptr);
}

TEST(semantic_include_unknown_header) {
    auto result = compile("#include <nonexistent/header>");
    
    EXPECT_FALSE(result.success);
    EXPECT_TRUE(result.analyzer.diagnostics().errorCount() > 0);
}

TEST(semantic_include_unknown_export) {
    auto result = compile("#include <engine/console> { NonExistent }");
    
    EXPECT_FALSE(result.success);
    EXPECT_TRUE(result.analyzer.diagnostics().errorCount() > 0);
}

TEST(semantic_include_time_constants) {
    auto result = compile(
        "#include <engine/time>\n"
        "void update() {\n"
        "    var dt = DeltaTime;\n"
        "}\n"
    );
    
    EXPECT_TRUE(result.success);
    
    auto* dtSym = result.analyzer.symbolTable().lookup("DeltaTime");
    EXPECT_TRUE(dtSym != nullptr);
    EXPECT_EQ(dtSym->kind, SymbolKind::Variable);
}

// ============================================================
// TYPE INFERENCE TESTS
// ============================================================

TEST(semantic_literal_type_inference) {
    auto result = compile(
        "void test() {\n"
        "    var a = 42;\n"
        "    var b = 3.14;\n"
        "    var c = true;\n"
        "    var d = \"hello\";\n"
        "}\n"
    );
    
    EXPECT_TRUE(result.success);
}

TEST(semantic_binary_expression_types) {
    auto result = compile(
        "void test() {\n"
        "    var x = 10 + 20;\n"
        "    var y = 3.14 * 2.0;\n"
        "    var z = true && false;\n"
        "}\n"
    );
    
    EXPECT_TRUE(result.success);
}

TEST(semantic_type_mismatch_error) {
    auto result = compile(
        "void test() {\n"
        "    int32 x = \"string

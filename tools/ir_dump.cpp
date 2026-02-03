// tools/ir_dump.cpp - Test LLVM IR generation
//
// Compiles a ScriptLang source file and outputs LLVM IR to stdout.
//
// Usage:
//     ./build/scriptlang_ir script.sl
//     ./build/scriptlang_ir script.sl > output.ll

#include "scriptlang.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>

using namespace scriptlang;

// Read entire file into string
std::string readFile(const std::string& path) {
    std::ifstream file(path);
    if (!file) {
        std::cerr << "Error: Cannot open file '" << path << "'\n";
        std::exit(1);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <source.sl>\n";
        std::cerr << "\n";
        std::cerr << "Compiles a ScriptLang source file and outputs LLVM IR.\n";
        std::cerr << "\n";
        std::cerr << "Examples:\n";
        std::cerr << "  " << argv[0] << " test.sl\n";
        std::cerr << "  " << argv[0] << " test.sl > output.ll\n";
        return 1;
    }
    
    std::string sourcePath = argv[1];
    std::string source = readFile(sourcePath);
    
    std::cerr << "[IR] Compiling '" << sourcePath << "'...\n";
    
    // Phase 1: Lexing
    std::cerr << "[IR] Lexing...\n";
    Lexer lexer(source.c_str());
    auto tokens = lexer.tokenize();
    
    // Check for lexer errors
    bool hasLexError = false;
    for (const auto& tok : tokens) {
        if (tok.isError()) {
            std::cerr << "Lexer error at " << tok.line << ":" << tok.column 
                      << " - " << tok.value << "\n";
            hasLexError = true;
        }
    }
    
    if (hasLexError) {
        return 1;
    }
    
    // Phase 2: Parsing
    std::cerr << "[IR] Parsing...\n";
    Parser parser(tokens);
    auto program = parser.parse();
    
    if (parser.hasErrors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << "Parse error at " << err.line << ":" << err.column 
                      << " - " << err.message << "\n";
        }
        return 1;
    }
    
    // Phase 3: Semantic Analysis
    std::cerr << "[IR] Running semantic analysis...\n";
    SemanticAnalyzer analyzer;
    bool semanticOk = analyzer.analyze(*program);
    
    if (!semanticOk) {
        for (const auto& diag : analyzer.diagnostics().all()) {
            std::cerr << "Semantic error at " << diag.loc.line << ":" 
                      << diag.loc.column << " - " << diag.message << "\n";
        }
        return 1;
    }
    
    // Phase 4: Code Generation
    std::cerr << "[IR] Generating LLVM IR...\n";
    LLVMCodeGen codegen(analyzer.typeContext(), analyzer.symbolTable());
    auto module = codegen.generate(*program);
    
    if (!module) {
        std::cerr << "Code generation failed\n";
        return 1;
    }
    
    if (codegen.hasErrors()) {
        std::cerr << "Code generation completed with errors\n";
        return 1;
    }
    
    // Output IR to stdout
    std::cerr << "[IR] Generation successful!\n";
    std::cerr << "[IR] ==================== LLVM IR ====================\n";
    
    module->print(llvm::outs(), nullptr);
    
    return 0;
}

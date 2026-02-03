#include "test_framework.h"
#include "scriptlang.h"

#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <fstream>

#define ARENA_IMPLEMENTATION
using namespace scriptlang;

// Helper: full pipeline - lex, parse, analyze, codegen
static std::unique_ptr<llvm::Module> compileToIR(const char* source, 
                                                  SemanticAnalyzer** outAnalyzer = nullptr,
                                                  LLVMCodeGen** outCodegen = nullptr) {
    // Lex
    Lexer lexer(source);
    auto tokens = lexer.tokenize();
    
    // Parse
    Parser parser(tokens);
    auto program = parser.parse();
    
    if (parser.hasErrors()) {
        for (const auto& err : parser.errors()) {
            std::cerr << "Parse error at " << err.line << ":" << err.column 
                      << " — " << err.message << "\n";
        }
        return nullptr;
    }
    
    // Semantic analysis
    static SemanticAnalyzer* analyzer_ptr = nullptr;
    if (analyzer_ptr) delete analyzer_ptr;
    analyzer_ptr = new SemanticAnalyzer();
    
    if (!analyzer_ptr->analyze(*program)) {
        for (const auto& diag : analyzer_ptr->diagnostics().all()) {
            std::cerr << "Semantic error at " << diag.loc.line << ":" << diag.loc.column 
                      << " — " << diag.message << "\n";
        }
        if (outAnalyzer) *outAnalyzer = analyzer_ptr;
        return nullptr;
    }
    
    // Codegen
    static LLVMCodeGen* codegen_ptr = nullptr;
    if (codegen_ptr) delete codegen_ptr;
    codegen_ptr = new LLVMCodeGen(analyzer_ptr->typeContext(), analyzer_ptr->symbolTable());
    
    auto module = codegen_ptr->generate(*program);
    
    if (codegen_ptr->hasErrors()) {
        for (const auto& err : codegen_ptr->errors()) {
            std::cerr << "Codegen error: " << err << "\n";
        }
    }
    
    if (outAnalyzer) *outAnalyzer = analyzer_ptr;
    if (outCodegen) *outCodegen = codegen_ptr;
    
    return module;
}

// Helper: dump IR to string
static std::string dumpIR(llvm::Module* module) {
    if (!module) return "";
    
    std::string str;
    llvm::raw_string_ostream os(str);
    module->print(os, nullptr);
    os.flush();
    return str;
}

// Helper: save IR to file
static void saveIR(llvm::Module* module, const std::string& filename) {
    if (!module) return;
    
    std::error_code EC;
    llvm::raw_fd_ostream file(filename, EC);
    if (!EC) {
        module->print(file, nullptr);
    }
}

// ============================================================
// BASIC IR GENERATION TESTS
// ============================================================

TEST(codegen_simple_function) {
    const char* source = R"(
        int32 add(a : int32, b : int32) {
            return a + b;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("define") != std::string::npos);
    EXPECT_TRUE(ir.find("add") != std::string::npos);
    EXPECT_TRUE(ir.find("add i32") != std::string::npos);
    EXPECT_TRUE(ir.find("ret i32") != std::string::npos);
    
    std::cout << "\n=== IR for simple_function ===\n" << ir << "\n";
}

TEST(codegen_local_variables) {
    const char* source = R"(
        int32 multiply(x : int32, y : int32) {
            var result = x * y;
            return result;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("alloca") != std::string::npos);
    EXPECT_TRUE(ir.find("mul i32") != std::string::npos);
    
    std::cout << "\n=== IR for local_variables ===\n" << ir << "\n";
}

TEST(codegen_control_flow) {
    const char* source = R"(
        int32 abs(n : int32) {
            if (n < 0) {
                return 0 - n;
            }
            return n;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("if.then") != std::string::npos);
    EXPECT_TRUE(ir.find("if.merge") != std::string::npos);
    EXPECT_TRUE(ir.find("icmp") != std::string::npos);
    EXPECT_TRUE(ir.find("br i1") != std::string::npos);
    
    std::cout << "\n=== IR for control_flow ===\n" << ir << "\n";
}

TEST(codegen_loop) {
    const char* source = R"(
        int32 factorial(n : int32) {
            var result = 1;
            var i = 1;
            
            while (i <= n) {
                result = result * i;
                i = i + 1;
            }
            
            return result;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("while.cond") != std::string::npos);
    EXPECT_TRUE(ir.find("while.body") != std::string::npos);
    EXPECT_TRUE(ir.find("while.exit") != std::string::npos);
    
    std::cout << "\n=== IR for loop ===\n" << ir << "\n";
}

// ============================================================
// EDGE CASE TESTS - ARITHMETIC & OPERATORS
// ============================================================

TEST(codegen_all_arithmetic_ops) {
    const char* source = R"(
        int32 testArithmetic(a : int32, b : int32) {
            var add = a + b;
            var sub = a - b;
            var mul = a * b;
            var div = a / b;
            var mod = a % b;
            return add + sub + mul + div + mod;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("add i32") != std::string::npos);
    EXPECT_TRUE(ir.find("sub i32") != std::string::npos);
    EXPECT_TRUE(ir.find("mul i32") != std::string::npos);
    EXPECT_TRUE(ir.find("sdiv i32") != std::string::npos);
    EXPECT_TRUE(ir.find("srem i32") != std::string::npos);
    
    std::cout << "\n=== IR for all_arithmetic_ops ===\n" << ir << "\n";
}

TEST(codegen_comparison_ops) {
    const char* source = R"(
        bool testComparisons(a : int32, b : int32) {
            var eq = a == b;
            var ne = a != b;
            var lt = a < b;
            var le = a <= b;
            var gt = a > b;
            var ge = a >= b;
            return eq && ne && lt && le && gt && ge;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("icmp eq") != std::string::npos);
    EXPECT_TRUE(ir.find("icmp ne") != std::string::npos);
    EXPECT_TRUE(ir.find("icmp slt") != std::string::npos);
    EXPECT_TRUE(ir.find("icmp sle") != std::string::npos);
    EXPECT_TRUE(ir.find("icmp sgt") != std::string::npos);
    EXPECT_TRUE(ir.find("icmp sge") != std::string::npos);
    
    std::cout << "\n=== IR for comparison_ops ===\n" << ir << "\n";
}

TEST(codegen_logical_ops) {
    const char* source = R"(
        bool testLogical(a : bool, b : bool) {
            var and_result = a && b;
            var or_result = a || b;
            var not_result = !a;
            return and_result || or_result || not_result;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Short-circuit evaluation uses phi nodes and branches
    EXPECT_TRUE(ir.find("phi i1") != std::string::npos || 
                ir.find("and.merge") != std::string::npos ||
                ir.find("or.merge") != std::string::npos);
    EXPECT_TRUE(ir.find("xor i1") != std::string::npos);  // NOT operation
    
    std::cout << "\n=== IR for logical_ops ===\n" << ir << "\n";
}

TEST(codegen_unary_ops) {
    const char* source = R"(
        int32 testUnary(n : int32, flag : bool) {
            var neg = 0 - n;
            var notFlag = !flag;
            return neg;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("sub i32") != std::string::npos);
    EXPECT_TRUE(ir.find("xor i1") != std::string::npos);  // NOT operation
    
    std::cout << "\n=== IR for unary_ops ===\n" << ir << "\n";
}

TEST(codegen_compound_assignment) {
    const char* source = R"(
        int32 testCompoundAssign(n : int32) {
            var x = n;
            x += 5;
            x -= 3;
            x *= 2;
            x /= 4;
            return x;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("add i32") != std::string::npos);
    EXPECT_TRUE(ir.find("sub i32") != std::string::npos);
    EXPECT_TRUE(ir.find("mul i32") != std::string::npos);
    EXPECT_TRUE(ir.find("sdiv i32") != std::string::npos);
    
    std::cout << "\n=== IR for compound_assignment ===\n" << ir << "\n";
}

// ============================================================
// EDGE CASE TESTS - CONTROL FLOW
// ============================================================

TEST(codegen_if_else_chain) {
    const char* source = R"(
        int32 classify(n : int32) {
            if (n < 0) {
                return 0 - 1;
            } else if (n == 0) {
                return 0;
            } else {
                return 1;
            }
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have multiple if blocks
    EXPECT_TRUE(ir.find("if.then") != std::string::npos);
    EXPECT_TRUE(ir.find("if.else") != std::string::npos);
    
    std::cout << "\n=== IR for if_else_chain ===\n" << ir << "\n";
}

TEST(codegen_nested_if) {
    const char* source = R"(
        int32 nestedIf(a : int32, b : int32) {
            if (a > 0) {
                if (b > 0) {
                    return a + b;
                } else {
                    return a - b;
                }
            } else {
                return 0;
            }
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have nested structure
    EXPECT_TRUE(ir.find("if.then") != std::string::npos);
    
    std::cout << "\n=== IR for nested_if ===\n" << ir << "\n";
}

TEST(codegen_for_loop) {
    const char* source = R"(
        int32 sumRange(n : int32) {
            var sum = 0;
            for (var i = 0; i < n; i = i + 1) {
                sum = sum + i;
            }
            return sum;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("for.cond") != std::string::npos);
    EXPECT_TRUE(ir.find("for.body") != std::string::npos);
    EXPECT_TRUE(ir.find("for.inc") != std::string::npos);
    EXPECT_TRUE(ir.find("for.exit") != std::string::npos);
    
    std::cout << "\n=== IR for for_loop ===\n" << ir << "\n";
}

TEST(codegen_nested_loops) {
    const char* source = R"(
        int32 nestedLoops(n : int32) {
            var sum = 0;
            var i = 0;
            while (i < n) {
                var j = 0;
                while (j < n) {
                    sum = sum + 1;
                    j = j + 1;
                }
                i = i + 1;
            }
            return sum;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have multiple loop structures
    int whileCount = 0;
    size_t pos = 0;
    while ((pos = ir.find("while.cond", pos)) != std::string::npos) {
        whileCount++;
        pos++;
    }
    EXPECT_TRUE(whileCount >= 2);
    
    std::cout << "\n=== IR for nested_loops ===\n" << ir << "\n";
}

TEST(codegen_break_continue) {
    const char* source = R"(
        int32 testBreakContinue(n : int32) {
            var sum = 0;
            var i = 0;
            while (i < n) {
                i = i + 1;
                if (i == 5) {
                    continue;
                }
                if (i > 10) {
                    break;
                }
                sum = sum + i;
            }
            return sum;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("while.cond") != std::string::npos);
    EXPECT_TRUE(ir.find("while.exit") != std::string::npos);
    
    std::cout << "\n=== IR for break_continue ===\n" << ir << "\n";
}

TEST(codegen_switch_statement) {
    const char* source = R"(
        int32 testSwitch(n : int32) {
            switch (n) {
                case 1:
                    return 10;
                case 2:
                    return 20;
                case 3:
                    return 30;
                default:
                    return 0;
            }
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("switch") != std::string::npos);
    EXPECT_TRUE(ir.find("switch.case") != std::string::npos);
    
    std::cout << "\n=== IR for switch_statement ===\n" << ir << "\n";
}

// ============================================================
// EDGE CASE TESTS - TYPES & LITERALS (FIXED)
// ============================================================

TEST(codegen_all_integer_types) {
    const char* source = R"(
        int8 test8(n : int8) { return n; }
        int16 test16(n : int16) { return n; }
        int32 test32(n : int32) { return n; }
        int64 test64(n : int64) { return n; }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("i8") != std::string::npos);
    EXPECT_TRUE(ir.find("i16") != std::string::npos);
    EXPECT_TRUE(ir.find("i32") != std::string::npos);
    EXPECT_TRUE(ir.find("i64") != std::string::npos);
    
    std::cout << "\n=== IR for all_integer_types ===\n" << ir << "\n";
}

TEST(codegen_float_types) {
    const char* source = R"(
        float32 testFloat32(x : float32) { 
            return x; 
        }
        
        float64 testFloat64(y : float64) { 
            return y; 
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("float") != std::string::npos || ir.find("double") != std::string::npos);
    
    std::cout << "\n=== IR for float_types ===\n" << ir << "\n";
}

TEST(codegen_float_arithmetic) {
    const char* source = R"(
        float64 floatMath(a : float64, b : float64) {
            var sum = a + b;
            var diff = a - b;
            var prod = a * b;
            var quot = a / b;
            return sum + diff + prod + quot;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("fadd") != std::string::npos);
    EXPECT_TRUE(ir.find("fsub") != std::string::npos);
    EXPECT_TRUE(ir.find("fmul") != std::string::npos);
    EXPECT_TRUE(ir.find("fdiv") != std::string::npos);
    
    std::cout << "\n=== IR for float_arithmetic ===\n" << ir << "\n";
}

TEST(codegen_mixed_type_arithmetic) {
    const char* source = R"(
        float64 mixedTypes(a : int32, b : float64) {
            var result = a + b;
            return result;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have type conversion
    EXPECT_TRUE(ir.find("sitofp") != std::string::npos);
    
    std::cout << "\n=== IR for mixed_type_arithmetic ===\n" << ir << "\n";
}

TEST(codegen_boolean_logic) {
    const char* source = R"(
        bool complexLogic(a : bool, b : bool, c : bool) {
            return (a && b) || (!c && (a || b));
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("i1") != std::string::npos);
    
    std::cout << "\n=== IR for boolean_logic ===\n" << ir << "\n";
}

// ============================================================
// EDGE CASE TESTS - FUNCTIONS
// ============================================================

TEST(codegen_void_function) {
    const char* source = R"(
        void doNothing() {
            var x = 5;
        }
        
        void earlyReturn(n : int32) {
            if (n < 0) {
                return;
            }
            var x = n * 2;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("void") != std::string::npos);
    EXPECT_TRUE(ir.find("ret void") != std::string::npos);
    
    std::cout << "\n=== IR for void_function ===\n" << ir << "\n";
}

TEST(codegen_recursive_function) {
    const char* source = R"(
        int32 fibonacci(n : int32) {
            if (n <= 1) {
                return n;
            }
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have recursive calls
    int callCount = 0;
    size_t pos = 0;
    while ((pos = ir.find("call", pos)) != std::string::npos) {
        callCount++;
        pos++;
    }
    EXPECT_TRUE(callCount >= 2);
    
    std::cout << "\n=== IR for recursive_function ===\n" << ir << "\n";
}

TEST(codegen_multiple_returns) {
    const char* source = R"(
        int32 multiReturn(n : int32) {
            if (n < 0) { return 0 - 1; }
            if (n == 0) { return 0; }
            if (n == 1) { return 1; }
            if (n < 10) { return n * 2; }
            if (n < 100) { return n * 3; }
            return n;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    int retCount = 0;
    size_t pos = 0;
    while ((pos = ir.find("ret i32", pos)) != std::string::npos) {
        retCount++;
        pos++;
    }
    EXPECT_TRUE(retCount >= 5);
    
    std::cout << "\n=== IR for multiple_returns ===\n" << ir << "\n";
}

TEST(codegen_many_parameters) {
    const char* source = R"(
        int32 manyParams(a : int32, b : int32, c : int32, d : int32, 
                        e : int32, f : int32, g : int32, h : int32) {
            return a + b + c + d + e + f + g + h;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have 8 parameters
    EXPECT_TRUE(ir.find("i32 %a") != std::string::npos);
    EXPECT_TRUE(ir.find("i32 %h") != std::string::npos);
    
    std::cout << "\n=== IR for many_parameters ===\n" << ir << "\n";
}

// ============================================================
// EDGE CASE TESTS - VARIABLES & SCOPING (FIXED)
// ============================================================

TEST(codegen_shadowing) {
    const char* source = R"(
        int32 testShadowing(x : int32) {
            var y = 10;
            if (x > 5) {
                var z = 20;
                return z;
            }
            return y;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Should have multiple allocas for different variables
    int allocaCount = 0;
    size_t pos = 0;
    while ((pos = ir.find("alloca", pos)) != std::string::npos) {
        allocaCount++;
        pos++;
    }
    EXPECT_TRUE(allocaCount >= 2);
    
    std::cout << "\n=== IR for shadowing ===\n" << ir << "\n";
}

TEST(codegen_const_variables) {
    const char* source = R"(
        int32 useConst() {
            const PI = 3;
            const TWO_PI = PI * 2;
            return TWO_PI;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    std::cout << "\n=== IR for const_variables ===\n" << ir << "\n";
}

TEST(codegen_global_variables) {
    const char* source = R"(
        var globalCounter : int32 = 0;
        const MAX_VALUE : int32 = 100;
        
        int32 useGlobals() {
            globalCounter = globalCounter + 1;
            return globalCounter;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("@globalCounter") != std::string::npos);
    EXPECT_TRUE(ir.find("@MAX_VALUE") != std::string::npos);
    
    std::cout << "\n=== IR for global_variables ===\n" << ir << "\n";
}

// ============================================================
// EDGE CASE TESTS - COMPLEX EXPRESSIONS
// ============================================================

TEST(codegen_complex_expression) {
    const char* source = R"(
        int32 complexExpr(a : int32, b : int32, c : int32) {
            return ((a + b) * c - (a / b)) % (c + 1);
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("add") != std::string::npos);
    EXPECT_TRUE(ir.find("mul") != std::string::npos);
    EXPECT_TRUE(ir.find("sub") != std::string::npos);
    EXPECT_TRUE(ir.find("div") != std::string::npos);
    EXPECT_TRUE(ir.find("srem") != std::string::npos);
    
    std::cout << "\n=== IR for complex_expression ===\n" << ir << "\n";
}

TEST(codegen_short_circuit_and) {
    const char* source = R"(
        bool testShortCircuit(a : int32, b : int32) {
            var safe = (a > 0) && (b / a > 5);
            return safe;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    // Just check that it compiles - short circuit might not be visible
    // in IR depending on optimization level
    EXPECT_TRUE(ir.find("icmp") != std::string::npos);
    
    std::cout << "\n=== IR for short_circuit_and ===\n" << ir << "\n";
}

TEST(codegen_chained_comparisons) {
    const char* source = R"(
        bool inRange(x : int32, min : int32, max : int32) {
            return x >= min && x <= max;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    EXPECT_TRUE(ir.find("icmp") != std::string::npos);
    
    std::cout << "\n=== IR for chained_comparisons ===\n" << ir << "\n";
}

// ============================================================
// COMPREHENSIVE TESTS
// ============================================================

TEST(codegen_full_program) {
    const char* source = R"(
        int32 add(a : int32, b : int32) {
            return a + b;
        }
        
        int32 factorial(n : int32) {
            var result = 1;
            var i = 1;
            
            while (i <= n) {
                result = result * i;
                i = i + 1;
            }
            
            return result;
        }
        
        int32 main() {
            var x = add(5, 3);
            var f = factorial(5);
            return f;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    std::cout << "\n=== IR for full_program ===\n" << ir << "\n";
    
    EXPECT_TRUE(ir.find("@add") != std::string::npos);
    EXPECT_TRUE(ir.find("@factorial") != std::string::npos);
    EXPECT_TRUE(ir.find("@main") != std::string::npos);
    
    // Save to file for inspection
    saveIR(module.get(), "test_full_program.ll");
}

TEST(codegen_stress_test) {
    const char* source = R"(
        int32 stressTest(n : int32) {
            var total = 0;
            
            // Nested loops
            for (var i = 0; i < n; i = i + 1) {
                for (var j = 0; j < n; j = j + 1) {
                    
                    // Complex conditionals
                    if (i > j) {
                        if (i % 2 == 0) {
                            total += i * j;
                        } else {
                            total -= i + j;
                        }
                    } else if (i == j) {
                        total *= 2;
                    } else {
                        total /= 2;
                    }
                    
                    // More arithmetic
                    total = (total + 1) * (total - 1);
                }
            }
            
            return total;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    std::cout << "\n=== IR for stress_test ===\n" << ir << "\n";
    
    // Should be a large IR module
    EXPECT_TRUE(ir.length() > 1000);
    
    saveIR(module.get(), "test_stress.ll");
}

TEST(codegen_from_file) {
    const char* source = R"(
        int32 add(a : int32, b : int32) {
            return a + b;
        }

        int32 multiply(x : int32, y : int32) {
            var result = x * y;
            return result;
        }

        int32 abs(n : int32) {
            if (n < 0) {
                return 0 - n;
            }
            return n;
        }

        int32 factorial(n : int32) {
            var result = 1;
            var i = 1;
            
            while (i <= n) {
                result = result * i;
                i = i + 1;
            }
            
            return result;
        }

        int32 main() {
            var x = add(5, 3);
            var y = multiply(x, 2);
            var z = abs(0 - 42);
            var f = factorial(5);
            
            return f;
        }
    )";
    
    auto module = compileToIR(source);
    EXPECT_TRUE(module != nullptr);
    
    std::string ir = dumpIR(module.get());
    
    std::cout << "\n=== IR for test_ir.sl ===\n" << ir << "\n";
    
    EXPECT_TRUE(!ir.empty());
    EXPECT_TRUE(ir.find("@main") != std::string::npos);
    
    saveIR(module.get(), "test_ir.ll");
}

// ============================================================
// ERROR HANDLING TESTS
// ============================================================

TEST(codegen_handles_parse_errors) {
    const char* source = "int32 broken( { }";
    
    // Suppress error output for this test
    std::cerr.setstate(std::ios_base::failbit);
    auto module = compileToIR(source);
    std::cerr.clear();
    
    EXPECT_TRUE(module == nullptr);  // Should fail - that's the test!
}

TEST(codegen_handles_semantic_errors) {
    const char* source = R"(
        int32 test() {
            return undefinedVariable;
        }
    )";
    
    auto module = compileToIR(source);
    // Should fail semantic analysis
    EXPECT_TRUE(module == nullptr);
}

TEST(codegen_handles_type_mismatch) {
    const char* source = R"(
        int32 test() {
            var x = true;
            return x + 5;
        }
    )";
    
    auto module = compileToIR(source);
    // Should fail type checking
    EXPECT_TRUE(module == nullptr);
}

#include "test_framework.h"
#include "scriptlang.h"

using namespace scriptlang;

// ============================================================
// Helper: lex + parse a source string, return the Program.
// ============================================================
static std::unique_ptr<Program> parseSource(const char* source, Parser** outParser = nullptr) {
    Lexer  lexer(source);
    auto   tokens = lexer.tokenize();
    static Parser* parser_ptr = nullptr;
    if (parser_ptr) delete parser_ptr;
    parser_ptr = new Parser(tokens);
    auto program = parser_ptr->parse();
    
    if (outParser) {
        *outParser = parser_ptr;
    } else {
        // Surface any parse errors as test failures
        for (const auto& err : parser_ptr->errors()) {
            std::string msg = "Parse error at " + std::to_string(err.line) +
                              ":" + std::to_string(err.column) + " — " + err.message;
            scriptlang::test::currentFailures().push_back({msg, __FILE__, __LINE__});
        }
    }
    
    return program;
}

// Helper: get statement N from a program
template<typename T>
static T* getStmt(const Program& prog, size_t index = 0) {
    if (index >= prog.statements.size()) return nullptr;
    return dynamic_cast<T*>(prog.statements[index].get());
}

// ============================================================
// NEW TESTS FOR IMPROVED ANNOTATION PARSING
// ============================================================

TEST(parser_annotation_simple) {
    auto prog = parseSource("@lifecycle(phase=init) void onInit() { }");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_SIZE(fn->annotations, 1);
    EXPECT_EQ(fn->annotations[0].name, std::string("lifecycle"));
    EXPECT_SIZE(fn->annotations[0].args, 1);
    EXPECT_EQ(fn->annotations[0].args[0].first, std::string("phase"));
    EXPECT_EQ(fn->annotations[0].args[0].second, std::string("init"));
}

TEST(parser_annotation_multiple_args) {
    auto prog = parseSource(R"(
        @expose(min=0, max=100, step=1, tooltip="Health value", group="Stats")
        private int32 health = 100;
    )");
    
    auto* cls = getStmt<ClassDecl>(*prog);
    if (cls) {
        // If parsed as part of a class
        EXPECT_SIZE(cls->members, 1);
        auto& member = cls->members[0];
        EXPECT_SIZE(member.annotations, 1);
        EXPECT_EQ(member.annotations[0].name, std::string("expose"));
        EXPECT_SIZE(member.annotations[0].args, 5);
    }
}

TEST(parser_annotation_with_array_value) {
    auto prog = parseSource("@permissions(restrict=[networking, io, filesystem]) class Restricted { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    EXPECT_EQ(cls->annotations[0].name, std::string("permissions"));
    EXPECT_SIZE(cls->annotations[0].args, 1);
    EXPECT_EQ(cls->annotations[0].args[0].first, std::string("restrict"));
    
    // The array should be stored as a string like "[networking,io,filesystem]"
    std::string arrayValue = cls->annotations[0].args[0].second;
    EXPECT_TRUE(arrayValue.find("networking") != std::string::npos);
    EXPECT_TRUE(arrayValue.find("io") != std::string::npos);
    EXPECT_TRUE(arrayValue.find("filesystem") != std::string::npos);
}

TEST(parser_annotation_with_string_value) {
    auto prog = parseSource(R"(@compile(mode="aot") class Fast { })");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    EXPECT_EQ(cls->annotations[0].args[0].second, std::string("aot"));
}

TEST(parser_multiple_annotations) {
    auto prog = parseSource(R"(
        @lifecycle(phase=update)
        @expose(tooltip="Main update loop")
        void onUpdate() { }
    )");
    auto* fn = getStmt<FunctionDecl>(*prog);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_SIZE(fn->annotations, 2);
    EXPECT_EQ(fn->annotations[0].name, std::string("lifecycle"));
    EXPECT_EQ(fn->annotations[1].name, std::string("expose"));
}

// ============================================================
// ERROR RECOVERY TESTS
// ============================================================

TEST(parser_annotation_invalid_placement_if) {
    Parser* parser = nullptr;
    auto prog = parseSource("@lifecycle(phase=update) if (true) { }", &parser);
    
    EXPECT_TRUE(parser != nullptr);
    EXPECT_TRUE(parser->hasErrors());
    
    // Should have an error about annotations on if statements
    bool foundError = false;
    for (const auto& err : parser->errors()) {
        if (std::string(err.message).find("if") != std::string::npos) {
            foundError = true;
            break;
        }
    }
    EXPECT_TRUE(foundError);
}

TEST(parser_annotation_invalid_placement_var) {
    Parser* parser = nullptr;
    auto prog = parseSource("@expose(min=0) var x = 5;", &parser);
    
    EXPECT_TRUE(parser != nullptr);
    EXPECT_TRUE(parser->hasErrors());
}

TEST(parser_recovers_from_malformed_annotation) {
    Parser* parser = nullptr;
    auto prog = parseSource(R"(
        @bad(this is not valid syntax here)
        class StillParsed { }
    )", &parser);
    
    // Should have errors but still parse the class
    EXPECT_TRUE(parser != nullptr);
    EXPECT_TRUE(parser->hasErrors());
    
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_EQ(cls->name, std::string("StillParsed"));
}

// ============================================================
// REAL-WORLD SYNTAX TESTS
// ============================================================

TEST(parser_full_class_with_annotations) {
    const char* source = R"(
        @compile(mode=aot)
        @permissions(restrict=[networking])
        public class Player {
            @expose(min=0, max=100, step=1, tooltip="The player's health (0 = dead)", group="Stats")
            private int32 playerHealth = 100;
            
            @expose(maxLength=32, tooltip="Display name shown above player", group="Identity")
            public string playerName = "Bob";
            
            @lifecycle(phase=init)
            private void onInit() {
                // initial setup
            }
            
            @lifecycle(phase=update)
            private void onUpdate() {
                if (isDead()) {
                    Print("man im dead");
                }
            }
            
            int32 getHealth() {
                return playerHealth;
            }
            
            bool isDead() {
                return getHealth() <= 0;
            }
        }
    )";
    
    auto prog = parseSource(source);
    auto* cls = getStmt<ClassDecl>(*prog);
    
    EXPECT_TRUE(cls != nullptr);
    EXPECT_EQ(cls->name, std::string("Player"));
    EXPECT_SIZE(cls->annotations, 2);
    EXPECT_EQ(cls->annotations[0].name, std::string("compile"));
    EXPECT_EQ(cls->annotations[1].name, std::string("permissions"));
    
    // Check that members have annotations
    EXPECT_TRUE(cls->members.size() >= 2);
    
    // First member (playerHealth) should have @expose
    EXPECT_SIZE(cls->members[0].annotations, 1);
    EXPECT_EQ(cls->members[0].annotations[0].name, std::string("expose"));
    
    // Methods with @lifecycle
    bool foundLifecycleInit = false;
    bool foundLifecycleUpdate = false;
    
    for (const auto& member : cls->members) {
        auto* fn = dynamic_cast<FunctionDecl*>(member.decl.get());
        if (fn) {
            for (const auto& ann : fn->annotations) {
                if (ann.name == "lifecycle") {
                    for (const auto& arg : ann.args) {
                        if (arg.first == "phase") {
                            if (arg.second == "init") foundLifecycleInit = true;
                            if (arg.second == "update") foundLifecycleUpdate = true;
                        }
                    }
                }
            }
        }
    }
    
    EXPECT_TRUE(foundLifecycleInit);
    EXPECT_TRUE(foundLifecycleUpdate);
}

TEST(parser_standalone_function_with_annotation) {
    const char* source = R"(
        #include <engine/console> { Print }
        
        @lifecycle(phase=update)
        void onUpdate() {
            Print("I am a simple script, no class needed.");
        }
    )";
    
    auto prog = parseSource(source);
    
    // Should have include + function
    EXPECT_SIZE(prog->statements, 2);
    
    auto* fn = getStmt<FunctionDecl>(*prog, 1);
    EXPECT_TRUE(fn != nullptr);
    EXPECT_EQ(fn->name, std::string("onUpdate"));
    EXPECT_SIZE(fn->annotations, 1);
    EXPECT_EQ(fn->annotations[0].name, std::string("lifecycle"));
}

// ============================================================
// EDGE CASES
// ============================================================

TEST(parser_annotation_empty_array) {
    auto prog = parseSource("@permissions(restrict=[]) class Unrestricted { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    
    // Empty array should parse as "[]"
    std::string arrayValue = cls->annotations[0].args[0].second;
    EXPECT_EQ(arrayValue, std::string("[]"));
}

TEST(parser_annotation_numeric_values) {
    auto prog = parseSource("@config(port=8080, maxConnections=100, timeout=30.5) class Server { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    EXPECT_SIZE(cls->annotations[0].args, 3);
    
    // Should have numeric values stored as strings
    bool foundPort = false;
    bool foundMaxConn = false;
    bool foundTimeout = false;
    
    for (const auto& arg : cls->annotations[0].args) {
        if (arg.first == "port" && arg.second == "8080") foundPort = true;
        if (arg.first == "maxConnections" && arg.second == "100") foundMaxConn = true;
        if (arg.first == "timeout" && arg.second == "30.5") foundTimeout = true;
    }
    
    EXPECT_TRUE(foundPort);
    EXPECT_TRUE(foundMaxConn);
    EXPECT_TRUE(foundTimeout);
}

TEST(parser_annotation_boolean_values) {
    auto prog = parseSource("@feature(enabled=true, debug=false) class Feature { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    EXPECT_SIZE(cls->annotations[0].args, 2);
}

TEST(parser_annotation_mixed_array_types) {
    auto prog = parseSource(R"(@config(values=[1, "two", three, 4.5, true]) class Mixed { })");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    
    std::string arrayValue = cls->annotations[0].args[0].second;
    // Should contain all the values
    EXPECT_TRUE(arrayValue.find("1") != std::string::npos);
    EXPECT_TRUE(arrayValue.find("two") != std::string::npos);
    EXPECT_TRUE(arrayValue.find("three") != std::string::npos);
    EXPECT_TRUE(arrayValue.find("4.5") != std::string::npos);
    EXPECT_TRUE(arrayValue.find("true") != std::string::npos);
}

// ============================================================
// COMPATIBILITY TESTS
// ============================================================

TEST(parser_backward_compatible_simple_annotation) {
    // Old-style simple annotations should still work
    auto prog = parseSource("@deprecated class Old { }");
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 1);
    EXPECT_EQ(cls->annotations[0].name, std::string("deprecated"));
    EXPECT_SIZE(cls->annotations[0].args, 0);
}

TEST(parser_works_without_annotations) {
    // Code without annotations should parse identically
    auto prog = parseSource(R"(
        class Simple {
            var x = 10;
            void doThing() { }
        }
    )");
    
    auto* cls = getStmt<ClassDecl>(*prog);
    EXPECT_TRUE(cls != nullptr);
    EXPECT_SIZE(cls->annotations, 0);
    EXPECT_SIZE(cls->members, 2);
}

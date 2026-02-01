#ifndef TEST_FRAMEWORK_H
#define TEST_FRAMEWORK_H

// ------------------------------------------------------------
// test_framework.h — minimal test framework.
//
// Usage in a test file:
//     #include "test_framework.h"
//     #include "scriptlang.h"  // the amalgamated header
//
//     TEST(lexer_scans_integer) {
//         // ... test body ...
//         EXPECT_EQ(tokens[0].type, scriptlang::TokenType::Integer);
//     }
//
// Tests are auto-registered. Just define them and they run.
// ------------------------------------------------------------

#include <string>
#include <vector>
#include <functional>
#include <iostream>
#include <sstream>

namespace scriptlang::test {

// --------------------------------------------------------
// Internal: test registry
// --------------------------------------------------------
struct TestCase {
    std::string              name;
    std::function<void()>    body;
};

struct TestFailure {
    std::string message;
    std::string file;
    int         line;
};

// Thread-local failure list — cleared before each test
inline std::vector<TestFailure>& currentFailures() {
    static thread_local std::vector<TestFailure> failures;
    return failures;
}

// Global test registry
inline std::vector<TestCase>& registry() {
    static std::vector<TestCase> tests;
    return tests;
}

// Auto-registration helper
struct TestRegistrar {
    TestRegistrar(const std::string& name, std::function<void()> body) {
        registry().push_back({name, std::move(body)});
    }
};

// --------------------------------------------------------
// Macros
// --------------------------------------------------------

#define TEST(name) \
    static void _test_body_##name(); \
    static scriptlang::test::TestRegistrar _test_reg_##name(#name, _test_body_##name); \
    static void _test_body_##name()

// Assertions
#define EXPECT_TRUE(expr) do { \
    if (!(expr)) { \
        scriptlang::test::currentFailures().push_back({ \
            std::string("Expected true: ") + #expr, __FILE__, __LINE__ \
        }); \
    } \
} while(0)

#define EXPECT_FALSE(expr) do { \
    if ((expr)) { \
        scriptlang::test::currentFailures().push_back({ \
            std::string("Expected false: ") + #expr, __FILE__, __LINE__ \
        }); \
    } \
} while(0)

#define EXPECT_EQ(a, b) do { \
    if ((a) != (b)) { \
        std::ostringstream oss; \
        oss << "Expected equal:\n" \
            << "  left:  " << (a) << "\n" \
            << "  right: " << (b); \
        scriptlang::test::currentFailures().push_back({ \
            oss.str(), __FILE__, __LINE__ \
        }); \
    } \
} while(0)

#define EXPECT_NE(a, b) do { \
    if ((a) == (b)) { \
        std::ostringstream oss; \
        oss << "Expected not equal:\n" \
            << "  both: " << (a); \
        scriptlang::test::currentFailures().push_back({ \
            oss.str(), __FILE__, __LINE__ \
        }); \
    } \
} while(0)

#define EXPECT_SIZE(container, expected) do { \
    if ((container).size() != (size_t)(expected)) { \
        std::ostringstream oss; \
        oss << "Expected size " << (expected) \
            << " but got " << (container).size(); \
        scriptlang::test::currentFailures().push_back({ \
            oss.str(), __FILE__, __LINE__ \
        }); \
    } \
} while(0)

// --------------------------------------------------------
// Runner — called from main()
// --------------------------------------------------------
inline int runAllTests() {
    auto& tests = registry();
    int passed  = 0;
    int failed  = 0;

    std::cout << "\n[TEST] Running " << tests.size() << " test(s)...\n\n";

    for (auto& test : tests) {
        currentFailures().clear();

        std::cout << "  " << test.name << " ... ";
        std::cout.flush();

        test.body();

        if (currentFailures().empty()) {
            std::cout << "PASS\n";
            passed++;
        } else {
            std::cout << "FAIL\n";
            failed++;
            for (auto& f : currentFailures()) {
                std::cout << "    " << f.file << ":" << f.line << "\n";
                std::cout << "    " << f.message << "\n\n";
            }
        }
    }

    std::cout << "\n[TEST] Results: " << passed << " passed, " << failed << " failed, " << tests.size() << " total\n\n";

    return failed > 0 ? 1 : 0;
}

} // namespace scriptlang::test

#endif // TEST_FRAMEWORK_H

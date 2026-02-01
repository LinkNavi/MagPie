#include "test_framework.h"
#include "scriptlang.h"

using namespace scriptlang;

// Helper: tokenize a string and return the tokens (excludes the trailing Eof)
static std::vector<Token> lex(const char* source) {
    Lexer lexer(source);
    auto tokens = lexer.tokenize();
    // Remove trailing Eof for easier testing
    if (!tokens.empty() && tokens.back().isEof()) {
        tokens.pop_back();
    }
    return tokens;
}

// ============================================================
// NEW FEATURE TESTS
// ============================================================

// --------------------------------------------------------
// Lambda arrow operator
// --------------------------------------------------------

TEST(lexer_lambda_arrow) {
    auto tokens = lex("=> = >");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::LambdaArrow);
    EXPECT_EQ(tokens[1].type, TokenType::Assign);
    EXPECT_EQ(tokens[2].type, TokenType::Greater);
}

TEST(lexer_lambda_function_syntax) {
    auto tokens = lex("(x, y) => x + y");
    
    EXPECT_SIZE(tokens, 9);
    EXPECT_EQ(tokens[0].type, TokenType::LeftParen);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
    EXPECT_EQ(tokens[2].type, TokenType::Comma);
    EXPECT_EQ(tokens[3].type, TokenType::Identifier);
    EXPECT_EQ(tokens[4].type, TokenType::RightParen);
    EXPECT_EQ(tokens[5].type, TokenType::LambdaArrow);
    EXPECT_EQ(tokens[6].type, TokenType::Identifier);
    EXPECT_EQ(tokens[7].type, TokenType::Plus);
    EXPECT_EQ(tokens[8].type, TokenType::Identifier);
}

// --------------------------------------------------------
// Range operators
// --------------------------------------------------------

TEST(lexer_dot_dot_range) {
    auto tokens = lex("0..10");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Integer);
    EXPECT_EQ(tokens[1].type, TokenType::DotDot);
    EXPECT_EQ(tokens[2].type, TokenType::Integer);
}

TEST(lexer_dot_dot_dot_spread) {
    auto tokens = lex("...args");
    
    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type, TokenType::DotDotDot);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
}

TEST(lexer_distinguishes_dot_variants) {
    auto tokens = lex(". .. ...");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Dot);
    EXPECT_EQ(tokens[1].type, TokenType::DotDot);
    EXPECT_EQ(tokens[2].type, TokenType::DotDotDot);
}

// --------------------------------------------------------
// Hexadecimal literals
// --------------------------------------------------------

TEST(lexer_hex_literal_lowercase) {
    auto tokens = lex("0xff");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::HexInteger);
    EXPECT_EQ(tokens[0].value, std::string("0xff"));
}

TEST(lexer_hex_literal_uppercase) {
    auto tokens = lex("0xFF 0XAB 0x1A2B");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::HexInteger);
    EXPECT_EQ(tokens[1].type, TokenType::HexInteger);
    EXPECT_EQ(tokens[2].type, TokenType::HexInteger);
}

TEST(lexer_hex_literal_color_example) {
    // Common use case: color values
    auto tokens = lex("color = 0xFF5733");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Identifier);
    EXPECT_EQ(tokens[1].type, TokenType::Assign);
    EXPECT_EQ(tokens[2].type, TokenType::HexInteger);
    EXPECT_EQ(tokens[2].value, std::string("0xFF5733"));
}

TEST(lexer_invalid_hex_literal) {
    auto tokens = lex("0x");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

// --------------------------------------------------------
// Binary literals
// --------------------------------------------------------

TEST(lexer_binary_literal) {
    auto tokens = lex("0b1010");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::BinaryInteger);
    EXPECT_EQ(tokens[0].value, std::string("0b1010"));
}

TEST(lexer_binary_literal_flags) {
    // Common use case: bit flags
    auto tokens = lex("flags = 0b11111111");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Identifier);
    EXPECT_EQ(tokens[1].type, TokenType::Assign);
    EXPECT_EQ(tokens[2].type, TokenType::BinaryInteger);
    EXPECT_EQ(tokens[2].value, std::string("0b11111111"));
}

TEST(lexer_invalid_binary_literal) {
    auto tokens = lex("0b");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

TEST(lexer_binary_with_invalid_digit) {
    auto tokens = lex("0b102");
    
    // Should tokenize as 0b10 followed by 2
    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type, TokenType::BinaryInteger);
    EXPECT_EQ(tokens[0].value, std::string("0b10"));
    EXPECT_EQ(tokens[1].type, TokenType::Integer);
}

// --------------------------------------------------------
// Scientific notation
// --------------------------------------------------------

TEST(lexer_scientific_notation_positive) {
    auto tokens = lex("1.5e3");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Scientific);
    EXPECT_EQ(tokens[0].value, std::string("1.5e3"));
}

TEST(lexer_scientific_notation_negative) {
    auto tokens = lex("2.0e-10");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Scientific);
    EXPECT_EQ(tokens[0].value, std::string("2.0e-10"));
}

TEST(lexer_scientific_notation_explicit_positive) {
    auto tokens = lex("3.14E+2");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Scientific);
    EXPECT_EQ(tokens[0].value, std::string("3.14E+2"));
}

TEST(lexer_scientific_integer_base) {
    auto tokens = lex("5e10");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Scientific);
    EXPECT_EQ(tokens[0].value, std::string("5e10"));
}

// --------------------------------------------------------
// Character literals
// --------------------------------------------------------

TEST(lexer_char_literal_simple) {
    auto tokens = lex("'a'");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Char);
    EXPECT_EQ(tokens[0].value, std::string("a"));
}

TEST(lexer_char_literal_escape_newline) {
    auto tokens = lex("'\\n'");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Char);
    EXPECT_EQ(tokens[0].value, std::string("\n"));
}

TEST(lexer_char_literal_escape_tab) {
    auto tokens = lex("'\\t'");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Char);
    EXPECT_EQ(tokens[0].value, std::string("\t"));
}

TEST(lexer_char_literal_escape_quote) {
    auto tokens = lex("'\\''");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Char);
    EXPECT_EQ(tokens[0].value, std::string("'"));
}

TEST(lexer_char_literal_escape_backslash) {
    auto tokens = lex("'\\\\'");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Char);
    EXPECT_EQ(tokens[0].value, std::string("\\"));
}

TEST(lexer_char_literal_empty_error) {
    auto tokens = lex("''");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

TEST(lexer_char_literal_unterminated) {
    auto tokens = lex("'a");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

TEST(lexer_char_literal_invalid_escape) {
    auto tokens = lex("'\\x'");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

// --------------------------------------------------------
// Raw strings (backticks)
// --------------------------------------------------------

TEST(lexer_raw_string_simple) {
    auto tokens = lex("`hello`");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::RawString);
    EXPECT_EQ(tokens[0].value, std::string("hello"));
}

TEST(lexer_raw_string_with_backslashes) {
    // In raw strings, backslashes are literal
    auto tokens = lex("`C:\\Users\\path`");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::RawString);
    EXPECT_EQ(tokens[0].value, std::string("C:\\Users\\path"));
}

TEST(lexer_raw_string_multiline) {
    auto tokens = lex("`line1\nline2\nline3`");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::RawString);
    EXPECT_EQ(tokens[0].value, std::string("line1\nline2\nline3"));
}

TEST(lexer_raw_string_shader_code) {
    // Common use case: shader code
    const char* shader = R"(`
        #version 330 core
        out vec4 FragColor;
        void main() {
            FragColor = vec4(1.0, 0.5, 0.2, 1.0);
        }
    `)";
    
    auto tokens = lex(shader);
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::RawString);
}

TEST(lexer_raw_string_unterminated) {
    auto tokens = lex("`unterminated");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

// --------------------------------------------------------
// String interpolation
// --------------------------------------------------------

TEST(lexer_interpolated_string_simple) {
    auto tokens = lex("\"Hello ${name}\"");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::InterpolatedString);
    EXPECT_EQ(tokens[0].value, std::string("Hello ${name}"));
}

TEST(lexer_interpolated_string_multiple) {
    auto tokens = lex("\"${x} + ${y} = ${result}\"");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::InterpolatedString);
}

TEST(lexer_regular_string_no_interpolation) {
    // String without ${} should be regular String, not InterpolatedString
    auto tokens = lex("\"Hello world\"");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::String);
}

TEST(lexer_dollar_sign_alone_not_interpolation) {
    // $ without { should not trigger interpolation
    auto tokens = lex("\"Cost: $5\"");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::String);
}

// --------------------------------------------------------
// 'in' keyword for for-in loops
// --------------------------------------------------------

TEST(lexer_in_keyword) {
    auto tokens = lex("for item in array");
    
    EXPECT_SIZE(tokens, 4);
    EXPECT_EQ(tokens[0].type, TokenType::For);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
    EXPECT_EQ(tokens[2].type, TokenType::In);
    EXPECT_EQ(tokens[3].type, TokenType::Identifier);
}

TEST(lexer_in_not_confused_with_identifier) {
    auto tokens = lex("inside in input");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Identifier); // inside
    EXPECT_EQ(tokens[1].type, TokenType::In);         // in
    EXPECT_EQ(tokens[2].type, TokenType::Identifier); // input
}

// --------------------------------------------------------
// Dollar sign for template markers
// --------------------------------------------------------

TEST(lexer_dollar_sign_standalone) {
    auto tokens = lex("$");
    
    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type, TokenType::Dollar);
}

TEST(lexer_dollar_with_identifier) {
    // This could be used for special syntax like $varName
    auto tokens = lex("$ name");
    
    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type, TokenType::Dollar);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
}

// ============================================================
// REALISTIC GAME DEV EXAMPLES
// ============================================================

TEST(lexer_color_constant_hex) {
    auto tokens = lex("const RED = 0xFF0000;");
    
    EXPECT_SIZE(tokens, 5);
    EXPECT_EQ(tokens[0].type, TokenType::Const);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
    EXPECT_EQ(tokens[2].type, TokenType::Assign);
    EXPECT_EQ(tokens[3].type, TokenType::HexInteger);
    EXPECT_EQ(tokens[4].type, TokenType::Semicolon);
}

TEST(lexer_physics_constants_scientific) {
    auto tokens = lex("const GRAVITY = 9.8e-3;");
    
    EXPECT_SIZE(tokens, 5);
    EXPECT_EQ(tokens[2].type, TokenType::Assign);
    EXPECT_EQ(tokens[3].type, TokenType::Scientific);
}

TEST(lexer_event_callback_lambda) {
    auto tokens = lex("button.onClick(() => { health -= 10; });");
    
    // Spot check key parts
    EXPECT_EQ(tokens[0].type, TokenType::Identifier); // button
    EXPECT_EQ(tokens[1].type, TokenType::Dot);
    EXPECT_EQ(tokens[2].type, TokenType::Identifier); // onClick
    EXPECT_EQ(tokens[3].type, TokenType::LeftParen);
    EXPECT_EQ(tokens[4].type, TokenType::LeftParen);
    EXPECT_EQ(tokens[5].type, TokenType::RightParen);
    EXPECT_EQ(tokens[6].type, TokenType::LambdaArrow);
}

TEST(lexer_range_based_for_loop) {
    auto tokens = lex("for i in 0..10 { }");
    
    EXPECT_EQ(tokens[0].type, TokenType::For);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier); // i
    EXPECT_EQ(tokens[2].type, TokenType::In);
    EXPECT_EQ(tokens[3].type, TokenType::Integer);    // 0
    EXPECT_EQ(tokens[4].type, TokenType::DotDot);
    EXPECT_EQ(tokens[5].type, TokenType::Integer);    // 10
}

TEST(lexer_input_handling_char) {
    auto tokens = lex("if (key == 'w') { moveForward(); }");
    
    // Find the char literal
    bool foundChar = false;
    for (const auto& tok : tokens) {
        if (tok.type == TokenType::Char && tok.value == "w") {
            foundChar = true;
            break;
        }
    }
    EXPECT_TRUE(foundChar);
}

TEST(lexer_debug_message_interpolation) {
    auto tokens = lex("Log(\"Player health: ${health}\");");
    
    // Check for interpolated string
    bool foundInterpolated = false;
    for (const auto& tok : tokens) {
        if (tok.type == TokenType::InterpolatedString) {
            foundInterpolated = true;
            break;
        }
    }
    EXPECT_TRUE(foundInterpolated);
}

TEST(lexer_shader_code_raw_string) {
    const char* code = "const vertexShader = `\n"
                       "    attribute vec3 position;\n"
                       "    void main() { }\n"
                       "`;";
    
    auto tokens = lex(code);
    
    EXPECT_EQ(tokens[0].type, TokenType::Const);
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
    EXPECT_EQ(tokens[2].type, TokenType::Assign);
    EXPECT_EQ(tokens[3].type, TokenType::RawString);
    EXPECT_EQ(tokens[4].type, TokenType::Semicolon);
}

TEST(lexer_bit_flags_binary) {
    auto tokens = lex("const FLAGS = 0b11010110;");
    
    EXPECT_EQ(tokens[0].type, TokenType::Const);
    EXPECT_EQ(tokens[3].type, TokenType::BinaryInteger);
}

TEST(lexer_spread_operator_function_call) {
    auto tokens = lex("spawn(player, ...args);");
    
    bool foundSpread = false;
    for (const auto& tok : tokens) {
        if (tok.type == TokenType::DotDotDot) {
            foundSpread = true;
            break;
        }
    }
    EXPECT_TRUE(foundSpread);
}

TEST(lexer_lambda_with_multiple_params) {
    auto tokens = lex("filter((x, y, z) => x > 0)");
    
    EXPECT_EQ(tokens[0].type, TokenType::Identifier); // filter
    EXPECT_EQ(tokens[1].type, TokenType::LeftParen);
    EXPECT_EQ(tokens[2].type, TokenType::LeftParen);
    // ... params ...
    EXPECT_EQ(tokens[8].type, TokenType::RightParen);
    EXPECT_EQ(tokens[9].type, TokenType::LambdaArrow);
}

// --------------------------------------------------------
// Edge cases and combinations
// --------------------------------------------------------

TEST(lexer_all_number_types_together) {
    auto tokens = lex("42 0xFF 0b1010 3.14 1.5e-3");
    
    EXPECT_SIZE(tokens, 5);
    EXPECT_EQ(tokens[0].type, TokenType::Integer);
    EXPECT_EQ(tokens[1].type, TokenType::HexInteger);
    EXPECT_EQ(tokens[2].type, TokenType::BinaryInteger);
    EXPECT_EQ(tokens[3].type, TokenType::Float);
    EXPECT_EQ(tokens[4].type, TokenType::Scientific);
}

TEST(lexer_all_string_types_together) {
    auto tokens = lex("\"normal\" `raw` \"${interpolated}\"");
    
    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::String);
    EXPECT_EQ(tokens[1].type, TokenType::RawString);
    EXPECT_EQ(tokens[2].type, TokenType::InterpolatedString);
}

TEST(lexer_arrow_vs_lambda_arrow) {
    auto tokens = lex("-> =>");
    
    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type, TokenType::Arrow);
    EXPECT_EQ(tokens[1].type, TokenType::LambdaArrow);
}

TEST(lexer_complex_realistic_snippet) {
    const char* code = R"(
        public class Enemy {
            @expose(min=0, max=100)
            private int32 health = 100;
            
            void takeDamage(int32 amount) {
                health -= amount;
                if (health <= 0) {
                    onDeath();
                }
            }
            
            void onDeath() {
                Log("Enemy died with message: ${deathMessage}");
                
                // Drop items
                for item in loot {
                    spawn(item, ...position);
                }
                
                // Binary flags for death effects
                const EFFECT_FLAGS = 0b11010110;
                
                // Lambda for delayed destruction
                Timer.after(1.0, () => {
                    destroy(this);
                });
            }
        }
    )";
    
    auto tokens = lex(code);
    
    // Just verify we successfully tokenized complex code
    EXPECT_TRUE(tokens.size() > 50);
    
    // Verify no errors
    for (const auto& tok : tokens) {
        EXPECT_FALSE(tok.isError());
    }
}

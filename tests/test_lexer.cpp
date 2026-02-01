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

// --------------------------------------------------------
// Single character tokens
// --------------------------------------------------------

TEST(lexer_single_char_tokens) {
    auto tokens = lex("( ) { } [ ] ; , . : # @ % + * / ?");

    EXPECT_SIZE(tokens, 17);
    EXPECT_EQ(tokens[0].type,  TokenType::LeftParen);
    EXPECT_EQ(tokens[1].type,  TokenType::RightParen);
    EXPECT_EQ(tokens[2].type,  TokenType::LeftBrace);
    EXPECT_EQ(tokens[3].type,  TokenType::RightBrace);
    EXPECT_EQ(tokens[4].type,  TokenType::LeftBracket);
    EXPECT_EQ(tokens[5].type,  TokenType::RightBracket);
    EXPECT_EQ(tokens[6].type,  TokenType::Semicolon);
    EXPECT_EQ(tokens[7].type,  TokenType::Comma);
    EXPECT_EQ(tokens[8].type,  TokenType::Dot);
    EXPECT_EQ(tokens[9].type,  TokenType::Colon);
    EXPECT_EQ(tokens[10].type, TokenType::Hash);
    EXPECT_EQ(tokens[11].type, TokenType::At);
    EXPECT_EQ(tokens[12].type, TokenType::Percent);
    EXPECT_EQ(tokens[13].type, TokenType::Plus);
    EXPECT_EQ(tokens[14].type, TokenType::Star);
    EXPECT_EQ(tokens[15].type, TokenType::Slash);
    EXPECT_EQ(tokens[16].type, TokenType::Question);
}

// --------------------------------------------------------
// Two character tokens
// --------------------------------------------------------

TEST(lexer_two_char_tokens) {
    auto tokens = lex("== != <= >= += -= *= /= && || -> .? ??");

    EXPECT_SIZE(tokens, 13);
    EXPECT_EQ(tokens[0].type,  TokenType::Equal);
    EXPECT_EQ(tokens[1].type,  TokenType::NotEqual);
    EXPECT_EQ(tokens[2].type,  TokenType::LessEqual);
    EXPECT_EQ(tokens[3].type,  TokenType::GreaterEqual);
    EXPECT_EQ(tokens[4].type,  TokenType::PlusAssign);
    EXPECT_EQ(tokens[5].type,  TokenType::MinusAssign);
    EXPECT_EQ(tokens[6].type,  TokenType::StarAssign);
    EXPECT_EQ(tokens[7].type,  TokenType::SlashAssign);
    EXPECT_EQ(tokens[8].type,  TokenType::And);
    EXPECT_EQ(tokens[9].type,  TokenType::Or);
    EXPECT_EQ(tokens[10].type, TokenType::Arrow);
    EXPECT_EQ(tokens[11].type, TokenType::DotQuestion);
    EXPECT_EQ(tokens[12].type, TokenType::QuestionQuestion);
}

TEST(lexer_ambiguous_single_vs_double) {
    // Make sure = vs ==, < vs <=, etc. resolve correctly
    auto tokens = lex("= < > ! - =");

    EXPECT_SIZE(tokens, 6);
    EXPECT_EQ(tokens[0].type, TokenType::Assign);
    EXPECT_EQ(tokens[1].type, TokenType::Less);
    EXPECT_EQ(tokens[2].type, TokenType::Greater);
    EXPECT_EQ(tokens[3].type, TokenType::Not);
    EXPECT_EQ(tokens[4].type, TokenType::Minus);
    EXPECT_EQ(tokens[5].type, TokenType::Assign);
}

// --------------------------------------------------------
// Integer literals
// --------------------------------------------------------

TEST(lexer_integer_literal) {
    auto tokens = lex("42");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::Integer);
    EXPECT_EQ(tokens[0].value, std::string("42"));
}

TEST(lexer_integer_zero) {
    auto tokens = lex("0");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::Integer);
    EXPECT_EQ(tokens[0].value, std::string("0"));
}

TEST(lexer_multiple_integers) {
    auto tokens = lex("1 22 333");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].value, std::string("1"));
    EXPECT_EQ(tokens[1].value, std::string("22"));
    EXPECT_EQ(tokens[2].value, std::string("333"));
}

// --------------------------------------------------------
// Float literals
// --------------------------------------------------------

TEST(lexer_float_literal) {
    auto tokens = lex("3.14");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::Float);
    EXPECT_EQ(tokens[0].value, std::string("3.14"));
}

TEST(lexer_float_zero) {
    auto tokens = lex("0.0");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::Float);
    EXPECT_EQ(tokens[0].value, std::string("0.0"));
}

TEST(lexer_float_does_not_eat_method_dot) {
    // "obj.method" should NOT be read as a float.
    // The dot after an identifier is a Dot token, not a decimal.
    auto tokens = lex("obj.method");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Identifier);
    EXPECT_EQ(tokens[0].value, std::string("obj"));
    EXPECT_EQ(tokens[1].type, TokenType::Dot);
    EXPECT_EQ(tokens[2].type, TokenType::Identifier);
    EXPECT_EQ(tokens[2].value, std::string("method"));
}

TEST(lexer_integer_followed_by_dot_method) {
    // "100.toString()" — 100 is an integer, .toString is a method call.
    // The dot is NOT a decimal because it's not followed by a digit.
    auto tokens = lex("100.toString()");

    EXPECT_SIZE(tokens, 5);
    EXPECT_EQ(tokens[0].type,  TokenType::Integer);
    EXPECT_EQ(tokens[0].value, std::string("100"));
    EXPECT_EQ(tokens[1].type,  TokenType::Dot);
    EXPECT_EQ(tokens[2].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[2].value, std::string("toString"));
    EXPECT_EQ(tokens[3].type,  TokenType::LeftParen);
    EXPECT_EQ(tokens[4].type,  TokenType::RightParen);
}

// --------------------------------------------------------
// String literals
// --------------------------------------------------------

TEST(lexer_string_literal) {
    auto tokens = lex("\"hello world\"");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::String);
    EXPECT_EQ(tokens[0].value, std::string("hello world"));
}

TEST(lexer_empty_string) {
    auto tokens = lex("\"\"");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::String);
    EXPECT_EQ(tokens[0].value, std::string(""));
}

TEST(lexer_string_escape_sequences) {
    auto tokens = lex("\"line1\\nline2\\ttab\\\\slash\\\"quote\"");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::String);
    EXPECT_EQ(tokens[0].value, std::string("line1\nline2\ttab\\slash\"quote"));
}

TEST(lexer_unterminated_string) {
    auto tokens = lex("\"hello");

    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

TEST(lexer_string_with_newline_is_error) {
    auto tokens = lex("\"hello\nworld\"");

    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

TEST(lexer_invalid_escape_sequence) {
    auto tokens = lex("\"hello\\q\"");

    EXPECT_SIZE(tokens, 1);
    EXPECT_TRUE(tokens[0].isError());
}

// --------------------------------------------------------
// Keywords
// --------------------------------------------------------

TEST(lexer_keywords) {
    auto tokens = lex("class public private static return if else while for switch case default include");

    EXPECT_SIZE(tokens, 13);
    EXPECT_EQ(tokens[0].type, TokenType::Class);
    EXPECT_EQ(tokens[1].type, TokenType::Public);
    EXPECT_EQ(tokens[2].type, TokenType::Private);
    EXPECT_EQ(tokens[3].type, TokenType::Static);
    EXPECT_EQ(tokens[4].type, TokenType::Return);
    EXPECT_EQ(tokens[5].type, TokenType::If);
    EXPECT_EQ(tokens[6].type, TokenType::Else);
    EXPECT_EQ(tokens[7].type, TokenType::While);
    EXPECT_EQ(tokens[8].type, TokenType::For);
    EXPECT_EQ(tokens[9].type, TokenType::Switch);
    EXPECT_EQ(tokens[10].type, TokenType::Case);
    EXPECT_EQ(tokens[11].type, TokenType::Default);
    EXPECT_EQ(tokens[12].type, TokenType::Include);
}

TEST(lexer_type_keywords) {
    auto tokens = lex("int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64 bool string void");

    EXPECT_SIZE(tokens, 13);
    EXPECT_EQ(tokens[0].type,  TokenType::Int8);
    EXPECT_EQ(tokens[1].type,  TokenType::Int16);
    EXPECT_EQ(tokens[2].type,  TokenType::Int32);
    EXPECT_EQ(tokens[3].type,  TokenType::Int64);
    EXPECT_EQ(tokens[4].type,  TokenType::UInt8);
    EXPECT_EQ(tokens[5].type,  TokenType::UInt16);
    EXPECT_EQ(tokens[6].type,  TokenType::UInt32);
    EXPECT_EQ(tokens[7].type,  TokenType::UInt64);
    EXPECT_EQ(tokens[8].type,  TokenType::Float32);
    EXPECT_EQ(tokens[9].type,  TokenType::Float64);
    EXPECT_EQ(tokens[10].type, TokenType::Bool);
    EXPECT_EQ(tokens[11].type, TokenType::String_);
    EXPECT_EQ(tokens[12].type, TokenType::Void);
}

TEST(lexer_literal_keywords) {
    auto tokens = lex("true false null");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::True);
    EXPECT_EQ(tokens[1].type, TokenType::False);
    EXPECT_EQ(tokens[2].type, TokenType::Null);
}

// --------------------------------------------------------
// Identifiers
// --------------------------------------------------------

TEST(lexer_identifier) {
    auto tokens = lex("playerHealth");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[0].value, std::string("playerHealth"));
}

TEST(lexer_identifier_with_underscore) {
    auto tokens = lex("_private my_var __dunder");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[0].value, std::string("_private"));
    EXPECT_EQ(tokens[1].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[1].value, std::string("my_var"));
    EXPECT_EQ(tokens[2].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[2].value, std::string("__dunder"));
}

TEST(lexer_identifier_not_confused_with_keyword) {
    // "classes" starts with "class" but is an identifier
    auto tokens = lex("classes returning iffy");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].type, TokenType::Identifier);
    EXPECT_EQ(tokens[0].value, std::string("classes"));
    EXPECT_EQ(tokens[1].type, TokenType::Identifier);
    EXPECT_EQ(tokens[1].value, std::string("returning"));
    EXPECT_EQ(tokens[2].type, TokenType::Identifier);
    EXPECT_EQ(tokens[2].value, std::string("iffy"));
}

// --------------------------------------------------------
// Comments
// --------------------------------------------------------

TEST(lexer_single_line_comment) {
    auto tokens = lex("42 // this is a comment\n100");

    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type,  TokenType::Integer);
    EXPECT_EQ(tokens[0].value, std::string("42"));
    EXPECT_EQ(tokens[1].type,  TokenType::Integer);
    EXPECT_EQ(tokens[1].value, std::string("100"));
}

TEST(lexer_multi_line_comment) {
    auto tokens = lex("42 /* this is\na multi-line\ncomment */ 100");

    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type,  TokenType::Integer);
    EXPECT_EQ(tokens[0].value, std::string("42"));
    EXPECT_EQ(tokens[1].type,  TokenType::Integer);
    EXPECT_EQ(tokens[1].value, std::string("100"));
}

TEST(lexer_comment_at_end_of_file) {
    auto tokens = lex("42 // trailing comment");

    EXPECT_SIZE(tokens, 1);
    EXPECT_EQ(tokens[0].type,  TokenType::Integer);
    EXPECT_EQ(tokens[0].value, std::string("42"));
}

// --------------------------------------------------------
// Line and column tracking
// --------------------------------------------------------

TEST(lexer_tracks_line_numbers) {
    auto tokens = lex("a\nb\nc");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].line, 1);
    EXPECT_EQ(tokens[1].line, 2);
    EXPECT_EQ(tokens[2].line, 3);
}

TEST(lexer_tracks_column_numbers) {
    auto tokens = lex("a  b   c");

    EXPECT_SIZE(tokens, 3);
    EXPECT_EQ(tokens[0].column, 1);
    EXPECT_EQ(tokens[1].column, 4);
    EXPECT_EQ(tokens[2].column, 8);
}

// --------------------------------------------------------
// Error cases
// --------------------------------------------------------

TEST(lexer_unexpected_character) {
    auto tokens = lex("42 $ 100");

    // Should get: 42, then an Error token for $
    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type, TokenType::Integer);
    EXPECT_TRUE(tokens[1].isError());
}

TEST(lexer_bare_ampersand_is_error) {
    auto tokens = lex("a & b");

    EXPECT_SIZE(tokens, 2); // 'a', then Error on &
    EXPECT_EQ(tokens[0].type, TokenType::Identifier);
    EXPECT_TRUE(tokens[1].isError());
}

TEST(lexer_bare_pipe_is_error) {
    auto tokens = lex("a | b");

    EXPECT_SIZE(tokens, 2);
    EXPECT_EQ(tokens[0].type, TokenType::Identifier);
    EXPECT_TRUE(tokens[1].isError());
}

// --------------------------------------------------------
// Realistic snippet
// --------------------------------------------------------

TEST(lexer_real_snippet) {
    // A realistic fragment from the target syntax
    const char* source =
        "public class Player {\n"
        "    private int32 health = 100;\n"
        "    public bool isDead() {\n"
        "        return health <= 0;\n"
        "    }\n"
        "}\n";

    auto tokens = lex(source);

    // Spot-check key tokens in order
    EXPECT_EQ(tokens[0].type,  TokenType::Public);
    EXPECT_EQ(tokens[1].type,  TokenType::Class);
    EXPECT_EQ(tokens[2].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[2].value, std::string("Player"));
    EXPECT_EQ(tokens[3].type,  TokenType::LeftBrace);
    EXPECT_EQ(tokens[4].type,  TokenType::Private);
    EXPECT_EQ(tokens[5].type,  TokenType::Int32);
    EXPECT_EQ(tokens[6].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[6].value, std::string("health"));
    EXPECT_EQ(tokens[7].type,  TokenType::Assign);
    EXPECT_EQ(tokens[8].type,  TokenType::Integer);
    EXPECT_EQ(tokens[8].value, std::string("100"));
    EXPECT_EQ(tokens[9].type,  TokenType::Semicolon);
}

TEST(lexer_annotation_syntax) {
    // @expose(min=0, max=100)
    // Tokens: @ expose ( min = 0 , max = 100 )
    auto tokens = lex("@expose(min=0, max=100)");

    EXPECT_SIZE(tokens, 11);
    EXPECT_EQ(tokens[0].type,   TokenType::At);
    EXPECT_EQ(tokens[1].type,   TokenType::Identifier);
    EXPECT_EQ(tokens[1].value,  std::string("expose"));
    EXPECT_EQ(tokens[2].type,   TokenType::LeftParen);
    EXPECT_EQ(tokens[3].type,   TokenType::Identifier);
    EXPECT_EQ(tokens[3].value,  std::string("min"));
    EXPECT_EQ(tokens[4].type,   TokenType::Assign);
    EXPECT_EQ(tokens[5].type,   TokenType::Integer);
    EXPECT_EQ(tokens[5].value,  std::string("0"));
    EXPECT_EQ(tokens[6].type,   TokenType::Comma);
    EXPECT_EQ(tokens[7].type,   TokenType::Identifier);
    EXPECT_EQ(tokens[7].value,  std::string("max"));
    EXPECT_EQ(tokens[8].type,   TokenType::Assign);
    EXPECT_EQ(tokens[9].type,   TokenType::Integer);
    EXPECT_EQ(tokens[9].value,  std::string("100"));
    EXPECT_EQ(tokens[10].type,  TokenType::RightParen);
}

TEST(lexer_import_syntax) {
    auto tokens = lex("#import <engine/console> { Print, Log }");

    EXPECT_EQ(tokens[0].type,  TokenType::Hash);
    EXPECT_EQ(tokens[1].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[1].value, std::string("import"));
    EXPECT_EQ(tokens[2].type,  TokenType::Less);
    EXPECT_EQ(tokens[3].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[3].value, std::string("engine"));
    EXPECT_EQ(tokens[4].type,  TokenType::Slash);
    EXPECT_EQ(tokens[5].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[5].value, std::string("console"));
    EXPECT_EQ(tokens[6].type,  TokenType::Greater);
    EXPECT_EQ(tokens[7].type,  TokenType::LeftBrace);
    EXPECT_EQ(tokens[8].type,  TokenType::Identifier);
    EXPECT_EQ(tokens[8].value, std::string("Print"));
    EXPECT_EQ(tokens[9].type,  TokenType::Comma);
    EXPECT_EQ(tokens[10].type, TokenType::Identifier);
    EXPECT_EQ(tokens[10].value, std::string("Log"));
    EXPECT_EQ(tokens[11].type, TokenType::RightBrace);
}

// --------------------------------------------------------
// Empty input
// --------------------------------------------------------

TEST(lexer_empty_input) {
    auto tokens = lex("");
    EXPECT_SIZE(tokens, 0);
}

TEST(lexer_only_whitespace) {
    auto tokens = lex("   \n\n\t  \n  ");
    EXPECT_SIZE(tokens, 0);
}

TEST(lexer_only_comments) {
    auto tokens = lex("// nothing here\n/* also nothing */");
    EXPECT_SIZE(tokens, 0);
}

#ifndef LEXER_H
#define LEXER_H

#include "token.h"

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <cctype>

// ------------------------------------------------------------
// lexer.h — Tokenizes source code into a stream of Tokens.
//
// UPDATED: Enhanced with game-dev friendly features:
// - Hex/binary integer literals (0xFF, 0b1010)
// - Scientific notation (1.5e-3)
// - Character literals ('a', '\n')
// - Raw strings with backticks (`raw text`)
// - String interpolation markers ($)
// - Lambda arrow (=>)
// - Range operators (.., ...)
// - 'in' keyword for for-in loops
//
// Usage:
//     scriptlang::Lexer lexer(sourceCode);
//     auto tokens = lexer.tokenize();
//     // tokens is a std::vector<Token>, always ending with Eof.
//     // If any token has type Error, its value field contains
//     // the error message.
// ------------------------------------------------------------

namespace scriptlang {

class Lexer {
public:
    explicit Lexer(std::string_view source)
        : source_(source), pos_(0), line_(1), column_(1)
    {}

    // Tokenize the entire source. Returns a token stream ending with Eof.
    // If an error is encountered, an Error token is added to the stream
    // and tokenization stops.
    std::vector<Token> tokenize() {
        std::vector<Token> tokens;

        while (!isAtEnd()) {
            skipWhitespaceAndComments();
            if (isAtEnd()) break;

            Token token = scanToken();
            tokens.push_back(token);

            if (token.isError()) {
                break; // stop on first error
            }
        }

        tokens.emplace_back(TokenType::Eof, "", line_, column_);
        return tokens;
    }

private:
    // --------------------------------------------------------
    // Source state
    // --------------------------------------------------------
    std::string_view source_;
    size_t pos_;
    int    line_;
    int    column_;

    // --------------------------------------------------------
    // Character access
    // --------------------------------------------------------
    char current() const {
        return source_[pos_];
    }

    char peek(int offset = 0) const {
        size_t idx = pos_ + offset;
        if (idx >= source_.size()) return '\0';
        return source_[idx];
    }

    char advance() {
        char c = source_[pos_];
        pos_++;
        if (c == '\n') {
            line_++;
            column_ = 1;
        } else {
            column_++;
        }
        return c;
    }

    bool isAtEnd() const {
        return pos_ >= source_.size();
    }

    bool match(char expected) {
        if (isAtEnd() || current() != expected) return false;
        advance();
        return true;
    }

    // --------------------------------------------------------
    // Whitespace & comments
    // --------------------------------------------------------
    void skipWhitespaceAndComments() {
        while (!isAtEnd()) {
            char c = current();

            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
                advance();
            }
            // Single-line comment: // ...
            else if (c == '/' && peek(1) == '/') {
                while (!isAtEnd() && current() != '\n') {
                    advance();
                }
            }
            // Multi-line comment: /* ... */
            else if (c == '/' && peek(1) == '*') {
                advance(); // /
                advance(); // *
                while (!isAtEnd()) {
                    if (current() == '*' && peek(1) == '/') {
                        advance(); // *
                        advance(); // /
                        break;
                    }
                    advance();
                }
            }
            else {
                break;
            }
        }
    }

    // --------------------------------------------------------
    // Main scan dispatch
    // --------------------------------------------------------
    Token scanToken() {
        int startLine   = line_;
        int startColumn = column_;
        char c = current();

        // --- Single & double character tokens ---
        switch (c) {
            case '(': advance(); return Token(TokenType::LeftParen,    "(", startLine, startColumn);
            case ')': advance(); return Token(TokenType::RightParen,   ")", startLine, startColumn);
            case '{': advance(); return Token(TokenType::LeftBrace,    "{", startLine, startColumn);
            case '}': advance(); return Token(TokenType::RightBrace,   "}", startLine, startColumn);
            case '[': advance(); return Token(TokenType::LeftBracket,  "[", startLine, startColumn);
            case ']': advance(); return Token(TokenType::RightBracket, "]", startLine, startColumn);
            case ';': advance(); return Token(TokenType::Semicolon,    ";", startLine, startColumn);
            case ',': advance(); return Token(TokenType::Comma,        ",", startLine, startColumn);
            case ':': advance(); return Token(TokenType::Colon,        ":", startLine, startColumn);
            case '#': advance(); return Token(TokenType::Hash,         "#", startLine, startColumn);
            case '@': advance(); return Token(TokenType::At,           "@", startLine, startColumn);
            case '%': advance(); return Token(TokenType::Percent,      "%", startLine, startColumn);
            case '$': advance(); return Token(TokenType::Dollar,       "$", startLine, startColumn);

            case '+':
                advance();
                if (match('=')) return Token(TokenType::PlusAssign,  "+=", startLine, startColumn);
                return Token(TokenType::Plus, "+", startLine, startColumn);

            case '*':
                advance();
                if (match('=')) return Token(TokenType::StarAssign,  "*=", startLine, startColumn);
                return Token(TokenType::Star, "*", startLine, startColumn);

            case '/':
                advance();
                if (match('=')) return Token(TokenType::SlashAssign, "/=", startLine, startColumn);
                return Token(TokenType::Slash, "/", startLine, startColumn);

            case '=':
                advance();
                if (match('=')) return Token(TokenType::Equal,  "==", startLine, startColumn);
                if (match('>')) return Token(TokenType::LambdaArrow, "=>", startLine, startColumn);
                return Token(TokenType::Assign, "=", startLine, startColumn);

            case '?':
                advance();
                if (match('?')) return Token(TokenType::QuestionQuestion, "??", startLine, startColumn);
                return Token(TokenType::Question, "?", startLine, startColumn);

            case '.':
                advance();
                if (match('?')) return Token(TokenType::DotQuestion, ".?", startLine, startColumn);
                if (match('.')) {
                    if (match('.')) return Token(TokenType::DotDotDot, "...", startLine, startColumn);
                    return Token(TokenType::DotDot, "..", startLine, startColumn);
                }
                return Token(TokenType::Dot, ".", startLine, startColumn);

            case '!':
                advance();
                if (match('=')) return Token(TokenType::NotEqual, "!=", startLine, startColumn);
                return Token(TokenType::Not, "!", startLine, startColumn);

            case '<':
                advance();
                if (match('=')) return Token(TokenType::LessEqual, "<=", startLine, startColumn);
                return Token(TokenType::Less, "<", startLine, startColumn);

            case '>':
                advance();
                if (match('=')) return Token(TokenType::GreaterEqual, ">=", startLine, startColumn);
                return Token(TokenType::Greater, ">", startLine, startColumn);

            case '&':
                advance();
                if (match('&')) return Token(TokenType::And, "&&", startLine, startColumn);
                return Token(TokenType::Error, "Unexpected character '&' (did you mean '&&'?)", startLine, startColumn);

            case '|':
                advance();
                if (match('|')) return Token(TokenType::Or, "||", startLine, startColumn);
                return Token(TokenType::Error, "Unexpected character '|' (did you mean '||'?)", startLine, startColumn);

            case '-':
                advance();
                if (match('>')) return Token(TokenType::Arrow,       "->", startLine, startColumn);
                if (match('=')) return Token(TokenType::MinusAssign, "-=", startLine, startColumn);
                return Token(TokenType::Minus, "-", startLine, startColumn);
        }

        // --- Character literal ---
        if (c == '\'') {
            return scanChar(startLine, startColumn);
        }

        // --- String literal (regular or interpolated) ---
        if (c == '"') {
            return scanString(startLine, startColumn);
        }

        // --- Raw string literal (backtick) ---
        if (c == '`') {
            return scanRawString(startLine, startColumn);
        }

        // --- Number literal (check for hex/binary FIRST, then regular numbers) ---
        // Special case: hex literal starting with 0x
        if (c == '0' && (peek(1) == 'x' || peek(1) == 'X')) {
            return scanHexNumber(startLine, startColumn);
        }

        // Special case: binary literal starting with 0b
        if (c == '0' && (peek(1) == 'b' || peek(1) == 'B')) {
            return scanBinaryNumber(startLine, startColumn);
        }

        // Regular number literal (integers, floats, scientific)
        if (isDigit(c)) {
            return scanNumber(startLine, startColumn);
        }

        // --- Identifier or keyword ---
        if (isAlpha(c)) {
            return scanIdentifier(startLine, startColumn);
        }

        // --- Unknown character ---
        advance();
        return Token(TokenType::Error,
            std::string("Unexpected character '") + c + "'",
            startLine, startColumn);
    }

    // --------------------------------------------------------
    // Character literal scanning
    // Handles: 'a', '\n', '\t', '\\', '\''
    // --------------------------------------------------------
    Token scanChar(int startLine, int startColumn) {
        advance(); // consume opening '

        if (isAtEnd()) {
            return Token(TokenType::Error, "Unterminated character literal", startLine, startColumn);
        }

        char value;
        if (current() == '\\') {
            advance(); // consume backslash
            if (isAtEnd()) {
                return Token(TokenType::Error, "Unterminated character literal", startLine, startColumn);
            }
            switch (current()) {
                case 'n':  value = '\n'; break;
                case 't':  value = '\t'; break;
                case 'r':  value = '\r'; break;
                case '\\': value = '\\'; break;
                case '\'': value = '\''; break;
                case '0':  value = '\0'; break;
                default:
                    return Token(TokenType::Error,
                        std::string("Invalid escape sequence '\\") + current() + "' in character literal",
                        startLine, startColumn);
            }
            advance();
        } else if (current() == '\'') {
            return Token(TokenType::Error, "Empty character literal", startLine, startColumn);
        } else if (current() == '\n') {
            return Token(TokenType::Error, "Unterminated character literal", startLine, startColumn);
        } else {
            value = current();
            advance();
        }

        if (isAtEnd() || current() != '\'') {
            return Token(TokenType::Error, "Unterminated character literal", startLine, startColumn);
        }

        advance(); // consume closing '
        return Token(TokenType::Char, std::string(1, value), startLine, startColumn);
    }

    // --------------------------------------------------------
    // String literal scanning
    // Handles escape sequences: \n \t \\ \"
    // Detects ${...} for interpolation marking
    // --------------------------------------------------------
    Token scanString(int startLine, int startColumn) {
        advance(); // consume opening "

        std::string value;
        bool hasInterpolation = false;

        while (!isAtEnd() && current() != '"') {
            if (current() == '\n') {
                return Token(TokenType::Error, "Unterminated string literal", startLine, startColumn);
            }
            if (current() == '\\') {
                advance(); // consume backslash
                if (isAtEnd()) {
                    return Token(TokenType::Error, "Unterminated string literal", startLine, startColumn);
                }
                switch (current()) {
                    case 'n':  value += '\n'; break;
                    case 't':  value += '\t'; break;
                    case 'r':  value += '\r'; break;
                    case '\\': value += '\\'; break;
                    case '"':  value += '"';  break;
                    default:
                        return Token(TokenType::Error,
                            std::string("Invalid escape sequence '\\") + current() + "'",
                            startLine, startColumn);
                }
                advance();
            } else if (current() == '$' && peek(1) == '{') {
                // Mark that this string has interpolation
                hasInterpolation = true;
                value += current();
                advance();
            } else {
                value += current();
                advance();
            }
        }

        if (isAtEnd()) {
            return Token(TokenType::Error, "Unterminated string literal", startLine, startColumn);
        }

        advance(); // consume closing "
        
        TokenType type = hasInterpolation ? TokenType::InterpolatedString : TokenType::String;
        return Token(type, value, startLine, startColumn);
    }

    // --------------------------------------------------------
    // Raw string literal scanning (backticks)
    // No escape sequences, everything is literal
    // --------------------------------------------------------
    Token scanRawString(int startLine, int startColumn) {
        advance(); // consume opening `

        std::string value;
        while (!isAtEnd() && current() != '`') {
            value += current();
            advance();
        }

        if (isAtEnd()) {
            return Token(TokenType::Error, "Unterminated raw string literal", startLine, startColumn);
        }

        advance(); // consume closing `
        return Token(TokenType::RawString, value, startLine, startColumn);
    }

    // --------------------------------------------------------
    // Hexadecimal number scanning (0xFF, 0x1A2B)
    // --------------------------------------------------------
    Token scanHexNumber(int startLine, int startColumn) {
        std::string value;
        value += current(); // 0
        advance();
        value += current(); // x or X
        advance();

        if (isAtEnd() || !isHexDigit(current())) {
            return Token(TokenType::Error, "Invalid hexadecimal literal", startLine, startColumn);
        }

        while (!isAtEnd() && isHexDigit(current())) {
            value += current();
            advance();
        }

        return Token(TokenType::HexInteger, value, startLine, startColumn);
    }

    // --------------------------------------------------------
    // Binary number scanning (0b1010, 0b11111111)
    // --------------------------------------------------------
    Token scanBinaryNumber(int startLine, int startColumn) {
        std::string value;
        value += current(); // 0
        advance();
        value += current(); // b or B
        advance();

        if (isAtEnd() || !isBinaryDigit(current())) {
            return Token(TokenType::Error, "Invalid binary literal", startLine, startColumn);
        }

        while (!isAtEnd() && isBinaryDigit(current())) {
            value += current();
            advance();
        }

        return Token(TokenType::BinaryInteger, value, startLine, startColumn);
    }

    // --------------------------------------------------------
    // Number literal scanning
    // Supports integers, floats, and scientific notation
    // --------------------------------------------------------
    Token scanNumber(int startLine, int startColumn) {
        std::string value;
        bool isFloat = false;
        bool isScientific = false;

        // Integer part
        while (!isAtEnd() && isDigit(current())) {
            value += current();
            advance();
        }

        // Decimal part
        if (!isAtEnd() && current() == '.' && isDigit(peek(1))) {
            isFloat = true;
            value += current();
            advance(); // consume '.'

            while (!isAtEnd() && isDigit(current())) {
                value += current();
                advance();
            }
        }

        // Scientific notation (e or E)
        if (!isAtEnd() && (current() == 'e' || current() == 'E')) {
            char savedChar = current();
            size_t savedPos = pos_;
            int savedLine = line_;
            int savedCol = column_;
            
            advance(); // consume e/E
            
            // Optional + or -
            if (!isAtEnd() && (current() == '+' || current() == '-')) {
                char sign = current();
                advance();
                
                if (isAtEnd() || !isDigit(current())) {
                    // Not a valid exponent, backtrack
                    pos_ = savedPos;
                    line_ = savedLine;
                    column_ = savedCol;
                } else {
                    isScientific = true;
                    value += savedChar;
                    value += sign;
                    while (!isAtEnd() && isDigit(current())) {
                        value += current();
                        advance();
                    }
                }
            } else if (!isAtEnd() && isDigit(current())) {
                isScientific = true;
                value += savedChar;
                while (!isAtEnd() && isDigit(current())) {
                    value += current();
                    advance();
                }
            } else {
                // Not a valid exponent, backtrack
                pos_ = savedPos;
                line_ = savedLine;
                column_ = savedCol;
            }
        }

        TokenType type = isScientific ? TokenType::Scientific : 
                        (isFloat ? TokenType::Float : TokenType::Integer);
        return Token(type, value, startLine, startColumn);
    }

    // --------------------------------------------------------
    // Identifier & keyword scanning
    // --------------------------------------------------------
    Token scanIdentifier(int startLine, int startColumn) {
        std::string value;

        while (!isAtEnd() && isAlphaNumeric(current())) {
            value += current();
            advance();
        }

        TokenType type = lookupKeyword(value);
        return Token(type, value, startLine, startColumn);
    }

    // --------------------------------------------------------
    // Keyword lookup table
    // --------------------------------------------------------
    static TokenType lookupKeyword(const std::string& word) {
        static const std::unordered_map<std::string, TokenType> keywords = {
            { "var",      TokenType::Var },
            { "const",    TokenType::Const },
            { "class",    TokenType::Class },
            { "struct",   TokenType::Struct },
            { "enum",     TokenType::Enum },
            { "public",   TokenType::Public },
            { "private",  TokenType::Private },
            { "static",   TokenType::Static },
            { "return",   TokenType::Return },
            { "if",       TokenType::If },
            { "else",     TokenType::Else },
            { "while",    TokenType::While },
            { "for",      TokenType::For },
            { "switch",   TokenType::Switch },
            { "break",    TokenType::Break },
            { "continue", TokenType::Continue },
            { "this",     TokenType::This },
            { "super",    TokenType::Super },
            { "new",      TokenType::New },
            { "void",     TokenType::Void },
            { "auto",     TokenType::Auto },
            { "true",     TokenType::True },
            { "false",    TokenType::False },
            { "null",     TokenType::Null },
            { "case",     TokenType::Case},
            { "default",  TokenType::Default},
            { "include",  TokenType::Include},
            { "in",       TokenType::In},
            // Type keywords
            { "int8",     TokenType::Int8 },
            { "int16",    TokenType::Int16 },
            { "int32",    TokenType::Int32 },
            { "int64",    TokenType::Int64 },
            { "uint8",    TokenType::UInt8 },
            { "uint16",   TokenType::UInt16 },
            { "uint32",   TokenType::UInt32 },
            { "uint64",   TokenType::UInt64 },
            { "float32",  TokenType::Float32 },
            { "float64",  TokenType::Float64 },
            { "bool",     TokenType::Bool },
            { "string",   TokenType::String_ },
        };

        auto it = keywords.find(word);
        if (it != keywords.end()) {
            return it->second;
        }
        return TokenType::Identifier;
    }

    // --------------------------------------------------------
    // Character classification
    // --------------------------------------------------------
    static bool isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    static bool isHexDigit(char c) {
        return (c >= '0' && c <= '9') ||
               (c >= 'a' && c <= 'f') ||
               (c >= 'A' && c <= 'F');
    }

    static bool isBinaryDigit(char c) {
        return c == '0' || c == '1';
    }

    static bool isAlpha(char c) {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               (c == '_');
    }

    static bool isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }
};

} // namespace scriptlang

#endif // LEXER_H

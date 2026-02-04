#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <string_view>
#include <ostream>

namespace scriptlang {

// All token types the lexer can produce.
enum class TokenType {
    // --------------- Literals ---------------
    Integer,        // 42, 0, -7
    HexInteger,     // 0xFF, 0x1A2B
    BinaryInteger,  // 0b1010, 0b11111111
    Float,          // 3.14, 0.0, -1.5
    Scientific,     // 1.5e-3, 2.0E+10
    String,         // "hello world"
    RawString,      // `raw string with ${no interpolation}`
    InterpolatedString, // "Hello ${name}" (will need special handling in parser)
    Char,           // 'a', '\n'
    True,           // true
    False,          // false
    Null,           // null

    // --------------- Identifiers & Keywords ---------------
    Identifier,     // any user-defined name

    // Keywords
    Var,            // var (for beginner/intermediate mode)
    Const,          // const
    Class,          // class
    Struct,         // struct
    Enum,           // enum
    Public,         // public
    Private,        // private
    Static,         // static
    Return,         // return
    If,             // if
    Else,           // else
    While,          // while
    For,            // for
    Switch,         // switch
    Break,          // break
    Continue,       // continue
    This,           // this
    Super,          // super
    New,            // new
    Void,           // void
    Auto,           // auto
    Case,           // case
    Default,        // default
    Include,        // include (or import - pick one!)
    In,             // in (for "for x in array" loops)

    // Type keywords
    Int8,           // int8
    Int16,          // int16
    Int32,          // int32
    Int64,          // int64
    UInt8,          // uint8
    UInt16,         // uint16
    UInt32,         // uint32
    UInt64,         // uint64
    Float32,        // float32
    Float64,        // float64
    Bool,           // bool
    String_,        // string (underscore to avoid collision with std::string)

    // --------------- Operators ---------------
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Question,       // ?
    QuestionQuestion, // ??
    DotQuestion,    // .?

    Assign,         // =
    PlusAssign,     // +=
    MinusAssign,    // -=
    StarAssign,     // *=
    SlashAssign,    // /=

    Equal,          // ==
    NotEqual,       // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=

    And,            // &&
    Or,             // ||
    Not,            // !

    // --------------- Delimiters ---------------
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]

    Semicolon,      // ;
    Comma,          // ,
    Dot,            // .
    DotDot,         // .. (range operator: 0..10)
    DotDotDot,      // ... (spread/rest operator)
    Colon,          // :
    Arrow,          // -> (function return type)
    LambdaArrow,    // => (lambda/arrow function)

    // --------------- Directives & Annotations ---------------
    Hash,           // # (start of directive like #include)
    At,             // @ (start of annotation like @expose)
    Dollar,         // $ (for string interpolation - marks start of ${...})

    // --------------- Special ---------------
    Backtick,       // ` (for raw strings)
    Eof,            // end of file
    Error,          // lexer error — carries the error message in `value`
};


// A single token produced by the lexer.
struct Token {
    TokenType    type;
    std::string  value;     // raw text of the token (e.g. "42", "hello", "playerHealth")
    int          line;      // 1-indexed line number
    int          column;    // 1-indexed column number

    Token() : type(TokenType::Eof), line(0), column(0) {}

    Token(TokenType type, std::string value, int line, int column)
        : type(type), value(std::move(value)), line(line), column(column) {}

    bool isError() const { return type == TokenType::Error; }
    bool isEof()   const { return type == TokenType::Eof; }
};


// Utility: convert a TokenType to a human-readable string (for debug/error messages)
inline std::string_view tokenTypeName(TokenType type) {
    switch (type) {
        case TokenType::Integer:        return "Integer";
        case TokenType::HexInteger:     return "HexInteger";
        case TokenType::BinaryInteger:  return "BinaryInteger";
        case TokenType::Float:          return "Float";
        case TokenType::Scientific:     return "Scientific";
        case TokenType::String:         return "String";
        case TokenType::RawString:      return "RawString";
        case TokenType::InterpolatedString: return "InterpolatedString";
        case TokenType::Char:           return "Char";
        case TokenType::True:           return "True";
        case TokenType::False:          return "False";
        case TokenType::Null:           return "Null";
        case TokenType::Identifier:     return "Identifier";
        case TokenType::Var:            return "var";
        case TokenType::Const:          return "const";
        case TokenType::Class:          return "class";
        case TokenType::Struct:         return "struct";
        case TokenType::Enum:           return "enum";
        case TokenType::Public:         return "public";
        case TokenType::Private:        return "private";
        case TokenType::Static:         return "static";
        case TokenType::Return:         return "return";
        case TokenType::If:             return "if";
        case TokenType::Else:           return "else";
        case TokenType::While:          return "while";
        case TokenType::For:            return "for";
        case TokenType::Break:          return "break";
        case TokenType::Continue:       return "continue";
        case TokenType::This:           return "this";
        case TokenType::Super:          return "super";
        case TokenType::New:            return "new";
        case TokenType::Void:           return "void";
        case TokenType::Auto:           return "auto";
        case TokenType::Int8:           return "int8";
        case TokenType::Int16:          return "int16";
        case TokenType::Int32:          return "int32";
        case TokenType::Int64:          return "int64";
        case TokenType::UInt8:          return "uint8";
        case TokenType::UInt16:         return "uint16";
        case TokenType::UInt32:         return "uint32";
        case TokenType::UInt64:         return "uint64";
        case TokenType::Float32:        return "float32";
        case TokenType::Float64:        return "float64";
        case TokenType::Bool:           return "bool";
        case TokenType::String_:        return "string";
        case TokenType::Include:        return "include";
        case TokenType::In:             return "in";
        case TokenType::Plus:           return "+";
        case TokenType::Minus:          return "-";
        case TokenType::Star:           return "*";
        case TokenType::Slash:          return "/";
        case TokenType::Percent:        return "%";
        case TokenType::Assign:         return "=";
        case TokenType::PlusAssign:     return "+=";
        case TokenType::MinusAssign:    return "-=";
        case TokenType::StarAssign:     return "*=";
        case TokenType::SlashAssign:    return "/=";
        case TokenType::Equal:          return "==";
        case TokenType::NotEqual:       return "!=";
        case TokenType::Less:           return "<";
        case TokenType::LessEqual:      return "<=";
        case TokenType::Greater:        return ">";
        case TokenType::GreaterEqual:   return ">=";
        case TokenType::And:            return "&&";
        case TokenType::Or:             return "||";
        case TokenType::Question:       return "?";
        case TokenType::QuestionQuestion: return "??";
        case TokenType::Default:        return "default";
        case TokenType::Case:           return "case";
        case TokenType::DotQuestion:    return ".?";
        case TokenType::Not:            return "!";
        case TokenType::LeftParen:      return "(";
        case TokenType::RightParen:     return ")";
        case TokenType::LeftBrace:      return "{";
        case TokenType::RightBrace:     return "}";
        case TokenType::LeftBracket:    return "[";
        case TokenType::RightBracket:   return "]";
        case TokenType::Semicolon:      return ";";
        case TokenType::Comma:          return ",";
        case TokenType::Dot:            return ".";
        case TokenType::DotDot:         return "..";
        case TokenType::DotDotDot:      return "...";
        case TokenType::Colon:          return ":";
        case TokenType::Arrow:          return "->";
        case TokenType::LambdaArrow:    return "=>";
        case TokenType::Hash:           return "#";
        case TokenType::At:             return "@";
        case TokenType::Dollar:         return "$";
        case TokenType::Backtick:       return "`";
        case TokenType::Eof:            return "<EOF>";
        case TokenType::Error:          return "<Error>";
        case TokenType::Switch:         return "switch";
    }
    return "<Unknown>";
}

// Stream support — needed so EXPECT_EQ can print TokenType in error messages.
inline std::ostream& operator<<(std::ostream& os, TokenType type) {
    return os << tokenTypeName(type);
}

} // namespace scriptlang

#endif // TOKEN_H

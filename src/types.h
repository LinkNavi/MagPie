#ifndef TYPES_H
#define TYPES_H

// ------------------------------------------------------------
// types.h — The complete type system for ScriptLang.
//
// Design goals:
//   1. Every type is heap-allocated exactly once and shared via
//      shared_ptr.  TypeContext is the factory/intern table — two
//      types that are structurally identical will always be the
//      same pointer, so equality checks are O(1).
//   2. The enum TypeKind mirrors the token-level primitives the
//      lexer already knows (int8 … float64, bool, string) and
//      adds the compound forms the semantic pass needs.
//   3. Nothing here depends on AST nodes.  The type checker owns
//      the mapping from AST → Type.
//
// How to add a new type later:
//   - Add a TypeKind enumerator.
//   - Add any extra fields you need to the Type struct (or a
//     sub-struct).
//   - Add a factory method to TypeContext.
//   - Update displayName() so error messages stay readable.
// ------------------------------------------------------------

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <functional>
#include <algorithm>
#include <sstream>
#include <optional>

namespace scriptlang {

// ============================================================
// Forward declarations
// ============================================================
struct Type;
using TypePtr = std::shared_ptr<Type>;
class  TypeContext;

// ============================================================
// TypeKind — the discriminator tag
// ============================================================
enum class TypeKind {
    // --- Primitives (match token keywords 1-to-1) ---
    Int8,
    Int16,
    Int32,         // default integer type
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,       // default float type
    Bool,
    String,

    // --- Void / special ---
    Void,          // functions that return nothing
    Null,          // the type of the `null` literal (bottom-ish)
    Auto,          // placeholder until inference fills it in

    // --- Compound ---
    Array,         // Array<T>  — elementType is set
    Optional,      // T?        — wraps one inner type
    Range,         // Range<T>  — start/end are the same type T

    // --- Callable ---
    Function,      // (T1, T2, …) -> R

    // --- User-defined ---
    Class,         // class Foo { … }
    Struct,        // struct Bar { … }
    Enum,          // enum Baz { … }

    // --- Sentinel ---
    Error,         // poison type — suppresses cascading errors
};

// ============================================================
// Type — the single struct that represents every type.
//
// Compound types carry extra data in the fields below; primitive
// types leave them empty.  The struct is intentionally flat — no
// inheritance hierarchy — because there are only ~25 variants
// and we want cache-friendly layout.
// ============================================================
struct Type {
    TypeKind kind;

    // --- User-defined name (Class / Struct / Enum) ---
    std::string name;          // e.g. "Player", "Color"

    // --- Array / Optional / Range: the single element type ---
    TypePtr elementType;       // Array<T>, Optional<T>, Range<T>

    // --- Function signature ---
    std::vector<TypePtr> paramTypes;   // argument types, in order
    TypePtr              returnType;   // what it returns
    bool                 isVariadic = false;  // last param is spread

    // --- Class/Struct member layout (populated by semantic pass) ---
    struct MemberInfo {
        std::string name;
        TypePtr     type;
        bool        isPublic;
        bool        isStatic;
        bool        isMethod;   // true = function member
    };
    std::vector<MemberInfo> members;   // only for Class/Struct

    // --- Enum entries (populated by semantic pass) ---
    struct EnumEntryInfo {
        std::string name;
        std::optional<int64_t> value;   // explicit value if given
    };
    std::vector<EnumEntryInfo> enumEntries;  // only for Enum

    // --- Inheritance ---
    TypePtr parentType;        // non-null for classes with `: Parent`

    // --------------------------------------------------------
    // Queries
    // --------------------------------------------------------
    bool isPrimitive()    const;
    bool isNumeric()      const;
    bool isInteger()      const;
    bool isFloating()     const;
    bool isSigned()       const;
    bool isUnsigned()     const;
    bool isNullable()     const;   // Optional, String, Class, Array, Null
    bool isCallable()     const { return kind == TypeKind::Function; }
    bool isUserDefined()  const;
    bool isVoid()         const { return kind == TypeKind::Void; }
    bool isError()        const { return kind == TypeKind::Error; }
    bool isAuto()         const { return kind == TypeKind::Auto; }

    /// Size in bytes on the target (0 = unknown / reference type).
    size_t sizeOf()       const;

    /// Human-readable name for error messages.  e.g. "int32",
    /// "Array<float64>", "(int32, string) -> bool"
    std::string displayName() const;

    // --------------------------------------------------------
    // Structural equality — used by TypeContext to intern.
    // Two types are equal iff they have the same kind AND all
    // kind-specific fields match (via pointer equality for
    // nested TypePtrs, which is safe because everything goes
    // through TypeContext).
    // --------------------------------------------------------
    bool structurallyEqual(const Type& other) const;

private:
    // Only TypeContext constructs Type objects.
    Type() : kind(TypeKind::Error) {}
    friend class TypeContext;
};

// ============================================================
// TypeContext — the intern table / factory.
//
// Create ONE instance per compilation unit and route all type
// creation through it.  Returned pointers are stable for the
// lifetime of the context.
// ============================================================
class TypeContext {
public:
    TypeContext();

    // --- Primitive singletons (no arguments needed) ---
    TypePtr Int8();
    TypePtr Int16();
    TypePtr Int32();
    TypePtr Int64();
    TypePtr UInt8();
    TypePtr UInt16();
    TypePtr UInt32();
    TypePtr UInt64();
    TypePtr Float32();
    TypePtr Float64();
    TypePtr Bool();
    TypePtr String();
    TypePtr Void();
    TypePtr Null();
    TypePtr Auto();
    TypePtr Error();

    // --- Compound type factories (intern automatically) ---
    TypePtr makeArray(TypePtr elementType);
    TypePtr makeOptional(TypePtr inner);
    TypePtr makeRange(TypePtr elementType);

    // --- Function type ---
    TypePtr makeFunction(std::vector<TypePtr> params,
                         TypePtr             returnType,
                         bool                isVariadic = false);

    // --- User-defined type stubs.
    //     Call once to register the name; fill in members /
    //     enumEntries afterwards by mutating the returned Type.
    //     Subsequent calls with the same name return the same ptr.
    // ---
    TypePtr makeClass(const std::string& name);
    TypePtr makeStruct(const std::string& name);
    TypePtr makeEnum(const std::string& name);

    // --- Lookup a user-defined type by name ---
    TypePtr lookupByName(const std::string& name) const;

    // --- Numeric promotion / coercion helpers ---
    /// Returns the "wider" of two numeric types, or nullptr if
    /// incompatible.  Follows C-like promotion rules:
    ///   int8 < int16 < int32 < int64
    ///   uint8 < uint16 < uint32 < uint64
    ///   any int + any float → float64 (safe widening)
    ///   float32 < float64
    TypePtr promoteNumeric(TypePtr a, TypePtr b) const;

    /// True if `from` can be implicitly converted to `to` without
    /// loss.  Examples: int32→int64 yes, int64→int32 no,
    /// Null→Optional<T> yes, T→Optional<T> yes.
    bool isImplicitlyConvertible(TypePtr from, TypePtr to) const;

    /// The default integer type (int32) and default float (float64).
    TypePtr defaultIntType()   const { return int32_; }
    TypePtr defaultFloatType() const { return float64_; }

private:
    // --- Primitive singletons ---
    TypePtr int8_;
    TypePtr int16_;
    TypePtr int32_;
    TypePtr int64_;
    TypePtr uint8_;
    TypePtr uint16_;
    TypePtr uint32_;
    TypePtr uint64_;
    TypePtr float32_;
    TypePtr float64_;
    TypePtr bool_;
    TypePtr string_;
    TypePtr void_;
    TypePtr null_;
    TypePtr auto_;
    TypePtr error_;

    // --- Intern tables for compound types ---
    // Key = displayName(), which is unique for structurally equal types.
    std::unordered_map<std::string, TypePtr> arrayIntern_;
    std::unordered_map<std::string, TypePtr> optionalIntern_;
    std::unordered_map<std::string, TypePtr> rangeIntern_;
    std::unordered_map<std::string, TypePtr> functionIntern_;

    // --- User-defined types, keyed by name ---
    std::unordered_map<std::string, TypePtr> userTypes_;

    // --- Helper: allocate a raw Type with a given kind ---
    static TypePtr allocType(TypeKind kind);

    // --- Numeric rank for promotion (higher = wider) ---
    static int numericRank(TypeKind k);
};

// ============================================================
// Type method implementations (inline — header-only)
// ============================================================

inline bool Type::isPrimitive() const {
    switch (kind) {
        case TypeKind::Int8:  case TypeKind::Int16:
        case TypeKind::Int32: case TypeKind::Int64:
        case TypeKind::UInt8: case TypeKind::UInt16:
        case TypeKind::UInt32: case TypeKind::UInt64:
        case TypeKind::Float32: case TypeKind::Float64:
        case TypeKind::Bool:  case TypeKind::String:
            return true;
        default:
            return false;
    }
}

inline bool Type::isInteger() const {
    switch (kind) {
        case TypeKind::Int8:  case TypeKind::Int16:
        case TypeKind::Int32: case TypeKind::Int64:
        case TypeKind::UInt8: case TypeKind::UInt16:
        case TypeKind::UInt32: case TypeKind::UInt64:
            return true;
        default:
            return false;
    }
}

inline bool Type::isFloating() const {
    return kind == TypeKind::Float32 || kind == TypeKind::Float64;
}

inline bool Type::isNumeric() const {
    return isInteger() || isFloating();
}

inline bool Type::isSigned() const {
    switch (kind) {
        case TypeKind::Int8:  case TypeKind::Int16:
        case TypeKind::Int32: case TypeKind::Int64:
        case TypeKind::Float32: case TypeKind::Float64:
            return true;
        default:
            return false;
    }
}

inline bool Type::isUnsigned() const {
    switch (kind) {
        case TypeKind::UInt8:  case TypeKind::UInt16:
        case TypeKind::UInt32: case TypeKind::UInt64:
            return true;
        default:
            return false;
    }
}

inline bool Type::isNullable() const {
    switch (kind) {
        case TypeKind::Optional:
        case TypeKind::String:
        case TypeKind::Class:
        case TypeKind::Array:
        case TypeKind::Null:
            return true;
        default:
            return false;
    }
}

inline bool Type::isUserDefined() const {
    return kind == TypeKind::Class ||
           kind == TypeKind::Struct ||
           kind == TypeKind::Enum;
}

inline size_t Type::sizeOf() const {
    switch (kind) {
        case TypeKind::Int8:   case TypeKind::UInt8:  case TypeKind::Bool:  return 1;
        case TypeKind::Int16:  case TypeKind::UInt16:                       return 2;
        case TypeKind::Int32:  case TypeKind::UInt32: case TypeKind::Float32: return 4;
        case TypeKind::Int64:  case TypeKind::UInt64: case TypeKind::Float64: return 8;
        case TypeKind::Struct: {
            // Structs are value types — sum of member sizes (no padding modeled yet)
            size_t total = 0;
            for (auto& m : members) {
                if (!m.isMethod && m.type) total += m.type->sizeOf();
            }
            return total;
        }
        // Reference types (classes, arrays, strings, functions) are pointers
        case TypeKind::Class:
        case TypeKind::Array:
        case TypeKind::String:
        case TypeKind::Function:
        case TypeKind::Optional:
            return 8; // pointer on 64-bit
        default:
            return 0;
    }
}

inline std::string Type::displayName() const {
    switch (kind) {
        case TypeKind::Int8:      return "int8";
        case TypeKind::Int16:     return "int16";
        case TypeKind::Int32:     return "int32";
        case TypeKind::Int64:     return "int64";
        case TypeKind::UInt8:     return "uint8";
        case TypeKind::UInt16:    return "uint16";
        case TypeKind::UInt32:    return "uint32";
        case TypeKind::UInt64:    return "uint64";
        case TypeKind::Float32:   return "float32";
        case TypeKind::Float64:   return "float64";
        case TypeKind::Bool:      return "bool";
        case TypeKind::String:    return "string";
        case TypeKind::Void:      return "void";
        case TypeKind::Null:      return "null";
        case TypeKind::Auto:      return "auto";
        case TypeKind::Error:     return "<error>";

        case TypeKind::Array: {
            return "Array<" + (elementType ? elementType->displayName() : "?") + ">";
        }
        case TypeKind::Optional: {
            return (elementType ? elementType->displayName() : "?") + "?";
        }
        case TypeKind::Range: {
            return "Range<" + (elementType ? elementType->displayName() : "?") + ">";
        }
        case TypeKind::Function: {
            std::string s = "(";
            for (size_t i = 0; i < paramTypes.size(); ++i) {
                if (i) s += ", ";
                if (isVariadic && i == paramTypes.size() - 1) s += "...";
                s += paramTypes[i] ? paramTypes[i]->displayName() : "?";
            }
            s += ") -> ";
            s += returnType ? returnType->displayName() : "?";
            return s;
        }
        case TypeKind::Class:
        case TypeKind::Struct:
        case TypeKind::Enum:
            return name;
    }
    return "<unknown>";
}

inline bool Type::structurallyEqual(const Type& other) const {
    if (kind != other.kind) return false;

    switch (kind) {
        // Primitives + Void/Null/Auto/Error: kind alone is enough
        case TypeKind::Int8:  case TypeKind::Int16:
        case TypeKind::Int32: case TypeKind::Int64:
        case TypeKind::UInt8: case TypeKind::UInt16:
        case TypeKind::UInt32: case TypeKind::UInt64:
        case TypeKind::Float32: case TypeKind::Float64:
        case TypeKind::Bool: case TypeKind::String:
        case TypeKind::Void: case TypeKind::Null:
        case TypeKind::Auto: case TypeKind::Error:
            return true;

        // Compound: compare element types by pointer (already interned)
        case TypeKind::Array:
        case TypeKind::Optional:
        case TypeKind::Range:
            return elementType == other.elementType;

        // Function: params + return + variadic flag
        case TypeKind::Function:
            if (returnType != other.returnType) return false;
            if (isVariadic != other.isVariadic) return false;
            return paramTypes == other.paramTypes;

        // User-defined: identity is by name
        case TypeKind::Class:
        case TypeKind::Struct:
        case TypeKind::Enum:
            return name == other.name;
    }
    return false;
}

// ============================================================
// TypeContext implementations (inline — header-only)
// ============================================================

inline TypePtr TypeContext::allocType(TypeKind kind) {
    auto t = std::shared_ptr<Type>(new Type());
    t->kind = kind;
    return t;
}

inline TypeContext::TypeContext() {
    int8_      = allocType(TypeKind::Int8);
    int16_     = allocType(TypeKind::Int16);
    int32_     = allocType(TypeKind::Int32);
    int64_     = allocType(TypeKind::Int64);
    uint8_     = allocType(TypeKind::UInt8);
    uint16_    = allocType(TypeKind::UInt16);
    uint32_    = allocType(TypeKind::UInt32);
    uint64_    = allocType(TypeKind::UInt64);
    float32_   = allocType(TypeKind::Float32);
    float64_   = allocType(TypeKind::Float64);
    bool_      = allocType(TypeKind::Bool);
    string_    = allocType(TypeKind::String);
    void_      = allocType(TypeKind::Void);
    null_      = allocType(TypeKind::Null);
    auto_      = allocType(TypeKind::Auto);
    error_     = allocType(TypeKind::Error);
}

// --- Primitive accessors ---
inline TypePtr TypeContext::Int8()     { return int8_; }
inline TypePtr TypeContext::Int16()    { return int16_; }
inline TypePtr TypeContext::Int32()    { return int32_; }
inline TypePtr TypeContext::Int64()    { return int64_; }
inline TypePtr TypeContext::UInt8()    { return uint8_; }
inline TypePtr TypeContext::UInt16()   { return uint16_; }
inline TypePtr TypeContext::UInt32()   { return uint32_; }
inline TypePtr TypeContext::UInt64()   { return uint64_; }
inline TypePtr TypeContext::Float32()  { return float32_; }
inline TypePtr TypeContext::Float64()  { return float64_; }
inline TypePtr TypeContext::Bool()     { return bool_; }
inline TypePtr TypeContext::String()   { return string_; }
inline TypePtr TypeContext::Void()     { return void_; }
inline TypePtr TypeContext::Null()     { return null_; }
inline TypePtr TypeContext::Auto()     { return auto_; }
inline TypePtr TypeContext::Error()    { return error_; }

// --- Compound factories ---
inline TypePtr TypeContext::makeArray(TypePtr elem) {
    std::string key = "Array<" + elem->displayName() + ">";
    auto it = arrayIntern_.find(key);
    if (it != arrayIntern_.end()) return it->second;

    auto t = allocType(TypeKind::Array);
    t->elementType = elem;
    arrayIntern_[key] = t;
    return t;
}

inline TypePtr TypeContext::makeOptional(TypePtr inner) {
    std::string key = inner->displayName() + "?";
    auto it = optionalIntern_.find(key);
    if (it != optionalIntern_.end()) return it->second;

    auto t = allocType(TypeKind::Optional);
    t->elementType = inner;
    optionalIntern_[key] = t;
    return t;
}

inline TypePtr TypeContext::makeRange(TypePtr elem) {
    std::string key = "Range<" + elem->displayName() + ">";
    auto it = rangeIntern_.find(key);
    if (it != rangeIntern_.end()) return it->second;

    auto t = allocType(TypeKind::Range);
    t->elementType = elem;
    rangeIntern_[key] = t;
    return t;
}

inline TypePtr TypeContext::makeFunction(std::vector<TypePtr> params,
                                         TypePtr ret,
                                         bool variadic) {
    // Build a unique key
    std::string key = "(";
    for (size_t i = 0; i < params.size(); ++i) {
        if (i) key += ",";
        if (variadic && i == params.size() - 1) key += "...";
        key += params[i]->displayName();
    }
    key += ")->" + ret->displayName();

    auto it = functionIntern_.find(key);
    if (it != functionIntern_.end()) return it->second;

    auto t = allocType(TypeKind::Function);
    t->paramTypes  = std::move(params);
    t->returnType  = ret;
    t->isVariadic  = variadic;
    functionIntern_[key] = t;
    return t;
}

// --- User-defined type factories ---
inline TypePtr TypeContext::makeClass(const std::string& name) {
    auto it = userTypes_.find(name);
    if (it != userTypes_.end()) return it->second;

    auto t = allocType(TypeKind::Class);
    t->name = name;
    userTypes_[name] = t;
    return t;
}

inline TypePtr TypeContext::makeStruct(const std::string& name) {
    auto it = userTypes_.find(name);
    if (it != userTypes_.end()) return it->second;

    auto t = allocType(TypeKind::Struct);
    t->name = name;
    userTypes_[name] = t;
    return t;
}

inline TypePtr TypeContext::makeEnum(const std::string& name) {
    auto it = userTypes_.find(name);
    if (it != userTypes_.end()) return it->second;

    auto t = allocType(TypeKind::Enum);
    t->name = name;
    userTypes_[name] = t;
    return t;
}

inline TypePtr TypeContext::lookupByName(const std::string& name) const {
    auto it = userTypes_.find(name);
    return (it != userTypes_.end()) ? it->second : nullptr;
}

// --- Numeric promotion ---
inline int TypeContext::numericRank(TypeKind k) {
    // Rank table — higher number = wider type.
    // Floats live in a separate "family" but we handle the
    // cross-family case explicitly in promoteNumeric.
    switch (k) {
        case TypeKind::Int8:   case TypeKind::UInt8:   return 1;
        case TypeKind::Int16:  case TypeKind::UInt16:  return 2;
        case TypeKind::Int32:  case TypeKind::UInt32:  return 3;
        case TypeKind::Int64:  case TypeKind::UInt64:  return 4;
        case TypeKind::Float32:                        return 5;
        case TypeKind::Float64:                        return 6;
        default:                                       return 0;
    }
}

inline TypePtr TypeContext::promoteNumeric(TypePtr a, TypePtr b) const {
    if (!a || !b || !a->isNumeric() || !b->isNumeric()) return nullptr;

    // If either is floating, result is at least float32; if either
    // is float64 the result is float64.
    bool aFloat = a->isFloating();
    bool bFloat = b->isFloating();

    if (aFloat || bFloat) {
        if (a->kind == TypeKind::Float64 || b->kind == TypeKind::Float64)
            return float64_;
        return float64_;   // int + float32 → float64 (safe widening)
    }

    // Both integers — pick the wider one.  If one is signed and the
    // other unsigned at the same width we widen to the next signed.
    int ra = numericRank(a->kind);
    int rb = numericRank(b->kind);

    if (ra == rb) {
        // Same rank.  If sign differs, widen.
        if (a->isSigned() != b->isSigned()) {
            // Widen to next rank signed
            switch (ra) {
                case 1: return int16_;
                case 2: return int32_;
                case 3: return int64_;
                default: return int64_;  // can't go wider
            }
        }
        return a; // same type
    }

    return (ra > rb) ? a : b;
}

inline bool TypeContext::isImplicitlyConvertible(TypePtr from, TypePtr to) const {
    if (!from || !to) return false;
    if (from == to)   return true;   // pointer equality — interned

    // Null is implicitly convertible to any nullable type
    if (from->kind == TypeKind::Null && to->isNullable()) return true;

    // T is implicitly convertible to Optional<T>
    if (to->kind == TypeKind::Optional && to->elementType == from) return true;

    // Numeric widening: smaller → larger within compatible families
    if (from->isNumeric() && to->isNumeric()) {
        TypePtr promoted = promoteNumeric(from, to);
        return promoted == to;
    }

    // Class → parent class (if parentType chain contains `to`)
    if (from->kind == TypeKind::Class && to->kind == TypeKind::Class) {
        TypePtr cur = from->parentType;
        while (cur) {
            if (cur == to) return true;
            cur = cur->parentType;
        }
    }

    return false;
}

} // namespace scriptlang

#endif // TYPES_H

//
// Usage:
//     #define SCRIPTLANG_IMPLEMENTATION
//     #include "frontend_enhanced.h"
// ============================================================

#ifndef SCRIPTLANG_FRONTEND_ENHANCED_H
#define SCRIPTLANG_FRONTEND_ENHANCED_H

#include <cstdint>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <unordered_map>
#include <vector>

namespace ScriptLang {

// ============================================================
// Forward Declarations
// ============================================================
namespace Internal {
class RuntimeImpl;
class ModuleImpl;
class TypeRegistry;
} // namespace Internal

// ============================================================
// Value - Enhanced with object handles
// ============================================================
class Value {
public:
  enum class Type {
    Void,
    Int32,
    Int64,
    Float32,
    Float64,
    Bool,
    String,
    Null,
    Object // For game objects/components
  };

  Value() : type_(Type::Void) { data_.i64 = 0; }
  Value(int32_t v) : type_(Type::Int32) { data_.i32 = v; }
  Value(int64_t v) : type_(Type::Int64) { data_.i64 = v; }
  Value(float v) : type_(Type::Float32) { data_.f32 = v; }
  Value(double v) : type_(Type::Float64) { data_.f64 = v; }
  Value(bool v) : type_(Type::Bool) { data_.b = v; }
  Value(const char *v) : type_(Type::String), stringData_(v) {}
  Value(const std::string &v) : type_(Type::String), stringData_(v) {}

  // Object handle constructor
  template <typename T>
  static Value Object(T *ptr, const std::string &typeName = "") {
    Value v;
    v.type_ = Type::Object;
    v.data_.ptr = static_cast<void *>(ptr);
    v.objectTypeName_ = typeName.empty() ? typeid(T).name() : typeName;
    return v;
  }

  static Value Null() {
    Value v;
    v.type_ = Type::Null;
    return v;
  }

  Type getType() const { return type_; }
  bool isInt32() const { return type_ == Type::Int32; }
  bool isInt64() const { return type_ == Type::Int64; }
  bool isFloat32() const { return type_ == Type::Float32; }
  bool isFloat64() const { return type_ == Type::Float64; }
  bool isBool() const { return type_ == Type::Bool; }
  bool isString() const { return type_ == Type::String; }
  bool isVoid() const { return type_ == Type::Void; }
  bool isNull() const { return type_ == Type::Null; }
  bool isObject() const { return type_ == Type::Object; }

  int32_t asInt32() const {
    if (type_ != Type::Int32)
      throw std::runtime_error("Value is not int32");
    return data_.i32;
  }
  int64_t asInt64() const {
    if (type_ != Type::Int64)
      throw std::runtime_error("Value is not int64");
    return data_.i64;
  }
  float asFloat32() const {
    if (type_ != Type::Float32)
      throw std::runtime_error("Value is not float32");
    return data_.f32;
  }
  double asFloat64() const {
    if (type_ != Type::Float64)
      throw std::runtime_error("Value is not float64");
    return data_.f64;
  }
  bool asBool() const {
    if (type_ != Type::Bool)
      throw std::runtime_error("Value is not bool");
    return data_.b;
  }
  std::string asString() const {
    if (type_ != Type::String)
      throw std::runtime_error("Value is not string");
    return stringData_;
  }

  template <typename T> T *asObject() const {
    if (type_ != Type::Object)
      throw std::runtime_error("Value is not object");
    return static_cast<T *>(data_.ptr);
  }

  void *asObjectPtr() const {
    if (type_ != Type::Object)
      throw std::runtime_error("Value is not object");
    return data_.ptr;
  }

  const std::string &getObjectTypeName() const { return objectTypeName_; }

private:
  Type type_;
  union {
    int32_t i32;
    int64_t i64;
    float f32;
    double f64;
    bool b;
    void *ptr;
  } data_;
  std::string stringData_;
  std::string objectTypeName_; // For LSP type information
};

// ============================================================
// Function Metadata (for LSP)
// ============================================================
struct ParameterInfo {
  std::string name;
  std::string type;
  std::string defaultValue;
  std::string description;
};

struct FunctionMetadata {
  std::string name;
  std::string returnType;
  std::vector<ParameterInfo> parameters;
  std::string documentation;
  std::string category; // e.g. "Entity", "Transform", "Physics"
};

// ============================================================
// Component/Type Metadata (for LSP)
// ============================================================
struct PropertyInfo {
  std::string name;
  std::string type;
  bool readOnly;
  std::string documentation;
};

struct MethodInfo {
  std::string name;
  std::string returnType;
  std::vector<ParameterInfo> parameters;
  std::string documentation;
};

struct TypeMetadata {
  std::string name;
  std::string baseType; // Parent type if any
  std::vector<PropertyInfo> properties;
  std::vector<MethodInfo> methods;
  std::string documentation;
};

// ============================================================
// Diagnostic
// ============================================================
enum class DiagnosticLevel { Error, Warning, Note };

struct Diagnostic {
  DiagnosticLevel level;
  std::string message;
  int line;
  int column;

  Diagnostic(DiagnosticLevel lvl, std::string msg, int l = 0, int c = 0)
      : level(lvl), message(std::move(msg)), line(l), column(c) {}
};

// ============================================================
// Module
// ============================================================
class Module {
public:
  Module();
  ~Module();

  bool isValid() const;
  bool hasErrors() const;
  const std::vector<Diagnostic> &getDiagnostics() const;
  std::vector<std::string> getExportedFunctions() const;
  std::string getIR() const;

private:
  friend class Runtime;
  std::unique_ptr<Internal::ModuleImpl> impl_;
};

// ============================================================
// Function Binding Types
// ============================================================
using NativeFunction = std::function<Value(const std::vector<Value> &)>;

// Function thunk for proper calling convention
template <typename Ret, typename... Args> struct FunctionThunk;

// Specialization for void return
template <typename... Args> struct FunctionThunk<void, Args...> {
  template <typename F>
  static NativeFunction wrap(F &&func, const FunctionMetadata &meta = {}) {
    return [func = std::forward<F>(func),
            meta](const std::vector<Value> &args) -> Value {
      if (args.size() != sizeof...(Args)) {
        throw std::runtime_error("Wrong number of arguments");
      }
      std::apply(
          func, convertArgs<Args...>(args, std::index_sequence_for<Args...>{}));
      return Value();
    };
  }

private:
  template <typename... ConvArgs, size_t... Is>
  static std::tuple<ConvArgs...> convertArgs(const std::vector<Value> &args,
                                             std::index_sequence<Is...>) {
    return std::make_tuple(convertArg<ConvArgs>(args[Is])...);
  }

  template <typename T> static T convertArg(const Value &v) {
    if constexpr (std::is_same_v<T, int32_t>)
      return v.asInt32();
    else if constexpr (std::is_same_v<T, int64_t>)
      return v.asInt64();
    else if constexpr (std::is_same_v<T, float>)
      return v.asFloat32();
    else if constexpr (std::is_same_v<T, double>)
      return v.asFloat64();
    else if constexpr (std::is_same_v<T, bool>)
      return v.asBool();
    else if constexpr (std::is_same_v<T, std::string>)
      return v.asString();
    else if constexpr (std::is_pointer_v<T>)
      return v.asObject<std::remove_pointer_t<T>>();
    else
      return T{};
  }
};

// Specialization for non-void return
template <typename Ret, typename... Args> struct FunctionThunk {
  template <typename F>
  static NativeFunction wrap(F &&func, const FunctionMetadata &meta = {}) {
    return [func = std::forward<F>(func),
            meta](const std::vector<Value> &args) -> Value {
      if (args.size() != sizeof...(Args)) {
        throw std::runtime_error("Wrong number of arguments");
      }
      auto result = std::apply(
          func, convertArgs<Args...>(args, std::index_sequence_for<Args...>{}));
      return convertResult<Ret>(result);
    };
  }

private:
  template <typename... ConvArgs, size_t... Is>
  static std::tuple<ConvArgs...> convertArgs(const std::vector<Value> &args,
                                             std::index_sequence<Is...>) {
    return std::make_tuple(convertArg<ConvArgs>(args[Is])...);
  }

  template <typename T> static T convertArg(const Value &v) {
    if constexpr (std::is_same_v<T, int32_t>)
      return v.asInt32();
    else if constexpr (std::is_same_v<T, int64_t>)
      return v.asInt64();
    else if constexpr (std::is_same_v<T, float>)
      return v.asFloat32();
    else if constexpr (std::is_same_v<T, double>)
      return v.asFloat64();
    else if constexpr (std::is_same_v<T, bool>)
      return v.asBool();
    else if constexpr (std::is_same_v<T, std::string>)
      return v.asString();
    else if constexpr (std::is_pointer_v<T>)
      return v.asObject<std::remove_pointer_t<T>>();
    else
      return T{};
  }

  template <typename T> static Value convertResult(const T &result) {
    if constexpr (std::is_same_v<T, int32_t>)
      return Value(result);
    else if constexpr (std::is_same_v<T, int64_t>)
      return Value(result);
    else if constexpr (std::is_same_v<T, float>)
      return Value(result);
    else if constexpr (std::is_same_v<T, double>)
      return Value(result);
    else if constexpr (std::is_same_v<T, bool>)
      return Value(result);
    else if constexpr (std::is_same_v<T, std::string>)
      return Value(result);
    else if constexpr (std::is_pointer_v<T>)
      return Value::Object(result);
    else
      return Value();
  }
};

// ============================================================
// Component Binder - Easy component exposure
// ============================================================
template <typename ComponentType> class ComponentBinder {
public:
  ComponentBinder(const std::string &typeName) : typeName_(typeName) {
    metadata_.name = typeName;
  }

  // Bind a property (getter/setter)
  template <typename T>
  ComponentBinder &property(const std::string &name, T ComponentType::*member,
                            const std::string &doc = "") {
    PropertyInfo info;
    info.name = name;
    info.type = getTypeName<T>();
    info.readOnly = false;
    info.documentation = doc;
    metadata_.properties.push_back(info);

    // Store getter/setter
    properties_[name] = {[member](void *obj) -> Value {
                           auto *comp = static_cast<ComponentType *>(obj);
                           return Value(comp->*member);
                         },
                         [member](void *obj, const Value &val) {
                           auto *comp = static_cast<ComponentType *>(obj);
                           comp->*member = convertValue<T>(val);
                         }};

    return *this;
  }

template<typename Getter, typename Setter>
ComponentBinder& property(const std::string& name,
                         Getter&& getter,
                         Setter&& setter,
                         const std::string& doc = "") {
    using T = std::invoke_result_t<Getter, ComponentType*>;
    
    PropertyInfo info;
    info.name = name;
    info.type = getTypeName<T>();
    info.readOnly = false;
    info.documentation = doc;
    metadata_.properties.push_back(info);
    
    properties_[name] = {
        [getter = std::forward<Getter>(getter)](void* obj) -> Value {
            auto* comp = static_cast<ComponentType*>(obj);
            return Value(getter(comp));
        },
        [setter = std::forward<Setter>(setter)](void* obj, const Value& val) {
            auto* comp = static_cast<ComponentType*>(obj);
            setter(comp, convertValue<T>(val));
        }
    };
    
    return *this;
}

// In ComponentBinder class, add this overload:
template<typename T>
ComponentBinder& property(const std::string& name,
                         std::function<T(ComponentType*)> getter,
                         std::function<void(ComponentType*, T)> setter,
                         const std::string& doc = "") {
    PropertyInfo info;
    info.name = name;
    info.type = getTypeName<T>();
    info.readOnly = false;
    info.documentation = doc;
    metadata_.properties.push_back(info);
    
    properties_[name] = {
        [getter](void* obj) -> Value {
            auto* comp = static_cast<ComponentType*>(obj);
            return Value(getter(comp));
        },
        [setter](void* obj, const Value& val) {
            auto* comp = static_cast<ComponentType*>(obj);
            setter(comp, convertValue<T>(val));
        }
    };
    
    return *this;
}
  // Bind a read-only property
  template <typename T>
  ComponentBinder &propertyReadOnly(const std::string &name,
                                    T ComponentType::*member,
                                    const std::string &doc = "") {
    PropertyInfo info;
    info.name = name;
    info.type = getTypeName<T>();
    info.readOnly = true;
    info.documentation = doc;
    metadata_.properties.push_back(info);

    properties_[name] = {[member](void *obj) -> Value {
                           auto *comp = static_cast<ComponentType *>(obj);
                           return Value(comp->*member);
                         },
                         nullptr};

    return *this;
  }

  // Bind a method
  template <typename Ret, typename... Args>
  ComponentBinder &method(const std::string &name,
                          Ret (ComponentType::*func)(Args...),
                          const std::string &doc = "") {
    MethodInfo info;
    info.name = name;
    info.returnType = getTypeName<Ret>();
    info.documentation = doc;
    metadata_.methods.push_back(info);

    methods_[name] = [func](void *obj,
                            const std::vector<Value> &args) -> Value {
      auto *comp = static_cast<ComponentType *>(obj);
      return invokeMethod(comp, func, args, std::index_sequence_for<Args...>{});
    };

    return *this;
  }
  template <typename Ret, typename... Args>
  ComponentBinder &method(const std::string &name,
                          Ret (ComponentType::*func)(Args...)
                              const, // const method
                          const std::string &doc = "") {
    MethodInfo info;
    info.name = name;
    info.returnType = getTypeName<Ret>();
    info.documentation = doc;
    metadata_.methods.push_back(info);

    methods_[name] = [func](void *obj,
                            const std::vector<Value> &args) -> Value {
      auto *comp = static_cast<ComponentType *>(obj);
      return invokeConstMethod(comp, func, args,
                               std::index_sequence_for<Args...>{});
    };

    return *this;
  }
  const TypeMetadata &getMetadata() const { return metadata_; }

  Value getProperty(void *obj, const std::string &name) const {
    auto it = properties_.find(name);
    if (it == properties_.end())
      throw std::runtime_error("Property not found: " + name);
    return it->second.getter(obj);
  }

 void setProperty(void *obj, const std::string &name,
                 const Value &val) const {  // Add const here
  auto it = properties_.find(name);
  if (it == properties_.end())
    throw std::runtime_error("Property not found: " + name);
  if (!it->second.setter)
    throw std::runtime_error("Property is read-only: " + name);
  it->second.setter(obj, val);
}

// Around line 391, add const qualifier to callMethod:
Value callMethod(void *obj, const std::string &name,
                 const std::vector<Value> &args) const {  // Add const here
  auto it = methods_.find(name);
  if (it == methods_.end())
    throw std::runtime_error("Method not found: " + name);
  return it->second(obj, args);
}

private:
  std::string typeName_;
  TypeMetadata metadata_;

  struct PropertyAccessor {
    std::function<Value(void *)> getter;
    std::function<void(void *, const Value &)> setter;
  };

  std::unordered_map<std::string, PropertyAccessor> properties_;
  std::unordered_map<std::string,
                     std::function<Value(void *, const std::vector<Value> &)>>
      methods_;

  template <typename T> static std::string getTypeName() {
    if constexpr (std::is_same_v<T, int32_t>)
      return "int32";
    else if constexpr (std::is_same_v<T, float>)
      return "float32";
    else if constexpr (std::is_same_v<T, double>)
      return "float64";
    else if constexpr (std::is_same_v<T, bool>)
      return "bool";
    else if constexpr (std::is_same_v<T, std::string>)
      return "string";
    else
      return typeid(T).name();
  }

  template <typename T> static T convertValue(const Value &val) {
    if constexpr (std::is_same_v<T, int32_t>)
      return val.asInt32();
    else if constexpr (std::is_same_v<T, float>)
      return val.asFloat32();
    else if constexpr (std::is_same_v<T, double>)
      return val.asFloat64();
    else if constexpr (std::is_same_v<T, bool>)
      return val.asBool();
    else if constexpr (std::is_same_v<T, std::string>)
      return val.asString();
    else
      return T{};
  }

  template <typename Ret, typename... Args, size_t... Is>
  static Value
  invokeMethod(ComponentType *obj, Ret (ComponentType::*func)(Args...),
               const std::vector<Value> &args, std::index_sequence<Is...>) {
    if constexpr (std::is_void_v<Ret>) {
      (obj->*func)(convertValue<Args>(args[Is])...);
      return Value();
    } else {
      return Value((obj->*func)(convertValue<Args>(args[Is])...));
    }
  }
};

// ============================================================
// Runtime - Enhanced with game engine features
// ============================================================
class Runtime {
public:
  Runtime();
  ~Runtime();

  // --------------------------------------------------------
  // Initialization
  // --------------------------------------------------------
  bool initialize();
  bool isInitialized() const;

  // --------------------------------------------------------
  // Execution
  // --------------------------------------------------------
  bool execute(const std::string &sourceCode);
  bool executeFile(const std::string &filepath);
  Module compile(const std::string &sourceCode);
  Module compileFile(const std::string &filepath);
  bool loadModule(const Module &module, const std::string &name = "main");
  void unloadModule(const std::string &moduleName);
  bool hasModule(const std::string &moduleName) const;

  // --------------------------------------------------------
  // Function Calls - Enhanced with proper thunking
  // --------------------------------------------------------
  Value callFunction(const std::string &functionName);
  Value callFunction(const std::string &functionName,
                     const std::vector<Value> &args);
  Value callFunction(const std::string &moduleName,
                     const std::string &functionName,
                     const std::vector<Value> &args = {});

  // --------------------------------------------------------
  // Function Binding - Enhanced with type-safe wrappers
  // --------------------------------------------------------

  // Raw binding
  void bindFunction(const std::string &name, NativeFunction func);
  void bindFunction(const std::string &name, std::function<void()> func);

  // Type-safe binding with metadata
  template <typename Ret, typename... Args>
  void bindFunction(const std::string &name, Ret (*func)(Args...),
                    const FunctionMetadata &meta = {}) {
    auto wrapped = FunctionThunk<Ret, Args...>::wrap(func, meta);
    bindFunction(name, wrapped);
    registerFunctionMetadata(name, meta);
  }

  // Lambda binding
  template <typename F>
  void bindFunction(const std::string &name, F &&func,
                    const FunctionMetadata &meta = {}) {
    bindFunction(name, NativeFunction(std::forward<F>(func)));
    registerFunctionMetadata(name, meta);
  }

  void bindPrintf();

  // --------------------------------------------------------
  // Component/Type Registration (for LSP)
  // --------------------------------------------------------
  template <typename ComponentType>
  ComponentBinder<ComponentType> registerType(const std::string &typeName,
                                              const std::string &doc = "") {
    ComponentBinder<ComponentType> binder(typeName);
    TypeMetadata meta;
    meta.name = typeName;
    meta.documentation = doc;
    registerTypeMetadata(typeName, meta);
    return binder;
  }

  void registerTypeMetadata(const std::string &typeName,
                            const TypeMetadata &meta);
  void registerFunctionMetadata(const std::string &funcName,
                                const FunctionMetadata &meta);

  // --------------------------------------------------------
  // Component Access
  // --------------------------------------------------------
  template <typename ComponentType>
  void bindComponentAccess(const std::string &typeName,
                           const ComponentBinder<ComponentType> &binder) {
    // Store binder for property/method access
    componentBinders_[typeName] = std::make_shared<ComponentBinderBase>(
        [binder](void *obj, const std::string &prop) {
          return binder.getProperty(obj, prop);
        },
        [binder](void *obj, const std::string &prop, const Value &val) {
          binder.setProperty(obj, prop, val);
        },
        [binder](void *obj, const std::string &method,
                 const std::vector<Value> &args) {
          return binder.callMethod(obj, method, args);
        });
  }

  // --------------------------------------------------------
  // LSP Query Interface
  // --------------------------------------------------------
  std::vector<FunctionMetadata> getAllFunctions() const;
  std::vector<TypeMetadata> getAllTypes() const;
  FunctionMetadata getFunctionMetadata(const std::string &name) const;
  TypeMetadata getTypeMetadata(const std::string &name) const;
  std::vector<std::string> getCompletionsAt(const std::string &file, int line,
                                            int col) const;

  // --------------------------------------------------------
  // Variable Access
  // --------------------------------------------------------
  Value getGlobal(const std::string &name) const;
  void setGlobal(const std::string &name, const Value &value);
  bool hasGlobal(const std::string &name) const;

  // --------------------------------------------------------
  // Memory Management
  // --------------------------------------------------------
  void setFrameArenaSize(size_t bytes);
  void setPersistentArenaSize(size_t bytes);
  void resetFrameArena();

  struct MemoryStats {
    size_t frameArenaUsed;
    size_t frameArenaCapacity;
    size_t persistentArenaUsed;
    size_t persistentArenaCapacity;
  };
  MemoryStats getMemoryStats() const;

  // --------------------------------------------------------
  // Hot Reload
  // --------------------------------------------------------
  void enableHotReload(const std::string &moduleName,
                       const std::string &filepath);
  void disableHotReload(const std::string &moduleName);
  bool reloadModule(const std::string &moduleName);
  using ReloadCallback =
      std::function<void(const std::string &moduleName, bool success)>;
  void setReloadCallback(ReloadCallback callback);

  // --------------------------------------------------------
  // Error Handling
  // --------------------------------------------------------
  std::string getLastError() const;
  bool hasErrors() const;
  const std::vector<Diagnostic> &getDiagnostics() const;
  void clearErrors();

  // --------------------------------------------------------
  // Configuration
  // --------------------------------------------------------
  void setOptimizationLevel(int level);
  void setJITEnabled(bool enabled);
  void setExecutionTimeout(uint64_t timeoutMs);

  // --------------------------------------------------------
  // Debugging
  // --------------------------------------------------------
  std::string dumpModuleIR(const std::string &moduleName) const;
  std::vector<std::string> getLoadedModules() const;
  std::vector<std::string> getFunctions(const std::string &moduleName) const;
  std::vector<std::string> getGlobals(const std::string &moduleName) const;

private:
  std::unique_ptr<Internal::RuntimeImpl> impl_;
  template <typename ComponentType, typename Ret, typename... Args, size_t... Is>
  static Value invokeConstMethod(const ComponentType *obj, // const obj
                                 Ret (ComponentType::*func)(Args...) const,
                                 const std::vector<Value> &args,
                                 std::index_sequence<Is...>) {
    if constexpr (std::is_void_v<Ret>) {
      (obj->*func)(convertValue<Args>(args[Is])...);
      return Value();
    } else {
      return Value((obj->*func)(convertValue<Args>(args[Is])...));
    }
  }
  struct ComponentBinderBase {
    std::function<Value(void *, const std::string &)> getProp;
    std::function<void(void *, const std::string &, const Value &)> setProp;
    std::function<Value(void *, const std::string &,
                        const std::vector<Value> &)>
        callMethod;

template<typename G, typename S, typename C>
    ComponentBinderBase(G &&get, S &&set, C &&call)
        : getProp(get), setProp(set), callMethod(call) {}
  };

  std::unordered_map<std::string, std::shared_ptr<ComponentBinderBase>>
      componentBinders_;
  std::unordered_map<std::string, FunctionMetadata> functionMetadata_;
  std::unordered_map<std::string, TypeMetadata> typeMetadata_;
};

// ============================================================
// Helper Macros
// ============================================================

// Bind typed function
#define SCRIPTLANG_BIND(runtime, name, func, ...)                              \
  runtime.bindFunction(name, func, ##__VA_ARGS__)

// Bind with documentation
#define SCRIPTLANG_BIND_DOC(runtime, name, func, doc)                          \
  runtime.bindFunction(name, func,                                             \
                       ScriptLang::FunctionMetadata{name, "", {}, doc})

// Register component type
#define SCRIPTLANG_REGISTER_TYPE(runtime, Type, doc)                           \
  runtime.registerType<Type>(#Type, doc)

// ============================================================
// Version
// ============================================================
struct Version {
  int major;
  int minor;
  int patch;
  std::string toString() const;
};

Version getVersion();
std::string getLLVMVersion();

// ============================================================
// Convenience
// ============================================================
inline bool execute(const std::string &sourceCode) {
  Runtime runtime;
  runtime.initialize();
  return runtime.execute(sourceCode);
}

inline bool executeFile(const std::string &filepath) {
  Runtime runtime;
  runtime.initialize();
  return runtime.executeFile(filepath);
}

} // namespace ScriptLang

#endif // SCRIPTLANG_FRONTEND_ENHANCED_H

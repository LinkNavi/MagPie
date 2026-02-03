// ============================================================
// Minimal Example - Quick Start
// ============================================================

#define SCRIPTLANG_IMPLEMENTATION
#include "frontend.h"
#include <iostream>

// Simple component
class Transform {
public:
    float x, y, z;
    Transform() : x(0), y(0), z(0) {}
    
    void Move(float dx, float dy, float dz) {
        x += dx; y += dy; z += dz;
        std::cout << "Position: (" << x << ", " << y << ", " << z << ")\n";
    }
};

// Simple entity
class Entity {
public:
    Transform* transform = new Transform();
    ~Entity() { delete transform; }
    Transform* GetTransform() { return transform; }
};

Entity* g_entity = nullptr;

// Engine functions
Entity* GetEntity() { return g_entity; }
void Log(const std::string& msg) { std::cout << "[Log] " << msg << "\n"; }

int main() {
    g_entity = new Entity();
    
    ScriptLang::Runtime runtime;
    runtime.initialize();
    
    // Register components
    auto transformBinder = runtime.registerType<Transform>("Transform")
        .property("x", &Transform::x)
        .property("y", &Transform::y)
        .property("z", &Transform::z)
        .method("Move", &Transform::Move);
    
    auto entityBinder = runtime.registerType<Entity>("Entity")
        .method("GetTransform", &Entity::GetTransform);
    
    runtime.bindComponentAccess("Transform", transformBinder);
    runtime.bindComponentAccess("Entity", entityBinder);
    
    // Bind functions
    runtime.bindFunction("GetEntity", &GetEntity);
    runtime.bindFunction("Log", &Log);
    
    // Run script
    runtime.execute(R"(
        var entity = GetEntity();
        var transform = entity.GetTransform();
        
        Log("Initial position: " + transform.x);
        
        transform.Move(10.0, 20.0, 5.0);
        transform.x = 100.0;
        
        Log("Final X: " + transform.x);
    )");
    
    delete g_entity;
    return 0;
}

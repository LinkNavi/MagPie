// ============================================================
// Game Engine Integration Example
// ============================================================
// Shows:
// 1. Component binding (Transform, RigidBody)
// 2. Entity system integration
// 3. Calling component methods from scripts
// 4. Property access from scripts
// 5. LSP-ready metadata
// ============================================================

#define SCRIPTLANG_IMPLEMENTATION
#include "frontend.h"
#include <iostream>
#include <cmath>

// ============================================================
// Example Game Engine Components
// ============================================================

struct Vector3 {
    float x, y, z;
    Vector3(float x = 0, float y = 0, float z = 0) : x(x), y(y), z(z) {}
};

class Transform {
public:
    Vector3 position;
    Vector3 rotation;
    Vector3 scale;
    
    Transform() : scale(1, 1, 1) {}
    
    void Translate(float x, float y, float z) {
        position.x += x;
        position.y += y;
        position.z += z;
        std::cout << "[Transform] Moved to (" << position.x << ", " 
                  << position.y << ", " << position.z << ")\n";
    }
    
    void SetPosition(float x, float y, float z) {
        position.x = x;
        position.y = y;
        position.z = z;
    }
    
    float GetDistance(Transform* other) const {
        float dx = position.x - other->position.x;
        float dy = position.y - other->position.y;
        float dz = position.z - other->position.z;
        return std::sqrt(dx*dx + dy*dy + dz*dz);
    }
};

class RigidBody {
public:
    Vector3 velocity;
    float mass;
    bool useGravity;
    
    RigidBody() : mass(1.0f), useGravity(true) {}
    
    void AddForce(float x, float y, float z) {
        velocity.x += x / mass;
        velocity.y += y / mass;
        velocity.z += z / mass;
        std::cout << "[RigidBody] Applied force, velocity now (" 
                  << velocity.x << ", " << velocity.y << ", " << velocity.z << ")\n";
    }
    
    void SetVelocity(float x, float y, float z) {
        velocity.x = x;
        velocity.y = y;
        velocity.z = z;
    }
    
    float GetSpeed() const {
        return std::sqrt(velocity.x*velocity.x + velocity.y*velocity.y + velocity.z*velocity.z);
    }
};

class Entity {
public:
    int id;
    std::string name;
    Transform* transform;
    RigidBody* rigidBody;
    
    Entity(int id, const std::string& name) 
        : id(id), name(name), transform(new Transform()), rigidBody(nullptr) {}
    
    ~Entity() {
        delete transform;
        delete rigidBody;
    }
    
    Transform* GetTransform() { return transform; }
    
    RigidBody* GetRigidBody() { 
        if (!rigidBody) rigidBody = new RigidBody();
        return rigidBody; 
    }
    
    void Destroy() {
        std::cout << "[Entity] Destroying " << name << "\n";
    }
};

// ============================================================
// Engine Functions
// ============================================================

Entity* g_player = nullptr;
Entity* g_enemy = nullptr;

Entity* GetPlayer() {
    return g_player;
}

Entity* GetEnemy() {
    return g_enemy;
}

void PrintMessage(const std::string& msg) {
    std::cout << "[Script] " << msg << "\n";
}

float DeltaTime() {
    return 0.016f;  // Simulated 60 FPS
}

// ============================================================
// Main
// ============================================================

int main() {
    std::cout << "=== ScriptLang Game Engine Integration ===\n\n";
    
    // Create entities
    g_player = new Entity(1, "Player");
    g_player->GetTransform()->SetPosition(0, 0, 0);
    
    g_enemy = new Entity(2, "Enemy");
    g_enemy->GetTransform()->SetPosition(10, 0, 5);
    g_enemy->GetRigidBody();  // Add RigidBody
    
    // Setup runtime
    ScriptLang::Runtime runtime;
    runtime.initialize();
    
    // --------------------------------------------------------
    // Register Component Types (for LSP)
    // --------------------------------------------------------
   auto transformBinder = runtime.registerType<Transform>("Transform", "3D transformation component")
    .property("x", 
        [](Transform* t) { return t->position.x; },
        [](Transform* t, float v) { t->position.x = v; })
    .property("y",
        [](Transform* t) { return t->position.y; },
        [](Transform* t, float v) { t->position.y = v; })
    .property("z",
        [](Transform* t) { return t->position.z; },
        [](Transform* t, float v) { t->position.z = v; })
        .method("Translate", &Transform::Translate, "Move by offset")
        .method("SetPosition", &Transform::SetPosition, "Set absolute position")
        .method("GetDistance", &Transform::GetDistance, "Get distance to another transform");
    
    auto rigidBodyBinder = runtime.registerType<RigidBody>("RigidBody", "Physics body component")
        .property("mass", &RigidBody::mass, "Mass in kg")
        .property("useGravity", &RigidBody::useGravity, "Should gravity affect this body")
        .method("AddForce", &RigidBody::AddForce, "Apply force to the body")
        .method("SetVelocity", &RigidBody::SetVelocity, "Set velocity directly")
        .method("GetSpeed", &RigidBody::GetSpeed, "Get current speed");
    
    auto entityBinder = runtime.registerType<Entity>("Entity", "Game entity")
        .propertyReadOnly("id", &Entity::id, "Entity unique ID")
        .propertyReadOnly("name", &Entity::name, "Entity name")
        .method("GetTransform", &Entity::GetTransform, "Get Transform component")
        .method("GetRigidBody", &Entity::GetRigidBody, "Get or create RigidBody component")
        .method("Destroy", &Entity::Destroy, "Destroy this entity");
    
    // Bind component access
    runtime.bindComponentAccess("Transform", transformBinder);
    runtime.bindComponentAccess("RigidBody", rigidBodyBinder);
    runtime.bindComponentAccess("Entity", entityBinder);
    
    // --------------------------------------------------------
    // Bind Engine Functions
    // --------------------------------------------------------
    runtime.bindFunction("GetPlayer", &GetPlayer, 
        ScriptLang::FunctionMetadata{"GetPlayer", "Entity", {}, "Get the player entity", "Entity"});
    
    runtime.bindFunction("GetEnemy", &GetEnemy,
        ScriptLang::FunctionMetadata{"GetEnemy", "Entity", {}, "Get the enemy entity", "Entity"});
    
    runtime.bindFunction("Print", &PrintMessage,
        ScriptLang::FunctionMetadata{"Print", "void", {{"msg", "string", "", "Message to print"}}, "Print a message", "Debug"});
    
    runtime.bindFunction("DeltaTime", &DeltaTime,
        ScriptLang::FunctionMetadata{"DeltaTime", "float32", {}, "Time since last frame in seconds", "Time"});
    
    // --------------------------------------------------------
    // Execute Script
    // --------------------------------------------------------
    std::cout << "Running script...\n\n";
    
    bool success = runtime.execute(R"(
        # Get entities
        var player = GetPlayer();
        var enemy = GetEnemy();
        
        Print("Player ID: " + player.id);
        Print("Enemy ID: " + enemy.id);
        
        # Access components
        var playerTransform = player.GetTransform();
        var enemyTransform = enemy.GetTransform();
        var enemyRigidBody = enemy.GetRigidBody();
        
        # Move player
        Print("Moving player...");
        playerTransform.Translate(5.0, 0.0, 3.0);
        
        # Check distance
        var distance = playerTransform.GetDistance(enemyTransform);
        Print("Distance to enemy: " + distance);
        
        # Apply physics to enemy
        Print("Applying force to enemy...");
        enemyRigidBody.AddForce(100.0, 50.0, 0.0);
        
        var speed = enemyRigidBody.GetSpeed();
        Print("Enemy speed: " + speed);
        
        # Modify properties
        enemyRigidBody.mass = 2.0;
        enemyRigidBody.useGravity = false;
        
        Print("Script complete!");
    )");
    
    if (!success) {
        std::cout << "\nERROR: " << runtime.getLastError() << "\n";
        for (const auto& diag : runtime.getDiagnostics()) {
            std::cout << "  Line " << diag.line << ": " << diag.message << "\n";
        }
    }
    
    std::cout << "\n";
    
    // --------------------------------------------------------
    // LSP Information Export
    // --------------------------------------------------------
    std::cout << "=== LSP Metadata ===\n\n";
    
    std::cout << "Registered Types:\n";
    for (const auto& type : runtime.getAllTypes()) {
        std::cout << "  " << type.name << ": " << type.documentation << "\n";
        for (const auto& prop : type.properties) {
            std::cout << "    - " << prop.name << ": " << prop.type 
                     << (prop.readOnly ? " (read-only)" : "") << "\n";
        }
        for (const auto& method : type.methods) {
            std::cout << "    - " << method.name << "() -> " << method.returnType << "\n";
        }
    }
    
    std::cout << "\nGlobal Functions:\n";
    for (const auto& func : runtime.getAllFunctions()) {
        std::cout << "  " << func.name << "() -> " << func.returnType;
        if (!func.category.empty()) std::cout << " [" << func.category << "]";
        std::cout << "\n";
        if (!func.documentation.empty()) {
            std::cout << "    " << func.documentation << "\n";
        }
    }
    
    // Cleanup
    delete g_player;
    delete g_enemy;
    
    return 0;
}

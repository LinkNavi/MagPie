// ============================================================
// Enhanced ScriptLang Syntax Examples for Game Development
// Demonstrates all the new lexer features
// ============================================================

#include <engine/core>
#include <engine/physics>
#include <engine/console> { Print, Log }

// ============================================================
// LAMBDAS & CALLBACKS
// ============================================================

public class Button {
    private auto onClick = null;
    
    void setOnClick(auto callback) {
        onClick = callback;
    }
    
    void click() {
        if (onClick != null) {
            onClick();
        }
    }
}

// Usage with lambda
var button = new Button();
button.setOnClick(() => {
    Print("Button clicked!");
});

// Lambda with parameters
var numbers = [1, 2, 3, 4, 5];
var doubled = numbers.map((x) => x * 2);
var filtered = numbers.filter((x) => x > 2);

// Multi-line lambda
var processPlayer = (player) => {
    player.heal(10);
    player.addScore(100);
    return player;
};

// ============================================================
// RANGE OPERATORS
// ============================================================

// Range-based for loop
for i in 0..10 {
    Print("Count: ${i}");
}

// Exclusive range (if your parser implements it differently)
for i in 0..100 {
    spawnEnemy(i);
}

// Array slicing (future parser feature)
// var subset = array[0..5];

// ============================================================
// HEX LITERALS (Colors, Flags, etc.)
// ============================================================

// Color constants
const COLOR_RED     = 0xFF0000;
const COLOR_GREEN   = 0x00FF00;
const COLOR_BLUE    = 0x0000FF;
const COLOR_WHITE   = 0xFFFFFF;
const COLOR_BLACK   = 0x000000;
const COLOR_PURPLE  = 0x9B59B6;

public class Sprite {
    @expose(tooltip="Tint color in RGB hex format")
    private uint32 tintColor = 0xFFFFFF;
    
    void setColor(uint32 color) {
        tintColor = color;
    }
}

// Bit masks
const PERMISSION_READ    = 0x01;
const PERMISSION_WRITE   = 0x02;
const PERMISSION_EXECUTE = 0x04;
const PERMISSION_ALL     = 0xFF;

// ============================================================
// BINARY LITERALS (Flags, Bit Manipulation)
// ============================================================

// Entity component flags
const FLAG_RENDERABLE   = 0b00000001;
const FLAG_COLLIDABLE   = 0b00000010;
const FLAG_INTERACTIVE  = 0b00000100;
const FLAG_SCRIPTABLE   = 0b00001000;
const FLAG_NETWORKED    = 0b00010000;

public class Entity {
    private uint8 flags = 0b00000000;
    
    void setFlags(uint8 newFlags) {
        flags = flags | newFlags;  // Bitwise OR
    }
    
    bool hasFlag(uint8 flag) {
        return (flags & flag) != 0;
    }
}

// Input key states (bit field)
const INPUT_UP    = 0b0001;
const INPUT_DOWN  = 0b0010;
const INPUT_LEFT  = 0b0100;
const INPUT_RIGHT = 0b1000;

// ============================================================
// SCIENTIFIC NOTATION (Physics Constants)
// ============================================================

// Physics constants
const GRAVITY_EARTH     = 9.81;
const GRAVITY_MOON      = 1.62;
const SPEED_OF_LIGHT    = 3.0e8;        // 300,000,000 m/s
const PLANCK_CONSTANT   = 6.626e-34;    // Very small number
const AVOGADRO_NUMBER   = 6.022e23;     // Very large number

// Game physics
const EPSILON           = 1.0e-6;       // Floating point comparison threshold
const MAX_VELOCITY      = 1.0e4;        // Maximum entity velocity
const MASS_ELECTRON     = 9.109e-31;    // For particle systems

public class PhysicsBody {
    private float64 mass = 1.0;
    private float64 drag = 1.2e-3;      // Air resistance coefficient
    
    void applyForce(float64 force) {
        // F = ma, so a = F/m
        float64 acceleration = force / mass;
        
        // Account for very small forces
        if (acceleration < EPSILON) {
            return;
        }
        
        // Apply acceleration...
    }
}

// ============================================================
// CHARACTER LITERALS (Input Handling)
// ============================================================

public class InputManager {
    private char lastKey = '\0';
    
    void onKeyPress(char key) {
        lastKey = key;
        
        // Check specific keys
        if (key == 'w' || key == 'W') {
            moveForward();
        } else if (key == 's' || key == 'S') {
            moveBackward();
        } else if (key == 'a' || key == 'A') {
            moveLeft();
        } else if (key == 'd' || key == 'D') {
            moveRight();
        } else if (key == ' ') {
            jump();
        } else if (key == '\n') {
            sendChatMessage();
        } else if (key == '\t') {
            toggleInventory();
        }
    }
    
    bool isMovementKey(char key) {
        return key == 'w' || key == 'a' || key == 's' || key == 'd';
    }
}

// ============================================================
// RAW STRINGS (Shader Code, JSON, etc.)
// ============================================================

// Vertex shader
const VERTEX_SHADER = `
    #version 330 core
    layout (location = 0) in vec3 aPos;
    layout (location = 1) in vec2 aTexCoord;
    
    out vec2 TexCoord;
    
    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;
    
    void main() {
        gl_Position = projection * view * model * vec4(aPos, 1.0);
        TexCoord = aTexCoord;
    }
`;

// Fragment shader
const FRAGMENT_SHADER = `
    #version 330 core
    out vec4 FragColor;
    in vec2 TexCoord;
    
    uniform sampler2D texture1;
    uniform vec4 tintColor;
    
    void main() {
        FragColor = texture(texture1, TexCoord) * tintColor;
    }
`;

// JSON configuration (could be parsed later)
const CONFIG_JSON = `{
    "window": {
        "width": 1920,
        "height": 1080,
        "fullscreen": false
    },
    "graphics": {
        "vsync": true,
        "antialiasing": 4,
        "shadows": "high"
    }
}`;

// Regular expressions or file paths
const SAVE_PATH = `C:\Users\Player\SaveData\game.sav`;
const ASSET_PATH = `./assets/textures/player.png`;

// ============================================================
// STRING INTERPOLATION (Debug Messages, UI)
// ============================================================

public class Player {
    @expose(min=0, max=100)
    private int32 health = 100;
    
    @expose
    private string name = "Hero";
    
    private int32 score = 0;
    private int32 level = 1;
    
    void takeDamage(int32 amount) {
        health -= amount;
        
        // Debug logging with interpolation
        Log("${name} took ${amount} damage. Health: ${health}");
        
        if (health <= 0) {
            Log("${name} died at level ${level} with score ${score}");
            onDeath();
        } else if (health <= 20) {
            Log("Warning: ${name}'s health is critically low!");
        }
    }
    
    void addScore(int32 points) {
        score += points;
        Print("Score: ${score} (+${points})");
    }
    
    string getStatus() {
        return "Player: ${name} | HP: ${health}/100 | Level: ${level} | Score: ${score}";
    }
}

// UI text with interpolation
void updateUI() {
    var player = getPlayer();
    var fps = getFPS();
    var ping = getNetworkPing();
    
    ui.setText("statusLabel", "${player.name} - HP: ${player.health}");
    ui.setText("fpsLabel", "FPS: ${fps}");
    ui.setText("pingLabel", "Ping: ${ping}ms");
}

// ============================================================
// SPREAD OPERATOR (Function Arguments)
// ============================================================

// Pass array elements as individual arguments
void spawnEnemies(...positions) {
    for pos in positions {
        var enemy = new Enemy();
        enemy.setPosition(pos);
    }
}

// Usage
var positions = [
    new Vector3(10, 0, 5),
    new Vector3(-5, 0, 10),
    new Vector3(0, 0, -8)
];
spawnEnemies(...positions);

// Merge arrays
var weapons = ["sword", "bow"];
var items = ["potion", "key"];
var allItems = [...weapons, ...items];

// ============================================================
// NULL COALESCING & OPTIONAL CHAINING
// ============================================================

public class GameManager {
    private Player player = null;
    private string defaultName = "Player";
    
    string getPlayerName() {
        // Null coalescing: use default if player is null
        return player?.name ?? defaultName;
    }
    
    void updatePlayer() {
        // Optional chaining: only call if player exists
        player?.update();
        player?.render();
        
        // Safe navigation through chain
        var weaponName = player?.inventory?.activeWeapon?.name ?? "none";
        Log("Active weapon: ${weaponName}");
    }
}

// ============================================================
// COMPREHENSIVE EXAMPLE: Combat System
// ============================================================

public class CombatSystem {
    private const MAX_ENEMIES = 100;
    private const DAMAGE_MULTIPLIER = 1.5e0;
    
    // Color-coded damage numbers
    private const COLOR_NORMAL_DAMAGE = 0xFFFFFF;
    private const COLOR_CRITICAL_DAMAGE = 0xFF0000;
    private const COLOR_HEAL = 0x00FF00;
    
    // Combat flags
    private const FLAG_CAN_CRIT     = 0b00000001;
    private const FLAG_IGNORE_ARMOR = 0b00000010;
    private const FLAG_LIFESTEAL    = 0b00000100;
    
    void dealDamage(Entity attacker, Entity target, float32 baseDamage, uint8 flags) {
        var finalDamage = baseDamage;
        
        // Check for critical hit
        if ((flags & FLAG_CAN_CRIT) != 0) {
            var critChance = 0.2;  // 20%
            if (Math.random() < critChance) {
                finalDamage *= DAMAGE_MULTIPLIER;
                showDamageNumber(target, finalDamage, COLOR_CRITICAL_DAMAGE);
                Log("${attacker.name} scored a CRITICAL HIT on ${target.name}!");
            } else {
                showDamageNumber(target, finalDamage, COLOR_NORMAL_DAMAGE);
            }
        }
        
        // Apply damage
        target.health -= finalDamage;
        
        // Lifesteal
        if ((flags & FLAG_LIFESTEAL) != 0) {
            var healAmount = finalDamage * 0.2;  // 20% lifesteal
            attacker.health += healAmount;
            showDamageNumber(attacker, healAmount, COLOR_HEAL);
        }
        
        Log("Combat: ${attacker.name} dealt ${finalDamage} damage to ${target.name}");
        
        // Check for death
        if (target.health <= 0) {
            onEntityDeath(target);
        }
    }
    
    void onEntityDeath(Entity entity) {
        // Play death animation
        entity.playAnimation(`death_anim_${entity.type}`);
        
        // Spawn loot with spread operator
        if (entity.loot != null) {
            spawnItems(...entity.loot);
        }
        
        // Schedule destruction with lambda
        Timer.after(2.0, () => {
            destroy(entity);
            Log("Entity ${entity.name} destroyed");
        });
    }
    
    // Event system with lambdas
    void setupCombatEvents() {
        // On player attack
        Events.on('attack', (attacker, target) => {
            dealDamage(attacker, target, attacker.attackPower, FLAG_CAN_CRIT);
        });
        
        // On enemy spawn
        Events.on('enemySpawn', (enemy) => {
            Log("Enemy spawned: ${enemy.name} at level ${enemy.level}");
        });
        
        // On level complete
        Events.on('levelComplete', () => {
            var enemies = getEnemiesInRange(0..MAX_ENEMIES);
            for enemy in enemies {
                enemy.despawn();
            }
        });
    }
}

// ============================================================
// INITIALIZATION
// ============================================================

@lifecycle(phase=init)
void onGameStart() {
    Print("Game started!");
    
    // Initialize with hex color
    var backgroundColor = 0x1E1E1E;
    Graphics.setClearColor(backgroundColor);
    
    // Load shader
    var shader = new Shader(VERTEX_SHADER, FRAGMENT_SHADER);
    
    // Set physics constants
    Physics.setGravity(9.81);
    Physics.setTimeStep(1.0e-2);  // 10ms timestep
    
    // Configure input
    Input.onKey('q', () => { quit(); });
    Input.onKey('\x1B', () => { showMenu(); });  // ESC key
    
    Log("Initialization complete!");
}

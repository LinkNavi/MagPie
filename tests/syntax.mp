// --- imports: selective importing to avoid collisions ---
#include <engine/time>
#include <engine/physics>
#include <engine/console> { Print, Log }

// --- compile mode: jit is default, so no annotation needed unless overriding ---
@compile(aot)

// --- permissions: restricts FROM a project-level default, rather than granting ---
@permissions(restrict=[networking])

public class Player {

    // --- @expose fleshed out, constraints match the actual types ---
    @expose(min=0, max=100, step=1, tooltip="The player's health (0 = dead)", group="Stats")
    private int32 playerHealth = 100;

    @expose(maxLength=32, tooltip="Display name shown above player", group="Identity")
    public string playerName = "Bob";

    // --- lifecycle hooks: all annotation-driven, no magic method names ---
    @lifecycle(phase=init)
    private void onInit() {
        // initial setup
    }

    @lifecycle(phase=late_init)
    private void onLateInit() {
        // runs after all objects have initialized
    }

    @lifecycle(phase=update)
    private void onUpdate() {
        if (isDead()) {
            Print("man im dead");
        }
    }

    @lifecycle(phase=fixed_update)
    private void onFixedUpdate() {
        // physics-synced logic here
    }

    @lifecycle(phase=destroy)
    private void onDestroy() {
        // cleanup
    }

    // --- regular methods: private by default if omitted ---
    int32 getHealth() {
        return playerHealth;
    }

    bool isDead() {
        return getHealth() <= 0;
    }
}


// ============================================================
// --- lightweight standalone script mode (no class needed) ---
// ============================================================

#include <engine/console> { Print }

@lifecycle(phase=update)
void onUpdate() {
    Print("I am a simple script, no class needed.");
}

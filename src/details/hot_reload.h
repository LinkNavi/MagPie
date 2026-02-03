// hot_reload.h - Advanced hot-reload system with state preservation
// for ScriptLang JIT compiler

#ifndef HOT_RELOAD_H
#define HOT_RELOAD_H

#include "jit_compiler.h"
#include <string>
#include <unordered_map>
#include <vector>
#include <functional>
#include <chrono>
#include <thread>
#include <atomic>
#include <fstream>
#ifdef _WIN32
    #include <windows.h>
#else
    #include <sys/inotify.h>
    #include <unistd.h>
    #include <poll.h>
#endif

namespace scriptlang {

// ============================================================
// StateSerializer - Serialize/deserialize script state
// ============================================================
class StateSerializer {
public:
    // State is stored as key-value pairs (variable name -> raw bytes)
    using StateMap = std::unordered_map<std::string, std::vector<uint8_t>>;
    
    /// Save state by calling a script's __save_state() function
    static StateMap saveState(JITCompiler& jit, const std::string& moduleName) {
        StateMap state;
        
        // Look up the __save_state() function
        auto* saveFunc = jit.lookupFunction(moduleName + "_SaveState");
        if (!saveFunc) {
            // No state saving function - that's okay
            return state;
        }
        
        // Call it (returns a pointer to serialized data)
        // In practice, you'd define a protocol for this
        // For example, return JSON or a binary format
        
        // Placeholder implementation
        return state;
    }
    
    /// Restore state by calling a script's __load_state() function
    static void restoreState(JITCompiler& jit, 
                           const std::string& moduleName,
                           const StateMap& state) {
        auto* loadFunc = jit.lookupFunction(moduleName + "_LoadState");
        if (!loadFunc) return;
        
        // Call the load function with the state data
        // Placeholder implementation
    }
    
    /// Serialize a single variable (POD types)
    template<typename T>
    static std::vector<uint8_t> serialize(const T& value) {
        std::vector<uint8_t> bytes(sizeof(T));
        std::memcpy(bytes.data(), &value, sizeof(T));
        return bytes;
    }
    
    /// Deserialize a single variable
    template<typename T>
    static T deserialize(const std::vector<uint8_t>& bytes) {
        T value;
        std::memcpy(&value, bytes.data(), std::min(sizeof(T), bytes.size()));
        return value;
    }
};

// ============================================================
// FileWatcher - Cross-platform file change monitoring
// ============================================================
class FileWatcher {
public:
    using ChangeCallback = std::function<void(const std::string& path)>;
    
    FileWatcher() : running_(false) {
#ifndef _WIN32
        inotifyFd_ = inotify_init1(IN_NONBLOCK);
#endif
    }
    
    ~FileWatcher() {
        stop();
#ifndef _WIN32
        if (inotifyFd_ >= 0) {
            close(inotifyFd_);
        }
#endif
    }
    
    /// Add a file to watch
    void watchFile(const std::string& path, ChangeCallback callback) {
        std::lock_guard<std::mutex> lock(mutex_);
        
        watches_[path] = callback;
        
#ifdef _WIN32
        // Windows implementation using ReadDirectoryChangesW
        // Store last modification time
        lastModTimes_[path] = getFileModTime(path);
#else
        // Linux/macOS implementation using inotify
        int wd = inotify_add_watch(inotifyFd_, path.c_str(), 
                                   IN_MODIFY | IN_CLOSE_WRITE);
        if (wd >= 0) {
            watchDescriptors_[wd] = path;
        }
#endif
    }
    
    /// Start watching in background thread
    void start() {
        if (running_) return;
        
        running_ = true;
        watchThread_ = std::thread(&FileWatcher::watchLoop, this);
    }
    
    /// Stop watching
    void stop() {
        if (!running_) return;
        
        running_ = false;
        if (watchThread_.joinable()) {
            watchThread_.join();
        }
    }
    
    /// Poll for changes once (for manual polling instead of background thread)
    void poll() {
#ifdef _WIN32
        // Check modification times
        std::lock_guard<std::mutex> lock(mutex_);
        for (auto& [path, callback] : watches_) {
            auto currentTime = getFileModTime(path);
            if (currentTime > lastModTimes_[path]) {
                lastModTimes_[path] = currentTime;
                callback(path);
            }
        }
#else
        // Check inotify events
        char buffer[4096];
        ssize_t len = read(inotifyFd_, buffer, sizeof(buffer));
        
        if (len > 0) {
            std::lock_guard<std::mutex> lock(mutex_);
            
            for (char* ptr = buffer; ptr < buffer + len;) {
                struct inotify_event* event = (struct inotify_event*)ptr;
                
                auto it = watchDescriptors_.find(event->wd);
                if (it != watchDescriptors_.end()) {
                    const std::string& path = it->second;
                    auto callbackIt = watches_.find(path);
                    if (callbackIt != watches_.end()) {
                        callbackIt->second(path);
                    }
                }
                
                ptr += sizeof(struct inotify_event) + event->len;
            }
        }
#endif
    }

private:
    std::unordered_map<std::string, ChangeCallback> watches_;
    std::unordered_map<std::string, uint64_t> lastModTimes_;
    
#ifdef _WIN32
    // Windows-specific members would go here
#else
    int inotifyFd_;
    std::unordered_map<int, std::string> watchDescriptors_;
#endif
    
    std::thread watchThread_;
    std::atomic<bool> running_;
    std::mutex mutex_;
    
    void watchLoop() {
        while (running_) {
            poll();
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }
    }
    
    uint64_t getFileModTime(const std::string& path) {
#ifdef _WIN32
        WIN32_FILE_ATTRIBUTE_DATA fileInfo;
        if (GetFileAttributesExA(path.c_str(), GetFileExInfoStandard, &fileInfo)) {
            ULARGE_INTEGER ull;
            ull.LowPart = fileInfo.ftLastWriteTime.dwLowDateTime;
            ull.HighPart = fileInfo.ftLastWriteTime.dwHighDateTime;
            return ull.QuadPart;
        }
        return 0;
#else
        struct stat fileInfo;
        if (stat(path.c_str(), &fileInfo) == 0) {
            return fileInfo.st_mtime;
        }
        return 0;
#endif
    }
};

// ============================================================
// HotReloadManager - High-level hot-reload orchestration
// ============================================================
class HotReloadManager {
public:
    HotReloadManager(JITCompiler& jit) : jit_(jit) {}
    
    /// Register a script for hot-reloading
    void registerScript(const std::string& moduleName, 
                       const std::string& sourcePath) {
        scriptPaths_[moduleName] = sourcePath;
        
        // Set up file watcher
        watcher_.watchFile(sourcePath, [this, moduleName](const std::string& path) {
            onFileChanged(moduleName, path);
        });
    }
    
    /// Start watching files
    void startWatching() {
        watcher_.start();
    }
    
    /// Stop watching files
    void stopWatching() {
        watcher_.stop();
    }
    
    /// Manually check for changes (alternative to background watching)
    void checkForChanges() {
        watcher_.poll();
    }
    
    /// Set a callback to be notified when a reload happens
    using ReloadCallback = std::function<void(const std::string& moduleName, bool success)>;
    void setReloadCallback(ReloadCallback callback) {
        reloadCallback_ = callback;
    }
    
    /// Get reload statistics
    struct ReloadStats {
        size_t totalReloads;
        size_t successfulReloads;
        size_t failedReloads;
        std::chrono::milliseconds averageReloadTime;
    };
    
    ReloadStats getStats() const {
        ReloadStats stats;
        stats.totalReloads = totalReloads_;
        stats.successfulReloads = successfulReloads_;
        stats.failedReloads = failedReloads_;
        
        if (totalReloads_ > 0) {
            stats.averageReloadTime = std::chrono::milliseconds(
                totalReloadTimeMs_ / totalReloads_);
        }
        
        return stats;
    }

private:
    JITCompiler& jit_;
    FileWatcher watcher_;
    std::unordered_map<std::string, std::string> scriptPaths_;
    std::unordered_map<std::string, StateSerializer::StateMap> savedStates_;
    
    ReloadCallback reloadCallback_;
    
    // Statistics
    size_t totalReloads_ = 0;
    size_t successfulReloads_ = 0;
    size_t failedReloads_ = 0;
    uint64_t totalReloadTimeMs_ = 0;
    
    void onFileChanged(const std::string& moduleName, const std::string& path) {
        std::cout << "[Hot-Reload] File changed: " << path << std::endl;
        std::cout << "[Hot-Reload] Reloading module: " << moduleName << std::endl;
        
        auto startTime = std::chrono::high_resolution_clock::now();
        
        // Save current state
        auto state = StateSerializer::saveState(jit_, moduleName);
        savedStates_[moduleName] = state;
        
        // Read the new source code
        std::ifstream file(path);
        if (!file) {
            std::cerr << "[Hot-Reload] Failed to read file: " << path << std::endl;
            failedReloads_++;
            if (reloadCallback_) {
                reloadCallback_(moduleName, false);
            }
            return;
        }
        
        std::stringstream buffer;
        buffer << file.rdbuf();
        std::string sourceCode = buffer.str();
        
        // Recompile (you'll need to implement the full pipeline here)
        // This is where you'd call Lexer → Parser → Analyzer → TypeChecker → CodeGen
        TypeContext types;
        SymbolTable symbols;
        
     
bool success = jit_.reloadModule(moduleName, sourceCode);

        
        if (success) {
            // Restore state
            StateSerializer::restoreState(jit_, moduleName, state);
            
            std::cout << "[Hot-Reload] Successfully reloaded: " << moduleName << std::endl;
            successfulReloads_++;
        } else {
            std::cerr << "[Hot-Reload] Failed to reload: " << moduleName << std::endl;
            failedReloads_++;
        }
        
        totalReloads_++;
        
        auto endTime = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
            endTime - startTime).count();
        totalReloadTimeMs_ += duration;
        
        std::cout << "[Hot-Reload] Reload time: " << duration << "ms" << std::endl;
        
        if (reloadCallback_) {
            reloadCallback_(moduleName, success);
        }
    }
};

// ============================================================
// Example: Integration with ImGui for editor
// ============================================================
#if 0
// This shows how you'd integrate with an editor UI

void renderHotReloadPanel(HotReloadManager& reloadMgr) {
    ImGui::Begin("Hot Reload");
    
    auto stats = reloadMgr.getStats();
    
    ImGui::Text("Total Reloads: %zu", stats.totalReloads);
    ImGui::Text("Successful: %zu", stats.successfulReloads);
    ImGui::Text("Failed: %zu", stats.failedReloads);
    ImGui::Text("Avg Reload Time: %lldms", stats.averageReloadTime.count());
    
    ImGui::Separator();
    
    if (ImGui::Button("Check for Changes Now")) {
        reloadMgr.checkForChanges();
    }
    
    ImGui::End();
}
#endif

} // namespace scriptlang

#endif // HOT_RELOAD_H

/* ============================================================
   USAGE EXAMPLE
   ============================================================

#include "hot_reload.h"

int main() {
    JITCompiler jit;
    jit.initialize();
    
    HotReloadManager reloadMgr(jit);
    
    // Register scripts for hot-reload
    reloadMgr.registerScript("PlayerController", "scripts/PlayerController.sl");
    reloadMgr.registerScript("EnemyAI", "scripts/EnemyAI.sl");
    
    // Set up notification callback
    reloadMgr.setReloadCallback([](const std::string& name, bool success) {
        if (success) {
            std::cout << "✓ " << name << " reloaded successfully" << std::endl;
        } else {
            std::cerr << "✗ " << name << " reload failed" << std::endl;
        }
    });
    
    // Start watching in background
    reloadMgr.startWatching();
    
    // Game loop
    while (running) {
        // Or manually check each frame:
        // reloadMgr.checkForChanges();
        
        update();
        render();
    }
    
    reloadMgr.stopWatching();
    
    // Print statistics
    auto stats = reloadMgr.getStats();
    std::cout << "Reload stats: " << stats.totalReloads << " total" << std::endl;
    
    return 0;
}

   ============================================================
*/

// arena.h - Header-only arena allocator
#ifndef ARENA_H
#define ARENA_H

#include <cstdlib>
#include <cstdint>

struct Arena {
    char* base;
    size_t offset;
    size_t capacity;
};

// Globals - define in ONE .cpp file with ARENA_IMPLEMENTATION

#ifdef ARENA_IMPLEMENTATION
Arena* g_frame_arena = nullptr;
Arena* g_persistent_arena = nullptr;
#else
extern Arena* g_frame_arena;
extern Arena* g_persistent_arena;
#endif

inline void* arena_alloc(Arena* a, size_t size) {
    if (!a || !a->base) return malloc(size);
    size = (size + 7) & ~7;
    if (a->offset + size > a->capacity) return nullptr;
    void* ptr = a->base + a->offset;
    a->offset += size;
    return ptr;
}

inline void* arenaAlloc(Arena* a, size_t size) {
    if (!a || !a->base) return malloc(size);
    size = (size + 7) & ~7;
    if (a->offset + size > a->capacity) return nullptr;
    void* ptr = a->base + a->offset;
    a->offset += size;
    return ptr;
}

inline void arena_reset(Arena* a) {
    if (a) a->offset = 0;
}

inline Arena* arena_create(size_t capacity) {
    Arena* a = (Arena*)malloc(sizeof(Arena));
    a->base = (char*)malloc(capacity);
    a->offset = 0;
    a->capacity = capacity;
    return a;
}

inline void arena_destroy(Arena* a) {
    if (a) {
        free(a->base);
        free(a);
    }
}

// C linkage for LLVM
extern "C" {
    inline void* sl_arena_alloc(Arena* a, size_t size) { return arena_alloc(a, size); }
    inline void sl_arena_reset(Arena* a) { arena_reset(a); }
}

#endif // ARENA_H

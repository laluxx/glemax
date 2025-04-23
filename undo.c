#include "undo.h"
#include "globals.h"
#include <string.h>
#include <stdio.h>

#define INITIAL_CAPACITY 16

// TODO Does it really make sense to check if the content changed
// before taking a screenshot, no.

// TODO Implement undo boundaries and atomic changes
// [ ] make procedures the atom.
// [ ] make lines an atom.
// [ ] make defun an atom.
// [ ] make N procedures an atom.
// [x] make words an atom.
// [x] make characters an atom.
// [x] make memory movement an atom.


void initUndos(Undos *undos) {
    undos->states = NULL;
    undos->count = 0;
    undos->capacity = 0;
    undos->current = 0;
}

void freeUndos(Undos *undos) {
    for (size_t i = 0; i < undos->count; i++) {
        free(undos->states[i].content);
    }
    free(undos->states);
    initUndos(undos);
}

static bool growUndos(Undos *undos) {
    size_t new_capacity = undos->capacity == 0 ? INITIAL_CAPACITY : undos->capacity * 2;
    UndoState *new_states = realloc(undos->states, new_capacity * sizeof(UndoState));
    
    if (!new_states) {
        return false;
    }
    
    undos->states = new_states;
    undos->capacity = new_capacity;
    return true;
}

void screenshot(Buffer *buffer) {
    if (!buffer || !buffer->content || inhibit_screenshot) return;
    
    Undos *undos = &buffer->undos;
    
    // Check if identical to current state
    if (undos->current < undos->count) {
        UndoState *current = &undos->states[undos->current];
        if (current->size == buffer->size && 
            current->point == buffer->point &&
            memcmp(current->content, buffer->content, buffer->size) == 0) {
            message("Undo: No changes detected (state %zu/%zu)", 
                   undos->current + 1, undos->count);
            return;
        }
    }
    
    // Check if identical to previous state (if any)
    if (undos->current > 0) {
        UndoState *prev = &undos->states[undos->current - 1];
        if (prev->size == buffer->size && 
            prev->point == buffer->point &&
            memcmp(prev->content, buffer->content, buffer->size) == 0) {
            message("Undo: Identical to previous state (state %zu/%zu)", 
                   undos->current, undos->count);
            return;
        }
    }
    
    // Discard any states after current position
    if (undos->current < undos->count) {
        for (size_t i = undos->current; i < undos->count; i++) {
            free(undos->states[i].content);
        }
        undos->count = undos->current;
    }
    
    // Grow array if needed
    if (undos->count >= undos->capacity && !growUndos(undos)) {
        message("Undo: Failed to allocate memory");
        return;
    }
    
    // Create new state
    UndoState *state = &undos->states[undos->count];
    state->content = malloc(buffer->size);
    if (!state->content) {
        message("Undo: Failed to allocate content");
        return;
    }
    
    memcpy(state->content, buffer->content, buffer->size);
    state->size = buffer->size;
    state->point = buffer->point;
    
    undos->count++;
    undos->current = undos->count;
    
    message("Undo: Saved state %zu/%zu (point: %zu)", 
           undos->current, undos->count, buffer->point);
}

bool undo(Buffer *buffer) {
    if (!buffer || !buffer->content) return false;
    
    Undos *undos = &buffer->undos;
    
    if (undos->current == 0) {
        message("Undo: Already at oldest state");
        return false;
    }
    
    if (undos->current > undos->count) {
        message("Undo: Invalid state index");
        return false;
    }
    
    undos->current--;
    const UndoState *state = &undos->states[undos->current];
    
    char *new_content = malloc(state->size);
    if (!new_content) {
        message("Undo: Failed to allocate memory");
        return false;
    }
    
    memcpy(new_content, state->content, state->size);
    free(buffer->content);
    
    buffer->content = new_content;
    buffer->size = state->size;
    buffer->point = state->point;
    buffer->modified = true;
    
    message("Undo: Restored state %zu/%zu (point: %zu)", 
           undos->current + 1, undos->count, state->point);
    return true;
}

bool redo(Buffer *buffer) {
    if (!buffer || !buffer->content) return false;
    
    Undos *undos = &buffer->undos;
    
    if (undos->current >= undos->count - 1) {
        message("Redo: Already at newest state");
        return false;
    }
    
    undos->current++;
    const UndoState *state = &undos->states[undos->current];
    
    char *new_content = malloc(state->size);
    if (!new_content) {
        message("Redo: Failed to allocate memory");
        return false;
    }
    
    memcpy(new_content, state->content, state->size);
    free(buffer->content);
    
    buffer->content = new_content;
    buffer->size = state->size;
    buffer->point = state->point;
    buffer->modified = true;
    
    message("Redo: Restored state %zu/%zu (point: %zu)", 
           undos->current + 1, undos->count, state->point);
    return true;
}

void printUndoInfo(Undos *undos) {
    message("Undo states: %zu/%zu (current: %zu)", 
           undos->count, undos->capacity, undos->current);
}

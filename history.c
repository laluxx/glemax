#include "history.h"
#include <stdlib.h>
#include <string.h>

// Goated module btw
// TODO savehist_mode
// TODO when we close glemax save all the named histories
// into ~/.config/glemax/named_histories file
// or ~/.glemax.d/named_histories
// or .named_histories

static History* get_history(NamedHistories *nh, const char *name) {
    for (int i = 0; i < nh->count; ++i) {
        if (strcmp(nh->names[i], name) == 0) {
            return nh->histories[i];
        }
    }
    return NULL;
}

void add_to_history(NamedHistories *nh, const char *name, const char *input) {
    History *history = get_history(nh, name);
    if (!history) {
        // NOTE If history doesn't exist, create it
        if (nh->count >= nh->capacity) {
            int newCap = nh->capacity > 0 ? nh->capacity * 2 : 2;
            History **newHistories = realloc(nh->histories, newCap * sizeof(History*));
            char **newNames = realloc(nh->names, newCap * sizeof(char*));
            if (!newHistories || !newNames) return;
            nh->histories = newHistories;
            nh->names = newNames;
            nh->capacity = newCap;
        }
        history = calloc(1, sizeof(History));
        nh->histories[nh->count] = history;
        nh->names[nh->count] = strdup(name);
        nh->count++;
    }
    if (history->size > 0 && strcmp(history->entries[history->size - 1], input) == 0) {
        // If the new input is the same as the last entry, do not add it
        return;
    }
    if (history->size >= history->capacity) {
        // Ensure there is enough capacity to add a new entry
        int newCap = history->capacity > 0 ? history->capacity * 2 : 4;
        char **newEntries = realloc(history->entries, newCap * sizeof(char*));
        if (!newEntries) return;
        history->entries = newEntries;
        history->capacity = newCap;
    }
    // Add the new entry
    history->entries[history->size++] = strdup(input);
    history->index = history->size;
}


const char* previous_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm) {
    History *history = get_history(nh, name);
    if (!history) return NULL;

    if (history->index == history->size) {
        free(history->currentInput);  // Free the previous input if it exists
        history->currentInput = strdup(minibuffer->content);  // Save the new input
    }

    if (history->index > 0) {
        history->index--;
        setBufferContent(minibuffer, history->entries[history->index]);
        return history->entries[history->index];
    } else {
        message("Beginning of history; no preceding item");
        return NULL;
    }
}

const char* next_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm) {
    History *history = get_history(nh, name);
    if (!history) return NULL;

    if (history->index == history->size) {
        message("End of defaults; no next item");
        return NULL;
    }

    history->index++;
    if (history->index >= history->size) {
        history->index = history->size;
        if (history->currentInput) {
            setBufferContent(minibuffer, history->currentInput);  // Restore the original content
        } else {
            message("No more next history entries.");
        }
        return NULL;
    }


    setBufferContent(minibuffer, history->entries[history->index]);
    return history->entries[history->index];
}

void resetHistoryIndex(NamedHistories *nh, const char *name) {
    History *history = get_history(nh, name);
    if (history) {
        history->index = history->size;
    }
}

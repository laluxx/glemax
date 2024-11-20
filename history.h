#ifndef HISTORY_H
#define HISTORY_H

#include "buffer.h"

typedef struct {
    char **entries;      // Array of history entries
    int size;            // Number of entries in history
    int capacity;        // Capacity of the history array
    int index;           // Current index in the history for navigation
    char *currentInput;  // Buffer to store the current input before navigating history
} History;

typedef struct {
    History **histories; // Array of pointer to History structures
    char **names;        // Array of history names
    int count;           // Number of named histories
    int capacity;        // Capacity of the histories array
} NamedHistories;

void add_to_history(NamedHistories *nh, const char *name, const char *input);
const char* previous_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm);
const char* next_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm);
void resetHistoryIndex(NamedHistories *nh, const char *name);

#endif // HISTORY_H

#ifndef COMPLETION_H
#define COMPLETION_H

#include <stdbool.h>
#include "buffer.h"
#include "edit.h"

typedef struct {
    char** items;        // Array of completion strings
    int count;           // Number of completions
    int currentIndex;    // Current index in the completion list
    bool isActive;       // Is completion active
} CompletionEngine;

void fetch_completions(const char* input, CompletionEngine *ce);
void insert_completions(Buffer *buffer, CompletionEngine *ce);


#endif

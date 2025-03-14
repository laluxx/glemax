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

typedef struct {
    char **binaries;  // Array of binary paths
    int count;        // Number of binaries
    bool initialized; // Whether the cache has been initialized
} BinaryCache;

extern BinaryCache binary_cache;

void fetch_path_completions(const char* input, CompletionEngine *ce);
void fetch_command_completions(const char* input, CompletionEngine *ce);

void initialize_binary_cache();
void fetch_shell_command_completions(const char *input, CompletionEngine *ce);

void collect_buffer_words(Buffer *buffer, char ***words, size_t *count);
void fetch_word_completions(const char *input, Buffer *target_buffer, CompletionEngine *ce);

void complete_at_point(const char *prompt, const char *input, CompletionEngine *ce, WindowManager *wm);

void insert_completions(Buffer *buffer, CompletionEngine *ce);


#endif

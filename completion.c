#include "completion.h"
#include "buffer.h"
#include "symbols.h"
#include "commands.h"
#include <dirent.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>


// TODO If we insert a completion with tab and it's a sole completion, then next tab
// should start completing again not try to cycle between 1 completion.


BinaryCache binary_cache = {NULL, 0, false};

void free_completion_engine(CompletionEngine *ce) {
    if (ce->items) {
        for (int i = 0; i < ce->count; i++) {
            if (ce->items[i]) {
                free(ce->items[i]);
            }
        }
        free(ce->items);
        ce->items = NULL;
    }
    ce->count = 0;
    ce->isActive = false;
    ce->currentIndex = -1;
}


void fetch_buffer_completions(const char *input, CompletionEngine *ce) {
    // Clear previous completions
    free_completion_engine(ce);

    // Get buffer count from buffer manager
    int buf_count = bm.count;
    if (buf_count == 0) {
        message("No buffers available");
        return;
    }

    // Collect matching buffer names
    size_t input_len = strlen(input);
    for (int i = 0; i < buf_count; i++) {
        Buffer *buf = bm.buffers[i];
        if (buf && buf->name) {
            // Match buffer names starting with input
            if (strncmp(buf->name, input, input_len) == 0) {
                ce->items = realloc(ce->items, sizeof(char *) * (ce->count + 1));
                ce->items[ce->count] = strdup(buf->name);
                if (!ce->items[ce->count]) {
                    perror("Failed to allocate memory for buffer completion");
                    continue;
                }
                ce->count++;
            }
        }
    }

    // Sort alphabetically
    if (ce->count > 0) {
        qsort(ce->items, ce->count, sizeof(char *),
              (int (*)(const void *, const void *))strcmp);
    }

    ce->isActive = ce->count > 0;
    ce->currentIndex = -1;
}

void fetch_path_completions(const char *input, CompletionEngine *ce) {
    DIR *dir;
    struct dirent *entry;
    char fullPath[PATH_MAX];
    char dirPath[PATH_MAX];
    const char *homeDir = getenv("HOME");

    // Handle home directory expansion
    if (input[0] == '~') {
        snprintf(fullPath, PATH_MAX, "%s%s", homeDir, input + 1);
    } else {
        strncpy(fullPath, input, PATH_MAX);
    }
    fullPath[PATH_MAX - 1] = '\0';

    // Extract directory path
    char *lastSlash = strrchr(fullPath, '/');
    if (lastSlash) {
        strncpy(dirPath, fullPath, lastSlash - fullPath);
        dirPath[lastSlash - fullPath] = 0;
    } else {
        strcpy(dirPath, ".");
    }

    dir = opendir(dirPath);
    if (!dir) {
        perror("Failed to open directory");
        return;
    }

    // Clear previous completions
    free_completion_engine(ce);

    // Collect new path completions
    while ((entry = readdir(dir)) != NULL) {
        const char *lastPart = lastSlash ? lastSlash + 1 : input;
        if (strncmp(entry->d_name, lastPart, strlen(lastPart)) == 0) {
            char formattedPath[PATH_MAX];

            if (entry->d_type == DT_DIR) {
                snprintf(formattedPath, PATH_MAX, "%s/%s/", dirPath, entry->d_name);
            } else {
                snprintf(formattedPath, PATH_MAX, "%s/%s", dirPath, entry->d_name);
            }

            if (strncmp(formattedPath, homeDir, strlen(homeDir)) == 0) {
                snprintf(formattedPath, PATH_MAX, "~%s",
                         formattedPath + strlen(homeDir));
            }

            ce->items = realloc(ce->items, sizeof(char *) * (ce->count + 1));
            ce->items[ce->count] = strdup(formattedPath);
            if (!ce->items[ce->count]) {
                perror("Failed to allocate memory for completion item");
                continue;
            }
            ce->count++;
        }
    }

    closedir(dir);
    ce->isActive = ce->count > 0;
    ce->currentIndex = -1;
}

void fetch_command_completions(const char *input, CompletionEngine *ce) {
    // Clear previous completions
    free_completion_engine(ce);

    // Collect command completions
    for (size_t i = 0; i < commands.size; ++i) {
        const char *cmd_name = commands.commands[i].name;
        if (strncmp(cmd_name, input, strlen(input)) == 0) {
            ce->items = realloc(ce->items, sizeof(char *) * (ce->count + 1));
            ce->items[ce->count] = strdup(cmd_name);
            if (!ce->items[ce->count]) {
                perror("Failed to allocate memory for completion item");
                continue;
            }
            ce->count++;
        }
    }

    ce->isActive = ce->count > 0;
    ce->currentIndex = -1;
}

void initialize_binary_cache() {
    if (binary_cache.initialized) {
        return;
    }

    // Get the PATH environment variable
    const char *path_env = getenv("PATH");
    if (!path_env) {
        message("PATH environment variable not found.");
        return;
    }

    // Make a copy of the PATH string to tokenize
    char *path_copy = strdup(path_env);
    if (!path_copy) {
        perror("Failed to allocate memory for PATH copy");
        return;
    }

    // Initialize the binary cache
    binary_cache.binaries = NULL;
    binary_cache.count = 0;
    binary_cache.initialized = true;

    // Tokenize the PATH and scan each directory
    char *path_token = strtok(path_copy, ":");
    while (path_token) {
        DIR *dir = opendir(path_token);
        if (dir) {
            struct dirent *entry;
            while ((entry = readdir(dir)) != NULL) {
                // Skip directories and hidden files
                if (entry->d_type == DT_DIR || entry->d_name[0] == '.') {
                    continue;
                }

                // Check if file is executable
                char fullpath[PATH_MAX];
                snprintf(fullpath, PATH_MAX, "%s/%s", path_token, entry->d_name);
                if (access(fullpath, X_OK) == 0) {
                    // Add to binary cache
                    binary_cache.binaries = realloc(
                                                    binary_cache.binaries, sizeof(char *) * (binary_cache.count + 1));
                    if (!binary_cache.binaries) {
                        perror("Failed to allocate memory for binary cache");
                        closedir(dir);
                        free(path_copy);
                        return;
                    }

                    binary_cache.binaries[binary_cache.count] = strdup(entry->d_name);
                    if (!binary_cache.binaries[binary_cache.count]) {
                        perror("Failed to allocate memory for binary name");
                        continue;
                    }
                    binary_cache.count++;
                }
            }
            closedir(dir);
        }

        path_token = strtok(NULL, ":");
    }

    free(path_copy);

    // Sort the binary names for better presentation
    if (binary_cache.count > 0) {
        qsort(binary_cache.binaries, binary_cache.count, sizeof(char *),
              (int (*)(const void *, const void *))strcmp);
    }

    char msg[128];
    snprintf(msg, sizeof(msg), "Binary cache initialized with %d commands.",
             binary_cache.count);
    message(msg);
}

// TODO Complete also the arguments and paths when needed
void fetch_shell_command_completions(const char *input, CompletionEngine *ce) {
    // Initialize binary cache if needed
    if (!binary_cache.initialized) {
        initialize_binary_cache();
    }

    // Clear previous completions
    free_completion_engine(ce);

    // Find binaries that match the input prefix
    size_t input_len = strlen(input);
    for (int i = 0; i < binary_cache.count; i++) {
        if (strncmp(binary_cache.binaries[i], input, input_len) == 0) {
            ce->items = realloc(ce->items, sizeof(char *) * (ce->count + 1));
            if (!ce->items) {
                perror("Failed to allocate memory for completion items");
                return;
            }

            ce->items[ce->count] = strdup(binary_cache.binaries[i]);
            if (!ce->items[ce->count]) {
                perror("Failed to allocate memory for completion item");
                continue;
            }
            ce->count++;
        }
    }

    ce->isActive = ce->count > 0;
    ce->currentIndex = -1;
}

void fetch_symbol_completions(const char *input, CompletionEngine *ce) {
    // Clear previous completions
    free_completion_engine(ce);

    // Check if we have symbols loaded
    if (symbols.count == 0) {
        message("No symbols loaded. Use M-x load-debug-symbols first.");
        return;
    }

    // Filter symbols based on input prefix
    size_t input_len = strlen(input);
    for (size_t i = 0; i < symbols.count; i++) {
        if (strncmp(symbols.array[i].name, input, input_len) == 0) {
            // Allocate space for the symbol name + null terminator
            size_t name_len = strlen(symbols.array[i].name);
            size_t buffer_size = name_len + 1;
            char *buffer = malloc(buffer_size);
            if (!buffer) {
                perror("Failed to allocate memory for symbol completion");
                continue;
            }

            // Copy only the symbol name into the buffer
            strcpy(buffer, symbols.array[i].name);

            // Add the name to the completion list
            ce->items = realloc(ce->items, sizeof(char *) * (ce->count + 1));
            if (!ce->items) {
                perror("Failed to allocate memory for completion items");
                free(buffer);
                return;
            }

            ce->items[ce->count] = buffer;
            ce->count++;
        }
    }

    // Sort alphabetically if there are completions
    if (ce->count > 0) {
        qsort(ce->items, ce->count, sizeof(char *),
              (int (*)(const void *, const void *))strcmp);
    }

    ce->isActive = ce->count > 0;
    ce->currentIndex = -1;

    // Display completion count message
    char msg[128];
    snprintf(msg, sizeof(msg), "Found %d matching symbols.", ce->count);
    message(msg);
}

// More like complete_minibuffer.
void complete_at_point(const char *prompt, const char *input, CompletionEngine *ce, WindowManager *wm) {
    Buffer *buffer = wm->activeWindow->buffer;
    if (strcmp(prompt, "M-x ") == 0) {
        fetch_command_completions(input, ce);
    } else if (strcmp(prompt, "Shell command: ") == 0 || major_mode_is(buffer, "term")) {
        fetch_shell_command_completions(input, ce);
    } else if (strcmp(prompt, "Keep lines containing match for regexp: ") == 0) {
        fetch_word_completions(input, buffer, ce);
    } else if (strcmp(prompt, "Symbol: ") == 0) {
        fetch_symbol_completions(input, ce);
    } else if (strcmp(prompt, "Switch to buffer: ") == 0) {
        fetch_buffer_completions(input, ce);
    } else {
        fetch_path_completions(input, ce);
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "Inserted %d completions.", ce->count);
    message(msg);
}


#include <ctype.h>

void collect_buffer_words(Buffer *buffer, char ***words, size_t *count) {
    *count = 0;
    *words = NULL;

    size_t capacity = 0;
    size_t word_start = 0;
    bool in_word = false;

    for (size_t i = 0; i <= buffer->size; i++) {
        char c = (i < buffer->size) ? buffer->content[i] : '\0';

        if (isalnum(c) || c == '_') {
            if (!in_word) {
                word_start = i;
                in_word = true;
            }
        } else {
            if (in_word) {
                // Extract word
                size_t word_len = i - word_start;
                char *word = malloc(word_len + 1);
                strncpy(word, buffer->content + word_start, word_len);
                word[word_len] = '\0';

                // Check if word already exists
                bool exists = false;
                for (size_t w = 0; w < *count; w++) {
                    if (strcmp((*words)[w], word) == 0) {
                        exists = true;
                        break;
                    }
                }

                if (!exists) {
                    // Add to list
                    if (*count >= capacity) {
                        capacity = (capacity == 0) ? 64 : capacity * 2;
                        *words = realloc(*words, sizeof(char *) * capacity);
                    }
                    (*words)[(*count)++] = word;
                } else {
                    free(word);
                }

                in_word = false;
            }
        }
    }
}

void fetch_word_completions(const char *input, Buffer *target_buffer,
                            CompletionEngine *ce) {
    free_completion_engine(ce);

    if (!target_buffer || !target_buffer->content) {
        message("No buffer content for completion");
        return;
    }

    char **words = NULL;
    size_t word_count = 0;
    collect_buffer_words(target_buffer, &words, &word_count);

    size_t input_len = strlen(input);
    for (size_t i = 0; i < word_count; i++) {
        if (strncmp(words[i], input, input_len) == 0) {
            ce->items = realloc(ce->items, sizeof(char *) * (ce->count + 1));
            ce->items[ce->count] = strdup(words[i]);
            ce->count++;
        }
    }

    // Cleanup words
    for (size_t i = 0; i < word_count; i++)
        free(words[i]);
    free(words);

    if (ce->count > 0) {
        qsort(ce->items, ce->count, sizeof(char *),
              (int (*)(const void *, const void *))strcmp);
    }

    ce->isActive = ce->count > 0;
    ce->currentIndex = -1;
}

// TODO vertico
// NOTE Unused
void insert_completions(Buffer *buffer, CompletionEngine *ce) {
    if (!buffer || !ce || ce->count == 0) {
        message("No match");
        return;
    }

    // Insert each completion item into the buffer, one per line
    for (int i = 0; i < ce->count; i++) {
        const char *completion = ce->items[i];
        if (!completion) continue;

        // Insert the completion item
        for (const char *p = completion; *p; p++) {
            insertChar(buffer, *p);
        }

        // Insert a newline after each completion (except the last one)
        if (i < ce->count - 1) {
            insertChar(buffer, '\n');
        }
    }

    char msg[128];
    snprintf(msg, sizeof(msg), "Inserted %d completions.", ce->count);
    message(msg);
}

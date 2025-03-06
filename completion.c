#include "completion.h"
#include <dirent.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// TODO Vertico style completion

void fetch_completions(const char* input, CompletionEngine *ce) {
    DIR* dir;
    struct dirent* entry;
    char fullPath[PATH_MAX];
    char dirPath[PATH_MAX];
    const char* homeDir = getenv("HOME");

    // Handle home directory expansion
    if (input[0] == '~') {
        snprintf(fullPath, PATH_MAX, "%s%s", homeDir, input + 1);
    } else {
        strncpy(fullPath, input, PATH_MAX);
    }
    fullPath[PATH_MAX - 1] = '\0';

    // Extract directory path
    char* lastSlash = strrchr(fullPath, '/');
    if (lastSlash) {
        strncpy(dirPath, fullPath, lastSlash - fullPath);
        dirPath[lastSlash - fullPath] = '\0';
    } else {
        // Handle the case where no directory is specified (use current directory)
        strcpy(dirPath, ".");
    }

    dir = opendir(dirPath);
    if (!dir) {
        perror("Failed to open directory");
        return;
    }

    // Clear previous completions
    for (int i = 0; i < ce->count; i++) {
        free(ce->items[i]);
    }
    free(ce->items);
    ce->items = NULL;
    ce->count = 0;

    // Collect new completions
    while ((entry = readdir(dir)) != NULL) {
        const char* lastPart = lastSlash ? lastSlash + 1 : input;
        if (strncmp(entry->d_name, lastPart, strlen(lastPart)) == 0) {
            char formattedPath[PATH_MAX];

            if (entry->d_type == DT_DIR) {
                snprintf(formattedPath, PATH_MAX, "%s/%s/", dirPath, entry->d_name);
            } else {
                snprintf(formattedPath, PATH_MAX, "%s/%s", dirPath, entry->d_name);
            }

            if (strncmp(formattedPath, homeDir, strlen(homeDir)) == 0) {
                snprintf(formattedPath, PATH_MAX, "~%s", formattedPath + strlen(homeDir));
            }

            ce->items = realloc(ce->items, sizeof(char*) * (ce->count + 1));
            ce->items[ce->count++] = strdup(formattedPath);
        }
    }

    closedir(dir);
    ce->isActive = ce->count > 0; // Activate only if there are completions
    ce->currentIndex = -1; // Reset index for new session
}

void insert_completions(Buffer *buffer, CompletionEngine *ce) {
    if (!buffer || !ce || ce->count == 0) {
        message("No completions to insert.");
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

    // Notify the user
    char msg[128];
    snprintf(msg, sizeof(msg), "Inserted %d completions.", ce->count);
    message(msg);
}

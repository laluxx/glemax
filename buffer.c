#include "buffer.h"
#include "draw.h"
#include "edit.h"
#include "faces.h"
#include <linux/limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "syntax.h"
#include "isearch.h"
#include "globals.h"
#include "theme.h"
#include "draw.h"
#include "git.h"
#include "undo.h"
#include <unistd.h>

double mouseX;
double mouseY;

// NOTE Global buffers
Buffer *minibuffer;
Buffer *prompt;
Buffer *vertico;
Buffer *footer;
Buffer *argBuffer;
Buffer *scratch;
Buffer *messages;


void updateDiffs(Buffer *buffer) {
    if (buffer->path) {
        // Free the existing diff information if it exists
        if (buffer->diffs.array) {
            free(buffer->diffs.array);
        }
        // Update the diff information
        buffer->diffs = getDiffInfo(buffer->path);
    }
}

void initDiffs(BufferManager *bm) {
    for (int i = 0; i < bm->count; i++) {
        updateDiffs(bm->buffers[i]);
    }
}


//-- Display Windows

void addDisplayWindowToBuffer(Buffer *buffer, Window *window) {
    // Check if we need to allocate or expand the array
    if (buffer->displayWindows.windowCount >= buffer->displayWindows.windowCapacity) {
        // Initial capacity or double the capacity
        int newCapacity = buffer->displayWindows.windowCapacity == 0 ? 2 : buffer->displayWindows.windowCapacity * 2;
        Window **newArray = realloc(buffer->displayWindows.windows, newCapacity * sizeof(Window*));
        
        if (!newArray) {
            fprintf(stderr, "Failed to allocate memory for buffer window list.\n");
            return;
        }
        
        buffer->displayWindows.windows = newArray;
        buffer->displayWindows.windowCapacity = newCapacity;
    }
    
    // Add the window to the array
    buffer->displayWindows.windows[buffer->displayWindows.windowCount++] = window;
}

// Remove a window from a buffer's display list
void removeDisplayWindowFromBuffer(Buffer *buffer, Window *window) {
    for (int i = 0; i < buffer->displayWindows.windowCount; i++) {
        if (buffer->displayWindows.windows[i] == window) {
            // Shift all elements after this one
            for (int j = i; j < buffer->displayWindows.windowCount - 1; j++) {
                buffer->displayWindows.windows[j] = buffer->displayWindows.windows[j + 1];
            }
            buffer->displayWindows.windowCount--;
            return;
        }
    }
}

// --^

void initBuffer(Buffer *buffer, const char *name, const char *path) {
    // Initialize basic buffer properties
    memset(buffer, 0, sizeof(Buffer));  // Zero out the entire struct
    
    // Allocate and initialize content
    buffer->capacity = 1024;
    buffer->content = malloc(buffer->capacity);
    if (!buffer->content) {
        fprintf(stderr, "Failed to allocate memory for buffer content.\n");
        exit(EXIT_FAILURE);
    }
    buffer->content[0] = '\0';  // Empty string
    buffer->size = 0;
    buffer->point = 0;

    // Initialize strings with proper null checks
    buffer->name = name ? strdup(name) : strdup("unnamed");
    buffer->path = path ? strdup(path) : strdup("");
    buffer->url = strdup("");
    buffer->fontPath = strdup(fontPath ? fontPath : "");
    buffer->major_mode = strdup("fundamental");

    // Initialize region and scale
    buffer->region.active = false;
    buffer->region.mark = 0;
    buffer->region.marked = false;
    buffer->scale.index = 0;
    buffer->goal_column = -1;

    // Initialize state flags
    buffer->readOnly = false;
    buffer->modified = false;
    buffer->version = 0;
    buffer->animatedLineNumber = -1;
    buffer->animationStartTime = 0.0f;

    // Initialize window tracking
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;

    // Initialize undo system
    initUndos(&buffer->undos);

    // Try to load file content if path exists
    FILE *file = path ? fopen(path, "r") : NULL;
    if (file) {
        fseek(file, 0, SEEK_END);
        buffer->originalSize = ftell(file);
        fseek(file, 0, SEEK_SET);

        buffer->originalContent = malloc(buffer->originalSize + 1);
        if (buffer->originalContent) {
            size_t bytes_read = fread(buffer->originalContent, 1, buffer->originalSize, file);
            buffer->originalContent[bytes_read] = '\0';
            
            // Set buffer content from file
            setBufferContent(buffer, buffer->originalContent, false);
        }
        fclose(file);
    } else {
        // For new buffers
        buffer->originalContent = strdup("");
        buffer->originalSize = 0;
    }

    // Initialize syntax highlighting
    buffer->tree = ts_parser_parse_string(inferParserForLanguage("c"), NULL, buffer->content, buffer->size);
    initSyntaxArray(&buffer->syntaxArray, 10);

    // Initialize scopes
    buffer->scopes.items = NULL;
    buffer->scopes.count = 0;
    buffer->scopes.capacity = 0;

    // Initialize diffs
    buffer->diffs.array = NULL;
    buffer->diffs.count = 0;
    buffer->diffs.capacity = 0;
}

/**
 * Create and return a buffer with a name based on NAME.
 * @param name The base name for the buffer (will be uniquified if needed)
 * @param path The file path associated with the buffer (can be NULL)
 * @param fontPath The font path for the buffer
 * @return The newly created buffer, or NULL on failure
 */
Buffer* generate_new_buffer(const char* name, const char* path, const char* fontPath) {
    // Generate a unique buffer name if needed
    char* unique_name = strdup(name);
    int counter = 1;
    
    while (getBuffer(&bm, unique_name) != NULL) {
        free(unique_name);
        unique_name = malloc(strlen(name) + 10); // Enough space for name<counter>
        sprintf(unique_name, "%s<%d>", name, counter++);
    }
    
    // Create and initialize the buffer
    Buffer* buffer = malloc(sizeof(Buffer));
    if (!buffer) {
        free(unique_name);
        return NULL;
    }
    
    initBuffer(buffer, unique_name, path ? path : "");
    free(unique_name);
    
    inferMajorMode(buffer);
    initScale(&buffer->scale);

    if (fontPath) {
        /* buffer->fontPath = strdup(fontPath); // FIXME */
        buffer->fontPath = fontPath; // FIXME
    }
    
    // Initialize display windows array
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;
    
    // Add to buffer manager without making it active
    if (bm.count >= bm.capacity) {
        bm.capacity = bm.capacity ? bm.capacity * 2 : 10;
        Buffer** new_buffers = realloc(bm.buffers, sizeof(Buffer*) * bm.capacity);
        if (!new_buffers) {
            freeBuffer(buffer);
            free(buffer);
            return NULL;
        }
        bm.buffers = new_buffers;
    }
    
    bm.buffers[bm.count++] = buffer;
    
    // Load font
    if (!globalFontCache[buffer->scale.index]) {
        globalFontCache[buffer->scale.index] = 
            loadFont(buffer->fontPath, fontsize, "name", tab);
    }
    buffer->font = globalFontCache[buffer->scale.index];
    
    screenshot(buffer);
    return buffer;
}


Buffer* newBuffer(BufferManager *bm, WindowManager *wm, const char *name,
                 const char *path, const char *fontPath) {
    // Validate inputs
    if (!name || !path || !fontPath) {
        fprintf(stderr, "newBuffer: Invalid arguments\n");
        return NULL;
    }

    Buffer *buffer = malloc(sizeof(Buffer));
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory for new buffer '%s'\n", name);
        return NULL;
    }

    // Initialize buffer core properties
    initBuffer(buffer, name, path);
    inferMajorMode(buffer);
    initScale(&buffer->scale);
    
    // Copy font path (check for allocation failure)
    buffer->fontPath = strdup(fontPath);
    if (!buffer->fontPath) {
        fprintf(stderr, "Failed to allocate font path for buffer '%s'\n", name);
        free(buffer);
        return NULL;
    }

    // Initialize display windows tracking
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;

    // Load or reuse font from global cache
    if (!globalFontCache[buffer->scale.index]) {
        globalFontCache[buffer->scale.index] = loadFont(fontPath, fontsize, "name", tab);
        if (!globalFontCache[buffer->scale.index]) {
            fprintf(stderr, "Failed to load font for buffer '%s'\n", name);
            free(buffer->fontPath);
            free(buffer);
            return NULL;
        }
    }
    buffer->font = globalFontCache[buffer->scale.index];

    // Expand buffer manager array if needed
    if (bm->count >= bm->capacity) {
        size_t new_capacity = bm->capacity ? bm->capacity * 2 : 4;
        Buffer **newBuffers = realloc(bm->buffers, sizeof(Buffer *) * new_capacity);
        if (!newBuffers) {
            fprintf(stderr, "Failed to expand buffer manager capacity\n");
            free(buffer->fontPath);
            free(buffer);
            return NULL;
        }
        bm->buffers = newBuffers;
        bm->capacity = new_capacity;
    }

    // Add buffer to manager
    bm->buffers[bm->count++] = buffer;
    bm->activeIndex = bm->count - 1;

    // Update active name (check for allocation failure)
    char *newActiveName = strdup(name);
    if (!newActiveName) {
        fprintf(stderr, "Failed to update active buffer name\n");
        // Rollback - remove buffer from manager
        bm->count--;
        free(buffer->fontPath);
        free(buffer);
        return NULL;
    }
    free(bm->activeName);
    bm->activeName = newActiveName;

    // Associate with active window if available
    if (wm && wm->activeWindow) {
        wm->activeWindow->buffer = buffer;
        wm->activeWindow->y = sh - buffer->font->ascent + buffer->font->descent;
        wm->activeWindow->height = wm->activeWindow->y;
        
        // Add window to buffer's display list
        addDisplayWindowToBuffer(buffer, wm->activeWindow);
    }

    // Optional debug screenshot
    screenshot(buffer);

    return buffer;
}

void freeBuffer(Buffer *buffer) {
    free(buffer->content);
    free(buffer->originalContent);
    free(buffer->name);
    free(buffer->major_mode);
    free(buffer->fontPath);
    buffer->content = NULL;
    buffer->name = NULL;
    buffer->size = 0;
    buffer->capacity = 0;
    buffer->point = 0;
    free(buffer->diffs.array);
    freeUndos(&buffer->undos);
    // Free window-buffer (displayWindows) relationships
    free(buffer->displayWindows.windows);
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;
}

// TODO Graveyard
void initBufferManager(BufferManager *bm) {
    bm->capacity = 10;
    bm->buffers = malloc(sizeof(Buffer*) * bm->capacity);
    bm->count = 0;
    bm->activeIndex = -1;
    bm->activeName = NULL;
}

// TODO Implement Graveyard then Free it here
void freeBufferManager(BufferManager *bm) {
    for (int i = 0; i < bm->count; i++) {
        freeBuffer(bm->buffers[i]);
        free(bm->buffers[i]);
    }
    free(bm->buffers);
    free(bm->activeName);
    bm->buffers = NULL;
    bm->activeName = NULL;
    bm->count = 0;
    bm->capacity = 0;
    bm->activeIndex = -1;
}

void switchToBuffer(BufferManager *bm, const char *bufferName) {
    // If the option is enabled, check if any window is already displaying the buffer
    if (focus_window_if_buffer_displayed && !isCurrentBuffer(bm, "minibuffer")) {
        Window *current = wm.head;
        while (current != NULL) {
            if (current->buffer && strcmp(current->buffer->name, bufferName) == 0) {
                // Focus the window that already displays the buffer
                wm.activeWindow->isActive = false;
                wm.activeWindow = current;
                current->isActive = true;
                return; // Exit early to avoid switching the buffer in the original window
            }
            current = current->next;
        }
    }

    // If no window is displaying the buffer, proceed with the original logic
    for (int i = 0; i < bm->count; i++) {
        if (strcmp(bm->buffers[i]->name, bufferName) == 0) {
            bm->activeIndex = i;
            fill_scopes(bm->buffers[i], &bm->buffers[i]->scopes);
            return;
        }
    }

    // If the buffer doesn't exist, show an error message
    message("Buffer not found.");
}

Buffer *getActiveBuffer(BufferManager *bm) {
    if (bm->activeIndex >= 0) {
        return bm->buffers[bm->activeIndex];
    }
    return NULL;
}

Buffer *getBuffer(BufferManager *bm, const char *name) {
    for (int i = 0; i < bm->count; i++) {
        if (strcmp(bm->buffers[i]->name, name) == 0) {
            return bm->buffers[i];
        }
    }
    return NULL; // Return NULL if no buffer is found
}

bool isCurrentBuffer(BufferManager *bm, const char *bufferName) {
    Buffer *currentBuffer = getActiveBuffer(bm);
    if (currentBuffer != NULL && strcmp(currentBuffer->name, bufferName) == 0) {
        return true;
    }
    return false;
}

void nextBuffer(BufferManager *bm) {
    if (bm->count > 0) {
        bm->activeIndex = (bm->activeIndex + 1) % bm->count;
        free(bm->activeName);
        bm->activeName = strdup(bm->buffers[bm->activeIndex]->name);
        printf("Switched to next buffer: %s\n", bm->activeName);
    }
}

void previousBuffer(BufferManager *bm) {
    if (bm->count > 0) {
        bm->activeIndex = (bm->activeIndex - 1 + bm->count) % bm->count;
        free(bm->activeName);
        bm->activeName = strdup(bm->buffers[bm->activeIndex]->name);
        printf("Switched to previous buffer: %s\n", bm->activeName);
    }
}

void activateRegion(Buffer *buffer) {
    if (!buffer->region.active) {
        buffer->region.mark = buffer->point;
        buffer->region.start = buffer->region.end = buffer->point;
        buffer->region.active = true;
    }
}

void updateRegion(Buffer *buffer, size_t new_point) {
    if (buffer->region.active) {
        buffer->region.end = new_point;

        // Normalize region boundaries based on the mark to ensure 'start' is less than 'end'
        if (buffer->region.mark <= new_point) {
            buffer->region.start = buffer->region.mark;
            buffer->region.end = new_point;
        } else {
            buffer->region.start = new_point;
            buffer->region.end = buffer->region.mark;
        }
    }
}

void deactivateRegion(Buffer *buffer) {
    buffer->region.active = false;
}

// TODO Set the mark ?
void setBufferContent(Buffer *buffer, const char *newContent, bool pointAtSize) {
    size_t newContentSize = strlen(newContent) + 1; // +1 for the null terminator

    // Check if buffer's current capacity is insufficient
    if (buffer->capacity < newContentSize) {
        char *newBufferContent = realloc(buffer->content, newContentSize);
        if (!newBufferContent) {
            fprintf(stderr, "Failed to allocate memory for buffer content.\n");
            exit(EXIT_FAILURE);
        }
        buffer->content = newBufferContent;
        buffer->capacity = newContentSize;
    }

    // Copy new content to buffer
    strcpy(buffer->content, newContent);
    buffer->size = newContentSize - 1; // Not counting the null terminator
    if (pointAtSize) {
        buffer->point = buffer->size;
    } else {
        buffer->point = 0;
    }
}

void appendToBuffer(Buffer *buffer, const char *content) {
    size_t contentLen = strlen(content);
    if (contentLen == 0) return;
    
    // Calculate new required size
    size_t newSize = buffer->size + contentLen;
    
    // Check if buffer's current capacity is insufficient (add 1 for null terminator)
    if (buffer->capacity < newSize + 1) {
        // Double the size to minimize frequent reallocations
        size_t newCapacity = (newSize + 1) * 2;
        char *newBufferContent = realloc(buffer->content, newCapacity);
        if (!newBufferContent) {
            fprintf(stderr, "Failed to allocate memory for buffer content.\n");
            return;
        }
        buffer->content = newBufferContent;
        buffer->capacity = newCapacity;
    }
    
    // Append the new content
    strcpy(buffer->content + buffer->size, content);
    buffer->size = newSize;
}

void executeAndOutputShellCommand(Buffer *buffer, const char *command, bool insertMode, size_t insertPos, bool pointAtSize) {
    if (!buffer || !command) return; // Wasted cycles

    // Save current directory
    char currentDir[PATH_MAX];
    if (!getcwd(currentDir, sizeof(currentDir))) {
        perror("Failed to get current directory");
        return;
    }

    // Change to buffer's directory if it has a path
    if (buffer->path && buffer->path[0] != '\0') {
        char dirPath[PATH_MAX];
        strncpy(dirPath, buffer->path, sizeof(dirPath));
        
        // Extract directory part
        char *lastSlash = strrchr(dirPath, '/');
        if (lastSlash) {
            *lastSlash = '\0';
            
            // Handle ~/ prefix
            if (dirPath[0] == '~') {
                const char *home = getenv("HOME");
                if (home) {
                    char expandedPath[PATH_MAX];
                    snprintf(expandedPath, sizeof(expandedPath), "%s%s", home, dirPath + 1);
                    strncpy(dirPath, expandedPath, sizeof(dirPath));
                }
            }
            
            if (chdir(dirPath) != 0) {
                perror("Failed to change to buffer's directory");
                // Continue with current directory
            }
        }
    }

    // Execute command and capture output
    FILE *pipe = popen(command, "r");
    if (!pipe) {
        perror("Failed to execute command");
        chdir(currentDir); // Restore directory
        return;
    }

    // For insert mode, we need to collect all output first
    char *output = NULL;
    size_t outputSize = 0;
    char chunk[4096];
    
    while (fgets(chunk, sizeof(chunk), pipe) != NULL) {
        size_t chunkLen = strlen(chunk);
        
        if (insertMode) {
            // Collect output for later insertion
            char *newOutput = realloc(output, outputSize + chunkLen + 1);
            if (!newOutput) {
                perror("Failed to allocate memory for command output");
                free(output);
                pclose(pipe);
                chdir(currentDir);
                return;
            }
            output = newOutput;
            memcpy(output + outputSize, chunk, chunkLen);
            outputSize += chunkLen;
        } else {
            // Directly append to buffer
            appendToBuffer(buffer, chunk);
        }
    }

    // Clean up pipe
    int status = pclose(pipe);
    if (status == -1) {
        perror("Failed to close command pipe");
    }

    if (insertMode && output) {
        output[outputSize] = '\0';
        
        // Insert the collected output at the specified position
        if (insertPos > buffer->size) insertPos = buffer->size;
        
        // Make space for the new content
        size_t newSize = buffer->size + outputSize;
        if (buffer->capacity < newSize + 1) {
            size_t newCapacity = (newSize + 1) * 2;
            char *newContent = realloc(buffer->content, newCapacity);
            if (!newContent) {
                perror("Failed to allocate memory for buffer content");
                free(output);
                chdir(currentDir);
                return;
            }
            buffer->content = newContent;
            buffer->capacity = newCapacity;
        }
        
        // Move existing content to make space
        memmove(buffer->content + insertPos + outputSize, 
                buffer->content + insertPos,
                buffer->size - insertPos);
        
        // Insert the new content
        memcpy(buffer->content + insertPos, output, outputSize);
        buffer->size = newSize;
        buffer->content[buffer->size] = '\0';
        
        // Update point if requested
        if (pointAtSize) {
            buffer->point = insertPos + outputSize;
        }
        
        free(output);
    } else if (!insertMode && pointAtSize) {
        // For append mode, just move point to end if requested
        buffer->point = buffer->size;
    }

    // Report command status if it failed
    if (status != -1 && WEXITSTATUS(status) != 0) {
        char errorMsg[256];
        snprintf(errorMsg, sizeof(errorMsg), "Command exited with status %d", WEXITSTATUS(status));
        if (insertMode) {
            // Need to insert error message too
            executeAndOutputShellCommand(buffer, errorMsg, insertMode, insertPos, pointAtSize);
        } else {
            appendToBuffer(buffer, errorMsg);
        }
    }

    // Restore original directory
    if (chdir(currentDir) != 0) {
        perror("Failed to restore original directory");
    }
}

void appendShellCommand(Buffer *buffer, char *command, bool pointAtSize) {
    executeAndOutputShellCommand(buffer, command, false, 0, pointAtSize);
}

void insertShellCommand(Buffer *buffer, char *command, bool pointAtSize) {
    executeAndOutputShellCommand(buffer, command, true, buffer->point, pointAtSize);
}

/**
   Return the contents of part of the current buffer as a string.
*/
char *buffer_substring(Buffer *buffer, size_t start, size_t end) {
    if (!buffer || !buffer->content) {return NULL;}
    
    // Clamp positions to buffer bounds
    if (start > buffer->size) start = buffer->size;
    if (end > buffer->size) end = buffer->size;
    
    // Ensure start <= end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }
    
    // Calculate substring length
    size_t length = end - start;
    if (length == 0) {
        return strdup("");  // Return empty string
    }
    
    // Allocate memory for substring (+1 for null terminator)
    char *substring = malloc(length + 1);
    if (!substring) {
        return NULL;
    }
    
    // Copy the substring
    memcpy(substring, buffer->content + start, length);
    substring[length] = '\0';
    
    return substring;
}


size_t line_beginning_position(Buffer *buffer) {
    if (!buffer || buffer->size == 0 || buffer->point == 0) {
        return 0;
    }
    
    size_t pos = buffer->point;
    // Walk backwards until we hit a newline or start of buffer
    while (pos > 0 && buffer->content[pos - 1] != '\n') {
        pos--;
    }
    
    return pos;
}

size_t line_beginning_position_at(Buffer *buffer, size_t line_number) {
    if (!buffer || buffer->size == 0 || line_number == 0) {
        return 0;
    }

    size_t pos = 0;
    size_t current_line = 1;  // Line numbers start at 1

    // Special case: first line
    if (line_number == 1) {
        return 0;
    }

    // Scan through the buffer to find the start of the requested line
    while (pos < buffer->size && current_line < line_number) {
        if (buffer->content[pos] == '\n') {
            current_line++;
            // Skip the newline character to get to start of next line
            pos++;
        } else {
            pos++;
        }
    }

    // If we reached the end of buffer before finding the line
    if (current_line < line_number) {
        return buffer->size;  // Return end of buffer
    }

    return pos;
}

size_t line_end_position_at(Buffer *buffer, size_t pos) {
    if (!buffer || pos >= buffer->size) return buffer->size;
    
    for (size_t i = pos; i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            return i;
        }
    }
    return buffer->size;
}


size_t line_end_position(Buffer *buffer) {
    if (!buffer || buffer->size == 0) {
        return 0;
    }
    
    size_t pos = buffer->point;
    // Walk forward until we hit a newline or end of buffer
    while (pos < buffer->size && buffer->content[pos] != '\n') {
        pos++;
    }
    
    return pos;
}


#include "editor.h"

#include <stdarg.h>


void message(const char *format, ...) {
    Buffer *minibuffer = getBuffer(&bm, "minibuffer");
    Buffer *messageBuffer = getBuffer(&bm, "footer");
    Buffer *messagesBuffer = getBuffer(&bm, "messages");

    if (!messageBuffer || !minibuffer || !messagesBuffer) return;

    // First, format the message with variable arguments
    va_list args;
    va_start(args, format);
    
    // Determine the required buffer size
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy) + 1; // +1 for null terminator
    va_end(args_copy);
    
    if (needed <= 0) {
        va_end(args);
        fprintf(stderr, "Error in formatting message.\n");
        return;
    }
    
    // Allocate memory for the formatted message
    char *formattedText = malloc(needed);
    if (!formattedText) {
        va_end(args);
        fprintf(stderr, "Failed to allocate memory for message.\n");
        return;
    }
    
    // Format the message
    vsnprintf(formattedText, needed, format, args);
    va_end(args);
    
    // Then add the square brackets
    size_t totalLen = needed + 2; // For '[' and ']'
    char *bracketedMessage = malloc(totalLen);
    
    if (bracketedMessage) {
        snprintf(bracketedMessage, totalLen, "[%s]", formattedText);
        
        if (isCurrentBuffer(&bm, "minibuffer") || isearch.searching) {
            setBufferContent(messageBuffer, bracketedMessage, true);
        } else {
            setBufferContent(minibuffer, formattedText, true);
        }
        
        // Append to messages buffer with a newline
        if (messagesBuffer != NULL) {
            char *messageWithNewline = malloc(needed + 1); // +1 for newline
            if (messageWithNewline) {
                sprintf(messageWithNewline, "%s\n", formattedText);
                appendToBuffer(messagesBuffer, messageWithNewline);
                free(messageWithNewline);
            }
        }
        
        free(bracketedMessage);
    } else {
        fprintf(stderr, "Failed to allocate memory for bracketed message.\n");
    }
    
    free(formattedText);
}

// Abort the command that requested this recursive edit or minibuffer input.
// TODO Do the SAME as C-g
void abort_recursive_edit() {
    Buffer *minibuffer = getBuffer(&bm, "minibuffer");
    Buffer *prompt = getBuffer(&bm, "prompt");
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    if (prompt->content) {
      free(prompt->content);
    } else {
        message("abort_recursive_edit() :: Tried to free NULL prompt->content");
    }
    prompt->content = strdup("");
    previousBuffer(&bm); // NOTE It might also be a recursive minibuffer.
}

void cleanBuffer(BufferManager *bm, char *name) {
    Buffer *buffer = getBuffer(bm, name);
    buffer->size = 0;
    buffer->point = 0;
    buffer->content[0] = 0;
}

Buffer *getBufferUnderCursor(WindowManager *wm) {
    Window *win = wm->head;
    while (win != NULL) {
        if (mouseX >= win->x && mouseX <= win->x + win->width &&
            mouseY >= win->y - win->height && mouseY <= win->y) {
            return win->buffer;
        }
        win = win->next;
    }
    
    return NULL;
}

void setMajorMode(Buffer *buffer, char *mode) {
    buffer->major_mode = strdup(mode);
    // TODO Clear SyntaxArray or assume each major mode does it
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

// Is an hashmap for this an overkill ?
void inferMajorMode(Buffer *buffer) {
    const char *extension = strrchr(buffer->name, '.');
    if (extension) {
        if (strcmp(extension, ".c") == 0 || strcmp(extension, ".h") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "c");
        }
        if (strcmp(extension, ".scm") == 0)  {
            free(buffer->major_mode);
            setMajorMode(buffer, "scheme");
        }
        if (strcmp(extension, ".html") == 0)  {
            free(buffer->major_mode);
            setMajorMode(buffer, "html");
        }
        if (strcmp(extension, ".query") == 0)  {
            free(buffer->major_mode);
            setMajorMode(buffer, "query");
        }
        if (   strcmp(extension, ".glsl") == 0
            || strcmp(extension, ".frag") == 0
            || strcmp(extension, ".vert") == 0)  {
            free(buffer->major_mode);
            setMajorMode(buffer, "glsl");
        }
        if (strcmp(extension, ".zig") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "zig");
        }
        if (strcmp(extension, ".odin") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "odin");
        }
        if (strcmp(extension, ".lisp") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "commonlisp");
        }
        if (strcmp(extension, ".scss") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "scss");
        }
        if (strcmp(extension, ".hs") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "haskell");
        }
        if (strcmp(extension, ".lua") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "lua");
        }
        if (strcmp(extension, ".rs") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "rust");
        }

        if (   strcmp(extension,    ".sh"  )   == 0
            || strcmp(extension,    ".bash")   == 0
            || strcmp(buffer->name, "~/.bashrc") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "bash");
        }

        if (strcmp(extension, ".el") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "elisp");
        }

        if (strcmp(extension, ".py") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "python");
        }
        if (strcmp(extension, ".ml") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "ocaml");
        }
        if (strcmp(extension, ".css") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "css");
        }
        if (strcmp(extension, ".js") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "javascript");
        }
        if (strcmp(extension, ".jl") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "julia");
        }
        if (   strcmp(extension, ".cc")  == 0
            || strcmp(extension, ".cpp") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "cpp");
        }
        if (strcmp(extension, ".go") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "go");
        }
        if (strcmp(extension, ".json") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "json");
        }

        if (strcmp(extension, ".d") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "d");
        }

        if (   strcmp(extension, ".regex") == 0
            || strcmp(extension, ".rgx"  ) == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "regex");
        }

        // Add more major-modes... and make this O(1)
    } else {
        // This can't be O(1) ?
        // we could also check the first line of buffer->content here checking for
        // modes that can't be determined by file extension alone
        if (strcmp(getFilename(buffer->name), "Makefile") == 0) {
            free(buffer->major_mode);
            setMajorMode(buffer, "make");
        }

    }
}

bool major_mode_is(Buffer *buffer, char *mode) {
    return strstr(buffer->major_mode, mode) != NULL;
}

// TODO HANDLE global_visual_line_mode and the window parameter
// also take a win instead of buffer can we please have only one function
// split in 2 function getVisualLineNumber() and getLogicLineNumber()
int getLineNumber(Buffer *buffer) {
    int lineNumber = 1;
    for (size_t i = 0; i < buffer->point && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') lineNumber++;
    }

    return lineNumber;
}

int lineNumberAtPoint(Buffer *buffer, size_t point) {
    int lineCount = 1; // Lines are 1-indexed
    for (size_t i = 0; i < point && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
        }
    }
    return lineCount;
}

// NOTE We could/should memoize them
Color foregroundColorAtPoint(Buffer *buffer, size_t point) {
    Color color = CT.text;

    // Check for diff highlighting first if enabled
    if (diff_hl_cursor) {
        int currentLine = lineNumberAtPoint(
                                               buffer, point); // Use the provided point to calculate the line
        for (int i = 0; i < buffer->diffs.count; ++i) {
            DiffInfo *diff = &buffer->diffs.array[i];
            if (diff->line == currentLine) {
                if (diff->type == DIFF_ADDED) {
                    color = CT.diff_hl_insert_cursor;
                } else if (diff->type == DIFF_CHANGED) {
                    color = CT.diff_hl_change_cursor;
                }
                break;
            }
        }
    }

    // Fallback to syntax color if no diff found and crystal_cursor_mode is
    // enabled
    if (colorsEqual(color, CT.text) && crystal_cursor_mode) {
        for (size_t i = 0; i < buffer->syntaxArray.used; ++i) {
            if (point >= buffer->syntaxArray.items[i].start &&
                point < buffer->syntaxArray.items[i].end) {
                color = *buffer->syntaxArray.items[i].color;
                break;
            }
        }
    }

    return color;
}


/**
   Returns a newly allocated string containing the text of the current line.
   Caller is responsible for freeing the returned string.
*/
char* getCurrentLine(Buffer *buffer) {
    if (buffer->size == 0) return strdup("");

    // Find the start of the current line
    size_t start = buffer->point;
    while (start > 0 && buffer->content[start - 1] != '\n') {
        start--;
    }

    // Find the end of the current line
    size_t end = buffer->point;
    while (end < buffer->size && buffer->content[end] != '\n') {
        end++;
    }

    // Calculate length of the line (and allocate memory for it)
    size_t len = end - start;
    char *line = malloc(len + 1);
    if (!line) {
        fprintf(stderr, "Failed to allocate memory for current line.\n");
        exit(EXIT_FAILURE);
    }

    // Copy the line content and null terminate it
    strncpy(line, buffer->content + start, len);
    line[len] = '\0';
    return line;
}

Buffer *getPreviousBuffer(BufferManager *bm) {
    if (bm->count > 0) {
        int previousIndex = (bm->activeIndex - 1 + bm->count) % bm->count;
        return bm->buffers[previousIndex];
    }
    return NULL; // No buffers available
}

Buffer *getNextBuffer(BufferManager *bm) {
    if (bm->count > 0) {
        int nextIndex = (bm->activeIndex + 1) % bm->count;
        return bm->buffers[nextIndex];
    }
    return NULL; // No buffers available
}

char* getPreviousBufferName(BufferManager *bm) {
    if (bm->count > 0) {
        int previousIndex = (bm->activeIndex - 1 + bm->count) % bm->count;
        return bm->buffers[previousIndex]->name;
    }
    return NULL; // No buffers available
}

char* getNextBufferName(BufferManager *bm) {
    if (bm->count > 0) {
        int nextIndex = (bm->activeIndex + 1) % bm->count;
        return bm->buffers[nextIndex]->name;
    }
    return NULL; // No buffers available
}

char* getPreviousBufferPath(BufferManager *bm) {
    if (bm->count > 0) {
        int previousIndex = (bm->activeIndex - 1 + bm->count) % bm->count;
        return bm->buffers[previousIndex]->path;
    }
    return NULL;
}

char* getNextBufferPath(BufferManager *bm) {
    if (bm->count > 0) {
        int nextIndex = (bm->activeIndex + 1) % bm->count;
        return bm->buffers[nextIndex]->path;
    }
    return NULL;
}

// Supported TS Major modes
bool major_mode_supported(Buffer *buffer) {
    if (   strcmp(buffer->major_mode, "c"     ) == 0
        || strcmp(buffer->major_mode, "html"  ) == 0
        || strcmp(buffer->major_mode, "query" ) == 0
        || strcmp(buffer->major_mode, "glsl"  ) == 0
        || strcmp(buffer->major_mode, "zig"   ) == 0
        || strcmp(buffer->major_mode, "odin"  ) == 0
        || strcmp(buffer->major_mode, "make"  ) == 0
        || strcmp(buffer->major_mode, "commonlisp"  ) == 0
        || strcmp(buffer->major_mode, "scss"  ) == 0
        || strcmp(buffer->major_mode, "haskell"  ) == 0
        || strcmp(buffer->major_mode, "lua"  ) == 0
        || strcmp(buffer->major_mode, "rust"  ) == 0
        || strcmp(buffer->major_mode, "bash"  ) == 0
        || strcmp(buffer->major_mode, "elisp"  ) == 0
        || strcmp(buffer->major_mode, "python"  ) == 0
        || strcmp(buffer->major_mode, "ocaml"  ) == 0
        || strcmp(buffer->major_mode, "css"  ) == 0
        || strcmp(buffer->major_mode, "javascript"  ) == 0
        || strcmp(buffer->major_mode, "julia"  ) == 0
        || strcmp(buffer->major_mode, "cpp"  ) == 0
        || strcmp(buffer->major_mode, "go"  ) == 0
        || strcmp(buffer->major_mode, "json"  ) == 0
        || strcmp(buffer->major_mode, "regex"  ) == 0
        || strcmp(buffer->major_mode, "d"  ) == 0
        || strcmp(buffer->major_mode, "scheme") == 0) { return true; } else { return false; }
}

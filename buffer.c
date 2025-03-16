#include "buffer.h"
#include "edit.h"
#include "faces.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "syntax.h"
#include "isearch.h"
#include "globals.h"
#include "theme.h"
#include "draw.h"
#include "git.h"

double mouseX;
double mouseY;

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
    buffer->capacity = 1024;
    buffer->content = malloc(buffer->capacity);

    if (!buffer->content) {
        fprintf(stderr, "Failed to allocate memory for buffer content.\n");
        exit(EXIT_FAILURE);
    }
    buffer->content[0] = '\0'; // Initialize content as empty string

    buffer->size = 0;
    buffer->point = 0;
    buffer->readOnly = false;
    buffer->name = strdup(name);
    buffer->path = strdup(path);
    buffer->region.active = false;
    buffer->scale.index = 0;
    buffer->goal_column = -1;
    buffer->region.mark = 0;

    buffer->animatedLineNumber = -1;
    buffer->animationStartTime = 0.0f;

    // Initialize BufferWindows structure
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;

    FILE *file = fopen(path, "r");
    if (file) {
        fseek(file, 0, SEEK_END);
        buffer->originalSize = ftell(file);
        fseek(file, 0, SEEK_SET);
        buffer->originalContent = malloc(buffer->originalSize + 1);
        fread(buffer->originalContent, 1, buffer->originalSize, file);
        buffer->originalContent[buffer->originalSize] = '\0';
        fclose(file);
    } else {
        buffer->originalContent = NULL;
        buffer->originalSize = 0;
    }

    setMajorMode(buffer, "fundamental");
    
    buffer->url = strdup(""); // NaU
    buffer->fontPath = strdup(fontPath);  // Use the global fontPath initially

    // Initialize syntax tree
    buffer->tree = ts_parser_parse_string(inferParserForLanguage("c"), NULL, buffer->content, buffer->size);
    initSyntaxArray(&buffer->syntaxArray, 10);

    buffer->scopes.items = NULL;
    buffer->scopes.count = 0;
    buffer->scopes.capacity = 0;

    buffer->diffs = (Diffs){NULL, 0, 0};
}

void newBuffer(BufferManager *manager, WindowManager *wm, const char *name,
               const char *path, char *fontPath, int sw, int sh) {
    Buffer *buffer = malloc(sizeof(Buffer));
    if (buffer == NULL) {
        fprintf(stderr, "Failed to allocate memory for new buffer.\n");
        return;
    }

    initBuffer(buffer, name, path);
    inferMajorMode(buffer);
    initScale(&buffer->scale);
    buffer->fontPath = strdup(fontPath);

    // Initialize the display windows array
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;

    // Use the global font cache
    if (!globalFontCache[buffer->scale.index]) {
        globalFontCache[buffer->scale.index] =
            loadFont(fontPath, fontsize, "name", tab);
    }
    buffer->font = globalFontCache[buffer->scale.index];

    if (manager->count >= manager->capacity) {
        manager->capacity *= 2;
        Buffer **newBuffers =
            realloc(manager->buffers, sizeof(Buffer *) * manager->capacity);
        if (newBuffers == NULL) {
            fprintf(stderr, "Failed to expand buffer manager capacity.\n");
            free(buffer); // Free allocated buffer on failure
            return;
        }
        manager->buffers = newBuffers;
    }

    manager->buffers[manager->count++] = buffer;

    // Set the buffer in the active window, ensuring it is immediately visible and
    // correctly positioned
    if (wm->activeWindow) {
        wm->activeWindow->buffer = buffer;
        wm->activeWindow->y = sh - buffer->font->ascent + buffer->font->descent;
        wm->activeWindow->height =
            wm->activeWindow->y; // Adjust height to maintain text position
    }

    // Optionally set the global active buffer if needed
    manager->activeIndex = manager->count - 1;
    free(manager->activeName);
    manager->activeName = strdup(name);
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
    // Free window-buffer (displayWindows) relationships
    free(buffer->displayWindows.windows);
    buffer->displayWindows.windows = NULL;
    buffer->displayWindows.windowCount = 0;
    buffer->displayWindows.windowCapacity = 0;
    
}

void initBufferManager(BufferManager *manager) {
    manager->capacity = 10;
    manager->buffers = malloc(sizeof(Buffer*) * manager->capacity);
    manager->count = 0;
    manager->activeIndex = -1;
    manager->activeName = NULL;
}

void freeBufferManager(BufferManager *manager) {
    for (int i = 0; i < manager->count; i++) {
        freeBuffer(manager->buffers[i]);
        free(manager->buffers[i]);
    }
    free(manager->buffers);
    free(manager->activeName);
    manager->buffers = NULL;
    manager->activeName = NULL;
    manager->count = 0;
    manager->capacity = 0;
    manager->activeIndex = -1;
}

void switchToBuffer(BufferManager *bm, const char *bufferName) {
    for (int i = 0; i < bm->count; i++) {
        if (strcmp(bm->buffers[i]->name, bufferName) == 0) {
            if (strcmp(getActiveBuffer(bm)->name, "minibuffer") != 0) {
                bm->lastBuffer = getActiveBuffer(bm);
            }
            bm->activeIndex = i;
            fill_scopes(bm->buffers[i], &bm->buffers[i]->scopes);
            return;
        }
    }
}

bool makeActiveBuffer(Buffer *buffer) {
    // TODO
}


Buffer *getActiveBuffer(BufferManager *bm) {
    if (bm->activeIndex >= 0) {
        return bm->buffers[bm->activeIndex];
    }
    return NULL;
}

Buffer *getBuffer(BufferManager *manager, const char *name) {
    for (int i = 0; i < manager->count; i++) {
        if (strcmp(manager->buffers[i]->name, name) == 0) {
            return manager->buffers[i];
        }
    }
    return NULL; // Return NULL if no buffer is found
}

bool isCurrentBuffer(BufferManager *manager, const char *bufferName) {
    Buffer *currentBuffer = getActiveBuffer(manager);
    if (currentBuffer != NULL && strcmp(currentBuffer->name, bufferName) == 0) {
        return true;
    }
    return false;
}

void nextBuffer(BufferManager *manager) {
    if (manager->count > 0) {
        manager->activeIndex = (manager->activeIndex + 1) % manager->count;
        free(manager->activeName);
        manager->activeName = strdup(manager->buffers[manager->activeIndex]->name);
        printf("Switched to next buffer: %s\n", manager->activeName);
    }
}

void previousBuffer(BufferManager *manager) {
    if (manager->count > 0) {
        manager->activeIndex = (manager->activeIndex - 1 + manager->count) % manager->count;
        free(manager->activeName);
        manager->activeName = strdup(manager->buffers[manager->activeIndex]->name);
        printf("Switched to previous buffer: %s\n", manager->activeName);
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

#include "editor.h"

#include <stdarg.h>


void message(const char *format, ...) {
    Buffer *minibuffer = getBuffer(&bm, "minibuffer");
    Buffer *messageBuffer = getBuffer(&bm, "message");
    Buffer *messagesBuffer = getBuffer(&bm, "messages");
    
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

/* void message(const char *format, ...) { */
/*     Buffer *minibuffer    = getBuffer(&bm, "minibuffer"); */
/*     Buffer *messageBuffer = getBuffer(&bm, "message"); */

/*     // First, format the message with variable arguments */
/*     va_list args; */
/*     va_start(args, format); */

/*     // Determine the required buffer size */
/*     va_list args_copy; */
/*     va_copy(args_copy, args); */
/*     int needed = */
/*         vsnprintf(NULL, 0, format, args_copy) + 1; // +1 for null terminator */
/*     va_end(args_copy); */

/*     if (needed <= 0) { */
/*         va_end(args); */
/*         fprintf(stderr, "Error in formatting message.\n"); */
/*         return; */
/*     } */

/*     // Allocate memory for the formatted message */
/*     char *formattedText = malloc(needed); */
/*     if (!formattedText) { */
/*         va_end(args); */
/*         fprintf(stderr, "Failed to allocate memory for message.\n"); */
/*         return; */
/*     } */

/*     // Format the message */
/*     vsnprintf(formattedText, needed, format, args); */
/*     va_end(args); */

/*     // Then add the square brackets */
/*     size_t totalLen = needed + 2; // For '[' and ']' */
/*     char *bracketedMessage = malloc(totalLen); */

/*     if (bracketedMessage) { */
/*         snprintf(bracketedMessage, totalLen, "[%s]", formattedText); */

/*         if (isCurrentBuffer(&bm, "minibuffer") || isearch.searching) { */
/*             setBufferContent(messageBuffer, bracketedMessage, true); */
/*         } else { */
/*             setBufferContent(minibuffer, formattedText, true); */
/*         } */

/*         free(bracketedMessage); */
/*     } else { */
/*         fprintf(stderr, "Failed to allocate memory for bracketed message.\n"); */
/*     } */

/*     free(formattedText); */
/* } */

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
}

void setMajorMode(Buffer *buffer, char *mode) {
    buffer->major_mode = strdup(mode);
    // TODO Clear SyntaxArray or assume each major mode does it
}

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

        // Add more major-modes...
    } else {
        // we could also check the buffer->content here for modes that can't be determined by file extension alone
    }
}

bool major_mode_is(Buffer *buffer, char *mode) {
    return strstr(buffer->major_mode, mode) != NULL;
}

// MODELINE

void addSegment(Segments *segments, const char *name, const char *content) {
    segments->segment = realloc(segments->segment, (segments->count + 1) * sizeof(Segment));
    segments->segment[segments->count].name = strdup(name);
    segments->segment[segments->count].content = strdup(content);
    segments->count++;
}

void initSegments(Segments *segments) {
    segments->segment = NULL;
    segments->count = 0;
    addSegment(segments, "logo",          "NaL");
    addSegment(segments, "readonly",      "NaR");
    addSegment(segments, "noOtherWindow", "NoW");
    addSegment(segments, "name",          "NAME");
    addSegment(segments, "url",           "NaU");
    addSegment(segments, "line-number",   "LINE-NUMBER");
    addSegment(segments, "scroll",        "Top");
    addSegment(segments, "region-chars",  "NaRC");
    addSegment(segments, "region-lines",  "NaRL");
    addSegment(segments, "isearch",       "[NaC]");
    addSegment(segments, "mode",          "Maybe");
    addSegment(segments, "scale",         "NaN");
    addSegment(segments, "branch",        "NaB");
}

void updateSegments(Modeline *modeline, Buffer *buffer) {
    for (size_t i = 0; i < modeline->segments.count; i++) {
        Segment *segment = &modeline->segments.segment[i];

        if (strcmp(segment->name, "line-number") == 0) {
            int lineNumber = getLineNumber(buffer);
            free(segment->content);
            char lineNumStr[32];
            snprintf(lineNumStr, sizeof(lineNumStr), "L%d", lineNumber);
            segment->content = strdup(lineNumStr);
        }
        else if (strcmp(segment->name, "branch") == 0) {
            free(segment->content);
            char* branch = getGitBranch(buffer->path);
            segment->content = branch;
        }
        else if (strcmp(segment->name, "name") == 0) {
            free(segment->content);
            segment->content = strdup(buffer->name);
        }
        else if (strcmp(segment->name, "url") == 0) {
            free(segment->content);
            segment->content = strdup(buffer->url);
        }

        else if (strcmp(segment->name, "mode") == 0) {
            free(segment->content);
            segment->content = strdup(buffer->major_mode);
        }
        else if (strcmp(segment->name, "scale") == 0) {
            free(segment->content);
            char scaleStr[32];
            snprintf(scaleStr, sizeof(scaleStr), "%d", buffer->scale.index);
            segment->content = strdup(scaleStr);
        }
        else if (strcmp(segment->name, "isearch") == 0) {
            free(segment->content);
            if (isearch.count > 0) {
                char countStr[32];
                snprintf(countStr, sizeof(countStr), "[%zu]", isearch.count);
                segment->content = strdup(countStr);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "readonly") == 0) {
            free(segment->content);
            if (buffer->readOnly) {
                segment->content = strdup("R");
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "noOtherWindow") == 0) {
            free(segment->content);
            if (modeline->window && modeline->window->parameters.noOtherWindow) {
                segment->content = strdup("NoW");
            } else {
                segment->content = strdup("");
            }
        }


        else if (strcmp(segment->name, "region-chars") == 0) {
            if (buffer->region.active) {
                free(segment ->content);
                // Calculate the number of characters selected
                size_t chars_selected = buffer->region.end - buffer->region.start;
                char chars_str[32];
                snprintf(chars_str, sizeof(chars_str), "%zu", chars_selected);
                segment->content = strdup(chars_str);
            } else {
                segment->content = strdup("");
            }
            // removed
        }

        else if (strcmp(segment->name, "region-lines") == 0) {
            if (buffer->region.active) {
                free(segment ->content);
                // Calculate the number of lines selected
                size_t lines_selected = 0;
                for (size_t i = buffer->region.start; i < buffer->region.end; i++) {
                    if (buffer->content[i] == '\n') {
                        lines_selected++;
                    }
                }
                char lines_str[32];
                snprintf(lines_str, sizeof(lines_str), "%zu", lines_selected);
                segment->content = strdup(lines_str);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "logo") == 0) {
            free(segment->content);
            if (strcmp(buffer->major_mode, "c") == 0) {
                segment->content = strdup("C");
            } else if (strcmp(buffer->major_mode, "scheme") == 0) {
                segment->content = strdup("G");
            } else if (strcmp(buffer->major_mode, "eterm") == 0) {
                segment->content = strdup("TERM");
            } else if (strcmp(buffer->major_mode, "gemini") == 0) {
                segment->content = strdup("Gem");
            } else {
                segment->content = strdup("F");
            }
        }
    }
}


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


/* Returns a newly allocated string containing the text of the current line.
   Caller is responsible for freeing the returned string. */
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


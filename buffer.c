#include "buffer.h"
#include "edit.h"
#include "faces.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lsp.h"
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
    buffer->region.marked = false;

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
        // FIXME This else is reached
        /* buffer->originalContent = NULL; */
        /* buffer->originalSize = 0; */

        buffer->originalContent = strdup(buffer->content);
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

void newBuffer(BufferManager *bm, WindowManager *wm, const char *name,
               const char *path, char *fontPath) {
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

    if (bm->count >= bm->capacity) {
        bm->capacity *= 2;
        Buffer **newBuffers =
            realloc(bm->buffers, sizeof(Buffer *) * bm->capacity);
        if (newBuffers == NULL) {
            fprintf(stderr, "Failed to expand buffer bm capacity.\n");
            free(buffer); // Free allocated buffer on failure
            return;
        }
        bm->buffers = newBuffers;
    }

    bm->buffers[bm->count++] = buffer;

    // Set the buffer in the active window, ensuring it is immediately visible and
    // correctly positioned
    if (wm->activeWindow) {
        wm->activeWindow->buffer = buffer;
        wm->activeWindow->y = sh - buffer->font->ascent + buffer->font->descent;
        wm->activeWindow->height =
            wm->activeWindow->y; // Adjust height to maintain text position
    }

    // Optionally set the global active buffer if needed
    bm->activeIndex = bm->count - 1;
    free(bm->activeName);
    bm->activeName = strdup(name);
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


// TODO Option to just focus the window if it's displaying that buffer
/* void switchToBuffer(BufferManager *bm, const char *bufferName) { */
/*     for (int i = 0; i < bm->count; i++) { */
/*         if (strcmp(bm->buffers[i]->name, bufferName) == 0) { */
/*             if (strcmp(getActiveBuffer(bm)->name, "minibuffer") != 0) { */
/*                 bm->lastBuffer = getActiveBuffer(bm); */
/*             } */
/*             bm->activeIndex = i; */
/*             fill_scopes(bm->buffers[i], &bm->buffers[i]->scopes); */
/*             return; */
/*         } */
/*     } */
/* } */


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

// MODELINE TODO Whatever it's doing make it faster

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
    addSegment(segments, "changed",       "Nothing");
    addSegment(segments, "readonly",      "NaR");
    addSegment(segments, "noOtherWindow", "NoW");
    addSegment(segments, "name",          "NAME");
    addSegment(segments, "url",           "NaU");
    addSegment(segments, "line-number",   "LINE-NUMBER");
    addSegment(segments, "scroll",        "Top");
    addSegment(segments, "lsp",           "NOLSP");
    addSegment(segments, "path",          "NaP");
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
        else if (strcmp(segment->name, "lsp") == 0) {
            free(segment->content);
            char *bufferDir = getBufferDirectory(buffer->path);
            if (strcmp(bufferDir, "~/xos/projects/c/glemax") == 0 && lspp(lspClient)) {
                segment->content = strdup("LSP");
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "path") == 0) {
            free(segment->content);
            if (strcmp(buffer->name, buffer->path) != 0) {
                segment->content = strdup(buffer->path);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "changed") == 0) {
            free(segment->content);
            if (strstr(buffer->content, buffer->originalContent)) {
                segment->content = strdup("");
            } else {
              segment->content = strdup("C");
            }
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
            } else if (strcmp(buffer->major_mode, "html") == 0) {
                segment->content = strdup("H");
            } else if (strcmp(buffer->major_mode, "scheme") == 0) {
                segment->content = strdup("S");
            } else if (strcmp(buffer->major_mode, "glsl") == 0) {
                segment->content = strdup("G");
            } else if (strcmp(buffer->major_mode, "eterm") == 0) {
                segment->content = strdup("TERM");
            } else if (strcmp(buffer->major_mode, "gemini") == 0) {
                segment->content = strdup("Gem");
            } else if (strcmp(buffer->major_mode, "query") == 0) {
                segment->content = strdup("Q");
            } else if (strcmp(buffer->major_mode, "zig") == 0) {
                segment->content = strdup("Z");
            } else if (strcmp(buffer->major_mode, "odin") == 0) {
                segment->content = strdup("O");
            } else if (strcmp(buffer->major_mode, "make") == 0) {
                segment->content = strdup("M");
            } else if (strcmp(buffer->major_mode, "commonlisp") == 0) {
                segment->content = strdup("C");
            } else if (strcmp(buffer->major_mode, "scss") == 0) {
                segment->content = strdup("SCSS");
            } else if (strcmp(buffer->major_mode, "haskell") == 0) {
                segment->content = strdup("H");
            } else if (strcmp(buffer->major_mode, "lua") == 0) {
                segment->content = strdup("L");
            } else if (strcmp(buffer->major_mode, "rust") == 0) {
                segment->content = strdup("R");
            } else if (strcmp(buffer->major_mode, "bash") == 0) {
                segment->content = strdup("B");
            } else if (strcmp(buffer->major_mode, "elisp") == 0) {
                segment->content = strdup("E");
            } else if (strcmp(buffer->major_mode, "python") == 0) {
                segment->content = strdup("P");
            } else if (strcmp(buffer->major_mode, "ocaml") == 0) {
                segment->content = strdup("ML");
            } else if (strcmp(buffer->major_mode, "css") == 0) {
                segment->content = strdup("CSS");
            } else if (strcmp(buffer->major_mode, "javascript") == 0) {
                segment->content = strdup("JS");
            } else if (strcmp(buffer->major_mode, "julia") == 0) {
                segment->content = strdup("JULIA");
            } else if (strcmp(buffer->major_mode, "cpp") == 0) {
                segment->content = strdup("CPP");
            } else if (strcmp(buffer->major_mode, "go") == 0) {
                segment->content = strdup("GO");
            } else if (strcmp(buffer->major_mode, "json") == 0) {
                segment->content = strdup("JSON");
            } else if (strcmp(buffer->major_mode, "regex") == 0) {
                segment->content = strdup("REGEX");
            } else {
              segment->content = strdup("F");
            }
        }
    }
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
        || strcmp(buffer->major_mode, "scheme") == 0) { return true; } else { return false; }
}

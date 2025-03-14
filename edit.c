#include "edit.h"
#include "buffer.h"
#include "commands.h"
#include "keychords.h"
#include "faces.h"
#include "symbols.h"
#include "syntax.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <libguile.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include "globals.h"
#include "syntax.h"

jmp_buf env; // NOTE Global jump buffer


void insertChar(Buffer *buffer, unsigned int codepoint) {
    if (buffer->readOnly) {
        message("Buffer is read-only: #<buffer FILENAME>");
        return;
    }

    // Handle negative values that came from signed chars
    if (codepoint > 0x10FFFF) {
        // Convert wrapped negative value back to control character range
        codepoint = (signed char)(codepoint & 0xFF);
        if (codepoint < 0) {
            codepoint = (unsigned char)codepoint;
        }
    }

    // First ensure we have enough capacity
    if (buffer->size + 4 >= buffer->capacity) {
        buffer->capacity *= 2;
        char *newContent = realloc(buffer->content, buffer->capacity * sizeof(char));
        if (!newContent) {
            fprintf(stderr, "Failed to reallocate memory for buffer.\n");
            return;
        }
        buffer->content = newContent;
    }

    // Handle control characters and ASCII directly
    if (codepoint <= 0x7F) {
        // Make space for the new character
        memmove(buffer->content + buffer->point + 1,
                buffer->content + buffer->point,
                buffer->size - buffer->point);
        
        // Insert the character
        buffer->content[buffer->point] = codepoint;
        buffer->point++;
        buffer->size++;
        buffer->content[buffer->size] = '\0';
        return;
    }

    // For Unicode characters, encode as UTF-8
    char utf8[5];
    int bytes;

    if (codepoint <= 0x7FF) {
        utf8[0] = 192 + (codepoint >> 6);
        utf8[1] = 128 + (codepoint & 63);
        bytes = 2;
    } else if (codepoint <= 0xFFFF) {
        utf8[0] = 224 + (codepoint >> 12);
        utf8[1] = 128 + ((codepoint >> 6) & 63);
        utf8[2] = 128 + (codepoint & 63);
        bytes = 3;
    } else if (codepoint <= 0x10FFFF) {
        utf8[0] = 240 + (codepoint >> 18);
        utf8[1] = 128 + ((codepoint >> 12) & 63);
        utf8[2] = 128 + ((codepoint >> 6) & 63);
        utf8[3] = 128 + (codepoint & 63);
        bytes = 4;
    } else {
        // This shouldn't happen due to initial check, but handle just in case
        fprintf(stderr, "Invalid Unicode codepoint after conversion: %u\n", codepoint);
        return;
    }

    // Make space for the UTF-8 sequence
    memmove(buffer->content + buffer->point + bytes,
            buffer->content + buffer->point,
            buffer->size - buffer->point);

    // Insert the UTF-8 sequence
    for (int i = 0; i < bytes; i++) {
        buffer->content[buffer->point + i] = utf8[i];
    }

    buffer->point += bytes;
    buffer->size += bytes;
    buffer->content[buffer->size] = '\0';
}

void beginning_of_buffer(Buffer *buffer) {
    if (buffer != NULL && buffer->content != NULL) buffer->point = 0;
    if (!buffer->region.marked) set_mark(buffer, buffer->point);
}

void end_of_buffer(Buffer *buffer) {
    if (buffer != NULL && buffer->content != NULL) buffer->point = buffer->size;
    if (!buffer->region.marked) set_mark(buffer, buffer->point);
}

void right_char(Buffer *buffer, bool shift, int arg) {
    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) {
            buffer->region.active = false;
        }
    }

    while (arg-- > 0 && buffer->point < buffer->size) {
        buffer->point++;
    }

    if (buffer->point >= buffer->size) {
        message("End of buffer");
    }
}

void left_char(Buffer *buffer, bool shift, int arg) {
    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) buffer->region.active = false;
    }

    while (arg-- > 0 && buffer->point > 0) {
        buffer->point--;
    }

    if (buffer->point <= 0) {
        message("Beginning of buffer");
    }
}

void previous_line(Buffer *buffer, bool shift, int goal_column) {
    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) {
            buffer->region.active = false;
        }
    }

    if (buffer->point == 0) {
        message("Beginning of buffer");
        return;
    }

    int previousLineEnd = 0;
    int previousLineStart = 0;
    int currentLineStart = 0;

    // Find the start of the current line
    for (int i = buffer->point - 1; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            currentLineStart = i + 1;
            break;
        }
    }

    if (currentLineStart == 0) {
        buffer->point = 0;
        message("Beginning of buffer");
        return;
    }

    // Find the start of the previous line
    for (int i = currentLineStart - 2; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            previousLineStart = i + 1;
            break;
        }
    }

    // Find the end of the previous line
    for (int i = currentLineStart - 1; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            previousLineEnd = i;
            break;
        }
    }

    // Calculate the current column position
    int column = buffer->point - currentLineStart;

    // If goal_column is set, use it instead of the current column position
    if (goal_column >= 0) {
        column = goal_column;
    }

    // Calculate the target position in the previous line
    int previousLineLength = previousLineEnd - previousLineStart;
    if (column >= previousLineLength) {
        buffer->point = previousLineEnd; // Move to the end of the previous line
    } else {
        buffer->point = previousLineStart + column; // Move to the goal column
    }
}

void set_mark(Buffer *buffer, size_t pos) {
    buffer->region.mark = pos;
    message("Mark set");
}

void set_mark_command(Buffer *buffer) {
    if (!buffer->region.active) {
        activateRegion(buffer);
        buffer->region.marked = true;
        message("Mark set");
    } else {
        deactivateRegion(buffer);
        buffer->region.marked = false;
    }
}

// TODO CLamp it to the beginning and of buffer
void exchange_point_and_mark (Buffer *buffer) {
    size_t save = buffer->region.mark;
    buffer->region.mark = buffer->point;
    buffer->point = save;
}


// TODO it doesn't work
void mark_scope(Buffer *buffer) {
    if (buffer->scopes.count == 0) {
        message("No scopes found");
        return;
    }

    size_t point = buffer->point;
    Scope *innermost = NULL;

    // Find the innermost scope containing the cursor
    for (size_t i = 0; i < buffer->scopes.count; i++) {
        Scope *scope = &buffer->scopes.items[i];
        if (scope->start <= point && point <= scope->end) {
            // Check if this scope is deeper (higher level) than the current innermost
            if (innermost == NULL || scope->level > innermost->level) {
                innermost = scope;
            }
        }
    }

    if (innermost != NULL) {
        // Set the region from the mark to the end of the scope
        buffer->region.start = buffer->region.mark;
        buffer->region.end = innermost->end;
        buffer->region.active = true;
        buffer->region.marked = false; // Optional: adjust based on your region handling
        /* message("Scope marked from %zu to %zu", buffer->region.start, buffer->region.end); */
    } else {
        /* message("Cursor is not within a scope"); */
    }
}

void set_goal_column(Buffer *buffer) {
    // Find the start of the current line
    size_t line_start = buffer->point;
    while (line_start > 0 && buffer->content[line_start - 1] != '\n') {
        line_start--;
    }

    int current_column = buffer->point - line_start;

    // Toggle the goal column
    if (buffer->goal_column == current_column) {
        buffer->goal_column = -1;
        message("No goal column");
    } else {
        buffer->goal_column = current_column;
        message("Goal column set! (use C-x C-n again to unset it)");
    }
}


void next_line(Buffer *buffer, bool shift, int goal_column) {
    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) {
            buffer->region.active = false;
        }
    }

    int currentLineEnd = buffer->point;
    int nextLineStart = buffer->size;
    int columnPosition = 0;

    // Determine the end of the current line.
    for (size_t i = buffer->point; i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            currentLineEnd = i;
            nextLineStart = i + 1;
            break;
        }
    }

    // Calculate column position.
    int currentLineStart = buffer->point;
    while (currentLineStart > 0 && buffer->content[currentLineStart - 1] != '\n') {
        currentLineStart--;
    }
    columnPosition = buffer->point - currentLineStart;

    // If goal_column is set, use it instead of the current column position.
    if (goal_column >= 0) {
        columnPosition = goal_column;
    }

    // Calculate the point position in the next line, limited by the line's length.
    size_t targetPosition = nextLineStart + columnPosition;
    if (nextLineStart >= buffer->size) {
        // If no new line to jump to, move cursor to the end and display message
        buffer->point = buffer->size;
        message("End of buffer");
    } else {
        for (size_t i = nextLineStart; i <= buffer->size; i++) {
            if (buffer->content[i] == '\n' || i == buffer->size) {
                if (targetPosition > i) {
                    targetPosition = i;
                }
                break;
            }
        }
        buffer->point = targetPosition;
    }
}

void move_beginning_of_line(Buffer * buffer, bool shift) {
    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) buffer->region.active = false;
    }

    for (int i = buffer->point - 1; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            buffer->point = i + 1; // Set point right after the newline.
            set_mark(buffer, buffer->point);
            return;
        }
    }
    buffer->point = 0; // no newline was found, go to the beginning of buffer
}

void move_end_of_line(Buffer *buffer, bool shift) {
    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) buffer->region.active = false;
    }

    for (size_t i = buffer->point; i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            buffer->point = i;
            /* set_mark(buffer, buffer->point); */
            return;
        }
    }
    buffer->point = buffer->size; // no newline was found, go to the end of buffer
}

void delete_char(Buffer *buffer) {
    if (buffer->point >= buffer->size) {
        message("End of buffer");
        return;
    }

    if (buffer->region.active) buffer->region.active = false;

    // Move all characters after the cursor left by one position
    MM(buffer->content + buffer->point, buffer->content + buffer->point + 1,
            buffer->size - buffer->point - 1);

    buffer->size--; // Decrease the size of the buffer
    buffer->content[buffer->size] = '\0'; // Null-terminate the string
}

void kill_sexp(Buffer *buffer, KillRing *kr, int arg) {
    if (arg == 0) arg = 1;  // Default to killing one sexp if no arg provided
    
    size_t start_point = buffer->point;
    size_t end_point = start_point;
    int direction = (arg > 0) ? 1 : -1;
    int count = abs(arg);
    
    while (count > 0) {
        int depth = 0;
        bool in_string = false;
        char string_delimiter = 0;
        
        while ((direction > 0 && end_point < buffer->size) ||
               (direction < 0 && end_point > 0)) {
            char c = buffer->content[end_point];
            
            if (!in_string) {
                if (c == '"' || c == '\'') {
                    in_string = true;
                    string_delimiter = c;
                } else if (c == '(' || c == '[' || c == '{') {
                    depth += (direction > 0) ? 1 : -1;
                } else if (c == ')' || c == ']' || c == '}') {
                    depth += (direction > 0) ? -1 : 1;
                }
            } else if (c == string_delimiter && buffer->content[end_point - 1] != '\\') {
                in_string = false;
            }
            
            end_point += direction;
            
            if (depth == 0 && !in_string &&
                ((direction > 0 && (end_point == buffer->size || isspace(buffer->content[end_point]))) ||
                 (direction < 0 && (end_point == 0 || isspace(buffer->content[end_point - 1]))))) {
                break;
            }
        }
        
        count--;
    }
    
    // Ensure start_point is always less than end_point
    if (start_point > end_point) {
        size_t temp = start_point;
        start_point = end_point;
        end_point = temp;
    }
    
    // Copy the killed text to the kill ring
    size_t length = end_point - start_point;
    char *killed_text = malloc(length + 1);
    if (killed_text) {
        memcpy(killed_text, buffer->content + start_point, length);
        killed_text[length] = '\0';
        kr_kill(kr, killed_text);
        free(killed_text);
    }
    
    // Remove the killed text from the buffer
    MM(buffer->content + start_point, buffer->content + end_point, buffer->size - end_point);
    buffer->size -= length;
    buffer->content[buffer->size] = '\0';
    
    // Set the point to where we started killing
    buffer->point = start_point;
}

bool bolp(Buffer *buffer) {
    if (buffer->point == 0) return true;
    return buffer->content[buffer->point - 1] == '\n';
}

void kill_line(Buffer *buffer, KillRing *kr) {
    if (buffer->point >= buffer->size) return; // Nothing to delete if at the end of the buffer

    size_t startOfLine = buffer->point;
    size_t endOfLine = startOfLine;

    // If we're at the beginning of a line, include the newline in the kill
    bool kill_newline = bolp(buffer);

    // Determine the end of the current line
    while (endOfLine < buffer->size && buffer->content[endOfLine] != '\n') {
        endOfLine++;
    }

    // Include the newline if we're killing from the beginning of the line
    if (kill_newline && endOfLine < buffer->size) {
        endOfLine++; // Move past the newline
    }

    size_t numToDelete = endOfLine - startOfLine;

    if (numToDelete > 0) {
        // Capture the text to be killed
        char *cut_text = malloc(numToDelete + 1);
        if (cut_text) {
            memcpy(cut_text, buffer->content + startOfLine, numToDelete);
            cut_text[numToDelete] = '\0';
            kr_kill(kr, cut_text); // Add to kill ring
            free(cut_text); // Free temporary text buffer
        }

        // Shift remaining text in the buffer left over the killed text
        MM(buffer->content + startOfLine, buffer->content + endOfLine, buffer->size - endOfLine + 1); // +1 for null terminator

        // Update buffer size
        buffer->size -= numToDelete;
    }

    // Handle special case for an empty line
    if (startOfLine == endOfLine && startOfLine < buffer->size && buffer->content[startOfLine] == '\n') {
        // Kill the newline itself
        MM(buffer->content + startOfLine, buffer->content + startOfLine + 1, buffer->size - startOfLine);
        buffer->size--;
    }
}


void open_line(Buffer *buffer) {
    // Ensure there is enough capacity, and if not, expand the buffer
    if (buffer->size + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
        char *newContent =
            realloc(buffer->content, buffer->capacity * sizeof(char));
        if (!newContent) {
            fprintf(stderr, "Failed to reallocate memory for buffer.\n");
            return;
        }
        buffer->content = newContent;
    }

    // Insert the newline character
    MM(buffer->content + buffer->point + 1, buffer->content + buffer->point,
            buffer->size - buffer->point + 1); // +1 for null terminator
    buffer->content[buffer->point] = '\n';
    buffer->size++;

    // Start the animation if lerp_line_mode is enabled
    if (lerp_line_mode) {
        buffer->animatedLineNumber = lineNumberAtPoint(buffer, buffer->point);
        buffer->animationStartTime = getTime();
    }
}

void delete_indentation(Buffer *buffer, BufferManager *bm, int arg) {
    move_beginning_of_line(buffer, false);

    if (buffer->point > 0) {
        left_char(buffer, false, arg);
        delete_char(buffer);
        insertChar(buffer, ' ');
        left_char(buffer, false, arg);
    }
}

void add_indentation(Buffer *buffer) {
    // Ensure there's enough capacity for one more character
    if (buffer->size + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
        char *newContent =
            realloc(buffer->content, buffer->capacity * sizeof(char));
        if (!newContent) {
            fprintf(stderr, "Failed to reallocate memory for buffer.\n");
            return;
        }
        buffer->content = newContent;
    }

    // Find the start of the current line
    int lineStart = buffer->point;
    while (lineStart > 0 && buffer->content[lineStart - 1] != '\n') {
        lineStart--;
    }

    // Insert a tab character at the beginning of the line
    memmove(buffer->content + lineStart + 1, buffer->content + lineStart,
            buffer->size - lineStart + 1); // Include null terminator
    buffer->content[lineStart] = '\t';
    buffer->size += 1;

    // Adjust cursor position if it's on or after the line start
    if (buffer->point >= lineStart) {
        buffer->point += 1;
    }
}

void remove_indentation(Buffer *buffer) {
    // Find the start of the current line
    int lineStart = buffer->point;
    while (lineStart > 0 && buffer->content[lineStart - 1] != '\n') {
        lineStart--;
    }

    // Check if there's a tab character at the line start
    if (buffer->content[lineStart] == '\t') {
        // Remove the tab by shifting the content left
        memmove(buffer->content + lineStart, buffer->content + lineStart + 1,
                buffer->size - (lineStart + 1) + 1); // Include null terminator
        buffer->size -= 1;

        // Adjust cursor position if necessary
        if (buffer->point > lineStart) {
            buffer->point -= 1;
        } else if (buffer->point >= lineStart) {
            buffer->point = lineStart;
        }
    }
}

void initKillRing(KillRing* kr, int capacity) {
    kr->entries = malloc(sizeof(char*) * capacity);
    kr->size = 0;
    kr->capacity = capacity;
    kr->index = 0;
    for (int i = 0; i < capacity; i++) {
        kr->entries[i] = NULL;
    }
}

void freeKillRing(KillRing* kr) {
    for (int i = 0; i < kr->capacity; i++) {
        free(kr->entries[i]);
    }
    free(kr->entries);
}


void copy_to_clipboard(const char* text) {
    char* command;
    // Note: Be cautious with this; if 'text' comes from user input, it may need escaping to prevent command injection.
    asprintf(&command, "echo '%s' | xclip -selection clipboard", text);
    if (command) {
        system(command);
        free(command);
    }
}

// TODO error killing lines that contain tab
void kr_kill(KillRing* kr, const char* text) {
    if (kr->size >= kr->capacity) {
        // Free the oldest entry if the ring is full
        free(kr->entries[kr->index]);
    } else {
        kr->size++;
    }

    kr->entries[kr->index] = strdup(text);
    kr->index = (kr->index + 1) % kr->capacity;

    // Also copy the text to the system clipboard
    copy_to_clipboard(text);
}


void delete_region(Buffer *buffer) {
    size_t start, end;

    if (buffer->region.active) {
        start = buffer->region.start;
        end = buffer->region.end;
    } else {
        // kill between mark and point if the region is not active
        start = buffer->region.mark;
        end = buffer->point;
    }

    // Ensure start is always less than end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    if (end > buffer->size)
        end = buffer->size; // Clamp end to buffer size
    size_t region_length = end - start;

    if (region_length == 0) {
        message("Empty region, nothing to kill.\n");
        return;
    }

    // Remove the region text from the buffer
    MM(buffer->content + start, buffer->content + end, buffer->size - end + 1);
    buffer->size -= region_length;
    buffer->point = start; // Update cursor position to start of the killed region
    buffer->region.active = false; // Deactivate region after killing it
}


void kill_region(Buffer *buffer, KillRing *kr) {
    size_t start, end;

    if (buffer->region.active) {
        start = buffer->region.start;
        end = buffer->region.end;
    } else {
        // kill between mark and point if the region is not active
        start = buffer->region.mark;
        end = buffer->point;
    }

    // Ensure start is always less than end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    if (end > buffer->size)
        end = buffer->size; // Clamp end to buffer size
    size_t region_length = end - start;

    if (region_length == 0) {
        printf("Empty region, nothing to kill.\n");
        return; // Empty region, nothing to kill
    }

    // Allocate and copy the region to kill
    char *cut_text = malloc(region_length + 1);
    if (cut_text) {
        memcpy(cut_text, buffer->content + start, region_length);
        cut_text[region_length] = '\0';
        kr_kill(kr, cut_text); // Add to kill ring
        free(cut_text);        // Free temporary text buffer
    }

    // Remove the region text from the buffer
    MM(buffer->content + start, buffer->content + end,
            buffer->size - end + 1);
    buffer->size -= region_length;

    // Update cursor position to start of the killed region
    buffer->point = start;

    // Deactivate region after killing it
    buffer->region.active = false;
}

char* paste_from_clipboard() {
    FILE* pipe = popen("xclip -o -selection clipboard", "r");
    if (!pipe) return NULL;

    char* result = NULL;
    size_t total_size = 0;
    size_t buffer_size = 4096;  // Use a larger buffer for better performance
    char* buffer = malloc(buffer_size);
    
    if (!buffer) {
        pclose(pipe);
        return NULL;
    }

    size_t bytes_read;
    while ((bytes_read = fread(buffer, 1, buffer_size, pipe)) > 0) {
        char* new_result = realloc(result, total_size + bytes_read + 1);
        if (!new_result) {
            free(result);
            free(buffer);
            pclose(pipe);
            return NULL;
        }
        result = new_result;
        memcpy(result + total_size, buffer, bytes_read);
        total_size += bytes_read;
    }

    free(buffer);
    pclose(pipe);

    if (result) {
        result[total_size] = '\0';  // Null terminate the string
    }

    return result;
}


// TODO use the arg to yank n times
// or yank at n lines from the cursor line positive or negative
// could be helpful with relative line numbers,
// emacs doesn't seem to use the universal argument for yank
void yank(Buffer *buffer, KillRing *kr, int arg) {
    char *clipboard_text = paste_from_clipboard();
    if (!clipboard_text) return;
    
    // Calculate actual content length (ignoring potential trailing newline)
    size_t len = strlen(clipboard_text);
    if (len > 0 && clipboard_text[len-1] == '\n') {
        len--;  // Don't include the trailing newline when inserting
    }
    
    // Insert the text character by character
    for (size_t i = 0; i < len; i++) {
        insertChar(buffer, clipboard_text[i]);
    }
    
    free(clipboard_text);
}

void kill_ring_save(Buffer *buffer, KillRing *kr) {
    size_t start, end;

    if (buffer->region.active) {
        start = buffer->region.start;
        end = buffer->region.end;
    } else {
        // Use the mark and point if the region is not active
        start = buffer->region.mark;
        end = buffer->point;
    }

    // Ensure start is always less than end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    if (end > buffer->size)
        end = buffer->size; // Clamp end to buffer size
    size_t region_length = end - start;

    if (region_length == 0)
        return; // Empty region, nothing to save

    char *text_to_save = malloc(region_length + 1);
    if (text_to_save) {
        memcpy(text_to_save, buffer->content + start, region_length);
        text_to_save[region_length] = '\0';
        kr_kill(kr, text_to_save);
        free(text_to_save);
    } else {
        fprintf(stderr, "Failed to allocate memory for kill ring save.\n");
    }

    // Deactivate region after saving it
    buffer->region.active = false;
}



void duplicate_line(Buffer *buffer) {
    if (buffer == NULL || buffer->content == NULL) return;

    size_t lineStart = buffer->point;
    size_t lineEnd = buffer->point;

    // Move lineStart to the beginning of the line
    while (lineStart > 0 && buffer->content[lineStart - 1] != '\n') {
        lineStart--;
    }

    // Move lineEnd to the end of the line (including the newline character if present)
    while (lineEnd < buffer->size && buffer->content[lineEnd] != '\n') {
        lineEnd++;
    }

    // Include the newline character in duplication if it exists
    bool hasNewLine = (lineEnd < buffer->size && buffer->content[lineEnd] == '\n');
    if (hasNewLine) {
        lineEnd++;
    }

    size_t lineLength = lineEnd - lineStart;

    // If duplicating the last line which does not end with a newline, add it first
    if (!hasNewLine && lineEnd == buffer->size) {
        // Ensure there is capacity for the newline
        if (buffer->size + 1 > buffer->capacity) {
            buffer->capacity = buffer->size + 2; // Just need one more byte for '\n'
            char *newContent = realloc(buffer->content, buffer->capacity);
            if (newContent == NULL) return; // Allocation failed
            buffer->content = newContent;
        }

        buffer->content[buffer->size] = '\n';
        buffer->size++;
        lineEnd++;
        lineLength++; // Now includes the newly added newline
    }

    // Ensure there is enough capacity in the buffer for duplication
    if (buffer->size + lineLength > buffer->capacity) {
        buffer->capacity = (buffer->size + lineLength) * 2;
        char *newContent = realloc(buffer->content, buffer->capacity);
        if (newContent == NULL) return; // Allocation failed
        buffer->content = newContent;
    }

    // Shift the text after lineEnd to make space for the duplicate line
    MM(buffer->content + lineEnd + lineLength, buffer->content + lineEnd, buffer->size - lineEnd);

    // Copy the line to duplicate
    memcpy(buffer->content + lineEnd, buffer->content + lineStart, lineLength);

    // Update buffer size
    buffer->size += lineLength;

    // Null-terminate the buffer
    buffer->content[buffer->size] = '\0';
}



bool isWordChar(char c) {
    return isalnum((unsigned char)c) || c == '_';
}

bool isPunctuationChar(char c) {
    // Checks common punctuation used in programming and text
    return strchr(",.;:!?'\"(){}[]<>-+*/&|^%$#@~", c) != NULL;
}

bool forward_word(Buffer *buffer, int count, bool shift) {
    if (buffer == NULL || buffer->content == NULL || count == 0)
        return false;

    if (shift && !buffer->region.active) {
        activateRegion(buffer);
    } else if (!shift && !buffer->region.marked) {
        buffer->region.active = false;
    }

    size_t pos = buffer->point;
    size_t end = buffer->size;
    int direction = count > 0 ? 1 : -1;
    count = abs(count);

    while (count > 0 && pos < end && pos > 0) {
        if (direction > 0) {
            // Skip non-word characters
            while (pos < end && !isWordChar(buffer->content[pos]))
                pos++;
            // Move through word characters
            while (pos < end && isWordChar(buffer->content[pos]))
                pos++;
        } else {
            // Move back to start of word or non-word sequence
            while (pos > 0 && !isWordChar(buffer->content[pos - 1]))
                pos--;
            while (pos > 0 && isWordChar(buffer->content[pos - 1]))
                pos--;
        }
        count--;
    }

    buffer->point = pos;
    return true;
}

bool backward_word(Buffer *buffer, int count, bool shift) {
    return forward_word(buffer, -count, shift);
}


// FIXME incorrect
void transpose_subr(Buffer *buffer, bool (*mover)(Buffer *, int, bool), int arg) {
    if (buffer == NULL || mover == NULL) return;

    size_t pos1_start, pos1_end, pos2_start, pos2_end;
    size_t original_point = buffer->point;

    if (arg == 0) {
        // TODO: Implement mark functionality
        return;
    } else if (arg > 0) {
        // Determine pos1 (current word)
        (*mover)(buffer, -1, false); // Move backward to start of current word
        pos1_start = buffer->point;
        (*mover)(buffer, 1, false); // Move forward to end of current word
        pos1_end = buffer->point;

        // Move to pos2 (arg words forward)
        buffer->point = original_point;
        for (int i = 0; i < arg; i++) {
            (*mover)(buffer, 1, false);
        }
        pos2_end = buffer->point;
        (*mover)(buffer, -1, false); // Move back to start of the arg-th word
        pos2_start = buffer->point;
    } else { // arg < 0
        // Move backward (-arg) times
        for (int i = 0; i > arg; i--) {
            (*mover)(buffer, -1, false);
        }
        pos2_start = buffer->point;
        (*mover)(buffer, 1, false); // Move forward to end of that word
        pos2_end = buffer->point;

        // Determine pos1 (current word)
        buffer->point = original_point;
        (*mover)(buffer, -1, false); // Move back to start of current word
        pos1_start = buffer->point;
        (*mover)(buffer, 1, false); // Move forward to end of current word
        pos1_end = buffer->point;
    }

    // Perform the transposition
    size_t len1 = pos1_end - pos1_start;
    size_t len2 = pos2_end - pos2_start;
    char *text1 = malloc(len1);
    char *text2 = malloc(len2);

    if (!text1 || !text2) {
        free(text1);
        free(text2);
        return;
    }

    memcpy(text1, buffer->content + pos1_start, len1);
    memcpy(text2, buffer->content + pos2_start, len2);

    // Swap the two regions
    MM(buffer->content + pos2_start, text1, len1);
    MM(buffer->content + pos1_start, text2, len2);

    free(text1);
    free(text2);

    // Position point correctly after transposed regions
    buffer->point = (arg > 0) ? (pos2_start + len1) : pos2_end;
}

void transpose_words(Buffer *buffer, int arg) {
    // Handle case where point is at the end of the buffer
    if (buffer->point == buffer->size) {
        backward_word(buffer, 1, false);
    }

    transpose_subr(buffer, forward_word, arg);
}

void transpose_chars(Buffer *buffer) {
    if (buffer == NULL || buffer->content == NULL
        || buffer->readOnly || buffer->size < 2) {
        message("Buffer is read-only: #<buffer FILENAME>");
        return;
    }

    // If at the end of the buffer, swap the last two characters
    if (buffer->point == buffer->size) {
        buffer->point--;
    }

    // Ensure we're not at the beginning of the buffer
    if (buffer->point == 0) {
        buffer->point++;
    }

    // Swap the character at the point with the previous character
    char temp = buffer->content[buffer->point];
    buffer->content[buffer->point] = buffer->content[buffer->point - 1];
    buffer->content[buffer->point - 1] = temp;

    // Move the point forward
    if (buffer->point < buffer->size) {
        buffer->point++;
    }
}

void kill_word(Buffer *buffer, KillRing *kr) {
    size_t start = buffer->point;
    size_t end = start;
    
    // Skip non-word characters at the current position
    while (end < buffer->size && !isWordChar(buffer->content[end])) {
        end++;
    }
    
    // Move forward until a non-word character is encountered, marking the end of the word
    while (end < buffer->size && isWordChar(buffer->content[end])) {
        end++;
    }
    
    size_t lengthToDelete = end - start;
    if (lengthToDelete == 0) return; // No word to delete if length is 0
    
    // Copy the word that will be killed
    char* killed_text = malloc(lengthToDelete + 1);
    if (killed_text) {
        memcpy(killed_text, buffer->content + start, lengthToDelete);
        killed_text[lengthToDelete] = '\0';
        kr_kill(kr, killed_text); // Assume this function handles the addition to the kill ring
        free(killed_text);
    }
    
    MM(buffer->content + start, buffer->content + end, buffer->size - end + 1); // Including null terminator
    buffer->size -= lengthToDelete;
}

void backward_kill_word(Buffer *buffer, KillRing *kr) {
    if (buffer == NULL || buffer->content == NULL || buffer->point == 0) return;

    size_t end = buffer->point;
    size_t start = end;

    // Skip non-word characters (like punctuation and spaces) just before the word
    while (start > 0 && !isWordChar(buffer->content[start - 1])) {
        start--;
    }

    // Move start backward until a non-word character is encountered, marking the start of the word
    while (start > 0 && isWordChar(buffer->content[start - 1])) {
        start--;
    }

    size_t lengthToDelete = end - start;
    if (lengthToDelete == 0) return; // No word to delete if length is 0

    // Copy the word that will be killed
    char* killed_text = malloc(lengthToDelete + 1);
    if (killed_text) {
        memcpy(killed_text, buffer->content + start, lengthToDelete);
        killed_text[lengthToDelete] = '\0';

        // Add the killed text to the kr_kill ring
        kr_kill(kr, killed_text); // Assume this function handles the addition to the kr_kill ring

        // Free the allocated memory for killed text
        free(killed_text);
    }

    // Remove the word from the buffer by shifting the remaining characters
    MM(buffer->content + start, buffer->content + end, buffer->size - end + 1); // Including null terminator

    // Update the size of the buffer
    buffer->size -= lengthToDelete;

    // Update the cursor position
    buffer->point = start;
}


void forward_paragraph(Buffer *buffer, bool shift) {
    if (buffer == NULL || buffer->content == NULL || buffer->point >= buffer->size) return;

    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) buffer->region.active = false;
    }


    size_t pos = buffer->point;
    bool is_empty_line = false;

    // Scan from the current position
    while (pos < buffer->size) {
        if (buffer->content[pos] == '\n') {
            size_t next_line_start = pos + 1;
            if (next_line_start < buffer->size && buffer->content[next_line_start] == '\n') {
                // Found an empty line
                buffer->point = next_line_start;
                return;
            }
        }
        pos++;
    }
    // If no empty line is found, move to end of buffer
    buffer->point = buffer->size;
}

void backward_paragraph(Buffer *buffer, bool shift) {
    if (buffer == NULL || buffer->content == NULL || buffer->point == 0) return;

    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) buffer->region.active = false;
    }

    size_t pos = buffer->point - 1;  // Start from one character before the current cursor position to check current line first
    bool found_empty_line = false;

    // Scan backward from the current position
    while (pos > 0) {
        if (buffer->content[pos] == '\n' && buffer->content[pos - 1] == '\n') {
            // Found an empty line
            buffer->point = pos;
            return;
        }
        pos--;
    }

    // If no empty line is found, move to start of buffer
    buffer->point = 0;
}


// TODO use tha arg, to indent n number of line after or before the point if negative
void indent(Buffer *buffer, int indentation, int arg) {
    size_t cursor_row_start = 0, cursor_row_end = buffer->size;
    int braceLevel = 0;
    bool startsWithClosingBrace = false;

    // Find the start of the current line
    for (int i = buffer->point - 1; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            cursor_row_start = i + 1;
            break;
        }
    }

    // Find the end of the current line
    for (size_t i = buffer->point; i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            cursor_row_end = i;
            break;
        }
    }

    // Calculate the current brace level up to the start of the current line
    for (size_t i = 0; i < cursor_row_start; ++i) {
        char c = buffer->content[i];
        if (c == '{') {
            braceLevel++;
        } else if (c == '}') {
            braceLevel = (braceLevel > 0) ? braceLevel - 1 : 0;
        }
    }

    // Check if the current line starts with a '}' before any other non-whitespace character
    size_t firstNonWhitespace = cursor_row_start;
    while (firstNonWhitespace < cursor_row_end && isspace(buffer->content[firstNonWhitespace])) {
        firstNonWhitespace++;
    }
    if (firstNonWhitespace < cursor_row_end && buffer->content[firstNonWhitespace] == '}') {
        startsWithClosingBrace = true;
        braceLevel = (braceLevel > 0) ? braceLevel - 1 : 0;  // Decrement brace level for the line that starts with }
    }

    // Determine indentation level
    int requiredIndentation = braceLevel * indentation;
    int currentIndentation = 0;

    // Count existing spaces at the beginning of the line
    size_t i = cursor_row_start;
    while (i < cursor_row_end && isspace(buffer->content[i])) {
        if (buffer->content[i] == ' ') currentIndentation++;
        i++;
    }

    // Adjust indentation to the required level
    size_t old_point = buffer->point;  // Save old cursor position
    buffer->point = cursor_row_start; // Move cursor to the start of the line

    while (currentIndentation < requiredIndentation) {
        insertChar(buffer, ' '); // Insert additional spaces
        currentIndentation++;
    }
    while (currentIndentation > requiredIndentation && currentIndentation > 0) {
        delete_char(buffer); // Delete excess spaces
        currentIndentation--;
    }

    // Correct cursor position based on the previous position of non-whitespace text
    if (old_point >= firstNonWhitespace) {
        buffer->point = old_point - (firstNonWhitespace - cursor_row_start - requiredIndentation);
    } else {
        buffer->point = cursor_row_start + requiredIndentation;
    }
}


// FIXME just loop the lines and call indent ?
void indent_region(Buffer *buffer, BufferManager *bm, int indentation, int arg) {
    if (!buffer->region.active) {
        printf("No active region to indent.\n");
        return; // No region active, nothing to indent
    }

    size_t start = buffer->region.start;
    size_t end = buffer->region.end;

    // Ensure start is less than end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    // Normalize start to the beginning of the first line in the region
    while (start > 0 && buffer->content[start - 1] != '\n') {
        start--;
    }

    // Normalize end to the end of the last line in the region
    while (end < buffer->size && buffer->content[end] != '\n') {
        end++;
    }

    // Save cursor's original line and column
    size_t cursor_line = 0;
    size_t line_start = 0;
    int cursor_column = 0;
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            cursor_line++;
            line_start = i + 1;
        }
    }
    cursor_column = buffer->point - line_start;

    // Process each line within the region
    size_t current_line_start = start;
    while (current_line_start < end) {
        buffer->point = current_line_start;
        indent(buffer, indentation, arg);  // Apply the indent function once per line

        // Move to the start of the next line
        do {
            current_line_start++;
        } while (current_line_start < buffer->size && buffer->content[current_line_start] != '\n');
        current_line_start++; // Move past the newline character, if not at the end of buffer
    }

    // Restore cursor's original line and column
    size_t new_line_start = start;
    size_t current_line = 0;
    while (current_line < cursor_line && new_line_start < buffer->size) {
        if (buffer->content[new_line_start] == '\n') {
            current_line++;
        }
        new_line_start++;
    }

    // Find the start of the cursor's original line
    size_t cursor_new_position = new_line_start;
    while (cursor_column > 0 && cursor_new_position < buffer->size && buffer->content[cursor_new_position] != '\n') {
        cursor_new_position++;
        cursor_column--;
    }

    buffer->point = cursor_new_position;
}

void enter(Buffer *buffer, BufferManager *bm, WindowManager *wm,
           Buffer *minibuffer, Buffer *prompt,
           int indentation, bool electric_indent_mode,
           int sw, int sh,
           NamedHistories *nh, int arg) {
    if (buffer->region.active) buffer->region.active = false;
    if (isearch.searching) {
        add_to_history(nh, prompt->content, minibuffer->content);
        isearch.lastSearch = strdup(minibuffer->content);
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        isearch.searching = false;
        isearch.count = 0;
        prompt->content = strdup("");
    } else if (strcmp(prompt->content, "Find file: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        find_file(bm, wm, sw, sh);
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        prompt->content = strdup("");
        ctrl_x_pressed = false; // NOTE this is hardcoded because we cant reset ctrl_x_pressed
        // inside the key callback (for now) TODO
    } else if (strcmp(prompt->content, "M-x ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        execute_extended_command(bm);
    } else if (strcmp(prompt->content, "Eval: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        eval_expression(bm); // Let eval_expression handle everything
    }
    else if (strcmp(prompt->content, "Keep lines containing match for regexp: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        keep_lines(bm);
    }
    else if (strcmp(prompt->content, "Switch font to: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        load_font(bm);
    }
    else if (strcmp(prompt->content, "Goto line: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        goto_line(bm);
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        prompt->content = strdup("");
    }
    else if (strcmp(prompt->content, "Shell command: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        cleanBuffer(bm, "prompt");
        execute_shell_command(bm, minibuffer->content);
    }

    else if (strcmp(prompt->content, "Symbol: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        cleanBuffer(bm, "prompt");

        // Get the vertico content
        char *symbol_results = findSymbolsByName(minibuffer->content);
        setBufferContent(getBuffer(bm, "vertico"), symbol_results, false);

        // Count the number of lines in the vertico output
        int line_count = 1; // Start with 1 (minimum one line)
        for (int i = 0; symbol_results[i] != '\0'; i++) {
            if (symbol_results[i] == '\n') {
                line_count++;
            }
        }

        // Cap at the maximum allowed lines
        size_t vertico_lines =
            (line_count > vertico_max_lines) ? vertico_max_lines : line_count;

        // Now add newlines to the minibuffer
        Buffer *minibuffer = getBuffer(bm, "minibuffer");

        // First save the original content
        char *original_content = strdup(minibuffer->content);
        if (!original_content) {
            fprintf(stderr, "Failed to allocate memory for minibuffer content.\n");
            free(symbol_results);
            return;
        }

        // Clear the minibuffer
        cleanBuffer(bm, "minibuffer");

        // Add the original content back
        setBufferContent(minibuffer, original_content, true);
        free(original_content);

        // Add newlines for the vertico space
        for (size_t i = 0; i < vertico_lines; i++) {
            // Move to the end of buffer
            minibuffer->point = minibuffer->size;
            // Insert a newline
            insertChar(minibuffer, '\n');
        }

        // Reset cursor position to the beginning
        minibuffer->point = 0;

        free(symbol_results);
    }

    else {
        if (buffer->point > 0 && buffer->point < buffer->size &&
            buffer->content[buffer->point - 1] == '{' && buffer->content[buffer->point] == '}') {
            // Insert a newline and indent for the opening brace
            insertChar(buffer, '\n');
            if (electric_indent_mode) {
                indent(buffer, indentation, arg);
            }

            size_t newCursorPosition = buffer->point;
            insertChar(buffer, '\n');

            if (electric_indent_mode) {
                indent(buffer, indentation, arg);
            }

            buffer->point = newCursorPosition;
        } else {
            insertChar(buffer, '\n');
        }

        if (electric_indent_mode) {
            indent(buffer, indentation, arg);
        }
    }
}

// TODO Dired when calling find_file on a directory

// NOTE We create files when they don't exist (and directories to get to that file)
// automatically, add an option to do it on save-buffer instead of find-file
int mkdirp(const char *path, mode_t mode) {
    char *p, *sep;
    char tmp[PATH_MAX];
    struct stat st;

    if (path == NULL) {
        errno = EINVAL;
        return -1;
    }

    strncpy(tmp, path, sizeof(tmp));
    tmp[sizeof(tmp) - 1] = '\0';
    p = tmp;

    while ((sep = strchr(p, '/')) != NULL) {
        if (sep != p) {
            *sep = '\0';
            if (stat(tmp, &st) != 0) {
                if (mkdir(tmp, mode) != 0 && errno != EEXIST) {
                    return -1;
                }
            } else if (!S_ISDIR(st.st_mode)) {
                errno = ENOTDIR;
                return -1;
            }
            *sep = '/';
        }
        p = sep + 1;
    }

    if (stat(tmp, &st) != 0) {
        if (mkdir(tmp, mode) != 0 && errno != EEXIST) {
            return -1;
        }
    } else if (!S_ISDIR(st.st_mode)) {
        errno = ENOTDIR;
        return -1;
    }

    return 0;
}


void trimTrailingFile(char *path) {
    char *lastSlash = strrchr(path, '/');
    if (lastSlash && *(lastSlash + 1) != '\0') {
        *(lastSlash + 1) = '\0'; // Keep the last '/' and terminate after it
    }
}

// FIXME Why the first time that it's called it's empty ?
void find_file(BufferManager *bm, WindowManager *wm, int sw, int sh) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    // Initial minibuffer setup
    if (minibuffer->size == 0) {
        if (bm->lastBuffer && bm->lastBuffer->path) {
            minibuffer->size = 0;
            minibuffer->content[0] = '\0';
            minibuffer->point = 0;
            trimTrailingFile(bm->lastBuffer->path);
            setBufferContent(minibuffer, bm->lastBuffer->path, true);
        }
        free(prompt->content);
        prompt->content = strdup("Find file: ");
        switchToBuffer(bm, "minibuffer");
        return; // NOTE
    }

    // Resolve path with home directory expansion
    const char *homeDir = getenv("HOME");
    if (!homeDir) {
        message("Environment variable HOME is not set");
        return;
    }

    char fullPath[PATH_MAX];
    const char *filePath = minibuffer->content;

    if (filePath[0] == '~') {
        snprintf(fullPath, sizeof(fullPath), "%s%s", homeDir, filePath + 1);
    } else {
        strncpy(fullPath, filePath, sizeof(fullPath) - 1);
        fullPath[sizeof(fullPath) - 1] = '\0';
    }

    // Create directories if they don't exist
    char *dirPath = strdup(fullPath);
    if (!dirPath) {
        message("Memory allocation failed");
        return;
    }

    char *lastSlash = strrchr(dirPath, '/');
    if (lastSlash) {
        *lastSlash = '\0';
        if (mkdirp(dirPath, 0755) != 0) {
            char errMsg[256];
            snprintf(errMsg, sizeof(errMsg), "Failed to create directory %s: %s",
                     dirPath, strerror(errno));
            message(errMsg);
            free(dirPath);
            return;
        }
    }
    free(dirPath);

    // Try to open existing file first
    FILE *file = fopen(fullPath, "r");
    bool isNewFile = false;

    if (!file) {
        // Create new file if it doesn't exist TODO DON'T
        file = fopen(fullPath, "w+");
        if (!file) {
            char errMsg[256];
            snprintf(errMsg, sizeof(errMsg), "Failed to create file %s: %s", fullPath,
                     strerror(errno));
            message(errMsg);
            return;
        }
        isNewFile = true;
    }

    // Create display path with ~ notation if applicable
    char displayPath[PATH_MAX];
    if (strncmp(fullPath, homeDir, strlen(homeDir)) == 0) {
        snprintf(displayPath, sizeof(displayPath), "~%s",
                 fullPath + strlen(homeDir));
    } else {
        strncpy(displayPath, fullPath, sizeof(displayPath) - 1);
        displayPath[sizeof(displayPath) - 1] = '\0';
    }

    // Check if the buffer already exists
    Buffer *existingBuffer = getBuffer(bm, displayPath);
    if (existingBuffer) {
        if (find_file_focus_existing) {
            // Find the window that already displays the buffer
            Window *win = wm->head;
            while (win != NULL) {
                if (win->buffer == existingBuffer) {
                    // Make this window the active one
                    wm->activeWindow = win;
                    switchToBuffer(bm, displayPath);
                    fclose(file);
                    return;
                }
                win = win->next;
            }
        } else {
            // Allow the same buffer to be displayed in multiple windows
            wm->activeWindow->buffer = existingBuffer;
            switchToBuffer(bm, displayPath);
            fclose(file);
            return;
        }
    }

    // Create and setup the buffer
    newBuffer(bm, wm, displayPath, displayPath, fontPath, sw, sh);
    Buffer *fileBuffer = getBuffer(bm, displayPath);
    if (!fileBuffer) {
        message("Failed to create buffer");
        fclose(file);
        return;
    }

    recenter(wm->activeWindow, true);

    if (!isNewFile) {
        // Read existing file content
        char readBuffer[1024];
        size_t bytesRead;

        while ((bytesRead = fread(readBuffer, 1, sizeof(readBuffer), file)) > 0) {
            // Ensure buffer capacity
            if (fileBuffer->size + bytesRead >= fileBuffer->capacity) {
                fileBuffer->capacity = (fileBuffer->size + bytesRead) * 2;
                char *newContent = realloc(fileBuffer->content, fileBuffer->capacity);
                if (!newContent) {
                    message("Failed to resize buffer");
                    free(fileBuffer->content);
                    fclose(file);
                    return;
                }
                fileBuffer->content = newContent;
            }

            // Copy read data to buffer
            memcpy(fileBuffer->content + fileBuffer->size, readBuffer, bytesRead);
            fileBuffer->size += bytesRead;
        }
    }

    // Null terminate buffer content
    fileBuffer->content[fileBuffer->size] = '\0';
    fclose(file);

    // Switch to new buffer and parse syntax
    switchToBuffer(bm, fileBuffer->name);
    if (major_mode_is(fileBuffer, "c")) parseSyntax(fileBuffer);
    updateDiffs(fileBuffer); // TODO if git_dir_p()

    // Show appropriate message
    if (isNewFile) {
        message("(New file)");
    } else {
        char msg[256];
        snprintf(msg, sizeof(msg), "Loaded %s", displayPath);
        message(msg);
    }
}

// Memmove wrapper NOTE DON'T USE DIRECTLY use MM macro instead.
void *mm(void *dest, const void *src, size_t n) {
    Buffer *buffer = wm.activeWindow->buffer;
    if (buffer->readOnly) {
        message("Buffer is read-only: #<buffer FILENAME>");
        longjmp(env, 1); // Jump back to the MM macro
    }
    return memmove(dest, src, n);
}

void backspace(Buffer *buffer, bool electric_pair_mode) {
    if (buffer->point > 0 && electric_pair_mode) {
        // Check if backspacing over an opening character that has a closing pair right after
        unsigned int currentChar = buffer->content[buffer->point - 1];
        unsigned int nextChar = buffer->content[buffer->point];
        if ((currentChar == '(' && nextChar == ')') ||
            (currentChar == '[' && nextChar == ']') ||
            (currentChar == '{' && nextChar == '}') ||
            (currentChar == '<' && nextChar == '>') ||
            (currentChar == '\'' && nextChar == '\'') ||
            (currentChar == '\"' && nextChar == '\"')) {
            // Remove both characters
            MM(buffer->content + buffer->point - 1, buffer->content + buffer->point + 1, buffer->size - buffer->point - 1);
            buffer->size -= 2;
            buffer->point--;
            buffer->content[buffer->size] = '\0';
            return;
        }
    }
    // Default backspace behavior when not deleting a pair
    if (buffer->point > 0) {
        buffer->point--;
        MM(buffer->content + buffer->point, buffer->content + buffer->point + 1, buffer->size - buffer->point);
        buffer->size--;
        buffer->content[buffer->size] = '\0';
    }
}

// TODO (Shell command succeeded with no output)
// in this case clear the minibuffer then go to the previous buffer
void execute_shell_command(BufferManager *bm, char *command) {
    char *output = NULL;
    char current_dir[PATH_MAX];
    char target_dir[PATH_MAX];
    bool dir_changed = false;

    // Get current working directory
    if (getcwd(current_dir, sizeof(current_dir)) == NULL) {
        message("Failed to get current directory");
        return;
    }

    // If we have a last buffer with a path, change to its directory
    if (bm->lastBuffer && bm->lastBuffer->path) {
        const char *homeDir = getenv("HOME");
        char fullPath[PATH_MAX];
        const char *filePath = bm->lastBuffer->path;

        // Resolve full path from buffer path
        if (filePath[0] == '~') {
            if (homeDir) {
                snprintf(fullPath, sizeof(fullPath), "%s%s", homeDir, filePath + 1);
            } else {
                message("HOME environment variable not set");
                return;
            }
        } else {
            strncpy(fullPath, filePath, sizeof(fullPath) - 1);
            fullPath[sizeof(fullPath) - 1] = '\0';
        }

        // Get directory part of the path
        strncpy(target_dir, fullPath, sizeof(target_dir) - 1);
        target_dir[sizeof(target_dir) - 1] = '\0';
        char *last_slash = strrchr(target_dir, '/');
        if (last_slash) {
            *last_slash = '\0';  // Truncate at last slash to get directory path
            
            // Change to target directory
            if (chdir(target_dir) == 0) {
                dir_changed = true;
            } else {
                char errMsg[256];
                snprintf(errMsg, sizeof(errMsg), "Failed to change directory: %s", strerror(errno));
                message(errMsg);
                return;
            }
        }
    }

    // Execute the command
    FILE *pipe = popen(command, "r");
    if (pipe == NULL) {
        message("Failed to execute command");
        if (dir_changed) {
            chdir(current_dir);  // Restore original directory
        }
        return;
    }

    // Read command output
    char buffer[128];
    size_t output_size = 0;
    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
        size_t chunk_length = strlen(buffer);
        char *new_output = realloc(output, output_size + chunk_length + 1);
        if (new_output == NULL) {
            free(output);
            pclose(pipe);
            message("Failed to allocate memory for command output");
            if (dir_changed) {
                chdir(current_dir);
            }
            return;
        }
        output = new_output;
        memcpy(output + output_size, buffer, chunk_length);
        output_size += chunk_length;
    }

    // Process output
    if (output != NULL) {
        output[output_size] = '\0';
        if (output_size > 0 && output[output_size - 1] == '\n') {
            output[output_size - 1] = '\0';
        }

        Buffer *minibuffer = getBuffer(bm, "minibuffer");
        if (minibuffer != NULL) {
            setBufferContent(minibuffer, output, true);
            minibuffer->point = 0;
        } else {
            message("Minibuffer not found");
        }
        free(output);
    }

    pclose(pipe);

    // Restore original working directory if we changed it
    if (dir_changed) {
        if (chdir(current_dir) != 0) {
            message("Failed to restore original directory");
        }
    }
}


void shell_command(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    // TODO IMPORTANT Recursive minibuffer
    /* if (minibuffer->size == 0) { */
        if (bm->lastBuffer && bm->lastBuffer->name) {
            minibuffer->size = 0;
            minibuffer->point = 0;
            minibuffer->content[0] = '\0';
            free(prompt->content);
            prompt->content = strdup("Shell command: ");
            switchToBuffer(bm, "minibuffer");
        } else {
            message("No last buffer to go to.");
        }
        return;
    /* } */


    // Clear minibuffer after operation
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    prompt->content = strdup("");
    switchToBuffer(bm, bm->lastBuffer->name);
}

void helpful_symbol(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    // TODO IMPORTANT Recursive minibuffer
    if (minibuffer->size == 0) {
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        free(prompt->content);
        prompt->content = strdup("Symbol: ");
        switchToBuffer(bm, "minibuffer");
        return;
    }


    // Clear minibuffer after operation
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    prompt->content = strdup("");
    switchToBuffer(bm, bm->lastBuffer->name);
}


#include "commands.h"

void execute_extended_command(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    if (minibuffer->size == 0) {
        // Initial setup when entering M-x mode
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        free(prompt->content);
        prompt->content = strdup("M-x ");
        switchToBuffer(bm, "minibuffer");
    } else {
        // Look up the command to determine its type first
        const char *cmd_name = strdup(minibuffer->content);
        bool command_executed = false;

        // Clean up
        cleanBuffer(bm, "minibuffer");
        cleanBuffer(bm, "prompt");

        // Find the command in our commands array
        for (size_t i = 0; i < commands.size; ++i) {
            if (strcmp(commands.commands[i].name, cmd_name) == 0) {

                switch (commands.commands[i].type) {
                case CMD_TYPE_C_VOID:
                    executeCommand(cmd_name);
                    command_executed = true;
                    break;
                case CMD_TYPE_C_BUFFER:
                    // Pass the current buffer to the command
                    executeBufferCommand(cmd_name, bm->lastBuffer);
                    command_executed = true;
                    break;
                case CMD_TYPE_C_BUFFERMANAGER:
                    executeBufferManagerCommand(cmd_name, bm);
                    command_executed = true;
                    return;
                    break;
                case CMD_TYPE_SCHEME:
                    executeCommand(cmd_name);
                    command_executed = true;
                    break;
                }
                break;
            }
        }

        // If command wasn't found or executed, show error
        if (!command_executed) {
            char err_msg[256];
            snprintf(err_msg, sizeof(err_msg), "Command '%s' not found.", cmd_name);
            message(err_msg);
        }

        switchToBuffer(bm, bm->lastBuffer->name);
        /* cleanBuffer(bm, "message"); */
    }
}

// TODO keep_lines_incremental() we like that
void keep_lines(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    // Initial minibuffer setup
    if (minibuffer->size == 0) {
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        free(prompt->content);
        prompt->content = strdup("Keep lines containing match for regexp: ");
        switchToBuffer(bm, "minibuffer");
        return;
    }

    // Get the pattern from minibuffer
    const char *pattern = minibuffer->content;
    Buffer *buffer = bm->lastBuffer;

    if (!buffer || !pattern || !buffer->content || buffer->size == 0) {
        message("Invalid buffer or pattern");
        return;
    }

    // Remember original point
    size_t original_point = buffer->point;
    size_t read_pos = 0;
    size_t write_pos = 0;
    size_t line_start = 0;
    bool keep_line = false;
    size_t lines_kept = 0;
    size_t lines_total = 0;

    while (read_pos <= buffer->size) {
        if (read_pos == buffer->size || buffer->content[read_pos] == '\n') {
            lines_total++;
            
            size_t line_length = read_pos - line_start;
            char *line = malloc(line_length + 1);
            if (!line) {
                message("Memory allocation failed");
                return;
            }
            
            strncpy(line, buffer->content + line_start, line_length);
            line[line_length] = '\0';

            keep_line = (strstr(line, pattern) != NULL);
            free(line);

            if (keep_line) {
                lines_kept++;
                if (write_pos != line_start) {
                    MM(buffer->content + write_pos, 
                            buffer->content + line_start, 
                            read_pos - line_start + 1);
                }
                write_pos += (read_pos - line_start + 1);
            }

            read_pos++;
            line_start = read_pos;
        } else {
            read_pos++;
        }
    }

    // Update buffer size and null terminate
    buffer->size = write_pos;
    buffer->content[buffer->size] = '\0';
    
    // Adjust cursor position if necessary
    if (original_point > buffer->size) {
        buffer->point = buffer->size;
    }

    // Display results message
    char msg[100];
    snprintf(msg, sizeof(msg), "Kept %zu out of %zu total lines", lines_kept, lines_total);
    message(msg);

    // Clean up minibuffer
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    prompt->content = strdup("");
    switchToBuffer(bm, buffer->name);
}


void eval_expression(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    if (bm->lastBuffer && bm->lastBuffer->name) {
        if (minibuffer->size == 0) {
            minibuffer->size = 0;
            minibuffer->point = 0;
            minibuffer->content[0] = '\0';
            free(prompt->content);
            prompt->content = strdup("Eval: ");
            setBufferContent(minibuffer, "()", true);
            minibuffer->point = 1;  // Place cursor between parentheses
            switchToBuffer(bm, "minibuffer");
        } else {
            // Evaluate the expression
            char *result = eval_scheme_string(minibuffer->content);
            message(result);
            free(result);

            // Reset region on the lastBuffer
            bm->lastBuffer->region.active = false;
            bm->lastBuffer->region.marked = false;

            cleanBuffer(bm, "minibuffer");
            cleanBuffer(bm, "prompt");
            switchToBuffer(bm, bm->lastBuffer->name);
        }
    } else {
        message("No last buffer to go to.");
    }
}


void load_font(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");
    // Initial minibuffer setup
    if (minibuffer->size == 0) {
        if (bm->lastBuffer && bm->lastBuffer->name) {
            minibuffer->size = 0;
            minibuffer->point = 0;
            minibuffer->content[0] = '\0';
            prompt->content = strdup("Switch font to: ");
            switchToBuffer(bm, "minibuffer");
        } else {
            message("No last buffer to return to.");
        }
        return;
    }
    // Try to get font path using fontconfig
    char *newFontPath = getFontPath(minibuffer->content);
    if (!newFontPath) {
        char error_msg[256];
        snprintf(error_msg, sizeof(error_msg), "Font not found: %s", minibuffer->content);
        message(error_msg);
        cleanBuffer(bm, "minibuffer");
        cleanBuffer(bm, "prompt");
        switchToBuffer(bm, bm->lastBuffer->name);
        return;
    }
    // Try to load the font at the base size first to verify it works
    Font *testFont = loadFont(newFontPath, fontsize, "name", tab);
    if (!testFont) {
        char error_msg[256];
        snprintf(error_msg, sizeof(error_msg), "Failed to load font: %s", minibuffer->content);
        message(error_msg);
        free(newFontPath);
        cleanBuffer(bm, "minibuffer");
        cleanBuffer(bm, "prompt");
        switchToBuffer(bm, bm->lastBuffer->name);
        return;
    }
    freeFont(testFont);  // Free the test font since we'll reload per buffer
    // Keep track of buffers we've successfully updated
    int successful_updates = 0;
    // Try to update each buffer's font
    for (int i = 0; i < bm->count; i++) {
        Buffer *buffer = bm->buffers[i];

        /* free(buffer->fontPath); */
        buffer->fontPath = strdup(newFontPath);
        
        Font *newFont = loadFont(newFontPath, buffer->scale.fontSizes[buffer->scale.index], "name", tab);
        
        if (!newFont) {
            // If we fail to load the font for any buffer, revert all previous changes
            for (int j = 0; j < successful_updates; j++) {
                Buffer *revert_buffer = bm->buffers[j];
                revert_buffer->font = loadFont(newFontPath, revert_buffer->scale.fontSizes[revert_buffer->scale.index], "name", tab);
            }
            
            char error_msg[256];
            snprintf(error_msg, sizeof(error_msg), "Failed to load font for buffer %s", buffer->name);
            message(error_msg);
            /* free(newFontPath); */
            cleanBuffer(bm, "minibuffer");
            cleanBuffer(bm, "prompt");
            switchToBuffer(bm, bm->lastBuffer->name);
            return;
        }
        // Free the old font before assigning the new one
        if (buffer->font) {
            /* freeFont(buffer->font); */
        }
        buffer->font = newFont;
        successful_updates++;
    }
    // Update global fontPath
    /* free(fontPath); */
    fontPath = strdup(newFontPath);
    // Update window positions
    Window *win = wm.head;
    while (win) {
        win->y = sh - win->buffer->font->ascent + win->buffer->font->descent;
        win->height = win->y;
        win = win->next;
    }
    char msg[256];
    snprintf(msg, sizeof(msg), "Switched font to: %s", newFontPath);
    message(msg);
    /* free(newFontPath); */
    cleanBuffer(bm, "minibuffer");
    cleanBuffer(bm, "prompt");
    switchToBuffer(bm, bm->lastBuffer->name);
}

void goto_line(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    // Activate minibuffer with "Goto line: " if it's empty
    if (minibuffer->size == 0) {
        if (bm->lastBuffer && bm->lastBuffer->name) {
            minibuffer->size = 0;
            minibuffer->point = 0;
            minibuffer->content[0] = '\0';
            free(prompt->content);
            prompt->content = strdup("Goto line: ");
            switchToBuffer(bm, "minibuffer");
        } else {
            message("No last buffer to go to.");
        }
        return;
    }

    // Parse the input as a line number and navigate
    long line_number = strtol(minibuffer->content, NULL, 10); // Parse the input to get the line number
    if (line_number <= 0) {
        message("Invalid line number.");
        return;
    }

    // Assume last buffer is the target unless otherwise specified
    Buffer *targetBuffer = bm->lastBuffer;
    if (!targetBuffer || !targetBuffer->content) {
        message("No valid last buffer.");
        return;
    }

    size_t index = 0;
    long current_line = 1;

    // Traverse the buffer content to find the start of the specified line
    while (current_line < line_number && index < targetBuffer->size) {
        if (targetBuffer->content[index] == '\n') {
            current_line++;
        }
        index++;
    }

    // If we found the line, set the cursor position
    if (current_line == line_number) {
        targetBuffer->point = index;
    } else {
        message("Line number exceeds total number of lines in the last buffer.");
    }

    // Clear minibuffer after operation
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    prompt->content = strdup("");
    switchToBuffer(bm, targetBuffer->name);
}

bool navigate_list(Buffer *buffer, int arg) {
    if (!buffer || arg == 0) return false;
    int direction = (arg > 0) ? 1 : -1;
    int groupsToMove = abs(arg);
    int depth = 0;
    size_t pos = buffer->point;
    bool foundGroup = false;

    // Adjust starting position for backward movement
    if (direction == -1 && pos > 0) pos--;

    while (groupsToMove > 0 && pos < buffer->size && pos != (size_t)-1) {
        char c = buffer->content[pos];
        if ((direction == 1 && (c == '(' || c == '[' || c == '{')) ||
            (direction == -1 && (c == ')' || c == ']' || c == '}'))) {
            depth += direction;
        } else if ((direction == 1 && (c == ')' || c == ']' || c == '}')) ||
                   (direction == -1 && (c == '(' || c == '[' || c == '{'))) {
            depth -= direction;
            if (depth == 0) {
                foundGroup = true;
                groupsToMove--;
                if (groupsToMove == 0) break;
            }
        }
        pos += direction;
    }

    if (foundGroup) {
        buffer->point = pos;
        return true;
    } else {
        message((arg > 0) ? "No next group" : "No previous group");
        return false;
    }
}

void forward_list(Buffer *buffer, int arg) {
    if (!buffer) return;
    if (arg == 0) arg = 1;  // Default to moving across one group
    if (navigate_list(buffer, arg)) {
        buffer->point++;  // Move past the closing delimiter
    }
}

void backward_list(Buffer *buffer, int arg) {
    if (!buffer) return;
    if (arg == 0) arg = 1;  // Default to moving across one group
    navigate_list(buffer, -arg);
}


bool forward_sexp(Buffer *buffer, int arg) {
    if (!buffer || arg == 0) return false;

    size_t original_point = buffer->point;
    int direction = (arg > 0) ? 1 : -1;
    int count = abs(arg);

    for (int i = 0; i < count; i++) {
        // Skip whitespace
        while (buffer->point < buffer->size && isspace(buffer->content[buffer->point])) {
            buffer->point++;
        }

        if (buffer->point >= buffer->size) break;

        char c = buffer->content[buffer->point];

        if (c == '(' || c == '[' || c == '{') {
            // List-like expression
            navigate_list(buffer, 1);
            if (buffer->point < buffer->size) {
                buffer->point++; // Move past the closing delimiter
            }
        } else if (c == '"') {
            // String
            buffer->point++; // Move past opening quote
            while (buffer->point < buffer->size && 
                   (buffer->content[buffer->point] != '"' || 
                    (buffer->point > 0 && buffer->content[buffer->point - 1] == '\\'))) {
                buffer->point++;
            }
            if (buffer->point < buffer->size) {
                buffer->point++; // Move past closing quote
            }
        } else if (isalnum(c) || c == '_' || c == '-') {
            // Symbol-like expression
            while (buffer->point < buffer->size && 
                   (isalnum(buffer->content[buffer->point]) || 
                    buffer->content[buffer->point] == '_' || 
                    buffer->content[buffer->point] == '-')) {
                buffer->point++;
            }
        } else {
            // Single character
            buffer->point++;
        }
    }

    if (buffer->point == original_point) {
        message("No next sexp");
        return false;
    }

    return true;
}

bool backward_sexp(Buffer *buffer, int arg) {
    if (!buffer || arg == 0) return false;

    size_t original_point = buffer->point;
    int direction = (arg > 0) ? -1 : 1; // Note the reversal of direction
    int count = abs(arg);

    for (int i = 0; i < count; i++) {
        // Skip whitespace
        while (buffer->point > 0 && isspace(buffer->content[buffer->point - 1])) {
            buffer->point--;
        }

        if (buffer->point == 0) break;

        char c = buffer->content[buffer->point - 1];

        if (c == ')' || c == ']' || c == '}') {
            // List-like expression
            buffer->point--; // Move to the closing delimiter
            navigate_list(buffer, -1);
        } else if (c == '"') {
            // String
            buffer->point--; // Move to the closing quote
            while (buffer->point > 0 && 
                   (buffer->content[buffer->point - 1] != '"' || 
                    (buffer->point > 1 && buffer->content[buffer->point - 2] == '\\'))) {
                buffer->point--;
            }
            if (buffer->point > 0) {
                buffer->point--; // Move to opening quote
            }
        } else if (isalnum(c) || c == '_' || c == '-') {
            // Symbol-like expression
            while (buffer->point > 0 && 
                   (isalnum(buffer->content[buffer->point - 1]) || 
                    buffer->content[buffer->point - 1] == '_' || 
                    buffer->content[buffer->point - 1] == '-')) {
                buffer->point--;
            }
        } else {
            // Single character
            buffer->point--;
        }
    }

    if (buffer->point == original_point) {
        message("No previous sexp");
        return false;
    }

    return true;
}


/* void navigate_list(Buffer *buffer, int arg) { */
/*     int direction = (arg > 0) ? 1 : -1; */
/*     int groupsToMove = abs(arg); */
/*     int depth = 0; */
/*     size_t pos = buffer->point; */
/*     bool foundGroup = false; */

/*     while (groupsToMove > 0 && pos >= 0 && pos < buffer->size) { */
/*         char c = buffer->content[pos]; */
/*         if ((direction == 1 && (c == '(' || c == '[' || c == '{')) || */
/*             (direction == -1 && (c == ')' || c == ']' || c == '}'))) { */
/*             depth += direction; */
/*         } else if ((direction == 1 && (c == ')' || c == ']' || c == '}')) || */
/*                    (direction == -1 && (c == '(' || c == '[' || c == '{'))) { */
/*             depth -= direction; */
/*             if (depth == 0) { */
/*                 foundGroup = true; */
/*                 groupsToMove--; */
/*             } */
/*         } */
/*         pos += direction; */
/*     } */

/*     if (foundGroup && groupsToMove == 0) { */
/*         buffer->point = pos - direction;  // Adjust position back to the last valid position */
/*     } else { */
/*         printf("No %s group\n", (arg > 0) ? "next" : "previous"); */
/*     } */
/* } */

/* void forward_list(Buffer *buffer, int arg) { */
/*     if (arg == 0) arg = 1;  // Default to moving across one group */
/*     navigate_list(buffer, arg); */
/*     buffer->point += 1; */
/* } */

/* void backward_list(Buffer *buffer, int arg) { */
/*     if (arg == 0) arg = 1;  // Default to moving across one group */
/*     navigate_list(buffer, -arg); */
/* } */

// NOTE This will be useful to implement LSP
void moveTo(Buffer *buffer, int ln, int col) {
    size_t current_line = 1; // Start counting lines from 1
    size_t current_column = 0; // Column count for the current line
    size_t i = 0;

    if (buffer == NULL || buffer->content == NULL) {
        printf("Buffer is not initialized.\n");
        return;
    }

    // Traverse the buffer until the desired line
    while (i < buffer->size && current_line < ln) {
        if (buffer->content[i] == '\n') {
            current_line++;
            current_column = 0; // Reset column at the start of a new line
        }
        i++;
    }

    // If the line was found, position at the specified column
    if (current_line == ln) {
        size_t line_start = i;
        while (current_column < col && i < buffer->size && buffer->content[i] != '\n') {
            i++;
            current_column++;
        }
        if (current_column == col) {
            buffer->point = i - 1;
        } else {
            printf("Column number exceeds the length of the line. Positioning at line end.\n");
            buffer->point = i; // If column exceeds line length, position at end of line
        }
    } else {
        printf("Line number exceeds the total number of lines in the buffer.\n");
    }

    // Ensure the cursor does not end up beyond the actual content
    if (buffer->point > buffer->size) {
        buffer->point = buffer->size;
    }
}

// FIXME
void delete_blank_lines(Buffer *buffer, int arg) {
    if (buffer == NULL || buffer->content == NULL) return;

    size_t point = buffer->point;
    size_t length = buffer->size;

    // Check if the current position is on a non-blank line; if yes, do nothing.
    size_t current = point;
    while (current < length && buffer->content[current] != '\n') {
        if (!isspace((unsigned char)buffer->content[current])) {
            return; // Current line is not blank, do nothing.
        }
        current++;
    }

    size_t start = point;
    size_t end = point;

    // Extend start backwards to include all blank lines before the current point
    while (start > 0 && (buffer->content[start - 1] == '\n' || isspace((unsigned char)buffer->content[start - 1]))) {
        start--;
        if (start > 0 && buffer->content[start - 1] == '\n' && !isspace((unsigned char)buffer->content[start - 1])) {
            start++; // Leave one newline character
            break;
        }
    }

    // Extend end forwards to include all blank lines after the current point,
    // but stop if we encounter a non-blank, non-newline character after the newline
    while (end < length && (buffer->content[end] == '\n' || isspace((unsigned char)buffer->content[end]))) {
        if (buffer->content[end] == '\n') {
            size_t next_pos = end + 1;
            while (next_pos < length && isspace((unsigned char)buffer->content[next_pos])) {
                // If next_pos reaches a non-whitespace character, stop extending end
                if (buffer->content[next_pos] != '\n') {
                    end = next_pos; // Retain the indentation
                    goto done;
                }
                next_pos++;
            }
        }
        end++;
    }

 done:

    // Ensure to keep one blank line where the point was
    if (start < point) {
        MM(buffer->content + start + 1, buffer->content + end, length - end + 1); // +1 for null terminator
        buffer->size = buffer->size - (end - start - 1);
        buffer->content[start] = '\n'; // Set a single newline at the start
        buffer->point = start; // Set point at the beginning of the preserved newline
    }
    insertChar(buffer, '\n');
}

#include <errno.h>

// TODO (no changes need to saved) How should we track it internally ?
void save_buffer(BufferManager *bm, Buffer *buffer) {
    // Check if the buffer has a valid path
    if (buffer->path == NULL || strlen(buffer->path) == 0) {
        message("No file path specified.");
        return;
    }

    // Resolve the full path if the path starts with '~'
    const char *homeDir = getenv("HOME");
    char fullPath[PATH_MAX];

    if (buffer->path[0] == '~') {
        if (homeDir) {
            snprintf(fullPath, sizeof(fullPath), "%s%s", homeDir, buffer->path + 1);
        } else {
            message("Environment variable HOME is not set.");
            return;
        }
    } else {
        strncpy(fullPath, buffer->path, sizeof(fullPath) - 1);
        fullPath[sizeof(fullPath) - 1] = '\0'; // Ensure null termination
    }

    // Open the file for writing
    FILE *file = fopen(fullPath, "w");
    if (file == NULL) {
        char errMsg[256];
        snprintf(errMsg, sizeof(errMsg), "Error saving file: %s", strerror(errno));
        message(errMsg);
        return;
    }

    // Write the content to the file
    size_t written = fwrite(buffer->content, sizeof(char), buffer->size, file);
    if (written != buffer->size) {
        fclose(file);
        message("Error writing to file.");
        return;
    }

    // Close the file after writing
    fclose(file);

    // Display a message indicating success using the full path
    char msg[512];
    snprintf(msg, sizeof(msg), "Wrote %s", fullPath);
    message(msg);
    updateDiffs(buffer);
}

void recenter(Window *window, bool instant) {
    if (!window || !window->buffer) return;

    Buffer *buffer = window->buffer;
    Font *font = buffer->font;
    float lineHeight = font->ascent + font->descent;

    // Calculate the vertical position of the cursor in the buffer
    int cursorLine = 0;
    for (size_t i = 0; i < buffer->point && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            cursorLine++;
        }
    }

    float cursorY = cursorLine * lineHeight;
    float verticalCenter = window->height / 2;
    float targetY = cursorY - verticalCenter + lineHeight / 2;

    // Clamp the target scroll position to avoid scrolling beyond the content
    float maxScroll = buffer->size * lineHeight - window->height;
    targetY = fmax(0, fmin(targetY, maxScroll));

    if (instant) {
        // Instantly set the scroll position
        window->scroll.y = targetY;
        window->targetScrollY = targetY;
        window->isScrolling = false;
    } else {
        // Use smooth scrolling (lerping)
        window->targetScrollY = targetY;
        window->isScrolling = true;
    }
}

/* void recenter(Window *window, bool instant) { */
/*     if (!window || !window->buffer) return; */

/*     Buffer *buffer = window->buffer; */
/*     Font *font = buffer->font; */
/*     float lineHeight = font->ascent + font->descent; */

/*     // Calculate the vertical position of the cursor in the buffer */
/*     int cursorLine = 0; */
/*     for (size_t i = 0; i < buffer->point && i < buffer->size; i++) { */
/*         if (buffer->content[i] == '\n') { */
/*             cursorLine++; */
/*         } */
/*     } */

/*     float cursorY = cursorLine * lineHeight; */
/*     float verticalCenter = window->height / 2; */
/*     float targetY = cursorY - verticalCenter; */

/*     if (instant) { */
/*         window->scroll.y = targetY; // Instantly set scroll position */
/*     } else { */
/*         window->targetScrollY = targetY; */
/*         window->isScrolling = true; */
/*     } */
/* } */

/* void recenter(Window *window) { */
/*     if (!window || !window->buffer) return; */

/*     Buffer *buffer = window->buffer; */
/*     Font *font = buffer->font; */
/*     float lineHeight = font->ascent + font->descent; */

/*     // Calculate the vertical position of the cursor in the buffer */
/*     int cursorLine = 0; */
/*     for (size_t i = 0; i < buffer->point && i < buffer->size; i++) { */
/*         if (buffer->content[i] == '\n') { */
/*             cursorLine++; */
/*         } */
/*     } */

/*     float cursorY = cursorLine * lineHeight; */
/*     float verticalCenter = window->height / 2; */
/*     float targetY = cursorY - verticalCenter + lineHeight / 2; */

/*     // Clamp the target scroll position to avoid scrolling beyond the content */
/*     float maxScroll = buffer->size * lineHeight - window->height; */
/*     targetY = fmax(0, fmin(targetY, maxScroll)); */

/*     if (scroll_lerp_mode) { */
/*         // Smooth scrolling: set targetScrollY and enable scrolling */
/*         window->targetScrollY = targetY; */
/*         window->isScrolling = true; */
/*     } else { */
/*         // Instant scrolling: directly set scroll.y */
/*         window->scroll.y = targetY; */
/*     } */
/* } */


void capitalize_word(Buffer *buffer) {
    if (!buffer || !buffer->content || buffer->readOnly || buffer->point >= buffer->size) {
        message("Buffer is read-only: #<buffer FILENAME>");
        return;
    }

    // Skip any whitespace or punctuation before the word
    while (buffer->point < buffer->size && 
           !isWordChar(buffer->content[buffer->point])) {
        buffer->point++;
    }

    // If we've reached the end of the buffer, return
    if (buffer->point >= buffer->size) {
        return;
    }

    // Capitalize the first character of the word
    if (isWordChar(buffer->content[buffer->point])) {
        buffer->content[buffer->point] = toupper((unsigned char)buffer->content[buffer->point]);
        buffer->point++;
    }

    // Convert the rest of the word to lowercase
    while (buffer->point < buffer->size && 
           isWordChar(buffer->content[buffer->point])) {
        buffer->content[buffer->point] = tolower((unsigned char)buffer->content[buffer->point]);
        buffer->point++;
    }
}

// DIFF-HL

// Helper function to compare integers for qsort
static int compare_ints(const void *a, const void *b) {
    int arg1 = *(const int *)a;
    int arg2 = *(const int *)b;
    return (arg1 > arg2) - (arg1 < arg2);
}

void diff_hl_next_hunk(Buffer *buffer) {
    if (buffer->diffs.count == 0)
        return;

    // Collect all unique line numbers from diffs
    int *lines = malloc(buffer->diffs.count * sizeof(int));
    int uniqueCount = 0;
    for (int i = 0; i < buffer->diffs.count; i++) {
        int line = buffer->diffs.array[i].line;
        bool found = false;
        for (int j = 0; j < uniqueCount; j++) {
            if (lines[j] == line) {
                found = true;
                break;
            }
        }
        if (!found) {
            lines[uniqueCount++] = line;
        }
    }

    if (uniqueCount == 0) {
        free(lines);
        return;
    }

    // Sort lines
    qsort(lines, uniqueCount, sizeof(int), compare_ints);

    // Group consecutive lines into hunks and record start lines
    int *hunkStarts = malloc(uniqueCount * sizeof(int));
    int hunkCount = 0;
    hunkStarts[0] = lines[0];
    hunkCount = 1;

    for (int i = 1; i < uniqueCount; i++) {
        if (lines[i] != lines[i - 1] + 1) {
            hunkStarts[hunkCount++] = lines[i];
        }
    }

    // Find current line
    int currentLine = getLineNumber(buffer);

    // Find next hunk
    int nextHunk = -1;
    for (int i = 0; i < hunkCount; i++) {
        if (hunkStarts[i] > currentLine) {
            nextHunk = hunkStarts[i];
            break;
        }
    }

    // Wrap around if needed
    if (nextHunk == -1 && hunkCount > 0) {
        nextHunk = hunkStarts[0];
    }

    if (nextHunk != -1) {
        moveTo(buffer, nextHunk, 0);
    }

    right_char(buffer, 0, 1);
    free(lines);
    free(hunkStarts);
}

void diff_hl_previous_hunk(Buffer *buffer) {
    if (buffer->diffs.count == 0)
        return;

    // Collect all unique line numbers from diffs
    int *lines = malloc(buffer->diffs.count * sizeof(int));
    int uniqueCount = 0;
    for (int i = 0; i < buffer->diffs.count; i++) {
        int line = buffer->diffs.array[i].line;
        bool found = false;
        for (int j = 0; j < uniqueCount; j++) {
            if (lines[j] == line) {
                found = true;
                break;
            }
        }
        if (!found) {
            lines[uniqueCount++] = line;
        }
    }

    if (uniqueCount == 0) {
        free(lines);
        return;
    }

    // Sort lines
    qsort(lines, uniqueCount, sizeof(int), compare_ints);

    // Group consecutive lines into hunks and record start lines
    int *hunkStarts = malloc(uniqueCount * sizeof(int));
    int hunkCount = 0;
    hunkStarts[0] = lines[0];
    hunkCount = 1;

    for (int i = 1; i < uniqueCount; i++) {
        if (lines[i] != lines[i - 1] + 1) {
            hunkStarts[hunkCount++] = lines[i];
        }
    }

    // Find current line
    int currentLine = getLineNumber(buffer);

    // Find previous hunk
    int prevHunk = -1;
    for (int i = hunkCount - 1; i >= 0; i--) {
        if (hunkStarts[i] < currentLine) {
            prevHunk = hunkStarts[i];
            break;
        }
    }

    // Wrap around if needed
    if (prevHunk == -1 && hunkCount > 0) {
        prevHunk = hunkStarts[hunkCount - 1];
    }

    if (prevHunk != -1) {
        moveTo(buffer, prevHunk, 0);
    }

    right_char(buffer, 0, 1);

    free(lines);
    free(hunkStarts);
}



void scroll(Window *window, int arg) {
    float lineHeight = window->buffer->font->ascent + window->buffer->font->descent;
    if (arg == 0) arg = 1;
    window->scroll.y += lineHeight * arg;
}

void scroll_up(Window *window, int arg) {
  if (!window || !window->buffer)
    return;
  if (arg == 0)
    arg = 1;
  Buffer *buffer = window->buffer;
  Font *font = buffer->font;
  float lineHeight = font->ascent + font->descent;
  if (lineHeight <= 0)
    return;

  // Calculate visible lines
  int visibleLines = (int)(window->height / lineHeight) - 1; // -1 for modeline

  // Emacs behavior: Move to new page but keep last two visible lines at top
  float scrollAmount = (visibleLines - 2) * lineHeight * arg;
  float newScrollY = window->targetScrollY + scrollAmount;

  // Bounds checking
  float bufferHeight = buffer->size * lineHeight;
  float maxScrollY = bufferHeight - window->height;
  if (maxScrollY < 0)
    maxScrollY = 0;
  if (newScrollY > maxScrollY)
    newScrollY = maxScrollY;

  window->targetScrollY = newScrollY;
  window->isScrolling = true;

  // Position cursor in visible area - specifically on the first line of new
  // viewport
  int newFirstVisibleLine = (int)(newScrollY / lineHeight);
  moveTo(buffer, newFirstVisibleLine, 0);
}

void scroll_down(Window *window, int arg) {
    if (!window || !window->buffer) return;
    if (arg == 0) arg = 1;
    Buffer *buffer = window->buffer;
    Font *font = buffer->font;
    float lineHeight = font->ascent + font->descent;

    if (lineHeight <= 0) return;

    // Calculate visible lines
    int visibleLines = (int)(window->height / lineHeight) - 1; // -1 for modeline

    // Emacs behavior: Move to previous page but keep first two lines of previous
    // view at bottom
    float scrollAmount = (visibleLines - 2) * lineHeight * arg;
    float newScrollY = window->targetScrollY - scrollAmount;

    // Bounds checking
    if (newScrollY < 0) newScrollY = 0;

    window->targetScrollY = newScrollY;
    window->isScrolling = true;

    // Position cursor on the first line of new viewport
    int newFirstVisibleLine = (int)(newScrollY / lineHeight);
    moveTo(buffer, newFirstVisibleLine, 0);
}


void read_only_mode(Buffer *buffer) {
    buffer->readOnly = !buffer->readOnly;
    if (buffer->readOnly) {
        message("Read-Only mode enabled in current buffer");
    } else {
        message("Read-Only mode disabled in current buffer");
    }
}



// EXTENSION

// Collect and insert Guile symbols into the current buffer

static SCM
symbol_error_handler (void *data, SCM key, SCM args)
{
    (void)data;
    scm_display_error (SCM_BOOL_F, scm_current_error_port(),
                       key,
                       scm_from_locale_string ("Error collecting symbols"),
                       args,
                       SCM_EOL);
    return SCM_BOOL_F;
}

static SCM collect_symbol_info(void *data) {
    (void)data;
    return scm_eval_string(scm_from_locale_string(
                                                  "(use-modules (ice-9 documentation) "
                                                  "             (oop goops) "  // For introspection
                                                  "             (system vm program)) " // For arities
                                                  "(let ((symbols '())) "
                                                  "  (define (get-procedure-info proc sym) "
                                                  "    (let* ((doc (procedure-documentation proc)) "
                                                  "           (arity (procedure-minimum-arity proc)) "
                                                  "           (req (car arity)) "     // Required args
                                                  "           (opt (cadr arity)) "    // Optional args
                                                  "           (rest? (caddr arity))) " // Rest args?
                                                  "      (list (symbol->string sym) "
                                                  "            (cond ((= req 0) "
                                                  "                   (if (> opt 0) \"[arg...]\" \"\")) "
                                                  "                  (else "
                                                  "                    (string-join "
                                                  "                      (append "
                                                  "                        (map (lambda (n) (string-append \"arg\" (number->string n))) "
                                                  "                             (iota req)) "
                                                  "                        (if (> opt 0) '(\"[opt-args...]\") '()) "
                                                  "                        (if rest? '(\"rest...\") '())) "
                                                  "                      \" \"))) "
                                                  "            doc))) "
                                                  "  (define (collect-from-module module) "
                                                  "    (module-for-each "
                                                  "      (lambda (sym var) "
                                                  "        (false-if-exception "  // Handle errors gracefully
                                                  "          (let ((value (variable-ref var))) "
                                                  "            (when (procedure? value) "
                                                  "              (set! symbols (cons "
                                                  "                (get-procedure-info value sym) "
                                                  "                symbols)))))) "
                                                  "      module)) "
                                                  "  (collect-from-module (current-module)) "
                                                  "  (collect-from-module (resolve-interface '(guile))) "
                                                  "  (sort symbols "
                                                  "        (lambda (a b) "
                                                  "          (string<? (car a) (car b)))))"));
}

void insert_guile_symbols(Buffer *buffer, BufferManager *bm) {
    SCM result = scm_c_catch(SCM_BOOL_T,
                             collect_symbol_info,
                             NULL,
                             symbol_error_handler,
                             NULL,
                             NULL,
                             NULL);

    if (scm_is_false(result)) {
        message("Failed to collect Guile symbols.");
        return;
    }

    /* Insert header with usage instructions */
    const char *header =
        "\n;; Guile Interactive Function Reference\n"
        ";; ================================\n"
        ";; Usage:\n"
        ";;  - Each function is shown with its name and arguments\n"
        ";;  - The line below shows example usage you can copy and modify\n"
        ";;  - Full documentation follows\n"
        ";;  - Required arguments are shown as arg0, arg1, etc.\n"
        ";;  - Optional arguments are shown in [brackets]\n"
        ";;  - Rest arguments are shown as rest...\n\n";

    for (const char *p = header; *p; p++) {
        insertChar(buffer, *p);
    }

    size_t count = 0;

    for (SCM lst = result; scm_is_pair(lst); lst = scm_cdr(lst)) {
        SCM info = scm_car(lst);
        char *name = scm_to_locale_string(scm_car(info));
        char *args = scm_to_locale_string(scm_cadr(info));
        char *doc = NULL;
        if (scm_is_string(scm_caddr(info))) {
            doc = scm_to_locale_string(scm_caddr(info));
        }

        if (name) {
            // Function signature
            insertChar(buffer, '(');
            for (char *p = name; *p; p++) {
                insertChar(buffer, *p);
            }
            if (args && *args) {
                insertChar(buffer, ' ');
                for (char *p = args; *p; p++) {
                    insertChar(buffer, *p);
                }
            }
            insertChar(buffer, ')');
            insertChar(buffer, '\n');

            // Example usage line (ready to modify)
            const char *indent = "    ";
            for (const char *p = indent; *p; p++) {
                insertChar(buffer, *p);
            }
            insertChar(buffer, '(');
            for (char *p = name; *p; p++) {
                insertChar(buffer, *p);
            }
            if (args && *args) {
                insertChar(buffer, ' ');
                // Replace arg0, arg1 etc with ... as placeholder
                for (char *p = args; *p; p++) {
                    if (strncmp(p, "arg", 3) == 0) {
                        insertChar(buffer, '.');
                        insertChar(buffer, '.');
                        insertChar(buffer, '.');
                        while (*p && *p != ' ') p++;
                        p--; // Compensate for loop increment
                    } else {
                        insertChar(buffer, *p);
                    }
                }
            }
            insertChar(buffer, ')');
            insertChar(buffer, '\n');

            // Documentation
            if (doc && *doc) {
                const char *doc_indent = "    ;; ";
                for (const char *p = doc_indent; *p; p++) {
                    insertChar(buffer, *p);
                }

                // Word wrap the documentation
                int col = strlen(doc_indent);
                const int wrap_at = 70;
                const char *doc_ptr = doc;

                while (*doc_ptr) {
                    if (col > wrap_at && *doc_ptr == ' ') {
                        insertChar(buffer, '\n');
                        for (const char *p = doc_indent; *p; p++) {
                            insertChar(buffer, *p);
                        }
                        col = strlen(doc_indent);
                        doc_ptr++;
                        continue;
                    }
                    insertChar(buffer, *doc_ptr);
                    col++;
                    doc_ptr++;
                }
                insertChar(buffer, '\n');
            }

            insertChar(buffer, '\n');
            count++;
        }

        if (name) free(name);
        if (args) free(args);
        if (doc) free(doc);
    }

    char msg[128];
    snprintf(msg, sizeof(msg), "Inserted documentation for %zu Guile functions.", count);
    message(msg);
}


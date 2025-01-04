#include "edit.h"
#include "commands.h"
#include "keychords.h"
#include "faces.h"
#include "syntax.h"
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <libguile.h>

// TODO kill_line() should delete the entire line if is composed of only whitespaces
// TODO kill_line() should kr_kill the entire line if its at the beginning of it

// FIXME why arg doesn't work ? 
/* void insertChar(Buffer *buffer, char c, int arg) { */
/*     if (arg <= 0) { */
/*         arg = 1; */
/*     } */

/*     while (buffer->size + arg >= buffer->capacity) { */
/*         buffer->capacity *= 2; */
/*         char *newContent = realloc(buffer->content, buffer->capacity * sizeof(char)); */
/*         if (!newContent) { */
/*             fprintf(stderr, "Failed to reallocate memory for buffer.\n"); */
/*             return; */
/*         } */
/*         buffer->content = newContent; */
/*     } */

/*     for (int i = 0; i < arg; i++) { */
/*         memmove(buffer->content + buffer->point + 1, buffer->content + buffer->point, buffer->size - buffer->point); */
/*         buffer->content[buffer->point] = c; */
/*         buffer->point++; */
/*         buffer->size++; */
/*     } */
/*     buffer->content[buffer->size] = '\0'; */
/* } */


#include "syntax.h"
void insertChar(Buffer *buffer, unsigned int codepoint) {
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

/* void insertChar(Buffer *buffer, char c) { */
/*     if (buffer->size + 1 >= buffer->capacity) { */
/*         buffer->capacity *= 2; */
/*         buffer->content = realloc(buffer->content, buffer->capacity * sizeof(char)); */
/*         if (!buffer->content) { */
/*             fprintf(stderr, "Failed to reallocate memory for buffer.\n"); */
/*             return; */
/*         } */
/*     } */
/*     memmove(buffer->content + buffer->point + 1, buffer->content + buffer->point, buffer->size - buffer->point); */
/*     buffer->content[buffer->point] = c; */
/*     buffer->point++; */
/*     buffer->size++; */
/*     buffer->content[buffer->size] = '\0'; */
/* } */

void beginning_of_buffer(Buffer *buffer) {
    if (buffer != NULL && buffer->content != NULL) {
        buffer->point = 0;
    }
}

void end_of_buffer(Buffer *buffer) {
    if (buffer != NULL && buffer->content != NULL) {
        buffer->point = buffer->size;
    }
}

void right_char(Buffer *buffer, bool shift, BufferManager *bm, int arg) {
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
        message(bm, "End of buffer");
    }
}

void left_char(Buffer *buffer, bool shift, BufferManager *bm, int arg) {
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
        message(bm, "Beginning of buffer");
    }
}

void previous_line(Buffer *buffer, bool shift, BufferManager *bm) {
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
        message(bm, "Beginning of buffer");
        return;
    }

    int previousLineEnd = 0;
    int previousLineStart = 0;
    int currentLineStart = 0;

    for (int i = buffer->point - 1; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            currentLineStart = i + 1;
            break;
        }
    }

    if (currentLineStart == 0) {
        buffer->point = 0;
        message(bm, "Beginning of buffer");
        return;
    }

    for (int i = currentLineStart - 2; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            previousLineStart = i + 1;
            break;
        }
    }

    for (int i = currentLineStart - 1; i >= 0; i--) {
        if (buffer->content[i] == '\n') {
            previousLineEnd = i;
            break;
        }
    }

    int column = buffer->point - currentLineStart;
    int previousLineLength = previousLineEnd - previousLineStart;

    if (column >= previousLineLength) {
        buffer->point = previousLineEnd;
    } else {
        buffer->point = previousLineStart + column;
    }
}

// TODO Goal column
void next_line(Buffer *buffer, bool shift, BufferManager *bm) {
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

    // Calculate the point position in the next line, limited by the line's length.
    size_t targetPosition = nextLineStart + columnPosition;
    if (nextLineStart >= buffer->size) {
        // If no new line to jump to, move cursor to the end and display message
        buffer->point = buffer->size;
        message(bm, "End of buffer");
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
            return;
        }
    }
    buffer->point = buffer->size; // no newline was found, go to the end of buffer
}

void delete_char(Buffer *buffer, BufferManager *bm) {
    if (buffer->point >= buffer->size) {
        message(bm, "End of buffer");
        return;
    }

    if (buffer->region.active) buffer->region.active = false;

    // Move all characters after the cursor left by one position
    memmove(buffer->content + buffer->point, buffer->content + buffer->point + 1,
            buffer->size - buffer->point - 1);
    // Decrease the size of the buffer
    buffer->size--;
    // Null-terminate the string
    buffer->content[buffer->size] = '\0';
}

void kill_line(Buffer *buffer, KillRing *kr) {
    if (buffer->point >= buffer->size) return; // Nothing to delete if at the end of the buffer

    size_t startOfLine = buffer->point;
    size_t endOfLine = startOfLine;

    // Determine the end of the current line, but do not include the newline character
    while (endOfLine < buffer->size && buffer->content[endOfLine] != '\n') {
        endOfLine++;
    }

    size_t numToDelete = endOfLine - startOfLine;

    if (numToDelete > 0) {
        // Capture the text to be killed
        char *cut_text = malloc(numToDelete + 1);
        if (cut_text) {
            memcpy(cut_text, buffer->content + startOfLine, numToDelete);
            cut_text[numToDelete] = '\0';
            kr_kill(kr, cut_text); // Add to kr_kill ring
            free(cut_text); // Free temporary text buffer
        }

        // Shift remaining text in the buffer left over the killed text
        memmove(buffer->content + startOfLine, buffer->content + endOfLine, buffer->size - endOfLine + 1); // +1 for null terminator

        // Update buffer size
        buffer->size -= numToDelete;
    }

    // Handle special case for an empty line or a line where the cursor is right at the newline
    if (startOfLine == endOfLine && startOfLine < buffer->size && buffer->content[startOfLine] == '\n') {
        // Kill the newline itself
        memmove(buffer->content + startOfLine, buffer->content + startOfLine + 1, buffer->size - startOfLine);
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

    memmove(buffer->content + buffer->point + 1, buffer->content + buffer->point,
            buffer->size - buffer->point + 1); // NOTE +1 for null terminator

    buffer->content[buffer->point] = '\n';
    buffer->size++;
}

void delete_indentation(Buffer *buffer, BufferManager *bm, int arg) {
    move_beginning_of_line(buffer, false);

    if (buffer->point > 0) {
        left_char(buffer, false, bm, arg);
        delete_char(buffer, bm);
        insertChar(buffer, ' ');
        left_char(buffer, false, bm, arg);
    }
}

/// NOTE Add one indentation from the beginning of the buffer.
void addIndentation(Buffer *buffer, int indentation) {
    if (buffer->size + indentation >= buffer->capacity) {
        buffer->capacity *= 2;
        char *newContent = realloc(buffer->content, buffer->capacity * sizeof(char));
        if (!newContent) {
            fprintf(stderr, "Failed to reallocate memory for buffer.\n");
            return;
        }
        buffer->content = newContent;
    }

    int lineStart = buffer->point;
    while (lineStart > 0 && buffer->content[lineStart - 1] != '\n') {
        lineStart--;
    }

    // Add spaces at the beginning of the line
    memmove(buffer->content + lineStart + indentation, buffer->content + lineStart, buffer->size - lineStart + 1); // Including null terminator
    for (int i = 0; i < indentation; i++) {
        buffer->content[lineStart + i] = ' ';
    }
    buffer->size += indentation;

    // Adjust cursor position relative to the added spaces if cursor is beyond the added spaces
    if (buffer->point >= lineStart) {
        buffer->point += indentation;
    }
}

/// NOTE Remove one indentation from the beginning of the buffer.
void removeIndentation(Buffer *buffer, int indentation) {
    int lineStart = buffer->point;
    while (lineStart > 0 && buffer->content[lineStart - 1] != '\n') {
        lineStart--;
    }

    // Determine the actual number of spaces we can remove
    int count = 0;
    for (int i = lineStart; i < lineStart + indentation && buffer->content[i] == ' '; i++) {
        count++;
    }

    if (count > 0) {
        memmove(buffer->content + lineStart, buffer->content + lineStart + count, buffer->size - (lineStart + count) + 1); // Including null terminator
        buffer->size -= count;

        // Adjust cursor position relative to the removed spaces if cursor is beyond the removed spaces
        if (buffer->point > lineStart + count) {
            buffer->point -= count;
        } else if (buffer->point > lineStart) {
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



void kill_region(Buffer *buffer, KillRing *kr) {
    if (!buffer->region.active) {
        printf("No active region to kr_kill.\n");
        return;  // No region active, nothing to kr_kill
    }

    size_t start = buffer->region.start;
    size_t end = buffer->region.end;

    // Ensure start is always less than end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    if (end > buffer->size) end = buffer->size;  // Clamp end to buffer size

    size_t region_length = end - start;
    if (region_length == 0) {
        printf("Empty region, nothing to kr_kill.\n");
        return;  // Empty region, nothing to kr_kill
    }

    // Allocate and copy the region to kr_kill
    char *cut_text = malloc(region_length + 1);
    if (cut_text) {
        memcpy(cut_text, buffer->content + start, region_length);
        cut_text[region_length] = '\0';
        kr_kill(kr, cut_text);  // Add to kr_kill ring
        free(cut_text);  // Free temporary text buffer
    }

    // Remove the region text from the buffer
    memmove(buffer->content + start, buffer->content + end, buffer->size - end + 1);
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
    char buffer[128];
    size_t len = 0;

    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
        size_t buffer_len = strlen(buffer);
        char* new_result = realloc(result, len + buffer_len + 1);
        if (!new_result) {
            free(result);
            pclose(pipe);
            return NULL;
        }
        result = new_result;
        strcpy(result + len, buffer);
        len += buffer_len;
    }

    if (result) {
        result[len] = '\0';  // Ensure the string is NULL-terminated
    }

    pclose(pipe);
    return result;
}

// TODO use the arg to yank n times
// or yank at n lines from the cursor line positive or negative
// could be helpful with relative line numbers,
// emacs doesn't seem to use the universal argument for yank

void yank(Buffer *buffer, KillRing *kr, int arg) {
    char *clipboard_text = paste_from_clipboard();
    if (clipboard_text) {
        for (char *p = clipboard_text; *p; p++) {
            insertChar(buffer, *p);
        }
        free(clipboard_text);
    }
}

void kill_ring_save(Buffer *buffer, KillRing *kr) {
    if (!buffer->region.active) return;

    size_t start = buffer->region.start;
    size_t end = buffer->region.end;

    // Ensure start is always less than end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    if (end > buffer->size) end = buffer->size;  // Clamp end to buffer size

    size_t region_length = end - start;
    if (region_length == 0) return;  // Empty region, nothing to save

    char *text_to_save = malloc(region_length + 1);
    if (text_to_save) {
        memcpy(text_to_save, buffer->content + start, region_length);
        text_to_save[region_length] = '\0';
        kr_kill(kr, text_to_save);
        free(text_to_save);
    } else {
        fprintf(stderr, "Failed to allocate memory for kr_kill ring save.\n");
    }
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
    memmove(buffer->content + lineEnd + lineLength, buffer->content + lineEnd, buffer->size - lineEnd);

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
    if (buffer == NULL || buffer->content == NULL) return false;

    if (shift) {
        if (!buffer->region.active) {
            activateRegion(buffer);
        }
    } else {
        if (!buffer->region.marked) buffer->region.active = false;
    }


    size_t pos = buffer->point;
    size_t end = buffer->size;
    int direction = count > 0 ? 1 : -1;

    while (pos < end && count != 0) {
        if (direction > 0) { // Moving forward
            // Skip initial spaces and punctuation
            while (pos < end && (isspace((unsigned char)buffer->content[pos]) || isPunctuationChar(buffer->content[pos])))
                pos++;

            // Move through the word
            while (pos < end && isWordChar(buffer->content[pos])) 
                pos++;

            // If the next character is punctuation, stop before it
            if (pos < end && isPunctuationChar(buffer->content[pos])) 
                break;
        } else { // Moving backward
            // Skip initial spaces and punctuation when moving backward
            while (pos > 0 && (isspace((unsigned char)buffer->content[pos - 1]) || isPunctuationChar(buffer->content[pos - 1])))
                pos--;

            // Move to the start of the word
            while (pos > 0 && isWordChar(buffer->content[pos - 1]))
                pos--;
        }
        count -= direction;
    }

    buffer->point = pos; // Update the cursor position
    return true;
}



bool backward_word(Buffer *buffer, int count, bool shift) {
    if (buffer == NULL || buffer->content == NULL) return false;
    
    // Ensure the cursor starts within the buffer bounds
    if (buffer->point >= buffer->size) {
        buffer->point = buffer->size - 1;
    }

    // Ensure cursor is moved to a valid position if it starts right at a non-character
    if (!isWordChar(buffer->content[buffer->point]) && !isspace(buffer->content[buffer->point]) && buffer->point > 0) {
        buffer->point--;
    }

    return forward_word(buffer, -count, shift);
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
    memmove(buffer->content + start, buffer->content + end, buffer->size - end + 1); // Including null terminator

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
void indent(Buffer *buffer, int indentation, BufferManager *bm, int arg) {
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
        delete_char(buffer, bm); // Delete excess spaces
        currentIndentation--;
    }

    // Correct cursor position based on the previous position of non-whitespace text
    if (old_point >= firstNonWhitespace) {
        buffer->point = old_point - (firstNonWhitespace - cursor_row_start - requiredIndentation);
    } else {
        buffer->point = cursor_row_start + requiredIndentation;
    }
}


// FIXME
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
        indent(buffer, indentation, bm, arg);  // Apply the indent function once per line

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
    } else if (strcmp(prompt->content, "Keep lines containing match for regexp: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        keep_lines(bm, wm);
    } else if (strcmp(prompt->content, "Goto line: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        goto_line(bm, wm, sw, sh);
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        prompt->content = strdup("");
    } else if (strcmp(prompt->content, "Shell command: ") == 0) {
        add_to_history(nh, prompt->content, minibuffer->content);
        cleanBuffer(bm, "prompt");
        execute_shell_command(bm, minibuffer->content);
    } else {
        if (buffer->point > 0 && buffer->point < buffer->size &&
            buffer->content[buffer->point - 1] == '{' && buffer->content[buffer->point] == '}') {
            // Insert a newline and indent for the opening brace
            insertChar(buffer, '\n');
            if (electric_indent_mode) {
                indent(buffer, indentation, bm, arg);
            }

            size_t newCursorPosition = buffer->point;
            insertChar(buffer, '\n');

            if (electric_indent_mode) {
                indent(buffer, indentation, bm, arg);
            }

            buffer->point = newCursorPosition;
        } else {
            insertChar(buffer, '\n');
        }

        if (electric_indent_mode) {
            indent(buffer, indentation, bm, arg);
        }
    }
}

// TODO Dired when calling find_file on a directory
// TODO Create files when they don't exist (and directories to get to that file)


void find_file(BufferManager *bm, WindowManager *wm, int sw, int sh) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    if (minibuffer->size == 0) {
        if (bm->lastBuffer && bm->lastBuffer->path) {
            minibuffer->size = 0;
            minibuffer->content[0] = '\0';
            minibuffer->point = 0;
            setBufferContent(minibuffer, bm->lastBuffer->path);
        }
        free(prompt->content);
        prompt->content = strdup("Find file: ");
        switchToBuffer(bm, "minibuffer");
        return;
    }

    const char *homeDir = getenv("HOME");
    char fullPath[PATH_MAX];
    const char *filePath = minibuffer->content;

    // Resolve full path
    if (filePath[0] == '~') {
        if (homeDir) {
            snprintf(fullPath, sizeof(fullPath), "%s%s", homeDir, filePath + 1);
        } else {
            fprintf(stderr, "Environment variable HOME is not set.\n");
            return;
        }
    } else {
        strncpy(fullPath, filePath, sizeof(fullPath) - 1);
        fullPath[sizeof(fullPath) - 1] = '\0'; // Ensure null termination
    }

    FILE *file = fopen(fullPath, "r");
    if (file) {
        // Creating buffer with '~' notation for user-friendly display
        char displayPath[PATH_MAX];
        if (strncmp(fullPath, homeDir, strlen(homeDir)) == 0) {
            snprintf(displayPath, sizeof(displayPath), "~%s", fullPath + strlen(homeDir));
        } else {
            strncpy(displayPath, fullPath, sizeof(displayPath));
            displayPath[sizeof(displayPath) - 1] = '\0';
        }

        newBuffer(bm, wm, displayPath, displayPath, fontname, sw, sh);
        Buffer *fileBuffer = getBuffer(bm, displayPath);

        if (fileBuffer) {
            fileBuffer->content = malloc(sizeof(char) * 1024);
            fileBuffer->capacity = 1024;
            fileBuffer->size = 0;

            char buffer[1024]; // Temporary buffer for reading file content
            size_t bytesRead;
            while ((bytesRead = fread(buffer, 1, sizeof(buffer), file)) > 0) {
                if (fileBuffer->size + bytesRead >= fileBuffer->capacity) {
                    fileBuffer->capacity = (fileBuffer->size + bytesRead) * 2;
                    char *newContent = realloc(fileBuffer->content, fileBuffer->capacity);
                    if (!newContent) {
                        free(fileBuffer->content);
                        fclose(file);
                        fprintf(stderr, "Failed to allocate memory for file content.\n");
                        return;
                    }
                    fileBuffer->content = newContent;
                }
                memcpy(fileBuffer->content + fileBuffer->size, buffer, bytesRead);
                fileBuffer->size += bytesRead;
            }
            fileBuffer->content[fileBuffer->size] = '\0';
            fclose(file);
            switchToBuffer(bm, fileBuffer->name);
        } else {
            fprintf(stderr, "Failed to retrieve or create buffer for file: %s\n", displayPath);
            fclose(file);
        }
        parseSyntax(fileBuffer);
    } else {
        fprintf(stderr, "File does not exist: %s\n", fullPath);
    }
}

void backspace(Buffer *buffer, bool electric_pair_mode) {
    if (buffer->point > 0 && electric_pair_mode) {
        // Check if backspacing over an opening character that has a closing pair right after
        unsigned int currentChar = buffer->content[buffer->point - 1];
        unsigned int nextChar = buffer->content[buffer->point];
        if ((currentChar == '(' && nextChar == ')') ||
            (currentChar == '[' && nextChar == ']') ||
            (currentChar == '{' && nextChar == '}') ||
            (currentChar == '\'' && nextChar == '\'') ||
            (currentChar == '\"' && nextChar == '\"')) {
            // Remove both characters
            memmove(buffer->content + buffer->point - 1, buffer->content + buffer->point + 1, buffer->size - buffer->point - 1);
            buffer->size -= 2;
            buffer->point--;
            buffer->content[buffer->size] = '\0';
            return;
        }
    }
    // Default backspace behavior when not deleting a pair
    if (buffer->point > 0) {
        buffer->point--;
        memmove(buffer->content + buffer->point, buffer->content + buffer->point + 1, buffer->size - buffer->point);
        buffer->size--;
        buffer->content[buffer->size] = '\0';
    }
}


void execute_shell_command(BufferManager *bm, char *command) {
    char *output = NULL;
    FILE *pipe = popen(command, "r");
    if (pipe == NULL) {
        message(bm, "Failed to execute shell command.");
        return;
    }

    char buffer[128];
    size_t output_size = 0;
    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
        size_t chunk_length = strlen(buffer);
        char *new_output = realloc(output, output_size + chunk_length + 1);
        if (new_output == NULL) {
            free(output);
            pclose(pipe);
            message(bm, "Failed to allocate memory for command output.");
            return;
        }
        output = new_output;
        memcpy(output + output_size, buffer, chunk_length);
        output_size += chunk_length;
    }

    if (output != NULL) {
        output[output_size] = '\0'; // Ensure null-terminated string

        // Remove any trailing newline if present
        if (output_size > 0 && output[output_size - 1] == '\n') {
            output[output_size - 1] = '\0';
        }

        // Set the output directly to the minibuffer's content
        Buffer *minibuffer = getBuffer(bm, "minibuffer");
        if (minibuffer != NULL) {
            setBufferContent(minibuffer, output);
            minibuffer->point = 0; // Reset the cursor position to the start of the buffer
        } else {
            message(bm, "Minibuffer not found.");
        }

        free(output);
    }

    pclose(pipe);
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
            message(bm, "No last buffer to go to.");
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

void execute_extended_command(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    if (bm->lastBuffer && bm->lastBuffer->name) {
        if (minibuffer->size == 0) {
            // Initial setup when entering M-x mode
            minibuffer->size = 0;
            minibuffer->point = 0;
            minibuffer->content[0] = '\0';
            free(prompt->content);
            prompt->content = strdup("M-x ");
            switchToBuffer(bm, "minibuffer");
        } else {
            // Execute the command and handle cleanup
            executeCommand(minibuffer->content);

            // Reset state of the last buffer
            bm->lastBuffer->region.active = false;
            bm->lastBuffer->region.marked = false;

            // Clean up
            cleanBuffer(bm, "minibuffer");
            cleanBuffer(bm, "prompt");
            switchToBuffer(bm, bm->lastBuffer->name);
            cleanBuffer(bm, "message");
        }
    } else {
        message(bm, "No last buffer to go to.");
    }
}


void keep_lines(BufferManager *bm, WindowManager *wm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");

    // Initial minibuffer setup
    if (minibuffer->size == 0) {
        if (bm->lastBuffer && bm->lastBuffer->name) {
            minibuffer->size = 0;
            minibuffer->point = 0;
            minibuffer->content[0] = '\0';
            free(prompt->content);
            prompt->content = strdup("Keep lines containing match for regexp: ");
            switchToBuffer(bm, "minibuffer");
        } else {
            message(bm, "No last buffer to go to.");
        }
        return;
    }

    // Get the pattern from minibuffer
    const char *pattern = minibuffer->content;
    Buffer *buffer = bm->lastBuffer;

    if (!buffer || !pattern || !buffer->content || buffer->size == 0) {
        message(bm, "Invalid buffer or pattern");
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
                message(bm, "Memory allocation failed");
                return;
            }
            
            strncpy(line, buffer->content + line_start, line_length);
            line[line_length] = '\0';

            keep_line = (strstr(line, pattern) != NULL);
            free(line);

            if (keep_line) {
                lines_kept++;
                if (write_pos != line_start) {
                    memmove(buffer->content + write_pos, 
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
    message(bm, msg);

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
            switchToBuffer(bm, "minibuffer");
        } else {
            // Evaluate the expression
            char *result = eval_scheme_string(minibuffer->content);
            message(bm, result);
            free(result);

            // Reset region on the lastBuffer
            bm->lastBuffer->region.active = false;
            bm->lastBuffer->region.marked = false;

            cleanBuffer(bm, "minibuffer");
            cleanBuffer(bm, "prompt");
            switchToBuffer(bm, bm->lastBuffer->name);
        }
    } else {
        message(bm, "No last buffer to go to.");
    }
}

void goto_line(BufferManager *bm, WindowManager *wm, int sw, int sh) {
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
            message(bm, "No last buffer to go to.");
        }
        return;
    }

    // Parse the input as a line number and navigate
    long line_number = strtol(minibuffer->content, NULL, 10); // Parse the input to get the line number
    if (line_number <= 0) {
        message(bm, "Invalid line number.");
        return;
    }

    // Assume last buffer is the target unless otherwise specified
    Buffer *targetBuffer = bm->lastBuffer;
    if (!targetBuffer || !targetBuffer->content) {
        message(bm, "No valid last buffer.");
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
        message(bm, "Line number exceeds total number of lines in the last buffer.");
    }

    // Clear minibuffer after operation
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    prompt->content = strdup("");
    switchToBuffer(bm, targetBuffer->name);
}


void navigate_list(Buffer *buffer, int arg) {
    int direction = (arg > 0) ? 1 : -1;
    int groupsToMove = abs(arg);
    int depth = 0;
    size_t pos = buffer->point;
    bool foundGroup = false;

    while (groupsToMove > 0 && pos >= 0 && pos < buffer->size) {
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
            }
        }
        pos += direction;
    }

    if (foundGroup && groupsToMove == 0) {
        buffer->point = pos - direction;  // Adjust position back to the last valid position
    } else {
        printf("No %s group\n", (arg > 0) ? "next" : "previous");
    }
}

void forward_list(Buffer *buffer, int arg) {
    if (arg == 0) arg = 1;  // Default to moving across one group
    navigate_list(buffer, arg);
    buffer->point += 1;
}

void backward_list(Buffer *buffer, int arg) {
    if (arg == 0) arg = 1;  // Default to moving across one group
    navigate_list(buffer, -arg);
}

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
        memmove(buffer->content + start + 1, buffer->content + end, length - end + 1); // +1 for null terminator
        buffer->size = buffer->size - (end - start - 1);
        buffer->content[start] = '\n'; // Set a single newline at the start
        buffer->point = start; // Set point at the beginning of the preserved newline
    }
    insertChar(buffer, '\n');
}

#include <errno.h>

// TODO (no changes need to saved)
// NOTE How to track it internally ?
// a dirty buffer system ?
void save_buffer(BufferManager *bm, Buffer *buffer) {
    // Check if the buffer is NULL or if it's read-only
    if (buffer == NULL || buffer->readOnly) {
        message(bm, "Cannot save a read-only buffer.");
        return;
    }

    // Check if the buffer has a valid path
    if (buffer->path == NULL || strlen(buffer->path) == 0) {
        message(bm, "No file path specified.");
        return;
    }

    // Resolve the full path if the path starts with '~'
    const char *homeDir = getenv("HOME");
    char fullPath[PATH_MAX];

    if (buffer->path[0] == '~') {
        if (homeDir) {
            snprintf(fullPath, sizeof(fullPath), "%s%s", homeDir, buffer->path + 1);
        } else {
            message(bm, "Environment variable HOME is not set.");
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
        message(bm, errMsg);
        return;
    }

    // Write the content to the file
    size_t written = fwrite(buffer->content, sizeof(char), buffer->size, file);
    if (written != buffer->size) {
        fclose(file);
        message(bm, "Error writing to file.");
        return;
    }

    // Close the file after writing
    fclose(file);

    // Display a message indicating success using the full path
    char msg[512];
    snprintf(msg, sizeof(msg), "Wrote %s", fullPath);
    message(bm, msg);
}

void recenter(Window *window) {
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
    window->scroll.y = cursorY - verticalCenter + lineHeight / 2;
    // Clamp the scroll position to avoid scrolling beyond the content
    float maxScroll = buffer->size * lineHeight - window->height;
    window->scroll.y = fmax(0, fmin(window->scroll.y, maxScroll));
}

int recenterState = 0; // 0: Initial, 1: Top, 2: Center, 3: Bottom

void recenter_top_bottom(Window *window) {
    if (!window || !window->buffer) return;

    Buffer *buffer = window->buffer;
    Font *font = buffer->font;
    float lineHeight = font->ascent + font->descent;

    // Calculate the cursor's current line number dynamically
    int cursorLine = 0;
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') cursorLine++;
    }

    float cursorY = cursorLine * lineHeight;

    // Determine the new scroll position based on the recenter state
    switch (recenterState) {
    case 0:  // Center
        recenter(window);  // Use existing recenter function
        break;
    case 1:  // Top
        window->scroll.y = cursorY;
        break;
    case 2:  // Bottom
        window->scroll.y = cursorY - window->height + lineHeight;
        break;
    default:
        break;
    }

    // Clamp the scroll position
    float maxScroll = buffer->size * lineHeight - window->height;
    window->scroll.y = fmax(0, fmin(window->scroll.y, maxScroll));

    // Cycle the recenter state
    recenterState = (recenterState + 1) % 3;
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




// SECOND
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
    if (!buffer) {
        message(bm, "No buffer to insert symbols into.");
        return;
    }

    SCM result = scm_c_catch(SCM_BOOL_T,
                             collect_symbol_info,
                             NULL,
                             symbol_error_handler,
                             NULL,
                             NULL,
                             NULL);

    if (scm_is_false(result)) {
        message(bm, "Failed to collect Guile symbols.");
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
    message(bm, msg);
}


// BASE
/* static SCM */
/* collect_symbols (void *data) */
/* { */
/*     (void)data; */
/*     return scm_eval_string (scm_from_locale_string ( */
/*                                                     "(sort (map (lambda (s) (symbol->string s)) \ */
/*            (append (module-map (lambda (sym var) sym) \ */
/*                              (current-module)) \ */
/*                   (module-map (lambda (sym var) sym) \ */
/*                              (resolve-interface '(guile))))) \ */
/*           string<?)")); */
/* } */

/* void */
/* insert_guile_symbols (Buffer *buffer, BufferManager *bm) */
/* { */
/*     if (!buffer) */
/*         { */
/*             message (bm, "No buffer to insert symbols into."); */
/*             return; */
/*         } */

/*     SCM result = scm_c_catch (SCM_BOOL_T, */
/*                               collect_symbols, */
/*                               NULL, */
/*                               symbol_error_handler, */
/*                               NULL, */
/*                               NULL, */
/*                               NULL); */

/*     if (scm_is_false (result)) */
/*         { */
/*             message (bm, "Failed to collect Guile symbols."); */
/*             return; */
/*         } */

/*     /\* Insert header *\/ */
/*     const char *header = "\n;; Available Guile Symbols:\n\n"; */
/*     for (const char *p = header; *p; p++) */
/*         insertChar (buffer, *p); */

/*     size_t count = 0; */

/*     /\* Process each symbol *\/ */
/*     for (SCM lst = result; scm_is_pair (lst); lst = scm_cdr (lst)) */
/*         { */
/*             SCM str = scm_car (lst); */
/*             char *symbol_name = scm_to_locale_string (str); */

/*             if (symbol_name) */
/*                 { */
/*                     /\* Insert with prefix *\/ */
/*                     const char *prefix = ";; "; */
/*                     for (const char *p = prefix; *p; p++) */
/*                         insertChar (buffer, *p); */

/*                     for (char *p = symbol_name; *p; p++) */
/*                         insertChar (buffer, *p); */
          
/*                     insertChar (buffer, '\n'); */
/*                     free (symbol_name); */
/*                     count++; */
/*                 } */
/*         } */

/*     /\* Report completion *\/ */
/*     char msg[128]; */
/*     snprintf (msg, sizeof(msg), "Inserted %zu Guile symbols into buffer.", count); */
/*     message (bm, msg); */
/* } */

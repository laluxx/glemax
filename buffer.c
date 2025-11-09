#include "buffer.h"
#include <ctype.h>

Buffer *buffer;

bool blink_cursor_mode = true;
size_t blink_cursor_blinks = 10;
float blink_cursor_interval = 0.5;
float blink_cursor_delay = 0.5;

bool visible_mark_mode = false;

int arg = 1;
bool mark_word_navigation = true;

bool shift;
bool ctrl;
bool alt;


KeyChordAction last_command = NULL;
bool last_command_was_kill = false;

bool is_kill_command(KeyChordAction action) {
    return action == kill_word ||
           action == backward_kill_word ||
           action == kill_line ||
           action == kill_region;
}

Buffer* buffer_create(Font *font) {
    Buffer *buffer = (Buffer*)malloc(sizeof(Buffer));
    if (!buffer) return NULL;
    
    buffer->rope = rope_new();
    buffer->pt = 0;
    buffer->cursor.x = 0;
    buffer->cursor.y = 0;
    buffer->cursor.visible = true;
    buffer->cursor.last_blink = 0.0;
    buffer->cursor.blink_count = 0;
    buffer->font = font;

    buffer->region.active = false;
    buffer->region.mark = 0;

    return buffer;
}

void buffer_destroy(Buffer *buffer) {
    if (!buffer) return;
    rope_free(buffer->rope);
    free(buffer);
}

void reset_cursor_blink(Buffer *buffer) {
    if (blink_cursor_mode) {
        buffer->cursor.blink_count = 0;
        buffer->cursor.last_blink = getTime();
        buffer->cursor.visible = true;
    }
}

void insert(uint32_t codepoint) {
    char utf8[5] = {0};
    size_t len = 0;
    
    if (codepoint < 0x80) {
        utf8[0] = (char)codepoint;
        len = 1;
    } else if (codepoint < 0x800) {
        utf8[0] = 0xC0 | (codepoint >> 6);
        utf8[1] = 0x80 | (codepoint & 0x3F);
        len = 2;
    } else if (codepoint < 0x10000) {
        utf8[0] = 0xE0 | (codepoint >> 12);
        utf8[1] = 0x80 | ((codepoint >> 6) & 0x3F);
        utf8[2] = 0x80 | (codepoint & 0x3F);
        len = 3;
    } else if (codepoint < 0x110000) {
        utf8[0] = 0xF0 | (codepoint >> 18);
        utf8[1] = 0x80 | ((codepoint >> 12) & 0x3F);
        utf8[2] = 0x80 | ((codepoint >> 6) & 0x3F);
        utf8[3] = 0x80 | (codepoint & 0x3F);
        len = 4;
    }
    
    if (len > 0) {

        if (buffer->pt < buffer->region.mark) buffer->region.mark++;

        buffer->rope = rope_insert_chars(buffer->rope, buffer->pt, utf8, len);
        buffer->pt++;
        update_goal_column();
    }
}

size_t delete(size_t pos, size_t count) {
    if (count == 0) return pos;
    
    size_t text_len = rope_char_length(buffer->rope);
    if (pos >= text_len) return pos;
    
    // Clamp count to available characters
    if (pos + count > text_len) {
        count = text_len - pos;
    }
    
    // Update mark if region is active
    size_t delete_end = pos + count;
    if (buffer->region.mark >= delete_end) {
        buffer->region.mark -= count;
    } else if (buffer->region.mark > pos) {
        buffer->region.mark = pos;
    }

    buffer->rope = rope_delete_chars(buffer->rope, pos, count);
    return pos;
}

void delete_backward_char() {
    if (buffer->region.active) {
        delete_region();
    } else

    if (buffer->pt > 0) {
        delete(buffer->pt - 1, 1);
        buffer->pt--;
    }
}

void delete_char() {
    delete(buffer->pt, 1);
}

void newline() {
    insert('\n');
}

void open_line() {
    insert('\n');
    buffer->pt--;
}

void forward_char() {
    size_t text_len = rope_char_length(buffer->rope); // O(1) cached!
    if (buffer->pt < text_len) {
        buffer->pt++;
    }
}

void backward_char() {
    if (buffer->pt > 0) {
        buffer->pt--;
    }
}

size_t line_beginning_position() {
    size_t pos = buffer->pt;
    
    // Scan backwards to find newline or start of buffer
    while (pos > 0) {
        uint32_t ch = rope_char_at(buffer->rope, pos - 1);
        if (ch == '\n') break;
        pos--;
    }
    
    return pos;
}

size_t line_end_position() {
    size_t text_len = rope_char_length(buffer->rope); // O(1) - uses cached value!
    size_t pos = buffer->pt;
    
    while (pos < text_len) {
        uint32_t ch = rope_char_at(buffer->rope, pos);
        if (ch == '\n') break;
        pos++;
    }
    
    return pos;
}

size_t current_column() {
    size_t line_start = line_beginning_position();
    return buffer->pt - line_start;
}

void update_goal_column() {
    buffer->cursor.goal_column = current_column();
}


void next_line() {
    size_t text_len = rope_char_length(buffer->rope); // O(1) cached
    
    // Find the end of current line
    size_t pos = buffer->pt;
    while (pos < text_len && rope_char_at(buffer->rope, pos) != '\n') {
        pos++;
    }
    
    // If we're at end of buffer, do nothing
    if (pos >= text_len) {
        return;
    }
    
    // Move past the newline
    pos++;
    
    // Now move to the goal column on the next line
    size_t line_start = pos;
    size_t line_end = pos;
    while (line_end < text_len && rope_char_at(buffer->rope, line_end) != '\n') {
        line_end++;
    }
    
    // Move to goal column or end of line, whichever is shorter
    size_t line_length = line_end - line_start;
    if (buffer->cursor.goal_column <= line_length) {
        buffer->pt = line_start + buffer->cursor.goal_column;
    } else {
        buffer->pt = line_end;
    }
}

void previous_line() {
    // Find the beginning of current line
    size_t pos = buffer->pt;
    while (pos > 0 && rope_char_at(buffer->rope, pos - 1) != '\n') {
        pos--;
    }
    
    // If we're at the first line, do nothing
    if (pos == 0) {
        return;
    }
    
    // Move to before the newline of previous line
    pos--;
    
    // Find the beginning of the previous line
    size_t line_start = pos;
    while (line_start > 0 && rope_char_at(buffer->rope, line_start - 1) != '\n') {
        line_start--;
    }
    
    // Calculate line length
    size_t line_length = pos - line_start;
    
    // Move to goal column or end of line, whichever is shorter
    if (buffer->cursor.goal_column <= line_length) {
        buffer->pt = line_start + buffer->cursor.goal_column;
    } else {
        buffer->pt = pos;
    }
}

void end_of_line() {
    buffer->pt = line_end_position();
}

void beginning_of_line() {
    buffer->pt = line_beginning_position();
}

/// REGION

bool transient_mark_mode = true;

void set_mark_command() {
    buffer->region.mark = buffer->pt;
    if (transient_mark_mode) buffer->region.active = true;
}

void exchange_point_and_mark() {
    size_t temp = buffer->pt;
    buffer->pt = buffer->region.mark;
    buffer->region.mark = temp;
}

// Get the bounds of the region (always returns min, max order)
void region_bounds(size_t *start, size_t *end) {
    if (buffer->region.mark < buffer->pt) {
        *start = buffer->region.mark;
        *end = buffer->pt;
    } else {
        *start = buffer->pt;
        *end = buffer->region.mark;
    }
}

void delete_region() {
    size_t start, end;
    region_bounds(&start, &end);
    
    // Nothing to delete if start == end
    if (start == end) {
        buffer->region.active = false;
        return;
    }
    
    size_t length = end - start;
    delete(start, length);
    buffer->pt = start;
    buffer->region.active = false;
}

void kill(size_t start, size_t end, bool prepend) {
    if (start >= end) return;
    
    size_t length = end - start;
    size_t buffer_size = length * 4 + 1;
    char *new_text = (char *)malloc(buffer_size);
    
    if (!new_text) return;
    
    size_t copied = rope_copy_chars(buffer->rope, start, length, new_text, buffer_size - 1);
    new_text[copied] = '\0';
    
    if (last_command_was_kill) {
        const char *existing = getClipboardString();
        if (existing && *existing) {
            size_t total_len = strlen(existing) + copied + 1;
            char *combined = (char *)malloc(total_len);
            if (prepend) {
                snprintf(combined, total_len, "%s%s", new_text, existing);
            } else {
                snprintf(combined, total_len, "%s%s", existing, new_text);
            }
            setClipboardString(combined);
            free(combined);
        } else {
            setClipboardString(new_text);
        }
    } else {
        setClipboardString(new_text);
    }
    
    free(new_text);
    delete(start, length);
}

void kill_line() {
    size_t line_end = line_end_position();
    size_t text_len = rope_char_length(buffer->rope);
    
    size_t start = buffer->pt;
    size_t end;
    
    if (buffer->pt == line_end && buffer->pt < text_len) {
        end = buffer->pt + 1;
    } else if (buffer->pt < line_end) {
        end = line_end;
    } else {
        return;
    }
    
    kill(start, end, false);
}

void kill_region() {
    size_t start, end;
    region_bounds(&start, &end);
    
    if (start == end) {
        buffer->region.active = false;
        return;
    }
    
    kill(start, end, false);
    buffer->pt = start;
    buffer->region.active = false;
}

void yank() {
    const char *clipboard_text = getClipboardString();
    
    if (clipboard_text && *clipboard_text) {
        buffer->region.mark = buffer->pt;
        // Insert each character from the clipboard
        const char *p = clipboard_text;
        while (*p) {
            // Decode UTF-8
            uint32_t codepoint = 0;
            int bytes = 0;
            
            unsigned char c = (unsigned char)*p;
            if (c < 0x80) {
                codepoint = c;
                bytes = 1;
            } else if ((c & 0xE0) == 0xC0) {
                codepoint = c & 0x1F;
                bytes = 2;
            } else if ((c & 0xF0) == 0xE0) {
                codepoint = c & 0x0F;
                bytes = 3;
            } else if ((c & 0xF8) == 0xF0) {
                codepoint = c & 0x07;
                bytes = 4;
            } else {
                p++; // Invalid UTF-8, skip
                continue;
            }
            
            // Read continuation bytes
            for (int i = 1; i < bytes; i++) {
                p++;
                if (!*p || (*p & 0xC0) != 0x80) {
                    bytes = i; // Incomplete sequence
                    break;
                }
                codepoint = (codepoint << 6) | (*p & 0x3F);
            }
            
            insert(codepoint);
            p++;
        }
    }
}

/// WORD

bool isWordChar(uint32_t c) {
    if (c >= ASCII) return false;
    return isalnum((unsigned char)c);
}

bool isPunctuationChar(uint32_t c) {
    if (c >= ASCII) return false;
    return strchr(",.;:!?'\"(){}[]<>-+*/=&|^%$#@~_", (char)c) != NULL;
}

/* Move to the end of the current word - EFFICIENT VERSION */
size_t end_of_word(Buffer *buffer, size_t pos) {
    size_t text_len = rope_char_length(buffer->rope);
    if (pos >= text_len) return text_len;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, pos);
    
    uint32_t ch;
    if (!rope_iter_next_char(&iter, &ch)) {
        rope_iter_destroy(&iter);
        return pos;
    }
    
    bool in_word = isWordChar(ch);
    
    if (in_word) {
        // Skip word characters
        while (rope_iter_next_char(&iter, &ch) && isWordChar(ch)) {
            // Keep going
        }
    } else {
        // Skip non-word characters until we hit a word
        while (rope_iter_next_char(&iter, &ch) && !isWordChar(ch)) {
            // Keep going
        }
        // Then skip the word
        while (rope_iter_next_char(&iter, &ch) && isWordChar(ch)) {
            // Keep going
        }
    }
    
    // iter.char_pos is now one past the last character we read
    // We want to return the position after the last word char we saw
    size_t result = iter.char_pos > 0 ? iter.char_pos - 1 : 0;
    
    // If we stopped because of EOF, don't subtract
    if (iter.char_pos >= text_len) {
        result = text_len;
    }
    
    rope_iter_destroy(&iter);
    return result;
}

/* Move to beginning - uses rope_copy_chars for local buffering */
size_t beginning_of_word(Buffer *buffer, size_t pos) {
    if (pos == 0) return 0;
    
    // Copy up to 200 characters before pos for local scanning (increased buffer)
    size_t scan_start = pos > 200 ? pos - 200 : 0;
    size_t scan_len = pos - scan_start;
    
    char buf[1024];  // Increased buffer size
    size_t copied = rope_copy_chars(buffer->rope, scan_start, scan_len, buf, sizeof(buf) - 1);
    
    if (copied == 0) return scan_start;
    
    // Build array of character positions
    size_t byte_positions[201];
    byte_positions[0] = 0;
    
    size_t char_count = 0;
    size_t byte_pos = 0;
    
    while (byte_pos < copied && char_count < scan_len) {
        size_t char_len = utf8_char_len((uint8_t)buf[byte_pos]);
        if (byte_pos + char_len > copied) break;
        byte_pos += char_len;
        char_count++;
        byte_positions[char_count] = byte_pos;
    }
    
    if (char_count == 0) return scan_start;
    
    // Start from the character before pos
    size_t rel_pos = char_count;
    
    // Decode character before position
    size_t bytes_read;
    uint32_t ch = utf8_decode(buf + byte_positions[rel_pos - 1],
                              copied - byte_positions[rel_pos - 1], &bytes_read);
    
    if (isWordChar(ch)) {
        // We're in a word, skip backwards to start of word
        while (rel_pos > 0) {
            ch = utf8_decode(buf + byte_positions[rel_pos - 1],
                           copied - byte_positions[rel_pos - 1], &bytes_read);
            if (!isWordChar(ch)) break;
            rel_pos--;
        }
    } else {
        // We're not in a word, skip backwards over non-word chars
        while (rel_pos > 0) {
            ch = utf8_decode(buf + byte_positions[rel_pos - 1],
                           copied - byte_positions[rel_pos - 1], &bytes_read);
            if (isWordChar(ch)) break;
            rel_pos--;
        }
        
        // Then skip backwards over the word we found
        while (rel_pos > 0) {
            ch = utf8_decode(buf + byte_positions[rel_pos - 1],
                           copied - byte_positions[rel_pos - 1], &bytes_read);
            if (!isWordChar(ch)) break;
            rel_pos--;
        }
    }
    
    return scan_start + rel_pos;
}

void forward_word() {
    if (arg == 0) return;

    // Handle region activation like Emacs does
    if (shift && !buffer->region.active) {
        buffer->region.active = true;
    } else if (!shift && buffer->region.active) {
        buffer->region.active = false;
    }

    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);

    // Simple movement when shift is held (no special marking)
    if (shift) {
        while (count-- > 0) {
            if (direction > 0) {
                buffer->pt = end_of_word(buffer, buffer->pt);
            } else {
                buffer->pt = beginning_of_word(buffer, buffer->pt);
            }
        }
        return;
    }

    // Precise word marking behavior
    if (mark_word_navigation) {
        if (direction > 0) {
            // Forward word marking
            size_t new_pos = buffer->pt;
            for (int i = 0; i < count; i++) {
                new_pos = end_of_word(buffer, new_pos);
            }
            
            buffer->region.mark = beginning_of_word(buffer, new_pos);
            buffer->pt = new_pos;
        } else {
            // Backward word marking
            size_t new_pos = buffer->pt;
            for (int i = 0; i < count; i++) {
                new_pos = beginning_of_word(buffer, new_pos);
            }
            
            buffer->region.mark = end_of_word(buffer, new_pos);
            buffer->pt = new_pos;
        }
    } else {
        // Simple movement without marking
        while (count-- > 0) {
            if (direction > 0) {
                buffer->pt = end_of_word(buffer, buffer->pt);
            } else {
                buffer->pt = beginning_of_word(buffer, buffer->pt);
            }
        }
    }
}

void backward_word() {
    arg = -abs(arg); 
    forward_word();
}

void kill_word() {
    size_t start = buffer->pt;
    size_t end = end_of_word(buffer, start);
    if (start == end) return;
    
    kill(start, end, false);  // Append to end
    buffer->pt = start;
}

void backward_kill_word() {
    size_t end = buffer->pt;
    size_t start = beginning_of_word(buffer, end);
    if (start == end) return;
    
    kill(start, end, true);  // Prepend to beginning
    buffer->pt = start;
}

void draw_cursor(Buffer *buffer, float start_x, float start_y) {
    float x = start_x;
    float y = start_y;
    float line_height = buffer->font->ascent + buffer->font->descent;
    size_t text_len = rope_char_length(buffer->rope);
    int lineCount = 0;
    
    // Use iterator to calculate cursor position - O(n) instead of O(n log n)
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    while (i < buffer->pt && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            lineCount++;
            x = start_x;
        } else if (ch < ASCII) {
            x += buffer->font->characters[ch].ax;
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    y = start_y - lineCount * line_height - (buffer->font->descent * 2);
    
    buffer->cursor.x = x;
    buffer->cursor.y = y;
    
    // Determine cursor width - check character at cursor position
    float cursor_width = buffer->font->characters[' '].ax; // Default
    
    if (buffer->pt < text_len) {
        uint32_t ch = rope_char_at(buffer->rope, buffer->pt);
        if (ch == '\n') {
            cursor_width = buffer->font->characters[' '].ax;
        } else if (ch < ASCII) {
            cursor_width = buffer->font->characters[ch].ax;
        }
    }
    
    float cursor_height = buffer->font->ascent + buffer->font->descent;
    
    // Handle cursor blinking
    if (blink_cursor_mode && buffer->cursor.blink_count < blink_cursor_blinks) {
        double currentTime = getTime();
        double interval = buffer->cursor.visible ? blink_cursor_interval : blink_cursor_delay;
        
        if (currentTime - buffer->cursor.last_blink >= interval) {
            buffer->cursor.visible = !buffer->cursor.visible;
            buffer->cursor.last_blink = currentTime;
            if (buffer->cursor.visible) {
                buffer->cursor.blink_count++;
            }
        }
        
        if (buffer->cursor.visible) {
            quad2D((vec2){buffer->cursor.x, buffer->cursor.y},
                   (vec2){cursor_width, cursor_height}, CT.cursor);
        }
    } else {
        // Always draw when blink limit reached or mode disabled
        quad2D((vec2){buffer->cursor.x, buffer->cursor.y},
               (vec2){cursor_width, cursor_height}, CT.cursor);
    }
}

void draw_mark(Buffer *buffer, float start_x, float start_y) {
    if (buffer->region.mark == buffer->pt) return;
    float mark_x = start_x;
    float mark_y = start_y;
    float line_height = buffer->font->ascent + buffer->font->descent;
    size_t text_len = rope_char_length(buffer->rope);
    int lineCount = 0;
    
    // Use iterator to calculate mark position
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    while (i < buffer->region.mark && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            lineCount++;
            mark_x = start_x;
        } else if (ch < ASCII) {
            mark_x += buffer->font->characters[ch].ax;
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    mark_y = start_y - lineCount * line_height - (buffer->font->descent * 2);
    
    // Determine mark width
    float mark_width = buffer->font->characters[' '].ax;
    
    if (buffer->region.mark < text_len) {
        uint32_t ch = rope_char_at(buffer->rope, buffer->region.mark);
        if (ch == '\n') {
            mark_width = buffer->font->characters[' '].ax;
        } else if (ch < ASCII) {
            mark_width = buffer->font->characters[ch].ax;
        }
    }
    
    float mark_height = buffer->font->ascent + buffer->font->descent;
    
    quad2D((vec2){mark_x, mark_y}, (vec2){mark_width, mark_height}, CT.function);
}

void draw_buffer(Buffer *buffer, float start_x, float start_y) {
    float x = start_x;
    float y = start_y;
    float line_height = buffer->font->ascent + buffer->font->descent;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    while (rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            x = start_x;
            y -= line_height;
        } else if (ch < ASCII) {
            Color char_color = (i == buffer->pt && buffer->cursor.visible || i == buffer->region.mark && visible_mark_mode) ? CT.bg : CT.text;
            float advance = character(buffer->font, (unsigned char)ch, x, y, char_color);
            x += advance;
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    

    if (visible_mark_mode) draw_mark(buffer, start_x, start_y);
    draw_cursor(buffer, start_x, start_y);
}

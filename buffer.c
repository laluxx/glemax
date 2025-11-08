#include "buffer.h"

Buffer *buffer;

bool blink_cursor_mode = true;
size_t blink_cursor_blinks = 10;
float blink_cursor_interval = 0.5;
float blink_cursor_delay = 0.5;

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
        buffer->rope = rope_insert_chars(buffer->rope, buffer->pt, utf8, len);
        buffer->pt++;
        update_goal_column();
    }
}

void delete_backward_char() {
    if (buffer->pt > 0) {
        buffer->rope = rope_delete_chars(buffer->rope, buffer->pt - 1, 1);
        buffer->pt--;
        update_goal_column();
    }
}

void delete_char() {
    buffer->rope = rope_delete_chars(buffer->rope, buffer->pt, 1);
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
    update_goal_column();
}

void backward_char() {
    if (buffer->pt > 0) {
        buffer->pt--;
    }
    update_goal_column();
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
    update_goal_column();
}

void beginning_of_line() {
    buffer->pt = line_beginning_position();
    update_goal_column();
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
            Color char_color = (i == buffer->pt && buffer->cursor.visible) ? CT.bg : CT.text;
            float advance = character(buffer->font, (unsigned char)ch, x, y, char_color);
            x += advance;
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    draw_cursor(buffer, start_x, start_y);
}

#include "buffer.h"
#include "wm.h"
#include <ctype.h>
#include <stdbool.h>

Buffer *all_buffers = NULL;
Buffer *current_buffer = NULL;

int arg = 1;
bool argument_manually_set = false;

bool shift;
bool ctrl;
bool alt;
 
/* KeyChordAction last_command = NULL; */

SCM last_command = SCM_BOOL_F; // Shouldn’t it be NULL ? 
bool last_command_was_kill = false;


#include "lisp.h"

bool is_kill_command(SCM proc) {
    return is_scm_proc(proc, "kill-word") ||
           is_scm_proc(proc, "backward-kill-word") ||
           is_scm_proc(proc, "kill-line") ||
           is_scm_proc(proc, "kill-region");
}

Buffer* buffer_create(Font *font, const char *name) {
    Buffer *buffer = (Buffer*)malloc(sizeof(Buffer));
    if (!buffer) return NULL;
    
    buffer->name = strdup(name);
    buffer->rope = rope_new();
    buffer->pt = 0;
    buffer->cursor.x = 0;
    buffer->cursor.y = 0;
    buffer->cursor.visible = true;
    buffer->cursor.last_blink = 0.0;
    buffer->cursor.blink_count = 0;
    buffer->cursor.goal_column = 0;
    buffer->font = font;
    buffer->region.active = false;
    buffer->region.mark = 0;
    
    // Add to circular buffer list
    if (all_buffers == NULL) {
        // First buffer - create circular list of one
        buffer->next = buffer;
        buffer->prev = buffer;
        all_buffers = buffer;
        current_buffer = buffer;
    } else {
        // Insert at end of list (before all_buffers)
        buffer->next = all_buffers;
        buffer->prev = all_buffers->prev;
        all_buffers->prev->next = buffer;
        all_buffers->prev = buffer;
    }
    
    return buffer;
}

void buffer_destroy(Buffer *buffer) {
    if (!buffer) return;
    
    // Remove from circular list
    if (buffer->next == buffer) {
        // Last buffer in list
        all_buffers = NULL;
        current_buffer = NULL;
    } else {
        buffer->prev->next = buffer->next;
        buffer->next->prev = buffer->prev;
        
        // Update head pointer if we're removing the head
        if (all_buffers == buffer) {
            all_buffers = buffer->next;
        }
        
        // Update current_buffer if we're removing it
        if (current_buffer == buffer) {
            current_buffer = buffer->next;
        }
    }
    
    free(buffer->name);
    rope_free(buffer->rope);
    free(buffer);
}

Buffer *get_buffer(const char *name) {
    if (!all_buffers) return NULL;
    
    Buffer *buf = all_buffers;
    do {
        if (strcmp(buf->name, name) == 0) {
            return buf;
        }
        buf = buf->next;
    } while (buf != all_buffers);
    
    return NULL;
}

Buffer *get_buffer_create(Font *font, const char *name) {
    Buffer *buf = get_buffer(name);
    if (buf) return buf;
    
    return buffer_create(font, name);
}

void switch_to_buffer(Buffer *buf) {
    if (!buf) return;


    if (buf == wm.minibuffer_window->buffer) {
        message("Can't switch to minibuf buffer");
        return; 
    }
    
    current_buffer = buf;
    
    // Update selected window to point to new buffer
    if (wm.selected) {
        wm.selected->buffer = buf;
        wm.selected->point = buf->pt;
    }
}

Buffer* other_buffer() {
    if (!current_buffer || !current_buffer->next) return current_buffer;
    
    Buffer *buf = current_buffer->next;
    Buffer *minibuf = wm.minibuffer_window->buffer;
    
    // Skip minibuffer and return to start if needed
    while (buf != current_buffer) {
        if (buf != minibuf) {
            return buf;
        }
        buf = buf->next;
    }
    
    return current_buffer;
}

void kill_buffer(Buffer *buf) {
    if (!buf) return;
    
    // Don't kill the last buffer
    if (buf->next == buf) {
        message("Cannot kill last buffer");
        return;
    }
    
    // If killing current buffer, switch to next one first
    if (buf == current_buffer) {
        switch_to_buffer(buf->next);
    }
    
    // Update all windows pointing to this buffer
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    
    for (int i = 0; i < count; i++) {
        if (leaves[i]->buffer == buf) {
            leaves[i]->buffer = current_buffer;
            leaves[i]->point = current_buffer->pt;
        }
    }
    
    buffer_destroy(buf);
}


void next_buffer() {
    if (arg == 0) return;
    
    // Don't switch in minibuffer
    if (current_buffer == wm.minibuffer_window->buffer) {
        message("Cannot switch buffers in minibuffer window");
        return;
    }
    
    Buffer *minibuf = wm.minibuffer_window->buffer;
    int count = abs(arg);
    int direction = arg > 0 ? 1 : -1;
    
    for (int i = 0; i < count; i++) {
        Buffer *start = current_buffer;
        Buffer *next = direction > 0 ? current_buffer->next : current_buffer->prev;
        
        // Skip minibuffer
        while (next != start && next == minibuf) {
            next = direction > 0 ? next->next : next->prev;
        }
        
        // If we looped back to start, no other buffers available
        if (next == start) {
            if (count == 1) {
                message("No next buffer");
            }
            return;
        }
        
        switch_to_buffer(next);
    }
}

void previous_buffer() {
    arg = -arg;
    next_buffer();
}


#include "lisp.h"

void reset_cursor_blink(Buffer *buffer) {
    bool blink_cursor_mode = scm_get_bool("blink-cursor-mode", true);

    if (blink_cursor_mode) {
        buffer->cursor.blink_count = 0;
        buffer->cursor.last_blink = getTime();
        buffer->cursor.visible = true;
    }
}

void adjust_all_window_points_after_modification(size_t pos, int delta) {
    // Collect all leaf windows
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    
    // Adjust point for each window viewing this buffer
    for (int i = 0; i < count; i++) {
        Window *win = leaves[i];
        if (win->buffer != current_buffer) continue; // Skip windows showing other buffers
        if (win == wm.selected) continue; // Skip selected window (already updated)
        
        if (delta > 0) {
            // Insertion: shift points after insertion position
            if (win->point > pos) {
                win->point += delta;
            }
        } else if (delta < 0) {
            // Deletion: adjust points in/after deleted region
            size_t abs_delta = -delta;
            size_t delete_end = pos + abs_delta;
            
            if (win->point >= delete_end) {
                win->point -= abs_delta;
            } else if (win->point > pos) {
                win->point = pos;
            }
        }
    }
}

inline void set_point(size_t new_pt) {
    current_buffer->pt = new_pt;
    wm.selected->point = new_pt;
}


inline void move_point(int delta) {
    size_t text_len = rope_char_length(current_buffer->rope);
    
    if (delta == 0) return;
    
    // Calculate new position (using signed arithmetic to detect overflow)
    ptrdiff_t new_pt;
    if (delta > 0) {
        new_pt = current_buffer->pt + delta;
    } else {
        new_pt = (ptrdiff_t)current_buffer->pt + delta; // delta is negative
    }
    
    // Check boundaries and show messages like Emacs does
    if (new_pt < 0) {
        set_point(0);
        message("Beginning of buffer");
        return;
    }
    
    if ((size_t)new_pt > text_len) {
        set_point(text_len);
        message("End of buffer");
        return;
    }
    
    set_point(new_pt);
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

        if (current_buffer->pt < current_buffer->region.mark) current_buffer->region.mark++;

        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt, utf8, len);
        adjust_all_window_points_after_modification(current_buffer->pt, 1);
        set_point(current_buffer->pt + 1);
        update_goal_column();
    }
}

size_t delete(size_t pos, size_t count) {
    if (count == 0) {
        // Deleting nothing - check if we're at a boundary
        size_t text_len = rope_char_length(current_buffer->rope);
        if (pos == 0) {
            message("Beginning of buffer");
        } else if (pos >= text_len) {
            message("End of buffer");
        }
        return pos;
    }
    
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Check for end of buffer
    if (pos >= text_len) {
        message("End of buffer");
        return pos;
    }
    
    // Clamp count to available characters
    if (pos + count > text_len) {
        count = text_len - pos;
    }
    
    // If clamping reduced count to 0, we hit end of buffer
    if (count == 0) {
        message("End of buffer");
        return pos;
    }
    
    // Update mark if region is active
    size_t delete_end = pos + count;
    if (current_buffer->region.mark >= delete_end) {
        current_buffer->region.mark -= count;
    } else if (current_buffer->region.mark > pos) {
        current_buffer->region.mark = pos;
    }
    
    current_buffer->rope = rope_delete_chars(current_buffer->rope, pos, count);
    adjust_all_window_points_after_modification(current_buffer->pt, -count);
    return pos;
}

bool is_pair(uint32_t left, uint32_t right) {
    return (left == '(' && right == ')') ||
           (left == '[' && right == ']') ||
           (left == '{' && right == '}') ||
           (left == '<' && right == '>') ||
           (left == '"' && right == '"') ||
           (left == '\'' && right == '\'') ||
           (left == '`' && right == '`');
}


void delete_backward_char() {
    if (arg == 0) return;
    
    if (current_buffer->region.active) {
        delete_region();
        return;
    }
    
    bool electric_pair_mode = scm_get_bool("electric-pair-mode", false);
    int count = abs(arg);
    
    if (arg > 0) {
        // Delete backward
        if ((size_t)count > current_buffer->pt) {
            count = current_buffer->pt;
        }
        
        // Check for pairs to delete
        if (electric_pair_mode) {
            size_t buffer_len = rope_char_length(current_buffer->rope);
            int pairs_deleted = 0;
            
            for (int i = 0; i < count; i++) {
                size_t pos = current_buffer->pt - i - 1 - pairs_deleted;
                
                // Check if we can look at both chars
                if (pos < buffer_len && pos + 1 < buffer_len) {
                    uint32_t char_before = rope_char_at(current_buffer->rope, pos);
                    uint32_t char_after = rope_char_at(current_buffer->rope, pos + 1);
                    
                    if (is_pair(char_before, char_after)) {
                        pairs_deleted++;
                    }
                }
            }
            
            // Delete the characters plus the paired closing ones
            delete(current_buffer->pt - count, count + pairs_deleted);
            set_point(current_buffer->pt - count);
        } else {
            delete(current_buffer->pt - count, count);
            set_point(current_buffer->pt - count);
        }
    } else {
        // Negative arg: delete forward
        delete(current_buffer->pt, count);
    }
}

void delete_char() {
    if (arg == 0) return;
    
    int count = abs(arg);
    if (arg > 0) {
        // Delete forward
        delete(current_buffer->pt, count);
    } else {
        // Delete backward
        if ((size_t)count > current_buffer->pt) {
            count = current_buffer->pt;
        }
        delete(current_buffer->pt - count, count);
        set_point(current_buffer->pt - count);
    }
}

void message(const char *format, ...) {
    va_list args;
    va_start(args, format);
    
    // Determine the required buffer size
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy) + 1;
    va_end(args_copy);
    
    if (needed <= 0) {
        va_end(args);
        return;
    }
    
    // Allocate and format the message
    char *formatted = malloc(needed);
    if (!formatted) {
        va_end(args);
        return;
    }
    
    vsnprintf(formatted, needed, format, args);
    va_end(args);
    
    // Clear and update minibuffer
    Buffer *minibuf = wm.minibuffer_window->buffer;
    size_t len = rope_char_length(minibuf->rope);
    if (len > 0) {
        minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
    }
    
    size_t msg_len = strlen(formatted);
    minibuf->rope = rope_insert_chars(minibuf->rope, 0, formatted, msg_len);
    wm.minibuffer_window->point = 0;
    
    // TODO Log to a *Messages* buffer
    
    free(formatted);
}

void newline() {
    if (arg == 0) return;
    
    if (arg < 0) {
        message("Repetition argument has to be non-negative");
        return;
    }
    
    for (int i = 0; i < arg; i++) {
        insert('\n');
    }
}

void open_line() {
    size_t start_pt = current_buffer->pt;
    newline();
    set_point(start_pt);
}

void split_line() {
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Skip forward over spaces and tabs
    while (current_buffer->pt < text_len) {
        uint32_t ch = rope_char_at(current_buffer->rope, current_buffer->pt);
        if (ch != ' ' && ch != '\t') break;
        set_point(current_buffer->pt + 1);
    }
    
    size_t col = current_column();
    size_t pos = current_buffer->pt;
    
    insert('\n');
    
    for (size_t i = 0; i < col; i++) {
        insert(' ');
    }
    
    // Go back
    set_point(pos);
}

void forward_char() {
    if (arg == 0) return;
    move_point(arg);
}

void backward_char() {
    if (arg == 0) return;
    move_point(-arg);
}

size_t line_beginning_position() {
    size_t pos = current_buffer->pt;
    
    // Scan backwards to find newline or start of buffer
    while (pos > 0) {
        uint32_t ch = rope_char_at(current_buffer->rope, pos - 1);
        if (ch == '\n') break;
        pos--;
    }
    
    return pos;
}

size_t line_end_position() {
    size_t text_len = rope_char_length(current_buffer->rope); // O(1) - uses cached value!
    size_t pos = current_buffer->pt;
    
    while (pos < text_len) {
        uint32_t ch = rope_char_at(current_buffer->rope, pos);
        if (ch == '\n') break;
        pos++;
    }
    
    return pos;
}

size_t current_column() {
    size_t line_start = line_beginning_position();
    return current_buffer->pt - line_start;
}

void update_goal_column() {
    current_buffer->cursor.goal_column = current_column();
}

void next_line() {
    if (arg == 0) return;
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t starting_pt = current_buffer->pt;  // Remember where we started
    
    for (int i = 0; i < count; i++) {
        if (direction > 0) {
            // Move down
            size_t pos = current_buffer->pt;
            while (pos < text_len && rope_char_at(current_buffer->rope, pos) != '\n') {
                pos++;
            }
            
            if (pos >= text_len) {
                // Can't move any further - we're at the last line
                if (current_buffer->pt == starting_pt) {
                    // We didn't move at all
                    message("End of buffer");
                }
                break; // At end of buffer
            }
            
            pos++; // Move past newline
            
            size_t line_start = pos;
            size_t line_end = pos;
            while (line_end < text_len && rope_char_at(current_buffer->rope, line_end) != '\n') {
                line_end++;
            }
            
            size_t line_length = line_end - line_start;
            if (current_buffer->cursor.goal_column <= line_length) {
                set_point(line_start + current_buffer->cursor.goal_column);
            } else {
                set_point(line_end);
            }
        } else {
            // Move up
            size_t pos = current_buffer->pt;
            while (pos > 0 && rope_char_at(current_buffer->rope, pos - 1) != '\n') {
                pos--;
            }
            
            if (pos == 0) {
                // Can't move any further - we're at the first line
                if (current_buffer->pt == starting_pt) {
                    // We didn't move at all
                    message("Beginning of buffer");
                }
                break; // At first line
            }
            
            pos--; // Move before newline
            
            size_t line_start = pos;
            while (line_start > 0 && rope_char_at(current_buffer->rope, line_start - 1) != '\n') {
                line_start--;
            }
            
            size_t line_length = pos - line_start;
            if (current_buffer->cursor.goal_column <= line_length) {
                set_point(line_start + current_buffer->cursor.goal_column);
            } else {
                set_point(pos);
            }
        }
    }
}

void previous_line() {
    arg = -arg;
    next_line();
}

void end_of_line() {
    set_point(line_end_position());
}

void beginning_of_line() {
    set_point(line_beginning_position());
}

void beginning_of_buffer() {
    set_point(0);
}

void end_of_buffer() {
    size_t text_len = rope_char_length(current_buffer->rope); // O(1) cached!
    set_point(text_len);
    
}

/// REGION

bool transient_mark_mode = false;

void set_mark_command() {
    current_buffer->region.mark = current_buffer->pt;
    if (transient_mark_mode) {
        current_buffer->region.active = true;
    } else {
        if (is_scm_proc(last_command, "set-mark-command")) {
            current_buffer->region.active = true;
        }
        /* if (last_command == set_mark_command) */
        /*     current_buffer->region.active = true; */
    }
}

void exchange_point_and_mark() {
    size_t temp = current_buffer->pt;
    set_point(current_buffer->region.mark);
    current_buffer->region.mark = temp;
}

// Get the bounds of the region (always returns min, max order)
void region_bounds(size_t *start, size_t *end) {
    if (current_buffer->region.mark < current_buffer->pt) {
        *start = current_buffer->region.mark;
        *end = current_buffer->pt;
    } else {
        *start = current_buffer->pt;
        *end = current_buffer->region.mark;
    }
}

void delete_region() {
    size_t start, end;
    region_bounds(&start, &end);
    
    // Nothing to delete if start == end
    if (start == end) {
        current_buffer->region.active = false;
        return;
    }
    
    size_t length = end - start;
    delete(start, length);
    set_point(start);
    current_buffer->region.active = false;
}

void rkill(size_t start, size_t end, bool prepend) {
    if (start >= end) return;
    
    size_t length = end - start;
    size_t buffer_size = length * 4 + 1;
    char *new_text = (char *)malloc(buffer_size);
    
    if (!new_text) return;
    
    size_t copied = rope_copy_chars(current_buffer->rope, start, length, new_text, buffer_size - 1);
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
    size_t text_len = rope_char_length(current_buffer->rope);
    
    if (arg == 0) {
        // Kill backwards from point to beginning of line
        size_t line_start = line_beginning_position();
        size_t start = line_start;
        size_t end = current_buffer->pt;
        
        if (start < end) {
            rkill(start, end, true);  // Prepend to kill ring
            set_point(start);
        }
        return;
    }
    
    if (arg == 1) {
        // No prefix arg: default behavior
        size_t line_end = line_end_position();
        size_t start = current_buffer->pt;
        size_t end;
        
        // Check if we're at end of buffer
        if (current_buffer->pt >= text_len) {
            return;
        }
        
        // Check if rest of line is empty (only whitespace or already at end)
        bool rest_is_empty = (current_buffer->pt == line_end);
        if (!rest_is_empty) {
            rest_is_empty = true;
            for (size_t i = current_buffer->pt; i < line_end; i++) {
                uint32_t ch = rope_char_at(current_buffer->rope, i);
                if (ch != ' ' && ch != '\t') {
                    rest_is_empty = false;
                    break;
                }
            }
        }
        
        bool kill_whole_line = scm_get_bool("kill-whole-line", false);

        // Special case: kill_whole_line and at beginning of line
        if (kill_whole_line && current_buffer->pt == line_beginning_position()) {
            end = line_end;
            if (end < text_len && rope_char_at(current_buffer->rope, end) == '\n') {
                end++;
            }
        }
        // If rest of line is empty/whitespace, kill through newline
        else if (rest_is_empty) {
            end = line_end;
            if (end < text_len && rope_char_at(current_buffer->rope, end) == '\n') {
                end++;
            }
        }
        // Otherwise just kill to end of line (not including newline)
        else {
            end = line_end;
        }
        
        if (start < end) {
            rkill(start, end, false);
        }
        return;
    }
    
    if (arg > 1) {
        // Positive arg > 1: kill arg lines forward (always including newlines)
        // This is equivalent to forward-line arg times
        size_t start = current_buffer->pt;
        size_t pos = start;
        
        for (int i = 0; i < arg; i++) {
            // Move to end of current line
            while (pos < text_len && rope_char_at(current_buffer->rope, pos) != '\n') {
                pos++;
            }
            
            // Include the newline
            if (pos < text_len && rope_char_at(current_buffer->rope, pos) == '\n') {
                pos++;
            }
            
            // If we're at end of buffer, stop
            if (pos >= text_len) {
                break;
            }
        }
        
        if (start < pos) {
            rkill(start, pos, false);  // Append to kill ring
        }
        return;
    }
    
    // arg < 0: Kill backwards arg lines
    // Move backwards |arg| lines
    size_t end = current_buffer->pt;
    size_t pos = end;
    
    for (int i = 0; i < -arg; i++) {
        // Move to beginning of current line
        while (pos > 0 && rope_char_at(current_buffer->rope, pos - 1) != '\n') {
            pos--;
        }
        
        // Move to previous line (past the newline)
        if (pos > 0) {
            pos--;
        }
    }
    
    // Now pos is at the beginning of the line we want to start killing from
    // We need to move back to the beginning of that line
    while (pos > 0 && rope_char_at(current_buffer->rope, pos - 1) != '\n') {
        pos--;
    }
    
    if (pos < end) {
        rkill(pos, end, true);  // Prepend to kill ring
        set_point(pos);
    }
}

void kill_region() {
    size_t start, end;
    region_bounds(&start, &end);
    
    if (start == end) {
        current_buffer->region.active = false;
        return;
    }
    
    rkill(start, end, false);
    set_point(start);
    current_buffer->region.active = false;
}

void yank() {
    const char *clipboard_text = getClipboardString();
    
    if (clipboard_text && *clipboard_text) {
        size_t len = strlen(clipboard_text);
        size_t old_char_len = rope_char_length(current_buffer->rope);
        

        current_buffer->region.mark = current_buffer->pt;
        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt, clipboard_text, len);
        
        size_t new_char_len = rope_char_length(current_buffer->rope);
        size_t chars_inserted = new_char_len - old_char_len;
        set_point(current_buffer->pt + chars_inserted); // End of inserted text
        
        // If C-u was used (raw prefix arg), exchange point and mark
        // This leaves point at beginning and mark at end of yanked text
        if (raw_prefix_arg) exchange_point_and_mark();

        adjust_all_window_points_after_modification(current_buffer->pt, chars_inserted);
    }
}

/// WORD

bool isWordChar(uint32_t c) {
    if (c >= 128) return false;
    return isalnum((unsigned char)c);
}

bool isPunctuationChar(uint32_t c) {
    if (c >= 128) return false;
    return strchr(",.;:!?'\"(){}[]<>-+*/=&|^%$#@~_", (char)c) != NULL;
}

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

// TODO Use Rope iterator here too
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
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    while (count-- > 0) {
        if (direction > 0) {
            set_point(end_of_word(current_buffer, current_buffer->pt));
        } else {
            set_point(beginning_of_word(current_buffer, current_buffer->pt));
        }
    }
}

void backward_word() {
    arg = -arg; 
    forward_word();
}

void kill_word() {
    if (arg == 0) return;
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t start = current_buffer->pt;
    size_t end = start;
    
    for (int i = 0; i < count; i++) {
        if (direction > 0) {
            end = end_of_word(current_buffer, end);
        } else {
            end = beginning_of_word(current_buffer, end);
        }
    }
    
    if (start == end) return;
    
    if (start < end) {
        rkill(start, end, false);
    } else {
        rkill(end, start, true);
        set_point(end);
    }
}

void backward_kill_word() {
    arg = -arg;
    kill_word();
}

/// PARAGRAPHS

void forward_paragraph() {
    if (arg == 0) return;
    
    size_t text_len = rope_char_length(current_buffer->rope);
    int count = abs(arg);
    
    if (arg > 0) {
        for (int i = 0; i < count; i++) {
            if (current_buffer->pt >= text_len) break;
            
            size_t pos = current_buffer->pt;
            bool found_text = false;
            
            while (pos < text_len) {
                if (rope_char_at(current_buffer->rope, pos) == '\n') {
                    size_t next_line_start = pos + 1;
                    if (next_line_start < text_len && rope_char_at(current_buffer->rope, next_line_start) == '\n') {
                        if (found_text) {
                            set_point(next_line_start);
                            break;
                        }
                    } else if (next_line_start < text_len) {
                        found_text = true;
                    }
                }
                pos++;
            }
            
            if (pos >= text_len) {
                set_point(text_len);
                break;
            }
        }
    } else {
        for (int i = 0; i < count; i++) {
            if (current_buffer->pt == 0) break;
            
            size_t pos = current_buffer->pt - 1;
            bool found_text = false;
            
            while (pos > 0) {
                if (rope_char_at(current_buffer->rope, pos) == '\n' && rope_char_at(current_buffer->rope, pos - 1) == '\n') {
                    if (found_text) {
                        set_point(pos);
                        break;
                    }
                } else if (rope_char_at(current_buffer->rope, pos) != '\n') {
                    found_text = true;
                }
                pos--;
            }
            
            if (pos == 0) {
                set_point(0);
                break;
            }
        }
    }
}

void backward_paragraph() {
    arg = -arg;
    forward_paragraph();
}


/// ARG

bool raw_prefix_arg = false;

// TODO Activate universal-argument-map and bind 0..9 to digit-argument
void universal_argument() {
    arg *= 4;
    raw_prefix_arg = true;  
}

void digit_argument() {
    // NOTE This function is called, but the actual digit handling
    // happens in after_keychord_hook by parsing the notation
}

void negative_argument() {
    arg = -arg;
    char msg[32];
    snprintf(msg, sizeof(msg), "C-u %d", arg);
    message(msg);
}

void execute_extended_command() {
    activate_minibuffer();
}

void keyboard_quit() {
    current_buffer->region.active = false;
    if (wm.selected == wm.minibuffer_window) {
        deactivate_minibuffer();
    }
}

void draw_cursor(Buffer *buffer, Window *win, float start_x, float start_y) {
    float x = start_x;
    float y = start_y;
    float line_height = buffer->font->ascent + buffer->font->descent;
    
    // Calculate usable width (accounting for right fringe)
    float max_x = start_x + (win->width - 2 * fringe_width);
    
    size_t text_len = rope_char_length(buffer->rope);
    int lineCount = 0;
    
    // Get point from window, not buffer
    size_t point = win ? win->point : buffer->pt;
    
    // Use iterator to calculate cursor position
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    while (i < point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            lineCount++;
            x = start_x;
        } else {
            // Check if we need to wrap before advancing
            float char_width = character_width(buffer->font, ch);
            if (x + char_width > max_x) {
                // Wrap to next line
                x = start_x;
                lineCount++;
            }
            
            // Get character width from font (handles all Unicode)
            Character *char_info = font_get_character(buffer->font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    y = start_y - lineCount * line_height - (buffer->font->descent * 2);
    
    buffer->cursor.x = x;
    buffer->cursor.y = y;
    
    // Determine cursor width - check character at cursor position
    float cursor_width;
    Character *space = font_get_character(buffer->font, ' ');
    float space_width = space ? space->ax : buffer->font->ascent;
    
    if (point < text_len) {
        uint32_t ch_at_cursor = rope_char_at(buffer->rope, point);
        if (ch_at_cursor == '\n') {
            cursor_width = space_width;
        } else {
            Character *char_info = font_get_character(buffer->font, ch_at_cursor);
            cursor_width = char_info ? char_info->ax : space_width;
        }
    } else {
        // Cursor is at end of buffer - use space width
        cursor_width = space_width;
    }
    
    float cursor_height = buffer->font->ascent + buffer->font->descent;
    
    // Check if this window is selected
    bool is_selected = win && win->is_selected;
    
    // Don't draw cursor in inactive minibuffer
    if (win && win->is_minibuffer && !wm.minibuffer_active) {
        return;
    }
    
    if (is_selected) {
        bool blink_cursor_mode = scm_get_bool("blink-cursor-mode", true);        
        size_t blink_cursor_blinks = scm_get_size_t("blink-cursor-blinks", 0);
        float blink_cursor_interval = scm_get_float("blink-cursor-interval", 0.1);
        float blink_cursor_delay = scm_get_float("blink-cursor-delay", 0.1);

        // Selected window: filled cursor with blinking
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
                // Draw filled cursor
                quad2D((vec2){buffer->cursor.x, buffer->cursor.y},
                       (vec2){cursor_width, cursor_height}, CT.cursor);
            }
        } else {
            // Always draw filled cursor when blink limit reached or mode disabled
            quad2D((vec2){buffer->cursor.x, buffer->cursor.y},
                   (vec2){cursor_width, cursor_height}, CT.cursor);
        }
    } else {
        // Non-selected window: hollow cursor (border only), no blinking
        float border_width = 1.0f;
        
        // Top border
        quad2D((vec2){buffer->cursor.x, buffer->cursor.y + cursor_height - border_width},
               (vec2){cursor_width, border_width}, CT.cursor);
        
        // Bottom border
        quad2D((vec2){buffer->cursor.x, buffer->cursor.y},
               (vec2){cursor_width, border_width}, CT.cursor);
        
        // Left border
        quad2D((vec2){buffer->cursor.x, buffer->cursor.y},
               (vec2){border_width, cursor_height}, CT.cursor);
        
        // Right border
        quad2D((vec2){buffer->cursor.x + cursor_width - border_width, buffer->cursor.y},
               (vec2){border_width, cursor_height}, CT.cursor);
    }
}

void draw_mark(Window *win, float start_x, float start_y) {
    Buffer *buffer = win->buffer;
    
    // Only draw mark in the selected window
    if (!win->is_selected) return;
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
        } else {
            Character *char_info = font_get_character(buffer->font, ch);
            if (char_info) {
                mark_x += char_info->ax;
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    mark_y = start_y - lineCount * line_height - (buffer->font->descent * 2);
    
    // Determine mark width
    float mark_width;
    Character *space = font_get_character(buffer->font, ' ');
    float space_width = space ? space->ax : buffer->font->ascent;
    
    if (buffer->region.mark < text_len) {
        uint32_t ch_at_mark = rope_char_at(buffer->rope, buffer->region.mark);
        if (ch_at_mark == '\n') {
            mark_width = space_width;
        } else {
            Character *char_info = font_get_character(buffer->font, ch_at_mark);
            mark_width = char_info ? char_info->ax : space_width;
        }
    } else {
        // Mark is at end of buffer - use space width
        mark_width = space_width;
    }
    
    float mark_height = buffer->font->ascent + buffer->font->descent;
    
    quad2D((vec2){mark_x, mark_y}, (vec2){mark_width, mark_height}, CT.function);
}

// Helper function to find the character position at a given scroll offset
static size_t find_start_position(Buffer *buffer, Window *win, float *out_start_y) {
    if (!win || win->is_minibuffer) {
        *out_start_y = 0;
        return 0;
    }
    
    float line_height = buffer->font->ascent + buffer->font->descent;
    float max_x = win->width - 2 * fringe_width;
    
    // Calculate how many lines are scrolled off the top
    // Add a buffer of a few lines to avoid visual glitches
    float lines_scrolled = (win->scrolly / line_height);
    int skip_lines = (int)lines_scrolled - 2;  // Start 2 lines earlier for safety
    if (skip_lines < 0) skip_lines = 0;
    
    // Find the character position at skip_lines
    size_t pos = 0;
    int current_line = 0;
    float x = 0;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    while (rope_iter_next_char(&iter, &ch) && current_line < skip_lines) {
        if (ch == '\n') {
            current_line++;
            x = 0;
        } else {
            float char_width = character_width(buffer->font, ch);
            if (x + char_width > max_x) {
                current_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(buffer->font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        pos++;
    }
    
    rope_iter_destroy(&iter);
    
    *out_start_y = current_line * line_height;
    return pos;
}

void draw_buffer(Buffer *buffer, Window *win, float start_x, float start_y) {
    float line_height = buffer->font->ascent + buffer->font->descent;
    
    // Calculate usable width (accounting for right fringe)
    float max_x = start_x + (win->width - 2 * fringe_width);
    
    // Calculate clipping region (don't draw outside window)
    float window_bottom = win->y;
    float window_top = win->y + win->height;
    
    // Account for modeline (but minibuffer doesn't have one)
    if (!win->is_minibuffer) {
        window_bottom += line_height;
    }
    
    // Get point from window if provided
    size_t point = win ? win->point : buffer->pt;
    
    // Check if this window is selected
    bool is_selected = win && win->is_selected;
    
    // Find where to start drawing based on scroll position
    float scroll_offset_y = 0;
    size_t start_pos = 0;
    
    if (win && !win->is_minibuffer) {
        start_pos = find_start_position(buffer, win, &scroll_offset_y);
    }
    
    // Initialize rendering position
    float x = start_x;
    float y = start_y + (win && !win->is_minibuffer ? win->scrolly : 0) - scroll_offset_y;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, start_pos);
    
    uint32_t ch;
    size_t i = start_pos;

    bool visible_mark_mode = scm_get_bool("visible-mark-mode", false);
    
    while (rope_iter_next_char(&iter, &ch)) {
        // Early exit if we've scrolled past the bottom of the window
        if (y < window_bottom - line_height) {
            break;
        }
        
        if (ch == '\n') {
            x = start_x;
            y -= line_height;
        } else {
            // Check if we need to wrap before drawing this character
            float char_width = character_width(buffer->font, ch);
            if (x + char_width > max_x) {
                // Wrap to next line
                x = start_x;
                y -= line_height;
                
                // Early exit if wrapped past bottom
                if (y < window_bottom - line_height) {
                    break;
                }
            }
            
            // Only draw if within visible region
            if (y >= window_bottom && y <= window_top) {
                
                // Determine color based on cursor/mark position
                Color char_color = CT.text; // Default color
                
                // Mark gets CT.bg (non-blinking) when visible-mark-mode is on and mark != point
                if (i == buffer->region.mark && is_selected && visible_mark_mode && 
                    buffer->region.mark != point) {
                    char_color = CT.bg;
                }
                // Cursor position gets CT.bg but only when visible (blinking)
                else if (i == point && is_selected && buffer->cursor.visible) {
                    char_color = CT.bg;
                }
                
                
                // Render the character
                float advance = character(buffer->font, ch, x, y, char_color);
                x += advance;
            } else if (y > window_top) {
                // NOTE Removing this changes nothing and gives a HUGE FPS boost
                /* // Still need to advance x even if not drawing (we're above viewport) */
                /* Character *char_info = font_get_character(buffer->font, ch); */
                /* if (char_info) { */
                /*     x += char_info->ax; */
                /* } */
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    // Calculate the scroll offset to pass to draw_mark and draw_cursor
    float scroll_offset = (win && !win->is_minibuffer) ? win->scrolly : 0;
    
    if (visible_mark_mode) draw_mark(win, start_x, start_y + scroll_offset);
    draw_cursor(buffer, win, start_x, start_y + scroll_offset);
}


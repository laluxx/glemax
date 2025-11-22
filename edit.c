#include "edit.h"
#include "buffer.h"
#include "lisp.h"
#include <ctype.h>

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
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
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

void newline() {
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
    if (arg == 0) return;
    move_point(arg);
}

void backward_char() {
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    next_line();
}

// TODO support ARG
void end_of_line() {
    set_point(line_end_position());
}

// TODO support ARG
void beginning_of_line() {
    set_point(line_beginning_position());
}

// TODO Support ARG
void beginning_of_buffer() {
    set_point(0);
}

// TODO Support ARG
void end_of_buffer() {
    size_t text_len = rope_char_length(current_buffer->rope); // O(1) cached!
    set_point(text_len);
    
}

// Helper function to check if a line is blank (only whitespace or empty)
static bool is_line_blank(Buffer *buffer, size_t line_start, size_t line_end) {
    for (size_t i = line_start; i < line_end; i++) {
        uint32_t ch = rope_char_at(buffer->rope, i);
        if (ch != ' ' && ch != '\t') {
            return false;
        }
    }
    return true;
}

void delete_blank_lines() {
    size_t text_len = rope_char_length(current_buffer->rope);
    if (text_len == 0) return;
    
    size_t line_start = line_beginning_position();
    size_t line_end = line_end_position();
    bool current_blank = is_line_blank(current_buffer, line_start, line_end);
    
    // Case 1: On nonblank line - delete following blank lines
    if (!current_blank) {
        size_t pos = line_end;
        if (pos < text_len && rope_char_at(current_buffer->rope, pos) == '\n') pos++;
        
        size_t delete_start = pos;
        while (pos < text_len) {
            size_t next_start = pos;
            size_t next_end = pos;
            while (next_end < text_len && rope_char_at(current_buffer->rope, next_end) != '\n') {
                next_end++;
            }
            
            if (!is_line_blank(current_buffer, next_start, next_end)) break;
            
            pos = next_end;
            if (pos < text_len && rope_char_at(current_buffer->rope, pos) == '\n') pos++;
        }
        
        if (pos > delete_start) {
            delete(delete_start, pos - delete_start);
        }
        return;
    }
    
    // Current line is blank - find the blank region boundaries
    size_t region_start = line_start;
    size_t pos = line_start;
    
    // Scan upward for blank lines
    while (pos > 0) {
        size_t prev_end = pos - 1;
        size_t prev_start = prev_end;
        while (prev_start > 0 && rope_char_at(current_buffer->rope, prev_start - 1) != '\n') {
            prev_start--;
        }
        
        if (!is_line_blank(current_buffer, prev_start, prev_end)) break;
        
        region_start = prev_start;
        pos = prev_start;
    }
    
    // Scan downward for blank lines
    pos = line_end;
    if (pos < text_len && rope_char_at(current_buffer->rope, pos) == '\n') pos++;
    
    size_t region_end = pos;
    while (pos < text_len) {
        size_t next_start = pos;
        size_t next_end = pos;
        while (next_end < text_len && rope_char_at(current_buffer->rope, next_end) != '\n') {
            next_end++;
        }
        
        if (!is_line_blank(current_buffer, next_start, next_end)) break;
        
        region_end = next_end;
        if (region_end < text_len && rope_char_at(current_buffer->rope, region_end) == '\n') {
            region_end++;
        }
        pos = region_end;
    }
    
    // Check if isolated (no blank lines before or after)
    bool isolated = (region_start == line_start && region_end == line_end + 
                     (line_end < text_len && rope_char_at(current_buffer->rope, line_end) == '\n' ? 1 : 0));
    
    if (isolated) {
        // Case 2: Isolated blank line - delete it
        size_t delete_end = line_end;
        if (delete_end < text_len && rope_char_at(current_buffer->rope, delete_end) == '\n') {
            delete_end++;
        }
        delete(line_start, delete_end - line_start);
        set_point(line_start);
    } else {
        // Case 3: Surrounded by blank lines - delete all, keep one
        delete(region_start, region_end - region_start);
        set_point(region_start);
        insert('\n');
        set_point(region_start);
    }
}

void back_to_indentation() {
    beginning_of_line();
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Skip forward over spaces and tabs
    while (current_buffer->pt < text_len) {
        uint32_t ch = rope_char_at(current_buffer->rope, current_buffer->pt);
        if (ch != ' ' && ch != '\t') {
            break;
        }
        set_point(current_buffer->pt + 1);
    }
    
    update_goal_column();
}

void delete_indentation() {
    int arg = get_prefix_arg();
    bool has_prefix = (arg != 1); // Check if user provided explicit prefix
    
    // Check if region is active and no prefix arg was given
    if (current_buffer->region.active && !has_prefix) {
        // Join all lines in region
        size_t start, end;
        region_bounds(&start, &end);
        
        if (start == end) {
            current_buffer->region.active = false;
            return;
        }
        
        // Move to start of region
        set_point(start);
        
        // Find the start of the first line in region
        size_t first_line_start = line_beginning_position();
        
        // Find the end of the last line in region
        size_t saved_pt = current_buffer->pt;
        set_point(end);
        size_t last_line_end = line_end_position();
        set_point(saved_pt);
        
        // Move to beginning of first line
        set_point(first_line_start);
        
        // Keep joining lines until we've covered the region
        while (current_buffer->pt < last_line_end) {
            size_t line_end = line_end_position();
            size_t text_len = rope_char_length(current_buffer->rope);
            
            // If at end of buffer, stop
            if (line_end >= text_len) break;
            
            // Check if there's a newline after this line
            if (rope_char_at(current_buffer->rope, line_end) != '\n') break;
            
            // Move to end of line
            set_point(line_end);
            
            // Perform the join (same as single line join below)
            size_t join_pos = current_buffer->pt;
            
            // Delete the newline
            if (join_pos < text_len && rope_char_at(current_buffer->rope, join_pos) == '\n') {
                delete(join_pos, 1);
                text_len--;
                last_line_end--;  // Adjust end position
            }
            
            // Delete whitespace at beginning of next line
            while (join_pos < text_len) {
                uint32_t ch = rope_char_at(current_buffer->rope, join_pos);
                if (ch != ' ' && ch != '\t') break;
                delete(join_pos, 1);
                text_len--;
                last_line_end--;  // Adjust end position
            }
            
            // Delete whitespace before the join point
            while (join_pos > 0) {
                uint32_t ch = rope_char_at(current_buffer->rope, join_pos - 1);
                if (ch != ' ' && ch != '\t') break;
                join_pos--;
                delete(join_pos, 1);
                text_len--;
                last_line_end--;  // Adjust end position
                set_point(join_pos);
            }
            
            // Insert a single space (unless we're at beginning of buffer or after opening paren)
            bool need_space = true;
            if (current_buffer->pt == 0) {
                need_space = false;
            } else if (current_buffer->pt > 0) {
                uint32_t prev_ch = rope_char_at(current_buffer->rope, current_buffer->pt - 1);
                // Don't add space after opening parens/brackets
                if (prev_ch == '(' || prev_ch == '[' || prev_ch == '{') {
                    need_space = false;
                }
            }
            
            // Check if next char is closing paren
            if (need_space && current_buffer->pt < text_len) {
                uint32_t next_ch = rope_char_at(current_buffer->rope, current_buffer->pt);
                if (next_ch == ')' || next_ch == ']' || next_ch == '}') {
                    need_space = false;
                }
            }
            
            if (need_space) {
                insert(' ');
                last_line_end++;  // Adjust end position
            }
        }
        
        current_buffer->region.active = false;
        return;
    }
    
    // Single line join (with or without prefix arg)
    size_t next_line_was_empty = false;
    
    if (has_prefix && arg > 0) {
        // With prefix arg: join current line to FOLLOWING line
        size_t line_end = line_end_position();
        size_t text_len = rope_char_length(current_buffer->rope);
        
        // Check if we're at last line
        if (line_end >= text_len) {
            return;
        }
        
        // Check if there's a newline
        if (rope_char_at(current_buffer->rope, line_end) != '\n') {
            return;
        }
        
        // Check if next line is empty (only whitespace)
        size_t check_pos = line_end + 1;
        next_line_was_empty = true;
        while (check_pos < text_len) {
            uint32_t ch = rope_char_at(current_buffer->rope, check_pos);
            if (ch == '\n') break;
            if (ch != ' ' && ch != '\t') {
                next_line_was_empty = false;
                break;
            }
            check_pos++;
        }
        
        // Move to end of line
        set_point(line_end);
    } else {
        // No prefix arg: join current line to PREVIOUS line
        size_t line_start = line_beginning_position();
        
        // Check if we're at first line
        if (line_start == 0) {
            return;
        }
        
        // Check if current line is empty (only whitespace)
        size_t line_end = line_end_position();
        next_line_was_empty = true;
        for (size_t check_pos = line_start; check_pos < line_end; check_pos++) {
            uint32_t ch = rope_char_at(current_buffer->rope, check_pos);
            if (ch != ' ' && ch != '\t') {
                next_line_was_empty = false;
                break;
            }
        }
        
        // Move to end of previous line (before the newline)
        set_point(line_start - 1);
    }
    
    size_t join_pos = current_buffer->pt;
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Delete the newline at join point
    if (join_pos < text_len && rope_char_at(current_buffer->rope, join_pos) == '\n') {
        delete(join_pos, 1);
        text_len--;
    }
    
    // Delete whitespace at beginning of next line (what was after newline)
    while (join_pos < text_len) {
        uint32_t ch = rope_char_at(current_buffer->rope, join_pos);
        if (ch != ' ' && ch != '\t') break;
        delete(join_pos, 1);
        text_len--;
    }
    
    // Delete trailing whitespace from previous line (before join point)
    while (join_pos > 0) {
        uint32_t ch = rope_char_at(current_buffer->rope, join_pos - 1);
        if (ch != ' ' && ch != '\t') break;
        join_pos--;
        delete(join_pos, 1);
        text_len--;
        set_point(join_pos);
    }
    
    // Insert exactly one space at the join point (unless at buffer start or special cases)
    bool need_space = true;
    
    // Don't add space if the line we joined was empty (only whitespace)
    if (next_line_was_empty) {
        need_space = false;
    } else if (current_buffer->pt == 0) {
        need_space = false;
    } else if (current_buffer->pt > 0) {
        uint32_t prev_ch = rope_char_at(current_buffer->rope, current_buffer->pt - 1);
        // Don't add space after opening parens/brackets
        if (prev_ch == '(' || prev_ch == '[' || prev_ch == '{') {
            need_space = false;
        }
    }
    
    // Check if next char is closing paren
    if (need_space && current_buffer->pt < text_len) {
        uint32_t next_ch = rope_char_at(current_buffer->rope, current_buffer->pt);
        if (next_ch == ')' || next_ch == ']' || next_ch == '}') {
            need_space = false;
        }
    }
    
    if (need_space) {
        insert(' ');
    }
}


/* void back_to_indentation() { */
/*     size_t line_start = line_beginning_position(); */
/*     size_t text_len = rope_char_length(current_buffer->rope); */
/*     size_t pos = line_start; */
    
/*     // Skip forward over spaces and tabs */
/*     while (pos < text_len) { */
/*         uint32_t ch = rope_char_at(current_buffer->rope, pos); */
/*         if (ch == '\n') { */
/*             // Line is all whitespace, stay at line_start */
/*             break; */
/*         } */
/*         if (ch != ' ' && ch != '\t') { */
/*             // Found first non-whitespace */
/*             break; */
/*         } */
/*         pos++; */
/*     } */
    
/*     set_point(pos); */
/*     update_goal_column(); */
/* } */

/* void delete_indentation() { */
/*     int arg = get_prefix_arg(); */
/*     bool has_prefix = (arg != 1); // Check if user provided explicit prefix */
    
/*     // Check if region is active and no prefix arg was given */
/*     if (current_buffer->region.active && !has_prefix) { */
/*         // Join all lines in region */
/*         size_t start, end; */
/*         region_bounds(&start, &end); */
        
/*         if (start == end) { */
/*             current_buffer->region.active = false; */
/*             return; */
/*         } */
        
/*         // Move to start of region */
/*         set_point(start); */
        
/*         // Find the start of the first line in region */
/*         size_t first_line_start = line_beginning_position(); */
        
/*         // Find the end of the last line in region */
/*         size_t saved_pt = current_buffer->pt; */
/*         set_point(end); */
/*         size_t last_line_end = line_end_position(); */
/*         set_point(saved_pt); */
        
/*         // Move to beginning of first line */
/*         set_point(first_line_start); */
        
/*         // Keep joining lines until we've covered the region */
/*         while (current_buffer->pt < last_line_end) { */
/*             size_t line_end = line_end_position(); */
/*             size_t text_len = rope_char_length(current_buffer->rope); */
            
/*             // If at end of buffer, stop */
/*             if (line_end >= text_len) break; */
            
/*             // Check if there's a newline after this line */
/*             if (rope_char_at(current_buffer->rope, line_end) != '\n') break; */
            
/*             // Move to end of line */
/*             set_point(line_end); */
            
/*             // Perform the join (same as single line join below) */
/*             size_t join_pos = current_buffer->pt; */
            
/*             // Delete the newline */
/*             if (join_pos < text_len && rope_char_at(current_buffer->rope, join_pos) == '\n') { */
/*                 delete(join_pos, 1); */
/*                 text_len--; */
/*                 last_line_end--;  // Adjust end position */
/*             } */
            
/*             // Delete whitespace at beginning of next line */
/*             while (join_pos < text_len) { */
/*                 uint32_t ch = rope_char_at(current_buffer->rope, join_pos); */
/*                 if (ch != ' ' && ch != '\t') break; */
/*                 delete(join_pos, 1); */
/*                 text_len--; */
/*                 last_line_end--;  // Adjust end position */
/*             } */
            
/*             // Delete whitespace before the join point */
/*             while (join_pos > 0) { */
/*                 uint32_t ch = rope_char_at(current_buffer->rope, join_pos - 1); */
/*                 if (ch != ' ' && ch != '\t') break; */
/*                 join_pos--; */
/*                 delete(join_pos, 1); */
/*                 text_len--; */
/*                 last_line_end--;  // Adjust end position */
/*                 set_point(join_pos); */
/*             } */
            
/*             // Insert a single space (unless we're at beginning of buffer or after opening paren) */
/*             bool need_space = true; */
/*             if (current_buffer->pt == 0) { */
/*                 need_space = false; */
/*             } else if (current_buffer->pt > 0) { */
/*                 uint32_t prev_ch = rope_char_at(current_buffer->rope, current_buffer->pt - 1); */
/*                 // Don't add space after opening parens/brackets */
/*                 if (prev_ch == '(' || prev_ch == '[' || prev_ch == '{') { */
/*                     need_space = false; */
/*                 } */
/*             } */
            
/*             // Check if next char is closing paren */
/*             if (need_space && current_buffer->pt < text_len) { */
/*                 uint32_t next_ch = rope_char_at(current_buffer->rope, current_buffer->pt); */
/*                 if (next_ch == ')' || next_ch == ']' || next_ch == '}') { */
/*                     need_space = false; */
/*                 } */
/*             } */
            
/*             if (need_space) { */
/*                 insert(' '); */
/*                 last_line_end++;  // Adjust end position */
/*             } */
/*         } */
        
/*         current_buffer->region.active = false; */
/*         return; */
/*     } */
    
/*     // Single line join (with or without prefix arg) */
/*     if (has_prefix && arg > 0) { */
/*         // With prefix arg: join current line to FOLLOWING line */
/*         size_t line_end = line_end_position(); */
/*         size_t text_len = rope_char_length(current_buffer->rope); */
        
/*         // Check if we're at last line */
/*         if (line_end >= text_len) { */
/*             return; */
/*         } */
        
/*         // Check if there's a newline */
/*         if (rope_char_at(current_buffer->rope, line_end) != '\n') { */
/*             return; */
/*         } */
        
/*         // Move to end of line */
/*         set_point(line_end); */
/*     } else { */
/*         // No prefix arg: join current line to PREVIOUS line */
/*         size_t line_start = line_beginning_position(); */
        
/*         // Check if we're at first line */
/*         if (line_start == 0) { */
/*             return; */
/*         } */
        
/*         // Move to end of previous line (before the newline) */
/*         set_point(line_start - 1); */
/*     } */
    
/*     size_t join_pos = current_buffer->pt; */
/*     size_t text_len = rope_char_length(current_buffer->rope); */
    
/*     // Delete the newline at join point */
/*     if (join_pos < text_len && rope_char_at(current_buffer->rope, join_pos) == '\n') { */
/*         delete(join_pos, 1); */
/*         text_len--; */
/*     } */
    
/*     // Delete whitespace at beginning of next line (what was after newline) */
/*     while (join_pos < text_len) { */
/*         uint32_t ch = rope_char_at(current_buffer->rope, join_pos); */
/*         if (ch != ' ' && ch != '\t') break; */
/*         delete(join_pos, 1); */
/*         text_len--; */
/*     } */
    
/*     // Delete trailing whitespace from previous line (before join point) */
/*     while (join_pos > 0) { */
/*         uint32_t ch = rope_char_at(current_buffer->rope, join_pos - 1); */
/*         if (ch != ' ' && ch != '\t') break; */
/*         join_pos--; */
/*         delete(join_pos, 1); */
/*         text_len--; */
/*         set_point(join_pos); */
/*     } */
    
/*     // Insert exactly one space at the join point (unless at buffer start or special cases) */
/*     bool need_space = true; */
    
/*     if (current_buffer->pt == 0) { */
/*         need_space = false; */
/*     } else if (current_buffer->pt > 0) { */
/*         uint32_t prev_ch = rope_char_at(current_buffer->rope, current_buffer->pt - 1); */
/*         // Don't add space after opening parens/brackets */
/*         if (prev_ch == '(' || prev_ch == '[' || prev_ch == '{') { */
/*             need_space = false; */
/*         } */
/*     } */
    
/*     // Check if next char is closing paren */
/*     if (need_space && current_buffer->pt < text_len) { */
/*         uint32_t next_ch = rope_char_at(current_buffer->rope, current_buffer->pt); */
/*         if (next_ch == ')' || next_ch == ']' || next_ch == '}') { */
/*             need_space = false; */
/*         } */
/*     } */
    
/*     if (need_space) { */
/*         insert(' '); */
/*     } */
/* } */



/// REGION

void set_mark_command() {
    current_buffer->region.mark = current_buffer->pt;
    bool transient_mark_mode = scm_get_bool("transient-mark-mode", false);
    if (transient_mark_mode) {
        current_buffer->region.active = true;
    } else {
        if (is_scm_proc(last_command, "set-mark-command")) {
            current_buffer->region.active = true;
        }
    }
}

void set_mark(size_t pos) {
    current_buffer->region.mark = pos;
    bool transient_mark_mode = scm_get_bool("transient-mark-mode", false);
    if (transient_mark_mode) activate_mark();
}

void activate_mark() {
    current_buffer->region.active = true;
}

void deactivate_mark() {
    current_buffer->region.active = false;
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
    
    int arg = get_prefix_arg();
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

// TODO Support ARG to yank N times
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
        bool raw_prefix_arg = get_raw_prefix_arg();
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
        while (rope_iter_next_char(&iter, &ch) && isWordChar(ch)) {
            // Keep going
        }
    } else {
        while (rope_iter_next_char(&iter, &ch) && !isWordChar(ch)) {
            // Keep going
        }
        while (rope_iter_next_char(&iter, &ch) && isWordChar(ch)) {
            // Keep going
        }
    }
    
    size_t result = iter.char_pos > 0 ? iter.char_pos - 1 : 0;
    
    if (iter.char_pos >= text_len) {
        result = text_len;
    }
    
    rope_iter_destroy(&iter);
    return result;
}


size_t beginning_of_word(Buffer *buffer, size_t pos) {
    if (pos == 0) return 0;
    
    size_t text_len = rope_char_length(buffer->rope);
    if (pos > text_len) pos = text_len;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, pos);
    
    uint32_t ch;
    
    // Move back one character to see what we're on
    if (!rope_iter_prev_char(&iter, &ch)) {
        rope_iter_destroy(&iter);
        return 0;
    }
    
    if (isWordChar(ch)) {
        // We're in a word, skip backwards to start of word
        while (rope_iter_prev_char(&iter, &ch) && isWordChar(ch)) {
            // Keep going back
        }
        // We went one too far (stopped on non-word char), move forward
        if (!isWordChar(ch)) {
            iter.char_pos++;
        }
    } else {
        // We're not in a word, skip backwards over non-word chars
        while (rope_iter_prev_char(&iter, &ch) && !isWordChar(ch)) {
            // Keep going back
        }
        
        // Now skip backwards over the word
        if (isWordChar(ch)) {
            while (rope_iter_prev_char(&iter, &ch) && isWordChar(ch)) {
                // Keep going back
            }
            // We went one too far, move forward
            if (!isWordChar(ch)) {
                iter.char_pos++;
            }
        }
    }
    
    size_t result = iter.char_pos;
    rope_iter_destroy(&iter);
    
    return result;
}

void forward_word() {
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    forward_word();
}

void kill_word() {
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    kill_word();
}

void capitalize_word() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t original_point = current_buffer->pt;
    
    for (int i = 0; i < count; i++) {
        size_t start_pos = current_buffer->pt;
        size_t word_end;
        
        if (direction > 0) {
            // Moving forward: capitalize from point to end of word
            word_end = end_of_word(current_buffer, start_pos);
        } else {
            // Moving backward: find previous word
            if (start_pos == 0) break;
            size_t word_start = beginning_of_word(current_buffer, start_pos);
            if (word_start == start_pos && start_pos > 0) {
                word_start = beginning_of_word(current_buffer, start_pos - 1);
            }
            start_pos = word_start;
            word_end = end_of_word(current_buffer, word_start);
        }
        
        if (start_pos >= word_end) continue;
        
        // Extract the text we're going to capitalize
        size_t word_len = word_end - start_pos;
        char *text = malloc(word_len * 4 + 1);
        if (!text) continue;
        
        size_t bytes = rope_copy_chars(current_buffer->rope, start_pos, word_len, text, word_len * 4);
        text[bytes] = '\0';
        
        // Capitalize: first letter upper, rest lower
        bool found_first = false;
        size_t text_pos = 0;
        
        while (text_pos < bytes) {
            uint32_t codepoint;
            size_t char_len;
            
            // Decode UTF-8
            unsigned char first = (unsigned char)text[text_pos];
            if (first < 0x80) {
                codepoint = first;
                char_len = 1;
            } else if ((first & 0xE0) == 0xC0) {
                codepoint = ((first & 0x1F) << 6) | (text[text_pos + 1] & 0x3F);
                char_len = 2;
            } else if ((first & 0xF0) == 0xE0) {
                codepoint = ((first & 0x0F) << 12) | 
                           ((text[text_pos + 1] & 0x3F) << 6) | 
                           (text[text_pos + 2] & 0x3F);
                char_len = 3;
            } else if ((first & 0xF8) == 0xF0) {
                codepoint = ((first & 0x07) << 18) | 
                           ((text[text_pos + 1] & 0x3F) << 12) | 
                           ((text[text_pos + 2] & 0x3F) << 6) | 
                           (text[text_pos + 3] & 0x3F);
                char_len = 4;
            } else {
                text_pos++;
                continue;
            }
            
            if (codepoint < 128 && isalpha(codepoint)) {
                if (!found_first) {
                    text[text_pos] = toupper(codepoint);
                    found_first = true;
                } else {
                    text[text_pos] = tolower(codepoint);
                }
            }
            
            text_pos += char_len;
        }
        
        // Replace the word
        current_buffer->rope = rope_delete_chars(current_buffer->rope, start_pos, word_len);
        adjust_all_window_points_after_modification(start_pos, -(int)word_len);
        
        current_buffer->rope = rope_insert_chars(current_buffer->rope, start_pos, text, bytes);
        adjust_all_window_points_after_modification(start_pos, word_len);
        
        free(text);
        
        // Move point appropriately
        if (direction > 0) {
            set_point(word_end);
        } else {
            // For backward, move to the beginning of the word we just processed
            // so the next iteration will find the previous word
            set_point(start_pos);
        }
    }
    
    // When going backward, return to original point at the end
    if (direction < 0) {
        set_point(original_point);
    }
}

void downcase_word() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t original_point = current_buffer->pt;
    
    for (int i = 0; i < count; i++) {
        size_t start_pos = current_buffer->pt;
        size_t word_end;
        
        if (direction > 0) {
            // Moving forward: downcase from point to end of word
            word_end = end_of_word(current_buffer, start_pos);
        } else {
            // Moving backward: find previous word
            if (start_pos == 0) break;
            size_t word_start = beginning_of_word(current_buffer, start_pos);
            if (word_start == start_pos && start_pos > 0) {
                word_start = beginning_of_word(current_buffer, start_pos - 1);
            }
            start_pos = word_start;
            word_end = end_of_word(current_buffer, word_start);
        }
        
        if (start_pos >= word_end) continue;
        
        // Extract the text we're going to downcase
        size_t word_len = word_end - start_pos;
        char *text = malloc(word_len * 4 + 1);
        if (!text) continue;
        
        size_t bytes = rope_copy_chars(current_buffer->rope, start_pos, word_len, text, word_len * 4);
        text[bytes] = '\0';
        
        // Convert to lowercase (ASCII only for now)
        for (size_t j = 0; j < bytes; j++) {
            if (text[j] >= 'A' && text[j] <= 'Z') {
                text[j] = tolower(text[j]);
            }
        }
        
        // Replace the word
        current_buffer->rope = rope_delete_chars(current_buffer->rope, start_pos, word_len);
        adjust_all_window_points_after_modification(start_pos, -(int)word_len);
        
        current_buffer->rope = rope_insert_chars(current_buffer->rope, start_pos, text, bytes);
        adjust_all_window_points_after_modification(start_pos, word_len);
        
        free(text);
        
        // Move point appropriately
        if (direction > 0) {
            set_point(word_end);
        } else {
            // For backward, move to the beginning of the word we just processed
            // so the next iteration will find the previous word
            set_point(start_pos);
        }
    }
    
    // When going backward, return to original point at the end
    if (direction < 0) {
        set_point(original_point);
    }
}

void upcase_word() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t original_point = current_buffer->pt;
    
    for (int i = 0; i < count; i++) {
        size_t start_pos = current_buffer->pt;
        size_t word_end;
        
        if (direction > 0) {
            // Moving forward: upcase from point to end of word
            word_end = end_of_word(current_buffer, start_pos);
        } else {
            // Moving backward: find previous word
            if (start_pos == 0) break;
            size_t word_start = beginning_of_word(current_buffer, start_pos);
            if (word_start == start_pos && start_pos > 0) {
                word_start = beginning_of_word(current_buffer, start_pos - 1);
            }
            start_pos = word_start;
            word_end = end_of_word(current_buffer, word_start);
        }
        
        if (start_pos >= word_end) continue;
        
        // Extract the text we're going to upcase
        size_t word_len = word_end - start_pos;
        char *text = malloc(word_len * 4 + 1);
        if (!text) continue;
        
        size_t bytes = rope_copy_chars(current_buffer->rope, start_pos, word_len, text, word_len * 4);
        text[bytes] = '\0';
        
        // Convert to uppercase (ASCII only for now)
        for (size_t j = 0; j < bytes; j++) {
            if (text[j] >= 'a' && text[j] <= 'z') {
                text[j] = toupper(text[j]);
            }
        }
        
        // Replace the word
        current_buffer->rope = rope_delete_chars(current_buffer->rope, start_pos, word_len);
        adjust_all_window_points_after_modification(start_pos, -(int)word_len);
        
        current_buffer->rope = rope_insert_chars(current_buffer->rope, start_pos, text, bytes);
        adjust_all_window_points_after_modification(start_pos, word_len);
        
        free(text);
        
        // Move point appropriately
        if (direction > 0) {
            set_point(word_end);
        } else {
            // For backward, move to the beginning of the word we just processed
            // so the next iteration will find the previous word
            set_point(start_pos);
        }
    }
    
    // When going backward, return to original point at the end
    if (direction < 0) {
        set_point(original_point);
    }
}




/// PARAGRAPHS

void forward_paragraph() {
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    forward_paragraph();
}

/// TRANSPOSE

void transpose_chars() {
    int arg = get_prefix_arg();
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Can't transpose with empty buffer
    if (text_len == 0) {
        return;
    }
    
    // Special case: at end of line but not beginning of buffer
    // Move back one character first (like Emacs does)
    if (current_buffer->pt < text_len) {
        uint32_t ch = rope_char_at(current_buffer->rope, current_buffer->pt);
        if (ch == '\n' && current_buffer->pt > 0) {
            set_point(current_buffer->pt - 1);
        }
    } else if (current_buffer->pt == text_len && current_buffer->pt > 0) {
        // At end of buffer - move back one
        set_point(current_buffer->pt - 1);
    }
    
    if (arg == 0) {
        // ARG is 0: transpose chars at point and mark
        if (current_buffer->region.mark == 0) {
            message("No mark set in this buffer");
            return;
        }
        
        size_t pos1 = current_buffer->pt;
        size_t pos2 = current_buffer->region.mark;
        
        // Need at least one char at each position
        if (pos1 >= text_len || pos2 >= text_len) {
            message("Don't have two things to transpose");
            return;
        }
        
        // Extract both characters (1 char each)
        char char1[5] = {0}, char2[5] = {0};
        size_t char1_bytes = rope_copy_chars(current_buffer->rope, pos1, 1, char1, 4);
        size_t char2_bytes = rope_copy_chars(current_buffer->rope, pos2, 1, char2, 4);
        char1[char1_bytes] = '\0';
        char2[char2_bytes] = '\0';
        
        // Swap them (handle order properly)
        if (pos1 < pos2) {
            // Delete and insert at pos1
            current_buffer->rope = rope_delete_chars(current_buffer->rope, pos1, 1);
            adjust_all_window_points_after_modification(pos1, -1);
            current_buffer->rope = rope_insert_chars(current_buffer->rope, pos1, char2, char2_bytes);
            adjust_all_window_points_after_modification(pos1, 1);
            
            // Delete and insert at pos2
            current_buffer->rope = rope_delete_chars(current_buffer->rope, pos2, 1);
            adjust_all_window_points_after_modification(pos2, -1);
            current_buffer->rope = rope_insert_chars(current_buffer->rope, pos2, char1, char1_bytes);
            adjust_all_window_points_after_modification(pos2, 1);
        } else {
            // pos2 < pos1
            current_buffer->rope = rope_delete_chars(current_buffer->rope, pos2, 1);
            adjust_all_window_points_after_modification(pos2, -1);
            current_buffer->rope = rope_insert_chars(current_buffer->rope, pos2, char1, char1_bytes);
            adjust_all_window_points_after_modification(pos2, 1);
            
            current_buffer->rope = rope_delete_chars(current_buffer->rope, pos1, 1);
            adjust_all_window_points_after_modification(pos1, -1);
            current_buffer->rope = rope_insert_chars(current_buffer->rope, pos1, char2, char2_bytes);
            adjust_all_window_points_after_modification(pos1, 1);
        }
        
        // Exchange point and mark, then move backward
        exchange_point_and_mark();
        backward_char();
        return;
    }
    
    // For positive or negative args
    size_t pos = current_buffer->pt;
    
    if (arg > 0) {
        // Positive: drag char before point forward
        if (pos == 0) {
            message("Beginning of buffer");
            return;
        }
        
        // Extract char before point (1 char)
        char char_before[5] = {0};
        size_t char_bytes = rope_copy_chars(current_buffer->rope, pos - 1, 1, char_before, 4);
        char_before[char_bytes] = '\0';
        
        // Delete char before point
        current_buffer->rope = rope_delete_chars(current_buffer->rope, pos - 1, 1);
        adjust_all_window_points_after_modification(pos - 1, -1);
        set_point(pos - 1);
        
        // Move forward arg positions
        size_t target_pos = current_buffer->pt;
        for (int i = 0; i < arg && target_pos < text_len - 1; i++) {
            target_pos++;
        }
        set_point(target_pos);
        
        // Insert the character
        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt, char_before, char_bytes);
        adjust_all_window_points_after_modification(current_buffer->pt, 1);
        set_point(current_buffer->pt + 1);
        
    } else if (arg < 0) {
        // Negative: drag char before point backward
        if (pos == 0) {
            message("Beginning of buffer");
            return;
        }
        
        // Extract char before point (1 char)
        char char_before[5] = {0};
        size_t char_bytes = rope_copy_chars(current_buffer->rope, pos - 1, 1, char_before, 4);
        char_before[char_bytes] = '\0';
        
        // Delete char before point
        current_buffer->rope = rope_delete_chars(current_buffer->rope, pos - 1, 1);
        adjust_all_window_points_after_modification(pos - 1, -1);
        set_point(pos - 1);
        
        // Move backward |arg| positions
        size_t target_pos = current_buffer->pt;
        for (int i = 0; i < -arg && target_pos > 0; i++) {
            target_pos--;
        }
        set_point(target_pos);
        
        // Insert the character
        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt, char_before, char_bytes);
        adjust_all_window_points_after_modification(current_buffer->pt, 1);
        set_point(current_buffer->pt + 1);
    }
}

void transpose_words() {
    int arg = get_prefix_arg();
    size_t text_len = rope_char_length(current_buffer->rope);
    
    if (text_len == 0) {
        return;
    }
    
    if (arg == 0) {
        // ARG is 0: transpose words at point and mark
        if (current_buffer->region.mark == 0) {
            message("No mark set in this buffer");
            return;
        }
        
        // Find word boundaries around point
        size_t word1_start = beginning_of_word(current_buffer, current_buffer->pt);
        size_t word1_end = end_of_word(current_buffer, current_buffer->pt);
        
        // Find word boundaries around mark
        size_t saved_pt = current_buffer->pt;
        set_point(current_buffer->region.mark);
        size_t word2_start = beginning_of_word(current_buffer, current_buffer->pt);
        size_t word2_end = end_of_word(current_buffer, current_buffer->pt);
        set_point(saved_pt);
        
        // Check if we have valid words
        if (word1_start == word1_end || word2_start == word2_end) {
            message("Don't have two things to transpose");
            return;
        }
        
        // Check for overlap
        if ((word1_start <= word2_start && word1_end > word2_start) ||
            (word2_start <= word1_start && word2_end > word1_start)) {
            message("Don't have two things to transpose");
            return;
        }
        
        // Ensure word1 comes before word2
        if (word1_start > word2_start) {
            size_t temp = word1_start;
            word1_start = word2_start;
            word2_start = temp;
            temp = word1_end;
            word1_end = word2_end;
            word2_end = temp;
        }
        
        // Extract both words
        size_t len1 = word1_end - word1_start;
        size_t len2 = word2_end - word2_start;
        
        char *word1 = malloc(len1 * 4 + 1);
        char *word2 = malloc(len2 * 4 + 1);
        
        size_t word1_bytes = rope_copy_chars(current_buffer->rope, word1_start, len1, word1, len1 * 4);
        word1[word1_bytes] = '\0';
        
        size_t word2_bytes = rope_copy_chars(current_buffer->rope, word2_start, len2, word2, len2 * 4);
        word2[word2_bytes] = '\0';
        
        // Delete word2 first (it's later in the buffer)
        current_buffer->rope = rope_delete_chars(current_buffer->rope, word2_start, len2);
        adjust_all_window_points_after_modification(word2_start, -(int)len2);
        
        // Insert word1 at word2's position
        current_buffer->rope = rope_insert_chars(current_buffer->rope, word2_start, word1, word1_bytes);
        adjust_all_window_points_after_modification(word2_start, len1);
        
        // Delete word1
        current_buffer->rope = rope_delete_chars(current_buffer->rope, word1_start, len1);
        adjust_all_window_points_after_modification(word1_start, -(int)len1);
        
        // Insert word2 at word1's position
        current_buffer->rope = rope_insert_chars(current_buffer->rope, word1_start, word2, word2_bytes);
        adjust_all_window_points_after_modification(word1_start, len2);
        
        free(word1);
        free(word2);
        
        // Exchange point and mark
        exchange_point_and_mark();
        return;
    }
    
    if (arg > 0) {
        // Positive: drag word backward from point forward past arg words
        // Find the word before/at point
        size_t word1_end = current_buffer->pt;
        size_t word1_start = beginning_of_word(current_buffer, word1_end);
        
        // If we're in whitespace, move back to get the actual word
        if (word1_start == word1_end) {
            if (word1_start == 0) {
                message("Don't have two things to transpose");
                return;
            }
            word1_end = word1_start;
            word1_start = beginning_of_word(current_buffer, word1_start - 1);
        }
        
        word1_end = end_of_word(current_buffer, word1_start);
        
        if (word1_start == word1_end) {
            message("Don't have two things to transpose");
            return;
        }
        
        // Now find word2 (the word to swap with)
        set_point(word1_end);
        
        // Skip whitespace to get to next word
        size_t word2_start = word1_end;
        while (word2_start < text_len) {
            uint32_t ch = rope_char_at(current_buffer->rope, word2_start);
            if (isWordChar(ch)) break;
            word2_start++;
        }
        
        if (word2_start >= text_len) {
            message("Don't have two things to transpose");
            backward_word();
            return;
        }
        
        // Move forward arg-1 more words if arg > 1
        for (int i = 1; i < arg; i++) {
            size_t next_end = end_of_word(current_buffer, word2_start);
            if (next_end == word2_start) break;
            
            // Skip to next word
            word2_start = next_end;
            while (word2_start < text_len) {
                uint32_t ch = rope_char_at(current_buffer->rope, word2_start);
                if (isWordChar(ch)) break;
                word2_start++;
            }
            
            if (word2_start >= text_len) break;
        }
        
        size_t word2_end = end_of_word(current_buffer, word2_start);
        
        if (word2_start == word2_end) {
            message("Don't have two things to transpose");
            return;
        }
        
        // Extract both words
        size_t len1 = word1_end - word1_start;
        size_t len2 = word2_end - word2_start;
        
        char *word1 = malloc(len1 * 4 + 1);
        char *word2 = malloc(len2 * 4 + 1);
        
        size_t word1_bytes = rope_copy_chars(current_buffer->rope, word1_start, len1, word1, len1 * 4);
        word1[word1_bytes] = '\0';
        
        size_t word2_bytes = rope_copy_chars(current_buffer->rope, word2_start, len2, word2, len2 * 4);
        word2[word2_bytes] = '\0';
        
        // Delete word2 first (it's later in buffer)
        current_buffer->rope = rope_delete_chars(current_buffer->rope, word2_start, len2);
        adjust_all_window_points_after_modification(word2_start, -(int)len2);
        
        // Insert word1 where word2 was
        current_buffer->rope = rope_insert_chars(current_buffer->rope, word2_start, word1, word1_bytes);
        adjust_all_window_points_after_modification(word2_start, len1);
        
        // Delete word1
        current_buffer->rope = rope_delete_chars(current_buffer->rope, word1_start, len1);
        adjust_all_window_points_after_modification(word1_start, -(int)len1);
        
        // Insert word2 where word1 was
        current_buffer->rope = rope_insert_chars(current_buffer->rope, word1_start, word2, word2_bytes);
        adjust_all_window_points_after_modification(word1_start, len2);
        
        // Set point to end of where word2 now is (accounting for length difference)
        size_t final_pos = word1_start + len2;
        // Add the space between original positions
        final_pos += (word2_start - word1_end);
        // Adjust for the length difference
        final_pos += len1;
        
        set_point(final_pos);
        
        free(word1);
        free(word2);
        
        
    } else if (arg < 0) {
        // Negative: drag word before point backward past |arg| words
        size_t word_end = current_buffer->pt;
        size_t word_start = beginning_of_word(current_buffer, word_end);
    
        // If we're in whitespace, move back to get the actual word
        if (word_start == word_end) {
            if (word_start == 0) {
                message("Don't have two things to transpose");
                return;
            }
            word_end = word_start;
            word_start = beginning_of_word(current_buffer, word_start - 1);
        }
    
        word_end = end_of_word(current_buffer, word_start);
    
        if (word_start == word_end) {
            message("Don't have two things to transpose");
            return;
        }
    
        // Extract the word
        size_t word_len = word_end - word_start;
        char *word = malloc(word_len * 4 + 1);
        size_t word_bytes = rope_copy_chars(current_buffer->rope, word_start, word_len, word, word_len * 4);
        word[word_bytes] = '\0';
    
        // Find the whitespace BEFORE the word - this is what we'll delete
        size_t delete_start = word_start;
        while (delete_start > 0) {
            uint32_t ch = rope_char_at(current_buffer->rope, delete_start - 1);
            if (isWordChar(ch)) break;
            delete_start--;
        }
    
        // Delete word with preceding whitespace
        size_t delete_len = word_end - delete_start;
        current_buffer->rope = rope_delete_chars(current_buffer->rope, delete_start, delete_len);
        adjust_all_window_points_after_modification(delete_start, -(int)delete_len);
        set_point(delete_start);
    
        // Move backward |arg| words
        for (int i = 0; i < -arg; i++) {
            if (current_buffer->pt == 0) break;
            size_t prev_start = beginning_of_word(current_buffer, current_buffer->pt - 1);
            if (prev_start == current_buffer->pt) break;
            set_point(prev_start);
        }
    
        // Insert the word followed by a space
        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt, word, word_bytes);
        adjust_all_window_points_after_modification(current_buffer->pt, word_len);
    
        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt + word_len, " ", 1);
        adjust_all_window_points_after_modification(current_buffer->pt + word_len, 1);
    
        set_point(current_buffer->pt + word_len);  // Cursor after word, before space
    
        free(word);
    }
}
    
    
/// LISTS

// NOTE We don’t handle <> because they are used as operators
// in many languages. We should make list scanning more sophisticated
// later when we have treesitter
static bool is_opening_paren(uint32_t c) {
    return c == '(' || c == '[' || c == '{';
}

static bool is_closing_paren(uint32_t c) {
    return c == ')' || c == ']' || c == '}';
}

static uint32_t matching_paren(uint32_t c) {
    switch (c) {
    case '(': return ')';
    case ')': return '(';
    case '[': return ']';
    case ']': return '[';
    case '{': return '}';
    case '}': return '{';
    default: return 0;
    }
}


// Core symbol chars (alphanumeric, underscore, dash)
static bool is_core_symbol_char(uint32_t c) {
    return isalnum(c) || c == '_' || c == '-';
}

// Symbols include alphanumeric, underscore, and dash
// but _ and - are only valid if followed by more symbol chars
static bool is_symbol_char(uint32_t c) {
    if (c >= 128) return false;
    if (isspace(c)) return false;
    if (c == '(' || c == ')' || c == '[' || c == ']' || 
        c == '{' || c == '}' || c == '"' || c == '\'' || 
        c == ';' || c == '`' || c == ',') {
        return false;
    }
    // Exclude operators (including >)
    if (c == '=' || c == '|' || c == '&' || c == '<' || c == '>' ||
        c == '+' || c == '*' || c == '/' || c == '%' || c == '!' ||
        c == '~' || c == '^' || c == '?' || c == ':' || c == '@') {
        return false;
    }
    return true;
}

size_t scan_lists(Buffer *buffer, size_t from, int count, int depth, bool sexpflag) {
    size_t text_len = rope_char_length(buffer->rope);
    int min_depth = depth;
    size_t last_good = from;
    
    if (depth > 0) min_depth = 0;
    
    if (count > 0) {
        // Scan forward
        rope_iter_t iter;
        rope_iter_init(&iter, buffer->rope, from);
        uint32_t ch;
        
        while (count > 0) {
            while (iter.char_pos < text_len) {
                if (!rope_iter_next_char(&iter, &ch)) break;
                
                if (depth == min_depth) {
                    last_good = iter.char_pos - 1;
                }
                
                // Skip escaped characters
                if (ch == '\\') {
                    if (rope_iter_next_char(&iter, &ch)) {
                        continue;
                    } else {
                        break;
                    }
                }
                
                // Handle strings
                if (ch == '"') {
                    bool escaped = false;
                    bool found_end = false;
                    
                    while (rope_iter_next_char(&iter, &ch)) {
                        if (escaped) {
                            escaped = false;
                            continue;
                        }
                        if (ch == '\\') {
                            escaped = true;
                            continue;
                        }
                        if (ch == '"') {
                            found_end = true;
                            break;
                        }
                    }
                    
                    if (!found_end) {
                        rope_iter_destroy(&iter);
                        return SIZE_MAX;
                    }
                    
                    if (depth == 0 && sexpflag) {
                        count--;
                        if (count == 0) break;
                    }
                    continue;
                }
                
                // Skip comments
                if (ch == ';') {
                    while (rope_iter_next_char(&iter, &ch)) {
                        if (ch == '\n') break;
                    }
                    continue;
                }
                
                // Handle symbols in sexp mode
                if (sexpflag && depth == 0 && is_symbol_char(ch)) {
                    // Special case: if we start on -, check if it's part of ->
                    if (ch == '-' && iter.char_pos < text_len) {
                        uint32_t next_ch = rope_char_at(buffer->rope, iter.char_pos);
                        if (next_ch == '>') {
                            // Skip the -> operator entirely
                            iter.char_pos++; // skip the >
                            continue; // don't count as a sexp, just skip it
                        }
                    }
                    
                    while (rope_iter_next_char(&iter, &ch)) {
                        if (ch == '\\') {
                            rope_iter_next_char(&iter, &ch);
                            continue;
                        }
                        if (!is_symbol_char(ch)) {
                            iter.char_pos--;
                            break;
                        }
                        // Stop before _ or - if not followed by more symbol chars
                        if (ch == '_' || ch == '-') {
                            size_t peek_pos = iter.char_pos;
                            uint32_t next_ch;
                            if (peek_pos < text_len) {
                                next_ch = rope_char_at(buffer->rope, peek_pos);
                                // Special case: -> is a single operator, skip both chars
                                if (ch == '-' && next_ch == '>') {
                                    iter.char_pos--;
                                    break;
                                }
                                if (!is_core_symbol_char(next_ch)) {
                                    iter.char_pos--;
                                    break;
                                }
                            } else {
                                iter.char_pos--;
                                break;
                            }
                        }
                    }
                    count--;
                    if (count == 0) break;
                    continue;
                }
                
                // Handle opening parens
                if (is_opening_paren(ch)) {
                    depth++;
                    if (depth == 0) {
                        count--;
                        if (count == 0) break;
                    }
                    continue;
                }
                
                // Handle closing parens
                if (is_closing_paren(ch)) {
                    depth--;
                    if (depth == 0) {
                        count--;
                        if (count == 0) break;
                    }
                    if (depth < min_depth) {
                        rope_iter_destroy(&iter);
                        return SIZE_MAX;
                    }
                    continue;
                }
            }
            
            if (iter.char_pos >= text_len) {
                rope_iter_destroy(&iter);
                if (depth != 0) {
                    return SIZE_MAX;
                }
                return text_len;
            }
        }
        
        size_t result = iter.char_pos;
        rope_iter_destroy(&iter);
        return result;
        
    } else {
        // Scan backward
        rope_iter_t iter;
        rope_iter_init(&iter, buffer->rope, from);
        uint32_t ch;
        
        while (count < 0) {
            while (iter.char_pos > 0) {
                if (!rope_iter_prev_char(&iter, &ch)) break;
                
                if (depth == min_depth) {
                    last_good = iter.char_pos;
                }
                
                // Handle strings when scanning backward
                if (ch == '"') {
                    bool is_escaped = false;
                    if (iter.char_pos > 0) {
                        size_t check_pos = iter.char_pos - 1;
                        int backslash_count = 0;
                        while (check_pos > 0 && rope_char_at(buffer->rope, check_pos) == '\\') {
                            backslash_count++;
                            check_pos--;
                        }
                        is_escaped = (backslash_count % 2 == 1);
                    }
                    
                    if (!is_escaped) {
                        bool found_start = false;
                        while (rope_iter_prev_char(&iter, &ch)) {
                            if (ch == '"') {
                                if (iter.char_pos > 0) {
                                    size_t check_pos = iter.char_pos - 1;
                                    int backslash_count = 0;
                                    while (check_pos > 0 && rope_char_at(buffer->rope, check_pos) == '\\') {
                                        backslash_count++;
                                        check_pos--;
                                    }
                                    if (backslash_count % 2 == 1) {
                                        continue;
                                    }
                                }
                                found_start = true;
                                break;
                            }
                        }
                        
                        if (!found_start) {
                            rope_iter_destroy(&iter);
                            return SIZE_MAX;
                        }
                        
                        if (depth == 0 && sexpflag) {
                            count++;
                            if (count == 0) break;
                        }
                        continue;
                    }
                }
                
                // Handle symbols when scanning backward
                if (sexpflag && depth == 0 && is_symbol_char(ch)) {
                    // Check if this is a trailing _ or - that shouldn't be included
                    if (ch == '_' || ch == '-') {
                        size_t peek_pos = iter.char_pos;
                        if (peek_pos < text_len) {
                            uint32_t next_ch = rope_char_at(buffer->rope, peek_pos);
                            if (!is_core_symbol_char(next_ch)) {
                                continue;
                            }
                        } else {
                            continue;
                        }
                    }
                    
                    while (rope_iter_prev_char(&iter, &ch)) {
                        if (ch == '\\') {
                            continue;
                        }
                        if (!is_symbol_char(ch)) {
                            iter.char_pos++;
                            break;
                        }
                    }
                    
                    if (iter.char_pos == 0 && is_symbol_char(rope_char_at(buffer->rope, 0))) {
                        // Already at start
                    }
                    
                    count++;
                    if (count == 0) break;
                    continue;
                }
                
                // Handle closing parens
                if (is_closing_paren(ch)) {
                    depth++;
                    if (depth == 0) {
                        count++;
                        if (count == 0) break;
                    }
                    continue;
                }
                
                // Handle opening parens
                if (is_opening_paren(ch)) {
                    depth--;
                    if (depth == 0) {
                        count++;
                        if (count == 0) break;
                    }
                    if (depth < min_depth) {
                        rope_iter_destroy(&iter);
                        return SIZE_MAX;
                    }
                    continue;
                }
            }
            
            if (iter.char_pos == 0) {
                rope_iter_destroy(&iter);
                if (depth != 0) {
                    return SIZE_MAX;
                }
                return 0;
            }
        }
        
        size_t result = iter.char_pos;
        rope_iter_destroy(&iter);
        return result;
    }
}

void forward_list() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    size_t result = scan_lists(current_buffer, current_buffer->pt, arg, 0, false);
    
    if (result == SIZE_MAX) {
        message(arg > 0 ? "No next group" : "No previous group");
        return;
    }
    
    set_point(result);
}

void backward_list() {
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    forward_list();
}


/// SEXPS

size_t scan_sexps(Buffer *buffer, size_t from, int count) {
    return scan_lists(buffer, from, count, 0, true);
}

void forward_sexp() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    size_t result = scan_sexps(current_buffer, current_buffer->pt, arg);
    
    if (result == SIZE_MAX) {
        message(arg > 0 ? "No next sexp" : "No previous sexp");
        return;
    }
    
    set_point(result);
}

void backward_sexp() {
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    forward_sexp();
}

void kill_sexp() {
    set_mark_command();
    forward_sexp();
    kill_region();
}

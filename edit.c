#include "edit.h"
#include "buffer.h"
#include "lisp.h"
#include "faces.h"
#include "frame.h"
#include <ctype.h>

const char *last_notation = NULL;


void read_only_mode() {
    current_buffer->read_only = !current_buffer->read_only;
}

void insert(const char *text) {
    if (!text) return;
    
    size_t len = strlen(text);
    if (len == 0) return;
    
    if (current_buffer->read_only) {
        message("Buffer is read-only: #<buffer %s>", current_buffer->name);
        return; 
    }
    
    size_t insert_pos = current_buffer->pt;
    
    // Check if we're trying to insert at a read-only position
    SCM readonly = get_text_property(current_buffer, insert_pos, scm_from_locale_symbol("read-only"));
    if (scm_is_true(readonly)) {
        message("Text is read-only");
        return;
    }
    
    // Get prefix argument (how many times to insert)
    int arg = get_prefix_arg();
    int insert_count = (arg == 0) ? 1 : abs(arg);
    
    // Calculate how many characters (not bytes) we're inserting per iteration
    size_t char_count = 0;
    for (size_t i = 0; i < len; ) {
        size_t bytes_read;
        uint32_t ch = utf8_decode(&text[i], len - i, &bytes_read);
        if (bytes_read == 0) break;
        i += bytes_read;
        char_count++;
    }
    
    size_t total_char_count = char_count * insert_count;
    
    // Check if tree-sitter is active
    bool has_treesit = current_buffer->ts_state && current_buffer->ts_state->tree;
    
    // Capture tree-sitter state BEFORE modification
    size_t start_byte = 0;
    TSPoint start_point = {0, 0};
    TSPoint new_end_point = {0, 0};
    
    if (has_treesit) {
        start_byte = rope_char_to_byte(current_buffer->rope, insert_pos);
        start_point = treesit_char_to_point(current_buffer, insert_pos);
        
        // Calculate new_end_point based on inserted content (repeated insert_count times)
        new_end_point = start_point;
        
        for (int repeat = 0; repeat < insert_count; repeat++) {
            // Scan inserted bytes for newlines
            size_t bytes_after_last_newline = 0;
            for (size_t i = 0; i < len; i++) {
                if (text[i] == '\n') {
                    new_end_point.row++;
                    new_end_point.column = 0;
                    bytes_after_last_newline = 0;
                } else {
                    bytes_after_last_newline++;
                }
            }
            
            // Update column based on whether we had newlines
            if (bytes_after_last_newline > 0 || new_end_point.row == start_point.row) {
                new_end_point.column += bytes_after_last_newline;
            }
        }
    }
    
    // Update region mark
    if (current_buffer->region.mark >= 0 && (size_t)current_buffer->region.mark > insert_pos) {
        current_buffer->region.mark += total_char_count;
    }
    
    // Perform the rope insertion (insert_count times)
    for (int i = 0; i < insert_count; i++) {
        current_buffer->rope = rope_insert_chars(
            current_buffer->rope,
            insert_pos + (i * char_count),
            text,
            len
        );
    }
    
    // Update tree-sitter if active
    if (has_treesit) {
        size_t new_end_byte = start_byte + (len * insert_count);
        
        treesit_update_tree(
            current_buffer,
            start_byte,
            start_byte,      // old_end_byte = start_byte (nothing was there before)
            new_end_byte,
            start_point,
            start_point,     // old_end_point = start_point (nothing was there before)
            new_end_point
        );
        
        treesit_reparse_if_needed(current_buffer);
        treesit_apply_highlights(current_buffer);
    }
    
    // Only adjust text properties if tree-sitter is NOT active
    if (!has_treesit) {
        adjust_text_properties(current_buffer, insert_pos, total_char_count);
    }
    
    adjust_all_window_points_after_modification(insert_pos, total_char_count);
    set_point(current_buffer->pt + total_char_count);
    update_goal_column();
    reset_cursor_blink(current_buffer);
    current_buffer->modified = true;
}

// Add this function in main.c or edit.c
// Helper function to check if there are unmatched closing characters of a specific type after point
static bool has_unmatched_closing_after(Buffer *buffer, size_t point, char opening, char closing) {
    size_t buf_len = rope_char_length(buffer->rope);
    if (point >= buf_len) return false;
    
    int balance = 0;
    
    // Scan from beginning to current point to get initial balance
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    uint32_t ch;
    
    while (iter.char_pos < point) {
        if (!rope_iter_next_char(&iter, &ch)) break;
        
        if (ch == (uint32_t)opening) {
            balance++;
        } else if (ch == (uint32_t)closing) {
            balance--;
        }
    }
    
    // Now scan from point to end, checking if balance ever goes negative
    while (iter.char_pos < buf_len) {
        if (!rope_iter_next_char(&iter, &ch)) break;
        
        if (ch == (uint32_t)opening) {
            balance++;
        } else if (ch == (uint32_t)closing) {
            balance--;
            // If balance goes negative, we found an unmatched closing char
            if (balance < 0) {
                rope_iter_destroy(&iter);
                return true;
            }
        }
    }
    
    rope_iter_destroy(&iter);
    return false;
}

#include "minibuf.h"

// TODO OBSIDIAN Capslock doesn't work
// CAPS j notation should come as J
void self_insert_command() {
    bool delete_selection_mode = scm_get_bool("delete-selection-mode", false);
    if (delete_selection_mode && current_buffer->region.active) {
        delete_region();
    } else {
        deactivate_mark(); // TODO Replace region if...
    }
    
    clear_minibuffer_message();
    
    // Translate special key notations to actual characters
    const char *text_to_insert = last_notation;
    char special_char[2] = {0};
    
    if (strcmp(last_notation, "SPC") == 0 || strcmp(last_notation, " ") == 0) {
        special_char[0] = ' ';
        text_to_insert = special_char;
    } else if (strcmp(last_notation, "TAB") == 0) {
        special_char[0] = '\t';
        text_to_insert = special_char;
    }
    
    bool electric_pair_mode = scm_get_bool("electric-pair-mode", false);
    
    // Handle electric pairing (only for single-byte characters)
    if (electric_pair_mode && text_to_insert[1] == '\0') {
        char ch = text_to_insert[0];
        char closing_char = 0;
        bool should_pair = false;
        bool is_closing = false;
        
        switch (ch) {
            case '(': closing_char = ')'; should_pair = true; break;
            case '[': closing_char = ']'; should_pair = true; break;
            case '{': closing_char = '}'; should_pair = true; break;
            case '<': closing_char = '>'; should_pair = true; break;
            case '"': closing_char = '"'; should_pair = true; break;
            case '\'': closing_char = '\''; should_pair = true; break;
            case '`': closing_char = '`'; should_pair = true; break;
            
            case ')':
            case ']':
            case '}':
            case '>':
                is_closing = true;
                break;
        }
        
        // If we typed a closing character, check if it matches the next character
        if (is_closing) {
            size_t buf_len = rope_char_length(current_buffer->rope);
            if (current_buffer->pt < buf_len) {
                uint32_t next_char = rope_char_at(current_buffer->rope, current_buffer->pt);
                
                if (next_char == (uint32_t)ch) {
                    set_point(current_buffer->pt + 1);
                    
                    if (scm_get_bool("make-pointer-invisible", true) &&
                        scm_get_bool("pointer-visible", true)) {
                        hideCursor();
                        scm_c_define("pointer-visible", SCM_BOOL_F);
                    }
                    return;
                }
            }
        }
        
        if (should_pair) {
            bool has_unmatched = has_unmatched_closing_after(current_buffer, current_buffer->pt, ch, closing_char);
            
            if (has_unmatched) {
                insert(text_to_insert);
                
                if (scm_get_bool("make-pointer-invisible", true) &&
                    scm_get_bool("pointer-visible", true)) {
                    hideCursor();
                    scm_c_define("pointer-visible", SCM_BOOL_F);
                }
                return;
            }
            
            // Insert both characters at once
            char pair_str[3] = {ch, closing_char, '\0'};
            insert(pair_str);
            
            // Move point back one position (between the pair)
            set_point(current_buffer->pt - 1);
            
            if (scm_get_bool("make-pointer-invisible", true) &&
                scm_get_bool("pointer-visible", true)) {
                hideCursor();
                scm_c_define("pointer-visible", SCM_BOOL_F);
            }
            return;
        }
    }
    
    // Normal insertion - insert the (possibly translated) text
    insert(text_to_insert);
    
    if (scm_get_bool("make-pointer-invisible", true) &&
        scm_get_bool("pointer-visible", true)) {
        hideCursor();
        scm_c_define("pointer-visible", SCM_BOOL_F);
    }
}

#include <setjmp.h>

jmp_buf delete_readonly;
jmp_buf kill_readonly;

size_t delete_impl(size_t pos, size_t count) {
    if (current_buffer->read_only) {
        message("Buffer is read-only: #<buffer %s>", current_buffer->name);
        longjmp(delete_readonly, 1);
    }

    if (count == 0) {
        size_t text_len = rope_char_length(current_buffer->rope);
        if (pos == 0) {
            message("Beginning of buffer");
        } else if (pos >= text_len) {
            message("End of buffer");
        }
        return pos;
    }
    
    size_t text_len = rope_char_length(current_buffer->rope);
    
    if (pos >= text_len) {
        message("End of buffer");
        return pos;
    }
    
    if (pos + count > text_len) {
        count = text_len - pos;
    }
    
    if (count == 0) {
        message("End of buffer");
        return pos;
    }
   
    size_t delete_end = pos + count;
    
    // Check for read-only text in the deletion range
    if (is_range_readonly(current_buffer, pos, delete_end)) {
        message("Text is read-only");
        longjmp(delete_readonly, 1);
    }
    
    // Check if tree-sitter is active
    bool has_treesit = current_buffer->ts_state && current_buffer->ts_state->tree;
    
    // Capture tree-sitter state BEFORE modification
    size_t start_byte = 0;
    size_t old_end_byte = 0;
    TSPoint start_point = {0, 0};
    TSPoint old_end_point = {0, 0};
    
    if (has_treesit) {
        start_byte = rope_char_to_byte(current_buffer->rope, pos);
        old_end_byte = rope_char_to_byte(current_buffer->rope, delete_end);
        start_point = treesit_char_to_point(current_buffer, pos);
        old_end_point = treesit_char_to_point(current_buffer, delete_end);
    }
    
    // Update region mark
    if (current_buffer->region.mark >= 0) {
        if ((size_t)current_buffer->region.mark >= delete_end) {
            current_buffer->region.mark -= count;
        } else if ((size_t)current_buffer->region.mark > pos) {
            current_buffer->region.mark = pos;
        }
    }
    
    // Perform the deletion
    current_buffer->rope = rope_delete_chars(current_buffer->rope, pos, count);
    
    // Update tree-sitter if active
    if (has_treesit) {
        size_t new_end_byte = start_byte;
        TSPoint new_end_point = start_point;
        
        treesit_update_tree(
            current_buffer,
            start_byte,
            old_end_byte,
            new_end_byte,
            start_point,
            old_end_point,
            new_end_point
        );
        
        treesit_reparse_if_needed(current_buffer);
        treesit_apply_highlights(current_buffer);
    }
    
    // Only adjust text properties if tree-sitter is NOT active
    if (!has_treesit) {
        adjust_text_properties(current_buffer, pos, -(int)count);
    }
    
    adjust_all_window_points_after_modification(pos, -(int)count);
    reset_cursor_blink(current_buffer);
    current_buffer->modified = true;
    return pos;
}

// Macro that sets up setjmp and calls delete_impl
// If longjmp occurs, it returns from the calling function
// NOTE This is so when we try to delete readonly text
// we don’t move the cursor if we didn’t delete the text
#define delete(pos, count)                      \
    do {                                        \
        if (setjmp(delete_readonly) != 0) {     \
            return;                             \
        }                                       \
        delete_impl((pos), (count));            \
    } while (0)



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

void save_buffer() {
    current_buffer->modified = false;
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
        insert("\n");
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
    
    insert("\n");
    
    for (size_t i = 0; i < col; i++) {
        insert(" ");
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


// Helper: Check if position has a field property
static bool has_field_property(size_t pos) {
    if (!current_buffer) return false;
    SCM field = get_text_property(current_buffer, pos, scm_from_locale_symbol("field"));
    return !scm_is_false(field);
}

// Check if two positions have different field values
static bool different_fields(size_t pos1, size_t pos2) {
    SCM field1 = get_text_property_field(current_buffer, pos1);
    SCM field2 = get_text_property_field(current_buffer, pos2);
    
    // Both have no field - same field
    if (scm_is_false(field1) && scm_is_false(field2)) {
        return false;
    }
    
    // One has field, other doesn't - different fields
    if (scm_is_false(field1) || scm_is_false(field2)) {
        return true;
    }
    
    // Both have fields - compare values
    return scm_is_false(scm_equal_p(field1, field2));
}

// Constrain target position to not cross field boundaries
static size_t constrain_to_field(size_t target_pos, size_t original_pos) {
    if (target_pos == original_pos) {
        return target_pos;
    }
    
    size_t text_len = rope_char_length(current_buffer->rope);
    bool moving_backward = target_pos < original_pos;
    
    if (moving_backward) {
        // Scan from target to original, stop at first field boundary
        for (size_t pos = target_pos; pos < original_pos; pos++) {
            if (different_fields(pos, pos + 1)) {
                // Found boundary - return position after it
                return pos + 1;
            }
        }
    } else {
        // Moving forward - scan from original to target
        for (size_t pos = original_pos; pos < target_pos && pos < text_len; pos++) {
            if (different_fields(pos, pos + 1)) {
                // Found boundary - return position before it
                return pos + 1;
            }
        }
    }
    
    return target_pos;
}

size_t line_beginning_position(int n) {
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t pos = current_buffer->pt;
    
    if (n > 0) {
        // Move forward n-1 lines, then to beginning of that line
        for (int i = 0; i < n - 1; i++) {
            while (pos < text_len && rope_char_at(current_buffer->rope, pos) != '\n') {
                pos++;
            }
            if (pos >= text_len) break;
            pos++; // Move past newline
        }
    } else if (n < 0) {
        // Move backward |n|+1 lines, then to beginning of that line
        int lines = abs(n) + 1;
        for (int i = 0; i < lines; i++) {
            // Go to beginning of current line
            while (pos > 0 && rope_char_at(current_buffer->rope, pos - 1) != '\n') {
                pos--;
            }
            if (pos == 0) break;
            if (i < lines - 1) {
                pos--; // Move before newline to continue to previous line
            }
        }
    }
    
    // Now find beginning of the line we're on
    while (pos > 0 && rope_char_at(current_buffer->rope, pos - 1) != '\n') {
        pos--;
    }
    
    return pos;
}

size_t line_end_position(int n) {
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t pos = current_buffer->pt;
    
    if (n > 0) {
        // Move forward n-1 lines, then to end of that line
        for (int i = 0; i < n - 1; i++) {
            while (pos < text_len && rope_char_at(current_buffer->rope, pos) != '\n') {
                pos++;
            }
            if (pos >= text_len) break;
            pos++; // Move past newline
        }
    } else if (n < 0) {
        // Move backward |n|+1 lines, then to end of that line
        int lines = abs(n) + 1;
        for (int i = 0; i < lines; i++) {
            // Go to beginning of current line
            while (pos > 0 && rope_char_at(current_buffer->rope, pos - 1) != '\n') {
                pos--;
            }
            if (pos == 0) break;
            if (i < lines - 1) {
                pos--; // Move before newline to continue to previous line
            }
        }
    }
    
    // Now find end of the line we're on
    while (pos < text_len && rope_char_at(current_buffer->rope, pos) != '\n') {
        pos++;
    }
    
    return pos;
}

size_t current_column() {
    size_t line_start = line_beginning_position(1);
    return current_buffer->pt - line_start;
}

static size_t get_effective_goal_column() {
    // First check if goal-column is set (semipermanent)
    SCM goal_col_sym = scm_from_utf8_symbol("goal-column");
    SCM goal_col_val = buffer_local_value(goal_col_sym, current_buffer);
    
    if (scm_is_true(goal_col_val) && scm_is_integer(goal_col_val)) {
        return scm_to_size_t(goal_col_val);
    }
    
    // Fall back to temporary-goal-column
    SCM temp_sym = scm_from_utf8_symbol("temporary-goal-column");
    SCM temp_val = buffer_local_value(temp_sym, current_buffer);
    if (scm_is_integer(temp_val)) {
        return scm_to_size_t(temp_val);
    }
    
    return 0;
}

static void set_temporary_goal_column(size_t column) {
    SCM sym = scm_from_utf8_symbol("temporary-goal-column");
    SCM val = scm_from_size_t(column);
    scm_setq_impl(sym, val);
}

void update_goal_column() {
    set_temporary_goal_column(current_column());
}


// MAYBE TODO in emacs it check if `goal-column' is set to decide whether to use
// visual movement, i don't think it makes much sense to disable visual movement
// if `goal-column' is set, I'll not do it for now..
void line_move() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    // Check if goal-column is set (overrides line-move-visual)
    SCM goal_col_sym = scm_from_utf8_symbol("goal-column");
    SCM goal_col_val = buffer_local_value(goal_col_sym, current_buffer);
    bool has_goal_column = scm_is_true(goal_col_val);
    
    // Check if we should use visual line movement
    bool line_move_visual_enabled = scm_get_bool("line-move-visual", true);
    
    // Check truncate-lines using buffer-local value
    SCM truncate_lines_sym = scm_from_utf8_symbol("truncate-lines");
    SCM truncate_lines_val = buffer_local_value(truncate_lines_sym, current_buffer);
    bool truncate_lines = scm_is_true(truncate_lines_val);
    
    // Determine if we should use visual movement
    // goal-column overrides line-move-visual
    bool use_visual = !has_goal_column && 
                     line_move_visual_enabled && 
                     !truncate_lines;
    
    if (use_visual) {
        line_move_visual();
    } else {
        line_move_logical();
    }
}

void line_move_logical() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t starting_pt = current_buffer->pt;
    
    for (int i = 0; i < count; i++) {
        if (direction > 0) {
            // Move down
            size_t pos = current_buffer->pt;
            while (pos < text_len && rope_char_at(current_buffer->rope, pos) != '\n') {
                pos++;
            }
            
            if (pos >= text_len) {
                // Can't move any further - we're at the last line
                bool add_newlines = scm_get_bool("next-line-add-newlines", false);
                
                if (add_newlines) {
                    set_point(text_len);
                    insert("\n");
                    text_len++;
                } else {
                    end_of_buffer();
                    message("End of buffer");
                }
                return;
            }
            
            pos++; // Move past newline
            
            size_t line_start = pos;
            size_t line_end = pos;
            while (line_end < text_len && rope_char_at(current_buffer->rope, line_end) != '\n') {
                line_end++;
            }
            
            size_t line_length = line_end - line_start;
            if (get_effective_goal_column() <= line_length) {
                set_point(line_start + get_effective_goal_column());
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
                if (current_buffer->pt == starting_pt) {
                    message("Beginning of buffer");
                }
                return;
            }
            
            pos--; // Move before newline
            
            size_t line_start = pos;
            while (line_start > 0 && rope_char_at(current_buffer->rope, line_start - 1) != '\n') {
                line_start--;
            }
            
            size_t line_length = pos - line_start;
            if (get_effective_goal_column() <= line_length) {
                set_point(line_start + get_effective_goal_column());
            } else {
                set_point(pos);
            }
        }
    }
}

// TODO we should check tab-width to properly do it,
// it's wrong when there are tabs in the buffer.
void line_move_visual() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    Window *win = selected_frame->wm.selected;
    if (!win) {
        line_move_logical();
        return;
    }
    
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    if (!default_font) {
        line_move_logical();
        return;
    }
    
    float start_x = win->x + selected_frame->left_fringe_width;
    float max_x = start_x + (win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width));
    
    int direction = arg > 0 ? 1 : -1;
    int count = abs(arg);
    
    size_t text_len = rope_char_length(current_buffer->rope);
    size_t starting_pt = current_buffer->pt;
    
    // Get the goal column to maintain across movements
    size_t goal_column = get_effective_goal_column();
    
    for (int iter = 0; iter < count; iter++) {
        size_t pos = current_buffer->pt;
        
        // Find start of current logical line
        size_t logical_line_start = pos;
        while (logical_line_start > 0 && rope_char_at(current_buffer->rope, logical_line_start - 1) != '\n') {
            logical_line_start--;
        }
        
        // Find end of current logical line
        size_t logical_line_end = pos;
        while (logical_line_end < text_len && rope_char_at(current_buffer->rope, logical_line_end) != '\n') {
            logical_line_end++;
        }
        
        // Build array of all visual line starts in current logical line
        size_t visual_starts[1024];
        int num_visual_lines = 0;
        visual_starts[num_visual_lines++] = logical_line_start;
        
        float x = start_x;
        size_t i = logical_line_start;
        
        while (i < logical_line_end) {
            uint32_t ch = rope_char_at(current_buffer->rope, i);
            
            int face_id = get_text_property_face(current_buffer, i);
            Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
            if (!face) face = default_face;
            Font *char_font = get_face_font(face);
            if (!char_font) char_font = default_font;
            
            float char_width = character_width(char_font, ch);
            
            if (x + char_width > max_x) {
                // Wrap point
                if (num_visual_lines < 1024) {
                    visual_starts[num_visual_lines++] = i;
                }
                x = start_x;
            }
            
            x += char_width;
            i++;
        }
        
        // Find which visual line contains pos
        int current_visual_line_idx = 0;
        for (int j = 0; j < num_visual_lines; j++) {
            if (visual_starts[j] <= pos) {
                current_visual_line_idx = j;
            } else {
                break;
            }
        }
        
        // Calculate goal_x from goal_column
        float goal_x = start_x;
        x = start_x;
        size_t chars_from_logical_start = 0;
        i = logical_line_start;
        
        while (i < logical_line_end && chars_from_logical_start < goal_column) {
            uint32_t ch = rope_char_at(current_buffer->rope, i);
            
            int face_id = get_text_property_face(current_buffer, i);
            Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
            if (!face) face = default_face;
            Font *char_font = get_face_font(face);
            if (!char_font) char_font = default_font;
            
            float char_width = character_width(char_font, ch);
            
            if (x + char_width > max_x) {
                x = start_x + char_width;
            } else {
                x += char_width;
            }
            
            i++;
            chars_from_logical_start++;
        }
        
        goal_x = x;
        
        if (direction > 0) {
            // Move down one visual line
            int target_visual_line_idx = current_visual_line_idx + 1;
            
            // Check if target is in the same logical line
            if (target_visual_line_idx < num_visual_lines) {
                // Target is in same logical line - use goal_x
                size_t target_visual_start = visual_starts[target_visual_line_idx];
                size_t target_visual_end;
                
                if (target_visual_line_idx + 1 < num_visual_lines) {
                    target_visual_end = visual_starts[target_visual_line_idx + 1];
                } else {
                    target_visual_end = logical_line_end;
                }
                
                // Find position at goal_x in target visual line
                x = start_x;
                size_t target_pos = target_visual_start;
                
                while (target_pos < target_visual_end) {
                    uint32_t ch = rope_char_at(current_buffer->rope, target_pos);
                    
                    int face_id = get_text_property_face(current_buffer, target_pos);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        break;
                    }
                    
                    if (x >= goal_x) {
                        break;
                    }
                    
                    x += char_width;
                    target_pos++;
                }
                
                set_point(target_pos);
            } else {
                // Need to move to next logical line
                if (logical_line_end >= text_len) {
                    // At end of buffer
                    bool add_newlines = scm_get_bool("next-line-add-newlines", false);
                    
                    if (add_newlines) {
                        set_point(text_len);
                        insert("\n");
                    } else {
                        end_of_buffer();
                        message("End of buffer");
                    }
                    return;
                }
                
                // Move to first visual line of next logical line
                size_t next_logical_start = logical_line_end + 1;
                size_t next_logical_end = next_logical_start;
                
                while (next_logical_end < text_len && rope_char_at(current_buffer->rope, next_logical_end) != '\n') {
                    next_logical_end++;
                }
                
                // Calculate goal_x from goal_column for the next logical line
                x = start_x;
                size_t chars_from_next_start = 0;
                i = next_logical_start;
                float next_goal_x = start_x;
                
                while (i < next_logical_end && chars_from_next_start < goal_column) {
                    uint32_t ch = rope_char_at(current_buffer->rope, i);
                    
                    int face_id = get_text_property_face(current_buffer, i);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        x = start_x + char_width;
                    } else {
                        x += char_width;
                    }
                    
                    i++;
                    chars_from_next_start++;
                }
                next_goal_x = x;
                
                // Find first wrap point in next logical line
                x = start_x;
                i = next_logical_start;
                size_t first_wrap = next_logical_end;
                
                while (i < next_logical_end) {
                    uint32_t ch = rope_char_at(current_buffer->rope, i);
                    
                    int face_id = get_text_property_face(current_buffer, i);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        first_wrap = i;
                        break;
                    }
                    
                    x += char_width;
                    i++;
                }
                
                // Find position at next_goal_x in first visual line
                x = start_x;
                size_t target_pos = next_logical_start;
                
                while (target_pos < first_wrap) {
                    uint32_t ch = rope_char_at(current_buffer->rope, target_pos);
                    
                    int face_id = get_text_property_face(current_buffer, target_pos);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        break;
                    }
                    
                    if (x >= next_goal_x) {
                        break;
                    }
                    
                    x += char_width;
                    target_pos++;
                }
                
                set_point(target_pos);
            }
        } else {
            // Move up one visual line
            if (pos == 0) {
                message("Beginning of buffer");
                return;
            }
            
            int target_visual_line_idx = current_visual_line_idx - 1;
            
            // Check if target is in the same logical line
            if (target_visual_line_idx >= 0) {
                // Target is in same logical line - use goal_x
                size_t target_visual_start = visual_starts[target_visual_line_idx];
                size_t target_visual_end;
                
                if (target_visual_line_idx + 1 < num_visual_lines) {
                    target_visual_end = visual_starts[target_visual_line_idx + 1];
                } else {
                    target_visual_end = logical_line_end;
                }
                
                // Find position at goal_x in target visual line
                x = start_x;
                size_t target_pos = target_visual_start;
                
                while (target_pos < target_visual_end) {
                    uint32_t ch = rope_char_at(current_buffer->rope, target_pos);
                    
                    int face_id = get_text_property_face(current_buffer, i);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x >= goal_x) {
                        break;
                    }
                    
                    x += char_width;
                    target_pos++;
                }
                
                set_point(target_pos);
            } else {
                // Need to move to previous logical line
                if (logical_line_start == 0) {
                    message("Beginning of buffer");
                    return;
                }
                
                // Find previous logical line
                size_t prev_logical_end = logical_line_start - 1;
                size_t prev_logical_start = prev_logical_end;
                
                while (prev_logical_start > 0 && rope_char_at(current_buffer->rope, prev_logical_start - 1) != '\n') {
                    prev_logical_start--;
                }
                
                // Build visual lines array for previous logical line
                num_visual_lines = 0;
                visual_starts[num_visual_lines++] = prev_logical_start;
                
                x = start_x;
                i = prev_logical_start;
                
                while (i < prev_logical_end) {
                    uint32_t ch = rope_char_at(current_buffer->rope, i);
                    
                    int face_id = get_text_property_face(current_buffer, i);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        if (num_visual_lines < 1024) {
                            visual_starts[num_visual_lines++] = i;
                        }
                        x = start_x;
                    }
                    
                    x += char_width;
                    i++;
                }
                
                // Move to last visual line of previous logical line
                size_t target_visual_start = visual_starts[num_visual_lines - 1];
                size_t target_visual_end = prev_logical_end;
                
                // Calculate goal_x from goal_column for the previous logical line
                x = start_x;
                size_t chars_from_prev_start = 0;
                i = prev_logical_start;
                float prev_goal_x = start_x;
                
                while (i < prev_logical_end && chars_from_prev_start < goal_column) {
                    uint32_t ch = rope_char_at(current_buffer->rope, i);
                    
                    int face_id = get_text_property_face(current_buffer, i);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        x = start_x + char_width;
                    } else {
                        x += char_width;
                    }
                    
                    i++;
                    chars_from_prev_start++;
                }
                prev_goal_x = x;
                
                // Now find position at prev_goal_x in the last visual line
                x = start_x;
                size_t target_pos = target_visual_start;
                
                while (target_pos < target_visual_end) {
                    uint32_t ch = rope_char_at(current_buffer->rope, target_pos);
                    
                    int face_id = get_text_property_face(current_buffer, target_pos);
                    Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
                    if (!face) face = default_face;
                    Font *char_font = get_face_font(face);
                    if (!char_font) char_font = default_font;
                    
                    float char_width = character_width(char_font, ch);
                    
                    if (x + char_width > max_x) {
                        break;
                    }
                    
                    if (x >= prev_goal_x) {
                        break;
                    }
                    
                    x += char_width;
                    target_pos++;
                }
                
                set_point(target_pos);
            }
        }
    }
}

void next_line() {
    line_move();
}

void previous_line() {
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    line_move();
}

void next_logical_line() {
    line_move_logical();
}

void previous_logical_line() {
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    line_move_logical();
}

void beginning_of_line() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    size_t orig = current_buffer->pt;
    size_t target = line_beginning_position(arg);
    
    // Constrain to field boundaries (only for beginning-of-line)
    target = constrain_to_field(target, orig);
    
    set_point(target);
}

void end_of_line() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    // end-of-line doesn't respect field property
    size_t target = line_end_position(arg);
    set_point(target);
}

void beginning_of_visual_line() {
    size_t pos = current_buffer->pt;
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Find start of logical line
    size_t logical_start = pos;
    while (logical_start > 0 && rope_char_at(current_buffer->rope, logical_start - 1) != '\n') {
        logical_start--;
    }
    
    // Find end of logical line
    size_t logical_end = pos;
    while (logical_end < text_len && rope_char_at(current_buffer->rope, logical_end) != '\n') {
        logical_end++;
    }
    
    Window *win = selected_frame->wm.selected;
    float start_x = win->x + selected_frame->left_fringe_width;
    float max_x = start_x + (win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width));
    float char_width = selected_frame->column_width;
    
    // Find all visual line wrap points
    size_t visual_starts[1024];
    int num_visual = 0;
    visual_starts[num_visual++] = logical_start;
    
    float x = start_x;
    for (size_t i = logical_start; i < logical_end; i++) {
        if (x + char_width > max_x) {
            visual_starts[num_visual++] = i;
            x = start_x;
        }
        x += char_width;
    }
    
    // Find which visual line contains pos
    size_t visual_start = logical_start;
    for (int j = num_visual - 1; j >= 0; j--) {
        if (visual_starts[j] <= pos) {
            visual_start = visual_starts[j];
            break;
        }
    }
    
    set_point(visual_start);
}

void end_of_visual_line() {
    size_t pos = current_buffer->pt;
    size_t text_len = rope_char_length(current_buffer->rope);
    
    // Find start of logical line
    size_t logical_start = pos;
    while (logical_start > 0 && rope_char_at(current_buffer->rope, logical_start - 1) != '\n') {
        logical_start--;
    }
    
    // Find end of logical line
    size_t logical_end = pos;
    while (logical_end < text_len && rope_char_at(current_buffer->rope, logical_end) != '\n') {
        logical_end++;
    }
    
    Window *win = selected_frame->wm.selected;
    float start_x = win->x + selected_frame->left_fringe_width;
    float max_x = start_x + (win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width));
    float char_width = selected_frame->column_width;
    
    // Find all visual line wrap points
    size_t visual_starts[1024];
    int num_visual = 0;
    visual_starts[num_visual++] = logical_start;
    
    float x = start_x;
    for (size_t i = logical_start; i < logical_end; i++) {
        if (x + char_width > max_x) {
            visual_starts[num_visual++] = i;
            x = start_x;
        }
        x += char_width;
    }
    
    // Find which visual line contains pos
    int visual_idx = 0;
    for (int j = 0; j < num_visual; j++) {
        if (visual_starts[j] <= pos) {
            visual_idx = j;
        } else {
            break;
        }
    }
    
    // End of this visual line is one before the next visual line start (or logical end)
    size_t visual_end;
    if (visual_idx + 1 < num_visual) {
        visual_end = visual_starts[visual_idx + 1] - 1;
    } else {
        visual_end = logical_end;
    }
    
    set_point(visual_end);
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
    
    size_t line_start = line_beginning_position(1);
    size_t line_end = line_end_position(1);
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
        insert("\n");
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
        size_t first_line_start = line_beginning_position(1);
        
        // Find the end of the last line in region
        size_t saved_pt = current_buffer->pt;
        set_point(end);
        size_t last_line_end = line_end_position(1);
        set_point(saved_pt);
        
        // Move to beginning of first line
        set_point(first_line_start);
        
        // Keep joining lines until we've covered the region
        while (current_buffer->pt < last_line_end) {
            size_t line_end = line_end_position(1);
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
                insert(" ");
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
        size_t line_end = line_end_position(1);
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
        size_t line_start = line_beginning_position(1);
        
        // Check if we're at first line
        if (line_start == 0) {
            return;
        }
        
        // Check if current line is empty (only whitespace)
        size_t line_end = line_end_position(1);
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
        insert(" ");
    }
}

/// REGION

bool transient_mark_mode_should_be_disabled = false;

void set_mark_command() {
    current_buffer->region.mark = current_buffer->pt;
    bool transient_mark_mode = scm_get_bool("transient-mark-mode", false);
    message("Mark set");
    if (transient_mark_mode) {
        current_buffer->region.active = true;
    } else if (is_scm_proc(last_command, "set-mark-command")) {
        activate_mark();
    }
}

void set_mark(size_t pos) {
    current_buffer->region.mark = pos;
    bool transient_mark_mode = scm_get_bool("transient-mark-mode", false);
    if (transient_mark_mode) activate_mark();
}

void activate_mark() {
    bool transient_mark_mode = scm_get_bool("transient-mark-mode", false);
    if (!transient_mark_mode) {
        transient_mark_mode_should_be_disabled = true;
        SCM var = scm_c_lookup("transient-mark-mode");
        scm_variable_set_x(var, scm_from_bool(true));
    }
    current_buffer->region.active = true;
    message("Mark activated");
}

void deactivate_mark() {
    if (transient_mark_mode_should_be_disabled) {
        SCM var = scm_c_lookup("transient-mark-mode");
        scm_variable_set_x(var, scm_from_bool(false));
        transient_mark_mode_should_be_disabled = false;
    }
    current_buffer->region.active = false;
}

void exchange_point_and_mark() {
    if (current_buffer->region.mark < 0) {
        message("No mark set in this buffer");
        return;
    }
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
    if (current_buffer->region.mark < 0) {
        message("The mark is not set now, so there is no region");
        return;
    }
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

void copy_region_as_kill() {
    if (current_buffer->region.mark < 0) {
        message("The mark is not set now, so there is no region");
        return;
    }
    
    size_t start, end;
    region_bounds(&start, &end);
    
    if (start == end) {
        current_buffer->region.active = false;
        return;
    }
    
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
            if (combined) {
                snprintf(combined, total_len, "%s%s", existing, new_text);
                setClipboardString(combined);
                free(combined);
            }
        } else {
            setClipboardString(new_text);
        }
    } else {
        setClipboardString(new_text);
    }
    
    free(new_text);
    deactivate_mark();
}

void rkill_impl(size_t start, size_t end, bool prepend) {
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
    
    // When delete hits read-only, it will longjmp to delete_readonly
    // which is set up in the delete macro below
    if (setjmp(delete_readonly) != 0) {
        // Delete failed, propagate up to kill_readonly
        longjmp(kill_readonly, 1);
    }
    delete_impl(start, length);
}

// Macro that sets up setjmp for kill operations
#define rkill(start, end, prepend) \
    do { \
        if (setjmp(kill_readonly) != 0) { \
            return; \
        } \
        rkill_impl((start), (end), (prepend)); \
    } while (0)


void kill_line() {
    size_t text_len = rope_char_length(current_buffer->rope);
    
    int arg = get_prefix_arg();
    if (arg == 0) {
        // Kill backwards from point to beginning of line
        size_t line_start = line_beginning_position(1);
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
        size_t line_end = line_end_position(1);
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
        if (kill_whole_line && current_buffer->pt == line_beginning_position(1)) {
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
    if (current_buffer->region.mark < 0) {
        message("The mark is not set now, so there is no region");
    }
    size_t start, end;
    region_bounds(&start, &end);
    
    if (start == end) {
        current_buffer->region.active = false;
        return;
    }
    
    rkill(start, end, false);
    set_point(start);

    deactivate_mark();
}

// TODO Support ARG to yank N times
// (Who even thinks let me yank the think i've killed 4 kills before)
// TODO Don’t adjust the syntax if treesit
// TODO parse if treesit

void yank() {
    const char *clipboard_text = getClipboardString();
    
    if (!clipboard_text || !*clipboard_text) {
        message("Kill ring is empty");
        return;
    }
    
    size_t len = strlen(clipboard_text);
    if (len == 0) return;
    
    // Check buffer read-only
    if (current_buffer->read_only) {
        message("Buffer is read-only: #<buffer %s>", current_buffer->name);
        return; 
    }
    
    size_t insert_pos = current_buffer->pt;
    
    // Check if we're trying to insert at a read-only position
    SCM readonly = get_text_property(current_buffer, insert_pos, scm_from_locale_symbol("read-only"));
    if (scm_is_true(readonly)) {
        message("Text is read-only");
        return;
    }
    
    // Get prefix argument (how many times to yank)
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();
    
    // If raw prefix (C-u without number), yank once and exchange point/mark
    // If numeric arg, yank that many times
    int yank_count = 1;
    if (!raw_prefix_arg && arg != 0) {
        yank_count = abs(arg);
    }
    
    // Calculate how many characters (not bytes) we're inserting per yank
    size_t char_count = 0;
    for (size_t i = 0; i < len; ) {
        size_t bytes_read;
        uint32_t ch = utf8_decode(&clipboard_text[i], len - i, &bytes_read);
        if (bytes_read == 0) break;
        i += bytes_read;
        char_count++;
    }
    
    size_t total_char_count = char_count * yank_count;
    
    // Check if tree-sitter is active
    bool has_treesit = current_buffer->ts_state && current_buffer->ts_state->tree;
    
    // Capture tree-sitter state BEFORE modification
    size_t start_byte = 0;
    TSPoint start_point = {0, 0};
    TSPoint new_end_point = {0, 0};
    
    if (has_treesit) {
        start_byte = rope_char_to_byte(current_buffer->rope, insert_pos);
        start_point = treesit_char_to_point(current_buffer, insert_pos);
        
        // Calculate new_end_point based on inserted content (repeated yank_count times)
        new_end_point = start_point;
        
        for (int repeat = 0; repeat < yank_count; repeat++) {
            // Scan inserted bytes for newlines
            size_t bytes_after_last_newline = 0;
            for (size_t i = 0; i < len; i++) {
                if (clipboard_text[i] == '\n') {
                    new_end_point.row++;
                    new_end_point.column = 0;
                    bytes_after_last_newline = 0;
                } else {
                    bytes_after_last_newline++;
                }
            }
            
            // Update column based on whether we had newlines
            if (bytes_after_last_newline > 0 || new_end_point.row == start_point.row) {
                new_end_point.column += bytes_after_last_newline;
            }
        }
    }
    
    // Set mark at beginning of yank
    current_buffer->region.mark = current_buffer->pt;
    
    // Perform the rope insertion (yank_count times)
    for (int i = 0; i < yank_count; i++) {
        current_buffer->rope = rope_insert_chars(
            current_buffer->rope,
            insert_pos + (i * char_count),
            clipboard_text,
            len
        );
    }
    
    // Update tree-sitter if active
    if (has_treesit) {
        size_t new_end_byte = start_byte + (len * yank_count);
        
        treesit_update_tree(
            current_buffer,
            start_byte,
            start_byte,      // old_end_byte = start_byte (nothing was there before)
            new_end_byte,
            start_point,
            start_point,     // old_end_point = start_point (nothing was there before)
            new_end_point
        );
        
        treesit_reparse_if_needed(current_buffer);
        treesit_apply_highlights(current_buffer);
    }
    
    // Only adjust text properties if tree-sitter is NOT active
    if (!has_treesit) {
        adjust_text_properties(current_buffer, insert_pos, total_char_count);
    }
    
    adjust_all_window_points_after_modification(insert_pos, total_char_count);
    set_point(current_buffer->pt + total_char_count); // End of inserted text
    
    // If C-u was used (raw prefix arg), exchange point and mark
    // This leaves point at beginning and mark at end of yanked text
    if (raw_prefix_arg) {
        exchange_point_and_mark();
    }
    
    update_goal_column();
    reset_cursor_blink(current_buffer);
    current_buffer->modified = true;
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
// later when we have treesitter, Emacs determines this based on the syntax table
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

// TODO It behaves bad with arg
// TODO It's wrong with complex lists
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

// TODO make sure it works well when `transient-mark-mode' is enabled
void mark_sexp() {
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();
    
    // Check if we should extend: region active AND last command was mark-sexp
    bool allow_extend = current_buffer->region.active && is_scm_proc(last_command, "mark-sexp");
    
    if (allow_extend) {
        // Continuing from previous mark-sexp - extend the region
        
        // If no explicit arg given, decide direction based on mark position
        if (!argument_manually_set && !raw_prefix_arg) {
            // Move mark 1 sexp away from point
            if (current_buffer->region.mark < current_buffer->pt) {
                arg = -1;  // Mark is before point, move backward
            } else {
                arg = 1;   // Mark is at/after point, move forward
            }
        }
        
        // Save current point
        size_t saved_point = current_buffer->pt;
        
        // Move to mark position
        set_point(current_buffer->region.mark);
        
        // Scan from mark
        size_t new_mark = scan_sexps(current_buffer, current_buffer->pt, arg);
        
        if (new_mark == SIZE_MAX) {
            // Restore point and show error
            set_point(saved_point);
            message("No more sexp to select");
            return;
        }
        
        // Update mark to new position
        current_buffer->region.mark = new_mark;
        
        // Restore point
        set_point(saved_point);
        
    } else {
        // Starting fresh - set mark from point
        
        // Default to 1 if no arg given
        if (!argument_manually_set && !raw_prefix_arg) {
            arg = 1;
        }
        
        // Save current point
        size_t saved_point = current_buffer->pt;
        
        // Scan from point
        size_t new_mark = scan_sexps(current_buffer, current_buffer->pt, arg);
        
        if (new_mark == SIZE_MAX) {
            message("No sexp to select");
            return;
        }
        
        // Set mark at the new position
        current_buffer->region.mark = new_mark;
        current_buffer->region.active = true;
        
        message("Mark set");
    }
}


/// DEFUN

// Helper to check if a node is a function definition
static bool is_function_definition(TSNode node) {
    const char *type = ts_node_type(node);
    return strcmp(type, "function_definition") == 0;
}

// Find the innermost function definition containing point
static TSNode find_containing_function(TSNode root, size_t byte_pos) {
    TSNode result = {0};
    
    uint32_t child_count = ts_node_child_count(root);
    for (uint32_t i = 0; i < child_count; i++) {
        TSNode child = ts_node_child(root, i);
        
        uint32_t start_byte = ts_node_start_byte(child);
        uint32_t end_byte = ts_node_end_byte(child);
        
        // Skip nodes that don't contain point
        if (byte_pos < start_byte || byte_pos > end_byte) {
            continue;
        }
        
        // If this is a function definition, it's a candidate
        if (is_function_definition(child)) {
            result = child;
        }
        
        // Recurse to find innermost function
        TSNode nested = find_containing_function(child, byte_pos);
        if (!ts_node_is_null(nested)) {
            result = nested;
        }
    }
    
    return result;
}

// Find the next function definition after byte_pos
static TSNode find_next_function(TSNode root, size_t byte_pos) {
    uint32_t child_count = ts_node_child_count(root);
    
    for (uint32_t i = 0; i < child_count; i++) {
        TSNode child = ts_node_child(root, i);
        uint32_t start_byte = ts_node_start_byte(child);
        
        if (start_byte > byte_pos) {
            if (is_function_definition(child)) {
                return child;
            }
            
            TSNode nested = find_next_function(child, byte_pos);
            if (!ts_node_is_null(nested)) {
                return nested;
            }
        } else {
            TSNode nested = find_next_function(child, byte_pos);
            if (!ts_node_is_null(nested)) {
                return nested;
            }
        }
    }
    
    TSNode null_node = {0};
    return null_node;
}

// Find the previous function definition before byte_pos
static TSNode find_prev_function(TSNode root, size_t byte_pos) {
    TSNode result = {0};
    uint32_t child_count = ts_node_child_count(root);
    
    for (uint32_t i = 0; i < child_count; i++) {
        TSNode child = ts_node_child(root, i);
        uint32_t start_byte = ts_node_start_byte(child);
        
        if (start_byte < byte_pos) {
            if (is_function_definition(child)) {
                result = child;
            }
            
            TSNode nested = find_prev_function(child, byte_pos);
            if (!ts_node_is_null(nested)) {
                result = nested;
            }
        }
    }
    
    return result;
}

void beginning_of_defun() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    if (!current_buffer->ts_state || !current_buffer->ts_state->tree) {
        message("Tree-sitter not available");
        return;
    }
    
    TSNode root = ts_tree_root_node(current_buffer->ts_state->tree);
    size_t byte_pos = treesit_point_to_byte(current_buffer, current_buffer->pt);
    int count = abs(arg);
    
    if (arg > 0) {
        // Move backward to previous function definitions
        for (int i = 0; i < count; i++) {
            TSNode current_func = find_containing_function(root, byte_pos);
            
            if (!ts_node_is_null(current_func)) {
                uint32_t func_start = ts_node_start_byte(current_func);
                
                if (byte_pos > func_start) {
                    byte_pos = func_start;
                    continue;
                }
            }
            
            TSNode prev_func = find_prev_function(root, byte_pos);
            if (ts_node_is_null(prev_func)) {
                set_point(0);
                return;
            }
            
            byte_pos = ts_node_start_byte(prev_func);
        }
        
        size_t char_pos = treesit_byte_to_point(current_buffer, byte_pos);
        set_point(char_pos);
        
    } else {
        // Move forward to next function definitions
        for (int i = 0; i < count; i++) {
            TSNode next_func = find_next_function(root, byte_pos);
            if (ts_node_is_null(next_func)) {
                size_t text_len = rope_char_length(current_buffer->rope);
                set_point(text_len);
                return;
            }
            
            byte_pos = ts_node_start_byte(next_func);
        }
        
        size_t char_pos = treesit_byte_to_point(current_buffer, byte_pos);
        set_point(char_pos);
    }
}

void end_of_defun() {
    int arg = get_prefix_arg();
    if (arg == 0) return;
    
    if (!current_buffer->ts_state || !current_buffer->ts_state->tree) {
        message("Tree-sitter not available");
        return;
    }
    
    TSNode root = ts_tree_root_node(current_buffer->ts_state->tree);
    size_t byte_pos = treesit_point_to_byte(current_buffer, current_buffer->pt);
    int count = abs(arg);
    
    if (arg > 0) {
        // Move forward to end of function definitions
        for (int i = 0; i < count; i++) {
            TSNode current_func = find_containing_function(root, byte_pos);
            
            if (!ts_node_is_null(current_func)) {
                uint32_t func_end = ts_node_end_byte(current_func);
                
                if (byte_pos < func_end) {
                    byte_pos = func_end;
                    continue;
                }
            }
            
            TSNode next_func = find_next_function(root, byte_pos);
            if (ts_node_is_null(next_func)) {
                size_t text_len = rope_char_length(current_buffer->rope);
                set_point(text_len);
                return;
            }
            
            byte_pos = ts_node_end_byte(next_func);
        }
        
        size_t char_pos = treesit_byte_to_point(current_buffer, byte_pos);
        // Save current position
        size_t saved_pt = current_buffer->pt;
        // Temporarily move point to the end of function
        current_buffer->pt = char_pos;
        // Get end of line from that position
        size_t line_end = line_end_position(1);
        // Move to start of next line
        if (line_end < rope_char_length(current_buffer->rope)) {
            char_pos = line_end + 1;
        }
        // Restore original point and set to new position
        current_buffer->pt = saved_pt;
        set_point(char_pos);
        
    } else {
        // Move backward to end of previous function definitions
        for (int i = 0; i < count; i++) {
            TSNode prev_func = find_prev_function(root, byte_pos);
            if (ts_node_is_null(prev_func)) {
                set_point(0);
                return;
            }
            
            byte_pos = ts_node_end_byte(prev_func);
        }
        
        size_t char_pos = treesit_byte_to_point(current_buffer, byte_pos);
        // Save current position
        size_t saved_pt = current_buffer->pt;
        // Temporarily move point to the end of function
        current_buffer->pt = char_pos;
        // Get end of line from that position
        size_t line_end = line_end_position(1);
        // Move to start of next line
        if (line_end < rope_char_length(current_buffer->rope)) {
            char_pos = line_end + 1;
        }
        // Restore original point and set to new position
        current_buffer->pt = saved_pt;
        set_point(char_pos);
    }
}

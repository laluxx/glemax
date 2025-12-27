#include <libguile.h>
#include <cglm/types.h>
#include <obsidian/context.h>
#include <obsidian/font.h>
#include <obsidian/input.h>
#include <obsidian/obsidian.h>
#include <obsidian/renderer.h>
#include <obsidian/window.h>
#include "buffer.h"
#include "faces.h"
#include "wm.h"
#include "lisp.h"
#include "edit.h"
#include "minibuf.h"
#include "frame.h"

#define ROPE_IMPLEMENTATION
#include "rope.h"

uint32_t sw = 500;
uint32_t sh = 500;



// Helper function to check if there are unmatched closing characters of a specific type after point
// Uses scan_lists for efficient balance checking
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

void text_callback(unsigned int codepoint) {
    clear_minibuffer();
    
    // Encode the codepoint to UTF-8
    char utf8_buf[5] = {0};  // Max 4 bytes + null terminator
    size_t len = utf8_encode(codepoint, utf8_buf);
    
    bool electric_pair_mode = scm_get_bool("electric-pair-mode", false);
    
    // Handle electric pairing
    if (electric_pair_mode && len == 1) {  // Only for single-byte characters
        char ch = utf8_buf[0];
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
            
            // Check if typed character is a closing character
            case ')':
            case ']':
            case '}':
            case '>':
                is_closing = true;
                break;
        }
        
        // If we typed a closing character, check if it matches the next character
        if (is_closing) {
            // Get the character at current point
            size_t buf_len = rope_char_length(current_buffer->rope);
            if (current_buffer->pt < buf_len) {
                uint32_t next_char = rope_char_at(current_buffer->rope, current_buffer->pt);
                
                // If the next character matches what we typed, just skip over it
                if (next_char == (uint32_t)ch) {
                    set_point(current_buffer->pt + 1);
                    update_windows_scroll();
                    
                    // Only hide if the feature is enabled and pointer is currently visible
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
            // Check if there are unmatched closing characters ahead
            bool has_unmatched = has_unmatched_closing_after(current_buffer, current_buffer->pt, ch, closing_char);
            
            if (has_unmatched) {
                // There's an unmatched closing char, so just insert the opening char
                insert(utf8_buf);
                update_windows_scroll();
                
                // Only hide if the feature is enabled and pointer is currently visible
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
            update_windows_scroll();
            
            // Only hide if the feature is enabled and pointer is currently visible
            if (scm_get_bool("make-pointer-invisible", true) &&
                scm_get_bool("pointer-visible", true)) {
                hideCursor();
                scm_c_define("pointer-visible", SCM_BOOL_F);
            }
            
            return;
        }
    }
    
    // Normal insertion (no pairing)
    insert(utf8_buf);
    update_windows_scroll();
    
    // Only hide if the feature is enabled and pointer is currently visible
    if (scm_get_bool("make-pointer-invisible", true) &&
        scm_get_bool("pointer-visible", true)) {
        hideCursor();
        scm_c_define("pointer-visible", SCM_BOOL_F);
    }
}


bool is_vertical_motion(SCM proc) {
    return is_scm_proc(proc, "next-line") ||
        is_scm_proc(proc, "previous-line");
}

bool is_argument_function(SCM proc) {
    return is_scm_proc(proc, "universal-argument") ||
        is_scm_proc(proc, "negative-argument") ||
        is_scm_proc(proc, "digit-argument");
}


void before_keychord_hook(const char *notation, KeyChordBinding *binding) {
    clear_minibuffer();
    if (scm_get_bool("make-pointer-invisible-on-keychords", true) && 
        scm_get_bool("pointer-visible", true)) {
        hideCursor();
        scm_c_define("pointer-visible", SCM_BOOL_F);
    }

}

void after_keychord_hook(const char *notation, KeyChordBinding *binding) {
    reset_cursor_blink(current_buffer);
    update_windows_scroll();
    int arg = get_prefix_arg();
    
    
    
    if (!is_scm_proc(binding->action.scheme_proc, "recenter-top-bottom") &&
        !is_scm_proc(binding->action.scheme_proc, "move-to-window-line-top-bottom"))
        recenter_positions = 0;
    
    // Handle digit argument
    if (is_scm_proc(binding->action.scheme_proc, "digit-argument")) {
        argument_manually_set = true;
        bool was_negative = false;
        
        // Remember current sign
        was_negative = (arg < 0);
        
        // If the last command was negative_argument and arg is still -1 or 1,
        // reset to 0 but preserve the sign (we're starting fresh)
        if (is_scm_proc(last_command, "negative-argument") && (arg == -1 || arg == 1)) {
            arg = 0;
            set_prefix_arg(arg);
        }
        
        // If the last command wasn't a digit or negative argument, reset completely
        else if (!is_scm_proc(last_command, "digit-argument") && 
                 !is_scm_proc(last_command, "negative-argument")) {
            arg = 0;
            set_prefix_arg(arg);
            was_negative = false;
        }
        
        
        if (was_negative) arg = -arg;  // Make it positive temporarily
        set_prefix_arg(arg);
        
        // Extract the digit from notation (e.g., "C-5" -> 5)
        const char *p = notation;
        while (*p) {
            if (*p >= '0' && *p <= '9') {
                int digit = *p - '0';
                
                // Check if multiplying by 10 would overflow
                if (arg <= INT_MAX / 10) {
                    int new_arg = arg * 10;
                    // Check if adding the digit would overflow
                    if (new_arg <= INT_MAX - digit) {
                        arg = new_arg + digit;
                        set_prefix_arg(arg);
                    }
                    // else: silently ignore the digit (overflow would occur)
                }
                // else: silently ignore the digit (overflow would occur)
                break;
            }
            p++;
        }
        
        // Restore the sign
        if (was_negative) arg = -arg;
        set_prefix_arg(arg);
        
        // Display the current arg value
        char msg[32];
        snprintf(msg, sizeof(msg), "C-u %d", arg);
        message(msg);
        
        set_raw_prefix_arg(false);
    } else if (!is_argument_function(binding->action.scheme_proc)) {
        arg = 1;
        set_prefix_arg(arg);
        argument_manually_set = false;
        set_raw_prefix_arg(false);
    }
    
    if (!is_vertical_motion(binding->action.scheme_proc)) update_goal_column();
    last_command_was_kill = is_kill_command(binding->action.scheme_proc);
    last_command = binding->action.scheme_proc;
}

void key_callback(int key, int action, int mods) {
    shift = mods & MOD_SHIFT;
    ctrl  = mods & MOD_CONTROL;
    alt   = mods & MOD_ALT;
    
    
    if (action == PRESS || action == REPEAT) {
        switch (key) {
        }
        
        reset_cursor_blink(current_buffer);
    }
}


// Find which window contains the given screen coordinates
static Window* find_window_at_point_recursive(Window *win, float x, float y) {
    if (!win) return NULL;
    
    // Check if point is within this window's bounds
    if (x >= win->x && x < win->x + win->width &&
        y >= win->y && y < win->y + win->height) {
        
        // If it's a leaf window, we found it
        if (is_leaf_window(win)) {
            return win;
        }
        
        // Otherwise, check children
        Window *result = find_window_at_point_recursive(win->left, x, y);
        if (result) return result;
        
        result = find_window_at_point_recursive(win->right, x, y);
        if (result) return result;
    }
    
    return NULL;
}

Window* find_window_at_point(float x, float y) {
    // Check minibuffer first if active
    if (selected_frame->wm.minibuffer_active && selected_frame->wm.minibuffer_window) {
        Window *mb = selected_frame->wm.minibuffer_window;
        if (x >= mb->x && x < mb->x + mb->width &&
            y >= mb->y && y < mb->y + mb->height) {
            return mb;
        }
    }
    
    // Then check regular windows
    return find_window_at_point_recursive(selected_frame->wm.root, x, y);
}

size_t point_at_window_position(Window *win, float click_x, float click_y) {
    if (!win || !win->buffer) return 0;
    
    Buffer *buffer = win->buffer;
    
    // Get default face font
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? default_face->font : NULL;
    if (!default_font) return 0;
    
    float line_height = default_font->ascent + default_font->descent;
    float max_x = win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    // Get truncate-lines setting
    SCM truncate_lines_sym = scm_from_utf8_symbol("truncate-lines");
    SCM truncate_lines_val = buffer_local_value(truncate_lines_sym, buffer);
    bool truncate_lines = scm_is_true(truncate_lines_val);
    
    // Calculate starting position (same as draw_buffer)
    float scroll_offset_y = 0;
    size_t start_pos = 0;
    if (!win->is_minibuffer) {
        start_pos = find_start_position(buffer, win, &scroll_offset_y);
    }
    
    // Apply horizontal scroll offset for truncate-lines
    float scroll_offset_x = (!win->is_minibuffer && truncate_lines) ? win->scrollx : 0;
    
    // Calculate buffer coordinates (matching draw_buffer exactly)
    float start_x = win->x + selected_frame->left_fringe_width;
    float start_y = win->y + win->height - default_font->ascent + default_font->descent;
    
    float line_start_x = start_x - scroll_offset_x;
    float x = line_start_x;
    float y = start_y + (!win->is_minibuffer ? win->scrolly : 0) - scroll_offset_y;
    
    float window_bottom = win->y;
    if (!win->is_minibuffer) {
        window_bottom += line_height;
    }
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, start_pos);
    
    uint32_t ch;
    size_t i = start_pos;
    size_t best_pos = start_pos;
    float current_line_height = line_height;
    bool on_target_line = false;
    size_t current_line_end = start_pos;
    size_t last_visible_pos = start_pos;  // ADD THIS: track the last position we saw
    
    while (rope_iter_next_char(&iter, &ch)) {
        // Get face and font for this character
        int face_id = get_text_property_face(buffer, i);
        Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
        if (!face) face = default_face;
        
        Font *char_font = get_face_font(face);
        if (!char_font) char_font = default_font;
        
        // Track maximum line height
        float font_height = char_font->ascent + char_font->descent;
        if (font_height > current_line_height) {
            current_line_height = font_height;
        }
        
        // Stop if we've scrolled past the visible area
        if (y < window_bottom - current_line_height) {
            break;
        }
        
        last_visible_pos = i;  // ADD THIS: update last visible position
        
        if (ch == '\n') {
            // Calculate line bounds
            float line_top = y - (char_font->descent * 2) + (char_font->ascent + char_font->descent);
            float line_bottom = y - (char_font->descent * 2);
            
            // Check if click is on this line (past end of line content)
            if (click_y >= line_bottom && click_y <= line_top) {
                rope_iter_destroy(&iter);
                // Return the position BEFORE the newline if we're on target line
                return on_target_line ? current_line_end : i;
            }
            
            // Move to next line
            x = line_start_x;
            y -= current_line_height;
            current_line_height = line_height;
            best_pos = i + 1;
            on_target_line = false;
            current_line_end = i + 1;
            
        } else {
            // Get character metrics
            Character *char_info = font_get_character(char_font, ch);
            float char_width = character_width(char_font, ch);
            float char_advance = char_info ? char_info->ax : char_width;
            
            // Handle truncation mode
            if (truncate_lines) {
                // Skip rest of line if we're past the right edge
                if (x > start_x + max_x) {
                    while (rope_iter_next_char(&iter, &ch) && ch != '\n') {
                        i++;
                        last_visible_pos = i;  // ADD THIS
                    }
                    if (ch == '\n') {
                        x = line_start_x;
                        y -= current_line_height;
                        current_line_height = line_height;
                        best_pos = i + 1;
                        current_line_end = i + 1;
                    }
                    i++;
                    continue;
                }
                
                // Skip characters off the left edge
                if (x + char_advance < start_x) {
                    x += char_advance;
                    i++;
                    continue;
                }
            } else {
                // Handle line wrapping
                if (x + char_advance > start_x + max_x) {
                    // Calculate line bounds before wrapping
                    float line_top = y - (char_font->descent * 2) + (char_font->ascent + char_font->descent);
                    float line_bottom = y - (char_font->descent * 2);
                    
                    // If click was on the line we just finished
                    if (on_target_line && click_y >= line_bottom && click_y <= line_top) {
                        rope_iter_destroy(&iter);
                        return current_line_end;
                    }
                    
                    // Wrap to next line
                    x = start_x;
                    y -= current_line_height;
                    current_line_height = line_height;
                    on_target_line = false;
                    current_line_end = i;
                    
                    // Stop if wrapped past visible area
                    if (y < window_bottom - current_line_height) {
                        break;
                    }
                }
            }
            
            // Calculate character bounds
            float char_top = y - (char_font->descent * 2) + (char_font->ascent + char_font->descent);
            float char_bottom = y - (char_font->descent * 2);
            
            // Check if click Y is on this line
            if (click_y >= char_bottom && click_y <= char_top) {
                on_target_line = true;
                current_line_end = i + 1;
                
                // Character region is from x (left edge) to x + char_advance (right edge)
                float char_left = x;
                float char_right = x + char_advance;
                float char_mid = char_left + (char_advance / 2.0f);
                
                // If click is within this character's bounds, return position i
                if (click_x >= char_left && click_x < char_right) {
                    rope_iter_destroy(&iter);
                    return i;
                }
            }
            
            // Advance X position
            x += char_advance;
        }
        
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    // If we were on the target line, return the end of that line
    if (on_target_line) {
        return current_line_end;
    }
    
    // CHANGED THIS: If click is below all content, go to end of buffer
    // Check if we've reached the end of the buffer
    size_t buf_len = rope_char_length(buffer->rope);
    if (last_visible_pos >= buf_len - 1 || i >= buf_len) {
        // We've seen the end of the buffer, so clicking below should go to the end
        return buf_len;
    }
    
    // Otherwise, clamp best position to buffer length
    if (best_pos > buf_len) {
        best_pos = buf_len;
    }
    
    return best_pos;
}


static GLFWcursor* arrow_cursor = NULL;
static GLFWcursor* hresize_cursor = NULL;
static GLFWcursor* vresize_cursor = NULL;
static GLFWcursor* current_cursor = NULL;

void init_cursors() {
    arrow_cursor = createStandardCursor(ARROW_CURSOR);
    hresize_cursor = createStandardCursor(HRESIZE_CURSOR);
    vresize_cursor = createStandardCursor(VRESIZE_CURSOR);
    current_cursor = arrow_cursor;  // Set initial cursor
}

// Returns true if the position is on a window divider
static bool is_on_divider(float x, float y, Window *win, Window **out_parent) {
    if (!win || is_leaf_window(win)) return false;
    
    if (win->split_type == SPLIT_VERTICAL) {
        float divider_x = win->x + win->width * win->split_ratio;
        float divider_width = 1.0f;  // TODO Once window dividers width is customizable
                                     // change this to use the customized one
        
        // Check if mouse is within divider bounds
        if (x >= divider_x - divider_width / 2 && 
            x <= divider_x + divider_width / 2 &&
            y >= win->y && 
            y <= win->y + win->height) {
            if (out_parent) *out_parent = win;
            return true;
        }
    }
    
    // Recursively check children
    if (is_on_divider(x, y, win->left, out_parent)) return true;
    if (is_on_divider(x, y, win->right, out_parent)) return true;
    
    return false;
}

static bool is_on_draggable_modeline(float x, float y, Window *win, Window **out_parent) {
    if (!win) return false;
    
    // If it's a leaf window with a modeline
    if (is_leaf_window(win) && !win->is_minibuffer) {
        Font *font = face_cache->faces[FACE_DEFAULT]->font;
        float line_height = font->ascent + font->descent;
        float modeline_y = win->y;
        
        // Check if mouse is on this window's modeline
        if (x >= win->x && x <= win->x + win->width &&
            y >= modeline_y && y <= modeline_y + line_height) {
            
            // Now check if this window has a horizontal split parent with this window on top
            // We need to traverse up to find the parent
            // For now, we'll check in the parent finding logic
            if (out_parent) {
                *out_parent = win;
            }
            return true;
        }
    }
    
    // Recursively check children
    bool found = false;
    if (win->left) {
        found = is_on_draggable_modeline(x, y, win->left, out_parent);
        if (found) return true;
    }
    if (win->right) {
        found = is_on_draggable_modeline(x, y, win->right, out_parent);
        if (found) return true;
    }
    
    return false;
}

// Check if 'target' is a descendant of 'node' (or is the node itself)
static bool is_descendant_of(Window *node, Window *target) {
    if (!node) return false;
    if (node == target) return true;
    
    if (is_leaf_window(node)) return false;
    
    return is_descendant_of(node->left, target) || is_descendant_of(node->right, target);
}

// Find the horizontal split parent where this window is in the top child subtree
static Window* find_horizontal_split_parent(Window *root, Window *target) {
    if (!root || is_leaf_window(root)) return NULL;
    
    // Check if this is a horizontal split with target in left (top) subtree
    if (root->split_type == SPLIT_HORIZONTAL && is_descendant_of(root->left, target)) {
        return root;
    }
    
    // Recursively search in children
    Window *result = find_horizontal_split_parent(root->left, target);
    if (result) return result;
    return find_horizontal_split_parent(root->right, target);
}


// Store absolute positions of all dividers when drag starts
typedef struct {
    Window *window;
    float absolute_position;  // Absolute X or Y position
} DividerPosition;

static DividerPosition saved_dividers[64];
static int saved_divider_count = 0;

static bool is_dragging_divider = false;
static bool is_dragging_modeline = false;
static Window *dragging_divider_parent = NULL;
static Window *dragging_modeline_window = NULL;
static Window *dragging_modeline_parent = NULL;

static float drag_start_x = 0;
static float drag_start_y = 0;
static float drag_start_ratio = 0;


static void save_divider_positions(Window *win, bool vertical, Window *exclude) {
    if (!win || is_leaf_window(win)) return;
    
    // Only save dividers of the same orientation (vertical or horizontal)
    if ((vertical && win->split_type == SPLIT_VERTICAL) ||
        (!vertical && win->split_type == SPLIT_HORIZONTAL)) {
        
        // Don't save the divider we're dragging
        if (win != exclude) {
            if (saved_divider_count < 64) {
                saved_dividers[saved_divider_count].window = win;
                if (vertical) {
                    saved_dividers[saved_divider_count].absolute_position = 
                        win->x + win->width * win->split_ratio;
                } else {
                    saved_dividers[saved_divider_count].absolute_position = 
                        win->y + win->height * win->split_ratio;
                }
                saved_divider_count++;
            }
        }
    }
    
    save_divider_positions(win->left, vertical, exclude);
    save_divider_positions(win->right, vertical, exclude);
}

// Check if a window's children would all meet minimum size requirements with given ratio
static bool check_min_sizes_recursive(Window *win, bool is_vertical) {
    if (!win || is_leaf_window(win)) return true;
    
    // Get minimum sizes from Scheme functions
    float min_width_pixels = scm_to_double(scm_window_min_pixel_width(SCM_UNDEFINED));
    float min_height_pixels = scm_to_double(scm_window_min_pixel_height(SCM_UNDEFINED));
    
    // Check this window's split
    if ((is_vertical && win->split_type == SPLIT_VERTICAL) ||
        (!is_vertical && win->split_type == SPLIT_HORIZONTAL)) {
        
        float size = is_vertical ? win->width : win->height;
        float min_size = is_vertical ? min_width_pixels : min_height_pixels;
        float left_size = size * win->split_ratio;
        float right_size = size * (1.0f - win->split_ratio);
        
        // If either child is too small, fail
        if (left_size < min_size || right_size < min_size) {
            return false;
        }
    }
    
    // Recursively check children
    if (!check_min_sizes_recursive(win->left, is_vertical)) return false;
    if (!check_min_sizes_recursive(win->right, is_vertical)) return false;
    
    return true;
}

// Restore divider positions, but allow them to move if constrained
// Returns true if all dividers were successfully restored without violating min size
static bool restore_divider_positions() {
    // Get minimum sizes from Scheme functions
    float min_width_pixels = scm_to_double(scm_window_min_pixel_width(SCM_UNDEFINED));
    float min_height_pixels = scm_to_double(scm_window_min_pixel_height(SCM_UNDEFINED));
    
    for (int i = 0; i < saved_divider_count; i++) {
        Window *win = saved_dividers[i].window;
        float target_abs_pos = saved_dividers[i].absolute_position;
        
        if (win->split_type == SPLIT_VERTICAL) {
            float new_ratio = (target_abs_pos - win->x) / win->width;
            float min_ratio = min_width_pixels / win->width;
            float max_ratio = 1.0f - min_ratio;
            
            // If we need to clamp, it means this divider is being pushed
            // Update its saved position so it doesn't snap back
            if (new_ratio < min_ratio) {
                new_ratio = min_ratio;
                saved_dividers[i].absolute_position = win->x + win->width * new_ratio;
            } else if (new_ratio > max_ratio) {
                new_ratio = max_ratio;
                saved_dividers[i].absolute_position = win->x + win->width * new_ratio;
            }
            
            win->split_ratio = new_ratio;
        } else if (win->split_type == SPLIT_HORIZONTAL) {
            float new_ratio = (target_abs_pos - win->y) / win->height;
            float min_ratio = min_height_pixels / win->height;
            float max_ratio = 1.0f - min_ratio;
            
            // If we need to clamp, it means this divider is being pushed
            // Update its saved position so it doesn't snap back
            if (new_ratio < min_ratio) {
                new_ratio = min_ratio;
                saved_dividers[i].absolute_position = win->y + win->height * new_ratio;
            } else if (new_ratio > max_ratio) {
                new_ratio = max_ratio;
                saved_dividers[i].absolute_position = win->y + win->height * new_ratio;
            }
            
            win->split_ratio = new_ratio;
        }
    }
    
    return true;
}

void mouse_button_callback(int button, int action, int mods) {
    if (button == MOUSE_BUTTON_LEFT) {
        double xpos, ypos;
        getCursorPos(context.window, &xpos, &ypos);
        
        float x = (float)xpos;
        float y = (float)(context.swapChainExtent.height - ypos);
        
        if (action == PRESS) {
            // Check if clicking on a vertical divider first (higher priority)
            Window *divider_parent = NULL;
            if (is_on_divider(x, y, selected_frame->wm.root, &divider_parent)) {
                // Start dragging the divider
                is_dragging_divider = true;
                dragging_divider_parent = divider_parent;
                drag_start_x = x;
                drag_start_y = y;
                drag_start_ratio = divider_parent->split_ratio;
                
                // Save all other divider positions
                saved_divider_count = 0;
                bool is_vertical = (divider_parent->split_type == SPLIT_VERTICAL);
                save_divider_positions(selected_frame->wm.root, is_vertical, divider_parent);
                
                return;
            }
            
            // Check if clicking on a draggable modeline
            Window *modeline_window = NULL;
            if (is_on_draggable_modeline(x, y, selected_frame->wm.root, &modeline_window)) {
                // Find the horizontal split parent
                Window *h_parent = find_horizontal_split_parent(selected_frame->wm.root, modeline_window);
                if (h_parent) {
                    // Start dragging the modeline
                    is_dragging_modeline = true;
                    dragging_modeline_window = modeline_window;
                    dragging_modeline_parent = h_parent;
                    drag_start_x = x;
                    drag_start_y = y;
                    drag_start_ratio = h_parent->split_ratio;
                    
                    // Save all other horizontal divider positions
                    saved_divider_count = 0;
                    save_divider_positions(selected_frame->wm.root, false, h_parent); // false = horizontal
                    
                    return;
                }
            }
            
            // Get font for line height calculation
            Font *font = face_cache->faces[FACE_DEFAULT]->font;
            float line_height = font->ascent + font->descent;
            
            // Check if clicked on inactive minibuffer area (bottom line of frame)
            if (!selected_frame->wm.minibuffer_active && y < line_height) {
                SCM view_echo_func = scm_c_lookup("view-echo-area-messages");
                if (scm_is_true(scm_variable_bound_p(view_echo_func))) {
                    scm_call_0(scm_variable_ref(view_echo_func));
                }
                return;
            }
            
            // Find window at this position
            Window *clicked_window = find_window_at_point(x, y);
            
            if (clicked_window) {
                // Check if click is on the modeline
                float modeline_y = clicked_window->y;
                float modeline_height = line_height;
                
                bool clicked_on_modeline = !clicked_window->is_minibuffer && 
                                           y >= modeline_y && 
                                           y < modeline_y + modeline_height;
                
                // Handle modeline click - just switch window, don't move cursor
                if (clicked_on_modeline) {
                    // Save point in current window if switching
                    if (clicked_window != selected_frame->wm.selected) {
                        selected_frame->wm.selected->point = current_buffer->pt;
                        
                        // Deselect old window
                        selected_frame->wm.selected->is_selected = false;
                        
                        // Select new window
                        clicked_window->is_selected = true;
                        selected_frame->wm.selected = clicked_window;
                        
                        // Update global buffer pointer and restore point
                        current_buffer = clicked_window->buffer;
                        current_buffer->pt = clicked_window->point;
                    }
                    return;
                }
                
                // Save point in current window if switching
                if (clicked_window != selected_frame->wm.selected) {
                    selected_frame->wm.selected->point = current_buffer->pt;
                    
                    // Deselect old window
                    selected_frame->wm.selected->is_selected = false;
                    
                    // Select new window
                    clicked_window->is_selected = true;
                    selected_frame->wm.selected = clicked_window;
                    
                    // Update global buffer pointer and restore point
                    current_buffer = clicked_window->buffer;
                    current_buffer->pt = clicked_window->point;
                }
                
                // Normal click - set point to clicked position within the window
                size_t new_point = point_at_window_position(clicked_window, x, y);
                set_point(new_point);
                clicked_window->point = new_point;
                update_goal_column();
            }
        } else if (action == RELEASE) {
            // Stop dragging
            if (is_dragging_divider) {
                is_dragging_divider = false;
                dragging_divider_parent = NULL;
                saved_divider_count = 0;
            }
            if (is_dragging_modeline) {
                is_dragging_modeline = false;
                dragging_modeline_window = NULL;
                dragging_modeline_parent = NULL;
                saved_divider_count = 0;
            }
        }
    }
    reset_cursor_blink(current_buffer);
}

void scroll_callback(double xoffset, double yoffset) {
    // Get mouse position
    double xpos, ypos;
    getCursorPos(context.window, &xpos, &ypos);
    
    // Convert to screen coordinates
    float x = (float)xpos;
    float y = (float)(context.swapChainExtent.height - ypos);
    
    // Find window at this position
    Window *scroll_window = find_window_at_point(x, y);
    
    if (!scroll_window) return;
    
    // Save current selection state
    Window *original_selected = selected_frame->wm.selected;
    Buffer *original_buffer = current_buffer;
    size_t original_point = current_buffer->pt;
    
    // Temporarily switch to the window under the mouse
    selected_frame->wm.selected = scroll_window;
    current_buffer = scroll_window->buffer;
    current_buffer->pt = scroll_window->point;
    
    // Set up prefix argument for scroll amount (2 lines)
    bool saved_manually_set = argument_manually_set;
    int saved_arg = get_prefix_arg();
    
    argument_manually_set = true;
    set_prefix_arg(2);
    
    // Scroll based on direction
    if (yoffset > 0) {
        // Scroll down (content moves down, window view moves up)
        scroll_down_command();
    } else if (yoffset < 0) {
        // Scroll up (content moves up, window view moves down)
        scroll_up_command();
    }
    
    // Restore prefix argument state
    argument_manually_set = saved_manually_set;
    set_prefix_arg(saved_arg);
    
    // Restore original selection if it was different
    if (scroll_window != original_selected) {
        // Save the scrolled window's point
        scroll_window->point = current_buffer->pt;
        
        // Restore original selection
        selected_frame->wm.selected = original_selected;
        current_buffer = original_buffer;
        current_buffer->pt = original_point;
    }
}

double lastX = WIDTH / 2.0f, lastY = HEIGHT / 2.0f;

// Helper function to snap to character width boundaries
static float snap_delta_to_char_width(float delta, float char_width, bool moving_positive) {
    if (char_width <= 0 || delta == 0) {
        return delta;
    }
    
    // Get the absolute delta and sign
    float abs_delta = fabsf(delta);
    
    // Snap the absolute delta
    float snapped_abs_delta;
    if (abs_delta < char_width) {
        // If delta is less than one char width, snap to either 0 or 1 char width
        // depending on if we've moved more than halfway
        snapped_abs_delta = (abs_delta >= char_width * 0.5f) ? char_width : 0.0f;
    } else {
        // Snap to nearest char width boundary in the direction of movement
        if (moving_positive) {
            snapped_abs_delta = ceilf(abs_delta / char_width) * char_width;
        } else {
            snapped_abs_delta = floorf(abs_delta / char_width) * char_width;
        }
    }
    
    // Restore the sign
    return (delta >= 0) ? snapped_abs_delta : -snapped_abs_delta;
}


// TODO Fix windows pushing eachoter after reaching
// min-width or min-height, it doesn't work really nice rn
// TODO Fix cursor hiding after we move at least once
// on a window divider thus changing the cursor
void cursor_pos_callback(double xpos, double ypos) {
    double xoffset = xpos - lastX;
    double yoffset = lastY - ypos;
    
    // Only show if pointer is currently hidden
    if (!scm_get_bool("pointer-visible", true)) {
        showCursor();
        scm_c_define("pointer-visible", SCM_BOOL_T);
    }
    
    float x = (float)xpos;
    float y = (float)(context.swapChainExtent.height - ypos);
    
    // Handle divider dragging
    if (is_dragging_divider && dragging_divider_parent) {
        bool is_vertical = (dragging_divider_parent->split_type == SPLIT_VERTICAL);
        bool pixelwise = scm_get_bool("window-resize-pixelwise", false);
        
        if (is_vertical) {
            // Vertical split - handle horizontal dragging
            float delta_x = x - drag_start_x;
            
            // Apply character-wise snapping to the delta, not the position
            if (!pixelwise) {
                float char_width = frame_char_width(selected_frame);
                bool moving_right = delta_x > 0;
                delta_x = snap_delta_to_char_width(delta_x, char_width, moving_right);
            }
            
            float new_position = drag_start_ratio * dragging_divider_parent->width + delta_x;
            float new_ratio = new_position / dragging_divider_parent->width;
            
            // Get minimum width in pixels
            float min_width = scm_to_double(scm_window_min_pixel_width(SCM_UNDEFINED));
            float min_ratio = min_width / dragging_divider_parent->width;
            float max_ratio = 1.0f - min_ratio;
            new_ratio = fmaxf(min_ratio, fminf(max_ratio, new_ratio));
            
            // Save old ratio for potential revert
            float old_ratio = dragging_divider_parent->split_ratio;
            
            // Apply new ratio
            dragging_divider_parent->split_ratio = new_ratio;
            wm_recalculate_layout();
            restore_divider_positions();
            wm_recalculate_layout();
            
            // Check if all windows still meet minimum size requirements
            if (!check_min_sizes_recursive(selected_frame->wm.root, is_vertical)) {
                // Revert - minimum size violated
                dragging_divider_parent->split_ratio = old_ratio;
                wm_recalculate_layout();
                restore_divider_positions();
                wm_recalculate_layout();
            }
            
        } else {
            // Horizontal split - handle vertical dragging
            float delta_y = y - drag_start_y;
            
            // Apply line-wise snapping to the delta
            if (!pixelwise) {
                float line_height = selected_frame->line_height;
                bool moving_down = delta_y > 0;
                delta_y = snap_delta_to_char_width(delta_y, line_height, moving_down);
            }
            
            float new_position = drag_start_ratio * dragging_divider_parent->height + delta_y;
            float new_ratio = new_position / dragging_divider_parent->height;
            
            // Get minimum height in pixels
            float min_height = scm_to_double(scm_window_min_pixel_height(SCM_UNDEFINED));
            float min_ratio = min_height / dragging_divider_parent->height;
            float max_ratio = 1.0f - min_ratio;
            new_ratio = fmaxf(min_ratio, fminf(max_ratio, new_ratio));
            
            // Save old ratio for potential revert
            float old_ratio = dragging_divider_parent->split_ratio;
            
            // Apply new ratio
            dragging_divider_parent->split_ratio = new_ratio;
            wm_recalculate_layout();
            restore_divider_positions();
            wm_recalculate_layout();
            
            // Check if all windows still meet minimum size requirements
            if (!check_min_sizes_recursive(selected_frame->wm.root, is_vertical)) {
                // Revert - minimum size violated
                dragging_divider_parent->split_ratio = old_ratio;
                wm_recalculate_layout();
                restore_divider_positions();
                wm_recalculate_layout();
            }
        }
        update_windows_scroll();
    }
    // Handle modeline dragging (vertical resize)
    else if (is_dragging_modeline && dragging_modeline_parent) {
        bool pixelwise = scm_get_bool("window-resize-pixelwise", false);
        
        // Horizontal split - handle vertical dragging
        float delta_y = y - drag_start_y;
        
        // Apply line-wise snapping to the delta
        if (!pixelwise) {
            float line_height = selected_frame->line_height;
            bool moving_down = delta_y > 0;
            delta_y = snap_delta_to_char_width(delta_y, line_height, moving_down);
        }
        
        float new_position = drag_start_ratio * dragging_modeline_parent->height + delta_y;
        float new_ratio = new_position / dragging_modeline_parent->height;
        
        // Get minimum height in pixels
        float min_height = scm_to_double(scm_window_min_pixel_height(SCM_UNDEFINED));
        float min_ratio = min_height / dragging_modeline_parent->height;
        float max_ratio = 1.0f - min_ratio;
        new_ratio = fmaxf(min_ratio, fminf(max_ratio, new_ratio));
        
        // Save old ratio for potential revert
        float old_ratio = dragging_modeline_parent->split_ratio;
        
        // Apply new ratio
        dragging_modeline_parent->split_ratio = new_ratio;
        wm_recalculate_layout();
        restore_divider_positions();
        wm_recalculate_layout();
        
        // Check if all windows still meet minimum size requirements
        if (!check_min_sizes_recursive(selected_frame->wm.root, false)) {  // false = horizontal
            // Revert - minimum size violated
            dragging_modeline_parent->split_ratio = old_ratio;
            wm_recalculate_layout();
            restore_divider_positions();
            wm_recalculate_layout();
        }
        update_windows_scroll();
    }
    else {
        // Not dragging - update cursor based on hover position
        Window *divider_parent = NULL;
        Window *modeline_window = NULL;
        GLFWcursor* desired_cursor = arrow_cursor;
        
        // Check vertical dividers first (higher priority)
        if (is_on_divider(x, y, selected_frame->wm.root, &divider_parent)) {
            desired_cursor = hresize_cursor;
        }
        // Then check draggable modelines
        else if (is_on_draggable_modeline(x, y, selected_frame->wm.root, &modeline_window)) {
            Window *h_parent = find_horizontal_split_parent(selected_frame->wm.root, modeline_window);
            if (h_parent) {
                desired_cursor = vresize_cursor;
            }
        }
        
        // ONLY change cursor if it's different from current
        if (desired_cursor != current_cursor) {
            setCursor(desired_cursor);
            current_cursor = desired_cursor;
        }
    }
    
    lastX = xpos;
    lastY = ypos;
}

void window_resize_callback(int width, int height) {
    sw = width;
    sh = height;
    
    selected_frame->height = height;
    selected_frame->width = width;

    // Calculate minibuffer height to properly size root window
    float minibuffer_height = calculate_minibuffer_height();
    
    selected_frame->wm.root->x = 0;
    selected_frame->wm.root->y = minibuffer_height;
    selected_frame->wm.root->width = width;
    selected_frame->wm.root->height = height - minibuffer_height;
    
    selected_frame->wm.minibuffer_window->width = width;
    selected_frame->wm.minibuffer_window->height = minibuffer_height;
    
    // Only recalculate if there are splits
    if (!is_leaf_window(selected_frame->wm.root)) {
        wm_recalculate_layout();
    }
    
    // We might make the modeline bump
    // into the cursor while resizing
    update_windows_scroll();

    /* printf("Width: %i, Height: %i\n", width, height); */
}

void window_focus_callback(int focused) {
    if (focused) {
        selected_frame->focused = true; 
    } else {
        selected_frame->focused = false;
    }
}

// NOTE This won’t work on Wayland (Future of Desktop btw)
void window_pos_callback(int xpos, int ypos) {
    selected_frame->x = xpos;
    selected_frame->y = ypos;
    printf("Frame position: %i %i\n", xpos, ypos);
}


static void inner_main (void *data, int argc, char **argv) {
    initWindow(sw, sh, "Glemax");

    init_cursors();

    registerKeyCallback(key_callback);
    registerTextCallback(text_callback);
    registerMouseButtonCallback(mouse_button_callback);
    registerCursorPosCallback(cursor_pos_callback);
    registerWindowResizeCallback(window_resize_callback);
    registerWindowFocusCallback(window_focus_callback);
    registerWindowPosCallback(window_pos_callback);
    registerScrollCallback(scroll_callback);

    
    register_after_keychord_hook(after_keychord_hook);
    register_before_keychord_hook(before_keychord_hook);
    
    
    Buffer *scratch_buffer = buffer_create("*scratch*");
    Buffer *minibuf = buffer_create("minibuf");
    Buffer *messages = buffer_create("*Messages*");
    
    sh = context.swapChainExtent.height; // TODO move into
    sw = context.swapChainExtent.width;  // Resize callback
    printf("sw: %u, sh: %u\n", sw, sh);


    // Create the one and only frame (for now)
    selected_frame = malloc(sizeof(Frame));
    selected_frame->x = 0;
    selected_frame->y = 0;
    selected_frame->width = 800;  // Default width
    selected_frame->height = 600; // Default height
    selected_frame->focused = true;
    selected_frame->left_fringe_width = 8;  // TODO Those could 
    selected_frame->right_fringe_width = 8; // be scm variable
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = get_face_font(default_face);
    selected_frame->line_height = default_font->ascent + default_font->descent;
    // What character does emacs check for the column width ?
    // Do they do an avarage for non monospace fonts ?
    Character *space = font_get_character(default_font, ' ');
    selected_frame->column_width = space->ax;

    wm_init(&selected_frame->wm, scratch_buffer, minibuf, 0, 0, sw, sh);
    
    init_faces();
    lisp_init(); // IMPORTANT After initializing the windowManager

    bool resize_pixelwise = scm_get_bool("frame-resize-pixelwise", false);
    if (!resize_pixelwise) {
        setWindowResizeIncrements(selected_frame->column_width, selected_frame->line_height, selected_frame->left_fringe_width, selected_frame->right_fringe_width);
    }
    
    
    while (!windowShouldClose()) {
        beginFrame();
        
        clear_background(face_cache->faces[FACE_DEFAULT]->bg);
        
        fps(face_cache->faces[FACE_DEFAULT]->font, sw - 400, 200, RED);
        
        wm_draw(&selected_frame->wm);
        
        endFrame();
   }
    
    wm_cleanup(&selected_frame->wm);
    buffer_destroy(scratch_buffer);
    cleanup(&context);
}

int main(int argc, char **argv) {
    scm_boot_guile (argc, argv, inner_main, 0);
    return 0;
}

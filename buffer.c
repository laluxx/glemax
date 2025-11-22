#include "buffer.h"
#include "wm.h"
#include "lisp.h"
#include <stdbool.h>

Buffer *all_buffers = NULL;
Buffer *current_buffer = NULL;

bool argument_manually_set = false;

bool shift;
bool ctrl;
bool alt;
 

SCM last_command = SCM_BOOL_F; // Shouldn’t it be NULL ? 
bool last_command_was_kill = false;



bool is_kill_command(SCM proc) {
    return is_scm_proc(proc, "kill-word") ||
           is_scm_proc(proc, "backward-kill-word") ||
           is_scm_proc(proc, "kill-line") ||
           is_scm_proc(proc, "kill-sexp") ||
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
    int arg = get_prefix_arg();
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
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
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

void append_to_buffer(Buffer *buf, const char *text, bool prepend_newline) {
    if (!buf || !text) return;
    
    size_t text_len = strlen(text);
    if (text_len == 0) return;
    
    size_t end_pos = rope_char_length(buf->rope);
    
    if (prepend_newline) {
        buf->rope = rope_insert_chars(buf->rope, end_pos, "\n", 1);
        end_pos++;
    }
    
    buf->rope = rope_insert_chars(buf->rope, end_pos, text, text_len);
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
    
    // Log to *Messages* buffer
    Buffer *messages_buf = get_buffer("*Messages*");
    if (messages_buf) {
        bool needs_newline = rope_char_length(messages_buf->rope) > 0;
        append_to_buffer(messages_buf, formatted, needs_newline);
    }
    
    free(formatted);
}



/// ARG


// TODO Activate universal-argument-map and bind 0..9 to digit-argument
void universal_argument() {
    int arg = get_prefix_arg();
    arg *= 4;
    set_prefix_arg(arg);
    bool raw_prefix_arg = scm_get_bool("raw-prefix-arg", false);
    set_raw_prefix_arg(true);
}

void digit_argument() {
    // NOTE This function is called, but the actual digit handling
    // happens in after_keychord_hook by parsing the notation
}

void negative_argument() {
    int arg = get_prefix_arg();
    arg = -arg;
    set_prefix_arg(arg);
    char msg[32];
    snprintf(msg, sizeof(msg), "C-u %d", arg);
    message(msg);
}

int get_prefix_arg() {
    SCM var = scm_c_lookup("prefix-arg");
    if (scm_is_false(var)) return 1;
    SCM val = scm_variable_ref(var);
    return scm_is_integer(val) ? scm_to_int(val) : 1;
}

void set_prefix_arg(int value) {
    scm_c_define("prefix-arg", scm_from_int(value));
}

bool get_raw_prefix_arg() {
    SCM var = scm_c_lookup("raw-prefix-arg");
    if (scm_is_false(var)) return false;
    SCM val = scm_variable_ref(var);
    return scm_to_bool(val);
}

void set_raw_prefix_arg(bool value) {
    scm_c_define("raw-prefix-arg", scm_from_bool(value));
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

// TODO It’s wrapping one character before where it should actually start wrapping
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


#include "buffer.h"
#include "wm.h"
#include "lisp.h"
#include "faces.h"
#include "theme.h"
#include <obsidian/window.h>
#include <stdbool.h>

Buffer *all_buffers = NULL;
Buffer *current_buffer = NULL;

bool argument_manually_set = false;

bool shift;
bool ctrl;
bool alt;
 

SCM last_command = SCM_BOOL_F;
bool last_command_was_kill = false;



bool is_kill_command(SCM proc) {
    return is_scm_proc(proc, "kill-word") ||
           is_scm_proc(proc, "backward-kill-word") ||
           is_scm_proc(proc, "kill-line") ||
           is_scm_proc(proc, "kill-sexp") ||
           is_scm_proc(proc, "kill-region");
}

Buffer* buffer_create(const char *name) {
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
    buffer->region.active = false;
    buffer->region.mark = -1;
    buffer->props = NULL;
    buffer->ts_state = NULL;

    // Initialize buffer-local variables as empty alist
    buffer->local_var_alist = SCM_EOL;
    scm_gc_protect_object(buffer->local_var_alist);

    // Initialize keymap as NULL (will use global keymap)
    buffer->keymap = NULL;    


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
    
    // Unprotect the alist
    scm_gc_unprotect_object(buffer->local_var_alist);


    // Free buffer-local keymap if it exists
    if (buffer->keymap) {
        keymap_free(buffer->keymap);
        free(buffer->keymap);
        buffer->keymap = NULL;
    }

    if (buffer->ts_state) {
        treesit_parser_delete(buffer->ts_state);
        buffer->ts_state = NULL;
    }


    free(buffer->name);
    rope_free(buffer->rope);
    clear_text_properties(buffer);
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

Buffer *get_buffer_create(const char *name) {
    Buffer *buf = get_buffer(name);
    if (buf) return buf;
    
    return buffer_create(name);
}

void switch_to_buffer(Buffer *buf) {
    if (!buf) return;

    if (buf == wm.minibuffer_window->buffer) {
        message("Can't switch to minibuf buffer");
        return; 
    }
    
    current_buffer = buf;
    
    // Update keymap stack
    keymap_stack_clear();
    if (buf->keymap) {
        keymap_stack_push(buf->keymap);
    }
    
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
    
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy) + 1;
    va_end(args_copy);
    
    if (needed <= 0) {
        va_end(args);
        return;
    }
    
    char *formatted = malloc(needed);
    if (!formatted) {
        va_end(args);
        return;
    }
    
    vsnprintf(formatted, needed, format, args);
    va_end(args);
    
    Buffer *minibuf = wm.minibuffer_window->buffer;
    
    if (wm.minibuffer_active) {
        // Clear any existing echo area message first
        if (wm.minibuffer_message_start > 0) {
            size_t current_len = rope_char_length(minibuf->rope);
            if (current_len > wm.minibuffer_message_start) {
                size_t msg_len = current_len - wm.minibuffer_message_start;
                remove_text_properties(minibuf, wm.minibuffer_message_start, current_len);
                minibuf->rope = rope_delete_chars(minibuf->rope, 
                                                  wm.minibuffer_message_start, 
                                                  msg_len);
            }
        }
        
        // Append new message in brackets
        size_t original_len = rope_char_length(minibuf->rope);
        wm.minibuffer_message_start = original_len;  // Track where message starts
        
        char *bracketed = malloc(strlen(formatted) + 4);
        if (bracketed) {
            sprintf(bracketed, " [%s]", formatted);
            size_t bracket_len = strlen(bracketed);
            
            minibuf->rope = rope_insert_chars(minibuf->rope, original_len, 
                                             bracketed, bracket_len);
            
            // Make the message read-only
            put_text_property(minibuf, original_len, original_len + bracket_len,
                            scm_from_locale_symbol("read-only"),
                            SCM_BOOL_T);
            
            // Set the face
            int msg_face = face_id_from_name("minibuffer-prompt");
            put_text_property(minibuf, original_len, original_len + bracket_len,
                              scm_from_locale_symbol("face"),
                              scm_from_int(msg_face));
            
            free(bracketed);
        }
    } else {
        // Minibuffer is not active - replace content
        wm.minibuffer_message_start = 0;  // Reset tracking
        size_t len = rope_char_length(minibuf->rope);
        if (len > 0) {
            clear_text_properties(minibuf);
            minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
        }
        
        size_t msg_len = strlen(formatted);
        minibuf->rope = rope_insert_chars(minibuf->rope, 0, formatted, msg_len);
        wm.minibuffer_window->point = 0;
    }
    
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
    /* bool raw_prefix_arg = scm_get_bool("raw-prefix-arg", false); */
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



void keyboard_quit() {
    current_buffer->region.active = false;
    if (wm.selected == wm.minibuffer_window) {
        deactivate_minibuffer();
    }
}


/// Buffer local variables


// Global alist for default values of buffer-local variables
static SCM buffer_local_defaults = SCM_BOOL_F;
// Global list of symbols that are automatically buffer-local
static SCM automatically_buffer_local = SCM_BOOL_F;

// TODO Track permanent-local variables 
// In Emacs, some buffer-local variables survive kill-all-local-variables
// (used when changing major modes).
/* static SCM permanent_local_variables = SCM_BOOL_F; */

void init_buffer_locals(void) {
    if (scm_is_false(buffer_local_defaults)) {
        buffer_local_defaults = SCM_EOL;
        scm_gc_protect_object(buffer_local_defaults);
        
        // Initialize the list of automatically buffer-local variables
        automatically_buffer_local = SCM_EOL;
        scm_gc_protect_object(automatically_buffer_local);
    }
}

bool is_automatically_buffer_local(SCM symbol) {
    if (scm_is_false(automatically_buffer_local)) {
        return false;
    }
    return scm_is_true(scm_memq(symbol, automatically_buffer_local));
}

void mark_automatically_buffer_local(SCM symbol) {
    if (!is_automatically_buffer_local(symbol)) {
        scm_gc_unprotect_object(automatically_buffer_local);
        automatically_buffer_local = scm_cons(symbol, automatically_buffer_local);
        scm_gc_protect_object(automatically_buffer_local);
    }
}

// Internal helper: set in alist
static void alist_set(SCM *alist_ptr, SCM symbol, SCM value) {
    SCM entry = scm_assq(symbol, *alist_ptr);
    
    if (scm_is_true(entry)) {
        scm_set_cdr_x(entry, value);
    } else {
        scm_gc_unprotect_object(*alist_ptr);
        *alist_ptr = scm_acons(symbol, value, *alist_ptr);
        scm_gc_protect_object(*alist_ptr);
    }
}

// Internal helper: get from alist
static SCM alist_get(SCM alist, SCM symbol, SCM default_val) {
    SCM entry = scm_assq(symbol, alist);
    return scm_is_true(entry) ? scm_cdr(entry) : default_val;
}

void set_default(SCM symbol, SCM value) {
    if (scm_is_false(buffer_local_defaults)) {
        init_buffer_locals();
    }
    alist_set(&buffer_local_defaults, symbol, value);
}

SCM default_value(SCM symbol) {
    if (scm_is_false(buffer_local_defaults)) {
        return SCM_BOOL_F;
    }
    return alist_get(buffer_local_defaults, symbol, SCM_BOOL_F);
}

SCM buffer_local_value(SCM variable, Buffer *buf) {
    if (!buf) return SCM_BOOL_F;
    
    SCM entry = scm_assq(variable, buf->local_var_alist);
    if (scm_is_true(entry)) {
        return scm_cdr(entry);
    }
    
    return default_value(variable);
}

SCM buffer_set(SCM symbol, SCM newval, Buffer *buf) {
    if (!buf) return newval;
    
    alist_set(&buf->local_var_alist, symbol, newval);
    return newval;
}

bool local_variable_p(SCM symbol, Buffer *buf) {
    if (!buf) return false;
    
    SCM entry = scm_assq(symbol, buf->local_var_alist);
    return scm_is_true(entry);
}

bool local_variable_if_set_p(SCM symbol, Buffer *buf) {
    // Check if it's already local OR if it's automatically buffer-local
    return local_variable_p(symbol, buf) || is_automatically_buffer_local(symbol);
}

SCM kill_local_variable(SCM symbol, Buffer *buf) {
    if (!buf) return symbol;
    
    scm_gc_unprotect_object(buf->local_var_alist);
    buf->local_var_alist = scm_assq_remove_x(buf->local_var_alist, symbol);
    scm_gc_protect_object(buf->local_var_alist);
    
    return symbol;
}

void kill_all_local_variables(Buffer *buf) {
    if (!buf) return;
    
    scm_gc_unprotect_object(buf->local_var_alist);
    buf->local_var_alist = SCM_EOL;
    scm_gc_protect_object(buf->local_var_alist);
}

SCM buffer_local_variables(Buffer *buf) {
    if (!buf) return SCM_EOL;
    
    // Return a copy of the alist
    return scm_list_copy(buf->local_var_alist);
}


/// Keymap


KeyChordMap* make_sparse_keymap(void) {
    KeyChordMap *map = malloc(sizeof(KeyChordMap));
    if (!map) return NULL;
    
    keymap_init(map);
    return map;
}


void use_local_map(KeyChordMap *local_map, Buffer *buf) {
    if (!buf) return;
    
    // If this is the current buffer, clear stack FIRST
    // This ensures no references to the old keymap exist in the stack
    if (buf == current_buffer) {
        keymap_stack_clear();
    }
    
    // DON'T free the old keymap here!
    // The Scheme foreign object still owns it and will GC it later
    // Just replace the pointer
    buf->keymap = local_map;
    
    // Push the new keymap to stack if this is the current buffer
    if (buf == current_buffer && local_map) {
        keymap_stack_push(local_map);
    }
}

KeyChordMap* current_local_map(Buffer *buf) {
    if (!buf) return NULL;
    return buf->keymap;
}

KeyChordMap* current_global_map(void) {
    return &keymap;
}


// Helper function to find the character position at a given scroll offset
static size_t find_start_position(Buffer *buffer, Window *win, float *out_start_y) {
    if (!win || win->is_minibuffer) {
        *out_start_y = 0;
        return 0;
    }
    
    // Get default face font for initial calculation
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? default_face->font : NULL;
    if (!default_font) {
        *out_start_y = 0;
        
        return 0;
    }
    
    float line_height = default_font->ascent + default_font->descent;
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
        // Get the face and font at this position
        int face_id = get_text_property_face(buffer, pos);
        Face *face = get_face(face_id);
        Font *font = (face && face->font) ? face->font : default_font;
        
        if (ch == '\n') {
            current_line++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            if (x + char_width > max_x) {
                current_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(font, ch);
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
    // Get default face for fallback
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    if (!default_font) return;
    
    // Get base colors from active theme
    Color base_bg = theme_cache->base_bg;
    Color base_fg = theme_cache->base_fg;
    
    float line_height = default_font->ascent + default_font->descent;
    float max_x = start_x + (win->width - 2 * fringe_width);
    
    // Get truncate-lines buffer-local variable
    SCM truncate_lines_sym = scm_from_utf8_symbol("truncate-lines");
    SCM truncate_lines_val = buffer_local_value(truncate_lines_sym, buffer);
    bool truncate_lines = scm_is_true(truncate_lines_val);
    
    float window_bottom = win->y;
    float window_top = win->y + win->height;
    
    if (!win->is_minibuffer) {
        window_bottom += line_height;
    }
    
    size_t point = win ? win->point : buffer->pt;
    bool is_selected = win && win->is_selected;
    
    float scroll_offset_y = 0;
    size_t start_pos = 0;
    
    if (win && !win->is_minibuffer) {
        start_pos = find_start_position(buffer, win, &scroll_offset_y);
    }
    
    // Apply horizontal scroll offset for truncate-lines
    float scroll_offset_x = (win && !win->is_minibuffer && truncate_lines) ? win->scrollx : 0;
    float line_start_x = start_x - scroll_offset_x;
    
    float x = line_start_x;
    float y = start_y + (win && !win->is_minibuffer ? win->scrolly : 0) - scroll_offset_y;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, start_pos);
    
    uint32_t ch;
    size_t i = start_pos;
    bool visible_mark_mode = scm_get_bool("visible-mark-mode", false);
    bool crystal_point_mode = scm_get_bool("crystal-point-mode", false);
    
    // Check if mark is actually set (mark is int, so negative means unset)
    bool mark_is_set = (buffer->region.mark >= 0);
    
    float current_line_height = line_height;
    
    // Track cursor position for drawing later
    float cursor_x = 0, cursor_y = 0;
    float cursor_width = 0, cursor_height = 0;
    bool cursor_found = false;
    Color cursor_color;
    
    while (rope_iter_next_char(&iter, &ch)) {
        int face_id = get_text_property_face(buffer, i);
        Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
        if (!face) face = default_face;
        
        Font *char_font = get_face_font(face);
        if (!char_font) char_font = default_font;
        
        float font_height = char_font->ascent + char_font->descent;
        if (font_height > current_line_height) {
            current_line_height = font_height;
        }
        
        if (y < window_bottom - current_line_height) {
            break;
        }
        
        if (ch == '\n') {
            // Check if cursor is at newline position
            if (i == point) {
                cursor_x = x;
                cursor_y = y - (char_font->descent * 2);
                Character *space = font_get_character(char_font, ' ');
                cursor_width = space ? space->ax : char_font->ascent;
                cursor_height = char_font->ascent + char_font->descent;
                cursor_color = face_cache->faces[FACE_CURSOR]->bg;
                cursor_found = true;
            }
            
            // Check if mark is at newline position (only if mark is set)
            if (mark_is_set && is_selected && visible_mark_mode && 
                i == (size_t)buffer->region.mark && (size_t)buffer->region.mark != point && 
                y >= window_bottom && y <= window_top) {
                Character *space = font_get_character(char_font, ' ');
                float mark_width = space ? space->ax : char_font->ascent;
                float mark_height = char_font->ascent + char_font->descent;
                quad2D((vec2){x, y - (char_font->descent * 2)},
                       (vec2){mark_width, mark_height}, 
                       face_cache->faces[FACE_VISIBLE_MARK]->bg);
            }
            
            x = line_start_x;
            y -= current_line_height;
            current_line_height = line_height;
        } else {
            float char_width = character_width(char_font, ch);
            
            // Check for truncation or wrapping based on truncate-lines
            if (truncate_lines) {
                if (x > max_x) {
                    // Skip rest of line until newline when truncating
                    while (rope_iter_next_char(&iter, &ch) && ch != '\n') {
                        i++;
                    }
                    if (ch == '\n') {
                        x = line_start_x;
                        y -= current_line_height;
                        current_line_height = line_height;
                    }
                    i += 2;
                    continue;
                }
            } else {
                if (x + char_width > max_x) {
                    x = start_x;
                    y -= current_line_height;
                    current_line_height = line_height;
                    
                    if (y < window_bottom - current_line_height) {
                        break;
                    }
                }
            }
            
            if (y >= window_bottom && y <= window_top) {
                Color char_color = face->fg;
                Color bg_color = face->bg;
                
                // Check if we're at the cursor position (regardless of selection)
                bool at_cursor = (i == point);
                
                // Check if we're at the mark position (but NOT the cursor, and mark must be set)
                bool at_mark = mark_is_set && is_selected && visible_mark_mode && 
                               i == (size_t)buffer->region.mark && (size_t)buffer->region.mark != point;
                
                if (at_cursor) {
                    // Save cursor position and properties for later drawing
                    cursor_x = x;
                    cursor_y = y - (char_font->descent * 2);
                    Character *char_info = font_get_character(char_font, ch);
                    cursor_width = char_info ? char_info->ax : char_width;
                    cursor_height = char_font->ascent + char_font->descent;
                    
                    // Determine cursor color based on crystal-point-mode
                    if (crystal_point_mode && ch != '\n' && ch != ' ' && ch != '\t') {
                        cursor_color = face->fg;
                    } else {
                        cursor_color = face_cache->faces[FACE_CURSOR]->bg;
                    }
                    cursor_found = true;
                    
                    // Only modify character appearance if window is selected and cursor is visible
                    if (is_selected && buffer->cursor.visible) {
                        char_color = base_bg;
                    }
                } else if (at_mark) {
                    // Draw mark background
                    bg_color = face_cache->faces[FACE_VISIBLE_MARK]->bg;
                    char_color = base_bg;
                }
                
                // Draw background if needed
                bool needs_bg = (!at_cursor || !is_selected || !buffer->cursor.visible) && 
                                (!color_equals(bg_color, base_bg) || at_mark);
                
                if (needs_bg) {
                    Character *char_info = font_get_character(char_font, ch);
                    float bg_width = char_info ? char_info->ax : char_width;
                    float bg_height = char_font->ascent + char_font->descent;
                    quad2D((vec2){x, y - char_font->descent * 2},
                           (vec2){bg_width, bg_height}, bg_color);
                }
                
                float advance = character(char_font, ch, x, y, char_color);
                x += advance;
            } else {
                if (y >= window_bottom) {
                    float advance = character_width(char_font, ch);
                    x += advance;
                }
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    // Handle cursor at end of buffer
    if (point >= rope_char_length(buffer->rope)) {
        int face_id = get_text_property_face(buffer, point);
        Face *face = get_face(face_id);
        Font *char_font = face ? face->font : default_font;
        
        cursor_x = x;
        cursor_y = y - (char_font->descent * 2);
        Character *space = font_get_character(char_font, ' ');
        cursor_width = space ? space->ax : char_font->ascent;
        cursor_height = char_font->ascent + char_font->descent;
        cursor_color = face_cache->faces[FACE_CURSOR]->bg;
        cursor_found = true;
    }
    
    // Update cursor position in buffer
    buffer->cursor.x = cursor_x;
    buffer->cursor.y = cursor_y;
    
    // Draw cursor based on window selection state
    if (cursor_found) {
        // Skip drawing if minibuffer is not active
        if (win && win->is_minibuffer && !wm.minibuffer_active) {
            return;
        }
        
        if (is_selected) {
            // Draw filled cursor with optional blinking for selected window
            bool blink_cursor_mode = scm_get_bool("blink-cursor-mode", true);
            size_t blink_cursor_blinks = scm_get_size_t("blink-cursor-blinks", 0);
            float blink_cursor_interval = scm_get_float("blink-cursor-interval", 0.1);
            float blink_cursor_delay = scm_get_float("blink-cursor-delay", 0.1);

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
                    quad2D((vec2){cursor_x, cursor_y},
                           (vec2){cursor_width, cursor_height}, cursor_color);
                }
            } else {
                quad2D((vec2){cursor_x, cursor_y},
                       (vec2){cursor_width, cursor_height}, cursor_color);
            }
        } else {
            // Draw hollow cursor for non-selected windows
            float border_width = 1.0f;
            
            // Bottom border
            quad2D((vec2){cursor_x, cursor_y + cursor_height - border_width},
                   (vec2){cursor_width, border_width}, cursor_color);
            
            // Top border
            quad2D((vec2){cursor_x, cursor_y},
                   (vec2){cursor_width, border_width}, cursor_color);
            
            // Left border
            quad2D((vec2){cursor_x, cursor_y},
                   (vec2){border_width, cursor_height}, cursor_color);
            
            // Right border
            quad2D((vec2){cursor_x + cursor_width - border_width, cursor_y},
                   (vec2){border_width, cursor_height}, cursor_color);
        }
    }
}

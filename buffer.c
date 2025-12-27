#include "buffer.h"
#include "frame.h"
#include "wm.h"
#include "lisp.h"
#include "faces.h"
#include "theme.h"
#include <obsidian/window.h>
#include <stdbool.h>
#include <stddef.h>

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
    buffer->read_only = false;
    buffer->modified = false;


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

    if (buf == selected_frame->wm.minibuffer_window->buffer) {
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
    if (selected_frame->wm.selected) {
        selected_frame->wm.selected->buffer = buf;
        selected_frame->wm.selected->point = buf->pt;
    }
}

Buffer* other_buffer() {
    if (!current_buffer || !current_buffer->next) return current_buffer;
    
    Buffer *buf = current_buffer->next;
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    
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
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
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
    if (current_buffer == selected_frame->wm.minibuffer_window->buffer) {
        message("Cannot switch buffers in minibuffer window");
        return;
    }
    
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
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
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    // Adjust point for each window viewing this buffer
    for (int i = 0; i < count; i++) {
        Window *win = leaves[i];
        if (win->buffer != current_buffer) continue; // Skip windows showing other buffers
        if (win == selected_frame->wm.selected) continue; // Skip selected window (already updated)
        
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
    selected_frame->wm.selected->point = new_pt;
}

size_t goto_char(size_t pos) {
    set_point(clip_to_bounds(0, pos, rope_char_length(current_buffer->rope)));
    return pos;
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
    
    size_t old_end_pos = rope_char_length(buf->rope);
    size_t end_pos = old_end_pos;
    
    // Track how many characters we're adding
    size_t chars_added = 0;
    
    if (prepend_newline) {
        buf->rope = rope_insert_chars(buf->rope, end_pos, "\n", 1);
        end_pos++;
        chars_added++;
    }
    
    buf->rope = rope_insert_chars(buf->rope, end_pos, text, text_len);
    
    // Count actual characters (not bytes) in the text
    for (size_t i = 0; i < text_len; ) {
        size_t bytes_read;
        utf8_decode(&text[i], text_len - i, &bytes_read);
        if (bytes_read == 0) break;
        i += bytes_read;
        chars_added++;
    }
    
    // Update buffer's point if it was at the end
    if (buf->pt == old_end_pos) {
        buf->pt = old_end_pos + chars_added;
    }
    
    // Update all windows viewing this buffer where point was at the end
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    for (int i = 0; i < count; i++) {
        Window *win = leaves[i];
        if (win->buffer == buf && win->point == old_end_pos) {
            win->point = old_end_pos + chars_added;
        }
    }
    
    buf->modified = true;
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
    
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    
    if (selected_frame->wm.minibuffer_active) {
        // Clear any existing echo area message first
        if (selected_frame->wm.minibuffer_message_start > 0) {
            size_t current_len = rope_char_length(minibuf->rope);
            if (current_len > selected_frame->wm.minibuffer_message_start) {
                size_t msg_len = current_len - selected_frame->wm.minibuffer_message_start;
                remove_text_properties(minibuf, selected_frame->wm.minibuffer_message_start, current_len);
                minibuf->rope = rope_delete_chars(minibuf->rope, 
                                                  selected_frame->wm.minibuffer_message_start, 
                                                  msg_len);
            }
        }
        
        // Append new message in brackets
        size_t original_len = rope_char_length(minibuf->rope);
        selected_frame->wm.minibuffer_message_start = original_len;  // Track where message starts
        
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
        selected_frame->wm.minibuffer_message_start = 0;  // Reset tracking
        size_t len = rope_char_length(minibuf->rope);
        if (len > 0) {
            clear_text_properties(minibuf);
            minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
        }
        
        size_t msg_len = strlen(formatted);
        minibuf->rope = rope_insert_chars(minibuf->rope, 0, formatted, msg_len);
        selected_frame->wm.minibuffer_window->point = 0;
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
size_t find_start_position(Buffer *buffer, Window *win, float *out_start_y) {
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
    float max_x = win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
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


// TODO Use frame->line_height and frame->column_width
// instead of querying the default face each frame to get line_height


// Helper to draw a character with its face properties
// Returns the advance width
static float draw_character_with_face(
    uint32_t ch,
    float x,
    float y,
    Face *face,
    Font *font,
    Color fg_color,
    bool draw_underline)
{
    // Draw the character
    float advance = character(font, ch, x, y, fg_color);
    
    // Draw underline if needed
    if (draw_underline) {
        int underline_minimum_offset = scm_get_int("underline-minimum-offset", 1);
        bool underline_at_descent_line = scm_get_bool("underline-at-descent-line", false);
        bool use_underline_position_properties = scm_get_bool("use-underline-position-properties", true);
        
        float underline_y;
        float underline_thickness;
        
        // Determine underline position
        if (underline_at_descent_line) {
            // Always use descent line for position when this is true
            underline_y = y - font->descent * 2;
        } else if (use_underline_position_properties) {
            underline_y = y - font->descent - font->underline_position;
        } else {
            // Default: minimum offset from baseline
            underline_y = y - font->descent - underline_minimum_offset;
        }
        
        // Determine underline thickness
        if (use_underline_position_properties) {
            underline_thickness = font->underline_thickness;
        } else {
            underline_thickness = 1.0f; // TODO This hardcoded 1 should come from the face
        }
        
        Character *char_info = font_get_character(font, ch);
        float underline_width = char_info ? char_info->ax : advance;
        
        quad2D((vec2){x, underline_y},
               (vec2){underline_width, underline_thickness},
               fg_color);
    }
    
    return advance;
}

// Helper to draw a background span for multiple characters with same background
static void draw_background_span(float x, float y, float width, float height, Color bg) {
    quad2D((vec2){x, y}, (vec2){width, height}, bg);
}

void draw_buffer(Buffer *buffer, Window *win, float start_x, float start_y) {
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    if (!default_font) return;
    
    Color base_bg = theme_cache->base_bg;
    Color base_fg = theme_cache->base_fg;
    
    // Use cached frame metrics
    float line_height = selected_frame->line_height;
    float max_x = start_x + (win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width));
    
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
    bool mark_is_set = (buffer->region.mark >= 0);
    
    float scroll_offset_y = 0;
    size_t start_pos = 0;
    
    if (win && !win->is_minibuffer) {
        start_pos = find_start_position(buffer, win, &scroll_offset_y);
    }
    
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
    
    float current_line_height = line_height;
    
    // Cursor tracking
    float cursor_x = 0, cursor_y = 0;
    float cursor_width = 0, cursor_height = 0;
    bool cursor_found = false;
    Color cursor_color;
    
    // Determine if we should draw filled cursor
    bool should_draw_filled = is_selected && selected_frame && selected_frame->focused;
    
    // Background batching
    float bg_span_start_x = 0;
    float bg_span_width = 0;
    float bg_span_y = 0;
    float bg_span_height = 0;
    Color bg_span_color = {0, 0, 0, 0};
    bool has_bg_span = false;
    
    while (rope_iter_next_char(&iter, &ch)) {
        // Get face info once
        int face_id = get_text_property_face(buffer, i);
        Face *face = (face_id == FACE_DEFAULT) ? default_face : get_face(face_id);
        if (!face) face = default_face;
        
        Font *char_font = get_face_font(face);
        if (!char_font) char_font = default_font;
        
        float font_height = char_font->ascent + char_font->descent;
        if (font_height > current_line_height) {
            current_line_height = font_height;
        }
        
        float char_width = character_width(char_font, ch);
        
        // Check state
        bool at_cursor = (i == point);
        bool at_mark = mark_is_set && is_selected && visible_mark_mode && 
                       i == (size_t)buffer->region.mark && (size_t)buffer->region.mark != point;
        
        // Check visibility
        if (y < window_bottom - current_line_height) {
            break;
        }
        
        if (ch == '\n') {
            // Flush any pending background span
            if (has_bg_span) {
                draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color);
                has_bg_span = false;
            }
            
            // Handle cursor at newline
            if (at_cursor) {
                cursor_x = x;
                cursor_y = y - (char_font->descent * 2);
                Character *space = font_get_character(char_font, ' ');
                cursor_width = space ? space->ax : char_font->ascent;
                cursor_height = font_height;
                cursor_color = get_face(FACE_CURSOR)->bg;
                cursor_found = true;
            }
            
            // Handle mark at newline
            if (at_mark && y >= window_bottom && y <= window_top) {
                Character *space = font_get_character(char_font, ' ');
                float mark_width = space ? space->ax : char_font->ascent;
                quad2D((vec2){x, y - (char_font->descent * 2)},
                       (vec2){mark_width, font_height}, 
                       get_face(FACE_VISIBLE_MARK)->bg);
            }
            
            x = line_start_x;
            y -= current_line_height;
            current_line_height = line_height;
        } else {
            // Handle truncation/wrapping
            if (truncate_lines) {
                if (x > max_x) {
                    // Flush background span
                    if (has_bg_span) {
                        draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color);
                        has_bg_span = false;
                    }
                    
                    // Skip to next line
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
                
                // Skip chars off left edge
                if (x + char_width < start_x) {
                    x += char_width;
                    i++;
                    continue;
                }
            } else {
                if (x + char_width > max_x) {
                    // Flush background span
                    if (has_bg_span) {
                        draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color);
                        has_bg_span = false;
                    }
                    
                    x = start_x;
                    y -= current_line_height;
                    current_line_height = line_height;
                    
                    if (y < window_bottom - current_line_height) {
                        break;
                    }
                }
            }
            
            // Determine colors
            Color char_color = face->fg;
            Color bg_color = face->bg;
            bool needs_bg = false;
            
            // Save cursor info if at cursor position
            if (at_cursor) {
                cursor_x = x;
                cursor_y = y - (char_font->descent * 2);
                Character *char_info = font_get_character(char_font, ch);
                cursor_width = char_info ? char_info->ax : char_width;
                cursor_height = font_height;
                
                if (crystal_point_mode && ch != '\n' && ch != ' ' && ch != '\t') {
                    cursor_color = face->fg;
                } else {
                    cursor_color = get_face(FACE_CURSOR)->bg;
                }
                cursor_found = true;
                
                // Modify character appearance if cursor should be drawn filled
                if (should_draw_filled && buffer->cursor.visible) {
                    char_color = base_bg;
                }
            } else if (at_mark) {
                bg_color = get_face(FACE_VISIBLE_MARK)->bg;
                char_color = base_bg;
                needs_bg = true;
            } else {
                needs_bg = !color_equals(bg_color, base_bg);
            }
            
            // Draw character if visible
            if (y >= window_bottom && y <= window_top) {
                // Handle background batching
                if (needs_bg && (!at_cursor || !should_draw_filled || !buffer->cursor.visible)) {
                    float bg_y = y - char_font->descent * 2;
                    float bg_height = font_height;
                    
                    // Check if we can extend the current background span
                    if (has_bg_span && 
                        color_equals(bg_color, bg_span_color) &&
                        bg_y == bg_span_y &&
                        bg_height == bg_span_height &&
                        x == bg_span_start_x + bg_span_width) {
                        // Extend the span
                        Character *char_info = font_get_character(char_font, ch);
                        bg_span_width += char_info ? char_info->ax : char_width;
                    } else {
                        // Flush previous span and start new one
                        if (has_bg_span) {
                            draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color);
                        }
                        
                        Character *char_info = font_get_character(char_font, ch);
                        bg_span_start_x = x;
                        bg_span_y = bg_y;
                        bg_span_width = char_info ? char_info->ax : char_width;
                        bg_span_height = bg_height;
                        bg_span_color = bg_color;
                        has_bg_span = true;
                    }
                } else {
                    // Flush any pending background span
                    if (has_bg_span) {
                        draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color);
                        has_bg_span = false;
                    }
                }
                
                // Draw character with underline if needed
                float advance = draw_character_with_face(ch, x, y, face, char_font, char_color, face->underline);
                x += advance;
            } else if (y >= window_bottom) {
                x += char_width;
            }
        }
        i++;
    }
    
    // Flush any remaining background span
    if (has_bg_span) {
        draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color);
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
        cursor_color = get_face(FACE_CURSOR)->bg;
        cursor_found = true;
    }
    
    // Update cursor position
    buffer->cursor.x = cursor_x;
    buffer->cursor.y = cursor_y;
    
    // Draw cursor
    if (cursor_found) {
        if (win && win->is_minibuffer && !selected_frame->wm.minibuffer_active) {
            return;
        }
        
        if (should_draw_filled) {
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
            if (is_selected) {
                buffer->cursor.visible = true;
                buffer->cursor.blink_count = 0;
            }
            
            float border_width = 1.0f;
            quad2D((vec2){cursor_x, cursor_y + cursor_height - border_width},
                   (vec2){cursor_width, border_width}, cursor_color);
            quad2D((vec2){cursor_x, cursor_y},
                   (vec2){cursor_width, border_width}, cursor_color);
            quad2D((vec2){cursor_x, cursor_y},
                   (vec2){border_width, cursor_height}, cursor_color);
            quad2D((vec2){cursor_x + cursor_width - border_width, cursor_y},
                   (vec2){border_width, cursor_height}, cursor_color);
        }
    }
}


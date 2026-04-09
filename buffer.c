#include "buffer.h"
#include "frame.h"
#include "theme.h"
#include "wm.h"
#include "lisp.h"
#include "faces.h"
#include "undo.h"
#include "fringe.h"
#include <obsidian/window.h>
#include <stdbool.h>
#include <stddef.h>

Buffer *all_buffers = NULL;
Buffer *current_buffer = NULL;

bool argument_manually_set = false;

SCM last_command = SCM_BOOL_F;
SCM last_command_event = SCM_BOOL_F;
bool last_command_was_kill = false;


bool is_kill_command(SCM proc) {
    return is_scm_proc(proc, "kill-word")
        || is_scm_proc(proc, "backward-kill-word")
        || is_scm_proc(proc, "kill-line")
        || is_scm_proc(proc, "kill-sexp")
        || is_scm_proc(proc, "kill-region");
}

#include <unistd.h>  // for getcwd

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
    buffer->region.active = false;
    buffer->region.mark = -1;
    textprop_store_init(&buffer->props);
    buffer->ts_state = NULL;

    buffer->newline_cache.newline_positions = NULL;
    buffer->newline_cache.count     = 0;
    buffer->newline_cache.capacity  = 0;
    buffer->newline_cache.valid     = false;
    buffer->newline_cache.last_line = 0;
    buffer->newline_cache.last_pos  = 0;

    buffer->wrap_cache.wrap_positions    = NULL;
    buffer->wrap_cache.count             = 0;
    buffer->wrap_cache.capacity          = 0;
    buffer->wrap_cache.valid             = false;
    buffer->wrap_cache.cached_window_width = 0.0f;

    undo_init(buffer);

    // Initialize buffer-local variables as empty alist
    buffer->local_var_alist = SCM_EOL;
    scm_gc_protect_object(buffer->local_var_alist);

    // Set default-directory buffer-local to cwd (with trailing slash)
    char cwd[PATH_MAX];
    char dir[PATH_MAX + 1];
    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        size_t len = strlen(cwd);
        memcpy(dir, cwd, len);
        if (cwd[len-1] != '/') { dir[len] = '/'; dir[len+1] = '\0'; }
        else { dir[len] = '\0'; }
    } else {
        strcpy(dir, "./");
    }

    SCM dd_sym = scm_from_locale_symbol("default-directory");
    SCM dd_val = scm_from_locale_string(dir);

    // Ensure the module variable exists so Scheme can resolve it as a bare variable.
    // If init.scm hasn't loaded yet (early buffer creation), we create it ourselves.
    // If it already exists from defvar-local, this just updates it.
    scm_c_define("default-directory", dd_val);

    // Also set it as a proper buffer-local in this buffer's alist
    buffer_set(dd_sym, dd_val, buffer);


    // Initialize keymap as NULL (will use global keymap)
    buffer->keymap = NULL;
    buffer->filename = NULL;
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

    // Init Minor Modes
    buffer->active_minor_modes = SCM_EOL;
    scm_gc_protect_object(buffer->active_minor_modes);

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
    // Unprotect Minor Modes
    scm_gc_unprotect_object(buffer->active_minor_modes);

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

    undo_cleanup(buffer->undo_state);
    free(buffer->newline_cache.newline_positions);
    free(buffer->wrap_cache.wrap_positions);

    free(buffer->filename);
    free(buffer->name);
    rope_free(buffer->rope);
    clear_text_properties(buffer);
    free(buffer);
}

void destroy_all_buffers(void) {
    if (!all_buffers) return;

    // Keep destroying until no buffers left
    while (all_buffers) {
        Buffer *to_destroy = all_buffers;

        // If this is the last buffer, all_buffers will become NULL
        if (to_destroy->next == to_destroy) {
            all_buffers = NULL;
        } else {
            // Move all_buffers pointer to next buffer before destroying
            all_buffers = to_destroy->next;
        }

        buffer_destroy(to_destroy);
    }

    current_buffer = NULL;
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

void set_buffer(Buffer *buf) {
    if (!buf || buf == current_buffer) return;

    current_buffer = buf;

    // Update keymap stack
    keymap_stack_clear();

    // 1. Add buffer-local keymap
    if (buf->keymap) {
        keymap_stack_push(buf->keymap);
    }

    // 2. Add all active minor mode keymaps
    SCM modes = buf->active_minor_modes;
    while (scm_is_pair(modes)) {
        SCM mode = scm_car(modes);

        // Build keymap variable name
        char *mode_name = scm_to_locale_string(scm_symbol_to_string(mode));
        char keymap_name[256];
        snprintf(keymap_name, sizeof(keymap_name), "%s-map", mode_name);
        free(mode_name);

        SCM keymap_var = scm_c_lookup(keymap_name);

        if (scm_is_true(keymap_var)) {
            SCM keymap_val = scm_variable_ref(keymap_var);
            if (!scm_is_false(keymap_val) && SCM_NIMP(keymap_val)) {
                KeyChordMap *map = scm_foreign_object_ref(keymap_val, 0);
                if (map) {
                    keymap_stack_push(map);
                }
            }
        }

        modes = scm_cdr(modes);
    }
}

void switch_to_buffer(Buffer *buf) {
    if (!buf) return;

    if (buf == selected_frame->wm.minibuffer_window->buffer) {
        message("Can't switch to minibuf buffer");
        return;
    }

    set_buffer(buf);

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

#include "minibuf.h"


void do_kill_buffer(Buffer *buf) {
    if (!buf) return;

    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    if (buf == minibuf) return;

    // Find a valid replacement (skip minibuffer and the buffer being killed)
    Buffer *replacement = buf->next;
    while (replacement == buf || replacement == minibuf) {
        replacement = replacement->next;
        if (replacement == buf) {
            replacement = buffer_create("*scratch*");
            break;
        }
    }

    // Update all windows pointing to this buffer
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    for (int i = 0; i < count; i++) {
        if (leaves[i]->buffer == buf) {
            leaves[i]->buffer = replacement;
            leaves[i]->point  = replacement->pt;
        }
    }

    if (buf == current_buffer)
        set_buffer(replacement);

    buffer_destroy(buf);
}

void kill_buffer() {
    do_kill_buffer(current_buffer);
}

void kill_current_buffer() {
    do_kill_buffer(current_buffer);
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

SCM scm_goto_char(SCM pos) {
    size_t n;
    if (SCM_UNBNDP(pos)) {
        SCM hist = scm_from_locale_symbol("goto-char-history");
        char *input = read_from_minibuffer("Goto char: ", NULL, hist);
        if (!input || !*input) {
            if (input) free(input);
            return SCM_UNSPECIFIED;
        }
        n = (size_t)atoi(input);
        free(input);
    } else {
        n = scm_to_size_t(pos);
    }
    goto_char(n);
    return SCM_UNSPECIFIED;
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

void append_to_buffer(Buffer *target, size_t start, size_t end) {
    if (!target) return;
    if (start >= end) return;

    size_t char_len = end - start;

    // Extract text from current buffer using rope_copy_chars
    // Allocate worst case: 4 bytes per UTF-8 char + null terminator
    size_t buf_size = char_len * 4 + 1;
    char *text = malloc(buf_size);
    if (!text) return;
    size_t bytes_copied = rope_copy_chars(current_buffer->rope, start, char_len,
                                          text, buf_size);
    text[bytes_copied] = '\0';

    // Insert before point in target buffer
    size_t insert_pos = target->pt;
    target->rope = rope_insert_chars(target->rope, insert_pos, text, bytes_copied);
    free(text);

    // Adjust target buffer point and windows
    target->pt = insert_pos + char_len;
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    for (int i = 0; i < count; i++) {
        Window *win = leaves[i];
        if (win->buffer == target && win->point >= insert_pos)
            win->point += char_len;
    }
    target->modified = true;
}

/* void append_to_buffer(Buffer *buf, const char *text, bool prepend_newline) { */
/*     if (!buf || !text) return; */

/*     size_t text_len = strlen(text); */
/*     if (text_len == 0) return; */

/*     size_t old_end_pos = rope_char_length(buf->rope); */
/*     size_t end_pos = old_end_pos; */

/*     // Track how many characters we're adding */
/*     size_t chars_added = 0; */

/*     if (prepend_newline) { */
/*         buf->rope = rope_insert_chars(buf->rope, end_pos, "\n", 1); */
/*         end_pos++; */
/*         chars_added++; */
/*     } */

/*     buf->rope = rope_insert_chars(buf->rope, end_pos, text, text_len); */

/*     // Count actual characters (not bytes) in the text */
/*     for (size_t i = 0; i < text_len; ) { */
/*         size_t bytes_read; */
/*         utf8_decode(&text[i], text_len - i, &bytes_read); */
/*         if (bytes_read == 0) break; */
/*         i += bytes_read; */
/*         chars_added++; */
/*     } */

/*     // Update buffer's point if it was at the end */
/*     if (buf->pt == old_end_pos) { */
/*         buf->pt = old_end_pos + chars_added; */
/*     } */

/*     // Update all windows viewing this buffer where point was at the end */
/*     Window *leaves[256]; */
/*     int count = 0; */
/*     collect_leaf_windows(selected_frame->wm.root, leaves, &count); */

/*     for (int i = 0; i < count; i++) { */
/*         Window *win = leaves[i]; */
/*         if (win->buffer == buf && win->point == old_end_pos) { */
/*             win->point = old_end_pos + chars_added; */
/*         } */
/*     } */

/*     buf->modified = true; */
/* } */


static void log_to_messages(const char *text) {
    Buffer *buf = get_buffer("*Messages*");
    if (!buf || !text) return;
    size_t text_len = strlen(text);
    if (text_len == 0) return;
    size_t old_end_pos = rope_char_length(buf->rope);
    size_t end_pos = old_end_pos;
    size_t chars_added = 0;
    bool prepend_newline = old_end_pos > 0;
    if (prepend_newline) {
        buf->rope = rope_insert_chars(buf->rope, end_pos, "\n", 1);
        end_pos++;
        chars_added++;
    }
    buf->rope = rope_insert_chars(buf->rope, end_pos, text, text_len);
    for (size_t i = 0; i < text_len; ) {
        size_t bytes_read;
        utf8_decode(&text[i], text_len - i, &bytes_read);
        if (bytes_read == 0) break;
        i += bytes_read;
        chars_added++;
    }
    if (buf->pt == old_end_pos)
        buf->pt = old_end_pos + chars_added;
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    for (int i = 0; i < count; i++) {
        Window *win = leaves[i];
        if (win->buffer == buf && win->point == old_end_pos)
            win->point = old_end_pos + chars_added;
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
        log_to_messages(formatted);
    }

    free(formatted);
}

void message_for(double seconds, const char *format, ...) {
    va_list args;
    va_start(args, format);
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy) + 1;
    va_end(args_copy);
    char *formatted = malloc(needed);
    vsnprintf(formatted, needed, format, args);
    va_end(args);

    message("%s", formatted);
    free(formatted);

    selected_frame->message_expiry = getTime() + seconds;
}

/// Newline cache

void newline_cache_invalidate(Buffer *buf) {
    buf->newline_cache.valid     = false;
    buf->newline_cache.last_line = 0;
    buf->newline_cache.last_pos  = 0;
}

static void newline_cache_collect(size_t char_pos, void *userdata) {
    NewlineCache *cache = (NewlineCache *)userdata;
    cache->newline_positions[cache->count++] = char_pos;
}

static void newline_cache_rebuild(Buffer *buf) {
    NewlineCache *cache = &buf->newline_cache;

    size_t expected = rope_stats(buf->rope).newlines;
    if (expected + 1 > cache->capacity) {
        cache->capacity = (expected + 1) * 2;
        cache->newline_positions = realloc(cache->newline_positions,
                                           cache->capacity * sizeof(size_t));
    }
    cache->count = 0;

    rope_each_newline(buf->rope, newline_cache_collect, cache);

    cache->valid     = true;
    cache->last_line = 0;
    cache->last_pos  = 0;
}

static inline void newline_cache_ensure(Buffer *buf) {
    if (!buf->newline_cache.valid)
        newline_cache_rebuild(buf);
}

// Newline cache public API — matching Emacs function names
// All O(log n) after the cache is built, O(n) on first call after modification

// Internal: 0-based line index for char_pos (number of newlines before pos)
static size_t charpos_to_linenum(Buffer *buf, size_t char_pos) {
    newline_cache_ensure(buf);
    NewlineCache *cache = &buf->newline_cache;
    if (cache->count == 0) return 0;
    size_t lo = 0, hi = cache->count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (cache->newline_positions[mid] < char_pos)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo;
}

// Internal: char pos of start of 0-based line N
static size_t linenum_to_charpos(Buffer *buf, size_t line) {
    newline_cache_ensure(buf);
    NewlineCache *cache = &buf->newline_cache;
    if (line == 0) return 0;
    if (line > cache->count) return rope_char_length(buf->rope);
    return cache->newline_positions[line - 1] + 1;
}

// count_lines(start, end) — number of newlines in [start, end)
// Equivalent to Emacs count_lines()
ptrdiff_t count_lines(Buffer *buf, size_t start, size_t end) {
    if (start >= end) return 0;
    return (ptrdiff_t)(charpos_to_linenum(buf, end) -
                       charpos_to_linenum(buf, start));
}

// line_number_at_pos(pos) — 1-based line number at char pos
// Equivalent to Emacs line_number_at_pos()
ptrdiff_t line_number_at_pos(Buffer *buf, size_t pos) {
    return (ptrdiff_t)charpos_to_linenum(buf, pos) + 1;
}

// line_at_char(pos) — char pos of the beginning of the line containing pos
// Equivalent to Emacs line_beginning_position() internals
size_t line_at_char(Buffer *buf, size_t pos) {
    if (pos == 0) return 0;
    return linenum_to_charpos(buf, charpos_to_linenum(buf, pos));
}

// next_line_at_char(pos) — char pos of the beginning of the line after pos
size_t next_line_at_char(Buffer *buf, size_t pos) {
    size_t text_len = rope_char_length(buf->rope);
    if (pos >= text_len) return text_len;
    return linenum_to_charpos(buf, charpos_to_linenum(buf, pos) + 1);
}

// buffer_line_count() — total number of lines (always >= 1)
// Equivalent to Emacs (line-number-at-pos (point-max))
size_t buffer_line_count(Buffer *buf) {
    newline_cache_ensure(buf);
    return buf->newline_cache.count + 1;
}

void wrap_cache_invalidate(Buffer *buf) {
    buf->wrap_cache.valid = false;
    buf->wrap_cache.count = 0;
}

void wrap_cache_ensure(Buffer *buf, float window_width, float max_x) {
    if (buf->wrap_cache.valid &&
        buf->wrap_cache.cached_window_width == window_width)
        return;

    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    if (!default_font) return;

    WrapCache *cache = &buf->wrap_cache;
    cache->count = 0;

    size_t buf_len = rope_char_length(buf->rope);
    float x = 0;

    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, 0);
    uint32_t ch;
    size_t i = 0;

    while (rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            x = 0;
        } else {
            Character *ci = font_get_character(default_font, ch);
            float char_width = ci ? ci->ax : 0;
            if (x + char_width > max_x) {
                if (cache->count >= cache->capacity) {
                    cache->capacity = cache->capacity ? cache->capacity * 2 : 1024;
                    cache->wrap_positions = realloc(cache->wrap_positions,
                                                    cache->capacity * sizeof(size_t));
                }
                cache->wrap_positions[cache->count++] = i;
                x = char_width;
            } else {
                x += char_width;
            }
        }
        i++;
    }
    rope_iter_destroy(&iter);

    cache->valid = true;
    cache->cached_window_width = window_width;
}

// Returns the visual line number (0-based) at char_pos,
// counting both logical newlines and visual wraps before it.
size_t wrap_cache_visual_line_at(Buffer *buf, size_t char_pos) {
    size_t logical = (size_t)line_number_at_pos(buf, char_pos) - 1;

    WrapCache *cache = &buf->wrap_cache;
    if (!cache->valid || cache->count == 0)
        return logical;

    // Binary search: count wrap_positions < char_pos that belong to lines <= logical line
    // We need wraps that occur before char_pos AND after the start of logical line 0..logical-1
    // Since wrap_positions are in order, just count those < char_pos
    size_t lo = 0, hi = cache->count;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (cache->wrap_positions[mid] < char_pos)
            lo = mid + 1;
        else
            hi = mid;
    }
    // lo = number of wraps before char_pos
    return logical + lo;
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

/// Minor modes

void enable_minor_mode(SCM mode_symbol, Buffer *buf) {
    if (!buf) return;

    // Check if already active
    if (scm_is_true(scm_memq(mode_symbol, buf->active_minor_modes))) {
        return;
    }

    // Add to active list
    scm_gc_unprotect_object(buf->active_minor_modes);
    buf->active_minor_modes = scm_cons(mode_symbol, buf->active_minor_modes);
    scm_gc_protect_object(buf->active_minor_modes);

    // If this is current buffer, update keymap stack
    if (buf == current_buffer) {
        // Build keymap variable name: "mode-name-map"
        char *mode_name = scm_to_locale_string(scm_symbol_to_string(mode_symbol));
        char keymap_name[256];
        snprintf(keymap_name, sizeof(keymap_name), "%s-map", mode_name);
        free(mode_name);

        // Try to lookup the keymap variable
        SCM keymap_var = scm_c_lookup(keymap_name);

        if (scm_is_true(keymap_var)) {
            SCM keymap_val = scm_variable_ref(keymap_var);
            // Check if it's a foreign object (keymap)
            if (!scm_is_false(keymap_val) && SCM_NIMP(keymap_val)) {
                // Try to extract as foreign object
                KeyChordMap *map = scm_foreign_object_ref(keymap_val, 0);
                if (map) {
                    keymap_stack_push(map);
                }
            }
        }
    }
}

void disable_minor_mode(SCM mode_symbol, Buffer *buf) {
    if (!buf) return;

    // Remove from active list
    scm_gc_unprotect_object(buf->active_minor_modes);
    buf->active_minor_modes = scm_delq(mode_symbol, buf->active_minor_modes);
    scm_gc_protect_object(buf->active_minor_modes);

    // If this is current buffer, rebuild keymap stack
    if (buf == current_buffer) {
        keymap_stack_clear();

        // Re-add buffer-local keymap if it exists
        if (buf->keymap) {
            keymap_stack_push(buf->keymap);
        }

        // Re-add all active minor mode keymaps
        SCM modes = buf->active_minor_modes;
        while (scm_is_pair(modes)) {
            SCM mode = scm_car(modes);

            // Build keymap variable name
            char *mode_name = scm_to_locale_string(scm_symbol_to_string(mode));
            char keymap_name[256];
            snprintf(keymap_name, sizeof(keymap_name), "%s-map", mode_name);
            free(mode_name);

            SCM keymap_var = scm_c_lookup(keymap_name);

            if (scm_is_true(keymap_var)) {
                SCM keymap_val = scm_variable_ref(keymap_var);
                if (!scm_is_false(keymap_val) && SCM_NIMP(keymap_val)) {
                    KeyChordMap *map = scm_foreign_object_ref(keymap_val, 0);
                    if (map) {
                        keymap_stack_push(map);
                    }
                }
            }
            modes = scm_cdr(modes);
        }
    }
}

bool minor_mode_active_p(SCM mode_symbol, Buffer *buf) {
    if (!buf) return false;
    return scm_is_true(scm_memq(mode_symbol, buf->active_minor_modes));
}


/// Draw

// ═══════════════════════════════════════════════════════════════════════════════
// GLOBAL SYMBOL / FACE CACHE
// Call init_draw_cache() once at startup (after Guile and faces are initialized).
// Every frame-hot SCM symbol is stored as a static SCM so scm_from_utf8_symbol
// is never called in the draw path.
// ═══════════════════════════════════════════════════════════════════════════════

typedef struct {
    // Interned symbols used in draw_buffer
    SCM face;
    SCM display;
    SCM space;
    SCM truncate_lines;
    SCM tab_width;
    // Property keywords for display specs
    SCM align_to;
    SCM width_kw;
    // Fringe
    SCM fringe_indicator_alist;
} DrawSymbols;


static DrawSymbols g_sym = {0};

// Pre-resolved underline/strikethrough geometry — updated whenever the
// relevant variables change (or just once per frame before drawing).
typedef struct {
    bool  underline_at_descent_line;
    bool  use_underline_pos;
    int   underline_minimum_offset;
    // Derived per-font values are cheap to compute inline; store the flags only.
} UnderlineConfig;

static UnderlineConfig g_ul = {0};

void init_draw_cache(void) {
    g_sym.face          = scm_from_utf8_symbol("face");
    g_sym.display       = scm_from_utf8_symbol("display");
    g_sym.space         = scm_from_utf8_symbol("space");
    g_sym.truncate_lines = scm_from_utf8_symbol("truncate-lines");
    g_sym.tab_width     = scm_from_utf8_symbol("tab-width");
    g_sym.align_to      = scm_from_utf8_symbol(":align-to");
    g_sym.width_kw      = scm_from_utf8_symbol(":width");

    // Protect all from GC
    scm_gc_protect_object(g_sym.face);
    scm_gc_protect_object(g_sym.display);
    scm_gc_protect_object(g_sym.space);
    scm_gc_protect_object(g_sym.truncate_lines);
    scm_gc_protect_object(g_sym.tab_width);
    scm_gc_protect_object(g_sym.align_to);
    scm_gc_protect_object(g_sym.width_kw);

    g_sym.fringe_indicator_alist = scm_from_utf8_symbol("fringe-indicator-alist");
    scm_gc_protect_object(g_sym.fringe_indicator_alist);
}

// Call once per frame (very cheap — just reads Scheme variables).
void refresh_draw_config(void) {
    g_ul.underline_at_descent_line = scm_get_bool("underline-at-descent-line", false);
    g_ul.use_underline_pos         = scm_get_bool("use-underline-position-properties", true);
    g_ul.underline_minimum_offset  = scm_get_int("underline-minimum-offset", 1);
}

// (no SCM calls — uses pre-resolved g_ul config)
static float draw_character_with_face(
    uint32_t ch,
    float x, float y,
    Face *face, Font *font,
    Color fg_color,
    bool draw_underline,    Color underline_color,
    bool draw_strike_through, Color strike_through_color)
{
    float advance = character(font, ch, x, y, fg_color);

    if (draw_underline) {
        Character *ci = font_get_character(font, ch);
        float char_width = ci ? ci->ax : advance;

        float underline_y, underline_thickness;
        if (g_ul.underline_at_descent_line) {
            underline_y = y - font->descent * 2;
        } else if (g_ul.use_underline_pos) {
            underline_y = y - font->descent - font->underline_position;
        } else {
            underline_y = y - font->descent - g_ul.underline_minimum_offset;
        }
        underline_thickness = g_ul.use_underline_pos ? font->underline_thickness : 1.0f;

        quad2D((vec2){x, underline_y}, (vec2){char_width, underline_thickness}, underline_color);
    }

    if (draw_strike_through) {
        Character *ci = font_get_character(font, ch);
        float char_width = ci ? ci->ax : advance;
        quad2D((vec2){x, y}, (vec2){char_width, 1.0f}, strike_through_color);
    }

    return advance;
}

// (no per-char SCM/face lookups in the wrap walk)
size_t find_start_position(Buffer *buffer, Window *win, float *out_start_y) {
    if (!win || win->is_minibuffer) { *out_start_y = 0; return 0; }

    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    if (!default_font) { *out_start_y = 0; return 0; }

    float line_height = default_font->ascent + default_font->descent;
    float max_x = win->width - (selected_frame->left_fringe_width +
                                 selected_frame->right_fringe_width);

    // Use cached symbol — no allocation.
    SCM truncate_lines_val = buffer_local_value(g_sym.truncate_lines, buffer);
    bool truncate_lines = scm_is_true(truncate_lines_val);

    float lines_scrolled = win->scrolly / line_height;
    int skip_lines = (int)lines_scrolled - 2;
    if (skip_lines < 0) skip_lines = 0;

    size_t total_lines = buffer_line_count(buffer);

    if (truncate_lines) {
        size_t target_line = (size_t)skip_lines;
        if (target_line >= total_lines) target_line = total_lines > 0 ? total_lines - 1 : 0;
        size_t pos = linenum_to_charpos(buffer, target_line);
        *out_start_y = target_line * line_height;
        return pos;
    }

    // Wrapping mode: walk forward from the skip_lines-th logical line,
    // counting visual wrap-lines — but use DEFAULT font only for width
    // measurement.  Face-specific fonts are rare and the error is sub-pixel
    // for a scroll-position estimate; this avoids one AVL stab per character.
    size_t start_logical_line = (size_t)skip_lines;
    if (start_logical_line >= total_lines)
        start_logical_line = total_lines > 0 ? total_lines - 1 : 0;

    size_t pos = linenum_to_charpos(buffer, start_logical_line);
    int current_line = (int)start_logical_line;
    float x = 0;

    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, pos);
    uint32_t ch;

    while (rope_iter_next_char(&iter, &ch) && current_line < skip_lines) {
        if (ch == '\n') {
            current_line++;
            x = 0;
        } else {
            // Use default font — avoids get_text_property_face() tree stab.
            // For the rare mixed-font buffer the wrap estimate is off by at most
            // one visual line, which the -2 slack in skip_lines already covers.
            Character *ci = font_get_character(default_font, ch);
            float char_width = ci ? ci->ax : 0;
            if (x + char_width > max_x) {
                current_line++;
                x = char_width;  // character starts the new visual line
            } else {
                x += char_width;
            }
        }
        pos++;
    }
    rope_iter_destroy(&iter);

    *out_start_y = current_line * line_height;
    return pos;
}

static void draw_box_span(float x, float y, float width, float height,
                          Color color, float thickness,
                          bool draw_left, bool draw_right) {
    // Top line
    quad2D((vec2){x, y + height},
           (vec2){width, thickness},
           color);

    // Bottom line
    quad2D((vec2){x, y - thickness},
           (vec2){width, thickness},
           color);

    // Left line (conditional)
    if (draw_left) {
        quad2D((vec2){x, y},
               (vec2){thickness, height},
               color);
    }

    // Right line (conditional)
    if (draw_right) {
        quad2D((vec2){x + width - thickness, y},
               (vec2){thickness, height},
               color);
    }
}


static void flush_box_span(float box_span_start_x, float box_span_y,
                          float box_span_width, float box_span_height,
                          Color box_span_color, float thickness,
                          bool no_left, bool no_right) {
    draw_box_span(box_span_start_x, box_span_y, box_span_width, box_span_height,
                  box_span_color, thickness, !no_left, !no_right);
}

// Helper to draw a background span for multiple characters with same background
static void draw_background_span(float x, float y, float width, float height, Color bg) {
    quad2D((vec2){x, y}, (vec2){width, height}, bg);
}

static void draw_face_extension(float x, float y, float max_x,
                                Face *face, Font *font,
                                Color bg_color, float box_thickness,
                                bool face_has_box) {
    if (!face->extend || !face->bg_set) return;

    float extend_width = max_x - x;
    if (extend_width <= 0) return;

    float extend_y = y - (font->descent * 2);
    float extend_height = font->ascent + font->descent;

    // Apply box offset if the face has a box
    if (face_has_box) {
        extend_y -= box_thickness;
        extend_height += (box_thickness * 2);
    }

    draw_background_span(x, extend_y, extend_width, extend_height, bg_color);
}


// Scans forward from `pos` through face runs only (no per-character work)
// to check whether any run before `line_end` has a box face.
// `line_end` must be the char position of the start of the NEXT line
// (i.e. the result of next_line_at_char(buf, any_pos_on_this_line)),
// which is already the position right after the '\n'.
// Cost: O(face-runs on this one logical line) — typically 1-5 tree lookups.
static bool line_has_box_face_fast(Buffer *buffer, size_t pos, size_t line_end) {
    while (pos < line_end) {
        size_t run_end;
        int    fid  = get_face_id_and_next_change(buffer, pos, line_end, &run_end);
        Face  *face = get_face(fid);
        if (face && face->box) return true;
        pos = run_end;
    }
    return false;
}



float measure_line_max_font_height(Buffer *buffer, size_t line_start,
                                          size_t line_end, float default_lh) {
    float max_h = default_lh;
    size_t pos = line_start;
    while (pos < line_end) {
        size_t run_end;
        int fid = get_face_id_and_next_change(buffer, pos, line_end, &run_end);
        Face *f = get_face(fid);
        Font *fn = f ? get_face_font(f) : NULL;
        if (fn) {
            float h = fn->ascent + fn->descent;
            if (f->box) h += 2.0f;
            if (h > max_h) max_h = h;
        }
        pos = run_end;
    }
    return max_h;
}

void draw_buffer(Buffer *buffer, Window *win, float start_x, float start_y) {
    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    if (!default_font) return;

    Color base_bg     = theme_cache->base_bg;
    float line_height = selected_frame->line_height;
    float max_x       = start_x + (win->width - (selected_frame->left_fringe_width +
                                                   selected_frame->right_fringe_width));
    size_t buf_len = rope_char_length(buffer->rope);

    SCM truncate_lines_val = buffer_local_value(g_sym.truncate_lines, buffer);
    bool truncate_lines    = scm_is_true(truncate_lines_val);

    SCM tab_width_val     = buffer_local_value(g_sym.tab_width, buffer);
    int   tab_width       = scm_to_int(tab_width_val);
    float tab_pixel_width = tab_width * selected_frame->column_width;

    bool stretch_cursor         = scm_get_bool("stretch-cursor", false);
    bool blink_cursor_mode      = scm_get_bool("blink-cursor-mode", true);
    bool cursor_in_non_selected = scm_get_bool("cursor-in-non-selected-windows", true);
    bool transient_mark_mode    = scm_get_bool("transient-mark-mode", false);
    bool visible_mark_mode      = scm_get_bool("visible-mark-mode", false);
    bool crystal_point_mode     = scm_get_bool("crystal-point-mode", false);
    size_t blink_cursor_blinks   = scm_get_size_t("blink-cursor-blinks", 0);
    float  blink_cursor_interval = scm_get_float("blink-cursor-interval", 0.1f);
    float  blink_cursor_delay    = scm_get_float("blink-cursor-delay", 0.1f);

    float window_bottom = win->y + (win->is_minibuffer ? 0 : line_height);
    float window_top    = win->y + win->height;

    size_t point       = win ? win->point : buffer->pt;
    bool   is_selected = win && win->is_selected;
    bool   mark_is_set = (buffer->region.mark >= 0);

    bool   has_region    = false;
    size_t region_start  = 0, region_end = 0;
    Face  *region_face   = get_face(FACE_REGION);
    bool   region_extend = region_face && region_face->extend;

    if (mark_is_set && is_selected && transient_mark_mode && buffer->region.active) {
        has_region   = true;
        region_start = ((size_t)buffer->region.mark < point)
                       ? (size_t)buffer->region.mark : point;
        region_end   = ((size_t)buffer->region.mark < point)
                       ? point : (size_t)buffer->region.mark;
    }

    bool should_draw_filled = is_selected && selected_frame && selected_frame->focused;

    float scroll_offset_y = 0;
    size_t start_pos = 0;
    if (win && !win->is_minibuffer)
        start_pos = find_start_position(buffer, win, &scroll_offset_y);

    size_t vis_end_limit = start_pos + (size_t)(win->height / line_height + 2)
                         * (size_t)(max_x / selected_frame->column_width + 2);
    if (vis_end_limit > buf_len) vis_end_limit = buf_len;
    textprop_set_viewport(current_buffer, start_pos, vis_end_limit);

    float scroll_offset_x = (win && !win->is_minibuffer && truncate_lines) ? win->scrollx : 0;
    float line_start_x    = start_x - scroll_offset_x;
    float x = line_start_x;
    float y = start_y + (win && !win->is_minibuffer ? win->scrolly : 0) - scroll_offset_y
        - default_font->descent;

    float box_line_thickness            = 1.0f;
    bool  current_line_has_box          = false;
    bool  previous_line_had_wrapped_box = false;
    float current_line_height           = line_height;

    size_t current_line_end = next_line_at_char(buffer, start_pos);

    // Pre-scan the first line's max font height for baseline alignment
    float line_max_font_height = measure_line_max_font_height(buffer, start_pos,
                                                               current_line_end, line_height);

    if (line_has_box_face_fast(buffer, start_pos, current_line_end)) {
        y                    -= box_line_thickness;
        current_line_has_box  = true;
    }

    float cursor_x = 0, cursor_y = 0, cursor_width = 0, cursor_height = 0;
    bool  cursor_found = false;
    Color cursor_color;

    float bg_span_start_x = 0, bg_span_width = 0, bg_span_y = 0, bg_span_height = 0;
    Color bg_span_color = {0};
    bool  has_bg_span   = false;

    float box_span_start_x = 0, box_span_width = 0, box_span_y = 0, box_span_height = 0;
    Color box_span_color   = {0};
    bool  has_box_span     = false;
    bool  box_span_no_left = false, box_span_no_right = false;
    bool  was_in_box       = false;

    Face  *last_drawn_face   = NULL;
    Font  *last_drawn_font   = NULL;
    Color  last_drawn_bg     = base_bg;
    bool   last_face_had_box = false;

    size_t i = start_pos;

    size_t face_run_end;
    int current_face_id = get_face_id_and_next_change(buffer, i, buf_len, &face_run_end);

    SCM    current_display_prop = get_text_property(buffer, i, g_sym.display);
    size_t display_run_end      = next_single_property_change(buffer, i, g_sym.display);

    size_t next_run_end = face_run_end < display_run_end ? face_run_end : display_run_end;
    int    prev_face_id = current_face_id;

    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, start_pos);
    uint32_t ch;

    while (rope_iter_next_char(&iter, &ch)) {

        if (i >= next_run_end) {
            if (i >= face_run_end)
                current_face_id = get_face_id_and_next_change(buffer, i, buf_len, &face_run_end);
            if (i >= display_run_end) {
                current_display_prop = get_text_property(buffer, i, g_sym.display);
                display_run_end      = next_single_property_change(buffer, i, g_sym.display);
            }
            next_run_end = face_run_end < display_run_end ? face_run_end : display_run_end;
        }

        Face *face = (current_face_id == FACE_DEFAULT) ? default_face : get_face(current_face_id);
        if (!face) face = default_face;
        Font *char_font = get_face_font(face);
        if (!char_font) char_font = default_font;

        float font_height = char_font->ascent + char_font->descent;

        float effective_font_height = face->box
            ? font_height + box_line_thickness * 2
            : font_height;
        if (effective_font_height > current_line_height)
            current_line_height = effective_font_height;

        float      char_width;
        bool       is_control_char = (ch < 0x20 && ch != '\t' && ch != '\n') || ch == 0x7f;
        Character *cached_char_info = NULL;

        if (ch == '\t') {
            char_width = tab_pixel_width;
            if (scm_is_pair(current_display_prop) &&
                scm_is_symbol(scm_car(current_display_prop)) &&
                scm_is_eq(scm_car(current_display_prop), g_sym.space)) {
                SCM plist = scm_cdr(current_display_prop);
                while (scm_is_pair(plist) && scm_is_pair(scm_cdr(plist))) {
                    SCM key = scm_car(plist);
                    SCM val = scm_cadr(plist);
                    if (scm_is_eq(key, g_sym.align_to) && scm_is_number(val)) {
                        float target_x = start_x + (float)scm_to_double(val);
                        char_width = target_x - x;
                        if (char_width < 0) char_width = 0;
                    } else if (scm_is_eq(key, g_sym.width_kw) && scm_is_number(val)) {
                        char_width = (float)scm_to_double(val) * selected_frame->column_width;
                    }
                    plist = scm_cddr(plist);
                }
            }
        } else if (is_control_char) {
            uint32_t visible = (ch == 0x7f) ? '?' : (ch + 64);
            char_width = character_width(char_font, '^') +
                         character_width(char_font, visible);
        } else {
            cached_char_info = font_get_character(char_font, ch);
            char_width = cached_char_info ? cached_char_info->ax : 0;
        }

        float acw = (ch == '\t') ? char_width
                                 : (cached_char_info ? cached_char_info->ax : char_width);

        bool at_cursor = (i == point);
        bool at_mark   = mark_is_set && is_selected && visible_mark_mode &&
                         !transient_mark_mode &&
                         i == (size_t)buffer->region.mark &&
                         (size_t)buffer->region.mark != point;
        bool in_region = has_region && i >= region_start && i < region_end;

        if (y < window_bottom - current_line_height) break;

        // ══════════════════════════════════════════════════════════════════════
        // NEWLINE
        // ══════════════════════════════════════════════════════════════════════
        if (ch == '\n') {
            if (has_bg_span) {
                draw_background_span(bg_span_start_x, bg_span_y, bg_span_width,
                                     bg_span_height, bg_span_color);
                has_bg_span = false;
            }
            if (y >= window_bottom && y <= window_top) {
                if (in_region && region_extend && region_face) {
                    float ext_width = max_x - x;
                    if (ext_width > 0)
                        draw_background_span(x, y - current_line_height + (default_font->descent * 3.4f),
                                             ext_width, current_line_height, region_face->bg);
                } else {
                    draw_face_extension(x, y, max_x, face, char_font,
                                        face->bg, box_line_thickness, face->box);
                }
            }
            if (has_box_span) {
                flush_box_span(box_span_start_x, box_span_y, box_span_width, box_span_height,
                               box_span_color, box_line_thickness, box_span_no_left, false);
                has_box_span = false;
            }

            if (at_cursor) {
                bool wide_nl_cursor = scm_get_bool("wide-newline-cursor", false);
                float nl_base_y = current_line_has_box ? y - box_line_thickness : y;
                float default_font_height = default_font->ascent + default_font->descent;
                float nl_draw_y = nl_base_y - (line_max_font_height - default_font_height)
                    + (char_font->descent * 2 - default_font->descent);
                cursor_x      = face->box ? x + box_line_thickness : x;
                cursor_y      = nl_draw_y - char_font->descent * 2;

                if (wide_nl_cursor) {
                    // find line start by scanning back from i
                    size_t line_start = i;
                    while (line_start > 0) {
                        uint32_t pc = rope_char_at(buffer->rope, line_start - 1);
                        if (pc == '\n') break;
                        line_start--;
                    }
                    float max_cw = 0;
                    rope_iter_t si;
                    rope_iter_init(&si, buffer->rope, line_start);
                    size_t sp = line_start;
                    uint32_t sc;
                    while (rope_iter_next_char(&si, &sc) && sc != '\n' && sp < i) {
                        size_t run_end;
                        int sfid = get_face_id_and_next_change(buffer, sp, i, &run_end);
                        Face *sf = get_face(sfid);
                        Font *sfn = sf ? get_face_font(sf) : default_font;
                        if (!sfn) sfn = default_font;
                        Character *sci = font_get_character(sfn, sc);
                        float scw = sci ? sci->ax : 0;
                        if (scw > max_cw) max_cw = scw;
                        sp++;
                    }
                    rope_iter_destroy(&si);
                    Character *spc = font_get_character(char_font, ' ');
                    cursor_width = max_cw > 0 ? max_cw : (spc ? spc->ax : char_font->ascent);
                } else {
                    Character *sp = font_get_character(char_font, ' ');
                    cursor_width  = sp ? sp->ax : char_font->ascent;
                }

                cursor_height = line_max_font_height;
                cursor_color  = get_face(FACE_CURSOR)->bg;
                cursor_found  = true;
            }






            if (at_mark && y >= window_bottom && y <= window_top) {
                float      mx = face->box ? x + box_line_thickness : x;
                float      my = y - char_font->descent * 2;
                Character *sp = font_get_character(char_font, ' ');
                float      mw = sp ? sp->ax : char_font->ascent;
                quad2D((vec2){mx, my}, (vec2){mw, font_height}, get_face(FACE_VISIBLE_MARK)->bg);
            }

            x = line_start_x;
            y -= current_line_height;
            if (previous_line_had_wrapped_box) y -= box_line_thickness * 2;
            current_line_height           = line_height;
            current_line_has_box          = false;
            was_in_box                    = false;
            box_span_no_left              = false;
            box_span_no_right             = false;
            previous_line_had_wrapped_box = false;
            last_drawn_face               = NULL;
            last_drawn_font               = NULL;
            last_drawn_bg                 = base_bg;
            last_face_had_box             = false;

            current_line_end = next_line_at_char(buffer, i + 1);
            // Pre-scan next line's max font height for baseline alignment
            line_max_font_height = measure_line_max_font_height(buffer, i + 1,
                                                                 current_line_end, line_height);
            if (line_has_box_face_fast(buffer, i + 1, current_line_end)) {
                y -= box_line_thickness;
                current_line_has_box = true;
            }

        // ══════════════════════════════════════════════════════════════════════
        // NORMAL CHARACTER
        // ══════════════════════════════════════════════════════════════════════
        } else {
            if (truncate_lines) {
                if (x > max_x) {
                    if (has_bg_span) { draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color); has_bg_span = false; }
                    if (last_drawn_face && y >= window_bottom && y <= window_top)
                        draw_face_extension(x, y, max_x, last_drawn_face, last_drawn_font, last_drawn_bg, box_line_thickness, last_face_had_box);
                    if (has_box_span) { flush_box_span(box_span_start_x, box_span_y, box_span_width, box_span_height, box_span_color, box_line_thickness, box_span_no_left, false); has_box_span = false; }

                    // Draw truncation arrow on the right fringe.
                    if (y >= window_bottom && y <= window_top) {
                        FringeIndicatorPair trunc = fringe_resolve_indicator(buffer, FRINGE_IND_TRUNCATION);
                        fringe_draw_pair(trunc, false, true, win, y, current_line_height);
                    }

                    while (rope_iter_next_char(&iter, &ch) && ch != '\n') i++;
                    if (ch == '\n') {
                        x = line_start_x;
                        y -= current_line_height;
                        if (previous_line_had_wrapped_box) y -= box_line_thickness * 2;
                        current_line_height           = line_height;
                        current_line_has_box          = false;
                        was_in_box                    = false;
                        box_span_no_left              = false;
                        box_span_no_right             = false;
                        previous_line_had_wrapped_box = false;
                        last_drawn_face               = NULL;
                        last_drawn_font               = NULL;
                        last_drawn_bg                 = base_bg;
                        last_face_had_box             = false;
                        current_line_end = next_line_at_char(buffer, i + 1);
                        line_max_font_height = measure_line_max_font_height(buffer, i + 1,
                                                                             current_line_end, line_height);
                        if (line_has_box_face_fast(buffer, i + 1, current_line_end)) {
                            y -= box_line_thickness;
                            current_line_has_box = true;
                        }
                    }
                    i += 2; continue;
                }
                if (x + char_width < start_x) {
                    prev_face_id = current_face_id; x += char_width; i++; continue;
                }
            } else {
                if (x + char_width > max_x) {
                    bool wcb = face->box;
                    if (has_bg_span) { draw_background_span(bg_span_start_x, bg_span_y, bg_span_width, bg_span_height, bg_span_color); has_bg_span = false; }
                    if (last_drawn_face && y >= window_bottom && y <= window_top)
                        draw_face_extension(x, y, max_x, last_drawn_face, last_drawn_font, last_drawn_bg, box_line_thickness, last_face_had_box);
                    if (has_box_span) { flush_box_span(box_span_start_x, box_span_y, box_span_width, box_span_height, box_span_color, box_line_thickness, box_span_no_left, wcb); has_box_span = false; }

                    // Right-curly goes on the line that just wrapped (current y, right fringe).
                    // Left-curly  goes on the new visual line     (y after decrement, left fringe).
                    if (y >= window_bottom && y <= window_top) {
                        FringeIndicatorPair cont = fringe_resolve_indicator(buffer, FRINGE_IND_CONTINUATION);
                        fringe_draw_pair(cont, false, true, win, y, current_line_height);
                    }
                    {
                        float next_y = y - current_line_height;
                        if (next_y >= window_bottom && next_y <= window_top) {
                            FringeIndicatorPair cont = fringe_resolve_indicator(buffer, FRINGE_IND_CONTINUATION);
                            fringe_draw_pair(cont, true, false, win, next_y, line_height);
                        }
                    }

                    x = start_x;
                    y -= current_line_height;
                    current_line_height = line_height;
                    // Re-scan remaining portion of this logical line for the new visual line
                    line_max_font_height = measure_line_max_font_height(buffer, i,
                                                                         current_line_end, line_height);
                    if (wcb) {
                        previous_line_had_wrapped_box = true;
                        current_line_has_box          = true;
                        y                            -= 2 * box_line_thickness;
                        box_span_no_left              = true;
                    } else {
                        previous_line_had_wrapped_box = false;
                        current_line_has_box          = false;
                        was_in_box                    = false;
                        box_span_no_left              = false;
                        if (line_has_box_face_fast(buffer, i, current_line_end)) {
                            y                    -= box_line_thickness;
                            current_line_has_box  = true;
                        }
                    }
                    box_span_no_right = false;
                    if (y < window_bottom - current_line_height) break;
                }
            }

            Color char_color = face->fg;
            Color bg_color   = face->bg;
            bool  needs_bg   = !color_equals(bg_color, base_bg);

            if (in_region && region_face) {
                bg_color = region_face->bg; char_color = region_face->fg; needs_bg = true;
            }

            if (at_cursor) {
                Character *sp = font_get_character(char_font, ' ');
                float face_space_width = sp ? sp->ax : char_font->ascent;
                cursor_width  = (ch == '\t' && stretch_cursor) ? char_width
                    : stretch_cursor                 ? acw
                    :                                  face_space_width;
                cursor_height = font_height;
                cursor_color  = (crystal_point_mode && ch != '\n' && ch != ' ' && ch != '\t')
                                 ? (is_control_char ? get_face(FACE_ESCAPE_GLYPH)->fg : face->fg)
                                 : get_face(FACE_CURSOR)->bg;
                cursor_found  = true;
                if (should_draw_filled && buffer->cursor.visible)
                    char_color = in_region ? region_face->bg : face->bg;
                needs_bg = !color_equals(bg_color, base_bg);
            } else if (at_mark && !in_region) {
                bg_color = get_face(FACE_VISIBLE_MARK)->bg; char_color = base_bg; needs_bg = true;
            }

            float draw_x = x;
            // Baseline-align: shift tall fonts down so all glyphs share the same baseline
            // at the bottom of the line slot. line_max_font_height is pre-scanned so this
            // is exact even for the first character on a line.
            float base_y = current_line_has_box ? y - box_line_thickness : y;
            float default_font_height = default_font->ascent + default_font->descent;
            float draw_y = base_y - (line_max_font_height - default_font_height) + (char_font->descent*2 - default_font->descent);


            if (face->box) {
                if (!was_in_box)
                    x += box_line_thickness;
                draw_x = x;
            } else if (was_in_box) {
                if (has_box_span) {
                    flush_box_span(box_span_start_x, box_span_y, box_span_width,
                                   box_span_height, box_span_color, box_line_thickness,
                                   box_span_no_left, false);
                    has_box_span = false;
                }
                x += box_line_thickness;
                was_in_box = false;
                box_span_no_left = false;
                draw_x = x;
            }

            if (at_cursor) {
                cursor_x = draw_x;
                cursor_y = draw_y - char_font->descent * 2;
            }


            if (at_mark && !in_region && y >= window_bottom && y <= window_top) {
                Character *sp = font_get_character(char_font, ' ');
                float mw = sp ? sp->ax : char_font->ascent;
                quad2D((vec2){draw_x, draw_y - char_font->descent * 2},
                       (vec2){mw, font_height}, get_face(FACE_VISIBLE_MARK)->bg);
            }

            if (y >= window_bottom && y <= window_top) {

                if (needs_bg && (!at_cursor || !should_draw_filled || !buffer->cursor.visible)) {
                    float bg_x = draw_x;
                    float bg_y, bg_h, bg_w;
                    if (face->box) {
                        bg_y = draw_y - char_font->descent * 2 - box_line_thickness;
                        bg_h = font_height + box_line_thickness * 2;
                        bg_w = acw;
                    } else {
                        bg_y = draw_y - char_font->descent * 2;
                        bg_h = font_height;
                        bg_w = char_width;
                    }
                    // For region, always fill the full line height so the highlight
                    // is uniform regardless of per-character font size (Emacs behavior)
                    if (in_region) {
                        bg_y = base_y - line_max_font_height + (default_font->descent * 3.4);
                        bg_h = line_max_font_height;

                        bg_w = char_width;
                    }
                    if (has_bg_span && color_equals(bg_color, bg_span_color) &&
                        bg_y == bg_span_y && bg_h == bg_span_height &&
                        fabsf(bg_x - (bg_span_start_x + bg_span_width)) < 0.5f) {
                        bg_span_width += bg_w;
                    } else {
                        if (has_bg_span)
                            draw_background_span(bg_span_start_x, bg_span_y, bg_span_width,
                                                 bg_span_height, bg_span_color);
                        bg_span_start_x = bg_x; bg_span_y = bg_y;
                        bg_span_width   = bg_w; bg_span_height = bg_h;
                        bg_span_color   = bg_color; has_bg_span = true;
                    }
                } else if (has_bg_span) {
                    draw_background_span(bg_span_start_x, bg_span_y, bg_span_width,
                                         bg_span_height, bg_span_color);
                    has_bg_span = false;
                }

                if (face->box) {
                    float box_y = draw_y - char_font->descent * 2 - box_line_thickness;
                    float box_h = font_height + box_line_thickness * 2;
                    Color cbc   = face->box_color;

                    if (!was_in_box) {
                        if (has_box_span) {
                            flush_box_span(box_span_start_x, box_span_y, box_span_width,
                                           box_span_height, box_span_color, box_line_thickness,
                                           box_span_no_left, box_span_no_right);
                            has_box_span = false;
                        }
                        bool snl          = box_span_no_left;
                        box_span_start_x  = snl ? x : (x - box_line_thickness);
                        box_span_y        = box_y;
                        box_span_height   = box_h;
                        box_span_color    = cbc;
                        box_span_width    = acw + box_line_thickness + (snl ? 0 : box_line_thickness);
                        box_span_no_left  = snl;
                        box_span_no_right = false;
                        has_box_span      = true;
                    } else {
                        if (has_box_span && color_equals(cbc, box_span_color) &&
                            box_y == box_span_y && box_h == box_span_height) {
                            box_span_width += acw;
                        } else {
                            if (has_box_span)
                                flush_box_span(box_span_start_x, box_span_y, box_span_width,
                                               box_span_height, box_span_color, box_line_thickness,
                                               box_span_no_left, box_span_no_right);
                            box_span_start_x  = x - box_line_thickness;
                            box_span_y        = box_y;
                            box_span_width    = acw + box_line_thickness * 2;
                            box_span_height   = box_h;
                            box_span_color    = cbc;
                            box_span_no_left  = false;
                            box_span_no_right = false;
                            has_box_span      = true;
                        }
                    }

                    if (ch != '\t') {
                        if (is_control_char) {
                            Face    *ef  = get_face(FACE_ESCAPE_GLYPH);
                            Color    efg = ef ? ef->fg : char_color;
                            uint32_t vis = (ch == 0x7f) ? '?' : (ch + 64);
                            Color    cc  = (at_cursor && should_draw_filled && buffer->cursor.visible)
                                           ? face->bg : efg;
                            float a1 = draw_character_with_face('^', draw_x, draw_y, face, char_font,
                                           cc, false, char_color, false, char_color);
                            draw_character_with_face(vis, draw_x + a1, draw_y, face, char_font,
                                           efg, face->underline, face->underline_color,
                                           face->strike_through, face->strike_through_color);
                        } else {
                            draw_character_with_face(ch, draw_x, draw_y, face, char_font,
                                           char_color, face->underline, face->underline_color,
                                           face->strike_through, face->strike_through_color);
                        }
                    } else if (face->underline || face->strike_through) {
                        if (face->underline)
                            quad2D((vec2){draw_x, draw_y}, (vec2){tab_pixel_width, 1.f}, face->underline_color);
                        if (face->strike_through)
                            quad2D((vec2){draw_x, draw_y}, (vec2){tab_pixel_width, 1.f}, face->strike_through_color);
                    }

                    x += acw;
                    was_in_box       = true;
                    last_drawn_face  = face; last_drawn_font = char_font;
                    last_drawn_bg    = bg_color; last_face_had_box = true;

                } else {
                    if (ch != '\t') {
                        if (is_control_char) {
                            Face    *ef  = get_face(FACE_ESCAPE_GLYPH);
                            Color    efg = ef ? ef->fg : char_color;
                            uint32_t vis = (ch == 0x7f) ? '?' : (ch + 64);
                            Color    cc  = (at_cursor && should_draw_filled && buffer->cursor.visible)
                                           ? face->bg : efg;
                            float a1 = draw_character_with_face('^', draw_x, draw_y, face, char_font,
                                           cc, false, char_color, false, char_color);
                            float a2 = draw_character_with_face(vis, draw_x + a1, draw_y, face, char_font,
                                           efg, face->underline, face->underline_color,
                                           face->strike_through, face->strike_through_color);
                            x += a1 + a2;
                        } else {
                            x += draw_character_with_face(ch, draw_x, draw_y, face, char_font,
                                             char_color, face->underline, face->underline_color,
                                             face->strike_through, face->strike_through_color);
                        }
                    } else {
                        if (face->underline) {
                            float uly, ult;
                            if (g_ul.underline_at_descent_line)
                                uly = draw_y - char_font->descent * 2;
                            else if (g_ul.use_underline_pos)
                                uly = draw_y - char_font->descent - char_font->underline_position;
                            else
                                uly = draw_y - char_font->descent - g_ul.underline_minimum_offset;
                            ult = g_ul.use_underline_pos ? char_font->underline_thickness : 1.f;
                            quad2D((vec2){draw_x, uly}, (vec2){char_width, ult}, face->underline_color);
                        }
                        if (face->strike_through)
                            quad2D((vec2){draw_x, draw_y}, (vec2){char_width, 1.f}, face->strike_through_color);
                        x += char_width;
                    }
                    last_drawn_face  = face; last_drawn_font = char_font;
                    last_drawn_bg    = bg_color; last_face_had_box = false;
                }

            } else if (y >= window_bottom) {
                if (face->box) {
                    if (!was_in_box) x += box_line_thickness;
                    x += acw;
                    was_in_box = true;
                } else {
                    if (was_in_box) {
                        x += box_line_thickness;
                        was_in_box = false;
                        box_span_no_left = false;
                    }
                    x += char_width;
                }
                last_drawn_face  = face; last_drawn_font = char_font;
                last_drawn_bg    = bg_color; last_face_had_box = face->box;
            }
        }
        prev_face_id = current_face_id;
        i++;
    }

    if (has_bg_span)
        draw_background_span(bg_span_start_x, bg_span_y, bg_span_width,
                              bg_span_height, bg_span_color);
    if (last_drawn_face && y >= window_bottom && y <= window_top)
        draw_face_extension(x, y, max_x, last_drawn_face, last_drawn_font,
                            last_drawn_bg, box_line_thickness, last_face_had_box);
    if (has_box_span)
        flush_box_span(box_span_start_x, box_span_y, box_span_width, box_span_height,
                       box_span_color, box_line_thickness, box_span_no_left, false);
    rope_iter_destroy(&iter);

    // Emit empty-line fringe indicators for visual lines below the buffer end.
    if (scm_get_bool("indicate-empty-lines", false)) {
        FringeIndicatorPair empty = fringe_resolve_indicator(buffer, FRINGE_IND_EMPTY_LINE);
        if (empty.left || empty.right) {
            float ey = y;
            while (ey >= window_bottom) {
                if (ey <= window_top)
                    fringe_draw_pair(empty, true, false, win, ey, line_height);
                ey -= line_height;
            }
        }
    }

    if (point >= buf_len) {
        int   fid      = get_text_property_face(buffer, point);
        Face *eob_face = get_face(fid);
        Font *cf       = eob_face ? get_face_font(eob_face) : default_font;
        float eob_base_y = current_line_has_box ? y - box_line_thickness : y;
        float default_font_height = default_font->ascent + default_font->descent;
        float eob_draw_y = eob_base_y - (line_max_font_height - default_font_height)
            + (cf->descent * 2 - default_font->descent);
        cursor_x = x; cursor_y = eob_draw_y - cf->descent * 2;
        Character *sp = font_get_character(cf, ' ');
        cursor_width  = sp ? sp->ax : cf->ascent;
        cursor_height = line_max_font_height;
        cursor_color  = get_face(FACE_CURSOR)->bg;
        cursor_found  = true;
    }

    buffer->cursor.x = cursor_x;
    buffer->cursor.y = cursor_y;
    if (!cursor_found) return;
    if (win && win->is_minibuffer && !selected_frame->wm.minibuffer_active) return;

    if (should_draw_filled) {
        if (blink_cursor_mode && buffer->cursor.blink_count < blink_cursor_blinks) {
            double now      = getTime();
            double interval = buffer->cursor.visible ? blink_cursor_interval : blink_cursor_delay;
            if (now - buffer->cursor.last_blink >= interval) {
                buffer->cursor.visible    = !buffer->cursor.visible;
                buffer->cursor.last_blink = now;
                if (buffer->cursor.visible) buffer->cursor.blink_count++;
            }
            if (buffer->cursor.visible)
                quad2D((vec2){cursor_x, cursor_y}, (vec2){cursor_width, cursor_height}, cursor_color);
        } else {
            quad2D((vec2){cursor_x, cursor_y}, (vec2){cursor_width, cursor_height}, cursor_color);
        }
    } else {
        if (is_selected) { buffer->cursor.visible = true; buffer->cursor.blink_count = 0; }
        if (is_selected || cursor_in_non_selected) {
            float bw = 1.f;
            quad2D((vec2){cursor_x, cursor_y + cursor_height - bw}, (vec2){cursor_width, bw}, cursor_color);
            quad2D((vec2){cursor_x, cursor_y},                      (vec2){cursor_width, bw}, cursor_color);
            quad2D((vec2){cursor_x, cursor_y},                      (vec2){bw, cursor_height}, cursor_color);
            quad2D((vec2){cursor_x + cursor_width - bw, cursor_y},  (vec2){bw, cursor_height}, cursor_color);
        }
    }
}

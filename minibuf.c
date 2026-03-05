#include "minibuf.h"
#include "buffer.h"
#include "edit.h"
#include "wm.h"
#include "rope.h"
#include "frame.h"
#include "faces.h"
#include "lisp.h"
#include <string.h>
#include <ctype.h>

// Global completion state
static SCM completion_collection = SCM_BOOL_F;
static SCM completion_predicate = SCM_BOOL_F;

// State for blocking minibuffer read
static bool minibuffer_exit_requested = false;
static bool minibuffer_abort_requested = false;
static char *minibuffer_result = NULL;

// Caches
bool frame_resized_since_last_complete = false;
static int last_completion_column_count = 0;
static int last_completion_count = 0;
static size_t last_completion_max_len = 0;

#define DEFAULT_HISTORY_LENGTH 100

/// History

static HistoryState *history_states = NULL;
static HistoryState *active_history_state = NULL;

static HistoryState* get_or_create_history_state(SCM history_var) {
    // Search for existing state
    HistoryState *state = history_states;
    while (state) {
        if (scm_is_eq(state->history_var, history_var)) {
            return state;
        }
        state = state->next;
    }

    // Create new state
    state = malloc(sizeof(HistoryState));
    state->history_var = history_var;
    state->current_position = -1;
    state->original_input = NULL;
    state->next = history_states;
    history_states = state;

    // Protect the symbol from GC
    scm_gc_protect_object(history_var);

    return state;
}

// Helper: Get the Scheme variable for a history symbol
static SCM get_history_variable(SCM history_sym) {
    SCM var = scm_module_variable(scm_current_module(), history_sym);

    // If variable doesn't exist, create it with an empty list
    if (scm_is_false(var)) {
        char *sym_name = scm_to_locale_string(scm_symbol_to_string(history_sym));
        scm_c_define(sym_name, SCM_EOL);
        free(sym_name);
        var = scm_module_variable(scm_current_module(), history_sym);
    }

    return var;
}

// Helper: Get history list from variable
static SCM get_history_list(SCM history_sym) {
    SCM var = get_history_variable(history_sym);
    SCM value = scm_variable_ref(var);

    // Ensure it's a list
    if (!scm_is_true(scm_list_p(value))) {
        scm_variable_set_x(var, SCM_EOL);
        return SCM_EOL;
    }

    return value;
}

// Helper: Set history list
static void set_history_list(SCM history_sym, SCM new_list) {
    SCM var = get_history_variable(history_sym);
    scm_variable_set_x(var, new_list);
}

void add_to_history(SCM history_sym, const char *value) {
    if (!value || *value == '\0') {
        return; // Don't add empty strings
    }

    SCM history = get_history_list(history_sym);
    SCM value_scm = scm_from_locale_string(value);

    // Check if this is a duplicate of the most recent entry
    if (scm_is_pair(history)) {
        SCM head = scm_car(history);
        if (scm_is_string(head)) {
            char *head_str = scm_to_locale_string(head);
            bool is_duplicate = strcmp(head_str, value) == 0;
            free(head_str);
            if (is_duplicate) {
                return; // Don't add duplicates
            }
        }
    }

    // Add to front of list
    history = scm_cons(value_scm, history);

    // Trim if too long
    int max_length = DEFAULT_HISTORY_LENGTH;

    // Try to get history-length variable if it exists
    SCM history_length_var = scm_module_variable(scm_current_module(),
                                                  scm_from_locale_symbol("history-length"));
    if (scm_is_true(history_length_var) && scm_is_true(scm_variable_bound_p(history_length_var))) {
        SCM length_val = scm_variable_ref(history_length_var);
        if (scm_is_integer(length_val)) {
            max_length = scm_to_int(length_val);
        }
    }

    int length = scm_to_int(scm_length(history));

    if (length > max_length) {
        // Take only the first max_length elements
        SCM trimmed = SCM_EOL;
        SCM tail = history;
        for (int i = 0; i < max_length && scm_is_pair(tail); i++) {
            trimmed = scm_cons(scm_car(tail), trimmed);
            tail = scm_cdr(tail);
        }
        history = scm_reverse(trimmed);
    }

    set_history_list(history_sym, history);
}

char* get_history_element(SCM history_sym, int offset) {
    SCM history = get_history_list(history_sym);

    if (offset < 0) {
        return NULL;
    }

    // Walk the list to the offset
    SCM tail = history;
    for (int i = 0; i < offset && scm_is_pair(tail); i++) {
        tail = scm_cdr(tail);
    }

    if (!scm_is_pair(tail)) {
        return NULL;
    }

    SCM elem = scm_car(tail);
    if (!scm_is_string(elem)) {
        return NULL;
    }

    return scm_to_locale_string(elem);
}

int get_history_position(SCM caller_func) {
    HistoryState *state = get_or_create_history_state(caller_func);
    return state->current_position;
}

void set_history_position(SCM caller_func, int pos) {
    HistoryState *state = get_or_create_history_state(caller_func);
    state->current_position = pos;
}

static int get_history_length(SCM history_sym) {
    SCM history = get_history_list(history_sym);
    return scm_to_int(scm_length(history));
}

// Save original input when starting to browse history
static void save_original_input(SCM caller_func, const char *input) {
    HistoryState *state = get_or_create_history_state(caller_func);
    if (state->original_input) {
        free(state->original_input);
    }
    state->original_input = strdup(input);
}

static char* get_original_input(SCM caller_func) {
    HistoryState *state = get_or_create_history_state(caller_func);
    return state->original_input;
}

static void clear_original_input(SCM caller_func) {
    HistoryState *state = get_or_create_history_state(caller_func);
    if (state->original_input) {
        free(state->original_input);
        state->original_input = NULL;
    }
}

void minibuffer_previous_history_element(int n) {
    if (!selected_frame->wm.minibuffer_active || !active_history_state) {
        message("Not in minibuffer");
        return;
    }

    SCM history_sym = active_history_state->history_var;
    int hist_len = get_history_length(history_sym);

    if (hist_len == 0) {
        message("No history");
        return;
    }

    int current_pos = get_history_position(history_sym);

    // If we're at the prompt (pos = -1), save current input
    if (current_pos == -1) {
        char *current_input = get_minibuffer_contents();
        save_original_input(history_sym, current_input);
        free(current_input);
        current_pos = -1;
    }

    // Move backward in history (toward older entries)
    int new_pos = current_pos + n;

    // Check if we're trying to go beyond the oldest entry
    if (new_pos >= hist_len) {
        new_pos = hist_len - 1;
        message("Beginning of history; no preceding item");

        if (current_pos == hist_len - 1) {
            return;
        }
    }

    if (new_pos < 0) {
        new_pos = 0;
    }

    // Get history element
    char *history_value = get_history_element(history_sym, new_pos);
    if (!history_value) {
        return;
    }

    // Update minibuffer contents
    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;
    size_t prompt_end = 0;
    SCM field_sym = scm_from_locale_symbol("field");
    size_t total_len = rope_char_length(mb->rope);

    // Find where the prompt ends (last char with field property + 1)
    for (size_t i = 0; i < total_len; i++) {
        SCM field = get_text_property(mb, i, field_sym);
        if (scm_is_true(field)) {
            prompt_end = i + 1;
        } else {
            break;
        }
    }

    // Delete old input
    size_t old_len = total_len - prompt_end;
    if (old_len > 0) {
        remove_text_properties(mb, prompt_end, total_len);
        mb->rope = rope_delete_chars(mb->rope, prompt_end, old_len);
    }

    // Insert history value
    size_t new_len = strlen(history_value);
    mb->rope = rope_insert_chars(mb->rope, prompt_end, history_value, new_len);
    mb->pt = prompt_end + new_len;
    selected_frame->wm.minibuffer_window->point = mb->pt;

    set_history_position(history_sym, new_pos);
    free(history_value);
}

void minibuffer_next_history_element(int n) {
    if (!selected_frame->wm.minibuffer_active || !active_history_state) {
        message("Not in minibuffer");
        return;
    }

    SCM history_sym = active_history_state->history_var;
    int hist_len = get_history_length(history_sym);

    if (hist_len == 0) {
        message("No history");
        return;
    }

    int current_pos = get_history_position(history_sym);

    if (current_pos == -1) {
        message("End of history");
        return;
    }

    // Move forward in history
    int new_pos = current_pos - n;

    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;
    size_t prompt_end = 0;
    SCM field_sym = scm_from_locale_symbol("field");
    size_t total_len = rope_char_length(mb->rope);

    // Find where the prompt ends (last char with field property + 1)
    for (size_t i = 0; i < total_len; i++) {
        SCM field = get_text_property(mb, i, field_sym);
        if (scm_is_true(field)) {
            prompt_end = i + 1;
        } else {
            break;
        }
    }

    // If we've gone past the newest entry, restore original input
    if (new_pos < 0) {
        char *original = get_original_input(history_sym);

        size_t old_len = total_len - prompt_end;
        if (old_len > 0) {
            remove_text_properties(mb, prompt_end, total_len);
            mb->rope = rope_delete_chars(mb->rope, prompt_end, old_len);
        }

        if (original && *original) {
            size_t new_len = strlen(original);
            mb->rope = rope_insert_chars(mb->rope, prompt_end, original, new_len);
            mb->pt = prompt_end + new_len;
        } else {
            mb->pt = prompt_end;
        }
        selected_frame->wm.minibuffer_window->point = mb->pt;

        set_history_position(history_sym, -1);
        clear_original_input(history_sym);
        return;
    }

    if (new_pos >= hist_len) {
        new_pos = hist_len - 1;
        message("Beginning of history; no preceding item");

        if (current_pos == hist_len - 1) {
            return;
        }
    }

    // Get history element
    char *history_value = get_history_element(history_sym, new_pos);
    if (!history_value) {
        return;
    }

    size_t old_len = total_len - prompt_end;
    if (old_len > 0) {
        remove_text_properties(mb, prompt_end, total_len);
        mb->rope = rope_delete_chars(mb->rope, prompt_end, old_len);
    }

    size_t new_len = strlen(history_value);
    mb->rope = rope_insert_chars(mb->rope, prompt_end, history_value, new_len);
    mb->pt = prompt_end + new_len;
    selected_frame->wm.minibuffer_window->point = mb->pt;

    set_history_position(history_sym, new_pos);
    free(history_value);
}

/// Minibuffer

void activate_minibuffer() {
    if (selected_frame->wm.minibuffer_active) {
        message("Command attempted to use minibuffer while in minibuffer");
        return;
    }

    selected_frame->wm.minibuffer_active = true;

    // Save window configuration before switching
    if (selected_frame->wm.saved_config.windows) free_window_configuration(&selected_frame->wm.saved_config);
    selected_frame->wm.saved_config = save_window_configuration();

    // Store the current window (for minibuffer highlight)
    selected_frame->wm.previous_window = selected_frame->wm.selected;

    // IMPORTANT: Reset all keychord states when entering minibuffer
    keychord_reset_state(&keymap);

    // Switch to minibuffer
    selected_frame->wm.selected->is_selected = false;
    selected_frame->wm.minibuffer_window->is_selected = true;
    selected_frame->wm.selected = selected_frame->wm.minibuffer_window;
    current_buffer = selected_frame->wm.minibuffer_window->buffer;
    current_buffer->pt = selected_frame->wm.minibuffer_window->point;
    reset_cursor_blink(current_buffer);

    SCM minibuffer_mode_func = scm_c_lookup("minibuffer-mode");
    scm_call_0(scm_variable_ref(minibuffer_mode_func));
}

void deactivate_minibuffer() {
    if (!selected_frame->wm.minibuffer_active) return;
    selected_frame->wm.minibuffer_active = false;

    // Clear minibuffer content
    if (selected_frame->wm.minibuffer_window->buffer) {
        size_t len = rope_char_length(selected_frame->wm.minibuffer_window->buffer->rope);
        if (len > 0) {
            // Clear text properties BEFORE deleting the text
            clear_text_properties(selected_frame->wm.minibuffer_window->buffer);

            selected_frame->wm.minibuffer_window->buffer->rope = rope_delete_chars(
                selected_frame->wm.minibuffer_window->buffer->rope, 0, len);
        }
        selected_frame->wm.minibuffer_window->point = 0;
    }

    // Force minibuffer to recalculate and reset to single line height
    selected_frame->wm.minibuffer_window->height = selected_frame->line_height;
    wm_recalculate_layout();

    selected_frame->wm.minibuffer_window->is_selected = false;

    // Restore saved window configuration
    restore_window_configuration(&selected_frame->wm.saved_config);
    free_window_configuration(&selected_frame->wm.saved_config);

    selected_frame->wm.previous_window = NULL;
}

void keyboard_quit() {
    deactivate_mark();
    Window *selected = selected_frame->wm.selected;
    if (selected == selected_frame->wm.minibuffer_window ||
        selected->buffer == get_buffer("*Completions*")) {
        // Signal abort from minibuffer
        minibuffer_abort_requested = true;
        minibuffer_exit_requested = true;
        if (minibuffer_result) {
            free(minibuffer_result);
            minibuffer_result = NULL;
        }
    }
    message("Quit");
}

// Extract minibuffer contents (after the prompt)
char *get_minibuffer_contents() {
    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;
    if (!mb) return strdup("");

    size_t total_len = rope_char_length(mb->rope);
    if (total_len == 0) return strdup("");

    // Find where the prompt ends (last char with field property + 1)
    size_t prompt_end = 0;
    SCM field_sym = scm_from_locale_symbol("field");

    for (size_t i = 0; i < total_len; i++) {
        SCM field = get_text_property(mb, i, field_sym);
        if (scm_is_true(field)) {
            prompt_end = i + 1;  // Update to position after this field char
        } else {
            break;  // Stop at first non-field char
        }
    }

    // Extract text after prompt
    if (prompt_end >= total_len) {
        return strdup("");
    }

    size_t content_len = total_len - prompt_end;
    size_t buffer_size = content_len * 4 + 1;
    char *buffer = malloc(buffer_size);
    if (!buffer) return strdup("");

    size_t copied = rope_copy_chars(mb->rope, prompt_end, content_len, buffer, buffer_size - 1);
    buffer[copied] = '\0';

    return buffer;
}

/// Completion

// Helper: check if string starts with prefix (case-insensitive)
static bool string_prefix_p(const char *str, const char *prefix) {
    while (*prefix) {
        if (tolower(*str) != tolower(*prefix)) {
            return false;
        }
        str++;
        prefix++;
    }
    return true;
}

static bool partial_completion_match(const char *pattern, const char *candidate) {
    // CRITICAL: Empty pattern matches EVERYTHING
    if (!pattern || *pattern == '\0') {
        return true;
    }

    // Empty candidate only matches empty pattern
    if (!candidate || *candidate == '\0') {
        return false;
    }

    // Check if pattern is a prefix of candidate (case-insensitive)
    const char *p = pattern;
    const char *c = candidate;

    while (*p != '\0' && *c != '\0') {
        if (tolower((unsigned char)*p) != tolower((unsigned char)*c)) {
            return false;
        }
        p++;
        c++;
    }

    // Pattern must be fully consumed
    return (*p == '\0');
}

static SCM try_completion_internal(const char *string, SCM collection) {
    // Programmed completion: call (collection string pred action)
    // action=nil means try-completion
    if (scm_is_true(scm_procedure_p(collection))) {
        return scm_call_3(collection,
                          scm_from_locale_string(string),
                          completion_predicate,
                          SCM_BOOL_F);
    }

    if (!scm_is_true(scm_list_p(collection))) {
        return SCM_BOOL_F;
    }

    SCM best_match = SCM_BOOL_F;
    size_t best_len = 0;
    int match_count = 0;
    SCM sole_match = SCM_BOOL_F;

    SCM tail = collection;
    while (scm_is_pair(tail)) {
        SCM item = scm_car(tail);
        if (scm_is_symbol(item)) item = scm_symbol_to_string(item);

        if (scm_is_string(item)) {
            char *candidate = scm_to_locale_string(item);
            if (partial_completion_match(string, candidate)) {
                match_count++;
                size_t len = strlen(candidate);
                if (match_count == 1) {
                    best_match = item;
                    sole_match = item;
                    best_len = len;
                } else {
                    char *best_str = scm_to_locale_string(best_match);
                    size_t common = 0;
                    while (common < best_len && common < len &&
                           tolower(best_str[common]) == tolower(candidate[common]))
                        common++;
                    if (common < best_len) {
                        best_match = scm_substring(best_match, scm_from_int(0),
                                                   scm_from_size_t(common));
                        best_len = common;
                    }
                    free(best_str);
                }
            }
            free(candidate);
        }
        tail = scm_cdr(tail);
    }

    if (match_count == 0)      return SCM_BOOL_F;
    else if (match_count == 1) return sole_match;
    else                       return best_match;
}

// Get all completions
static SCM all_completions_internal(const char *string, SCM collection) {
    // Programmed completion: call (collection string pred action)
    // action=t means all-completions
    if (scm_is_true(scm_procedure_p(collection))) {
        SCM result = scm_call_3(collection,
                                scm_from_locale_string(string),
                                completion_predicate,
                                SCM_BOOL_T);
        if (scm_is_true(scm_list_p(result))) return result;
        return SCM_EOL;
    }

    if (!scm_is_true(scm_list_p(collection))) return SCM_EOL;

    SCM matches = SCM_EOL;
    SCM tail = collection;
    while (scm_is_pair(tail)) {
        SCM item = scm_car(tail);
        SCM item_str = item;
        if (scm_is_symbol(item)) item_str = scm_symbol_to_string(item);

        if (scm_is_string(item_str)) {
            char *candidate = scm_to_locale_string(item_str);
            if (partial_completion_match(string, candidate))
                matches = scm_cons(item_str, matches);
            free(candidate);
        }
        tail = scm_cdr(tail);
    }
    return scm_reverse(matches);
}

// Helper: check if string exists exactly in collection
static bool is_exact_completion(const char *string, SCM collection) {
    // Programmed completion: call (collection string pred action)
    // action='lambda means test exact match
    if (scm_is_true(scm_procedure_p(collection))) {
        SCM result = scm_call_3(collection,
                                scm_from_locale_string(string),
                                completion_predicate,
                                scm_from_locale_symbol("lambda"));
        return scm_is_true(result);
    }

    if (!scm_is_true(scm_list_p(collection))) return false;

    SCM tail = collection;
    while (scm_is_pair(tail)) {
        SCM item = scm_car(tail);
        if (scm_is_symbol(item)) item = scm_symbol_to_string(item);
        if (scm_is_string(item)) {
            char *candidate = scm_to_locale_string(item);
            bool match = (strcasecmp(string, candidate) == 0);
            free(candidate);
            if (match) return true;
        }
        tail = scm_cdr(tail);
    }
    return false;
}

// Helper: find common prefix length between two strings
static size_t common_prefix_length(const char *s1, const char *s2) {
    size_t len = 0;
    while (s1[len] && s2[len] && tolower(s1[len]) == tolower(s2[len])) {
        len++;
    }
    return len;
}

static void display_completions(SCM completions, const char *input) {
    Buffer *completions_buf = get_buffer("*Completions*");
    if (!completions_buf) {
        completions_buf = buffer_create("*Completions*");
    }

    // Clear buffer
    size_t len = rope_char_length(completions_buf->rope);
    if (len > 0) {
        clear_text_properties(completions_buf);
        completions_buf->rope = rope_delete_chars(completions_buf->rope, 0, len);
    }

    size_t pos = 0;
    SCM face_sym = scm_from_locale_symbol("face");

    int shadow_face = face_id_from_name("shadow");
    int help_key_binding_face = face_id_from_name("help-key-binding");
    int common_part_face = face_id_from_name("completions-common-part");
    int first_diff_face = face_id_from_name("completions-first-difference");
    int highlight_face = face_id_from_name("completions-highlight");

    bool show_help_text = scm_get_bool("completion-show-help", true);

    // Show help text if enabled
    if (show_help_text) {
        const char *line1 = "Click or type ";
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, line1, strlen(line1));
        pos += strlen(line1);

        const char *key1 = "M-RET";
        size_t key1_start = pos;
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, key1, strlen(key1));
        put_text_property(completions_buf, key1_start, key1_start + strlen(key1),
                         face_sym, scm_from_int(help_key_binding_face));
        pos += strlen(key1);

        const char *line2 = " on a completion to select it.\nType ";
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, line2, strlen(line2));
        pos += strlen(line2);

        const char *key2 = "M-<down>";
        size_t key2_start = pos;
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, key2, strlen(key2));
        put_text_property(completions_buf, key2_start, key2_start + strlen(key2),
                         face_sym, scm_from_int(help_key_binding_face));
        pos += strlen(key2);

        const char *line3 = " or ";
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, line3, strlen(line3));
        pos += strlen(line3);

        const char *key3 = "M-<up>";
        size_t key3_start = pos;
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, key3, strlen(key3));
        put_text_property(completions_buf, key3_start, key3_start + strlen(key3),
                         face_sym, scm_from_int(help_key_binding_face));
        pos += strlen(key3);

        const char *line4 = " to move point between completions.\n";
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, line4, strlen(line4));
        pos += strlen(line4);
    }

    // Count line
    int count = scm_to_int(scm_length(completions));
    char count_line[128];
    snprintf(count_line, sizeof(count_line), "%d possible completion%s:\n",
             count, count == 1 ? "" : "s");
    size_t count_line_start = pos;
    completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, count_line, strlen(count_line));
    put_text_property(completions_buf, count_line_start, count_line_start + strlen(count_line),
                     face_sym, scm_from_int(shadow_face));
    pos += strlen(count_line);

    // Add blank line after count ONLY if help was shown
    if (show_help_text) {
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, "\n", 1);
        pos += 1;
    }

    // Store where first completion starts
    size_t first_completion_start = pos;

    // Calculate common prefix
    size_t common_len = 0;
    SCM tail = completions;
    char *first_completion = NULL;

    if (scm_is_pair(tail)) {
        SCM item = scm_car(tail);
        if (scm_is_symbol(item)) {
            item = scm_symbol_to_string(item);
        }
        if (scm_is_string(item)) {
            first_completion = scm_to_locale_string(item);
            common_len = strlen(first_completion);
        }
        tail = scm_cdr(tail);
    }

    if (first_completion) {
        while (scm_is_pair(tail)) {
            SCM item = scm_car(tail);
            if (scm_is_symbol(item)) {
                item = scm_symbol_to_string(item);
            }
            if (scm_is_string(item)) {
                char *str = scm_to_locale_string(item);
                size_t prefix = common_prefix_length(first_completion, str);
                if (prefix < common_len) {
                    common_len = prefix;
                }
                free(str);
            }
            tail = scm_cdr(tail);
        }
        free(first_completion);
    }

    // Collect all completions and find max length
    typedef struct {
        char *str;
        size_t len;
    } CompletionItem;

    CompletionItem *items = malloc(count * sizeof(CompletionItem));
    int item_idx = 0;
    size_t max_len = 0;

    tail = completions;
    while (scm_is_pair(tail)) {
        SCM item = scm_car(tail);
        if (scm_is_symbol(item)) {
            item = scm_symbol_to_string(item);
        }
        if (scm_is_string(item)) {
            items[item_idx].str = scm_to_locale_string(item);
            items[item_idx].len = strlen(items[item_idx].str);
            if (items[item_idx].len > max_len) {
                max_len = items[item_idx].len;
            }
            item_idx++;
        }
        tail = scm_cdr(tail);
    }

    // Calculate columns
    Window *ref_window = selected_frame->wm.selected;
    int window_width = ref_window->width / selected_frame->column_width;
    if (window_width < 40) window_width = 80;

    int tab_width = 8;
    int column_width = max_len + tab_width;
    int num_columns = window_width / column_width;
    if (num_columns < 1) num_columns = 1;
    if (num_columns > count) num_columns = count;

    // Cache
    last_completion_column_count = num_columns;
    last_completion_count        = count;
    last_completion_max_len      = max_len;

    // Insert completions
    size_t first_completion_end = 0;
    for (int i = 0; i < item_idx; i++) {
        size_t start_pos = pos;

        // Insert completion text
        completions_buf->rope = rope_insert_chars(completions_buf->rope, pos,
                                                   items[i].str, items[i].len);

        // Track first completion end
        if (i == 0) {
            first_completion_end = pos + items[i].len;
        }

        // Apply faces for common part and first difference
        if (common_len > 0 && common_len <= items[i].len) {
            put_text_property(completions_buf, start_pos, start_pos + common_len,
                             face_sym, scm_from_int(common_part_face));
        }

        if (common_len > 0 && common_len < items[i].len) {
            put_text_property(completions_buf, start_pos + common_len,
                             start_pos + common_len + 1,
                             face_sym, scm_from_int(first_diff_face));
        }

        pos += items[i].len;

        // Determine row position
        int col = i % num_columns;
        bool is_last_in_row = (col == num_columns - 1) || (i == item_idx - 1);

        if (is_last_in_row) {
            // End of row - newline
            completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, "\n", 1);
            pos += 1;
        } else {
            // Insert a tab with display property (space :align-to N)
            // to align to the start of the next column slot
            completions_buf->rope = rope_insert_chars(completions_buf->rope, pos, "\t", 1);

            int next_col_idx = (i % num_columns) + 1;
            int target_pixel = next_col_idx * column_width * (int)selected_frame->column_width;

            SCM space_spec = scm_list_3(
                scm_from_utf8_symbol("space"),
                scm_from_utf8_symbol(":align-to"),
                scm_from_int(target_pixel)
            );
            put_text_property(completions_buf, pos, pos + 1,
                              scm_from_utf8_symbol("display"),
                              space_spec);
            pos += 1;
        }

    }

    // Cleanup
    for (int i = 0; i < item_idx; i++) {
        free(items[i].str);
    }
    free(items);

    // Highlight first completion
    if (first_completion_end > first_completion_start) {
        put_text_property(completions_buf, first_completion_start, first_completion_end,
                         face_sym, scm_from_int(highlight_face));
    }

    completions_buf->pt = first_completion_start;
    completions_buf->modified = false;
    completions_buf->read_only = true;

    // Display window

    Window *leaves[256];
    int window_count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &window_count);

    Window *existing = NULL;
    for (int i = 0; i < window_count; i++) {
        if (leaves[i]->buffer == completions_buf) {
            existing = leaves[i];
            break;
        }
    }

    // Count lines in the completed buffer to size the window
    int line_count = 0;
    {
        size_t buf_len = rope_char_length(completions_buf->rope);
        for (size_t i = 0; i < buf_len; i++) {
            char c;
            rope_copy_chars(completions_buf->rope, i, 1, &c, 1);
            if (c == '\n') line_count++;
        }
        // Count final line if no trailing newline
        if (buf_len > 0) {
            char last;
            rope_copy_chars(completions_buf->rope, buf_len - 1, 1, &last, 1);
            if (last != '\n') line_count++;
        }
    }

    // At least 3 lines, no upper cap — let the window be as tall as the content
    // NOTE The ? 3 is not really needed
    int desired_lines = line_count < 3 ? 3 : line_count;

    if (!existing) {
        Window *bottom = split_root_window_below(-desired_lines);
        if (bottom) {
            bottom->buffer = completions_buf;
            bottom->point = first_completion_start;
        }
    } else {
        existing->point = first_completion_start;
        fit_window_to_buffer(existing);
    }
}

static void close_completion_window() {
    Buffer *completions_buf = get_buffer("*Completions*");
    if (!completions_buf) return;

    Window *leaves[256];
    int window_count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &window_count);

    Window *completions_win = NULL;
    for (int i = 0; i < window_count; i++) {
        if (leaves[i]->buffer == completions_buf) {
            completions_win = leaves[i];
            break;
        }
    }
    if (!completions_win) return;

    Window *mb_win = selected_frame->wm.minibuffer_window;
    size_t mb_pt = mb_win->point;

    // The completions window is always the right/bottom child of root
    // (created by split_root_window_below). Its sibling is top_wrapper
    // which contains all the real windows. Promote it back to root.
    Window *parent = completions_win->parent;
    if (!parent) return;

    Window *sibling = (parent->left == completions_win) ? parent->right : parent->left;
    if (!sibling) return;

    // Promote sibling to take parent's place
    sibling->parent = parent->parent;
    if (parent->parent) {
        if (parent->parent->left == parent)
            parent->parent->left = sibling;
        else
            parent->parent->right = sibling;
    } else {
        selected_frame->wm.root = sibling;
    }

    sibling->x = parent->x;
    sibling->y = parent->y;
    sibling->width = parent->width;
    sibling->height = parent->height;

    free(completions_win);
    free(parent);
    selected_frame->wm.window_count--;

    // Restore minibuffer as selected — do NOT touch current_buffer's pt
    // via set_buffer, just wire everything up directly
    selected_frame->wm.selected->is_selected = false;
    selected_frame->wm.selected = mb_win;
    mb_win->is_selected = true;
    current_buffer = mb_win->buffer;
    current_buffer->pt = mb_pt;
    mb_win->point = mb_pt;

    wm_recalculate_layout();
}

void minibuffer_complete() {
    if (!selected_frame->wm.minibuffer_active) return;

    if (scm_is_false(completion_collection)) {
        message("No completion collection set");
        return;
    }

    bool repeated = is_scm_proc(last_command, "minibuffer-complete");

    if (repeated && frame_resized_since_last_complete && last_completion_max_len > 0) {
        Buffer *completions_buf = get_buffer("*Completions*");
        Window *completions_win = completions_buf ? get_buffer_window(completions_buf) : NULL;
        if (completions_win) {
            int window_width = completions_win->width / selected_frame->column_width;
            if (window_width < 40) window_width = 80;
            int new_columns = window_width / ((int)last_completion_max_len + 8);
            if (new_columns < 1) new_columns = 1;
            if (new_columns > last_completion_count) new_columns = last_completion_count;
            repeated = (new_columns == last_completion_column_count);
        }
    }
    frame_resized_since_last_complete = false;

    if (repeated) {
        Buffer *completions_buf = get_buffer("*Completions*");
        Window *completions_win = completions_buf ? get_buffer_window(completions_buf) : NULL;

        if (completions_win) {
            Window *saved_selected = selected_frame->wm.selected;
            Buffer *saved_buffer   = current_buffer;
            size_t  saved_pt       = saved_buffer->pt;

            selected_frame->wm.selected = completions_win;
            current_buffer              = completions_buf;
            current_buffer->pt          = completions_win->point;

            Font *font          = face_cache->faces[FACE_DEFAULT]->font;
            float line_height   = font->ascent + font->descent;
            float usable_height = completions_win->height - line_height;
            float scroll_bottom = completions_win->scrolly + usable_height;

            size_t total_lines = 0;
            rope_iter_t iter;
            rope_iter_init(&iter, completions_buf->rope, 0);
            uint32_t ch;
            while (rope_iter_next_char(&iter, &ch)) {
                if (ch == '\n') total_lines++;
            }
            rope_iter_destroy(&iter);
            float last_line_y = total_lines * line_height;

            if (scroll_bottom >= last_line_y) {
                completions_win->scrolly = 0;
                completions_win->point   = 0;
                current_buffer->pt       = 0;
            } else {
                scroll_up_command();
                completions_win->point = current_buffer->pt;
            }

            selected_frame->wm.selected = saved_selected;
            current_buffer              = saved_buffer;
            current_buffer->pt          = saved_pt;
            return;
        }
        // Window gone — fall through to re-show completions
    }

    char *input = get_minibuffer_contents();
    if (!input) input = strdup("");

    // For empty input with a list collection, return all items.
    // For a procedure collection, all_completions_internal handles it.
    SCM all_matches;
    if (*input == '\0' && scm_is_true(scm_list_p(completion_collection))) {
        all_matches = SCM_EOL;
        SCM tail = completion_collection;
        while (scm_is_pair(tail)) {
            SCM item = scm_car(tail);
            if (scm_is_symbol(item))
                all_matches = scm_cons(scm_symbol_to_string(item), all_matches);
            else if (scm_is_string(item))
                all_matches = scm_cons(item, all_matches);
            tail = scm_cdr(tail);
        }
        all_matches = scm_reverse(all_matches);
    } else {
        all_matches = all_completions_internal(input, completion_collection);
    }

    int match_count = scm_to_int(scm_length(all_matches));

    if (match_count == 0) {
        message("No match");
        free(input);
        return;
    }

    // Helper: replace minibuffer input (after prompt) with completion string
    // For file completion the procedure returns the full path from try_completion,
    // so we always replace the entire input, not just the last component.
#define REPLACE_MINIBUFFER_INPUT(comp_str)                                      \
    do {                                                                        \
        close_completion_window();                                              \
        Buffer *mb        = selected_frame->wm.minibuffer_window->buffer;       \
        SCM field_sym     = scm_from_locale_symbol("field");                    \
        size_t total_len  = rope_char_length(mb->rope);                         \
        size_t prompt_end = 0;                                                  \
        for (size_t i = 0; i < total_len; i++) {                                \
            SCM field = get_text_property(mb, i, field_sym);                    \
            if (scm_is_true(field)) {                                           \
                prompt_end = i + 1;                                             \
            } else {                                                            \
                break;                                                          \
            }                                                                   \
        }                                                                       \
        size_t old_len = total_len - prompt_end;                                \
        if (old_len > 0) {                                                      \
            remove_text_properties(mb, prompt_end, total_len);                  \
            mb->rope = rope_delete_chars(mb->rope, prompt_end, old_len);        \
        }                                                                       \
        size_t comp_len = strlen(comp_str);                                     \
        mb->rope = rope_insert_chars(mb->rope, prompt_end, comp_str, comp_len); \
        mb->pt   = prompt_end + comp_len;                                       \
        selected_frame->wm.minibuffer_window->point = mb->pt;                   \
    } while (0)

    if (match_count == 1) {
        SCM match = scm_car(all_matches);
        if (scm_is_symbol(match)) match = scm_symbol_to_string(match);
        char *completion = scm_to_locale_string(match);

        // For procedure collections, try_completion returns the full path.
        // Use that instead of the bare all-completions entry.
        if (scm_is_true(scm_procedure_p(completion_collection))) {
            SCM full = try_completion_internal(input, completion_collection);
            if (scm_is_string(full)) {
                free(completion);
                completion = scm_to_locale_string(full);
            }
        }

        if (strcasecmp(input, completion) == 0) {
            message("Sole completion");
            free(completion);
            free(input);
            return;
        }

        REPLACE_MINIBUFFER_INPUT(completion);
        free(completion);
        free(input);
        return;
    }

    // Multiple matches — get longest common prefix via try_completion
    SCM result = try_completion_internal(input, completion_collection);

    if (scm_is_false(result)) {
        message("No match");
        free(input);
        return;
    }

    // try_completion returns #t when input is already an exact match
    if (scm_is_true(result) && !scm_is_string(result)) {
        display_completions(all_matches, input);
        free(input);
        return;
    }

    char *completion = scm_to_locale_string(result);

    if (strcasecmp(input, completion) != 0) {
        REPLACE_MINIBUFFER_INPUT(completion);
    } else {
        bool is_valid = is_exact_completion(input, completion_collection);
        if (is_valid) message("Complete, but not unique");
        display_completions(all_matches, input);
    }

#undef REPLACE_MINIBUFFER_INPUT

    free(completion);
    free(input);
}

void minibuffer_complete_and_exit() {
    if (!selected_frame->wm.minibuffer_active) return;

    if (scm_is_false(completion_collection)) {
        minibuffer_result = get_minibuffer_contents();
        minibuffer_exit_requested = true;
        minibuffer_abort_requested = false;
        return;
    }

    char *input = get_minibuffer_contents();
    if (!input) input = strdup("");

    // Empty input — exit with empty string so caller can apply its default
    if (*input == '\0') {
        minibuffer_result = input;
        minibuffer_exit_requested = true;
        minibuffer_abort_requested = false;
        return;
    }

    bool exact_match = is_exact_completion(input, completion_collection);
    if (exact_match) {
        minibuffer_result = input;
        minibuffer_exit_requested = true;
        minibuffer_abort_requested = false;
    } else {
        message("No match");
        free(input);
    }
}

char *read_from_minibuffer_with_completion(const char *prompt, const char *initial_contents, SCM collection, SCM predicate, SCM hist) {
    // Set completion state
    completion_collection = collection;
    completion_predicate = predicate;

    // Read from minibuffer with history
    char *result = read_from_minibuffer_internal(prompt, initial_contents, hist);

    // Clear completion state
    completion_collection = SCM_BOOL_F;
    completion_predicate = SCM_BOOL_F;

    return result;
}

// Recursive edit - runs event loop until minibuffer exits
static void recursive_edit() {
    // Run the event loop until minibuffer_exit_requested becomes true
    while (!minibuffer_exit_requested && !windowShouldClose()) {
        beginFrame();

        // Draw everything
        clear_background(face_cache->faces[FACE_DEFAULT]->bg);
        wm_draw(&selected_frame->wm);

        endFrame();
    }
}

char *read_from_minibuffer_internal(const char *prompt,
                                    const char *initial_contents,
                                    SCM hist) {
    SCM history_symbol;
    bool record_history = true;

    if (SCM_UNBNDP(hist) || scm_is_false(hist)) {
        history_symbol = scm_from_locale_symbol("minibuffer-history");
    } else if (scm_is_eq(hist, SCM_BOOL_T)) {
        history_symbol = scm_from_locale_symbol("minibuffer-history-no-record");
        record_history = false;
    } else if (scm_is_symbol(hist)) {
        history_symbol = hist;
    } else {
        history_symbol = scm_from_locale_symbol("minibuffer-history");
    }

    get_history_variable(history_symbol);
    active_history_state = get_or_create_history_state(history_symbol);
    set_history_position(history_symbol, -1);
    clear_original_input(history_symbol);

    minibuffer_exit_requested = false;
    minibuffer_abort_requested = false;
    if (minibuffer_result) {
        free(minibuffer_result);
        minibuffer_result = NULL;
    }

    activate_minibuffer();
    selected_frame->wm.minibuffer_message_start = 0;

    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;
    size_t pos = 0;

    if (prompt) {
        size_t len = strlen(prompt);
        mb->rope = rope_insert_chars(mb->rope, pos, prompt, len);
        put_text_property(mb, pos, len, scm_from_locale_symbol("read-only"), SCM_BOOL_T);
        int msg_face = face_id_from_name("minibuffer-prompt");
        put_text_property(mb, pos, len, scm_from_locale_symbol("face"), scm_from_int(msg_face));
        put_text_property(mb, pos, len, scm_from_locale_symbol("field"), SCM_BOOL_T);
        pos += len;
    }

    if (initial_contents && *initial_contents) {
        size_t len = strlen(initial_contents);
        mb->rope = rope_insert_chars(mb->rope, pos, initial_contents, len);
        pos += len;
    }

    mb->pt = pos;
    selected_frame->wm.minibuffer_window->point = mb->pt;

    recursive_edit();

    deactivate_minibuffer();

    char *result;
    if (minibuffer_abort_requested) {
        // C-g — return NULL so callers can distinguish abort from empty RET
        result = NULL;
    } else {
        result = minibuffer_result ? minibuffer_result : strdup("");
        if (result && *result && record_history) {
            add_to_history(history_symbol, result);
        }
    }

    minibuffer_result = NULL;
    minibuffer_exit_requested = false;
    minibuffer_abort_requested = false;
    active_history_state = NULL;

    return result;
}

char *read_from_minibuffer(const char *prompt, const char *initial_contents, SCM hist) {
    return read_from_minibuffer_internal(prompt, initial_contents, hist);
}

void execute_extended_command() {
    int arg = get_prefix_arg();
    set_prefix_arg(1);

    char prompt[64];
    if (arg != 1)
        snprintf(prompt, sizeof(prompt), "%d M-x ", arg);
    else
        snprintf(prompt, sizeof(prompt), "M-x ");

    SCM all_symbols = scm_c_eval_string(
        "(hash-map->list (lambda (k v) k) (module-obarray (current-module)))");
    SCM commands = SCM_EOL;
    SCM tail = all_symbols;

    while (scm_is_pair(tail)) {
        SCM sym = scm_car(tail);
        if (scm_is_symbol(sym)) {
            SCM var = scm_module_variable(scm_current_module(), sym);
            if (scm_is_true(var) && scm_is_true(scm_variable_bound_p(var))) {
                SCM value = scm_variable_ref(var);
                if (scm_is_true(scm_procedure_p(value))) {
                    SCM spec = scm_procedure_property(
                        value, scm_from_utf8_symbol("interactive-spec"));
                    if (!scm_is_false(spec))
                        commands = scm_cons(sym, commands);
                }
            }
        }
        tail = scm_cdr(tail);
    }

    SCM hist = scm_from_locale_symbol("extended-command-history");
    char *input = read_from_minibuffer_with_completion(prompt, NULL,
                                                        commands, SCM_BOOL_F, hist);
    if (!input) return;

    if (*input) {
        SCM command_sym = scm_from_locale_symbol(input);
        SCM command_var = scm_module_variable(scm_current_module(), command_sym);

        if (scm_is_false(command_var) ||
            scm_is_false(scm_variable_bound_p(command_var))) {
            message("No such command: %s", input);
            free(input);
            return;
        }

        SCM command_func = scm_variable_ref(command_var);

        if (!scm_is_true(scm_procedure_p(command_func))) {
            message("%s is not a command", input);
            free(input);
            return;
        }

        for (int i = 0; i < abs(arg); i++) {
            set_prefix_arg(arg < 0 ? -1 : 1);
            call_interactively(command_func);
        }

        // Find key binding for this command
        const char *found_notation = NULL;

        for (size_t i = 0; i < keymap.count && !found_notation; i++) {
            KeyChordBinding *b = &keymap.bindings[i];
            if (b->action_type == ACTION_SCHEME_PROC &&
                scm_is_eq(b->action.scheme_proc, command_func))
                found_notation = b->notation;
        }

        for (int s = (int)keymap_stack_count - 1; s >= 0 && !found_notation; s--) {
            KeyChordMap *local = keymap_stack[s];
            if (!local) continue;
            for (size_t i = 0; i < local->count && !found_notation; i++) {
                KeyChordBinding *b = &local->bindings[i];
                if (b->action_type == ACTION_SCHEME_PROC &&
                    scm_is_eq(b->action.scheme_proc, command_func))
                    found_notation = b->notation;
            }
        }

        if (found_notation) {
            char prefix_str[512];
            snprintf(prefix_str, sizeof(prefix_str),
                     "You can run the command '%s' with %s", input, found_notation);
//                     TODO  I want to use    ‘  ’ but for some reason it doesn't apply the face if i use smart quotes.
            message_for(2.0, "%s", prefix_str);

            Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
            size_t prefix_len   = strlen(prefix_str) - strlen(found_notation);
            size_t notation_len = strlen(found_notation);
            int face = face_id_from_name("help-key-binding");
            put_text_property(minibuf, prefix_len, prefix_len + notation_len,
                              scm_from_locale_symbol("face"),
                              scm_from_int(face));
        }



    }

    free(input);
}

void eval_expression() {
    // Use 'read-expression-history for eval
    SCM hist = scm_from_locale_symbol("read-expression-history");
    char *input = read_from_minibuffer("Eval: ", NULL, hist);

    if (input && *input) {
        // Evaluate with error handling
        SCM result = scm_internal_catch(SCM_BOOL_T,
                                        eval_string_body, input,
                                        error_handler, NULL);

        // Display result (either value or error message)
        char *result_str = scm_to_locale_string(scm_object_to_string(result, SCM_UNDEFINED));
        message(result_str);
        free(result_str);
    }

    free(input);
}


char *get_current_message(void) {
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    if (selected_frame->wm.minibuffer_active) return strdup("");
    size_t len = rope_char_length(minibuf->rope);
    if (len == 0) return strdup("");
    size_t byte_len = 0;
    char *result = rope_to_string(minibuf->rope, &byte_len);
    if (!result) return strdup("");
    return result;
}

/* char *get_current_message(void) { */
/*     Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer; */
/*     if (selected_frame->wm.minibuffer_active) return strdup(""); */
/*     size_t len = rope_char_length(minibuf->rope); */
/*     if (len == 0) return strdup(""); */
/*     char *result = rope_to_cstring(minibuf->rope); */
/*     return result ? result : strdup(""); */
/* } */

void restore_message(const char *saved) {
    if (!saved) return;
    if (selected_frame->wm.minibuffer_active) return;
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    size_t len = rope_char_length(minibuf->rope);
    if (len > 0) {
        clear_text_properties(minibuf);
        minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
    }
    size_t slen = strlen(saved);
    if (slen > 0)
        minibuf->rope = rope_insert_chars(minibuf->rope, 0, saved, slen);
    selected_frame->wm.minibuffer_window->point = 0;
    selected_frame->wm.minibuffer_message_start = 0;
}





#include <sys/stat.h>
#include <libgen.h>  // for dirname/basename

// Helper to check if file exists
static bool file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

// Helper to read file contents
static char* read_file_contents(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *contents = malloc(size + 1);
    if (!contents) {
        fclose(f);
        return NULL;
    }

    size_t read = fread(contents, 1, size, f);
    contents[read] = '\0';
    fclose(f);

    if (out_len) *out_len = read;
    return contents;
}

// Helper to extract directory from path
static char* get_directory_from_path(const char *path) {
    char *path_copy = strdup(path);
    char *dir = dirname(path_copy);
    char *result = strdup(dir);
    free(path_copy);
    return result;
}

// Helper to extract filename from path
static char* get_filename_from_path(const char *path) {
    char *path_copy = strdup(path);
    char *base = basename(path_copy);
    char *result = strdup(base);
    free(path_copy);
    return result;
}

#include <pwd.h>
#include <unistd.h>

// Helper to expand ~ to home directory
static char* expand_tilde(const char *path) {
    if (path[0] != '~') {
        return strdup(path);
    }

    const char *home;
    const char *rest = path + 1;

    if (path[1] == '/' || path[1] == '\0') {
        // ~/... or just ~
        home = getenv("HOME");
        if (!home) {
            struct passwd *pw = getpwuid(getuid());
            home = pw ? pw->pw_dir : ".";
        }
    } else {
        // ~username/...
        const char *slash = strchr(path, '/');
        size_t username_len = slash ? (slash - path - 1) : strlen(path + 1);
        char username[256];
        strncpy(username, path + 1, username_len);
        username[username_len] = '\0';

        struct passwd *pw = getpwnam(username);
        if (!pw) {
            return strdup(path); // Can't expand, return as-is
        }
        home = pw->pw_dir;
        rest = slash ? slash : "";
    }

    char *result = malloc(strlen(home) + strlen(rest) + 1);
    sprintf(result, "%s%s", home, rest);
    return result;
}

// Helper to abbreviate home directory to ~
static char* abbreviate_home(const char *path) {
    const char *home = getenv("HOME");
    if (!home) {
        struct passwd *pw = getpwuid(getuid());
        home = pw ? pw->pw_dir : NULL;
    }

    if (!home) {
        return strdup(path);
    }

    size_t home_len = strlen(home);
    if (strncmp(path, home, home_len) == 0 &&
        (path[home_len] == '/' || path[home_len] == '\0')) {
        // Path starts with home directory
        char *result = malloc(strlen(path) - home_len + 2);
        sprintf(result, "~%s", path + home_len);
        return result;
    }

    return strdup(path);
}

#include <dirent.h>

static SCM build_file_completions(const char *typed_path) {
    // Split into directory part and prefix to match against.
    // "~/src/gl"  -> dir_part="~/src/"  prefix="gl"
    // "~/src/"    -> dir_part="~/src/"  prefix=""
    // "foo"       -> dir_part=""        prefix="foo"
    char dir_part[PATH_MAX] = "";
    char prefix[PATH_MAX]   = "";

    const char *last_slash = strrchr(typed_path, '/');
    if (last_slash) {
        size_t dir_len = last_slash - typed_path + 1;
        strncpy(dir_part, typed_path, dir_len);
        dir_part[dir_len] = '\0';
        strncpy(prefix, last_slash + 1, PATH_MAX - 1);
    } else {
        strncpy(prefix, typed_path, PATH_MAX - 1);
    }

    // Expand dir_part to a real path we can opendir()
    char *expanded = expand_tilde(*dir_part ? dir_part : ".");
    char abs_dir[PATH_MAX];
    if (realpath(expanded, abs_dir) == NULL)
        strncpy(abs_dir, expanded, PATH_MAX - 1);
    free(expanded);

    DIR *d = opendir(abs_dir);
    if (!d) return SCM_EOL;

    // Four buckets matching Emacs ordering:
    //   0: "../"
    //   1: "./"
    //   2: hidden entries (start with '.' but not "." or "..")
    //   3: normal entries
    typedef struct Entry { char name[PATH_MAX + 1]; struct Entry *next; } Entry;
    Entry *buckets[4] = {NULL, NULL, NULL, NULL};
    int    counts[4]  = {0,    0,    0,    0};

    struct dirent *de;
    while ((de = readdir(d)) != NULL) {
        const char *e = de->d_name;

        // Build the display name: directories get a trailing '/'
        char display[PATH_MAX + 2];
        bool is_dotdot = (strcmp(e, "..") == 0);
        bool is_dot    = (strcmp(e, ".")  == 0);

        if (is_dot || is_dotdot) {
            snprintf(display, sizeof(display), "%s/", e);
        } else {
            char full[PATH_MAX];
            snprintf(full, sizeof(full), "%s/%s", abs_dir, e);
            struct stat st;
            if (stat(full, &st) == 0 && S_ISDIR(st.st_mode))
                snprintf(display, sizeof(display), "%s/", e);
            else
                snprintf(display, sizeof(display), "%s", e);
        }

        // Filter by prefix
        if (!string_prefix_p(display, prefix))
            continue;

        // Assign bucket
        int b;
        if (is_dotdot)            b = 0;
        else if (is_dot)          b = 1;
        else if (e[0] == '.')     b = 2;
        else                      b = 3;

        Entry *entry = malloc(sizeof(Entry));
        strncpy(entry->name, display, PATH_MAX);
        entry->next  = buckets[b];
        buckets[b]   = entry;
        counts[b]++;
    }
    closedir(d);

    // Sort each bucket alphabetically
    for (int b = 0; b < 4; b++) {
        if (counts[b] < 2) continue;

        char **arr = malloc(counts[b] * sizeof(char *));
        Entry *cur = buckets[b];
        for (int i = 0; i < counts[b]; i++, cur = cur->next)
            arr[i] = cur->name;

        for (int i = 1; i < counts[b]; i++) {
            char *key = arr[i];
            int   j   = i - 1;
            while (j >= 0 && strcmp(arr[j], key) > 0) {
                arr[j+1] = arr[j];
                j--;
            }
            arr[j+1] = key;
        }

        cur = buckets[b];
        for (int i = 0; i < counts[b]; i++, cur = cur->next)
            cur->name[0] = '\0',
            strncpy(cur->name, arr[i], PATH_MAX);
        free(arr);
    }

    // Build SCM list — bare names only, no dir_part prefix
    SCM result = SCM_EOL;
    // Walk buckets in reverse order so scm_cons builds the list forwards
    for (int b = 3; b >= 0; b--) {
        // Collect bucket into array for reverse-order consing
        int cnt = counts[b];
        if (cnt == 0) continue;
        char **arr = malloc(cnt * sizeof(char *));
        Entry *cur = buckets[b];
        for (int i = 0; i < cnt; i++, cur = cur->next)
            arr[i] = cur->name;

        for (int i = cnt - 1; i >= 0; i--)
            result = scm_cons(scm_from_locale_string(arr[i]), result);
        free(arr);
    }

    // Free bucket lists
    for (int b = 0; b < 4; b++) {
        Entry *cur = buckets[b];
        while (cur) { Entry *next = cur->next; free(cur); cur = next; }
    }

    return result;
}

Buffer *find_file(const char *filename) {
    char *input = NULL;
    bool should_free_input = false;

    if (filename == NULL || *filename == '\0') {
        SCM read_file_name_func = scm_variable_ref(scm_c_lookup("read-file-name"));
        SCM result = scm_call_1(read_file_name_func,
                                scm_from_locale_string("Find file: "));
        input = scm_to_locale_string(result);
        should_free_input = true;
        if (!input || !*input) {
            if (input) free(input);
            return NULL;
        }
    } else {
        input = (char *)filename;
    }

    // Expand tilde if present
    char *expanded_input = expand_tilde(input);
    if (should_free_input) free(input);

    // Combine with default-directory if relative
    char full_path[PATH_MAX];
    if (expanded_input[0] == '/') {
        snprintf(full_path, PATH_MAX, "%s", expanded_input);
    } else {
        SCM dd_val = buffer_local_value(scm_from_locale_symbol("default-directory"),
                                        current_buffer);
        const char *default_dir = scm_is_string(dd_val)
                                  ? scm_to_locale_string(dd_val)
                                  : "./";
        snprintf(full_path, PATH_MAX, "%s/%s", default_dir, expanded_input);
    }
    free(expanded_input);

    // Resolve to absolute path
    char absolute_path[PATH_MAX];
    if (realpath(full_path, absolute_path) == NULL) {
        // File doesn't exist yet (new file) — use full_path as-is
        snprintf(absolute_path, PATH_MAX, "%s", full_path);
    }

    // If it's a directory, hand off to dired
    struct stat st;
    if (stat(absolute_path, &st) == 0 && S_ISDIR(st.st_mode)) {
        // Ensure trailing slash
        size_t alen = strlen(absolute_path);
        if (alen + 2 < PATH_MAX && absolute_path[alen - 1] != '/')
            absolute_path[alen] = '/', absolute_path[alen + 1] = '\0';

        SCM dired_var = scm_c_lookup("dired");
        if (scm_is_true(scm_variable_bound_p(dired_var)))
            scm_call_1(scm_variable_ref(dired_var),
                       scm_from_locale_string(absolute_path));
        return current_buffer;
    }

    // Check if a buffer already exists for this file
    Buffer *buf = all_buffers;
    if (buf) {
        do {
            if (buf->filename && strcmp(buf->filename, absolute_path) == 0) {
                switch_to_buffer(buf);
                message("Switched to existing buffer: %s", buf->name);
                return buf;
            }
            buf = buf->next;
        } while (buf != all_buffers);
    }

    // Create a unique buffer name from the filename
    char *base_name = get_filename_from_path(absolute_path);
    char *buffer_name = strdup(base_name);
    int suffix = 1;
    while (get_buffer(buffer_name)) {
        free(buffer_name);
        buffer_name = malloc(strlen(base_name) + 20);
        sprintf(buffer_name, "%s<%d>", base_name, suffix++);
    }

    // Create the buffer
    buf = buffer_create(buffer_name);
    free(buffer_name);
    free(base_name);

    // Set filename
    buf->filename = strdup(absolute_path);

    // Set default-directory to the file's directory (with trailing slash)
    char *dir = get_directory_from_path(absolute_path);
    size_t dlen = strlen(dir);
    char *dir_slash = malloc(dlen + 2);
    memcpy(dir_slash, dir, dlen);
    if (dir[dlen - 1] != '/') { dir_slash[dlen] = '/'; dir_slash[dlen + 1] = '\0'; }
    else                       { dir_slash[dlen] = '\0'; }
    free(dir);
    buffer_set(scm_from_locale_symbol("default-directory"),
               scm_from_locale_string(dir_slash), buf);
    free(dir_slash);

    // Read file contents if the file exists
    if (file_exists(absolute_path)) {
        size_t file_len;
        char *contents = read_file_contents(absolute_path, &file_len);
        if (contents) {
            buf->rope = rope_insert_chars(buf->rope, 0, contents, file_len);
            buf->modified = false;
            free(contents);
            message("Read %zu characters from %s", file_len, absolute_path);
        } else {
            message("Error reading file: %s", absolute_path);
        }
    } else {
        message("(New file)");
        buf->modified = false;
    }

    // Switch to the new buffer
    switch_to_buffer(buf);
    buf->pt = 0;

    // Run find-file-hook
    SCM find_file_hook = scm_c_lookup("find-file-hook");
    if (!scm_is_false(scm_variable_bound_p(find_file_hook))) {
        SCM hooks = scm_variable_ref(find_file_hook);
        if (scm_is_true(scm_list_p(hooks))) {
            while (!scm_is_null(hooks)) {
                SCM hook = scm_car(hooks);
                if (scm_is_true(scm_procedure_p(hook)))
                    scm_call_0(hook);
                hooks = scm_cdr(hooks);
            }
        }
    }

    return buf;
}

void clear_minibuffer_message() {
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;

    if (selected_frame->wm.minibuffer_active) {
        // Clear only the echo area (the [...] message part)
        if (selected_frame->wm.minibuffer_message_start > 0) {
            size_t current_len = rope_char_length(minibuf->rope);
            if (current_len > selected_frame->wm.minibuffer_message_start) {
                size_t msg_len = current_len - selected_frame->wm.minibuffer_message_start;
                remove_text_properties(minibuf, selected_frame->wm.minibuffer_message_start, current_len);
                minibuf->rope = rope_delete_chars(minibuf->rope,
                                                  selected_frame->wm.minibuffer_message_start,
                                                  msg_len);
                selected_frame->wm.minibuffer_message_start = 0;
            }
        }
    } else {
        // Clear entire minibuffer if not active
        size_t len = rope_char_length(minibuf->rope);
        if (len > 0) {
            clear_text_properties(minibuf);
            minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
            selected_frame->wm.minibuffer_window->point = 0;
        }
    }
}

// Helper to find the start of a completion item at or before point
static size_t find_completion_start(Buffer *buf, size_t point) {
    if (point == 0) return 0;

    // Skip backwards over the current completion until we hit:
    // - start of buffer
    // - a newline
    // - a tab
    size_t pos = point;
    while (pos > 0) {
        char c;
        rope_copy_chars(buf->rope, pos - 1, 1, &c, 1);
        if (c == '\n' || c == '\t') {
            break;
        }
        pos--;
    }

    return pos;
}

// Helper to find the end of a completion item at or after point
static size_t find_completion_end(Buffer *buf, size_t point) {
    size_t len = rope_char_length(buf->rope);
    size_t pos = point;

    while (pos < len) {
        char c;
        rope_copy_chars(buf->rope, pos, 1, &c, 1);
        if (c == '\n' || c == '\t') {
            break;
        }
        pos++;
    }

    return pos;
}

// Helper to skip to the start of the next completion item
static size_t next_completion_position(Buffer *buf, size_t point, int n) {
    size_t len = rope_char_length(buf->rope);
    size_t pos = point;

    // Find where completions actually start (after help text)
    // Look for the line that says "N possible completion"
    size_t completions_start = 0;
    bool found_count_line = false;

    for (size_t i = 0; i < len; i++) {
        char line[256];
        size_t line_len = 0;

        // Read until newline or end
        while (i + line_len < len && line_len < sizeof(line) - 1) {
            rope_copy_chars(buf->rope, i + line_len, 1, &line[line_len], 1);
            if (line[line_len] == '\n') {
                break;
            }
            line_len++;
        }
        line[line_len] = '\0';

        // Check if this is the count line
        if (strstr(line, "possible completion") != NULL) {
            found_count_line = true;
            // Skip to next line after the count line
            completions_start = i + line_len + 1;

            // Skip one more blank line if present
            if (completions_start < len) {
                char next;
                rope_copy_chars(buf->rope, completions_start, 1, &next, 1);
                if (next == '\n') {
                    completions_start++;
                }
            }
            break;
        }

        // Skip to next line
        i += line_len;
    }

    // If we're before completions start, jump to first completion
    if (pos < completions_start) {
        pos = completions_start;
    }

    // Move n times
    for (int i = 0; i < abs(n); i++) {
        if (n > 0) {
            // Move forward
            // Skip to end of current completion
            pos = find_completion_end(buf, pos);

            // Skip whitespace (tabs, spaces, newlines)
            while (pos < len) {
                char c;
                rope_copy_chars(buf->rope, pos, 1, &c, 1);
                if (c != ' ' && c != '\t' && c != '\n') {
                    break;
                }
                pos++;
            }

            // If we hit the end, wrap to first completion
            if (pos >= len) {
                pos = completions_start;
            }
        } else {
            // Move backward
            // Skip to start of current completion
            pos = find_completion_start(buf, pos);

            // If we're at the start already, skip back
            if (pos > completions_start) {
                pos--;
            }

            // Skip whitespace backwards
            while (pos > completions_start) {
                char c;
                rope_copy_chars(buf->rope, pos, 1, &c, 1);
                if (c != ' ' && c != '\t' && c != '\n') {
                    break;
                }
                pos--;
            }

            // If we went before completions start, wrap to end
            if (pos < completions_start) {
                pos = len;
                // Skip back over trailing whitespace
                while (pos > completions_start) {
                    pos--;
                    char c;
                    rope_copy_chars(buf->rope, pos, 1, &c, 1);
                    if (c != ' ' && c != '\t' && c != '\n') {
                        pos++;
                        break;
                    }
                }
            }

            // Now find the start of this completion
            pos = find_completion_start(buf, pos);
        }
    }

    return pos;
}

static char* get_completion_at_point(Buffer *buf, size_t point) {
    size_t start = find_completion_start(buf, point);
    size_t end = find_completion_end(buf, point);

    if (start >= end) {
        return strdup("");
    }

    size_t len = end - start;
    char *text = malloc(len * 4 + 1); // UTF-8 safety
    if (!text) return strdup("");

    size_t copied = rope_copy_chars(buf->rope, start, len, text, len * 4);
    text[copied] = '\0';

    // Trim trailing whitespace
    while (copied > 0 && (text[copied - 1] == ' ' || text[copied - 1] == '\t')) {
        text[--copied] = '\0';
    }

    return text;
}

// Helper to clear all completions-highlight faces
static void clear_completion_highlights(Buffer *buf) {
    size_t len = rope_char_length(buf->rope);
    if (len == 0) return;

    SCM face_sym = scm_from_locale_symbol("face");
    int highlight_face = face_id_from_name("completions-highlight");

    // Remove highlight face from entire buffer
    for (size_t i = 0; i < len; i++) {
        SCM face_val = get_text_property(buf, i, face_sym);
        if (scm_is_integer(face_val) && scm_to_int(face_val) == highlight_face) {
            remove_text_properties(buf, i, i + 1);
        }
    }
}

// Helper to highlight the completion at point
static void highlight_completion_at_point(Buffer *buf, size_t point) {
    size_t start = find_completion_start(buf, point);
    size_t end = find_completion_end(buf, point);

    if (start >= end) return;

    SCM face_sym = scm_from_locale_symbol("face");
    int highlight_face = face_id_from_name("completions-highlight");

    // Apply highlight face to the completion
    put_text_property(buf, start, end, face_sym, scm_from_int(highlight_face));
}

static void insert_completion_into_minibuffer(const char *completion) {
    if (!selected_frame->wm.minibuffer_active) return;

    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;

    // Find where the prompt ends
    size_t prompt_end = 0;
    SCM field_sym = scm_from_locale_symbol("field");
    size_t total_len = rope_char_length(mb->rope);

    for (size_t i = 0; i < total_len; i++) {
        SCM field = get_text_property(mb, i, field_sym);
        if (scm_is_false(field) || scm_is_null(field)) {
            prompt_end = i;
            break;
        }
    }

    // Delete old input
    size_t old_len = total_len - prompt_end;
    if (old_len > 0) {
        remove_text_properties(mb, prompt_end, total_len);
        mb->rope = rope_delete_chars(mb->rope, prompt_end, old_len);
    }

    // Insert new completion
    size_t comp_len = strlen(completion);
    mb->rope = rope_insert_chars(mb->rope, prompt_end, completion, comp_len);
    mb->pt = prompt_end + comp_len;
    selected_frame->wm.minibuffer_window->point = mb->pt;
}

void minibuffer_next_completion(int n) {
    // Get the *Completions* buffer
    Buffer *completions_buf = get_buffer("*Completions*");
    if (!completions_buf) {
        message("No completions window");
        return;
    }

    // Find the window showing completions
    Window *leaves[256];
    int window_count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &window_count);

    Window *completions_window = NULL;
    for (int i = 0; i < window_count; i++) {
        if (leaves[i]->buffer == completions_buf) {
            completions_window = leaves[i];
            break;
        }
    }

    if (!completions_window) {
        message("No completions window");
        return;
    }

    // Clear previous highlights
    clear_completion_highlights(completions_buf);

    // Move to next completion
    size_t new_point = next_completion_position(completions_buf,
                                                 completions_window->point,
                                                 n);

    completions_window->point = new_point;
    completions_buf->pt = new_point;

    // Highlight the new completion
    highlight_completion_at_point(completions_buf, new_point);

    // Get the completion text at the new position
    char *completion = get_completion_at_point(completions_buf, new_point);

    // Check if minibuffer-completion-auto-choose is set
    bool auto_choose = scm_get_bool("minibuffer-completion-auto-choose", false);

    if (auto_choose && completion && *completion) {
        // Insert the completion into the minibuffer
        insert_completion_into_minibuffer(completion);
    }

    free(completion);
}

void minibuffer_previous_completion(int n) {
    minibuffer_next_completion(-n);
}

void minibuffer_choose_completion() {
    // Get the *Completions* buffer
    Buffer *completions_buf = get_buffer("*Completions*");
    if (!completions_buf) {
        message("No completions");
        return;
    }

    // Find the window showing completions
    Window *leaves[256];
    int window_count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &window_count);

    Window *completions_window = NULL;
    for (int i = 0; i < window_count; i++) {
        if (leaves[i]->buffer == completions_buf) {
            completions_window = leaves[i];
            break;
        }
    }

    if (!completions_window) {
        message("No completions");
        return;
    }

    // Get the completion at point
    char *completion = get_completion_at_point(completions_buf,
                                               completions_window->point);

    if (!completion || !*completion) {
        free(completion);
        message("No completion at point");
        return;
    }

    // Insert it into the minibuffer
    insert_completion_into_minibuffer(completion);
    free(completion);

    // Exit
    if (selected_frame->wm.minibuffer_active) {
        minibuffer_result = get_minibuffer_contents();
        minibuffer_exit_requested = true;
        minibuffer_abort_requested = false;
    }
}

/// SCM bindings

static SCM scm_read_from_minibuffer(SCM prompt, SCM initial, SCM hist) {
    if (!scm_is_string(prompt)) {
        scm_wrong_type_arg("read-from-minibuffer", 1, prompt);
    }

    char *prompt_str = scm_to_locale_string(prompt);
    char *initial_str = NULL;

    if (!SCM_UNBNDP(initial) && scm_is_string(initial)) {
        initial_str = scm_to_locale_string(initial);
    }

    // Pass hist through (can be #f, #t, symbol, or unbound)
    char *result = read_from_minibuffer(prompt_str, initial_str, hist);

    free(prompt_str);
    if (initial_str) free(initial_str);

    SCM result_scm = scm_from_locale_string(result);
    free(result);

    return result_scm;
}

static SCM scm_read_from_minibuffer_with_completion(SCM prompt, SCM initial, SCM collection, SCM predicate, SCM hist) {
    if (!scm_is_string(prompt))
        scm_wrong_type_arg("read-from-minibuffer-with-completion", 1, prompt);

    char *prompt_str = scm_to_locale_string(prompt);
    char *initial_str = NULL;

    if (!SCM_UNBNDP(initial) && scm_is_string(initial))
        initial_str = scm_to_locale_string(initial);

    char *result = read_from_minibuffer_with_completion(
        prompt_str,
        initial_str,
        SCM_UNBNDP(collection)  ? SCM_BOOL_F : collection,
        SCM_UNBNDP(predicate)   ? SCM_BOOL_F : predicate,
        SCM_UNBNDP(hist)        ? SCM_BOOL_F : hist
    );

    free(prompt_str);
    if (initial_str) free(initial_str);

    SCM result_scm = scm_from_locale_string(result ? result : "");
    free(result);
    return result_scm;
}

static SCM scm_execute_extended_command(void) {
    execute_extended_command();
    return SCM_UNSPECIFIED;
}

static SCM scm_eval_expression(void) {
    eval_expression();
    return SCM_UNSPECIFIED;
}

static SCM scm_find_file(SCM filename) {
    char *filename_str = NULL;

    // Check if filename argument was provided
    if (!SCM_UNBNDP(filename)) {
        if (!scm_is_string(filename)) {
            scm_wrong_type_arg("find-file", 1, filename);
        }
        filename_str = scm_to_locale_string(filename);
    }

    Buffer *buf = find_file(filename_str);

    if (filename_str) free(filename_str);

    if (buf) {
        return get_or_make_buffer_object(buf);
    }

    return SCM_BOOL_F;
}

static SCM scm_keyboard_quit(void) {
    keyboard_quit();
    return SCM_UNSPECIFIED;
}

static SCM scm_clear_minibuffer_message(void) {
    clear_minibuffer_message();
    return SCM_UNSPECIFIED;
}

static SCM scm_minibuffer_complete_and_exit(void) {
    minibuffer_complete_and_exit();
    return SCM_UNSPECIFIED;
}

static SCM scm_minibuffer_complete(void) {
    minibuffer_complete();
    return SCM_UNSPECIFIED;
}

static SCM scm_all_completions(SCM string, SCM collection) {
    if (!scm_is_string(string)) {
        scm_wrong_type_arg("all-completions", 1, string);
    }

    char *str = scm_to_locale_string(string);
    SCM result = all_completions_internal(str, collection);
    free(str);

    return result;
}

static SCM scm_try_completion(SCM string, SCM collection) {
    if (!scm_is_string(string)) {
        scm_wrong_type_arg("try-completion", 1, string);
    }

    char *str = scm_to_locale_string(string);
    SCM result = try_completion_internal(str, collection);
    free(str);

    return result;
}

static SCM scm_minibuffer_next_completion(SCM n) {
    int count = 1;
    if (!SCM_UNBNDP(n)) {
        if (!scm_is_integer(n)) {
            scm_wrong_type_arg("minibuffer-next-completion", 1, n);
        }
        count = scm_to_int(n);
    }

    minibuffer_next_completion(count);
    return SCM_UNSPECIFIED;
}

static SCM scm_minibuffer_previous_completion(SCM n) {
    int count = 1;
    if (!SCM_UNBNDP(n)) {
        if (!scm_is_integer(n)) {
            scm_wrong_type_arg("minibuffer-previous-completion", 1, n);
        }
        count = scm_to_int(n);
    }

    minibuffer_previous_completion(count);
    return SCM_UNSPECIFIED;
}

static SCM scm_minibuffer_choose_completion(void) {
    minibuffer_choose_completion();
    return SCM_UNSPECIFIED;
}

// SCM bindings for history

static SCM scm_previous_history_element(SCM n) {
    int count = 1;
    if (!SCM_UNBNDP(n)) {
        if (!scm_is_integer(n)) {
            scm_wrong_type_arg("minibuffer-previous-history-element", 1, n);
        }
        count = scm_to_int(n);
    }

    minibuffer_previous_history_element(count);
    return SCM_UNSPECIFIED;
}

static SCM scm_next_history_element(SCM n) {
    int count = 1;
    if (!SCM_UNBNDP(n)) {
        if (!scm_is_integer(n)) {
            scm_wrong_type_arg("minibuffer-next-history-element", 1, n);
        }
        count = scm_to_int(n);
    }

    minibuffer_next_history_element(count);
    return SCM_UNSPECIFIED;
}


static SCM scm_current_message(void) {
    char *msg    = get_current_message();
    SCM   result = scm_from_locale_string(msg);
    free(msg);
    return result;
}

static SCM scm_restore_message(SCM saved) {
    if (scm_is_string(saved)) {
        char *s = scm_to_locale_string(saved);
        restore_message(s);
        free(s);
    } else {
        restore_message("");
    }
    return SCM_UNSPECIFIED;
}

static SCM scm_with_temp_message(SCM msg_scm, SCM thunk) {
    char *saved = get_current_message();

    if (scm_is_string(msg_scm)) {
        char *msg = scm_to_locale_string(msg_scm);
        message("%s", msg);
        free(msg);
    }

    SCM result = scm_call_0(thunk);

    restore_message(saved);
    free(saved);
    return result;
}

void init_minibuf_bindings(void) {
    scm_c_define_gsubr("read-from-minibuffer",                 1, 2, 0, scm_read_from_minibuffer);
    scm_c_define_gsubr("read-from-minibuffer-with-completion", 3, 2, 0, scm_read_from_minibuffer_with_completion);
    scm_c_define_gsubr("execute-extended-command",             0, 0, 0, scm_execute_extended_command);
    scm_c_define_gsubr("eval-expression",                      0, 0, 0, scm_eval_expression);
    scm_c_define_gsubr("find-file",                            0, 1, 0, scm_find_file);
    scm_c_define_gsubr("keyboard-quit",                        0, 0, 0, scm_keyboard_quit);
    scm_c_define_gsubr("clear-minibuffer-message",             0, 0, 0, scm_clear_minibuffer_message);
    scm_c_define_gsubr("minibuffer-complete-and-exit",         0, 0, 0, scm_minibuffer_complete_and_exit);
    scm_c_define_gsubr("minibuffer-complete",                  0, 0, 0, scm_minibuffer_complete);
    scm_c_define_gsubr("all-completions",                      2, 0, 0, scm_all_completions);
    scm_c_define_gsubr("try-completion",                       2, 0, 0, scm_try_completion);
    scm_c_define_gsubr("minibuffer-next-completion",           0, 1, 0, scm_minibuffer_next_completion);
    scm_c_define_gsubr("minibuffer-previous-completion",       0, 1, 0, scm_minibuffer_previous_completion);
    scm_c_define_gsubr("minibuffer-choose-completion",         0, 0, 0, scm_minibuffer_choose_completion);
    scm_c_define_gsubr("previous-history-element",             0, 1, 0, scm_previous_history_element);
    scm_c_define_gsubr("next-history-element",                 0, 1, 0, scm_next_history_element);
    scm_c_define_gsubr("current-message",                      0, 0, 0, scm_current_message);
    scm_c_define_gsubr("restore-message",                      1, 0, 0, scm_restore_message);
    scm_c_define_gsubr("with-temp-message",                    2, 0, 0, scm_with_temp_message);
}

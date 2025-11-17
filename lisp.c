#include <libguile.h>
#include <obsidian/input.h>
#include <obsidian/theme.h>
#include <stdbool.h>
#include "lisp.h"
#include "libguile/scm.h"
#include "libguile/strings.h"
#include "rope.h"
#include "buffer.h"


// Error handler that captures error message with full details
static SCM error_handler(void *data, SCM key, SCM args) {
    SCM port = scm_open_output_string();
    
    // args structure: (subr message (arg ...) (extra ...))
    
    if (scm_is_pair(args)) {
        SCM rest = scm_cdr(args);
        
        if (scm_is_pair(rest)) {
            SCM message_template = scm_car(rest);  // The error message template with ~A, ~S
            SCM rest2 = scm_cdr(rest);
            
            // Get the error arguments to fill into the template
            SCM error_args = SCM_EOL;
            if (scm_is_pair(rest2)) {
                error_args = scm_car(rest2);
            }
            
            // Use scm_simple_format to properly format the message with arguments
            // This will replace ~A, ~S with the actual values
            if (!scm_is_null(error_args)) {
                scm_simple_format(port, message_template, error_args);
            } else {
                scm_display(message_template, port);
            }
        } else {
            // Fallback
            scm_display(args, port);
        }
    } else {
        // Simple fallback
        scm_display(args, port);
    }
    
    SCM str = scm_get_output_string(port);
    scm_close_port(port);
    
    return str;
}

// Body function wrapper for scm_c_catch
static SCM eval_string_body(void *data) {
    const char *str = (const char *)data;
    return scm_c_eval_string(str);
}

// Helper to safely evaluate string with error handling
static SCM safe_eval_string(const char *str, bool *had_error) {
    *had_error = false;
    
    SCM result = scm_c_catch(SCM_BOOL_T,
                             eval_string_body, (void *)str,
                             error_handler, NULL,
                             NULL, NULL);
    
    // Check if result is an error string (from our error_handler)
    if (scm_is_string(result)) {
        char *str_result = scm_to_locale_string(result);
        // Check if it contains common error keys
        if (strstr(str_result, "unbound-variable") ||
            strstr(str_result, "wrong-type-arg") ||
            strstr(str_result, "wrong-number-of-args") ||
            strstr(str_result, "misc-error") ||
            strstr(str_result, "out-of-range") ||
            strstr(str_result, "system-error")) {
            *had_error = true;
            return result;
        }
        free(str_result);
    }
    
    return result;
}

// Helper to convert SCM to string for display
static char* scm_to_display_string(SCM obj) {
    SCM str_port = scm_open_output_string();
    scm_write(obj, str_port);  // Changed from scm_display to scm_write
    SCM str_scm = scm_get_output_string(str_port);
    scm_close_port(str_port);
    
    char *result = scm_to_locale_string(str_scm);
    return result;
}


// Find the start of the last S-expression before point
static size_t find_sexp_start(Buffer *buf, size_t from_pos) {
    if (from_pos == 0) return 0;
    
    int paren_depth = 0;
    size_t pos = from_pos;
    
    // First, skip backwards over any whitespace
    while (pos > 0) {
        uint32_t ch = rope_char_at(buf->rope, pos - 1);
        if (ch != ' ' && ch != '\n' && ch != '\t') {
            break;
        }
        pos--;
    }
    
    if (pos == 0) return 0;
    
    // Check what's immediately before point (after skipping whitespace)
    uint32_t ch = rope_char_at(buf->rope, pos - 1);
    
    if (ch == ')' || ch == ']' || ch == '}') {
        // We're after a closing paren - find the matching opening paren
        paren_depth = 1;
        pos--;
        
        while (pos > 0 && paren_depth > 0) {
            ch = rope_char_at(buf->rope, pos - 1);
            
            if (ch == ')' || ch == ']' || ch == '}') {
                paren_depth++;
            } else if (ch == '(' || ch == '[' || ch == '{') {
                paren_depth--;
            }
            pos--;
        }
        
        // Now check for reader macros before the opening paren
        while (pos > 0) {
            ch = rope_char_at(buf->rope, pos - 1);
            // Check for quote ', backquote `, comma , or comma-at ,@
            if (ch == '\'' || ch == '`' || ch == ',') {
                pos--;
                // Handle ,@ (unquote-splicing)
                if (ch == ',' && pos > 0 && rope_char_at(buf->rope, pos - 1) == '@') {
                    pos--;
                }
            } else if (ch == '#') {
                // Could be #' or other syntax like #t, #f
                // For safety, include the # if it's right before our sexp
                pos--;
            } else {
                break;
            }
        }
        
        return pos;
    } else {
        // We're after an atom (symbol, number, etc.) - find its start
        pos--;
        while (pos > 0) {
            uint32_t prev = rope_char_at(buf->rope, pos - 1);
            if (prev == ' ' || prev == '\n' || prev == '\t' || 
                prev == '(' || prev == ')' || prev == '[' || prev == ']' ||
                prev == '{' || prev == '}') {
                break;
            }
            pos--;
        }
        
        // Check for reader macros before the atom
        while (pos > 0) {
            ch = rope_char_at(buf->rope, pos - 1);
            if (ch == '\'' || ch == '`' || ch == ',') {
                pos--;
                if (ch == ',' && pos > 0 && rope_char_at(buf->rope, pos - 1) == '@') {
                    pos--;
                }
            } else if (ch == '#') {
                pos--;
            } else {
                break;
            }
        }
        
        return pos;
    }
}

void eval_last_sexp() {
    Buffer *buf = wm.selected->buffer;
    
    size_t point = buf->pt;
    if (point == 0) {
        message("Beginning of buffer");
        return;
    }
    
    // Find the start of the s-expression
    size_t start = find_sexp_start(buf, point);
    
    if (start >= point) {
        message("No expression before point");
        return;
    }
    
    // Extract the expression
    size_t len = point - start;
    char *expr = malloc(len + 1);
    if (!expr) {
        message("Memory allocation failed");
        return;
    }
    
    for (size_t i = 0; i < len; i++) {
        expr[i] = (char)rope_char_at(buf->rope, start + i);
    }
    expr[len] = '\0';
    
    // Evaluate with error handling
    bool had_error = false;
    SCM result = safe_eval_string(expr, &had_error);
    
    if (had_error) {
        // Display error message
        char *error_str = scm_to_locale_string(result);
        message("%s", error_str);
        free(error_str);
    } else {
        // ALWAYS display result, even if it's #<unspecified>
        // Removed the check: if (!scm_is_eq(result, SCM_UNSPECIFIED))
        char *result_str = scm_to_display_string(result);
        message("=> %s", result_str);  // Added "=> " prefix like Emacs
        free(result_str);
    }
    
    free(expr);
}

void eval_region() {
    Buffer *buf = wm.selected->buffer;
    
    size_t start, end;
    region_bounds(&start, &end);
    
    if (start >= end) {
        message("Empty region");
        return;
    }
    
    // Extract the region text
    size_t len = end - start;
    char *code = malloc(len + 1);
    if (!code) {
        message("Memory allocation failed");
        return;
    }
    
    for (size_t i = 0; i < len; i++) {
        code[i] = (char)rope_char_at(buf->rope, start + i);
    }
    code[len] = '\0';
    
    // Evaluate with error handling
    bool had_error = false;
    SCM result = safe_eval_string(code, &had_error);
    
    if (had_error) {
        char *error_str = scm_to_locale_string(result);
        message("%s", error_str);
        free(error_str);
    } else {
        // ALWAYS display result
        char *result_str = scm_to_display_string(result);
        message("=> %s", result_str);
        free(result_str);
    }
    
    free(code);
}

void eval_buffer() {
    Buffer *buf = wm.selected->buffer;
    
    size_t len = rope_char_length(buf->rope);
    if (len == 0) {
        message("Empty buffer");
        return;
    }
    
    // Extract entire buffer
    char *code = malloc(len + 1);
    if (!code) {
        message("Memory allocation failed");
        return;
    }
    
    for (size_t i = 0; i < len; i++) {
        code[i] = (char)rope_char_at(buf->rope, i);
    }
    code[len] = '\0';
    
    // Evaluate with error handling
    bool had_error = false;
    SCM result = safe_eval_string(code, &had_error);
    
    if (had_error) {
        char *error_str = scm_to_locale_string(result);
        message("%s", error_str);
        free(error_str);
    } else {
        // ALWAYS display result
        char *result_str = scm_to_display_string(result);
        message("=> %s", result_str);
        free(result_str);
    }
    
    free(code);
}


// Helper to get procedure name as string
const char* scm_proc_name(SCM proc) {
    if (scm_is_false(proc) || !scm_is_true(scm_procedure_p(proc))) {
        return NULL;
    }
    
    SCM name = scm_procedure_name(proc);
    if (scm_is_false(name)) {
        return NULL;
    }
    
    return scm_to_locale_string(scm_symbol_to_string(name));
}

// Check if procedure matches a name
bool is_scm_proc(SCM proc, const char *name) {
    const char *proc_name = scm_proc_name(proc);
    if (!proc_name) return false;
    return strcmp(proc_name, name) == 0;
}



// =============================================================================
// SCHEME BINDINGS - Wrapper functions to expose C functions to Guile
// =============================================================================

// Movement commands
static SCM scm_forward_char(void) {
    forward_char();
    return SCM_UNSPECIFIED;
}

static SCM scm_backward_char(void) {
    backward_char();
    return SCM_UNSPECIFIED;
}

static SCM scm_next_line(void) {
    next_line();
    return SCM_UNSPECIFIED;
}

static SCM scm_previous_line(void) {
    previous_line();
    return SCM_UNSPECIFIED;
}

static SCM scm_forward_word(void) {
    forward_word();
    return SCM_UNSPECIFIED;
}

static SCM scm_backward_word(void) {
    backward_word();
    return SCM_UNSPECIFIED;
}

static SCM scm_forward_paragraph(void) {
    forward_paragraph();
    return SCM_UNSPECIFIED;
}

static SCM scm_backward_paragraph(void) {
    backward_paragraph();
    return SCM_UNSPECIFIED;
}

static SCM scm_beginning_of_line(void) {
    beginning_of_line();
    return SCM_UNSPECIFIED;
}

static SCM scm_end_of_line(void) {
    end_of_line();
    return SCM_UNSPECIFIED;
}

static SCM scm_beginning_of_buffer(void) {
    beginning_of_buffer();
    return SCM_UNSPECIFIED;
}

static SCM scm_end_of_buffer(void) {
    end_of_buffer();
    return SCM_UNSPECIFIED;
}

// Editing commands

static SCM scm_insert(SCM codepoint) {
    insert(scm_to_uint32(codepoint));
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_backward_char(void) {
    delete_backward_char();
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_char(void) {
    delete_char();
    return SCM_UNSPECIFIED;
}

static SCM my_scm_newline(void) {
    newline();
    return SCM_UNSPECIFIED;
}

static SCM scm_open_line(void) {
    open_line();
    return SCM_UNSPECIFIED;
}

static SCM scm_split_line(void) {
    split_line();
    return SCM_UNSPECIFIED;
}

// Kill/yank commands
static SCM scm_kill_line(void) {
    kill_line();
    return SCM_UNSPECIFIED;
}

static SCM scm_kill_word(void) {
    kill_word();
    return SCM_UNSPECIFIED;
}

static SCM scm_backward_kill_word(void) {
    backward_kill_word();
    return SCM_UNSPECIFIED;
}

static SCM scm_kill_region(void) {
    kill_region();
    return SCM_UNSPECIFIED;
}

static SCM scm_yank(void) {
    yank();
    return SCM_UNSPECIFIED;
}

// Region commands
static SCM scm_set_mark_command(void) {
    set_mark_command();
    return SCM_UNSPECIFIED;
}

static SCM scm_exchange_point_and_mark(void) {
    exchange_point_and_mark();
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_region(void) {
    delete_region();
    return SCM_UNSPECIFIED;
}

// Window management
static SCM scm_split_window_below(void) {
    split_window_below();
    return SCM_UNSPECIFIED;
}

static SCM scm_split_window_right(void) {
    split_window_right();
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_window(void) {
    delete_window();
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_other_windows(void) {
    delete_other_windows();
    return SCM_UNSPECIFIED;
}

static SCM scm_other_window(void) {
    other_window();
    return SCM_UNSPECIFIED;
}

static SCM scm_balance_windows(void) {
    balance_windows();
    return SCM_UNSPECIFIED;
}

static SCM scm_enlarge_window(void) {
    enlarge_window();
    return SCM_UNSPECIFIED;
}

static SCM scm_recenter(void) {
    recenter();
    return SCM_UNSPECIFIED;
}

static SCM scm_recenter_top_bottom(void) {
    recenter_top_bottom();
    return SCM_UNSPECIFIED;
}

// Query functions
static SCM scm_point(void) {
    Buffer *buf = wm.selected->buffer;
    return scm_from_size_t(buf->pt);
}

static SCM scm_mark(void) {
    Buffer *buf = wm.selected->buffer;
    return scm_from_size_t(buf->region.mark);
}

static SCM scm_mark_active_p(void) {
    Buffer *buf = wm.selected->buffer;
    return scm_from_bool(buf->region.active);
}

static SCM scm_buffer_size(void) {
    Buffer *buf = wm.selected->buffer;
    return scm_from_size_t(rope_char_length(buf->rope));
}

static SCM scm_current_column(void) {
    return scm_from_size_t(current_column());
}

static SCM scm_line_beginning_position(void) {
    return scm_from_size_t(line_beginning_position());
}

static SCM scm_line_end_position(void) {
    return scm_from_size_t(line_end_position());
}


// arg

static SCM scm_universal_argument(void) {
    universal_argument();
    return SCM_UNSPECIFIED;
}

static SCM scm_negative_argument(void) {
    negative_argument();
    return SCM_UNSPECIFIED;
}

static SCM scm_digit_argument(void) {
    digit_argument();
    return SCM_UNSPECIFIED;
}

static SCM scm_execute_extended_command(void) {
    execute_extended_command();
    return SCM_UNSPECIFIED;
}

static SCM scm_keyboard_quit(void) {
    keyboard_quit();
    return SCM_UNSPECIFIED;
}

// Eval

static SCM scm_eval_last_sexp(void) {
    eval_last_sexp();
    return SCM_UNSPECIFIED;
}

static SCM scm_eval_buffer(void) {
    eval_buffer();
    return SCM_UNSPECIFIED;
}

static SCM scm_eval_region(void) {
    eval_region();
    return SCM_UNSPECIFIED;
}


// Buffer content access
static SCM scm_buffer_substring(SCM start, SCM end) {
    if (!scm_is_integer(start) || !scm_is_integer(end)) {
        scm_wrong_type_arg("buffer-substring", 0, SCM_BOOL_F);
    }
    
    size_t start_pos = scm_to_size_t(start);
    size_t end_pos = scm_to_size_t(end);
    Buffer *buf = wm.selected->buffer;
    
    if (start_pos > end_pos || end_pos > rope_char_length(buf->rope)) {
        scm_out_of_range("buffer-substring", end);
    }
    
    size_t len = end_pos - start_pos;
    char *str = malloc(len + 1);
    
    for (size_t i = 0; i < len; i++) {
        str[i] = (char)rope_char_at(buf->rope, start_pos + i);
    }
    str[len] = '\0';
    
    SCM result = scm_from_locale_string(str);
    free(str);
    return result;
}

static SCM scm_char_after(SCM pos) {
    Buffer *buf = wm.selected->buffer;
    size_t p;
    
    if (scm_is_integer(pos)) {
        p = scm_to_size_t(pos);
    } else {
        p = buf->pt;
    }
    
    if (p >= rope_char_length(buf->rope)) {
        return SCM_BOOL_F;
    }
    
    return scm_from_uint32(rope_char_at(buf->rope, p));
}

static SCM scm_char_before(SCM pos) {
    Buffer *buf = wm.selected->buffer;
    size_t p;
    
    if (scm_is_integer(pos)) {
        p = scm_to_size_t(pos);
    } else {
        p = buf->pt;
    }
    
    if (p == 0) {
        return SCM_BOOL_F;
    }
    
    return scm_from_uint32(rope_char_at(buf->rope, p - 1));
}

static SCM scm_message(SCM fmt, SCM rest) {
    if (SCM_UNBNDP(fmt)) {
        return SCM_UNSPECIFIED;
    }
    
    if (!scm_is_string(fmt)) {
        fmt = scm_object_to_string(fmt, SCM_UNDEFINED);
    }
    
    char *format_str = scm_to_locale_string(fmt);
    
    if (SCM_UNBNDP(rest) || scm_is_null(rest)) {
        message("%s", format_str);
        SCM result = scm_from_locale_string(format_str);
        free(format_str);
        return result;
    }
    
    // Use full format from (ice-9 format)
    SCM format_proc = scm_c_public_ref("ice-9 format", "format");
    SCM format_args = scm_cons(SCM_BOOL_F, scm_cons(fmt, rest));
    SCM formatted_scm = scm_apply_0(format_proc, format_args);
    
    char *formatted = scm_to_locale_string(formatted_scm);
    message("%s", formatted);
    
    SCM result = scm_from_locale_string(formatted);
    free(format_str);
    free(formatted);
    return result;
}

static SCM scm_load_theme(SCM name) {
    if (!scm_is_string(name)) {
        scm_wrong_type_arg("load-theme", 1, name);
    }

    loadThemeByName(scm_to_locale_string(name));
    return SCM_UNSPECIFIED;
}


bool scm_get_bool(const char *name, bool default_value) {
    SCM var = scm_c_lookup(name);
    if (scm_is_false(var)) {
        return default_value;
    }
    SCM val = scm_variable_ref(var);
    if (scm_is_bool(val)) {
        return scm_to_bool(val);
    }
    return default_value;
}

size_t scm_get_size_t(const char *name, size_t default_value) {
    SCM var = scm_c_lookup(name);
    if (scm_is_false(var)) {
        return default_value;
    }
    SCM val = scm_variable_ref(var);
    if (scm_is_integer(val)) {
        return scm_to_size_t(val);
    }
    return default_value;
}

float scm_get_float(const char *name, float default_value) {
    SCM var = scm_c_lookup(name);
    if (scm_is_false(var)) {
        return default_value;
    }
    SCM val = scm_variable_ref(var);
    if (scm_is_real(val)) {
        return (float)scm_to_double(val);
    }
    return default_value;
}


// Get documentation from a Scheme procedure
static char* get_scheme_proc_documentation(SCM proc) {
    // Try to get procedure documentation
    SCM doc = scm_procedure_documentation(proc);
    if (scm_is_string(doc)) {
        return scm_to_locale_string(doc);
    }
    return NULL;
}



static SCM scm_keychord_bind(SCM notation_scm, SCM action_scm) {
    if (!scm_is_string(notation_scm)) {
        scm_wrong_type_arg("keychord-bind", 1, notation_scm);
    }
    if (!scm_is_true(scm_procedure_p(action_scm))) {
        scm_wrong_type_arg("keychord-bind", 2, action_scm);
    }
    
    char *notation = scm_to_locale_string(notation_scm);
    char *description = get_scheme_proc_documentation(action_scm);
    
    bool result = keychord_bind_scheme(&keymap, notation, action_scm,
                                       description, PRESS | REPEAT);
    
    free(notation);
    if (description) {
        free(description);
    }
    
    return scm_from_bool(result);
}

static SCM scm_keychord_unbind(SCM notation_scm) {
    if (!scm_is_string(notation_scm)) {
        scm_wrong_type_arg("keychord-unbind", 1, notation_scm);
    }
    
    char *notation = scm_to_locale_string(notation_scm);
    bool result = keychord_unbind(&keymap, notation);
    
    free(notation);
    
    return scm_from_bool(result);
}

static SCM scm_keychord_documentation(SCM notation_scm) {
    if (!scm_is_string(notation_scm)) {
        scm_wrong_type_arg("keychord-documentation", 1, notation_scm);
    }
    
    char *notation = scm_to_locale_string(notation_scm);
    KeyChordBinding *binding = keychord_find_binding(&keymap, notation);
    
    SCM result;
    if (binding && binding->description) {
        result = scm_from_locale_string(binding->description);
    } else {
        result = scm_from_locale_string("");  // Empty string instead of #f
    }
    
    free(notation);
    return result;
}


// Get all keychord bindings as an association list
static SCM scm_keychord_bindings(void) {
    SCM alist = SCM_EOL;
    
    for (size_t i = 0; i < keymap.count; i++) {
        KeyChordBinding *binding = &keymap.bindings[i];
        
        SCM key = scm_from_locale_string(binding->notation);
        SCM doc = binding->description 
            ? scm_from_locale_string(binding->description)
            : SCM_BOOL_F;
        
        // Create (notation . description) pair
        SCM pair = scm_cons(key, doc);
        alist = scm_cons(pair, alist);
    }
    
    return alist;
}

/// BUFFER


static SCM buffer_type;

// Create a buffer foreign object from a Buffer pointer
static SCM make_buffer_object(Buffer *buf) {
    return scm_make_foreign_object_1(buffer_type, buf);
}

// Extract Buffer* from foreign object
static Buffer* scm_to_buffer(SCM obj) {
    scm_assert_foreign_object_type(buffer_type, obj);
    return scm_foreign_object_ref(obj, 0);
}

// Type predicate
static SCM buffer_p(SCM obj) {
    return scm_from_bool(SCM_IS_A_P(obj, buffer_type));
}

// Printer for buffer objects
static int print_buffer(SCM buffer_obj, SCM port, scm_print_state *pstate SCM_UNUSED) {
    Buffer *buf = scm_to_buffer(buffer_obj);
    scm_puts("#<buffer ", port);
    scm_puts(buf->name, port);
    scm_puts(">", port);
    return 1;
}

// =============================================================================
// UPDATED BUFFER FUNCTIONS
// =============================================================================

// switch-to-buffer now accepts either string or buffer object
static SCM scm_switch_to_buffer(SCM buf_or_name) {
    Buffer *buf = NULL;
    
    if (SCM_IS_A_P(buf_or_name, buffer_type)) {
        // It's a buffer foreign object
        buf = scm_to_buffer(buf_or_name);
    } else if (scm_is_string(buf_or_name)) {
        // It's a string name
        char *buffer_name = scm_to_locale_string(buf_or_name);
        buf = get_buffer(buffer_name);
        
        if (!buf) {
            // Create new buffer if doesn't exist
            buf = buffer_create(current_buffer->font, buffer_name);
        }
        free(buffer_name);
    } else {
        scm_wrong_type_arg("switch-to-buffer", 1, buf_or_name);
    }
    
    if (buf) {
        switch_to_buffer(buf);
        return SCM_BOOL_T;
    }
    
    return SCM_BOOL_F;
}

// kill-buffer: fixed to not kill minibuf or leave only minibuf
static SCM scm_kill_buffer(SCM name) {
    Buffer *buf;
    
    if (SCM_UNBNDP(name)) {
        buf = current_buffer;
    } else if (SCM_IS_A_P(name, buffer_type)) {
        buf = scm_to_buffer(name);
    } else if (scm_is_string(name)) {
        char *buffer_name = scm_to_locale_string(name);
        buf = get_buffer(buffer_name);
        free(buffer_name);
        
        if (!buf) {
            return SCM_BOOL_F;
        }
    } else {
        scm_wrong_type_arg("kill-buffer", 1, name);
        return SCM_BOOL_F;
    }
    
    // Don't kill minibuffer
    if (buf == wm.minibuffer_window->buffer) {
        message("Cannot kill minibuffer");
        return SCM_BOOL_F;
    }
    
    // Count non-minibuffer buffers
    int non_minibuf_count = 0;
    Buffer *temp = all_buffers;
    do {
        if (temp != wm.minibuffer_window->buffer) {
            non_minibuf_count++;
        }
        temp = temp->next;
    } while (temp != all_buffers);
    
    // Don't kill if it would leave only minibuffer
    if (non_minibuf_count <= 1) {
        message("Cannot kill last buffer");
        return SCM_BOOL_F;
    }
    
    kill_buffer(buf);
    return SCM_BOOL_T;
}

// other-buffer: returns buffer object, doesn't switch
static SCM scm_other_buffer(void) {
    Buffer *next = other_buffer();
    if (next) {
        return make_buffer_object(next);
    }
    return SCM_EOL;
}

// get-buffer: returns buffer object instead of boolean
static SCM scm_get_buffer(SCM name) {
    if (!scm_is_string(name)) {
        scm_wrong_type_arg("get-buffer", 1, name);
    }
    
    char *buffer_name = scm_to_locale_string(name);
    Buffer *buf = get_buffer(buffer_name);
    free(buffer_name);
    
    return buf ? make_buffer_object(buf) : SCM_EOL;
}

// current-buffer: returns current buffer as object
static SCM scm_current_buffer(void) {
    if (!current_buffer) return SCM_EOL;
    return make_buffer_object(current_buffer);
}

// buffer-name: can take optional buffer argument
static SCM scm_buffer_name(SCM buf_arg) {
    Buffer *buf;
    
    if (SCM_UNBNDP(buf_arg)) {
        buf = current_buffer;
    } else if (SCM_IS_A_P(buf_arg, buffer_type)) {
        buf = scm_to_buffer(buf_arg);
    } else {
        scm_wrong_type_arg("buffer-name", 1, buf_arg);
    }
    
    if (!buf) return SCM_EOL;
    return scm_from_locale_string(buf->name);
}

// buffer-list: returns list of buffer objects
static SCM scm_buffer_list(void) {
    if (!all_buffers) return SCM_EOL;
    
    SCM list = SCM_EOL;
    Buffer *buf = all_buffers;
    
    do {
        SCM buf_obj = make_buffer_object(buf);
        list = scm_cons(buf_obj, list);
        buf = buf->next;
    } while (buf != all_buffers);
    
    return scm_reverse(list);
}


static SCM scm_next_buffer(void) {
    next_buffer();
    return SCM_UNSPECIFIED;
}

static SCM scm_previous_buffer(void) {
    previous_buffer();
    return SCM_UNSPECIFIED;
}




#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>

// Helper to check if file exists
static bool file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

// Get home directory
static const char* get_home_directory(void) {
    const char *home = getenv("HOME");
    if (home) return home;
    
    struct passwd *pw = getpwuid(getuid());
    if (pw) return pw->pw_dir;
    
    return NULL;
}

// Find and set user-init-file
static void setup_user_init_file(void) {
    char path[1024];
    
    // Try ./init.scm first (current directory)
    snprintf(path, sizeof(path), "./init.scm");
    if (file_exists(path)) {
        // Get absolute path
        char abs_path[1024];
        if (realpath(path, abs_path)) {
            scm_c_define("user-init-file", scm_from_locale_string(abs_path));
            return;
        }
    }
    
    const char *home = get_home_directory();
    if (!home) {
        scm_c_define("user-init-file", SCM_BOOL_F);
        return;
    }
    
    // Try ~/.glemax
    snprintf(path, sizeof(path), "%s/.glemax", home);
    if (file_exists(path)) {
        scm_c_define("user-init-file", scm_from_locale_string(path));
        return;
    }
    
    // Try ~/.glemax.d/init.scm
    snprintf(path, sizeof(path), "%s/.glemax.d/init.scm", home);
    if (file_exists(path)) {
        scm_c_define("user-init-file", scm_from_locale_string(path));
        return;
    }
    
    // Try ~/.config/glemax/init.scm
    snprintf(path, sizeof(path), "%s/.config/glemax/init.scm", home);
    if (file_exists(path)) {
        scm_c_define("user-init-file", scm_from_locale_string(path));
        return;
    }
    
    // No init file found
    scm_c_define("user-init-file", SCM_BOOL_F);
}

static SCM scm_load(SCM filename) {
    if (!scm_is_string(filename)) {
        scm_wrong_type_arg("load", 1, filename);
    }
    
    char *path = scm_to_locale_string(filename);
    
    if (!file_exists(path)) {
        char msg[1024];
        snprintf(msg, sizeof(msg), "Cannot open load file: %s", path);
        free(path);
        scm_misc_error("load", msg, SCM_EOL);
    }
    
    // Use scm_c_primitive_load
    SCM result = scm_c_primitive_load(path);
    free(path);
    
    return result;
}



void lisp_init(void) {

    // Initialize buffer foreign object type
    SCM name = scm_from_utf8_symbol("buffer");
    SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
    buffer_type = scm_make_foreign_object_type(name, slots, NULL);

    setup_user_init_file();


    scm_c_define_gsubr("load",                     1, 0, 0, scm_load);


    // Eval
    scm_c_define_gsubr("eval-last-sexp",           0, 0, 0, scm_eval_last_sexp);
    scm_c_define_gsubr("eval-buffer",              0, 0, 0, scm_eval_buffer);
    scm_c_define_gsubr("eval-region",              0, 0, 0, scm_eval_region);

    // Arg
    scm_c_define_gsubr("universal-argument",       0, 0, 0, scm_universal_argument);
    scm_c_define_gsubr("negative-argument",        0, 0, 0, scm_negative_argument);
    scm_c_define_gsubr("digit-argument",           0, 0, 0, scm_digit_argument);
    scm_c_define_gsubr("execute-extended-command", 0, 0, 0, scm_execute_extended_command);
    scm_c_define_gsubr("keyboard-quit",            0, 0, 0, scm_keyboard_quit);

    // Buffer
    scm_c_define_gsubr("buffer?",                  1, 0, 0, buffer_p);
    scm_c_define_gsubr("switch-to-buffer",         1, 0, 0, scm_switch_to_buffer);
    scm_c_define_gsubr("kill-buffer",              0, 1, 0, scm_kill_buffer);
    scm_c_define_gsubr("buffer-name",              0, 1, 0, scm_buffer_name);
    scm_c_define_gsubr("buffer-list",              0, 0, 0, scm_buffer_list);
    scm_c_define_gsubr("other-buffer",             0, 0, 0, scm_other_buffer);
    scm_c_define_gsubr("get-buffer",               1, 0, 0, scm_get_buffer);
    scm_c_define_gsubr("current-buffer",           0, 0, 0, scm_current_buffer);
    scm_c_define_gsubr("next-buffer",              0, 0, 0, scm_next_buffer);
    scm_c_define_gsubr("previous-buffer",          0, 0, 0, scm_previous_buffer);        

    // Keychord
    scm_c_define_gsubr("keychord-bind",            2, 0, 0, scm_keychord_bind);
    scm_c_define_gsubr("keychord-unbind",          1, 0, 0, scm_keychord_unbind);
    scm_c_define_gsubr("keychord-documentation",   1, 0, 0, scm_keychord_documentation);
    scm_c_define_gsubr("keychord-bindings",        0, 0, 0, scm_keychord_bindings);    

    // Movement
    scm_c_define_gsubr("forward-char",             0, 0, 0, scm_forward_char);
    scm_c_define_gsubr("backward-char",            0, 0, 0, scm_backward_char);
    scm_c_define_gsubr("next-line",                0, 0, 0, scm_next_line);
    scm_c_define_gsubr("previous-line",            0, 0, 0, scm_previous_line);
    scm_c_define_gsubr("forward-word",             0, 0, 0, scm_forward_word);
    scm_c_define_gsubr("backward-word",            0, 0, 0, scm_backward_word);
    scm_c_define_gsubr("forward-paragraph",        0, 0, 0, scm_forward_paragraph);
    scm_c_define_gsubr("backward-paragraph",       0, 0, 0, scm_backward_paragraph);
    scm_c_define_gsubr("beginning-of-line",        0, 0, 0, scm_beginning_of_line);
    scm_c_define_gsubr("end-of-line",              0, 0, 0, scm_end_of_line);
    scm_c_define_gsubr("beginning-of-buffer",      0, 0, 0, scm_beginning_of_buffer);
    scm_c_define_gsubr("end-of-buffer",            0, 0, 0, scm_end_of_buffer);
    
    // Editing
    scm_c_define_gsubr("insert",                   1, 0, 0, scm_insert);
    scm_c_define_gsubr("delete-backward-char",     0, 0, 0, scm_delete_backward_char);
    scm_c_define_gsubr("delete-char",              0, 0, 0, scm_delete_char);
    scm_c_define_gsubr("newline",                  0, 0, 0, my_scm_newline);
    scm_c_define_gsubr("open-line",                0, 0, 0, scm_open_line);
    scm_c_define_gsubr("split-line",               0, 0, 0, scm_split_line);
    
    // Kill/yank
    scm_c_define_gsubr("kill-line",                0, 0, 0, scm_kill_line);
    scm_c_define_gsubr("kill-word",                0, 0, 0, scm_kill_word);
    scm_c_define_gsubr("backward-kill-word",       0, 0, 0, scm_backward_kill_word);
    scm_c_define_gsubr("kill-region",              0, 0, 0, scm_kill_region);
    scm_c_define_gsubr("yank",                     0, 0, 0, scm_yank);
    
    // Region
    scm_c_define_gsubr("set-mark-command",         0, 0, 0, scm_set_mark_command);
    scm_c_define_gsubr("exchange-point-and-mark",  0, 0, 0, scm_exchange_point_and_mark);
    scm_c_define_gsubr("delete-region",            0, 0, 0, scm_delete_region);
    
    // Windows
    scm_c_define_gsubr("split-window-below",       0, 0, 0, scm_split_window_below);
    scm_c_define_gsubr("split-window-right",       0, 0, 0, scm_split_window_right);
    scm_c_define_gsubr("delete-window",            0, 0, 0, scm_delete_window);
    scm_c_define_gsubr("delete-other-windows",     0, 0, 0, scm_delete_other_windows);
    scm_c_define_gsubr("other-window",             0, 0, 0, scm_other_window);
    scm_c_define_gsubr("balance-windows",          0, 0, 0, scm_balance_windows);
    scm_c_define_gsubr("enlarge-window",           0, 0, 0, scm_enlarge_window);
    scm_c_define_gsubr("recenter",                 0, 0, 0, scm_recenter);
    scm_c_define_gsubr("recenter-top-bottom",      0, 0, 0, scm_recenter_top_bottom);
    
    // Query functions
    scm_c_define_gsubr("point",                    0, 0, 0, scm_point);
    scm_c_define_gsubr("mark",                     0, 0, 0, scm_mark);
    scm_c_define_gsubr("mark-active?",             0, 0, 0, scm_mark_active_p);
    scm_c_define_gsubr("buffer-size",              0, 0, 0, scm_buffer_size);
    scm_c_define_gsubr("current-column",           0, 0, 0, scm_current_column);
    scm_c_define_gsubr("line-beginning-position",  0, 0, 0, scm_line_beginning_position);
    scm_c_define_gsubr("line-end-position",        0, 0, 0, scm_line_end_position);
    
    // Buffer content
    scm_c_define_gsubr("buffer-substring",         2, 0, 0, scm_buffer_substring);
    scm_c_define_gsubr("char-after",               0, 1, 0, scm_char_after);
    scm_c_define_gsubr("char-before",              0, 1, 0, scm_char_before);
    
    // Message
    scm_c_define_gsubr("message",                  1, 0, 1, scm_message);    
    scm_c_define_gsubr("load-theme",               1, 0, 0, scm_load_theme);

    // NOTE Eval init.scm after defining subroutine
    /* scm_c_eval_string( */
    /*     "(when (file-exists? \"init.scm\") " */
    /*     "  (load \"init.scm\"))" */
    /* ); */

    scm_c_eval_string(
        "(when (and user-init-file (string? user-init-file))"
        "  (catch #t"
        "    (lambda () (load user-init-file))"
        "    (lambda (key . args)"
        "      (message \"Error loading init file: ~a\" user-init-file))))"
    );


}

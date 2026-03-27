#include <libguile.h>
#include <obsidian/input.h>
#include <obsidian/theme.h>
#include <obsidian/window.h>
#include <stdbool.h>
#include "lisp.h"
#include "faces.h"
#include "libguile/scm.h"
#include "libguile/strings.h"
#include "rope.h"
#include "buffer.h"
#include "edit.h"
#include "textprop.h"
#include "theme.h"
#include "treesit.h"
#include "wm.h"
#include "minibuf.h"
#include "frame.h"
#include "fileio.h"
#include "glemax.h"

// Error handler that captures error message with full details
SCM error_handler(void *data, SCM key, SCM args) {
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
SCM eval_string_body(void *data) {
    const char *str = (const char *)data;
    return scm_c_eval_string(str);
}

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

    if (scm_is_string(obj)) {
        // For strings, add quotes manually but display the content
        scm_display(scm_from_locale_string("\""), str_port);
        scm_display(obj, str_port);
        scm_display(scm_from_locale_string("\""), str_port);
    } else {
        // For other types, use write to get proper representation
        scm_write(obj, str_port);
    }

    SCM str_scm = scm_get_output_string(str_port);
    scm_close_port(str_port);

    char *result = scm_to_locale_string(str_scm);
    return result;
}


// Centralized function to display evaluation results
// Respects eval-display-prompt and eval-prompt variables
static void display_eval_result(SCM result, bool had_error) {
    if (had_error) {
        char *error_str = scm_to_locale_string(result);
        message("%s", error_str);
        free(error_str);
    } else {
        char *result_str = scm_to_display_string(result);

        bool display_prompt = scm_get_bool("eval-display-prompt", true);

        if (display_prompt) {
            char *prompt = scm_get_string("eval-prompt", "=> ");
            message("%s%s", prompt, result_str);
            free(prompt);
        } else {
            message("%s", result_str);
        }

        free(result_str);
    }
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
static char *eval_expression_print_format(SCM value) {
    if (!scm_is_integer(value)) return NULL;

    long n = scm_to_long(value);

    // Get eval-expression-print-maximum-character from Scheme
    long max_char = 127; // default fallback
    SCM max_char_var = scm_c_lookup("eval-expression-print-maximum-character");
    if (scm_is_true(scm_variable_bound_p(max_char_var))) {
        SCM max_char_val = scm_variable_ref(max_char_var);
        if (scm_is_integer(max_char_val))
            max_char = scm_to_long(max_char_val);
    }

    if (n >= 0 && n <= max_char) {
        char char_str[16] = {0};

        if (n == 0) {
            snprintf(char_str, sizeof(char_str), "#\\nul");
        } else if (n < 32) {
            // Guile named control characters
            const char *ctrl_names[] = {
                "nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel",
                "bs",  "tab", "newline", "vt", "page", "return", "so", "si",
                "dle", "dc1", "dc2", "dc3", "dc4", "nak", "syn", "etb",
                "can", "em",  "sub", "escape", "fs", "gs", "rs", "us"
            };
            snprintf(char_str, sizeof(char_str), "#\\%s", ctrl_names[n]);
        } else if (n == 127) {
            snprintf(char_str, sizeof(char_str), "#\\delete");
        } else if (n >= 32 && n < 127) {
            snprintf(char_str, sizeof(char_str), "#\\%c", (char)n);
        }

        if (char_str[0]) {
            int len = snprintf(NULL, 0, " (#o%lo, #x%lx, %s)", n, n, char_str) + 1;
            char *buf = malloc(len);
            if (!buf) return NULL;
            snprintf(buf, len, " (#o%lo, #x%lx, %s)", n, n, char_str);
            return buf;
        }
    }

    int len = snprintf(NULL, 0, " (#o%lo, #x%lx)", n, n) + 1;
    char *buf = malloc(len);
    if (!buf) return NULL;
    snprintf(buf, len, " (#o%lo, #x%lx)", n, n);
    return buf;
}

void eval_last_sexp(void) {
    size_t point = current_buffer->pt;
    if (point == 0) {
        message("Beginning of buffer");
        return;
    }
    size_t start = find_sexp_start(current_buffer, point);
    if (start >= point) {
        message("No expression before point");
        return;
    }
    size_t len = point - start;
    char *expr = malloc(len + 1);
    if (!expr) {
        message("Memory allocation failed");
        return;
    }
    for (size_t i = 0; i < len; i++)
        expr[i] = (char)rope_char_at(current_buffer->rope, start + i);
    expr[len] = '\0';
    bool had_error = false;
    SCM result = safe_eval_string(expr, &had_error);
    free(expr);
    if (had_error) {
        display_eval_result(result, had_error);
        return;
    }
    int prefix = get_prefix_arg();

    bool raw_prefix = false;
    SCM raw_prefix_var = scm_c_lookup("raw-prefix-arg");
    if (scm_is_true(scm_variable_bound_p(raw_prefix_var)))
        raw_prefix = scm_is_true(scm_variable_ref(raw_prefix_var));

    bool insert_into_buffer = (argument_manually_set && prefix != 0) || raw_prefix;
    bool insert_full        = (prefix < 0) || raw_prefix;

    // Print the raw value (no prompt — used for both insert and message)
    SCM port = scm_open_output_string();
    scm_write(result, port);
    char *str = scm_to_locale_string(scm_get_output_string(port));

    if (insert_into_buffer) {
        set_prefix_arg(1);
        if (insert_full) {
            char *full_suffix = eval_expression_print_format(result);
            if (full_suffix) {
                int total_len = strlen(str) + strlen(full_suffix) + 1;
                char *combined = malloc(total_len);
                if (combined) {
                    snprintf(combined, total_len, "%s%s", str, full_suffix);
                    insert(combined);
                    free(combined);
                }
                free(full_suffix);
            } else {
                insert(str);
            }
        } else {
            insert(str);
        }
    } else {
        // Show in message, respecting eval-display-prompt and eval-prompt
        char *suffix = eval_expression_print_format(result);
        char *value_with_suffix = NULL;
        if (suffix) {
            int total_len = strlen(str) + strlen(suffix) + 1;
            value_with_suffix = malloc(total_len);
            if (value_with_suffix)
                snprintf(value_with_suffix, total_len, "%s%s", str, suffix);
            free(suffix);
        }
        const char *display_str = value_with_suffix ? value_with_suffix : str;
        bool display_prompt = scm_get_bool("eval-display-prompt", true);
        if (display_prompt) {
            char *prompt = scm_get_string("eval-prompt", "=> ");
            message("%s%s", prompt, display_str);
            free(prompt);
        } else {
            message("%s", display_str);
        }
        if (value_with_suffix) free(value_with_suffix);
    }
    free(str);
}

void eval_region() {
    size_t start, end;
    region_bounds(&start, &end);

    if (start >= end) {
        message("Empty region");
        return;
    }

    size_t len = end - start;
    char *code = malloc(len + 1);
    if (!code) {
        message("Memory allocation failed");
        return;
    }

    for (size_t i = 0; i < len; i++) {
        code[i] = (char)rope_char_at(current_buffer->rope, start + i);
    }
    code[len] = '\0';

    bool had_error = false;
    SCM result = safe_eval_string(code, &had_error);
    display_eval_result(result, had_error);

    free(code);
}

void eval_buffer() {
    size_t len = rope_char_length(current_buffer->rope);
    if (len == 0) {
        message("Empty buffer");
        return;
    }

    char *code = malloc(len + 1);
    if (!code) {
        message("Memory allocation failed");
        return;
    }

    for (size_t i = 0; i < len; i++) {
        code[i] = (char)rope_char_at(current_buffer->rope, i);
    }
    code[len] = '\0';

    bool had_error = false;
    SCM result = safe_eval_string(code, &had_error);
    display_eval_result(result, had_error);

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



inline int clip_to_bounds (int lower, int num, int upper) {
    return max (lower, min (num, upper));
}



/// SCHEME BINDINGS



// NOT NEEDED

static SCM scm_char_or_string_p(SCM object) {
    if (scm_is_string(object)) {
        return SCM_BOOL_T;
    }

    if (scm_is_integer(object)) {
        // Check if it's a valid character codepoint (0 to 0x10FFFF)
        // but exclude surrogate pairs (0xD800 to 0xDFFF)
        if (scm_is_unsigned_integer(object, 0, 0x10FFFF)) {
            uint32_t cp = scm_to_uint32(object);
            if (cp >= 0xD800 && cp <= 0xDFFF) {
                return SCM_BOOL_F;  // Surrogate pairs are not valid characters
            }
            return SCM_BOOL_T;
        }
    }

    return SCM_BOOL_F;
}

static SCM scm_insert(SCM rest) {
    // Iterate through all arguments
    while (!scm_is_null(rest)) {
        SCM arg = scm_car(rest);

        if (scm_is_integer(arg)) {
            // Single character (codepoint) - convert to string
            uint32_t codepoint = scm_to_uint32(arg);

            // Validate codepoint range
            if (codepoint >= 0x110000 || (codepoint >= 0xD800 && codepoint <= 0xDFFF)) {
                scm_wrong_type_arg("insert", 0, arg);
            }

            char utf8[5] = {0};
            size_t len = utf8_encode(codepoint, utf8);

            if (len > 0) {
                utf8[len] = '\0';
                insert(utf8);
            }
        } else if (scm_is_string(arg)) {
            char *text = scm_to_utf8_string(arg);
            insert(text);
            free(text);
        } else {
            scm_wrong_type_arg("insert", 0, arg);
        }

        rest = scm_cdr(rest);
    }

    return SCM_UNSPECIFIED;
}

static SCM scm_quoted_insert(void) {
    quoted_insert();
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_blank_lines(void) {
    delete_blank_lines();
    return SCM_UNSPECIFIED;
}

static SCM scm_back_to_indentation(void) {
    back_to_indentation();
    return SCM_UNSPECIFIED;
}

static SCM scm_set_mark(SCM pos) {
    size_t position = scm_to_size_t(pos);
    set_mark(position);
    return SCM_UNSPECIFIED;
}

// TODO Whit positive ARG activate transient-mark-mode
static SCM scm_exchange_point_and_mark(void) {
    exchange_point_and_mark();
    return SCM_UNSPECIFIED;
}

static SCM scm_delete_region(void) {
    delete_region();
    return SCM_UNSPECIFIED;
}

static SCM scm_activate_mark(void) {
    activate_mark();
    return SCM_UNSPECIFIED;
}

static SCM scm_deactivate_mark(void) {
    deactivate_mark();
    return SCM_UNSPECIFIED;
}

/// Arg

static SCM sym_interactive_spec = SCM_BOOL_F;


void init_interactive_system(void) {
    // Just a plain Scheme variable, not a parameter object
    scm_c_define("current-interactive-proc", SCM_BOOL_F);

    sym_interactive_spec = scm_from_utf8_symbol("interactive-spec");
    scm_gc_protect_object(sym_interactive_spec);
}

static SCM read_interactive_args(const char *spec) {
    if (!spec || *spec == '\0') return SCM_EOL;

    SCM args = SCM_EOL;
    const char *p = spec;

    while (*p) {
        switch (*p) {

        case 'p': {
            args = scm_append(scm_list_2(args,
                    scm_list_1(scm_from_int(get_prefix_arg()))));
            p++;
            break;
        }

        case 'P': {
            SCM var = scm_c_lookup("prefix-arg");
            SCM val = scm_is_false(var) ? SCM_BOOL_F : scm_variable_ref(var);
            SCM raw = scm_is_integer(val) ? val : SCM_BOOL_F;
            args = scm_append(scm_list_2(args, scm_list_1(raw)));
            p++;
            break;
        }

        case 'r': {
            if (current_buffer->region.mark < 0) {
                message("The mark is not set now, so there is no region");
                return SCM_BOOL_F;
            }
            size_t pt    = current_buffer->pt;
            size_t mark  = (size_t)current_buffer->region.mark;
            size_t start = pt < mark ? pt : mark;
            size_t end   = pt < mark ? mark : pt;
            args = scm_append(scm_list_2(args,
                                         scm_list_2(scm_from_size_t(start),
                                         scm_from_size_t(end))));
            p++;
            break;
        }

        /* case 'r': { */
        /*     if (!current_buffer->region.active || current_buffer->region.mark < 0) { */
        /*         message("The mark is not set now, so there is no region"); */
        /*         return SCM_BOOL_F; */
        /*     } */
        /*     size_t pt    = current_buffer->pt; */
        /*     size_t mark  = (size_t)current_buffer->region.mark; */
        /*     size_t start = pt < mark ? pt : mark; */
        /*     size_t end   = pt < mark ? mark : pt; */
        /*     args = scm_append(scm_list_2(args, */
        /*             scm_list_2(scm_from_size_t(start), */
        /*                        scm_from_size_t(end)))); */
        /*     p++; */
        /*     break; */
        /* } */

        case 's': {
            p++;
            char prompt[256] = "";
            size_t i = 0;
            while (*p && *p != '\n' && i < sizeof(prompt) - 1)
                prompt[i++] = *p++;
            prompt[i] = '\0';
            if (*p == '\n') p++;

            SCM hist = scm_from_locale_symbol("minibuffer-history");
            char *input = read_from_minibuffer(prompt, NULL, hist);
            if (!input) return SCM_BOOL_F;
            args = scm_append(scm_list_2(args,
                    scm_list_1(scm_from_locale_string(input))));
            free(input);
            break;
        }

        case 'n': {
            p++;
            char prompt[256] = "Number: ";
            size_t i = 0;
            char tmp[256] = "";
            while (*p && *p != '\n' && i < sizeof(tmp) - 1)
                tmp[i++] = *p++;
            tmp[i] = '\0';
            if (*p == '\n') p++;
            if (i > 0) strncpy(prompt, tmp, sizeof(prompt) - 1);

            SCM hist = scm_from_locale_symbol("minibuffer-history");
            char *input = read_from_minibuffer(prompt, NULL, hist);
            if (!input) return SCM_BOOL_F;
            int n = atoi(input);
            free(input);
            args = scm_append(scm_list_2(args, scm_list_1(scm_from_int(n))));
            break;
        }

        case 'B':
        case 'b': {
            // 'b' — default is current buffer
            // 'B' — default is other-buffer (like switch-to-buffer)
            bool is_B = (*p == 'B');
            p++;
            char base_prompt[256] = "";
            size_t i = 0;
            char tmp[256] = "";
            while (*p && *p != '\n' && i < sizeof(tmp) - 1)
                tmp[i++] = *p++;
            tmp[i] = '\0';
            if (*p == '\n') p++;
            if (i > 0)
                strncpy(base_prompt, tmp, sizeof(base_prompt) - 1);
            else
                strncpy(base_prompt, is_B ? "Switch to buffer" : "Buffer",
                        sizeof(base_prompt) - 1);

            Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
            Buffer *def_buf = is_B ? other_buffer() : current_buffer;

            // Build completion list — 'B' excludes current buffer like Emacs
            SCM buffer_names = SCM_EOL;
            Buffer *buf = all_buffers;
            do {
                if (buf != minibuf && !(is_B && buf == current_buffer))
                    buffer_names = scm_cons(scm_from_locale_string(buf->name),
                                            buffer_names);
                buf = buf->next;
            } while (buf != all_buffers);

            // Build prompt with default, e.g. "Switch to buffer (default foo): "
            char full_prompt[512];
            snprintf(full_prompt, sizeof(full_prompt),
                     "%s (default %s): ", base_prompt,
                     def_buf ? def_buf->name : "none");

            SCM hist = scm_from_locale_symbol("buffer-name-history");
            char *input = read_from_minibuffer_with_completion(
                full_prompt, NULL, buffer_names, SCM_BOOL_F, hist);
            if (!input) return SCM_BOOL_F;

            // Empty input -> use default
            const char *chosen = (*input) ? input : (def_buf ? def_buf->name : "");
            args = scm_append(scm_list_2(args,
                    scm_list_1(scm_from_locale_string(chosen))));
            free(input);
            break;
        }

        case 'f': {
            p++;
            char prompt[256] = "File: ";
            size_t i = 0;
            char tmp[256] = "";
            while (*p && *p != '\n' && i < sizeof(tmp) - 1)
                tmp[i++] = *p++;
            tmp[i] = '\0';
            if (*p == '\n') p++;
            if (i > 0) strncpy(prompt, tmp, sizeof(prompt) - 1);

            SCM read_file_name_func = scm_variable_ref(
                scm_c_lookup("read-file-name"));
            SCM result = scm_call_1(read_file_name_func,
                                    scm_from_locale_string(prompt));
            if (scm_is_false(result)) return SCM_BOOL_F;
            args = scm_append(scm_list_2(args, scm_list_1(result)));
            break;
        }

        default:
            p++;
            break;
        }
    }

    return args;
}

typedef struct {
    SCM proc;
    SCM args;
} ApplyData;

static SCM apply_body(void *data) {
    ApplyData *d = (ApplyData *)data;
    return scm_apply_0(d->proc, d->args);
}

SCM call_interactively(SCM proc) {
    if (!scm_is_true(scm_procedure_p(proc)))
        return SCM_BOOL_F;

    scm_c_define("current-interactive-proc", proc);

    SCM spec_val = scm_procedure_property(proc, sym_interactive_spec);
    SCM result = SCM_UNSPECIFIED;

    if (scm_is_false(spec_val)) {
        // No interactive spec — call with no args
        result = scm_internal_catch(SCM_BOOL_T,
                     (scm_t_catch_body)scm_call_0, proc,
                     error_handler, NULL);
    } else if (scm_is_string(spec_val)) {
        char *spec = scm_to_locale_string(spec_val);
        SCM args = read_interactive_args(spec);
        free(spec);

        if (scm_is_false(args) && !scm_is_null(args)) {
            scm_c_define("current-interactive-proc", SCM_BOOL_F);
            return SCM_UNSPECIFIED;
        }

        ApplyData data = { proc, args };
        result = scm_internal_catch(SCM_BOOL_T,
                     apply_body, &data,
                     error_handler, NULL);
    } else {
        result = scm_internal_catch(SCM_BOOL_T,
                     (scm_t_catch_body)scm_call_0, proc,
                     error_handler, NULL);
    }

    scm_c_define("current-interactive-proc", SCM_BOOL_F);
    return result;
}

static SCM scm_call_interactively(SCM proc) {
    return call_interactively(proc);
}

static SCM scm_interactive(SCM spec) {
    (void)spec;
    return SCM_UNSPECIFIED;
}

static SCM scm_interactive_form(SCM proc) {
    if (!scm_is_true(scm_procedure_p(proc))) return SCM_BOOL_F;
    return scm_procedure_property(proc, sym_interactive_spec);
}

static SCM scm_commandp(SCM obj) {
    if (!scm_is_true(scm_procedure_p(obj))) return SCM_BOOL_F;
    SCM spec = scm_procedure_property(obj, sym_interactive_spec);
    return scm_is_false(spec) ? SCM_BOOL_F : SCM_BOOL_T;
}






// NOTE This is our way to do (interactive) for C functions
// passed to scheme, so later M-x will have access
// to all commands registered trough this macro
#define DEFINE_SCM_COMMAND(scm_name, c_func, doc_string)      \
    static SCM scm_name(SCM arg) {                            \
        if (!SCM_UNBNDP(arg)) {                               \
            if (!scm_is_integer(arg)) {                       \
                scm_wrong_type_arg(#scm_name, 1, arg);        \
            }                                                 \
            set_prefix_arg(scm_to_int(arg));                  \
        }                                                     \
        c_func();                                             \
        return SCM_UNSPECIFIED;                               \
    }                                                         \
    static const char* scm_name##_doc = doc_string;

#include "theme.h"


DEFINE_SCM_COMMAND(scm_self_insert_command, self_insert_command,
"Insert the character you type."
"Whichever character C you type to run this command is inserted."
"The numeric prefix argument N says how many times to repeat the insertion."
"Before insertion, `expand-abbrev' is executed if the inserted character does"
"not have word syntax and the previous character in the buffer does."
"After insertion, `internal-auto-fill' is called if"
"`auto-fill-function' is non-nil and if the `auto-fill-chars' table has"
"a non-nil value for the inserted character.  At the end, it runs"
"`post-self-insert-hook'.");


DEFINE_SCM_COMMAND(my_scm_newline, newline,
"Insert a newline, and move to left margin of the new line.\n"
"With prefix argument ARG, insert that many newlines.\n"
"\n"
"TODO If `electric-indent-mode' is enabled, this indents the final new line\n"
"that it adds, and reindents the preceding line.  To just insert\n"
"a newline, use \\[electric-indent-just-newline].");

DEFINE_SCM_COMMAND(scm_open_line, open_line,
"Insert a newline and leave point before it.\n"
"TODO If there is a fill prefix and/or a `left-margin', insert them on\n"
"the new line if the line would have been blank.\n"
"With arg N, insert N newlines.");


DEFINE_SCM_COMMAND(scm_split_line, split_line,
"Split current line, moving portion beyond point vertically down.\n"
"TODO If the current line starts with `fill-prefix', insert it on the new\n"
"line as well.  With prefix ARG, don't insert `fill-prefix' on new line.\n"
"\n"
"TODO When called from Lisp code, ARG may be a prefix string to copy.");

DEFINE_SCM_COMMAND(scm_kill_line, kill_line,
"Kill the rest of the current line; if no nonblanks there, kill thru newline.\n"
"With prefix argument ARG, kill that many lines from point.\n"
"Negative arguments kill lines backward.\n"
"With zero argument, kills the text before point on the current line.\n"
"\n"
"When calling from a program, a number counts as a prefix arg.\n"
"\n"
"To kill a whole line, when point is not at the beginning, type \\\n"
"\\[move-beginning-of-line] \\[kill-line] \\[kill-line].\n"
"\n"
"If option `kill-whole-line' is #t, then this command kills the whole line\n"
"including its terminating newline, when used at the beginning of a line\n"
"with no argument.  As a consequence, you can always kill a whole line\n"
"by typing \\[move-beginning-of-line] \\[kill-line].\n"
"\n"
"If you want to append the killed line to the last killed text,\n"
"use \\[append-next-kill] before \\[kill-line].\n"
"\n"
"TODO If the buffer is read-only, Glemax will beep and refrain from deleting\n"
"the line, but put the line in the kill ring anyway.  This means that\n"
"you can use this command to copy text from a read-only buffer.\n"
"\(If the variable `kill-read-only-ok' is #t, then this won't\n"
"even beep.\n");

DEFINE_SCM_COMMAND(scm_kill_word, kill_word,
"Kill characters forward until encountering the end of a word.\n"
"With argument ARG, do this that many times.");

DEFINE_SCM_COMMAND(scm_backward_kill_word, backward_kill_word,
"Kill characters backward until encountering the beginning of a word."
"With argument ARG, do this that many times.");

DEFINE_SCM_COMMAND(scm_kill_region, kill_region,
"Kill (\"cut\") text between point and mark.\n"
"This deletes the text from the buffer and saves it in the kill ring.\n"
"The command \\[yank] can retrieve it from there.\n"
"\(If you want to save the region without killing it, use \\[kill-ring-save].)\n"
"\n"
"TODO If you want to append the killed region to the last killed text,\n"
"use \\[append-next-kill] before \\[kill-region].\n"
"\n"
"TODO Any command that calls this function is a \"kill command\".\n"
"If the previous command was also a kill command,\n"
"the text killed this time appends to the text killed last time\n"
"to make one entry in the kill ring.\n"
"\n"
"TODO If the buffer is read-only, Glemax will beep and refrain from deleting\n"
"the text, but put the text in the kill ring anyway.  This means that\n"
"you can use the killing commands to copy text from a read-only buffer.");

DEFINE_SCM_COMMAND(scm_copy_region_as_kill, copy_region_as_kill,
"Save the region as if killed, but don't kill it."
"\n"
"In Transient Mark mode, deactivate the mark.");

DEFINE_SCM_COMMAND(scm_yank, yank,
"Reinsert (\"paste\") the last stretch of killed text.\n"
"More precisely, reinsert the most recent kill, which is the stretch of\n"
"text most recently killed OR yanked, as returned by `current-kill' (which\n"
"see).  Put point at the end, and set mark at the beginning without\n"
"activating it. With just \\[universal-argument] as argument, put point\n"
"at beginning, and mark at end.\n"
"TODO With argument N, reinsert the Nth most recent kill.\n"
"\n"
"TODO This command honors the `yank-handled-properties' and\n"
"`yank-excluded-properties' variables, and the `yank-handler' text\n"
"property, as described below.\n"
"\n"
"Properties listed in `yank-handled-properties' are processed,\n"
"then those listed in `yank-excluded-properties' are discarded.");

DEFINE_SCM_COMMAND(scm_duplicate_line, duplicate_line,
"Duplicate the current line N times."
"Interactively, N is the prefix numeric argument, and defaults to 1."
"The user option `duplicate-line-final-position' specifies where to"
"move point after duplicating the line."
"Also see the `copy-from-above-command' command.");

DEFINE_SCM_COMMAND(scm_duplicate_region, duplicate_region, NULL);

DEFINE_SCM_COMMAND(scm_duplicate_dwim, duplicate_dwim,
"Duplicate the current line or region N times."
"If the region is inactive, duplicate the current line (like `duplicate-line')."
"Otherwise, duplicate the region, which remains active afterwards."
"If the region is rectangular, duplicate on its right-hand side."
"Interactively, N is the prefix numeric argument, and defaults to 1."
"The variables `duplicate-line-final-position' and"
"`duplicate-region-final-position' control the position of point"
"and the region after the duplication.");



DEFINE_SCM_COMMAND(scm_read_only_mode, read_only_mode,
"Change whether the current buffer is read-only.");


// TODO Add Docstring for all those commands...

DEFINE_SCM_COMMAND(scm_save_buffer,                    save_buffer,                    NULL);

DEFINE_SCM_COMMAND(scm_set_mark_command, set_mark_command,
"Set the mark where point is, and activate it; or jump to the mark."
"Setting the mark also alters the region, which is the text"
"between point and mark; this is the closest equivalent in"
"Emacs to what some editors call the \"selection\"."
"\n"
"With no prefix argument, set the mark at point, and push the"
"old mark position on local mark ring.  Also push the new mark on"
"global mark ring, if the previous mark was set in another buffer."
"\n"
"When Transient Mark Mode is off, immediately repeating this"
"command activates `transient-mark-mode' temporarily."
"\n"
"With prefix argument (e.g., \\[universal-argument] \\[set-mark-command]),"
"jump to the mark, and set the mark from"
"position popped off the local mark ring (this does not affect the global"
"mark ring).  Use \\[pop-global-mark] to jump to a mark popped off the global"
"mark ring (see `pop-global-mark')."
"\n"
"If `set-mark-command-repeat-pop' is non-nil, repeating"
"the \\[set-mark-command] command with no prefix argument pops the next position"
"off the local (or global) mark ring and jumps there."
"\n"
"With \\[universal-argument] \\[universal-argument] as prefix"
"argument, unconditionally set mark where point is, even if"
"`set-mark-command-repeat-pop' is non-nil."
"\n"
"Novice Emacs Lisp programmers often try to use the mark for the wrong"
"purposes.  See the documentation of `set-mark' for more information.");




DEFINE_SCM_COMMAND(scm_delete_indentation, delete_indentation,
"Join this line to previous and fix up whitespace at join."
"If there is a fill prefix, delete it from the beginning of this"
"line."
"With prefix ARG, join the current line to the following line."
"When BEG and END are non-nil, join all lines in the region they"
"define.  Interactively, BEG and END are, respectively, the start"
"and end of the region if it is active, else nil.  (The region is"
"ignored if prefix ARG is given.)");

DEFINE_SCM_COMMAND(scm_delete_char,                    delete_char,                    NULL);
DEFINE_SCM_COMMAND(scm_delete_backward_char,           delete_backward_char,           NULL);

DEFINE_SCM_COMMAND(scm_capitalize_word, capitalize_word,
"Capitalize from point to the end of word, moving over."
"With numerical argument ARG, capitalize the next ARG-1 words as well."
"This gives the word(s) a first character in upper case"
"and the rest lower case."
"\n"
"If point is in the middle of a word, the part of that word before point"
"is ignored when moving forward."
"\n"
"With negative argument, capitalize previous words but do not move.");

DEFINE_SCM_COMMAND(scm_downcase_word, downcase_word,
"Convert to lower case from point to end of word, moving over."
"\n"
"If point is in the middle of a word, the part of that word before point"
"is ignored when moving forward."
"\n"
"With negative argument, convert previous words but do not move.");

DEFINE_SCM_COMMAND(scm_upcase_word, upcase_word,
"Convert to upper case from point to end of word, moving over."
"\n"
"If point is in the middle of a word, the part of that word before point"
"is ignored when moving forward."
"\n"
"With negative argument, convert previous words but do not move."
"See also `capitalize-word'.");

DEFINE_SCM_COMMAND(scm_transpose_chars, transpose_chars,
"Interchange characters around point, moving forward one character."
"With prefix arg ARG, effect is to take character before point"
"and drag it forward past ARG other characters (backward if ARG negative)."
"If at end of line, the previous two chars are exchanged.");

DEFINE_SCM_COMMAND(scm_transpose_words, transpose_words,
"Interchange words around point, leaving point at end of them."
"With prefix arg ARG, effect is to take word before or around point"
"and drag it forward past ARG other words (backward if ARG negative)."
"If ARG is zero, the words around or after point and around or after mark"
"are interchanged.");

DEFINE_SCM_COMMAND(scm_forward_list, forward_list,
"Move forward across one balanced group of parentheses."
"This command will also work on other parentheses-like expressions"
"defined by the current language mode."
"With ARG, do it that many times."
"Negative arg -N means move backward across N groups of parentheses."
"This command assumes point is not in a string or comment."
"Calls `forward-list-function' to do the work, if that is non-nil."
"If INTERACTIVE is non-nil, as it is interactively,"
"report errors as appropriate for this kind of usage.");

DEFINE_SCM_COMMAND(scm_backward_list, backward_list,
"Move backward across one balanced group of parentheses."
"This command will also work on other parentheses-like expressions"
"defined by the current language mode."
"With ARG, do it that many times."
"Negative arg -N means move forward across N groups of parentheses."
"This command assumes point is not in a string or comment."
"Uses `forward-list' to do the work."
"If INTERACTIVE is non-nil, as it is interactively,"
"report errors as appropriate for this kind of usage.");

DEFINE_SCM_COMMAND(scm_forward_sexp, forward_sexp,
"Move forward across one balanced expression (sexp)."
"With ARG, do it that many times.  Negative arg -N means move"
"backward across N balanced expressions.  This command assumes"
"point is not in a string or comment.  Calls"
"`forward-sexp-function' to do the work, if that is non-nil."
"If unable to move over a sexp, signal `scan-error' with three"
"arguments: a message, the start of the obstacle (usually a"
"parenthesis or list marker of some kind), and end of the"
"obstacle.  If INTERACTIVE is non-nil, as it is interactively,"
"report errors as appropriate for this kind of usage.");

DEFINE_SCM_COMMAND(scm_backward_sexp, backward_sexp,
"Move backward across one balanced expression (sexp)."
"With ARG, do it that many times.  Negative arg -N means"
"move forward across N balanced expressions."
"This command assumes point is not in a string or comment."
"Uses `forward-sexp' to do the work."
"If INTERACTIVE is non-nil, as it is interactively,"
"report errors as appropriate for this kind of usage.");

DEFINE_SCM_COMMAND(scm_kill_sexp, kill_sexp,
"Kill the sexp (balanced expression) following point."
"With ARG, kill that many sexps after point."
"Negative arg -N means kill N sexps before point."
"This command assumes point is not in a string or comment."
"If INTERACTIVE is non-nil, as it is interactively,"
"report errors as appropriate for this kind of usage.");



DEFINE_SCM_COMMAND(scm_mark_sexp, mark_sexp,
"Set mark ARG sexps from point or move mark one sexp."
"When called from Lisp with ALLOW-EXTEND omitted or nil, mark is"
"set ARG sexps from point."
"With ARG and ALLOW-EXTEND both non-nil (interactively, with prefix"
"argument), the place to which mark goes is the same place \\[forward-sexp]"
"would move to with the same argument; if the mark is active, it moves"
"ARG sexps from its current position, otherwise it is set ARG sexps"
"from point."
"When invoked interactively without a prefix argument and no active"
"region, mark moves one sexp forward."
"When invoked interactively without a prefix argument, and region"
"is active, mark moves one sexp away of point (i.e., forward"
"if mark is at or after point, back if mark is before point), thus"
"extending the region by one sexp.  Since the direction of region"
"extension depends on the relative position of mark and point, you"
"can change the direction by \\[exchange-point-and-mark]."
"This command assumes point is not in a string or comment.");


DEFINE_SCM_COMMAND(scm_forward_char, forward_char,
"Move point N characters forward (backward if N is negative)."
"\n"
"On reaching end or beginning of buffer, stop and signal error."
"Interactively, N is the numeric prefix argument."
"If N is omitted, move point 1 character forward."
"\n"
"Depending on the bidirectional context, the movement may be to the"
"right or to the left on the screen.  This is in contrast with"
"`right-char', which see.");

DEFINE_SCM_COMMAND(scm_backward_char, backward_char,
"Move point N characters backward (forward if N is negative)."
"\n"
"On attempt to pass beginning or end of buffer, stop and signal error."
"Interactively, N is the numeric prefix argument."
"If N is omitted, move point 1 character backward."
"\n"
"Depending on the bidirectional context, the movement may be to the"
"right or to the left on the screen.  This is in contrast with"
"left-char, which see.");

DEFINE_SCM_COMMAND(scm_next_line, next_line,
"Move cursor vertically down ARG lines."
"ARG defaults to 1."
"\n"
"If there is no character in the target line exactly under the current column,"
"the cursor is positioned after the character in that line that spans this"
"column, or at the end of the line if it is not long enough."
"If there is no line in the buffer after this one, behavior depends on the"
"value of `next-line-add-newlines'.  If #t, it inserts a newline character"
"to create a line, and moves the cursor to that line.  Otherwise it moves the"
"cursor to the end of the buffer."
"\n"
"If the variable `line-move-visual' is #t, this command moves"
"by display lines.  Otherwise, it moves by buffer lines, without"
"taking (TODO variable-width characters) or continued lines into account."
"See \\[next-logical-line] for a command that always moves by buffer lines."
"\n"
"The command \\[set-goal-column] can be used to create"
"a semipermanent goal column for this command."
"Then instead of trying to move exactly vertically (or as close as possible),"
"this command moves to the specified goal column (or as close as possible)."
"The goal column is stored in the variable `goal-column', which is #f"
"when there is no goal column.  TODO Note that setting `goal-column'"
"overrides `line-move-visual' and causes this command to move by buffer"
"lines rather than by display lines.");

DEFINE_SCM_COMMAND(scm_previous_line, previous_line,
"Move cursor vertically up ARG lines."
"ARG defaults to 1."
"\n"
"If there is no character in the target line exactly over the current column,"
"the cursor is positioned after the character in that line that spans this"
"column, or at the end of the line if it is not long enough."
""
"If the variable `line-move-visual' is #t, this command moves"
"by display lines.  Otherwise, it moves by buffer lines, without"
"taking (TODO variable-width characters) or continued lines into account."
"See \\[previous-logical-line] for a command that always moves by buffer lines."
"\n"
"The command \\[set-goal-column] can be used to create"
"a semipermanent goal column for this command."
"Then instead of trying to move exactly vertically (or as close as possible),"
"this command moves to the specified goal column (or as close as possible)."
"The goal column is stored in the variable `goal-column', which is #f"
"when there is no goal column.  TODO Note that setting `goal-column'"
"overrides `line-move-visual' and causes this command to move by buffer"
"lines rather than by display lines.");

DEFINE_SCM_COMMAND(scm_line_move, line_move,
"Move forward ARG lines.");

DEFINE_SCM_COMMAND(scm_line_move_logical, line_move_logical,
"Move forward ARG lines logically.");

DEFINE_SCM_COMMAND(scm_line_move_visual, line_move_visual,
"Move ARG lines forward visually.");

DEFINE_SCM_COMMAND(scm_next_logical_line, next_logical_line,
"Move cursor vertically down ARG lines."
"This is identical to `next-line', except that it always moves"
"by logical lines instead of visual lines, ignoring the value of"
"the variable `line-move-visual'.");

DEFINE_SCM_COMMAND(scm_previous_logical_line, previous_logical_line,
"Move cursor vertically up ARG lines."
"This is identical to `previous-line', except that it always moves"
"by logical lines instead of visual lines, ignoring the value of"
"the variable `line-move-visual'.");


DEFINE_SCM_COMMAND(scm_forward_word,                   forward_word,                   NULL);
DEFINE_SCM_COMMAND(scm_backward_word,                  backward_word,                  NULL);
DEFINE_SCM_COMMAND(scm_forward_paragraph,              forward_paragraph,              NULL);
DEFINE_SCM_COMMAND(scm_backward_paragraph,             backward_paragraph,             NULL);

DEFINE_SCM_COMMAND(scm_forward_page,                   forward_page,
"Move forward to page boundary.  With arg, repeat, or go back if negative."
"A page boundary is any line whose beginning matches the regexp"
"`page-delimiter'.");

DEFINE_SCM_COMMAND(scm_backward_page, backward_page,
"Move backward to page boundary.  With arg, repeat, or go fwd if negative."
"A page boundary is any line whose beginning matches the regexp"
"`page-delimiter'.");

DEFINE_SCM_COMMAND(scm_mark_page, mark_page,
"Put mark at end of page, point at beginning."
"A numeric arg specifies to move forward or backward by that many pages,"
"thus marking a page other than the one point was originally in.");

DEFINE_SCM_COMMAND(scm_count_lines_page, count_lines_page,
"Report number of lines on current page, and how many are before or after point.");


DEFINE_SCM_COMMAND(scm_beginning_of_line, beginning_of_line,
"Move point to beginning of current line (in the logical order)."
"With argument N not nil or 1, move forward N - 1 lines first."
"If point reaches the beginning or end of buffer, it stops there."
"\n"
"This function constrains point to the current field unless this moves"
"point to a different line from the original, unconstrained result."
"If N is nil or 1, and a front-sticky field starts at point, the point"
"does not move.  To ignore field boundaries bind"
"`inhibit-field-text-motion' to t, or use the `forward-line' function"
"instead.  For instance, `(forward-line 0)' does the same thing as"
"`(beginning-of-line)', except that it ignores field boundaries.");

DEFINE_SCM_COMMAND(scm_end_of_line, end_of_line,
"Move point to end of current line (in the logical order)."
"With argument N not nil or 1, move forward N - 1 lines first."
"If point reaches the beginning or end of buffer, it stops there."
"To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
"\n"
"This function constrains point to the current field unless this moves"
"point to a different line from the original, unconstrained result.  If"
"N is nil or 1, and a rear-sticky field ends at point, the point does"
"not move.  To ignore field boundaries bind `inhibit-field-text-motion'"
"to t.");

DEFINE_SCM_COMMAND(scm_beginning_of_visual_line,       beginning_of_visual_line,       NULL);
DEFINE_SCM_COMMAND(scm_end_of_visual_line,             end_of_visual_line,             NULL);

DEFINE_SCM_COMMAND(scm_beginning_of_buffer,            beginning_of_buffer,            NULL);
DEFINE_SCM_COMMAND(scm_end_of_buffer,                  end_of_buffer,                  NULL);
DEFINE_SCM_COMMAND(scm_split_window_below,             split_window_below,             NULL);
DEFINE_SCM_COMMAND(scm_split_window_right,             split_window_right,             NULL);
DEFINE_SCM_COMMAND(scm_delete_window,                  delete_window,                  NULL);
DEFINE_SCM_COMMAND(scm_delete_other_windows,           delete_other_windows,           NULL);
DEFINE_SCM_COMMAND(scm_other_window,                   other_window,                   NULL);
DEFINE_SCM_COMMAND(scm_balance_windows,                balance_windows,                NULL);
DEFINE_SCM_COMMAND(scm_enlarge_window,                 enlarge_window,                 NULL);
DEFINE_SCM_COMMAND(scm_recenter,                       recenter,                       NULL);
DEFINE_SCM_COMMAND(scm_recenter_top_bottom,            recenter_top_bottom,            NULL);
DEFINE_SCM_COMMAND(scm_scroll_down_command,            scroll_down_command,            NULL);
DEFINE_SCM_COMMAND(scm_scroll_up_command,              scroll_up_command,              NULL);
DEFINE_SCM_COMMAND(scm_scroll_other_window,            scroll_other_window,            NULL);
DEFINE_SCM_COMMAND(scm_scroll_other_window_down,       scroll_other_window_down,       NULL);
DEFINE_SCM_COMMAND(scm_move_to_window_line,            move_to_window_line,            NULL);
DEFINE_SCM_COMMAND(scm_move_to_window_line_top_bottom, move_to_window_line_top_bottom, NULL);
DEFINE_SCM_COMMAND(scm_next_buffer,                    next_buffer,                    NULL);
DEFINE_SCM_COMMAND(scm_previous_buffer,                previous_buffer,                NULL);

DEFINE_SCM_COMMAND(scm_beginning_of_defun,             beginning_of_defun,             NULL);
DEFINE_SCM_COMMAND(scm_end_of_defun,                   end_of_defun,                   NULL);

DEFINE_SCM_COMMAND(scm_describe_key_briefly, describe_key_briefly,
"Print the name of the functions KEY-LIST invokes."
"KEY-LIST is a list of pairs (SEQ . RAW-SEQ) of key sequences, where"
"RAW-SEQ is the untranslated form of the key sequence SEQ."
"If INSERT (the prefix arg) is non-nil, insert the message in the buffer."
"\n"
"While reading KEY-LIST interactively, this command temporarily enables"
"menu items or tool-bar buttons that are disabled to allow getting help"
"on them."
"\n"
"BUFFER is the buffer in which to lookup those keys; it defaults to the"
"current buffer.");

DEFINE_SCM_COMMAND(scm_exit_recursive_edit, exit_recursive_edit,
"Exit from the innermost recursive edit or minibuffer.");



// Query functions
static SCM scm_point(void) {
    return scm_from_size_t(current_buffer->pt);
}

static SCM scm_mark(void) {
    return scm_from_size_t(current_buffer->region.mark);
}

static SCM scm_mark_active_p(void) {
    return scm_from_bool(current_buffer->region.active);
}

static SCM scm_current_column(void) {
    return scm_from_size_t(current_column());
}

static SCM scm_line_beginning_position(SCM n_scm) {
    int n = 1;  // Default to 1

    if (!SCM_UNBNDP(n_scm)) {
        if (!scm_is_integer(n_scm)) {
            scm_wrong_type_arg("line-beginning-position", 1, n_scm);
        }
        n = scm_to_int(n_scm);
    }

    return scm_from_size_t(line_beginning_position(n));
}

static SCM scm_line_end_position(SCM n_scm) {
    int n = 1;  // Default to 1

    if (!SCM_UNBNDP(n_scm)) {
        if (!scm_is_integer(n_scm)) {
            scm_wrong_type_arg("line-end-position", 1, n_scm);
        }
        n = scm_to_int(n_scm);
    }

    return scm_from_size_t(line_end_position(n));
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


static SCM scm_hide_cursor(void) {
    hideCursor();
    scm_c_define("pointer-visible", SCM_BOOL_F);
    return SCM_UNSPECIFIED;
}

static SCM scm_show_cursor(void) {
    showCursor();
    scm_c_define("pointer-visible", SCM_BOOL_T);
    return SCM_UNSPECIFIED;
}

// Buffer content access
static SCM scm_buffer_substring(SCM start, SCM end) {
    if (!scm_is_integer(start) || !scm_is_integer(end)) {
        scm_wrong_type_arg("buffer-substring", 0, SCM_BOOL_F);
    }

    size_t start_pos = scm_to_size_t(start);
    size_t end_pos = scm_to_size_t(end);

    if (start_pos > end_pos || end_pos > rope_char_length(current_buffer->rope)) {
        scm_out_of_range("buffer-substring", end);
    }

    size_t len = end_pos - start_pos;
    char *str = malloc(len + 1);

    for (size_t i = 0; i < len; i++) {
        str[i] = (char)rope_char_at(current_buffer->rope, start_pos + i);
    }
    str[len] = '\0';

    SCM result = scm_from_locale_string(str);
    free(str);
    return result;
}

static SCM scm_char_after(SCM pos) {
    size_t p;

    if (scm_is_integer(pos)) {
        p = scm_to_size_t(pos);
    } else {
        p = current_buffer->pt;
    }

    if (p >= rope_char_length(current_buffer->rope)) {
        return SCM_BOOL_F;
    }

    return scm_from_uint32(rope_char_at(current_buffer->rope, p));
}

static SCM scm_char_before(SCM pos) {
    size_t p;

    if (scm_is_integer(pos)) {
        p = scm_to_size_t(pos);
    } else {
        p = current_buffer->pt;
    }

    if (p == 0) {
        return SCM_BOOL_F;
    }

    return scm_from_uint32(rope_char_at(current_buffer->rope, p - 1));
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

int scm_get_int(const char *name, int default_value) {
    SCM var = scm_c_lookup(name);
    if (scm_is_false(var)) {
        return default_value;
    }
    SCM val = scm_variable_ref(var);
    if (scm_is_integer(val)) {
        return scm_to_int(val);
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

char* scm_get_string(const char *name, const char *default_value) {
    SCM var = scm_c_lookup(name);
    if (scm_is_false(var)) {
        return default_value ? strdup(default_value) : NULL;
    }
    SCM val = scm_variable_ref(var);
    if (scm_is_string(val)) {
        return scm_to_locale_string(val);
    }
    return default_value ? strdup(default_value) : NULL;
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

static SCM scm_keymap_global_set(SCM notation_scm, SCM action_scm) {
    if (!scm_is_string(notation_scm))
        scm_wrong_type_arg("keymap-global-set", 1, notation_scm);
    if (!scm_is_true(scm_procedure_p(action_scm)))
        scm_wrong_type_arg("keymap-global-set", 2, action_scm);

    char *notation = scm_to_locale_string(notation_scm);
    char *description = get_scheme_proc_documentation(action_scm);

    bool result = keychord_bind_scheme(&keymap, notation, action_scm,
                                       description, PRESS | REPEAT);

    // Mirror into sub-keymaps
    const struct { const char *prefix; const char *map_var; } mirrors[] = {
        { "C-x 4 ", "ctl-x-4-map" },
        { "C-x 5 ", "ctl-x-5-map" },
        { "C-x ",   "ctl-x-map"   },
    };
    for (size_t i = 0; i < sizeof(mirrors) / sizeof(mirrors[0]); i++) {
        size_t plen = strlen(mirrors[i].prefix);
        if (strncmp(notation, mirrors[i].prefix, plen) == 0) {
            const char *suffix = notation + plen;
            SCM map_var = scm_c_lookup(mirrors[i].map_var);
            if (scm_is_true(map_var)) {
                SCM map_val = scm_variable_ref(map_var);
                if (!scm_is_false(map_val)) {
                    KeyChordMap *map = scm_to_keymap(map_val);
                    if (map)
                        keychord_bind_scheme(map, suffix, action_scm,
                                             description, PRESS | REPEAT);
                }
            }
            break;
        }
    }

    free(notation);
    if (description) free(description);
    return scm_from_bool(result);
}

/* static SCM scm_keymap_global_set(SCM notation_scm, SCM action_scm) { */
/*     if (!scm_is_string(notation_scm)) { */
/*         scm_wrong_type_arg("keymap-global-set", 1, notation_scm); */
/*     } */
/*     if (!scm_is_true(scm_procedure_p(action_scm))) { */
/*         scm_wrong_type_arg("keymap-global-set", 2, action_scm); */
/*     } */

/*     char *notation = scm_to_locale_string(notation_scm); */
/*     char *description = get_scheme_proc_documentation(action_scm); */

/*     bool result = keychord_bind_scheme(&keymap, notation, action_scm, */
/*                                        description, PRESS | REPEAT); */

/*     free(notation); */
/*     if (description) { */
/*         free(description); */
/*     } */

/*     return scm_from_bool(result); */
/* } */

static SCM scm_keymap_global_unset(SCM notation_scm) {
    if (!scm_is_string(notation_scm)) {
        scm_wrong_type_arg("keymap-global-unset", 1, notation_scm);
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
static SCM buffer_object_cache;  // Hash table: Buffer* -> SCM

SCM get_or_make_buffer_object(Buffer *buf) {
    if (!buf) return SCM_BOOL_F;

    // Use pointer as key (convert to integer)
    SCM key = scm_from_uintptr_t((uintptr_t)buf);
    SCM cached = scm_hashq_ref(buffer_object_cache, key, SCM_BOOL_F);

    if (scm_is_true(cached)) {
        return cached;
    }

    // Not cached - create new object and cache it
    SCM obj = scm_make_foreign_object_1(buffer_type, buf);
    scm_hashq_set_x(buffer_object_cache, key, obj);
    return obj;
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

static SCM scm_enable_minor_mode(SCM mode_symbol, SCM buffer_obj) {
    Buffer *buf = SCM_UNBNDP(buffer_obj) ? current_buffer : scm_to_buffer(buffer_obj);
    enable_minor_mode(mode_symbol, buf);
    return SCM_UNSPECIFIED;
}

static SCM scm_disable_minor_mode(SCM mode_symbol, SCM buffer_obj) {
    Buffer *buf = SCM_UNBNDP(buffer_obj) ? current_buffer : scm_to_buffer(buffer_obj);
    disable_minor_mode(mode_symbol, buf);
    return SCM_UNSPECIFIED;
}

static SCM scm_minor_mode_active_p(SCM mode_symbol, SCM buffer_obj) {
    Buffer *buf = SCM_UNBNDP(buffer_obj) ? current_buffer : scm_to_buffer(buffer_obj);
    return minor_mode_active_p(mode_symbol, buf) ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_buffer_size(SCM buffer_obj) {
    Buffer *buf;

    if (SCM_UNBNDP(buffer_obj)) {
        buf = current_buffer;
    } else if (SCM_IS_A_P(buffer_obj, buffer_type)) {
        buf = scm_to_buffer(buffer_obj);
    } else {
        scm_wrong_type_arg("buffer-size", 1, buffer_obj);
    }

    if (!buf) {
        return scm_from_size_t(0);
    }

    return scm_from_size_t(rope_char_length(buf->rope));
}

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
            buf = buffer_create(buffer_name);
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

static SCM scm_kill_buffer(SCM buffer_or_name) {
    Buffer *buf;
    if (SCM_UNBNDP(buffer_or_name)) {
        buf = current_buffer;
    } else if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        buf = scm_to_buffer(buffer_or_name);
    } else if (scm_is_string(buffer_or_name)) {
        char *name = scm_to_locale_string(buffer_or_name);
        buf = get_buffer(name);
        free(name);
        if (!buf) return SCM_BOOL_F;
    } else {
        scm_wrong_type_arg("kill-buffer", 1, buffer_or_name);
        return SCM_BOOL_F;
    }
    do_kill_buffer(buf);
    return SCM_BOOL_T;
}

static SCM scm_kill_current_buffer(void) {
    kill_current_buffer();
    return SCM_BOOL_T;
}

static SCM scm_other_buffer(void) {
    Buffer *next = other_buffer();
    if (next) {
        return get_or_make_buffer_object(next);
    }
    return SCM_EOL;
}

static SCM scm_get_buffer(SCM buffer_or_name) {
    Buffer *buf = NULL;

    if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        // It's already a buffer object - just return it
        return buffer_or_name;
    } else if (scm_is_string(buffer_or_name)) {
        // It's a string name - look it up
        char *buffer_name = scm_to_locale_string(buffer_or_name);
        buf = get_buffer(buffer_name);
        free(buffer_name);

        return buf ? get_or_make_buffer_object(buf) : SCM_BOOL_F;
    } else {
        scm_wrong_type_arg("get-buffer", 1, buffer_or_name);
        return SCM_BOOL_F;  // Won't reach here, but satisfies compiler
    }
}

static SCM scm_get_buffer_create(SCM buffer_or_name) {
    Buffer *buf = NULL;

    if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        // It's already a buffer object - just return it
        return buffer_or_name;
    } else if (scm_is_string(buffer_or_name)) {
        // It's a string name - get or create it
        char *buffer_name = scm_to_locale_string(buffer_or_name);
        buf = get_buffer_create(buffer_name);
        free(buffer_name);

        return get_or_make_buffer_object(buf);
    } else {
        scm_wrong_type_arg("get-buffer-create", 1, buffer_or_name);
        return SCM_BOOL_F;  // Won't reach here, but satisfies compiler
    }
}

static SCM scm_current_buffer(void) {
    if (!current_buffer) return SCM_EOL;
    return get_or_make_buffer_object(current_buffer);
}

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

static SCM scm_buffer_list(void) {
    if (!all_buffers) return SCM_EOL;

    SCM list = SCM_EOL;
    Buffer *buf = all_buffers;

    do {
        SCM buf_obj = get_or_make_buffer_object(buf);
        list = scm_cons(buf_obj, list);
        buf = buf->next;
    } while (buf != all_buffers);

    return scm_reverse(list);
}

static SCM scm_append_to_buffer(SCM buffer_or_name, SCM start, SCM end) {
    Buffer *buf = NULL;
    if (scm_is_string(buffer_or_name)) {
        char *name = scm_to_utf8_string(buffer_or_name);
        buf = get_buffer_create(name);  // creates if doesn't exist, per docstring
        free(name);
    } else if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        buf = (Buffer *)scm_foreign_object_ref(buffer_or_name, 0);
    } else {
        scm_wrong_type_arg("append-to-buffer", 1, buffer_or_name);
        return SCM_UNSPECIFIED;
    }

    size_t s, e;
    if (SCM_UNBNDP(start) || SCM_UNBNDP(end)) {
        // Interactive: use region
        if (!current_buffer->region.active || current_buffer->region.mark < 0) {
            message("The mark is not set now, so there is no region");
            return SCM_UNSPECIFIED;
        }
        size_t pt   = current_buffer->pt;
        size_t mark = (size_t)current_buffer->region.mark;
        s = pt < mark ? pt : mark;
        e = pt < mark ? mark : pt;
    } else {
        s = scm_to_size_t(start);
        e = scm_to_size_t(end);
    }

    append_to_buffer(buf, s, e);
    return SCM_UNSPECIFIED;
}

static SCM scm_buffer_file_name(SCM buffer_obj) {
    Buffer *buf;

    // If buffer argument is not provided, use current_buffer
    if (SCM_UNBNDP(buffer_obj)) {
        buf = current_buffer;
    } else if (SCM_IS_A_P(buffer_obj, buffer_type)) {
        // It's a buffer foreign object
        buf = scm_to_buffer(buffer_obj);
    } else {
        scm_wrong_type_arg("buffer-file-name", 1, buffer_obj);
    }

    if (!buf) {
        return SCM_BOOL_F;
    }

    char *filename = buf->filename;

    if (filename) {
        return scm_from_locale_string(filename);
    } else {
        return SCM_BOOL_F;
    }
}

/// Buffer local variables

static SCM scm_set_default(SCM symbol, SCM value) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("set-default", 1, symbol);
    }

    set_default(symbol, value);
    return value;
}

static SCM scm_default_value(SCM symbol) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("default-value", 1, symbol);
    }

    return default_value(symbol);
}

static SCM scm_buffer_local_value(SCM variable, SCM buffer_obj) {
    if (!scm_is_symbol(variable)) {
        scm_wrong_type_arg("buffer-local-value", 1, variable);
    }

    Buffer *buf;

    // Make buffer argument optional - default to current_buffer
    if (SCM_UNBNDP(buffer_obj)) {
        buf = current_buffer;
    } else {
        buf = scm_to_buffer(buffer_obj);
        if (!buf) {
            scm_wrong_type_arg("buffer-local-value", 2, buffer_obj);
        }
    }

    if (!buf) {
        // No current buffer - return #f or try default value
        return default_value(variable);
    }

    return buffer_local_value(variable, buf);
}

static SCM scm_set(SCM symbol, SCM newval) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("set", 1, symbol);
    }

    return buffer_set(symbol, newval, current_buffer);
}

#include <gc/gc.h>

SCM scm_setq_impl(SCM symbol, SCM value) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("setq", 1, symbol);
    }

    if (!current_buffer) {
        // No current buffer, set globally
        SCM symbol_str = scm_symbol_to_string(symbol);
        char *c_str = scm_to_locale_string(symbol_str);
        scm_c_module_define(scm_current_module(), c_str, value);
        /* free(c_str); */

        // Check if we're setting frame-resize-pixelwise
        if (strcmp(c_str, "frame-resize-pixelwise") == 0 && selected_frame)
            update_frame_resize_mode(selected_frame);

        // Check if we're setting gc-cons-threshold
        if (strcmp(c_str, "gc-free-space-divisor") == 0) {
            printf("IT RUN!\n");
            GC_set_free_space_divisor((GC_word)scm_to_size_t(value));
        }

        return value;
    }

    // Check if variable is already buffer-local in current buffer
    if (local_variable_p(symbol, current_buffer)) {
        return buffer_set(symbol, value, current_buffer);
    }

    // Check if variable is marked as automatically buffer-local
    if (is_automatically_buffer_local(symbol)) {
        if (!local_variable_p(symbol, current_buffer)) {
            SCM def_val = default_value(symbol);
            buffer_set(symbol, def_val, current_buffer);
        }
        return buffer_set(symbol, value, current_buffer);
    }

    // Not buffer-local, set globally
    SCM symbol_str = scm_symbol_to_string(symbol);
    char *c_str = scm_to_locale_string(symbol_str);
    scm_c_module_define(scm_current_module(), c_str, value);

    // Check if we're setting frame-resize-pixelwise
    if (strcmp(c_str, "frame-resize-pixelwise") == 0 && selected_frame)
        update_frame_resize_mode(selected_frame);

    if (strcmp(c_str, "gc-free-space-divisor") == 0)
        GC_set_free_space_divisor((GC_word)scm_to_size_t(value));

    free(c_str);
    return value;
}

// Register as a macro/syntax, not a regular procedure
void init_setq() {
    // Define the macro in Scheme that calls our C implementation
    scm_c_eval_string(
        "(define-syntax setq "
        "  (syntax-rules () "
        "    ((setq var val) "
        "     (setq-impl 'var val))))"
    );

    // Register the implementation function
}

static SCM scm_local_variable_p(SCM symbol, SCM buffer_obj) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("local-variable-p", 1, symbol);
    }

    Buffer *buf;
    if (SCM_UNBNDP(buffer_obj)) {
        buf = current_buffer;
    } else {
        buf = scm_to_buffer(buffer_obj);
        if (!buf) {
            scm_wrong_type_arg("local-variable-p", 2, buffer_obj);
        }
    }

    return scm_from_bool(local_variable_p(symbol, buf));
}

static SCM scm_local_variable_if_set_p(SCM symbol, SCM buffer_obj) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("local-variable-if-set-p", 1, symbol);
    }

    Buffer *buf;
    if (SCM_UNBNDP(buffer_obj)) {
        buf = current_buffer;
    } else {
        buf = scm_to_buffer(buffer_obj);
        if (!buf) {
            scm_wrong_type_arg("local-variable-if-set-p", 2, buffer_obj);
        }
    }

    return scm_from_bool(local_variable_if_set_p(symbol, buf));
}

static SCM scm_kill_local_variable(SCM symbol) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("kill-local-variable", 1, symbol);
    }

    return kill_local_variable(symbol, current_buffer);
}

static SCM scm_kill_all_local_variables(void) {
    kill_all_local_variables(current_buffer);
    return SCM_UNSPECIFIED;
}

static SCM scm_buffer_local_variables(SCM buffer_obj) {
    Buffer *buf;
    if (SCM_UNBNDP(buffer_obj)) {
        buf = current_buffer;
    } else {
        buf = scm_to_buffer(buffer_obj);
        if (!buf) {
            scm_wrong_type_arg("buffer-local-variables", 1, buffer_obj);
        }
    }

    return buffer_local_variables(buf);
}

static SCM scm_make_local_variable(SCM symbol) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("make-local-variable", 1, symbol);
    }

    if (!current_buffer) {
        scm_misc_error("make-local-variable", "No current buffer", SCM_EOL);
    }

    // If not already local, copy default value to make it local
    if (!local_variable_p(symbol, current_buffer)) {
        SCM def_val = default_value(symbol);
        buffer_set(symbol, def_val, current_buffer);
    }

    return symbol;
}

static SCM scm_make_variable_buffer_local(SCM symbol) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("make-variable-buffer-local", 1, symbol);
    }

    // Mark this variable as automatically buffer-local
    mark_automatically_buffer_local(symbol);

    return symbol;
}

static SCM scm_automatically_buffer_local_p(SCM symbol) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("automatically-buffer-local?", 1, symbol);
    }

    return scm_from_bool(is_automatically_buffer_local(symbol));
}


/// WINDOW

static SCM window_type;

static SCM window_object_cache;  // Hash table: Window* -> SCM

// NOTE This is so (eq? (next-window) (next-window)) => #t
// Making Window objects have identity.
// That's needed because every call to make_window_object()
// creates a new Scheme foreign object, even if it wraps the same underlying C Window* pointer.

SCM get_or_make_window_object(Window *win) {
    if (!win) return SCM_BOOL_F;

    // Use pointer as key (convert to integer)
    SCM key = scm_from_uintptr_t((uintptr_t)win);
    SCM cached = scm_hashq_ref(window_object_cache, key, SCM_BOOL_F);

    if (scm_is_true(cached)) {
        return cached;
    }

    // Not cached - create new object and cache it
    SCM obj = scm_make_foreign_object_1(window_type, win);
    scm_hashq_set_x(window_object_cache, key, obj);
    return obj;
}

static Window* scm_to_window(SCM obj) {
    scm_assert_foreign_object_type(window_type, obj);
    return scm_foreign_object_ref(obj, 0);
}

static SCM window_p(SCM obj) {
    return scm_from_bool(SCM_IS_A_P(obj, window_type));
}

static SCM scm_selected_window(void) {
    return get_or_make_window_object(selected_frame->wm.selected);
}

static SCM scm_minibuffer_window(void) {
    return get_or_make_window_object(selected_frame->wm.minibuffer_window);
}

static SCM scm_window_buffer(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-buffer", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    return get_or_make_buffer_object(win->buffer);
}

static SCM scm_window_point(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        // No argument provided - use selected window
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-point", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    return scm_from_size_t(win->point);
}

static SCM scm_set_window_point(SCM window_obj, SCM pos_scm) {
    if (!scm_is_true(window_p(window_obj))) {
        scm_wrong_type_arg("set-window-point", 1, window_obj);
    }
    if (!scm_is_integer(pos_scm)) {
        scm_wrong_type_arg("set-window-point", 2, pos_scm);
    }

    Window *win = scm_to_window(window_obj);
    win->point = scm_to_size_t(pos_scm);

    // If this is the selected window, update buffer's point too
    if (win == selected_frame->wm.selected && win->buffer) {
        win->buffer->pt = win->point;
    }

    return SCM_UNSPECIFIED;
}

static SCM scm_window_list(void) {
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);

    SCM result = SCM_EOL;
    for (int i = count - 1; i >= 0; i--) {
        result = scm_cons(get_or_make_window_object(leaves[i]), result);
    }

    // Add minibuffer if active
    if (selected_frame->wm.minibuffer_active) {
        result = scm_append(scm_list_2(result,
                           scm_list_1(get_or_make_window_object(selected_frame->wm.minibuffer_window))));
    }

    return result;
}

static SCM scm_next_window(SCM window_obj, SCM minibuf_scm) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("next-window", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    // TODO: handle minibuf argument (whether to include minibuffer in cycle)
    // For now, use your existing next_window() logic
    Window *next = next_window(win);
    return get_or_make_window_object(next);
}

static SCM scm_previous_window(SCM window_obj, SCM minibuf_scm) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("previous-window", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    Window *prev = previous_window(win);
    return get_or_make_window_object(prev);
}

static SCM scm_select_window(SCM window_obj) {
    if (!scm_is_true(window_p(window_obj))) {
        scm_wrong_type_arg("select-window", 1, window_obj);
    }

    Window *win = scm_to_window(window_obj);

    if (win == selected_frame->wm.selected) {
        return SCM_UNSPECIFIED;
    }

    // Save current window's point
    selected_frame->wm.selected->point = current_buffer->pt;

    // Switch selection
    selected_frame->wm.selected->is_selected = false;
    win->is_selected = true;
    selected_frame->wm.selected = win;

    // Update buffer and point
    current_buffer = win->buffer;
    current_buffer->pt = win->point;

    return SCM_UNSPECIFIED;
}

static SCM scm_window_pixel_width(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-width", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    return scm_from_double(win->width);
}

static SCM scm_window_pixel_height(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-height", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    return scm_from_double(win->height);
}

static SCM scm_minibuffer_window_p(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-minibuffer-p", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    return scm_from_bool(is_minibuffer_window(win));
}

static SCM scm_set_window_buffer(SCM window_obj, SCM buffer_obj) {
    if (!scm_is_true(window_p(window_obj))) {
        scm_wrong_type_arg("set-window-buffer", 1, window_obj);
    }

    Buffer *buf = NULL;
    if (scm_is_string(buffer_obj)) {
        char *name = scm_to_utf8_string(buffer_obj);
        buf = get_buffer(name);
        free(name);
        if (!buf) {
            scm_misc_error("set-window-buffer", "No buffer named ~S", scm_list_1(buffer_obj));
        }
    } else if (SCM_IS_A_P(buffer_obj, buffer_type)) {
        buf = scm_to_buffer(buffer_obj);
    } else {
        scm_wrong_type_arg("set-window-buffer", 2, buffer_obj);
    }

    Window *win = scm_to_window(window_obj);
    win->buffer = buf;

    // If this is the selected window, update current_buffer
    if (win == selected_frame->wm.selected) {
        current_buffer = buf;
        current_buffer->pt = win->point;
    }

    return SCM_UNSPECIFIED;
}

static SCM scm_window_try_horizontal_split(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-try-horizontal-split", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    // Temporarily set selected window if different
    Window *original_selected = selected_frame->wm.selected;
    selected_frame->wm.selected = win;

    bool success = window_try_horizontal_split(win);

    Window *new_window = NULL;
    if (success) {
        new_window = selected_frame->wm.selected;  // The newly selected window
    }

    // Restore original selection
    selected_frame->wm.selected = original_selected;

    if (new_window) {
        return get_or_make_window_object(new_window);
    } else {
        return SCM_BOOL_F;
    }
}

static SCM scm_window_try_vertical_split(SCM window_obj) {
    Window *win;

    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        if (!scm_is_true(window_p(window_obj))) {
            scm_wrong_type_arg("window-try-vertical-split", 1, window_obj);
        }
        win = scm_to_window(window_obj);
    }

    // Temporarily set selected window if different
    Window *original_selected = selected_frame->wm.selected;
    selected_frame->wm.selected = win;

    bool success = window_try_vertical_split(win);

    Window *new_window = NULL;
    if (success) {
        new_window = selected_frame->wm.selected;  // The newly selected window
    }

    // Restore original selection
    selected_frame->wm.selected = original_selected;

    if (new_window) {
        return get_or_make_window_object(new_window);
    } else {
        return SCM_BOOL_F;
    }
}

static SCM scm_split_window_sensibly(SCM codepoint) {
    Window *new_window = split_window_sensibly();

    if (new_window) {
        return get_or_make_window_object(new_window);
    } else {
        return SCM_BOOL_F;  // Return #f if split failed
    }
}

static SCM scm_display_buffer(SCM buffer_or_name, SCM action, SCM frame) {
    Buffer *buffer = NULL;

    // Handle BUFFER-OR-NAME
    if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        buffer = scm_to_buffer(buffer_or_name);
    } else if (scm_is_string(buffer_or_name)) {
        char *buffer_name = scm_to_locale_string(buffer_or_name);
        buffer = get_buffer(buffer_name);
        free(buffer_name);

        if (!buffer) {
            scm_misc_error("display-buffer", "No buffer named ~S",
                          scm_list_1(buffer_or_name));
        }
    } else {
        scm_wrong_type_arg("display-buffer", 1, buffer_or_name);
    }

    Window *window = display_buffer(buffer);

    if (window) {
        return get_or_make_window_object(window);
    }

    return SCM_BOOL_F;
}

static SCM scm_fit_window_to_buffer(SCM window_obj) {
    Window *win;
    if (SCM_UNBNDP(window_obj)) {
        win = selected_frame->wm.selected;
    } else {
        win = scm_to_window(window_obj);
        if (!win) return SCM_BOOL_F;
    }
    fit_window_to_buffer(win);
    return SCM_UNSPECIFIED;
}

static SCM scm_get_buffer_window(SCM buffer_or_name) {
    Buffer *buffer = NULL;

    if (scm_is_string(buffer_or_name)) {
        // It's a buffer name string
        char *name = scm_to_locale_string(buffer_or_name);
        buffer = get_buffer(name);
        free(name);
    } else {
        // Assume it's a buffer object (smob)
        buffer = scm_to_buffer(buffer_or_name);
    }

    if (!buffer) return SCM_BOOL_F;

    Window *win = get_buffer_window(buffer);
    if (!win) return SCM_BOOL_F;

    return get_or_make_window_object(win);
}

/* static SCM scm_goto_char(SCM position) { */
/*     if (!scm_is_integer(position)) { */
/*         scm_wrong_type_arg("goto-char", 1, position); */
/*     } */

/*     size_t pos = scm_to_size_t(position); */
/*     size_t result = goto_char(pos); */

/*     return scm_from_size_t(result); */
/* } */

/// Keymap

static SCM keymap_type;          // owned by Scheme — has finalizer
static SCM keymap_type_borrowed; // NOT owned by Scheme — no finalizer
static SCM keymap_object_cache;  // Hash table: KeyChordMap* -> SCM

static SCM keymap_p(SCM obj) {
    return scm_from_bool(
        SCM_IS_A_P(obj, keymap_type) ||
        SCM_IS_A_P(obj, keymap_type_borrowed)
    );
}

static SCM keymap_to_scm_owned(KeyChordMap *map) {
    // For make-sparse-keymap: Scheme owns it, finalizer will free it
    if (!map) return SCM_BOOL_F;
    SCM obj = scm_make_foreign_object_1(keymap_type, map);
    return obj;
}

SCM keymap_to_scm(KeyChordMap *map) {
    // For wrapping existing C keymaps (global map, buffer keymaps):
    // cached, no finalizer
    if (!map) return SCM_BOOL_F;

    SCM key = scm_from_uintptr_t((uintptr_t)map);
    SCM cached = scm_hashq_ref(keymap_object_cache, key, SCM_BOOL_F);
    if (scm_is_true(cached)) {
        return cached;
    }

    SCM obj = scm_make_foreign_object_1(keymap_type_borrowed, map);
    scm_hashq_set_x(keymap_object_cache, key, obj);
    return obj;
}

KeyChordMap* scm_to_keymap(SCM obj) {
    if (scm_is_false(obj)) return NULL;
    // Accept either type
    if (SCM_IS_A_P(obj, keymap_type)) {
        return scm_foreign_object_ref(obj, 0);
    }
    if (SCM_IS_A_P(obj, keymap_type_borrowed)) {
        return scm_foreign_object_ref(obj, 0);
    }
    scm_wrong_type_arg("keymap", 1, obj);
    return NULL;
}

static SCM scm_make_sparse_keymap(void) {
    KeyChordMap *map = make_sparse_keymap();
    return keymap_to_scm_owned(map);  // Scheme owns this one
}

static SCM scm_use_local_map(SCM keymap_scm) {
    KeyChordMap *map = scm_to_keymap(keymap_scm);
    use_local_map(map, current_buffer);
    return SCM_UNSPECIFIED;
}

static SCM scm_current_local_map(void) {
    KeyChordMap *map = current_local_map(current_buffer);
    return keymap_to_scm(map);  // borrowed — buffer owns it
}

static SCM scm_current_global_map(void) {
    KeyChordMap *map = current_global_map();
    return keymap_to_scm(map);  // borrowed — global map is static
}

// Keymap finalizer - called when Scheme GCs the keymap
// Finalizer ONLY runs for keymap_type (owned), never for keymap_type_borrowed
static void finalize_keymap(SCM keymap_obj) {
    KeyChordMap *map = scm_foreign_object_ref(keymap_obj, 0);
    if (map) {
        keymap_free(map);
        free(map);
    }
}

static SCM scm_define_key(SCM keymap_scm, SCM key_scm, SCM def_scm) {
    if (!scm_is_string(key_scm)) {
        scm_wrong_type_arg("define-key", 2, key_scm);
    }
    if (!scm_is_true(scm_procedure_p(def_scm))) {
        scm_wrong_type_arg("define-key", 3, def_scm);
    }

    KeyChordMap *map = scm_to_keymap(keymap_scm);
    if (!map) {
        scm_misc_error("define-key", "Invalid keymap", SCM_EOL);
    }

    char *notation = scm_to_locale_string(key_scm);
    char *description = get_scheme_proc_documentation(def_scm);

    bool result = keychord_bind_scheme(map, notation, def_scm,
                                       description, PRESS | REPEAT);

    free(notation);
    if (description) {
        free(description);
    }

    return scm_from_bool(result);
}

#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>
#include <libgen.h>

static bool file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

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
    char *init_file = NULL;
    char *glemax_dir = NULL;

    // Helper macro to set both variables and return
    #define SET_AND_RETURN(init_path) do { \
        init_file = strdup(init_path); \
        char *tmp = strdup(init_path); \
        glemax_dir = strdup(dirname(tmp)); \
        free(tmp); \
        goto define_vars; \
    } while(0)

    // Try ./init.scm first (current directory)
    snprintf(path, sizeof(path), "./init.scm");
    if (file_exists(path)) {
        char abs_path[1024];
        if (realpath(path, abs_path)) {
            SET_AND_RETURN(abs_path);
        }
    }

    const char *home = get_home_directory();
    if (!home) {
        // No home → no init file, no glemax dir
        goto define_false;
    }

    // Try ~/.glemax
    snprintf(path, sizeof(path), "%s/.glemax", home);
    if (file_exists(path)) {
        SET_AND_RETURN(path);
    }

    // Try ~/.glemax.d/init.scm
    snprintf(path, sizeof(path), "%s/.glemax.d/init.scm", home);
    if (file_exists(path)) {
        SET_AND_RETURN(path);
    }

    // Try ~/.config/glemax/init.scm
    snprintf(path, sizeof(path), "%s/.config/glemax/init.scm", home);
    if (file_exists(path)) {
        SET_AND_RETURN(path);
    }

define_false:
    // No init file found
    scm_c_define("user-init-file", SCM_BOOL_F);
    scm_c_define("user-glemax-directory", SCM_BOOL_F);
    return;

define_vars:
    // Define both variables
    scm_c_define("user-init-file", scm_from_locale_string(init_file));
    scm_c_define("user-glemax-directory", scm_from_locale_string(glemax_dir));

    // Clean up
    free(init_file);
    free(glemax_dir);
}

// Add this near the top with other global variables
static SCM load_path = SCM_UNDEFINED;


// Helper function to search for file in load-path
static char* find_file_in_load_path(const char *filename) {
    // If filename is absolute or starts with ./ or ../, use it directly
    if (filename[0] == '/' ||
        (filename[0] == '.' && filename[1] == '/') ||
        (filename[0] == '.' && filename[1] == '.' && filename[2] == '/')) {
        if (file_exists(filename)) {
            return strdup(filename);
        }
        return NULL;
    }

    // Search in load-path
    SCM path_list = load_path;
    while (scm_is_pair(path_list)) {
        SCM path_scm = scm_car(path_list);

        if (scm_is_string(path_scm)) {
            char *dir = scm_to_locale_string(path_scm);

            // Construct full path
            size_t full_path_len = strlen(dir) + strlen(filename) + 2; // +2 for '/' and '\0'
            char *full_path = malloc(full_path_len);
            snprintf(full_path, full_path_len, "%s/%s", dir, filename);

            free(dir);

            if (file_exists(full_path)) {
                return full_path;
            }

            free(full_path);
        }

        path_list = scm_cdr(path_list);
    }

    return NULL;
}

static SCM scm_load(SCM filename) {
    if (!scm_is_string(filename)) {
        scm_wrong_type_arg("load", 1, filename);
    }

    char *filename_str = scm_to_locale_string(filename);
    char *path = find_file_in_load_path(filename_str);

    if (!path) {
        char msg[1024];
        snprintf(msg, sizeof(msg), "Cannot open load file: %s", filename_str);
        free(filename_str);
        scm_misc_error("load", msg, SCM_EOL);
    }

    free(filename_str);

    // Use scm_c_primitive_load
    SCM result = scm_c_primitive_load(path);
    free(path);

    return result;
}

// Helper to add a path to load-path
static void add_to_load_path(const char *path) {
    if (access(path, F_OK) == 0) {  // Check if directory exists
        SCM path_scm = scm_from_locale_string(path);
        load_path = scm_cons(path_scm, load_path);
    }
}

// Setup load paths
static void setup_load_paths(void) {
    // Initialize load-path as empty list
    load_path = SCM_EOL;

    // Add paths in reverse priority order (last added = highest priority)

    // System installation paths
    add_to_load_path("/usr/local/share/glemax/lisp");
    add_to_load_path("/usr/local/share/glemax/etc/themes");

    // Development paths (higher priority)
    add_to_load_path("./etc/themes");
    add_to_load_path("./lisp");

    // User's home directory (highest priority)
    const char *home = getenv("HOME");
    if (home) {
        char user_lisp_path[1024];
        snprintf(user_lisp_path, sizeof(user_lisp_path), "%s/.glemax/lisp", home);
        add_to_load_path(user_lisp_path);

        snprintf(user_lisp_path, sizeof(user_lisp_path), "%s/.glemax/themes", home);
        add_to_load_path(user_lisp_path);
    }

    // Protect from garbage collection
    scm_gc_protect_object(load_path);

    // Make it available to Scheme code
    scm_c_define("load-path", load_path);
}

#include <dirent.h>

// Helper function to check if a string ends with a suffix
static bool str_ends_with(const char *str, const char *suffix) {
    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);

    if (suffix_len > str_len) {
        return false;
    }

    return strcmp(str + str_len - suffix_len, suffix) == 0;
}


// Load all .scm files from a directory
static void load_directory(const char *dir_path) {
    DIR *dir = opendir(dir_path);
    if (!dir) {
        return;  // Directory doesn't exist or can't be opened
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        // Skip . and .. and non-.scm files
        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0 ||
            strcmp(entry->d_name, "subr.scm") == 0 ||  // Skip subr.scm
            !str_ends_with(entry->d_name, ".scm")) {
            continue;
        }

        // Construct full path
        size_t full_path_len = strlen(dir_path) + strlen(entry->d_name) + 2;
        char *full_path = malloc(full_path_len);
        snprintf(full_path, full_path_len, "%s/%s", dir_path, entry->d_name);

        // Check if it's a regular file
        if (file_exists(full_path)) {
            // Load the file, catching any errors
            char load_expr[4096];
            snprintf(load_expr, sizeof(load_expr),
                "(catch #t"
                "  (lambda () (primitive-load \"%s\"))"
                "  (lambda (key . args)"
                "    (let* ((error-str"
                "             (catch #t"
                "               (lambda ()"
                "                 (cond"
                "                   ((and (pair? args) (pair? (cdr args)) (string? (cadr args)))"
                "                    (let ((fmt (cadr args))"
                "                          (params (if (and (pair? (cddr args)) (list? (caddr args)))"
                "                                      (caddr args)"
                "                                      '())))"
                "                      (apply format #f fmt params)))"
                "                   ((pair? args)"
                "                    (format #f \"~s: ~s\" key (car args)))"
                "                   (else (format #f \"~s\" key))))"
                "               (lambda _ "
                "                 (format #f \"~s ~s\" key args))))"
                "           (full-msg (string-append \"Error loading %s: \" error-str)))"
                "      (message full-msg))))",
                full_path, entry->d_name);

            scm_c_eval_string(load_expr);
        }

        free(full_path);
    }

    closedir(dir);
}

// Preload all default themes
static void preload_themes(void) {
    load_directory("./etc/themes");
}

// Preload all default major modes
static void preload_lisp(void) {
    load_directory("./lisp");
}

// Preload all default major modes
static void preload_progmodes(void) {
    load_directory("./lisp/progmodes");
}

// Scheme wrapper for load-directory
static SCM scm_load_directory(SCM dirname) {
    if (!scm_is_string(dirname)) {
        scm_wrong_type_arg("load-directory", 1, dirname);
    }

    char *dir_path = scm_to_locale_string(dirname);

    if (access(dir_path, F_OK) != 0) {
        char msg[1024];
        snprintf(msg, sizeof(msg), "Directory not found: %s", dir_path);
        free(dir_path);
        scm_misc_error("load-directory", msg, SCM_EOL);
    }

    load_directory(dir_path);
    free(dir_path);

    return SCM_UNSPECIFIED;
}

// This marks the function registered from C as Interactive
// TODO use this to register all other commands with doc
#define REGISTER_COMMAND(scheme_name, scm_func, spec)                          \
  do {                                                                         \
    scm_c_define_gsubr(scheme_name, 0, 1, 0, scm_func);                        \
    SCM _proc = scm_variable_ref(scm_c_lookup(scheme_name));                   \
    scm_set_procedure_property_x(_proc,                                        \
                                 scm_from_utf8_symbol("interactive-spec"),     \
                                 scm_from_utf8_string(spec));                  \
    if (scm_func##_doc) {                                                      \
      scm_set_procedure_property_x(_proc,                                      \
                                   scm_from_utf8_symbol("documentation"),      \
                                   scm_from_utf8_string(scm_func##_doc));      \
    }                                                                          \
  } while (0)

#define REGISTER_COMMAND_EX(scheme_name, scm_func, req, opt, rst, spec)        \
  do {                                                                         \
    scm_c_define_gsubr(scheme_name, req, opt, rst, scm_func);                  \
    SCM _proc = scm_variable_ref(scm_c_lookup(scheme_name));                   \
    scm_set_procedure_property_x(_proc,                                        \
                                 scm_from_utf8_symbol("interactive-spec"),     \
                                 scm_from_utf8_string(spec));                  \
    if (scm_func##_doc) {                                                      \
      scm_set_procedure_property_x(_proc,                                      \
                                   scm_from_utf8_symbol("documentation"),      \
                                   scm_from_utf8_string(scm_func##_doc));      \
    }                                                                          \
  } while (0)


static SCM scm_set_buffer(SCM buffer_or_name) {
    Buffer *buf = NULL;

    if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        buf = scm_to_buffer(buffer_or_name);
    } else if (scm_is_string(buffer_or_name)) {
        char *name = scm_to_locale_string(buffer_or_name);
        buf = get_buffer(name);
        free(name);

        if (!buf) {
            scm_misc_error("set-buffer", "No buffer named ~S",
                          scm_list_1(buffer_or_name));
        }
    } else {
        scm_wrong_type_arg("set-buffer", 1, buffer_or_name);
    }

    if (!buf) {
        return SCM_BOOL_F;
    }

    // Just change current_buffer (don't update windows)
    current_buffer = buf;

    return get_or_make_buffer_object(buf);
}


static void* gc_after_hook(void *hook_data, void *fn_data, void *data) {
    SCM gcs_done_var = (SCM)fn_data;
    SCM gc_times = scm_cdr(scm_assq(scm_from_utf8_symbol("gc-times"), scm_gc_stats()));
    scm_variable_set_x(gcs_done_var, gc_times);
    return hook_data;
}

static SCM scm_garbage_collect(void) {
    scm_gc();
    return scm_gc_stats();
}


#include "theme.h"
void lisp_init(void) {

    scm_c_define("gcs-done", scm_from_size_t(0));
    SCM gcs_done_var = scm_c_lookup("gcs-done");
    scm_c_hook_add(&scm_after_gc_c_hook, gc_after_hook, (void *)gcs_done_var, 0);

    setup_load_paths();

    // Initialize buffer foreign object type
    SCM name = scm_from_utf8_symbol("buffer");
    SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
    buffer_type = scm_make_foreign_object_type(name, slots, NULL);

    // Initialize buffer object cache
    buffer_object_cache = scm_make_hash_table(scm_from_int(16));
    scm_gc_protect_object(buffer_object_cache);


    // Initialize window foreign object type
    name = scm_from_utf8_symbol("window");
    slots = scm_list_1(scm_from_utf8_symbol("data"));
    window_type = scm_make_foreign_object_type(name, slots, NULL);

    // Initialize window object cache
    window_object_cache = scm_make_hash_table(scm_from_int(16));
    scm_gc_protect_object(window_object_cache);

    // Owned keymap type (Scheme called make-sparse-keymap, finalizer frees it)
    name = scm_from_utf8_symbol("keymap");
    slots = scm_list_1(scm_from_utf8_symbol("data"));
    keymap_type = scm_make_foreign_object_type(name, slots, finalize_keymap);

    // Borrowed keymap type (wraps C-owned keymaps, no finalizer)
    name = scm_from_utf8_symbol("keymap-borrowed");
    slots = scm_list_1(scm_from_utf8_symbol("data"));
    keymap_type_borrowed = scm_make_foreign_object_type(name, slots, NULL);

    // Cache for borrowed keymaps
    keymap_object_cache = scm_make_hash_table(scm_from_int(16));
    scm_gc_protect_object(keymap_object_cache);



    setup_user_init_file();

    init_glemax_bindings();
    init_undo_bindings();
    init_fileio_bindings();
    init_face_bindings();
    init_textprop_bindings();
    init_theme_bindings();
    init_wm_bindings();
    init_frame_bindings();


    init_buffer_locals();


    init_treesit_bindings();
    init_minibuf_bindings();

    char *scm_garbage_collect_doc =
        "Reclaim storage for Lisp objects no longer needed."
        "\n"
        "Garbage collection happens automatically if you cons more than"
        "`gc-cons-threshold' bytes of Lisp data since previous garbage collection."
        "garbage-collect normally returns a list with info on amount of space in use,"
        "where each entry has the form (NAME SIZE USED FREE), where:"
        "- NAME is a symbol describing the kind of objects this entry represents,"
        "- SIZE is the number of bytes used by each one,"
        "- USED is the number of those objects that were found live in the heap,"
        "- FREE is the number of those objects that are not live but that Emacs"
        "  keeps around for future allocations (maybe because it does not know how"
        "  to return them to the OS)."
        "\n"
        "Note that calling this function does not guarantee that absolutely all"
        "unreachable objects will be garbage-collected.  Emacs uses a"
        "mark-and-sweep garbage collector, but is conservative when it comes to"
        "collecting objects in some circumstances."
        "\n"
        "For further details, see Info node (elisp)Garbage Collection.";
    REGISTER_COMMAND("garbage-collect", scm_garbage_collect, "p");


    scm_c_define_gsubr("set-buffer",                    1, 0, 0, scm_set_buffer);

    char *scm_goto_char_doc =
        "Set point to POSITION, a number or marker."
        "\n"
        "Beginning of buffer is position (point-min), end is (point-max)."
        "\n"
        "The return value is POSITION."
        "\n"
        "If called interactively, a numeric prefix argument specifies"
        "POSITION; without a numeric prefix argument, read POSITION from the"
        "minibuffer.  The default value is the number at point (if any).";
    REGISTER_COMMAND("goto-char", scm_goto_char, "");


    scm_c_define_gsubr("window--try-horizontal-split",  0, 1, 0, scm_window_try_horizontal_split);
    scm_c_define_gsubr("window--try-vertical-split",    0, 1, 0, scm_window_try_vertical_split);
    scm_c_define_gsubr("split-window-sensibly",         0, 0, 0, scm_split_window_sensibly);
    scm_c_define_gsubr("display-buffer",                1, 2, 0, scm_display_buffer);


    scm_c_define_gsubr("show-cursor",                   0, 0, 0, scm_show_cursor); // Why ?
    scm_c_define_gsubr("hide-cursor",                   0, 0, 0, scm_hide_cursor); // Why ?

    // Keymap functions
    scm_c_define_gsubr("make-sparse-keymap",            0, 0, 0, scm_make_sparse_keymap);
    scm_c_define_gsubr("use-local-map",                 1, 0, 0, scm_use_local_map);
    scm_c_define_gsubr("current-local-map",             0, 0, 0, scm_current_local_map);
    scm_c_define_gsubr("current-global-map",            0, 0, 0, scm_current_global_map);
    scm_c_define_gsubr("define-key",                    3, 0, 0, scm_define_key);


    // Buffer local variables
    scm_c_define_gsubr("set-default!",                  2, 0, 0, scm_set_default);
    scm_c_define_gsubr("setq-default",                  2, 0, 0, scm_set_default);
    scm_c_define_gsubr("default-value",                 1, 0, 0, scm_default_value);
    scm_c_define_gsubr("buffer-local-value",            1, 1, 0, scm_buffer_local_value);


    scm_c_define_gsubr("set",                           2, 0, 0, scm_set);
    scm_c_define_gsubr("setq-impl",                     2, 0, 0, scm_setq_impl);

    scm_c_define_gsubr("local-variable?",               1, 1, 0, scm_local_variable_p);
    scm_c_define_gsubr("local-variable-if-set?",        1, 1, 0, scm_local_variable_if_set_p);
    scm_c_define_gsubr("kill-local-variable",           1, 0, 0, scm_kill_local_variable);
    scm_c_define_gsubr("kill-all-local-variables",      0, 0, 0, scm_kill_all_local_variables);
    scm_c_define_gsubr("buffer-local-variables",        0, 1, 0, scm_buffer_local_variables);
    scm_c_define_gsubr("make-local-variable",           1, 0, 0, scm_make_local_variable);
    scm_c_define_gsubr("make-variable-buffer-local",    1, 0, 0, scm_make_variable_buffer_local);
    scm_c_define_gsubr("automatically-buffer-local?",   1, 0, 0, scm_automatically_buffer_local_p);

    // Set up common buffer-local defaults
    set_default(scm_from_utf8_symbol("fill-column"),    scm_from_int(70));

    mark_automatically_buffer_local(scm_from_utf8_symbol("fill-column"));


    scm_c_define_gsubr("load",                           1, 0, 0, scm_load);


    // Eval
    char *scm_eval_last_sexp_doc =
        "Evaluate sexp before point; print value in the echo area."
        "Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument."
        "With a non `-' prefix argument, print output into current buffer."
        "\n"
        "Normally, this function truncates long output according to the"
        "value of the variables `eval-expression-print-length' and"
        "`eval-expression-print-level'.  With a prefix argument of zero,"
        "however, there is no such truncation."
        "Integer values are printed in several formats (decimal, octal,"
        "and hexadecimal).  When the prefix argument is -1 or the value"
        "doesn't exceed `eval-expression-print-maximum-character', an"
        "integer value is also printed as a character of that codepoint."
        "\n"
        "If `eval-expression-debug-on-error' is non-nil, which is the default,"
        "this command arranges for all errors to enter the debugger.";
    REGISTER_COMMAND("eval-last-sexp", scm_eval_last_sexp, "p");

    char *scm_eval_buffer_doc =
        "Execute the accessible portion of current buffer as Lisp code."
        "You can use \\[narrow-to-region] to limit the part of buffer to be evaluated."
        "When called from a Lisp program (i.e., not interactively), this"
        "function accepts up to five optional arguments:"
        "BUFFER is the buffer to evaluate (nil means use current buffer),"
        " or a name of a buffer (a string)."
        "PRINTFLAG controls printing of output by any output functions in the"
        " evaluated code, such as `print', `princ', and `prin1':"
        "  a value of nil means discard it; anything else is the stream to print to."
        "  See Info node `(elisp)Output Streams' for details on streams."
        "FILENAME specifies the file name to use for `load-history'."
        "UNIBYTE is obsolete and ignored."
        "DO-ALLOW-PRINT, if non-nil, specifies that output functions in the"
        " evaluated code should work normally even if PRINTFLAG is nil, in"
        " which case the output is displayed in the echo area."
        "\n"
        "This function preserves the position of point.";
    REGISTER_COMMAND("eval-buffer", scm_eval_buffer, "p");

    char *scm_eval_region_doc =
        "Execute the region as Lisp code."
        "When called from programs, expects two arguments,"
        "giving starting and ending indices in the current buffer"
        "of the text to be executed."
        "Programs can pass third argument PRINTFLAG which controls output:"
        " a value of nil means discard it; anything else is stream for printing it."
        " See Info node `(elisp)Output Streams' for details on streams."
        "Also the fourth argument READ-FUNCTION, if non-nil, is used"
        "instead of `read' to read each expression.  It gets one argument"
        "which is the input stream for reading characters."
        "\n"
        "This function does not move point.";
    REGISTER_COMMAND("eval-region", scm_eval_region, "p");

    // Arg
    char *scm_universal_argument_doc =
        "Begin a numeric argument for the following command."
        "Digits or minus sign following \\[universal-argument] make up the numeric argument."
        "\\[universal-argument] following the digits or minus sign ends the argument."
        "\\[universal-argument] without digits or minus sign provides 4 as argument."
        "Repeating \\[universal-argument] without digits or minus sign"
        " multiplies the argument by 4 each time."
        "For some commands, just \\[universal-argument] by itself serves as a flag"
        "that is different in effect from any particular numeric argument."
        "These commands include \\[set-mark-command] and \\[start-kbd-macro].";
    REGISTER_COMMAND("universal-argument", scm_universal_argument, "p");

    char *scm_negative_argument_doc =
        "Begin a negative numeric argument for the next command."
        "\\[universal-argument] following digits or minus sign ends the argument.";
    REGISTER_COMMAND("negative-argument", scm_negative_argument, "p");

    char *scm_digit_argument_doc =
        "Part of the numeric argument for the next command."""
        "\\[universal-argument] following digits or minus sign ends the argument.";
    REGISTER_COMMAND("digit-argument", scm_digit_argument, "p");

    init_interactive_system();
    scm_c_define_gsubr("interactive",                   1, 0, 0, scm_interactive);
    scm_c_define_gsubr("call-interactively",            1, 0, 0, scm_call_interactively);
    scm_c_define_gsubr("interactive-form",              1, 0, 0, scm_interactive_form);
    scm_c_define_gsubr("commandp",                      1, 0, 0, scm_commandp);

    // Buffer
    scm_c_define_gsubr("buffer?",                        1, 0, 0, buffer_p);

    char *scm_switch_to_buffer_doc =
        "Display buffer BUFFER-OR-NAME in the selected window."
        "\n"
        "WARNING: This is NOT the way to work on another buffer temporarily"
        "within a Lisp program!  Use `set-buffer' instead.  That avoids"
        "messing with the `window-buffer' correspondences."
        "\n"
        "If the selected window cannot display the specified buffer"
        "because it is a minibuffer window or strongly dedicated to"
        "another buffer, call `pop-to-buffer' to select the buffer in"
        "another window.  In interactive use, if the selected window is"
        "strongly dedicated to its buffer, the value of the option"
        "`switch-to-buffer-in-dedicated-window' specifies how to proceed."
        "\n"
        "If called interactively, read the buffer name using `read-buffer'."
        "The variable `confirm-nonexistent-file-or-buffer' determines"
        "whether to request confirmation before creating a new buffer."
        "See `read-buffer' for features related to input and completion"
        "of buffer names."
        "\n"
        "BUFFER-OR-NAME may be a buffer, a string (a buffer name), or nil."
        "If BUFFER-OR-NAME is a string that does not identify an existing"
        "buffer, create a buffer with that name.  If BUFFER-OR-NAME is"
        "nil, switch to the buffer returned by `other-buffer'."
        "\n"
        "If optional argument NORECORD is non-nil, do not put the buffer"
        "at the front of the buffer list, and do not make the window"
        "displaying it the most recently selected one."
        "\n"
        "If optional argument FORCE-SAME-WINDOW is non-nil, the buffer"
        "must be displayed in the selected window when called"
        "non-interactively; if that is impossible, signal an error rather"
        "than calling `pop-to-buffer'.  It has no effect when the option"
        "`switch-to-buffer-obey-display-actions' is non-nil."
        "\n"
        "The option `switch-to-buffer-preserve-window-point' can be used"
        "to make the buffer appear at its last position in the selected"
        "window."
        "\n"
        "If the option `switch-to-buffer-obey-display-actions' is non-nil,"
        "run the function `pop-to-buffer-same-window' instead."
        "This may display the buffer in another window as specified by"
        "`display-buffer-overriding-action', `display-buffer-alist' and"
        "other display related variables.  If this results in displaying"
        "the buffer in the selected window, window start and point are adjusted"
        "as prescribed by the option `switch-to-buffer-preserve-window-point'."
        "Otherwise, these are left alone."
        "\n"
        "In either case, call `display-buffer-record-window' to avoid disrupting"
        "a sequence of `display-buffer' operations using this window."
        "\n"
        "Return the buffer switched to.";
    REGISTER_COMMAND("switch-to-buffer", scm_switch_to_buffer, "B");

    char *scm_kill_buffer_doc =
        "Kill the buffer specified by BUFFER-OR-NAME."
        "The argument may be a buffer or the name of an existing buffer."
        "Argument nil or omitted means kill the current buffer.  Return t if the"
        "buffer is actually killed, nil otherwise."
        "\n"
        "The functions in `kill-buffer-query-functions' are called with the"
        "buffer to be killed as the current buffer.  If any of them returns nil,"
        "the buffer is not killed. TODO The hook `kill-buffer-hook' is run before the"
        "buffer is actually killed.  The buffer being killed will be current"
        "while the hook is running.  Functions called by any of these hooks are"
        "supposed to not change the current buffer.  Neither hook is run for"
        "internal or temporary buffers created by `get-buffer-create' or"
        "`generate-new-buffer' with argument INHIBIT-BUFFER-HOOKS non-nil."
        "\n"
        "Any processes that have this buffer as the `process-buffer' are killed"
        "with SIGHUP.  This function calls `replace-buffer-in-windows' for"
        "cleaning up all windows currently displaying the buffer to be killed.";
    REGISTER_COMMAND("kill-buffer", scm_kill_buffer, "b");

    char *scm_kill_current_buffer_doc =
        "Kill the current buffer."
        "When called in the minibuffer, get out of the minibuffer"
        "using `abort-recursive-edit'."
        "\n"
        "This is like `kill-this-buffer', but it doesn't have to be invoked"
        "via the menu bar, and pays no attention to the menu-bar's frame.";
    REGISTER_COMMAND("kill-current-buffer", scm_kill_current_buffer, "");

    scm_c_define_gsubr("buffer-name",                    0, 1, 0, scm_buffer_name);
    scm_c_define_gsubr("buffer-list",                    0, 0, 0, scm_buffer_list);
    scm_c_define_gsubr("other-buffer",                   0, 0, 0, scm_other_buffer);
    scm_c_define_gsubr("get-buffer",                     1, 0, 0, scm_get_buffer);
    scm_c_define_gsubr("get-buffer-create",              1, 0, 0, scm_get_buffer_create);
    scm_c_define_gsubr("current-buffer",                 0, 0, 0, scm_current_buffer);

    char *scm_next_buffer_doc =
        "In selected window switch to ARGth next buffer."
        "Call `switch-to-next-buffer' unless the selected window is the"
        "minibuffer window or is dedicated to its buffer.";
    REGISTER_COMMAND("next-buffer", scm_next_buffer, "");

    char *scm_previous_buffer_doc =
        "In selected window switch to ARGth previous buffer."
        "Call `switch-to-prev-buffer' unless the selected window is the"
        "minibuffer window or is dedicated to its buffer.";
    REGISTER_COMMAND("previous-buffer", scm_previous_buffer, "");

    char *scm_append_to_buffer_doc =
        "Append to specified BUFFER the text of the region."
        "The text is inserted into that buffer before its point."
        "BUFFER can be a buffer or the name of a buffer; this"
        "function will create BUFFER if it doesn't already exist."
        "\n"
        "When calling from a program, give three arguments:"
        "BUFFER (or buffer name), START and END."
        "START and END specify the portion of the current buffer to be copied.";
    REGISTER_COMMAND_EX("append-to-buffer", scm_append_to_buffer, 1, 2, 0, "BAppend to buffer: \nr");

    scm_c_define_gsubr("buffer-file-name",               0, 1, 0, scm_buffer_file_name);

    // Minor modes
    scm_c_define_gsubr("enable-minor-mode",             1, 1, 0, scm_enable_minor_mode);
    scm_c_define_gsubr("disable-minor-mode",            1, 1, 0, scm_disable_minor_mode);
    scm_c_define_gsubr("minor-mode-active?",           1, 1, 0, scm_minor_mode_active_p);

    // Window
    scm_c_define_gsubr("window?",                        1, 0, 0, window_p);
    scm_c_define_gsubr("selected-window",                0, 0, 0, scm_selected_window);
    scm_c_define_gsubr("minibuffer-window",              0, 0, 0, scm_minibuffer_window);
    scm_c_define_gsubr("window-buffer",                  0, 1, 0, scm_window_buffer);
    scm_c_define_gsubr("window-point",                   0, 1, 0, scm_window_point);
    scm_c_define_gsubr("set-window-point",               2, 0, 0, scm_set_window_point);
    scm_c_define_gsubr("window-list",                    0, 0, 0, scm_window_list);

    scm_c_define_gsubr("next-window",                    0, 2, 0, scm_next_window);
    scm_c_define_gsubr("previous-window",                0, 2, 0, scm_previous_window);
    scm_c_define_gsubr("select-window",                  1, 0, 0, scm_select_window);
    scm_c_define_gsubr("window-pixel-width",             0, 1, 0, scm_window_pixel_width);
    scm_c_define_gsubr("window-pixel-height",            0, 1, 0, scm_window_pixel_height);
    scm_c_define_gsubr("window-min-pixel-width",         0, 1, 0, scm_window_min_pixel_width);
    scm_c_define_gsubr("window-min-pixel-height",        0, 1, 0, scm_window_min_pixel_height);

    scm_c_define_gsubr("minibuffer-window?",             0, 1, 0, scm_minibuffer_window_p);
    scm_c_define_gsubr("set-window-buffer",              2, 0, 0, scm_set_window_buffer);

    char *scm_fit_window_to_buffer_doc =
        "Adjust size of WINDOW to display its buffer's contents exactly."
        "WINDOW must be a live window and defaults to the selected one."
        "\n"
        "If WINDOW is part of a vertical combination, adjust WINDOW's"
        "height.  The new height is calculated from the actual height of"
        "the accessible portion of its buffer.  The optional argument"
        "MAX-HEIGHT specifies a maximum height and defaults to the height"
        "of WINDOW's frame.  The optional argument MIN-HEIGHT specifies a"
        "minimum height and defaults to `window-min-height'.  Both"
        "MAX-HEIGHT and MIN-HEIGHT are specified in lines and include mode"
        "and header line and a bottom divider, if any."
        "\n"
        "If WINDOW is part of a horizontal combination and the value of"
        "the option `fit-window-to-buffer-horizontally' is non-nil, adjust"
        "WINDOW's width.  The new width of WINDOW is calculated from the"
        "maximum length of its buffer's lines that follow the current"
        "start position of WINDOW.  The optional argument MAX-WIDTH"
        "specifies a maximum width and defaults to the width of WINDOW's"
        "frame.  The optional argument MIN-WIDTH specifies a minimum width"
        "and defaults to `window-min-width'.  Both MAX-WIDTH and MIN-WIDTH"
        "are specified in columns and include fringes, margins, a"
        "scrollbar and a vertical divider, if any."
        "\n"
        "Optional argument PRESERVE-SIZE non-nil means to preserve the"
        "size of WINDOW (see `window-preserve-size')."
        "\n"
        "Fit pixelwise if the option `window-resize-pixelwise' is non-nil."
        "If WINDOW is its frame's root window and the option"
        "`fit-frame-to-buffer' is non-nil, call `fit-frame-to-buffer' to"
        "adjust the frame's size."
        "\n"
        "Note that even if this function makes WINDOW large enough to show"
        "_all_ parts of its buffer you might not see the first part when"
        "WINDOW was scrolled.  If WINDOW is resized horizontally, you will"
        "not see the top of its buffer unless WINDOW starts at its minimum"
        "accessible position.";
    REGISTER_COMMAND("fit-window-to-buffer", scm_fit_window_to_buffer, "");

    scm_c_define_gsubr("get-buffer-window",              1, 0, 0, scm_get_buffer_window);

    // Keymap
    scm_c_define_gsubr("keymap?",                        1, 0, 0, keymap_p);

    char *scm_keymap_global_set_doc =
        "Give KEY a global binding as COMMAND."
        "When called interactively, KEY is a key sequence.  When called from"
        "Lisp, KEY is a string that must satisfy `key-valid-p'."
        "\n"
        "COMMAND is the command definition to use.  When called interactively,"
        "this function prompts for COMMAND and accepts only names of known"
        "commands, i.e., symbols that satisfy the `commandp' predicate.  When"
        "called from Lisp, COMMAND can be anything that `keymap-set' accepts"
        "as its DEFINITION argument."
        "\n"
        "If COMMAND is a string (which can only happen when this function is"
        "called from Lisp), it must satisfy `key-valid-p'."
        ""
        "The `key-description' convenience function converts a simple"
        "string of characters to an equivalent form that is acceptable for"
        "COMMAND."
        "\n"
        "Note that if KEY has a local binding in the current buffer,"
        "that local binding will continue to shadow any global binding"
        "that you make with this function.";
    REGISTER_COMMAND("keymap-global-set", scm_keymap_global_set, "");

    scm_c_define_gsubr("keymap-global-set",              2, 0, 0, scm_keymap_global_set);
    scm_c_define_gsubr("keymap-global-unset",            1, 0, 0, scm_keymap_global_unset);
    scm_c_define_gsubr("keychord-documentation",         1, 0, 0, scm_keychord_documentation);
    scm_c_define_gsubr("keychord-bindings",              0, 0, 0, scm_keychord_bindings);

    // Movement
    REGISTER_COMMAND("forward-char",          scm_forward_char,          "p");
    REGISTER_COMMAND("backward-char",         scm_backward_char,         "p");
    REGISTER_COMMAND("line-move",             scm_line_move,             "p");
    REGISTER_COMMAND("line-move-logical",     scm_line_move_logical,     "p");
    REGISTER_COMMAND("line-move-visual",      scm_line_move_visual,      "p");
    REGISTER_COMMAND("next-line",             scm_next_line,             "p");
    REGISTER_COMMAND("previous-line",         scm_previous_line,         "p");
    REGISTER_COMMAND("next-logical-line",     scm_next_logical_line,     "p");
    REGISTER_COMMAND("previous-logical-line", scm_previous_logical_line, "p");

    char *scm_forward_word_doc =
        "Move point forward ARG words (backward if ARG is negative)."
        "If ARG is omitted or nil, move point forward one word."
        "Normally returns t."
        "If an edge of the buffer or a field boundary is reached, point is"
        "left there and the function returns nil.  Field boundaries are not"
        "noticed if `inhibit-field-text-motion' is non-nil."
        "\n"
        "The word boundaries are normally determined by the buffer's syntax"
        "table and character script (according to `char-script-table'), but"
        "`find-word-boundary-function-table', such as set up by `subword-mode',"
        "can change that.  If a Lisp program needs to move by words determined"
        "strictly by the syntax table, it should use `forward-word-strictly'"
        "instead.  See Info node `(elisp) Word Motion' for details.";
    REGISTER_COMMAND("forward-word", scm_forward_word, "p");

    char *scm_backward_word_doc =
        "Move backward until encountering the beginning of a word."
        "With argument ARG, do this that many times."
        "If ARG is omitted or nil, move point backward one word."
        "\n"
        "The word boundaries are normally determined by the buffer's"
        "syntax table and character script (according to"
        "`char-script-table'), but `find-word-boundary-function-table',"
        "such as set up by `subword-mode', can change that.  If a Lisp"
        "program needs to move by words determined strictly by the syntax"
        "table, it should use `backward-word-strictly' instead.  See Info"
        "node `(elisp) Word Motion' for details.";
    REGISTER_COMMAND("backward-word", scm_backward_word, "p");

    char *scm_forward_paragraph_doc =
        "Move forward to end of paragraph."
        "With argument ARG, do it ARG times;"
        "a negative argument ARG = -N means move backward N paragraphs."
        "\n"
        "A line which `paragraph-start' matches either separates paragraphs"
        "\(if `paragraph-separate' matches it also) or is the first line of a paragraph."
        "A paragraph end is the beginning of a line which is not part of the paragraph"
        "to which the end of the previous line belongs, or the end of the buffer."
        "Returns the count of paragraphs left to move.";
    REGISTER_COMMAND("forward-paragraph", scm_forward_paragraph, "p");

    char *scm_backward_paragraph_doc =
        "Move backward to start of paragraph."
        "With argument ARG, do it ARG times;"
        "a negative argument ARG = -N means move forward N paragraphs."
        "\n"
        "A paragraph start is the beginning of a line which is a"
        "`paragraph-start' or which is ordinary text and follows a"
        "`paragraph-separate'ing line; except: if the first real line of a"
        "paragraph is preceded by a blank line, the paragraph starts at that"
        "blank line."
        "\n"
        "See `forward-paragraph' for more information.";
    REGISTER_COMMAND("backward-paragraph", scm_backward_paragraph, "p");

    REGISTER_COMMAND("forward-page",             scm_forward_page,     "p");
    REGISTER_COMMAND("backward-page",            scm_backward_page,    "p");
    REGISTER_COMMAND("mark-page",                scm_mark_page,        "p");
    REGISTER_COMMAND("count-lines-page",         scm_count_lines_page, "p");


    REGISTER_COMMAND("beginning-of-line",        scm_beginning_of_line,        "p");
    REGISTER_COMMAND("end-of-line",              scm_end_of_line,              "p");
    REGISTER_COMMAND("beginning-of-visual-line", scm_beginning_of_visual_line, "p");
    REGISTER_COMMAND("end-of-visual-line",       scm_end_of_visual_line,       "p");


    char *scm_end_of_buffer_doc =
        "Move point to the end of the buffer."
        "With numeric arg N, put point N/10 of the way from the end."
        "If the buffer is narrowed, this command uses the end of the"
        "accessible part of the buffer."
        "\n"
        "Push mark at previous position, unless either a \\[universal-argument] prefix"
        "is supplied, or Transient Mark mode is enabled and the mark is active.";
    REGISTER_COMMAND("end-of-buffer", scm_end_of_buffer, "p");

    char *scm_beginning_of_buffer_doc =
        "Move point to the beginning of the buffer."
        "With numeric arg N, put point N/10 of the way from the beginning."
        "If the buffer is narrowed, this command uses the beginning of the"
        "accessible part of the buffer."
        "\n"
        "Push mark at previous position, unless either a \\[universal-argument] prefix"
        "is supplied, or Transient Mark mode is enabled and the mark is active.";
    REGISTER_COMMAND("beginning-of-buffer", scm_beginning_of_buffer, "p");


    // Editing
    scm_c_define_gsubr("char-or-string?",                1, 0, 0, scm_char_or_string_p);
    scm_c_define_gsubr("insert",                         0, 0, 1, scm_insert);
    REGISTER_COMMAND("self-insert-command",     scm_self_insert_command, "p");

    char *scm_quoted_insert_doc =
        "Read next input character and insert it."
        "This is useful for inserting control characters."
        "With argument, insert ARG copies of the character."
        "\n"
        "If the first character you type is an octal digit, the sequence of"
        "one or more octal digits you type is interpreted to specify a"
        "character code.  Any character that is not an octal digit terminates"
        "the sequence.  If the terminator is a RET, it is discarded; any"
        "other terminator is used itself as input and is inserted."
        "\n"
        "The variable `read-quoted-char-radix' specifies the radix for this feature;"
        "set it to 10 or 16 to use decimal or hex instead of octal.  If you change"
        "the radix, the characters interpreted as specifying a character code"
        "change accordingly: 0 to 9 for decimal, 0 to F for hex."
        "\n"
        "this function inserts the character anyway, and"
        "does not handle octal (or decimal or hex) digits specially.  This means"
        "that if you use overwrite mode as your normal editing mode, you can use"
        "this function to insert characters when necessary."
        "\n"
        "In binary overwrite mode, this function does overwrite, and octal"
        "\(or decimal or hex) digits are interpreted as a character code.  This"
        "is intended to be useful for editing binary files.";
    REGISTER_COMMAND("quoted-insert", scm_quoted_insert, "p");

    char *scm_delete_backward_char_doc =
        "Delete the previous N characters (following if N is negative)."
        "If Transient Mark mode is enabled, the mark is active, and N is 1,"
        "delete the text in the region and deactivate the mark instead."
        "To disable this, set option `delete-active-region' to nil."
        "\n"
        "Optional second arg KILLFLAG, if non-nil, means to kill (save in"
        "kill ring) instead of delete.  If called interactively, a numeric"
        "prefix argument specifies N, and KILLFLAG is also set if a prefix"
        "argument is used."
        "\n"
        "When killing, the killed text is filtered by"
        "`filter-buffer-substring' before it is saved in the kill ring, so"
        "the actual saved text might be different from what was killed."
        "\n"
        "In Overwrite mode, single character backward deletion may replace"
        "tabs with spaces so as to back over columns, unless point is at"
        "the end of the line.";
    REGISTER_COMMAND("delete-backward-char", scm_delete_backward_char, "p");

    char *scm_delete_char_doc =
        "Delete the following N characters (previous if N is negative)."
        "Optional second arg KILLFLAG non-nil means kill instead (save in kill ring)."
        "Interactively, N is the prefix arg, and KILLFLAG is set if"
        "N was explicitly specified."
        "\n"
        "The command `delete-forward-char' is preferable for interactive use, e.g."
        "because it respects values of `delete-active-region' and `overwrite-mode'.";
    REGISTER_COMMAND("delete-char", scm_delete_char, "p");

    char *scm_delete_blank_lines_doc =
        "On blank line, delete all surrounding blank lines, leaving just one."
        "On isolated blank line, delete that one."
        "On nonblank line, delete any immediately following blank lines.";
    REGISTER_COMMAND("delete-blank-lines", scm_delete_blank_lines, "p");

    REGISTER_COMMAND("delete-indentation", scm_delete_indentation, "p");

    char *scm_back_to_indentation_doc =
        "Move point to the first non-whitespace character on this line.";
    REGISTER_COMMAND("back-to-indentation", scm_back_to_indentation, "p");
    REGISTER_COMMAND("read-only-mode",      scm_read_only_mode,      "p");
    REGISTER_COMMAND("save-buffer",         scm_save_buffer,         "p");
    REGISTER_COMMAND("newline",             my_scm_newline,          "p");
    REGISTER_COMMAND("beginning-of-defun",  scm_beginning_of_defun,  "p");
    REGISTER_COMMAND("end-of-defun",        scm_end_of_defun,        "p");
    REGISTER_COMMAND("open-line",           scm_open_line,           "p");
    REGISTER_COMMAND("split-line",          scm_split_line,          "p");
    REGISTER_COMMAND("capitalize-word",     scm_capitalize_word,     "p");
    REGISTER_COMMAND("downcase-word",       scm_downcase_word,       "p");
    REGISTER_COMMAND("upcase-word",         scm_upcase_word,         "p");

    // Transpose
    REGISTER_COMMAND("transpose-chars", scm_transpose_chars, "p");
    REGISTER_COMMAND("transpose-words", scm_transpose_words, "p");

    // List
    REGISTER_COMMAND("forward-list",  scm_forward_list,  "p");
    REGISTER_COMMAND("backward-list", scm_backward_list, "p");

    // Sexps

    REGISTER_COMMAND("forward-sexp",  scm_forward_sexp,  "p");
    REGISTER_COMMAND("backward-sexp", scm_backward_sexp, "p");
    REGISTER_COMMAND("kill-sexp",     scm_kill_sexp,     "p");
    REGISTER_COMMAND("mark-sexp",     scm_mark_sexp,     "p");


    // Kill/yank
    REGISTER_COMMAND("kill-line",           scm_kill_line,           "p");
    REGISTER_COMMAND("kill-word",           scm_kill_word,           "p");
    REGISTER_COMMAND("kill-region",         scm_kill_region,         "p");
    REGISTER_COMMAND("copy-region-as-kill", scm_copy_region_as_kill, "p");
    REGISTER_COMMAND("yank",                scm_yank,                "p");
    REGISTER_COMMAND("duplicate-line",      scm_duplicate_line,      "p");
    REGISTER_COMMAND("duplicate-region",    scm_duplicate_region,    "p");
    REGISTER_COMMAND("duplicate-dwim",      scm_duplicate_dwim,      "p");
    REGISTER_COMMAND("backward-kill-word",  scm_backward_kill_word,  "p");

    // Region
    REGISTER_COMMAND("set-mark-command",  scm_set_mark_command,  "p");
    scm_c_define_gsubr("set-mark",                       1, 0, 0, scm_set_mark);

    char *scm_exchange_point_and_mark_doc =
        "Put the mark where point is now, and point where the mark is now."
        "This command works even when the mark is not active, and it reactivates"
        "the mark unless `exchange-point-and-mark-highlight-region' is nil."
        "\n"
        "If Transient Mark mode is on, a prefix ARG deactivates the mark if it is"
        "active, and otherwise avoids reactivating it.  However, if"
        "`exchange-point-and-mark-highlight-region' is nil, then using a prefix"
        "argument does reactivate the mark; effectively, when Transient Mark mode"
        "is on, setting `exchange-point-and-mark-highlight-region' to nil swaps"
        "the meanings of the presence and absence of a prefix argument."
        "\n"
        "If Transient Mark mode is off, a prefix ARG enables Transient Mark mode"
        "temporarily.";
    REGISTER_COMMAND("exchange-point-and-mark",  scm_exchange_point_and_mark,  "p");

    char *scm_delete_region_doc =
        "Delete the text between START and END."
        "If called interactively, delete the region between point and mark."
        "This command deletes buffer text without modifying the kill ring.";
    REGISTER_COMMAND("delete-region",  scm_delete_region,  "p");

    REGISTER_COMMAND("exit-recursive-edit",  scm_exit_recursive_edit,  "p");

    // TODO Continue making stuff interactive from HERE

    scm_c_define_gsubr("activate-mark",                  0, 0, 0, scm_activate_mark);
    scm_c_define_gsubr("deactivate-mark",                0, 0, 0, scm_deactivate_mark);

    // Windows
    scm_c_define_gsubr("split-window-below",             0, 1, 0, scm_split_window_below);
    scm_c_define_gsubr("split-window-right",             0, 1, 0, scm_split_window_right);
    scm_c_define_gsubr("delete-window",                  0, 1, 0, scm_delete_window);
    scm_c_define_gsubr("delete-other-windows",           0, 1, 0, scm_delete_other_windows);
    scm_c_define_gsubr("other-window",                   0, 1, 0, scm_other_window);
    scm_c_define_gsubr("balance-windows",                0, 1, 0, scm_balance_windows);
    scm_c_define_gsubr("enlarge-window",                 0, 1, 0, scm_enlarge_window);
    scm_c_define_gsubr("recenter",                       0, 1, 0, scm_recenter);
    scm_c_define_gsubr("recenter-top-bottom",            0, 1, 0, scm_recenter_top_bottom);
    scm_c_define_gsubr("scroll-down-command",            0, 1, 0, scm_scroll_down_command);
    scm_c_define_gsubr("scroll-up-command",              0, 1, 0, scm_scroll_up_command);
    scm_c_define_gsubr("scroll-other-window",            0, 1, 0, scm_scroll_other_window);
    scm_c_define_gsubr("scroll-other-window-down",       0, 1, 0, scm_scroll_other_window_down);
    scm_c_define_gsubr("move-to-window-line",            0, 1, 0, scm_move_to_window_line);
    scm_c_define_gsubr("move-to-window-line-top-bottom", 0, 1, 0, scm_move_to_window_line_top_bottom);

    // Query functions
    scm_c_define_gsubr("point",                          0, 0, 0, scm_point);
    scm_c_define_gsubr("mark",                           0, 0, 0, scm_mark);
    scm_c_define_gsubr("mark-active?",                   0, 0, 0, scm_mark_active_p);
    /* scm_c_define_gsubr("buffer-size",                    0, 0, 0, scm_buffer_size); */
    scm_c_define_gsubr("buffer-size",                    0, 1, 0, scm_buffer_size);
    scm_c_define_gsubr("current-column",                 0, 0, 0, scm_current_column);
    scm_c_define_gsubr("line-beginning-position",        0, 1, 0, scm_line_beginning_position);
    scm_c_define_gsubr("line-end-position",              0, 1, 0, scm_line_end_position);

    // Buffer content
    scm_c_define_gsubr("buffer-substring",               2, 0, 0, scm_buffer_substring);
    scm_c_define_gsubr("char-after",                     0, 1, 0, scm_char_after);
    scm_c_define_gsubr("char-before",                    0, 1, 0, scm_char_before);

    // Message
    scm_c_define_gsubr("message",                        1, 0, 1, scm_message);

    // Theme
    char *scm_load_theme_doc =
        "Load Custom theme named THEME from its file and possibly enable it."
        "The theme file is named THEME-theme.el, in one of the directories"
        "specified by `custom-theme-load-path'."
        "\n"
        "Normally, this function also enables THEME.  If optional arg"
        "NO-ENABLE is non-nil, load the theme but don't enable it, unless"
        "the theme was already enabled."
        "\n"
        "Note that enabling THEME does not disable any other"
        "already-enabled themes.  If THEME is enabled, it has the highest"
        "precedence (after `user') among enabled themes.  To disable other"
        "themes, use `disable-theme'."
        "\n"
        "This function is normally called through Customize when setting"
        "`custom-enabled-themes'.  If used directly in your init file, it"
        "should be called with a non-nil NO-CONFIRM argument, or after"
        "`custom-safe-themes' has been loaded."
        "\n"
        "Return t if THEME was successfully loaded, nil otherwise.";
    REGISTER_COMMAND("load-theme", scm_load_theme, "");

    char *scm_disable_theme_doc =
        "Disable all variable and face settings defined by THEME."
        "See `custom-enabled-themes' for a list of enabled themes."
        "\n"
        "After THEME has been disabled, runs `disable-theme-functions'.";
    REGISTER_COMMAND("disable-theme", scm_disable_theme, "");

    REGISTER_COMMAND("describe-key-briefly", scm_describe_key_briefly, "");



    // NOTE: Load core macros FIRST - this must happen before any other .scm files
    scm_c_eval_string(
        "(catch #t"
        "  (lambda () (primitive-load \"./lisp/subr.scm\"))"
        "  (lambda (key . args)"
        "    (display \"Error loading subr.scm: \") (display key) (display \" \") (display args) (newline)))"
    );


    // NOTE Preload everything BEFORE loading user init file
    preload_themes();
    preload_lisp();
    preload_progmodes();

    // NOTE Eval init.scm after defining subroutine

    scm_c_eval_string(
        "(when (and user-init-file (string? user-init-file))"
        "  (catch #t"
        "    (lambda () (load user-init-file))"
        "    (lambda (key . args)"
        "      (let* ((port (open-output-string))"
        "             (subr (and (pair? args) (car args)))"
        "             (msg-template (and (pair? args) (pair? (cdr args)) (cadr args)))"
        "             (msg-args (and (pair? args) (pair? (cdr args)) (pair? (cddr args)) (caddr args))))"
        "        (display \"Error loading \" port)"
        "        (display user-init-file port)"
        "        (display \": \" port)"
        "        (display key port)"
        "        (when msg-template"
        "          (display \" - \" port)"
        "          (if (and msg-args (not (null? msg-args)))"
        "              (simple-format port msg-template msg-args)"
        "              (display msg-template port)))"
        "        (message \"~a\" (get-output-string port))))))"
    );
}

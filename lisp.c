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

void eval_last_sexp() {
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
    
    for (size_t i = 0; i < len; i++) {
        expr[i] = (char)rope_char_at(current_buffer->rope, start + i);
    }
    expr[len] = '\0';
    
    bool had_error = false;
    SCM result = safe_eval_string(expr, &had_error);
    display_eval_result(result, had_error);
    
    free(expr);
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


// NOTE This is our way to do (interactive) so later M-x will have access
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

DEFINE_SCM_COMMAND(scm_read_only_mode, read_only_mode,
"Change whether the current buffer is read-only.");


// TODO Add Docstring for all those commands...

DEFINE_SCM_COMMAND(scm_save_buffer,                    save_buffer,                    NULL);

DEFINE_SCM_COMMAND(scm_set_mark_command,               set_mark_command,               NULL);
DEFINE_SCM_COMMAND(scm_delete_indentation,             delete_indentation,             NULL);
DEFINE_SCM_COMMAND(scm_delete_char,                    delete_char,                    NULL);
DEFINE_SCM_COMMAND(scm_delete_backward_char,           delete_backward_char,           NULL);
DEFINE_SCM_COMMAND(scm_capitalize_word,                capitalize_word,                NULL);
DEFINE_SCM_COMMAND(scm_downcase_word,                  downcase_word,                  NULL);
DEFINE_SCM_COMMAND(scm_upcase_word,                    upcase_word,                    NULL);
DEFINE_SCM_COMMAND(scm_transpose_chars,                transpose_chars,                NULL);
DEFINE_SCM_COMMAND(scm_transpose_words,                transpose_words,                NULL);
DEFINE_SCM_COMMAND(scm_forward_list,                   forward_list,                   NULL);
DEFINE_SCM_COMMAND(scm_backward_list,                  backward_list,                  NULL);
DEFINE_SCM_COMMAND(scm_forward_sexp,                   forward_sexp,                   NULL);
DEFINE_SCM_COMMAND(scm_backward_sexp,                  backward_sexp,                  NULL);
DEFINE_SCM_COMMAND(scm_kill_sexp,                      kill_sexp,                      NULL);

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
DEFINE_SCM_COMMAND(scm_beginning_of_line,              beginning_of_line,              NULL);
DEFINE_SCM_COMMAND(scm_end_of_line,                    end_of_line,                    NULL);
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

static SCM scm_execute_extended_command(void) {
    execute_extended_command();
    return SCM_UNSPECIFIED;
}

static SCM scm_eval_expression(void) {
    eval_expression();
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
    if (!scm_is_string(notation_scm)) {
        scm_wrong_type_arg("keymap-global-set", 1, notation_scm);
    }
    if (!scm_is_true(scm_procedure_p(action_scm))) {
        scm_wrong_type_arg("keymap-global-set", 2, action_scm);
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

static SCM get_or_make_buffer_object(Buffer *buf) {
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
    if (buf == selected_frame->wm.minibuffer_window->buffer) {
        message("Cannot kill minibuffer");
        return SCM_BOOL_F;
    }
    
    // Count non-minibuffer buffers
    int non_minibuf_count = 0;
    Buffer *temp = all_buffers;
    do {
        if (temp != selected_frame->wm.minibuffer_window->buffer) {
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

static SCM scm_append_to_buffer(SCM buffer_or_name, SCM text, SCM prepend_newline_scm) {
    Buffer *buf = NULL;
    
    if (scm_is_string(buffer_or_name)) {
        // It's a buffer name string
        char *name = scm_to_utf8_string(buffer_or_name);
        buf = get_buffer(name);
        free(name);
        
        if (!buf) {
            scm_misc_error("append-to-buffer", "No buffer named ~S", scm_list_1(buffer_or_name));
            return SCM_UNSPECIFIED;
        }
    } else if (SCM_IS_A_P(buffer_or_name, buffer_type)) {
        // It's a buffer object
        buf = (Buffer*)scm_foreign_object_ref(buffer_or_name, 0);
    } else {
        scm_wrong_type_arg("append-to-buffer", 1, buffer_or_name);
        return SCM_UNSPECIFIED;
    }
    
    if (!buf) {
        return SCM_UNSPECIFIED;
    }
    
    // Get the text to append
    if (!scm_is_string(text)) {
        scm_wrong_type_arg("append-to-buffer", 2, text);
        return SCM_UNSPECIFIED;
    }
    
    char *text_str = scm_to_utf8_string(text);
    
    // Get prepend_newline flag (default to #t)
    bool prepend_newline = true;
    if (!SCM_UNBNDP(prepend_newline_scm)) {
        prepend_newline = scm_to_bool(prepend_newline_scm);
    }
    
    // Call the C function
    append_to_buffer(buf, text_str, prepend_newline);
    
    free(text_str);
    return SCM_UNSPECIFIED;
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
    
    Buffer *buf = scm_to_buffer(buffer_obj);
    if (!buf) {
        scm_wrong_type_arg("buffer-local-value", 2, buffer_obj);
    }
    
    return buffer_local_value(variable, buf);
}

static SCM scm_set(SCM symbol, SCM newval) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("set", 1, symbol);
    }
    
    return buffer_set(symbol, newval, current_buffer);
}


SCM scm_setq_impl(SCM symbol, SCM value) {
    if (!scm_is_symbol(symbol)) {
        scm_wrong_type_arg("setq", 1, symbol);
    }
    
    if (!current_buffer) {
        // No current buffer, set globally
        SCM symbol_str = scm_symbol_to_string(symbol);
        char *c_str = scm_to_locale_string(symbol_str);
        scm_c_module_define(scm_current_module(), c_str, value);
        free(c_str);
        
        // Check if we're setting frame-resize-pixelwise
        if (strcmp(c_str, "frame-resize-pixelwise") == 0 && selected_frame) {
            update_frame_resize_mode(selected_frame);
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
    if (strcmp(c_str, "frame-resize-pixelwise") == 0 && selected_frame) {
        update_frame_resize_mode(selected_frame);
    }
    
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

static SCM scm_goto_char(SCM position) {
    if (!scm_is_integer(position)) {
        scm_wrong_type_arg("goto-char", 1, position);
    }
    
    size_t pos = scm_to_size_t(position);
    size_t result = goto_char(pos);
    
    return scm_from_size_t(result);
}


/// Keymap

// Keymap type for Scheme
static SCM keymap_type;

// Convert KeyChordMap* to SCM
static SCM keymap_to_scm(KeyChordMap *map) {
    if (!map) return SCM_BOOL_F;
    
    return scm_make_foreign_object_1(keymap_type, map);
}

// Convert SCM to KeyChordMap*
static KeyChordMap* scm_to_keymap(SCM obj) {
    if (scm_is_false(obj)) return NULL;
    
    scm_assert_foreign_object_type(keymap_type, obj);
    return scm_foreign_object_ref(obj, 0);
}

static SCM scm_make_sparse_keymap(void) {
    KeyChordMap *map = make_sparse_keymap();
    return keymap_to_scm(map);
}

static SCM scm_use_local_map(SCM keymap_scm) {
    KeyChordMap *map = scm_to_keymap(keymap_scm);
    use_local_map(map, current_buffer);
    return SCM_UNSPECIFIED;
}

static SCM scm_current_local_map(void) {
    KeyChordMap *map = current_local_map(current_buffer);
    return keymap_to_scm(map);
}

static SCM scm_current_global_map(void) {
    KeyChordMap *map = current_global_map();
    return keymap_to_scm(map);
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

// Keymap finalizer - called when Scheme GCs the keymap
static void finalize_keymap(SCM keymap_obj) {
    KeyChordMap *map = scm_foreign_object_ref(keymap_obj, 0);
    if (map) {
        keymap_free(map);
        free(map);
    }
}





#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>

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
            char load_expr[2048];
            snprintf(load_expr, sizeof(load_expr),
                "(catch #t"
                "  (lambda () (primitive-load \"%s\"))"
                "  (lambda (key . args)"
                "    (message \"Error loading %s: ~~s~~@?\" key"
                "      (if (and (pair? args) (pair? (cdr args)))"
                "          (cadr args)"
                "          \"\") "
                "      (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args)))"
                "          (caddr args)"
                "          '()))))",
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


// TODO use this to register all other commands with doc
#define REGISTER_COMMAND(scheme_name, scm_func)                                \
  do {                                                                         \
    scm_c_define_gsubr(scheme_name, 0, 1, 0, scm_func);                        \
    if (scm_func##_doc) {                                                      \
      SCM proc = scm_variable_ref(scm_c_lookup(scheme_name));                  \
      scm_set_procedure_property_x(proc,                                       \
                                   scm_from_utf8_symbol("documentation"),      \
                                   scm_from_utf8_string(scm_func##_doc));      \
    }                                                                          \
  } while (0)



#include "theme.h"


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
    

    // Initialize keymap foreign object type WITH FINALIZER
    name = scm_from_utf8_symbol("keymap");
    slots = scm_list_1(scm_from_utf8_symbol("data"));
    keymap_type = scm_make_foreign_object_type(name, slots, finalize_keymap);


    setup_user_init_file();

    init_face_bindings();
    init_textprop_bindings();
    init_theme_bindings();
    init_frame_bindings();


    init_buffer_locals();
    

    init_treesit_bindings();


    scm_c_define_gsubr("garbage-collect",               0, 0, 0, scm_garbage_collect);

    scm_c_define_gsubr("set-buffer",                    1, 0, 0, scm_set_buffer);
    scm_c_define_gsubr("goto-char",                     1, 0, 0, scm_goto_char);
    
    scm_c_define_gsubr("window--try-horizontal-split",  0, 1, 0, scm_window_try_horizontal_split);
    scm_c_define_gsubr("window--try-vertical-split",    0, 1, 0, scm_window_try_vertical_split);
    scm_c_define_gsubr("split-window-sensibly",         0, 0, 0, scm_split_window_sensibly);
    scm_c_define_gsubr("display-buffer",                1, 2, 0, scm_display_buffer);


    scm_c_define_gsubr("show-cursor",                   0, 0, 0, scm_show_cursor);
    scm_c_define_gsubr("hide-cursor",                   0, 0, 0, scm_hide_cursor);

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
    scm_c_define_gsubr("buffer-local-value",            2, 0, 0, scm_buffer_local_value);
    scm_c_define_gsubr("set",                           2, 0, 0, scm_set);
    /* scm_c_define_gsubr("setq",                          2, 0, 0, scm_setq); */
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
    set_default(scm_from_utf8_symbol("truncate-lines"), SCM_BOOL_F);
    set_default(scm_from_utf8_symbol("fill-column"),    scm_from_int(70));
    set_default(scm_from_utf8_symbol("tab-width"),      scm_from_int(8));

    mark_automatically_buffer_local(scm_from_utf8_symbol("truncate-lines"));
    mark_automatically_buffer_local(scm_from_utf8_symbol("fill-column"));
    mark_automatically_buffer_local(scm_from_utf8_symbol("tab-width"));


    scm_c_define_gsubr("load",                           1, 0, 0, scm_load);


    // Eval
    scm_c_define_gsubr("eval-last-sexp",                 0, 0, 0, scm_eval_last_sexp);
    scm_c_define_gsubr("eval-buffer",                    0, 0, 0, scm_eval_buffer);
    scm_c_define_gsubr("eval-region",                    0, 0, 0, scm_eval_region);

    // Arg
    scm_c_define_gsubr("universal-argument",             0, 0, 0, scm_universal_argument);
    scm_c_define_gsubr("negative-argument",              0, 0, 0, scm_negative_argument);
    scm_c_define_gsubr("digit-argument",                 0, 0, 0, scm_digit_argument);
    scm_c_define_gsubr("execute-extended-command",       0, 0, 0, scm_execute_extended_command);
    scm_c_define_gsubr("eval-expression",                0, 0, 0, scm_eval_expression);
    scm_c_define_gsubr("keyboard-quit",                  0, 0, 0, scm_keyboard_quit);

    // Buffer
    scm_c_define_gsubr("buffer?",                        1, 0, 0, buffer_p);
    scm_c_define_gsubr("switch-to-buffer",               1, 0, 0, scm_switch_to_buffer);
    scm_c_define_gsubr("kill-buffer",                    0, 1, 0, scm_kill_buffer);
    scm_c_define_gsubr("buffer-name",                    0, 1, 0, scm_buffer_name);
    scm_c_define_gsubr("buffer-list",                    0, 0, 0, scm_buffer_list);
    scm_c_define_gsubr("other-buffer",                   0, 0, 0, scm_other_buffer);
    scm_c_define_gsubr("get-buffer",                     1, 0, 0, scm_get_buffer);
    scm_c_define_gsubr("get-buffer-create",              1, 0, 0, scm_get_buffer_create);
    scm_c_define_gsubr("current-buffer",                 0, 0, 0, scm_current_buffer);
    scm_c_define_gsubr("next-buffer",                    0, 1, 0, scm_next_buffer);
    scm_c_define_gsubr("previous-buffer",                0, 1, 0, scm_previous_buffer);
    scm_c_define_gsubr("append-to-buffer",               2, 1, 0, scm_append_to_buffer);


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


    // Keychord
    scm_c_define_gsubr("keymap-global-set",              2, 0, 0, scm_keymap_global_set);
    scm_c_define_gsubr("keymap-global-unset",            1, 0, 0, scm_keymap_global_unset);
    scm_c_define_gsubr("keychord-documentation",         1, 0, 0, scm_keychord_documentation);
    scm_c_define_gsubr("keychord-bindings",              0, 0, 0, scm_keychord_bindings);    

    // Movement
    REGISTER_COMMAND("forward-char",          scm_forward_char);
    REGISTER_COMMAND("backward-char",         scm_backward_char);
    REGISTER_COMMAND("line-move",             scm_line_move);
    REGISTER_COMMAND("line-move-logical",     scm_line_move_logical);
    REGISTER_COMMAND("line-move-visual",      scm_line_move_visual);
    REGISTER_COMMAND("next-line",             scm_next_line);
    REGISTER_COMMAND("previous-line",         scm_previous_line);
    REGISTER_COMMAND("next-logical-line",     scm_next_logical_line);
    REGISTER_COMMAND("previous-logical-line", scm_previous_logical_line);


    scm_c_define_gsubr("forward-word",                   0, 1, 0, scm_forward_word);
    scm_c_define_gsubr("backward-word",                  0, 1, 0, scm_backward_word);
    scm_c_define_gsubr("forward-paragraph",              0, 1, 0, scm_forward_paragraph);
    scm_c_define_gsubr("backward-paragraph",             0, 1, 0, scm_backward_paragraph);
    scm_c_define_gsubr("beginning-of-line",              0, 1, 0, scm_beginning_of_line);
    scm_c_define_gsubr("end-of-line",                    0, 1, 0, scm_end_of_line);
    scm_c_define_gsubr("end-of-buffer",                  0, 1, 0, scm_end_of_buffer);
    scm_c_define_gsubr("beginning-of-buffer",            0, 1, 0, scm_beginning_of_buffer);
    
    // Editing
    scm_c_define_gsubr("char-or-string?",                1, 0, 0, scm_char_or_string_p);
    scm_c_define_gsubr("insert",                         0, 0, 1, scm_insert);
    scm_c_define_gsubr("delete-backward-char",           0, 1, 0, scm_delete_backward_char);
    scm_c_define_gsubr("delete-char",                    0, 1, 0, scm_delete_char);
    scm_c_define_gsubr("delete-blank-lines",             0, 0, 0, scm_delete_blank_lines);
    scm_c_define_gsubr("delete-indentation",             0, 1, 0, scm_delete_indentation);
    scm_c_define_gsubr("back-to-indentation",            0, 0, 0, scm_back_to_indentation);


    REGISTER_COMMAND("read-only-mode",     scm_read_only_mode);
    REGISTER_COMMAND("save-buffer",        scm_save_buffer);

    REGISTER_COMMAND("newline",            my_scm_newline);
    REGISTER_COMMAND("beginning-of-defun", scm_beginning_of_defun);
    REGISTER_COMMAND("end-of-defun",       scm_end_of_defun);
    REGISTER_COMMAND("open-line",          scm_open_line);
    REGISTER_COMMAND("split-line",         scm_split_line);

    scm_c_define_gsubr("capitalize-word",                0, 1, 0, scm_capitalize_word);
    scm_c_define_gsubr("downcase-word",                  0, 1, 0, scm_downcase_word);
    scm_c_define_gsubr("upcase-word",                    0, 1, 0, scm_upcase_word);


    // Transpose
    scm_c_define_gsubr("transpose-chars",                0, 1, 0, scm_transpose_chars);
    scm_c_define_gsubr("transpose-words",                0, 1, 0, scm_transpose_words);

    // List
    scm_c_define_gsubr("forward-list",                   0, 1, 0, scm_forward_list);
    scm_c_define_gsubr("backward-list",                  0, 1, 0, scm_backward_list);

    // Sexps

    scm_c_define_gsubr("forward-sexp",                   0, 1, 0, scm_forward_sexp);
    scm_c_define_gsubr("backward-sexp",                  0, 1, 0, scm_backward_sexp);
    scm_c_define_gsubr("kill-sexp",                      0, 1, 0, scm_kill_sexp);
    REGISTER_COMMAND("mark-sexp",         scm_mark_sexp);

    
    // Kill/yank
    REGISTER_COMMAND("kill-line",   scm_kill_line);
    REGISTER_COMMAND("kill-word",   scm_kill_word);
    REGISTER_COMMAND("kill-region", scm_kill_region);
    REGISTER_COMMAND("yank",        scm_yank);

    scm_c_define_gsubr("backward-kill-word",             0, 1, 0, scm_backward_kill_word);
    /* scm_c_define_gsubr("kill-region",                    0, 1, 0, scm_kill_region); */
    /* scm_c_define_gsubr("yank",                           0, 1, 0, scm_yank); */
    
    // Region
    scm_c_define_gsubr("set-mark-command",               0, 1, 0, scm_set_mark_command);
    scm_c_define_gsubr("set-mark",                       1, 0, 0, scm_set_mark);
    scm_c_define_gsubr("exchange-point-and-mark",        0, 0, 0, scm_exchange_point_and_mark);
    scm_c_define_gsubr("delete-region",                  0, 0, 0, scm_delete_region);
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


    // NOTE: Load core macros FIRST - this must happen before any other .scm files
    scm_c_eval_string(
        "(catch #t"
        "  (lambda () (primitive-load \"./lisp/subr.scm\"))"
        "  (lambda (key . args)"
        "    (message \"Error loading subr.scm: ~s~@?\" key"
        "      (if (and (pair? args) (pair? (cdr args)))"
        "          (cadr args)"
        "          \"\") "
        "      (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args)))"
        "          (caddr args)"
        "          '()))))"
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

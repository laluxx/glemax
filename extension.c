#include "extension.h"
#include "commands.h"
#include "buffer.h"
#include "screen.h"
#include "wm.h"
#include "faces.h"
#include "theme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

// Configuration paths
static const char* INIT_FILE_PATHS[] = {
    "./init.scm",
    "~/.config/glemax/init.scm",
    "~/.glemax.d/init.scm",
};

// Global Scheme environment
static SCM global_env = SCM_BOOL_F;
static int initialized = 0;

// Error handling
static SCM handle_scheme_error(void *data, SCM key, SCM args) {
    (void)data; // Unused parameter
    SCM subr = SCM_BOOL_F;
    SCM message = scm_symbol_to_string(key);
    SCM rest = SCM_BOOL_F;
    
    scm_display_error(SCM_BOOL_F, scm_current_error_port(), 
                      subr, message, args, rest);
    return SCM_BOOL_F;
}

// Initialize Scheme environment
void init_scheme_environment(void) {
    if (initialized) return;
    scm_init_guile();
    global_env = scm_interaction_environment();
    initialized = 1;
}

// Helper to safely convert SCM strings to C strings
char* safe_scm_to_string(SCM str) {
    if (!scm_is_string(str)) return NULL;
    return scm_to_locale_string(str);
}

// Buffer Management Functions
SCM scm_buffer_new(SCM name, SCM path, SCM fontname) {
    char *c_name = safe_scm_to_string(name);
    char *c_path = safe_scm_to_string(path);
    char *c_font = safe_scm_to_string(fontname);
    
    if (!c_name || !c_path || !c_font) {
        free(c_name); free(c_path); free(c_font);
        scm_throw(scm_from_locale_symbol("glemax-error"),
                  scm_list_1(scm_from_locale_string("Invalid arguments to buffer-new")));
        return SCM_BOOL_F;
    }

    // Use a fallback font if the requested one is "default-font"
    if (strcmp(c_font, "default-font") == 0) {
        free(c_font);
        c_font = strdup("jetb.ttf");  // Use the actual font name as fallback
    }

    newBuffer(&bm, &wm, c_name, c_path, c_font, sw, sh);
    
    free(c_name); free(c_path); free(c_font);
    return SCM_BOOL_T;
}

SCM scm_buffer_switch(SCM name) {
    char *c_name = safe_scm_to_string(name);
    if (!c_name) return SCM_BOOL_F;
    
    Buffer *buffer = getBuffer(&bm, c_name);
    if (buffer) {
        switchToBuffer(&bm, c_name);
        free(c_name);
        return SCM_BOOL_T;
    }
    
    free(c_name);
    return SCM_BOOL_F;
}

SCM scm_buffer_get_content(SCM name) {
    char *c_name = safe_scm_to_string(name);
    if (!c_name) return SCM_BOOL_F;
    
    Buffer *buffer = getBuffer(&bm, c_name);
    free(c_name);
    
    if (buffer && buffer->content) {
        return scm_from_locale_string(buffer->content);
    }
    return SCM_BOOL_F;
}

SCM scm_buffer_set_content(SCM name, SCM content) {
    char *c_name = safe_scm_to_string(name);
    char *c_content = safe_scm_to_string(content);
    
    if (!c_name || !c_content) {
        free(c_name); free(c_content);
        return SCM_BOOL_F;
    }
    
    Buffer *buffer = getBuffer(&bm, c_name);
    if (buffer) {
        setBufferContent(buffer, c_content);
        free(c_name); free(c_content);
        return SCM_BOOL_T;
    }
    
    free(c_name); free(c_content);
    return SCM_BOOL_F;
}

// Window Management Functions
SCM scm_window_split_vertical(void) {
    split_window_below(&wm, font, sw, sh);
    return SCM_BOOL_T;
}

SCM scm_window_split_horizontal(void) {
    split_window_right(&wm, font, sw, sh);
    return SCM_BOOL_T;
}

SCM scm_window_focus_next(SCM count) {
    int c_count = scm_to_int(count);
    other_window(&wm, c_count);
    return SCM_BOOL_T;
}

SCM scm_window_delete(void) {
    delete_window(&wm);
    return SCM_BOOL_T;
}

// UI Functions

SCM scm_message(SCM fmt, SCM rest) {
    char *format_str = safe_scm_to_string(fmt);
    if (!format_str) return SCM_BOOL_F;
    
    // If we have additional arguments
    if (scm_is_true(rest)) {
        // Calculate needed buffer size first
        SCM args = rest;
        int arg_count = scm_to_int(scm_length(rest));
        if (arg_count > 10) {  // Reasonable limit
            free(format_str);
            return SCM_BOOL_F;
        }
        
        // Convert SCM arguments to C values
        long c_args[10];  // Max 10 arguments
        int i = 0;
        while (!scm_is_null(args) && i < 10) {
            SCM arg = scm_car(args);
            if (scm_is_integer(arg)) {
                c_args[i] = scm_to_long(arg);
            } else if (scm_is_string(arg)) {
                c_args[i] = (long)safe_scm_to_string(arg);
            }
            args = scm_cdr(args);
            i++;
        }
        
        // Format the string using the arguments
        char buffer[1024];  // Adjust size as needed
        snprintf(buffer, sizeof(buffer), format_str,
                 c_args[0], c_args[1], c_args[2], c_args[3], c_args[4],
                 c_args[5], c_args[6], c_args[7], c_args[8], c_args[9]);
        
        // Clean up any string arguments
        for (int j = 0; j < i; j++) {
            if (!scm_is_integer(scm_list_ref(rest, scm_from_int(j)))) {
                free((char*)c_args[j]);
            }
        }
        
        message(&bm, buffer);
        SCM result = scm_from_locale_string(buffer);
        free(format_str);
        return result;
    }
    
    // Simple case - just the format string
    message(&bm, format_str);
    SCM result = scm_from_locale_string(format_str);
    free(format_str);
    return result;
}

/* SCM scm_message(SCM msg) { */
/*     char *c_msg = safe_scm_to_string(msg); */
/*     if (!c_msg) return SCM_BOOL_F; */
    
/*     message(&bm, c_msg); */
/*     SCM result = scm_from_locale_string(c_msg);  // Convert the message back to SCM */
/*     free(c_msg); */
/*     return result;  // Return the message itself */
/* } */


SCM scm_theme_next(void) {
    nextTheme();
    return SCM_BOOL_T;
}

SCM scm_theme_previous(void) {
    previousTheme();
    return SCM_BOOL_T;
}

// Module initialization

SCM scm_glemax_version(void) {
    return scm_from_locale_string(GLEMAX_VERSION);
}

static void init_glemax_primitives(void* data) {
    (void)data; // Unused parameter
    
    // Create module and get its public interface
    SCM module = scm_c_define_module(GLEMAX_MODULE_NAME, NULL, NULL);
    scm_c_use_module(GLEMAX_MODULE_NAME);
    
    // Define primitives
#define DEFSUBR(name, func, req, opt, rst)          \
    scm_c_define_gsubr(name, req, opt, rst, func);  \
    scm_c_export(name, NULL);
    
    // Buffer operations
    DEFSUBR("buffer-new", scm_buffer_new, 3, 0, 0);
    DEFSUBR("buffer-switch", scm_buffer_switch, 1, 0, 0);
    DEFSUBR("buffer-get-content", scm_buffer_get_content, 1, 0, 0);
    DEFSUBR("buffer-set-content", scm_buffer_set_content, 2, 0, 0);
    
    // Window operations
    DEFSUBR("window-split-vertical", scm_window_split_vertical, 0, 0, 0);
    DEFSUBR("window-split-horizontal", scm_window_split_horizontal, 0, 0, 0);
    DEFSUBR("window-focus-next", scm_window_focus_next, 1, 0, 0);
    DEFSUBR("window-delete", scm_window_delete, 0, 0, 0);
    
    // UI operations
    /* DEFSUBR("message", scm_message, 1, 0, 0); */
    DEFSUBR("message", scm_message, 1, 0, 1);  // 1 required arg, variable rest args
    DEFSUBR("theme-next", scm_theme_next, 0, 0, 0);
    DEFSUBR("theme-previous", scm_theme_previous, 0, 0, 0);
    
    DEFSUBR("register-command", scm_register_command, 3, 0, 0);
    
    // Version
    DEFSUBR("glemax-version", scm_glemax_version, 0, 0, 0);
    
#undef DEFSUBR

    // Instead of module-export-all, we explicitly exported each symbol above
}

/* static void init_glemax_primitives(void* data) { */
/*     (void)data; // Unused parameter */
    
/*     // Define primitives */
/* #define DEFSUBR(name, func, req, opt, rst)          \ */
/*     scm_c_define_gsubr(name, req, opt, rst, func);  \ */
/*     scm_c_export(name, NULL); */
    
/*     // Buffer operations */
/*     DEFSUBR("buffer-new", scm_buffer_new, 3, 0, 0); */
/*     DEFSUBR("buffer-switch", scm_buffer_switch, 1, 0, 0); */
/*     DEFSUBR("buffer-get-content", scm_buffer_get_content, 1, 0, 0); */
/*     DEFSUBR("buffer-set-content", scm_buffer_set_content, 2, 0, 0); */
    
/*     // Window operations */
/*     DEFSUBR("window-split-vertical", scm_window_split_vertical, 0, 0, 0); */
/*     DEFSUBR("window-split-horizontal", scm_window_split_horizontal, 0, 0, 0); */
/*     DEFSUBR("window-focus-next", scm_window_focus_next, 1, 0, 0); */
/*     DEFSUBR("window-delete", scm_window_delete, 0, 0, 0); */
    
/*     // UI operations */
/*     DEFSUBR("message", scm_message, 1, 0, 0); */
/*     DEFSUBR("theme-next", scm_theme_next, 0, 0, 0); */
/*     DEFSUBR("theme-previous", scm_theme_previous, 0, 0, 0); */
    
/* #undef DEFSUBR */
    
/*     // Define version in the module */
/*     scm_c_define_gsubr("glemax-version", 0, 0, 0, */
/*                        SCM_FUNC(scm_from_locale_string("0.1.0"))); */
/* } */

void init_glemax_bindings(void) {
    // Make sure Scheme is initialized
    if (!initialized) {
        init_scheme_environment();
    }
    
    // Define and initialize our module
    scm_c_define_module("glemax", init_glemax_primitives, NULL);
    
    // Load the module into the current environment
    scm_c_use_module("glemax");
}

// Configuration loading
void load_init_file(void) {
    for (size_t i = 0; i < sizeof(INIT_FILE_PATHS)/sizeof(INIT_FILE_PATHS[0]); i++) {
        char expanded_path[1024];
        
        if (INIT_FILE_PATHS[i][0] == '~') {
            const char *home = getenv("HOME");
            if (!home) continue;
            snprintf(expanded_path, sizeof(expanded_path), "%s%s", 
                     home, INIT_FILE_PATHS[i] + 1);
        } else {
            snprintf(expanded_path, sizeof(expanded_path), "%s", 
                     INIT_FILE_PATHS[i]);
        }
        
        if (access(expanded_path, F_OK) != -1) {
            SCM result = scm_c_catch(SCM_BOOL_T,
                                     (scm_t_catch_body)scm_primitive_load,
                                     scm_from_locale_string(expanded_path),
                                     handle_scheme_error, NULL,
                                     NULL, NULL);
                
            if (scm_is_true(result)) {
                return;  // Successfully loaded
            }
        }
    }
    
    fprintf(stderr, "Warning: No init.scm found in search paths\n");
}

// Evaluation functions
SCM eval_in_scheme(const char *expr) {
    if (!initialized) {
        init_scheme_environment();
    }
    
    SCM form = scm_c_read_string(expr);
    return scm_eval(form, global_env);
}

char* eval_scheme_string(const char* expr) {
    SCM result = SCM_BOOL_F;
    char* output = NULL;
    
    // Catch any errors during evaluation
    result = scm_c_catch(
                         SCM_BOOL_T,
                         (scm_t_catch_body)eval_in_scheme, (void*)expr,
                         handle_scheme_error, NULL,
                         NULL, NULL
                         );
    
    if (scm_is_true(result)) {
        // Convert result to string
        SCM str = scm_object_to_string(result, SCM_UNDEFINED);
        output = scm_to_locale_string(str);
    } else {
        output = strdup("Error during evaluation");
    }
    
    return output;
}

SCM scm_interactive(void) {
    // Get the current stack frame
    SCM stack = scm_make_stack(SCM_BOOL_T, SCM_EOL);
    
    // Get the current function name from the stack
    SCM frame = scm_stack_ref(stack, scm_from_int(1));
    SCM source = scm_frame_source(frame);
    
    if (scm_is_true(source)) {
        SCM name = scm_source_property(source, scm_from_utf8_symbol("name"));
        if (scm_is_true(name)) {
            char* c_name = safe_scm_to_string(scm_symbol_to_string(name));
            if (c_name) {
                char* c_expr = malloc(strlen(c_name) + 3);
                sprintf(c_expr, "(%s)", c_name);

                addSchemeCommand(&commands, c_name,
                                 "Interactive function", c_expr);
                
                free(c_name);
                free(c_expr);
            }
        }
    }
    
    return SCM_BOOL_T;
}

// FROM EDIT MODULE

static size_t find_enclosing_sexp_start(const char* content, size_t point) {
    int paren_level = 0;
    bool in_string = false;
    size_t i = point;

    // First, move back to the start of the current or previous sexp
    while (i > 0 && (isspace((unsigned char)content[i-1]) || content[i-1] == ')')) i--;

    // Now find the start of the enclosing sexp
    while (i > 0) {
        if (content[i] == '"' && (i == 0 || content[i-1] != '\\')) {
            in_string = !in_string;
        }
        if (!in_string) {
            if (content[i] == ')') paren_level++;
            else if (content[i] == '(') {
                if (paren_level == 0) return i;
                paren_level--;
            }
        }
        i--;
    }
    return 0; // Return the start of the buffer if no start found
}

void eval_last_sexp(BufferManager *bm) {
    Buffer *buffer = getActiveBuffer(bm);
    if (!buffer || !buffer->content) {
        message(bm, "No buffer to evaluate.");
        return;
    }

    // Find the last complete sexp before or at point
    size_t point = buffer->point;
    size_t sexp_start = 0;
    size_t sexp_end = point;
    int paren_level = 0;
    bool found = false;

    // First find the closing parenthesis
    while (sexp_end > 0) {
        if (buffer->content[sexp_end - 1] == ')') {
            found = true;
            break;
        }
        sexp_end--;
    }

    if (!found) {
        message(bm, "No S-expression found before point.");
        return;
    }

    // Now find the matching opening parenthesis
    sexp_start = sexp_end - 1;
    paren_level = 1; // We start after finding a closing paren

    while (sexp_start > 0) {
        if (buffer->content[sexp_start - 1] == ')') {
            paren_level++;
        } else if (buffer->content[sexp_start - 1] == '(') {
            paren_level--;
            if (paren_level == 0) {
                break;
            }
        }
        sexp_start--;
    }

    if (paren_level != 0) {
        message(bm, "Unmatched parentheses.");
        return;
    }

    // Extract the s-expression
    sexp_start--; // Include the opening parenthesis
    size_t sexp_length = sexp_end - sexp_start;
    char *sexp = malloc(sexp_length + 1);
    if (!sexp) {
        message(bm, "Memory allocation failed.");
        return;
    }

    strncpy(sexp, buffer->content + sexp_start, sexp_length);
    sexp[sexp_length] = '\0';

    char *result = eval_scheme_string(sexp);
    free(sexp);

    message(bm, result ? result : "Evaluation returned #f");
    free(result);
}

void eval_buffer(BufferManager *bm) {
    Buffer *buffer = getActiveBuffer(bm);
    if (!buffer || !buffer->content) { message(bm, "No buffer to evaluate."); return; }

    char *result = eval_scheme_string(buffer->content);
    if (result) {
        message(bm, result);
        free(result);
    } else {
        message(bm, "Buffer evaluation returned #f");
    }
}

void eval_region(BufferManager *bm) {
    Buffer *buffer = getActiveBuffer(bm);
    if (!buffer || !buffer->content || !buffer->region.active) {
        message(bm, "No active region to evaluate.");
        return;
    }

    size_t start = buffer->region.start;
    size_t end = buffer->region.end;
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    // Get the region text
    size_t region_length = end - start;
    char *region_text = malloc(region_length + 1);
    if (!region_text) {
        message(bm, "Memory allocation failed.");
        return;
    }
    strncpy(region_text, buffer->content + start, region_length);
    region_text[region_length] = '\0';

    // Process each expression in the region
    size_t expr_start = 0;
    size_t pos = 0;
    int paren_level = 0;
    char *last_result = NULL;

    while (pos < region_length) {
        char c = region_text[pos];

        // Skip whitespace between expressions
        if (expr_start == pos && isspace(c)) {
            expr_start++;
            pos++;
            continue;
        }

        // Track parentheses
        if (c == '(') {
            paren_level++;
        } else if (c == ')') {
            paren_level--;
            
            // Found a complete expression
            if (paren_level == 0) {
                pos++; // Include the closing paren
                
                // Extract and evaluate this expression
                size_t expr_len = pos - expr_start;
                char *expr = malloc(expr_len + 1);
                if (expr) {
                    strncpy(expr, region_text + expr_start, expr_len);
                    expr[expr_len] = '\0';

                    // Free the previous result before getting new one
                    free(last_result);
                    last_result = eval_scheme_string(expr);
                    free(expr);
                }
                
                // Move to next expression
                expr_start = pos;
            }
        }
        
        pos++;
    }

    // Display the last result
    if (last_result) {
        message(bm, last_result);
        free(last_result);
    } else {
        message(bm, "No complete expressions found in region");
    }

    free(region_text);
}

/* void eval_region(BufferManager *bm) { */
/*     Buffer *buffer = getActiveBuffer(bm); */
/*     if (!buffer || !buffer->content || !buffer->region.active) { */
/*         message(bm, "No active region to evaluate."); */
/*         return; */
/*     } */

/*     size_t start = buffer->region.start; */
/*     size_t end = buffer->region.end; */
/*     if (start > end) { */
/*         size_t temp = start; */
/*         start = end; */
/*         end = temp; */
/*     } */

/*     size_t region_length = end - start; */
/*     char *region_text = malloc(region_length + 1); */
/*     if (!region_text) { */
/*         message(bm, "Memory allocation failed."); */
/*         return; */
/*     } */
/*     strncpy(region_text, buffer->content + start, region_length); */
/*     region_text[region_length] = '\0'; */

/*     char *result = eval_scheme_string(region_text); */
/*     free(region_text); */

/*     if (result) { */
/*         message(bm, result); */
/*         free(result); */
/*     } else { */
/*         message(bm, "Evaluation failed or returned nil."); */
/*     } */
/* } */

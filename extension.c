#include "extension.h"
#include "buffer.h"
#include "screen.h"
#include "wm.h"
#include "faces.h"
#include "theme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
static char* safe_scm_to_string(SCM str) {
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
SCM scm_message_show(SCM msg) {
    char *c_msg = safe_scm_to_string(msg);
    if (!c_msg) return SCM_BOOL_F;
    
    message(&bm, c_msg);
    free(c_msg);
    return SCM_BOOL_T;
}

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
    DEFSUBR("message", scm_message_show, 1, 0, 0);
    DEFSUBR("theme-next", scm_theme_next, 0, 0, 0);
    DEFSUBR("theme-previous", scm_theme_previous, 0, 0, 0);
    
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
/*     DEFSUBR("message", scm_message_show, 1, 0, 0); */
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

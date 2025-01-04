#ifndef EXTENSION_H
#define EXTENSION_H

#include <libguile.h>
#include "buffer.h"
#include "wm.h"
#include "screen.h"

#define GLEMAX_MODULE_NAME "glemax"
#define GLEMAX_VERSION "0.1.0"

// Core functions
void init_scheme_environment(void);
void init_glemax_bindings(void);
void load_init_file(void);

// Evaluation functions
SCM eval_in_scheme(const char *expr);
char* eval_scheme_string(const char* expr);

char* safe_scm_to_string(SCM str);
SCM scm_register_command(SCM name, SCM description, SCM expr);

// Scheme interface functions
// These should not be called directly from C code
// Buffer operations
SCM scm_buffer_new(SCM name, SCM path, SCM fontname);
SCM scm_buffer_switch(SCM name);
SCM scm_buffer_get_content(SCM name);
SCM scm_buffer_set_content(SCM name, SCM content);

// Window operations
SCM scm_window_split_vertical(void);
SCM scm_window_split_horizontal(void);
SCM scm_window_focus_next(SCM count);
SCM scm_window_delete(void);

// UI operations
/* SCM scm_message(SCM msg); */
SCM scm_message(SCM fmt, SCM rest);
SCM scm_theme_next(void);
SCM scm_theme_previous(void);

// Version function
SCM scm_glemax_version(void);

void eval_last_sexp(BufferManager *bm);
void eval_region(BufferManager *bm);


SCM scm_interactive(void);

#endif // EXTENSION_H

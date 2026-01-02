#pragma once

#include <libguile.h>
#include "wm.h"

/* The ubiquitous max and min macros.  */
#undef min
#undef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

SCM error_handler(void *data, SCM key, SCM args);
SCM eval_string_body(void *data);


SCM get_or_make_buffer_object(Buffer *buf);


int clip_to_bounds(int lower, int num, int upper);

void lisp_init();

void lisp_cleanup();
void lisp_interrupt();

void eval_last_sexp();
void eval_region();
void eval_buffer();


const char* scm_proc_name(SCM proc);
bool is_scm_proc(SCM proc, const char *name);


bool scm_get_bool(const char *name, bool default_value);
size_t scm_get_size_t(const char *name, size_t default_value);
int scm_get_int(const char *name, int default_value);
float scm_get_float(const char *name, float default_value);
char* scm_get_string(const char *name, const char *default_value);


SCM get_or_make_window_object(Window *win);
SCM scm_setq_impl(SCM symbol, SCM value);

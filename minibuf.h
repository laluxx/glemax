#pragma once

#include "buffer.h"

typedef struct HistoryState {
    SCM history_var;           // The history variable symbol
    int current_position;      // Current position in history (-1 = at prompt)
    char *original_input;      // The input before browsing history
    struct HistoryState *next;
} HistoryState;

void add_to_history(SCM caller_func, const char *value);
char* get_history_element(SCM caller_func, int offset);
int get_history_position(SCM caller_func);
void set_history_position(SCM caller_func, int pos);

char *get_minibuffer_contents();

char *read_from_minibuffer(const char *prompt, const char *initial_contents, SCM hist);
char *read_from_minibuffer_internal(const char *prompt, const char *initial_contents, SCM hist);
char *read_from_minibuffer_with_completion(const char *prompt, const char *initial_contents, SCM collection, SCM predicate, SCM hist);


void execute_extended_command(void);
void eval_expression(void);
void activate_minibuffer(void);
void deactivate_minibuffer(void);
void keyboard_quit(void);
void clear_minibuffer_message(void);
void minibuffer_complete(void);
void minibuffer_complete_and_exit(void);
void init_minibuf_bindings(void);

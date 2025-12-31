#pragma once

char *read_from_minibuffer(const char *prompt);
void execute_extended_command(void);
void eval_expression(void);
void activate_minibuffer(void);
void deactivate_minibuffer(void);
void keyboard_quit(void);
void clear_minibuffer_message(void);
void minibuffer_complete_and_exit(void);
void init_minibuf_bindings(void);

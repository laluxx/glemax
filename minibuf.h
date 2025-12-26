#pragma once

void read_from_minibuffer(const char *prompt);
void execute_extended_command();
void eval_expression();

void activate_minibuffer();
void deactivate_minibuffer();
void keyboard_quit();

void clear_minibuffer();

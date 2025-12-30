#pragma once

#include "buffer.h"
#include <stddef.h>

extern bool transient_mark_mode_should_be_disabled;

void message(const char *format, ...);
void insert(const char *text);
void delete_backward_char();
void delete_char();
void newline();
void open_line();
void split_line();

size_t line_beginning_position(int n);
size_t line_end_position(int n);

size_t current_column();
void update_goal_column();

void read_only_mode();

void save_buffer();
void forward_char();
void backward_char();

void line_move();
void line_move_logical();
void line_move_visual();
void next_line();
void previous_line();
void next_logical_line();
void previous_logical_line();
void beginning_of_line();
void end_of_line();
void beginning_of_visual_line();
void end_of_visual_line();
void beginning_of_buffer();
void end_of_buffer();
void delete_blank_lines();
void back_to_indentation();
void delete_indentation();


void set_mark_command();
void set_mark(size_t pos);
void activate_mark();
void deactivate_mark();

void exchange_point_and_mark();
void region_bounds(size_t *start, size_t *end);
void delete_region();

void rkill(size_t start, size_t end, bool prepend);
void kill_line();
void kill_region();
void yank();

/// WORDS

bool isWordChar(uint32_t c);
bool isPunctuationChar(uint32_t c);
size_t beginning_of_word(Buffer *buffer, size_t pos);
size_t end_of_word(Buffer *buffer, size_t pos);
void forward_word();
void backward_word();

void capitalize_word();
void downcase_word();
void upcase_word();

/// PARAGRAPHS

void forward_paragraph();
void backward_paragraph();

/// TRANSPOSE

void transpose_chars();
void transpose_words();

/// LIST

void forward_list();
void backward_list();

/// SEXPS

size_t scan_sexps(Buffer *buffer, size_t from, int count);
void forward_sexp();
void backward_sexp();
void kill_sexp();
void mark_sexp();

/// DEFUN
void beginning_of_defun();
void end_of_defun();

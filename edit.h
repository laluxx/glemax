#ifndef EDIT_H
#define EDIT_H

#include "buffer.h"
#include "isearch.h"
#include "history.h"
#include "extension.h"

#define KILL_RING_SIZE 60

typedef struct {
    char** entries;    // Array of strings
    int size;          // Number of entries currently in the kill ring
    int capacity;      // Maximum number of entries
    int index;         // Current index for yanking
} KillRing;

void initKillRing(KillRing* kr, int capacity);
void freeKillRing(KillRing* kr);
void kr_kill(KillRing* kr, const char* text);
void kill_region(Buffer *buffer, KillRing *kr);
void yank(Buffer *buffer, KillRing *kr, int arg);
void kill_ring_save(Buffer *buffer, KillRing *kr);
void insertChar(Buffer *buffer, unsigned int codepoint);
void buffer_insert_char(Buffer *buffer, char c);

void right_char(Buffer *buffer, bool shift, BufferManager *bm, int count);
void left_char(Buffer *buffer, bool shift, BufferManager *bm, int count);
/* void previous_line(Buffer *buffer, bool shift, BufferManager *bm); */
void previous_line(Buffer *buffer, bool shift, BufferManager *bm, int goal_column);
/* void next_line(Buffer *buffer, bool shift, BufferManager *bm); */
void next_line(Buffer *buffer, bool shift, BufferManager *bm, int goal_column);
void set_goal_column(Buffer *buffer);
void move_end_of_line(Buffer *buffer, bool shift);
void move_beginning_of_line(Buffer * buffer, bool shift);

void delete_char(Buffer *buffer, BufferManager *bm);
void kill_line(Buffer *buffer, KillRing *kr);

// Mark
void set_mark(Buffer *buffer, size_t pos);
void set_mark_command(Buffer *buffer);


// Sexpressions
bool navigate_list(Buffer *buffer, int arg);
void forward_list(Buffer *buffer, int arg);
void backward_list(Buffer *buffer, int arg);
bool forward_sexp(Buffer *buffer, int arg);
bool backward_sexp(Buffer *buffer, int arg);
void kill_sexp(Buffer *buffer, KillRing *kr, int arg);

// Transposing
void transpose_subr(Buffer *buffer, bool (*mover)(Buffer *, int, bool), int arg);
void transpose_words(Buffer *buffer, int arg);
void transpose_chars(Buffer *buffer);

void open_line(Buffer *buffer);

void delete_indentation(Buffer *buffer, BufferManager *bm, int arg);
bool isWordChar(char c);
void addIndentation(Buffer *buffer, int indentation);
void removeIndentation(Buffer *buffer, int indentation);
void duplicate_line(Buffer *buffer);
bool is_word_char(char c);
bool is_punctuation_char(char c);
bool backward_word(Buffer *buffer, int count, bool shift);
bool forward_word(Buffer *buffer, int count, bool shift);
void backward_kill_word(Buffer *buffer, KillRing *kr);
void forward_paragraph(Buffer *buffer, bool shift);
void backward_paragraph(Buffer *buffer, bool shift);
void beginning_of_buffer(Buffer *buffer);
void end_of_buffer(Buffer *buffer);
void indent(Buffer *buffer, int indentation, BufferManager *bm, int arg);
void indent_region(Buffer *buffer, BufferManager *bm, int indentation, int arg);
void goto_line(BufferManager *bm, WindowManager *wm, int sw, int sh);
void enter(Buffer *buffer, BufferManager *bm, WindowManager *wm, Buffer *minibuffer, Buffer *prompt, int indentation, bool electric_indent_mode, int sw, int sh, NamedHistories *nh, int arg);


void find_file(BufferManager *bm, WindowManager *wm, int sw, int sh);
void backspace(Buffer *buffer, bool electric_pair_mode);

char* paste_from_clipboard();
void copy_to_clipboard(const char* text);

void moveTo(Buffer *buffer, int ln, int col);
void delete_blank_lines(Buffer *buffer, int arg);
void save_buffer(BufferManager *bm, Buffer *buffer);

// MINIBUFFER FUNCTIONS
void execute_shell_command(BufferManager *bm, char *command);
void shell_command(BufferManager *bm);
void execute_extended_command(BufferManager *bm);
void eval_expression(BufferManager *bm);
void keep_lines(BufferManager *bm, WindowManager *wm);
void load_font(BufferManager *bm, WindowManager *wm, int sw, int sh);


/* void recenter(Window *window); */
void recenter(Window *window, bool instant);
void capitalize_word(Buffer *buffer);

bool bolp(Buffer *buffer);
void mark_scope(Buffer *buffer);

// LERP

float getCurrentTime();

// DIFF-HL
void diff_hl_next_hunk(Buffer *buffer);
void diff_hl_previous_hunk(Buffer *buffer);

// EXTENSION
static SCM symbol_error_handler(void *data, SCM key, SCM args);
static SCM collect_symbols_helper(void *data);
void insert_guile_symbols(Buffer *buffer, BufferManager *bm);


void exchange_point_and_mark(Buffer *buffer);

void scroll(Window *window, int arg);
void scroll_up(Window *window, int arg);
void scroll_down(Window *window, int arg);

void trimTrailingFile(char *path);

#endif

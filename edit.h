#ifndef EDIT_H
#define EDIT_H

#include "buffer.h"
#include "isearch.h"
#include "history.h"
#include "extension.h"

#define KILL_RING_SIZE 60

typedef struct {
    char **entries; // Array of strings
    int size;       // Number of entries currently in the kill ring
    int capacity;   // Maximum number of entries
    int index;      // Current index for yanking
} KillRing;

void initKillRing(       KillRing* kr, int capacity);
void freeKillRing(       KillRing* kr);
void kr_kill(            KillRing* kr, const char* text);

void kill_region(        Buffer *buffer);
void delete_region(      Buffer *buffer);
void yank              ( Buffer *buffer, int arg);
void kill_ring_save    ( Buffer *buffer);
void kill_word         ( Buffer *buffer);
void backward_kill_word( Buffer *buffer);
void kill_line(          Buffer *buffer);
void kill_sexp(          Buffer *buffer);



/* void insertChar(Buffer *buffer, unsigned int codepoint); */
void insertChar(Buffer *buffer, unsigned int codepoint);
void buffer_insert_char(Buffer *buffer, char c);

void right_char(   Buffer *buffer, bool shift, int arg);
void left_char(    Buffer *buffer, bool shift, int arg);
void previous_line(Window *win, bool shift, int arg);
void next_line(    Window *win, bool shift, int arg);

void set_goal_column(Buffer *buffer);

void move_end_of_line(      Window *win, bool shift, int arg);
void move_beginning_of_line(Window *win, bool shift, int arg);


void delete_char(Buffer *buffer);

// Mark
void set_mark(Buffer *buffer, size_t pos);
void set_mark_command(Buffer *buffer);


// Sexpressions
bool navigate_list(Buffer *buffer, bool shift, int arg);
void forward_list( Buffer *buffer, bool shift, int arg);
void backward_list(Buffer *buffer, bool shift, int arg);
void forward_sexp( Buffer *buffer, bool shift, int arg);
void backward_sexp(Buffer *buffer, bool shift, int arg);

// Transposing
void transpose_subr(Buffer *buffer, void (*mover)(Buffer *, bool, int), int arg);
void transpose_words(Buffer *buffer, int arg);
void transpose_chars(Buffer *buffer);

void open_line(Buffer *buffer);

void delete_indentation(Buffer *buffer, int arg);
bool isWordChar(char c);
void add_indentation(Buffer *buffer);
void remove_indentation(Buffer *buffer);

void duplicate_line(Buffer *buffer);
bool is_word_char(char c);
bool is_punctuation_char(char c);

void backward_word(Buffer *buffer, bool shift, int arg);
void forward_word( Buffer *buffer, bool shift, int arg);

void forward_paragraph(Buffer *buffer, bool shift);
void backward_paragraph(Buffer *buffer, bool shift);
void beginning_of_buffer(Buffer *buffer);
void end_of_buffer(Buffer *buffer);
void indent_line(Buffer *buffer, bool shift, int arg);
void indent_region(Buffer *buffer, bool shift, int arg);
void goto_line(BufferManager *bm);
void enter(Buffer *buffer, BufferManager *bm, WindowManager *wm, Buffer *minibuffer, Buffer *prompt, int indentation, bool electric_indent_mode, int sw, int sh, NamedHistories *nh, int arg);

void find_file(BufferManager *bm, WindowManager *wm);
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
void keep_lines(BufferManager *bm);
void load_font(BufferManager *bm);

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

char *getBufferDirectory(const char *path);

void read_only_mode(Buffer *buffer);

void helpful_symbol(BufferManager *bm);

// EXTENSION
/* void insert_guile_symbols(Buffer *buffer, BufferManager *bm); */
void insert_guile_symbols(Buffer *buffer);


void exchange_point_and_mark(Buffer *buffer);

void scroll(Window *window, int arg);
void scroll_up(Window *window, int arg);
void scroll_down(Window *window, int arg);

const char *getProjectRoot(const char *path);
const char *getTrailingFile(const char *path);
const char *getFilename(const char *path);
const char *getFileExtension(const char* filename);
void trimTrailingFile(char *path);



#include <setjmp.h>

extern jmp_buf env; // Global jump buffer

#define MM(dest, src, n, buffer, index, lengthChange)                          \
  if (setjmp(env)) {                                                           \
    return; /* Return from the caller function if longjmp is called */         \
  }                                                                            \
  mm(dest, src, n, buffer, index, lengthChange)

void *mm(void *dest, const void *src, size_t n, Buffer *buffer, int index, int lengthChange);




void switch_to_buffer(BufferManager *bm);
void updateSyntaxHighlighting(Buffer *buffer, int index, int lengthChange);

size_t find_visual_line_start(Buffer *buffer, Window *win, size_t pos);
size_t find_visual_line_end(Buffer *buffer, Window *win, size_t start_pos);


void change_major_mode(BufferManager *bm);
void handle_minibuffer_command(BufferManager *bm, WindowManager *wm, Buffer *minibuffer, Buffer *prompt, NamedHistories *nh);
int getGlobalArg(Buffer *argBuffer);


bool bobp(Buffer *b);
bool eobp(Buffer *b);
void forward_line(Buffer *b, int n);
bool looking_at(Buffer *b, const char *regex);
int current_column(Buffer *buffer);

#endif

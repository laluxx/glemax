#ifndef EDIT_H
#define EDIT_H

#include "buffer.h"
#include "isearch.h"
#include "history.h"

#define KILL_RING_SIZE 60

typedef struct {
    char** entries;    // Array of strings
    int size;          // Number of entries currently in the kill ring
    int capacity;      // Maximum number of entries
    int index;         // Current index for yanking
} KillRing;


void initKillRing(KillRing* kr, int capacity);
void freeKillRing(KillRing* kr);
void kill(KillRing* kr, const char* text);
void kill_region(Buffer *buffer, KillRing *kr);
/* void yank(Buffer *buffer, KillRing *kr); */
void yank(Buffer *buffer, KillRing *kr, int arg);
void kill_ring_save(Buffer *buffer, KillRing *kr);
void insertChar(Buffer *buffer, char c);
/* void insertChar(Buffer *buffer, char c, int arg); */


/* void right_char(Buffer *buffer, bool shift, BufferManager *bm); */
/* void left_char(Buffer *buffer, bool shift, BufferManager *bm); */
void right_char(Buffer *buffer, bool shift, BufferManager *bm, int count);
void left_char(Buffer *buffer, bool shift, BufferManager *bm, int count);

void previous_line(Buffer *buffer, bool shift, BufferManager *bm);
void next_line(Buffer *buffer, bool shift, BufferManager *bm);

void move_end_of_line(Buffer *buffer, bool shift);
void move_beginning_of_line(Buffer * buffer, bool shift);
/* void delete_char(Buffer *buffer); */
void delete_char(Buffer *buffer, BufferManager *bm);
void kill_line(Buffer *buffer, KillRing *kr);
void open_line(Buffer *buffer);
/* void delete_indentation(Buffer *buffer, BufferManager *bm); */
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
/* void indent(Buffer *buffer, int indentation, BufferManager *bm); */
void indent(Buffer *buffer, int indentation, BufferManager *bm, int arg);
void indent_region(Buffer *buffer, BufferManager *bm, int indentation, int arg);

void goto_line(BufferManager *bm, WindowManager *wm, int sw, int sh);

/* void enter(Buffer *buffer, BufferManager *bm, WindowManager *wm, Buffer *minibuffer, Buffer *prompt, ISearch *isearch, int indentation, bool electric_indent_mode, int sw, int sh, NamedHistories *nh, int arg); */
void enter(Buffer *buffer, BufferManager *bm, WindowManager *wm, Buffer *minibuffer, Buffer *prompt, int indentation, bool electric_indent_mode, int sw, int sh, NamedHistories *nh, int arg);

void find_file(BufferManager *bm, WindowManager *wm, int sw, int sh);

void backspace(Buffer *buffer, bool electric_pair_mode);


char* paste_from_clipboard();
void copy_to_clipboard(const char* text);

void navigate_list(Buffer *buffer, int arg);
void forward_list(Buffer *buffer, int arg);
void backward_list(Buffer *buffer, int arg);
void moveTo(Buffer *buffer, int ln, int col);
void delete_blank_lines(Buffer *buffer, int arg);
void save_buffer(BufferManager *bm, Buffer *buffer);

void execute_shell_command(BufferManager *bm, char *command);
void shell_command(BufferManager *bm);


void recenter(Window *window);
extern int recenterState; // 0: Initial, 1: Top, 2: Center, 3: Bottom
void recenter_top_bottom(Window *window);

#endif

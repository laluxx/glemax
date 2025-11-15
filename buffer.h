#pragma once
#include "rope.h"
#include <obsidian/obsidian.h>

typedef struct {
    float x;
    float y;
    bool visible;
    double last_blink;
    size_t blink_count;
    size_t goal_column;
} Cursor;

typedef struct {
    size_t mark;
    bool active;
} Region;

typedef struct {
    rope_t *rope;           
    Cursor cursor;
    size_t pt;
    Font *font;
    Region region;
} Buffer;

extern Buffer *buffer;


extern bool shift;
extern bool ctrl;
extern bool alt;
extern int arg;
extern bool argument_manually_set;


extern bool kill_whole_line;


Buffer* buffer_create(Font *font);
void buffer_destroy(Buffer *buffer);

void set_point(size_t new_pt);
void move_point(int delta);

void insert(uint32_t codepoint);
void delete_backward_char();
void delete_char();
void newline();
void open_line();
void split_line();


size_t line_beginning_position();
size_t line_end_position();
size_t current_column();
void update_goal_column();


void forward_char();
void backward_char();
void next_line();
void previous_line();
void end_of_line();
void beginning_of_line();
void beginning_of_buffer();
void end_of_buffer();


void set_mark_command();
void exchange_point_and_mark();
void region_bounds(size_t *start, size_t *end);
void delete_region();

void kill(size_t start, size_t end, bool prepend);
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


/// PARAGRAPHS

void forward_paragraph();
void backward_paragraph();

/// ARG

extern bool raw_prefix_arg;
void universal_argument();
void digit_argument();
void negative_argument();



extern KeyChordAction last_command;
extern bool last_command_was_kill;

bool is_kill_command(KeyChordAction action);
void kill_word();
void backward_kill_word();

#include "wm.h"
typedef struct Window Window;

void execute_extended_command();
void keyboard_quit();


void draw_buffer(Buffer *buffer, Window *win, float start_x, float start_y);
void draw_cursor(Buffer *buffer, Window *win, float start_x, float start_y);
void reset_cursor_blink(Buffer *buffer);

void message(const char *format, ...);

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

typedef struct Buffer {
    struct Buffer *next;    // Next buffer in circular list
    struct Buffer *prev;    // Previous buffer in circular list
    char *name;
    rope_t *rope;           
    Cursor cursor;
    size_t pt;
    Font *font;
    Region region;
} Buffer;

extern Buffer *all_buffers;
extern Buffer *current_buffer;


extern bool shift;
extern bool ctrl;
extern bool alt;
extern bool argument_manually_set;

Buffer* buffer_create(Font *font, const char *name);
void buffer_destroy(Buffer *buffer);
Buffer *get_buffer(const char *name);
Buffer *get_buffer_create(Font *font, const char *name);
void switch_to_buffer(Buffer *buf);
Buffer *other_buffer();
void kill_buffer(Buffer *buf);

void next_buffer();
void previous_buffer();

void set_point(size_t new_pt);
void move_point(int delta);

void end_of_line();
void beginning_of_line();
void beginning_of_buffer();
void end_of_buffer();



/// ARG
void universal_argument();
void digit_argument();
void negative_argument();

int get_prefix_arg();
void set_prefix_arg(int value);
bool get_raw_prefix_arg();
void set_raw_prefix_arg(bool value);


extern SCM last_command;
extern bool last_command_was_kill;

bool is_kill_command(SCM proc);
void kill_word();
void backward_kill_word();

#include "wm.h"
typedef struct Window Window;

void execute_extended_command();
void keyboard_quit();

bool is_pair(uint32_t left, uint32_t right);

void draw_buffer(Buffer *buffer, Window *win, float start_x, float start_y);
void draw_cursor(Buffer *buffer, Window *win, float start_x, float start_y);
void reset_cursor_blink(Buffer *buffer);
void adjust_all_window_points_after_modification(size_t pos, int delta);

void append_to_buffer(Buffer *buf, const char *text, bool prepend_newline);
void message(const char *format, ...);

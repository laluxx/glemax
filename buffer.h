#pragma once
#include "rope.h"
#include <obsidian/obsidian.h>

typedef struct {
    float x;
    float y;
    bool visible;
    double last_blink;
    int blink_count;
    size_t goal_column;
} Cursor;

typedef struct {
    rope_t *rope;           
    Cursor cursor;
    size_t pt;
    Font *font;
} Buffer;

extern Buffer *buffer;


Buffer* buffer_create(Font *font);
void buffer_destroy(Buffer *buffer);

void insert(uint32_t codepoint);
void delete_backward_char();
void delete_char();
void newline();
void open_line();

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


void draw_buffer(Buffer *buffer, float start_x, float start_y);
void draw_cursor(Buffer *buffer, float start_x, float start_y);
void reset_cursor_blink(Buffer *buffer);

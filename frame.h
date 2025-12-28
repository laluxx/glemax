#pragma once

#include <stdbool.h>
#include "wm.h"

// TODO Make this a linked list
// Emacs uses an elisp list
// But we can manage our own memory :)
typedef struct {
    int x;
    int y;
    int width;
    int height;
    bool focused;
    WindowManager wm;
    size_t left_fringe_width, right_fringe_width;
    // Canonical X unit.  Width of default font, in pixels.
    int column_width;
    // Canonical Y unit.  Height of a line, in pixels.
    int line_height;
} Frame;

extern Frame *selected_frame;

Frame* create_frame(int x, int y, int width, int height);
void destroy_frame(Frame *frame);

void set_frame_position(Frame *frame, int x, int y);
void init_frame_bindings(void);
size_t frame_char_height(Frame *frame);
size_t frame_char_width(Frame *frame);


void update_frame_resize_mode(Frame *frame);

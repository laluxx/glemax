#pragma once

#include "buffer.h"
#include <stdbool.h>

typedef enum {
    SPLIT_NONE,       // Leaf window (no split)
    SPLIT_VERTICAL,   // Split left/right
    SPLIT_HORIZONTAL  // Split top/bottom
} SplitType;

// Forward declaration
typedef struct Window Window;

// Binary tree
struct Window {
    // Tree structure
    Window *parent;
    Window *left;   // Left child for vertical split, top for horizontal
    Window *right;  // Right child for vertical split, bottom for horizontal
    
    // Window properties
    SplitType split_type;
    Buffer *buffer;
    
    // Per-window point (like Emacs)
    size_t point;
    size_t goal_column;
    
    // Geometry (in pixels)
    float x, y;
    float width, height;
    
    // Split ratio (0.0 to 1.0) - size of left/top child relative to total
    float split_ratio;
    
    // Visual state
    bool is_selected;
};

typedef struct {
    Window *root;    
    Window *selected;
    int window_count;
} WindowManager;

extern WindowManager wm;

extern size_t fringe_width;


void wm_init(Buffer *initial_buffer, float x, float y, float width, float height);
void wm_cleanup();

void split_window_below();
void split_window_right();
void delete_window();
void delete_other_windows();

void other_window();
Window* next_window(Window *current);
Window* previous_window(Window *current);

// Window queries
bool is_leaf_window(Window *win);
int count_windows();
Window* get_selected_window();
Buffer* get_selected_buffer();

// Layout and rendering
void wm_recalculate_layout();
void wm_draw();

void balance_windows();
void enlarge_window();
void shrink_window();

Window* window_create(Window *parent, Buffer *buffer);
void window_destroy(Window *win);
void collect_leaf_windows(Window *win, Window **leaves, int *count);

void debug_print_windows();

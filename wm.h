#pragma once

#include "buffer.h"
#include <cglm/types.h>
#include <stdbool.h>

typedef enum {
    SPLIT_NONE,       // Leaf window (no split)
    SPLIT_VERTICAL,   // Split left/right
    SPLIT_HORIZONTAL  // Split top/bottom
} SplitType;

// Forward declaration
typedef struct Window Window;

struct Window {
    // Binary tree structure
    Window *parent;
    Window *left;   // Left child for vertical split, top for horizontal
    Window *right;  // Right child for vertical split, bottom for horizontal
    
    SplitType split_type;
    Buffer *buffer;
    size_t point;

    float x, y;
    float scrollx, scrolly;
    float width, height;

    float split_ratio;
    bool is_selected;
    bool is_minibuffer;
};

typedef struct {
    SplitType split_type;
    float split_ratio;
    Buffer *buffer;
    size_t point;
    bool is_selected;
    float scrollx, scrolly;
    int left_index;   // Index of left child in array  (-1 if leaf)
    int right_index;  // Index of right child in array (-1 if leaf)
} WindowSnapshot;

typedef struct {
    WindowSnapshot *windows; // Array of window snapshots
    int count;               // Number of windows
    int root_index;          // Index of root window
    float root_x, root_y;
    float  root_width, root_height;
} WindowConfiguration;


// TODO Minibuffer struct inside WindowManager
typedef struct {
    Window *root;    
    Window *selected;
    Window *minibuffer_window;
    Window *previous_window;   // NOTE Only tracked for the modeline we might want to change name
    bool minibuffer_active;
    size_t minibuffer_message_start;
    int window_count;
    WindowConfiguration saved_config;
} WindowManager;

extern WindowManager wm;

extern size_t fringe_width;

void wm_init(Buffer *initial_buffer, Buffer *minibuffer, float x, float y,
             float width, float height);

void wm_cleanup();

void split_window_below();
void split_window_right();
void delete_window();
void delete_other_windows();

void other_window();
Window* next_window(Window *current);
Window* previous_window(Window *current);

// Minibuffer functions
void activate_minibuffer();
void go_inside_minibuffer();
void deactivate_minibuffer();
bool is_minibuffer_window(Window *win);

// Window queries
bool is_leaf_window(Window *win);
int count_windows();
Window* get_selected_window();
Buffer* get_selected_buffer();


float calculate_minibuffer_height();

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


void update_window_scroll(Window *win);
void update_windows_scroll();
void update_window_scroll_lerp(Window *win);


// Window related editing functions

void recenter();
extern int recenter_positions;
void recenter_top_bottom();


void scroll_up_command();
void scroll_down_command();
void scroll_other_window();
void scroll_other_window_down();

void move_to_window_line();
extern int move_to_window_line_positions;
void move_to_window_line_top_bottom();


WindowConfiguration save_window_configuration();
Window* restore_window_recursive(WindowSnapshot *snapshots, int index, Window *parent);
void restore_window_configuration(WindowConfiguration *config);
void free_window_configuration(WindowConfiguration *config);

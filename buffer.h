#pragma once
#include "rope.h"
#include <obsidian/obsidian.h>
#include <sys/types.h>

typedef struct {
    float x;
    float y;
    bool visible;
    double last_blink;
    size_t blink_count;
    /* size_t goal_column; */
} Cursor;

typedef struct {
    int mark;
    bool active;
} Region;

#include "textprop.h"
#include "treesit.h"

typedef struct Buffer {
    struct Buffer *next; // Next buffer in circular list
    struct Buffer *prev; // Previous buffer in circular list
    char *name;
    rope_t *rope;           
    Cursor cursor;
    size_t pt;
    Region region;
    TextProp *props;    
    SCM local_var_alist; // Alist of (SYMBOL . VALUE) pairs
    KeyChordMap *keymap;  // Buffer-local keymap (can be NULL)
    TreeSitterState *ts_state; // (NULL if not using tree-sitter)
    bool read_only;
    bool modified;
} Buffer;

extern Buffer *all_buffers;
extern Buffer *current_buffer;

// Keymap functions
KeyChordMap* make_sparse_keymap(void);
void use_local_map(KeyChordMap *local_map, Buffer *buf);
KeyChordMap* current_local_map(Buffer *buf);
KeyChordMap* current_global_map(void);


extern bool shift;
extern bool ctrl;
extern bool alt;
extern bool argument_manually_set;

Buffer* buffer_create(const char *name);
void buffer_destroy(Buffer *buffer);
Buffer *get_buffer(const char *name);
Buffer *get_buffer_create(const char *name);
void switch_to_buffer(Buffer *buf);
Buffer *other_buffer();
void kill_buffer(Buffer *buf);

void next_buffer();
void previous_buffer();

void set_point(size_t new_pt);
size_t goto_char(size_t pos);
void move_point(int delta);

void end_of_line();
void beginning_of_line();
void beginning_of_buffer();
void end_of_buffer();

/// Buffer local variables

void init_buffer_locals(void);
bool is_automatically_buffer_local(SCM symbol);
void mark_automatically_buffer_local(SCM symbol);
void set_default(SCM symbol, SCM value);
SCM default_value(SCM symbol);
SCM buffer_local_value(SCM variable, Buffer *buf);
SCM buffer_set(SCM symbol, SCM newval, Buffer *buf);
bool local_variable_p(SCM symbol, Buffer *buf);
bool local_variable_if_set_p(SCM symbol, Buffer *buf);
SCM kill_local_variable(SCM symbol, Buffer *buf);
void kill_all_local_variables(Buffer *buf);
SCM buffer_local_variables(Buffer *buf);



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


bool is_pair(uint32_t left, uint32_t right);

size_t find_start_position(Buffer *buffer, Window *win, float *out_start_y);
void draw_buffer(Buffer *buffer, Window *win, float start_x, float start_y);
void draw_cursor(Buffer *buffer, Window *win, float start_x, float start_y);
void reset_cursor_blink(Buffer *buffer);
void adjust_all_window_points_after_modification(size_t pos, int delta);

void append_to_buffer(Buffer *buf, const char *text, bool prepend_newline);
void message(const char *format, ...);

#ifndef GLOBALS_H
#define GLOBALS_H

#include <stdio.h>

extern bool dragging;
extern double initialMouseX;
extern double initialMouseY;
extern double dragThreshold;
extern bool  eatchar; // NOTE Keychords could be implemented in scheme using this very simple approach
extern bool  electric_pair_mode;     // TODO Wrap selection for () [] {} '' ""
extern bool  auto_text_scale_mode;
extern bool  blink_cursor_mode;
extern float blink_cursor_delay;     // Seconds of idle time before the first blink of the cursor.
extern float blink_cursor_interval;  // Lenght of cursor blink interval in seconds.
extern int   blink_cursor_blinks;    // How many times to blink before stopping.
extern double lastBlinkTime;         // Last time the cursor state changed
extern bool cursorVisible;           // Initial state of the cursor visibility
extern int blinkCount;               // Counter for number of blinks
extern int   indentation;
extern bool  show_paren_mode;
extern float show_paren_delay;
extern int   kill_ring_max;
extern bool  electric_indent_mode;
extern bool  rainbow_mode;
extern bool  crystal_cursor_mode;
extern float mouse_wheel_scroll_amount;
extern bool  minimap_mode;
extern bool  minimap_cursor;
extern bool  color_fringe_mode;
extern bool  fringe_mode;
extern bool  hide_region_mode;
extern char  *first_theme_name;
extern float minimap_width;
extern size_t fringe;
extern size_t minimap_left_padding;
extern bool   minimap_padding_mode;
extern bool   hl_scope_mode;
extern float  hl_scope_base_brightness;
extern float  hl_scope_brightness_step;
extern bool   theme_lerp_mode;
extern float  theme_lerp_speed;
extern float  theme_lerp_threshold;
extern bool   diff_hl_mode;
extern bool   diff_hl_bg;
extern bool   scroll_lerp_mode;
extern float  scroll_lerp_speed;




#endif

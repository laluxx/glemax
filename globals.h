#ifndef GLOBALS_H
#define GLOBALS_H

#include <lume.h>
#include <stdio.h>

extern bool   dragging;
extern double initialMouseX;
extern double initialMouseY;
extern double dragThreshold;
extern bool   eatchar;
extern bool   electric_pair_mode;
extern bool   auto_text_scale_mode;
extern bool   blink_cursor_mode;
extern float  blink_cursor_delay;
extern float  blink_cursor_interval;
extern int    blink_cursor_blinks;
extern double lastBlinkTime;
extern bool   cursorVisible;
extern int    blinkCount;
extern int    indentation;
extern bool   show_paren_mode;
extern float  show_paren_delay;
extern int    kill_ring_max;
extern bool   electric_indent_mode;
extern bool   rainbow_mode;
extern bool   crystal_cursor_mode;
extern float  mouse_wheel_scroll_amount;
extern bool   mouse_wheel_lerp_mode;
extern float  mouse_wheel_lerp_speed;
extern bool   minimap_mode;
extern bool   minimap_cursor;
extern bool   fringe_mode;
extern bool   color_fringe_mode;
extern bool   hide_region_mode;
extern char   *first_theme_name;
extern float  minimap_width;
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
extern float  diff_hl_bg_alpha;
extern bool   diff_hl_text;
extern bool   diff_hl_cursor;
extern bool   scroll_lerp_mode;
extern float  scroll_lerp_speed;
extern bool   clock_mode;
extern float  clockScale;
extern Vec2f  clockPosition;
extern char   *clock_shader;
extern char   *buffer_shader;
extern char   *scratch_buffer_content;
extern bool   mark_mode;
extern bool   region_fg_mode;
extern bool   region_alpa_mode;
extern bool   lerp_line_mode;
extern float  lerp_line_duration;
extern bool   vertico_mode;
extern bool   find_file_focus_existing;
extern size_t max_gemini_redirections;

#endif

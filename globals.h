#ifndef GLOBALS_H
#define GLOBALS_H

#include <lume.h>
#include <stdio.h>

// TODO add all those names as commands && Guile procedures automatically if they are bool.
// Calling that command or functions will invert the boolean variable
extern bool   dragging;
extern double initialMouseX;
extern double initialMouseY;
extern double dragThreshold;
extern bool   eatchar;
extern bool   electric_pair_mode;
extern bool   mark_electric_pairs_mode;
extern bool   auto_text_scale_mode;
extern bool   blink_cursor_mode;
extern float  blink_cursor_delay;
extern float  blink_cursor_interval;
extern int    blink_cursor_blinks;
extern double lastBlinkTime;
extern bool   cursorVisible;
extern int    blinkCount;
/* extern int    indentation; */
extern size_t indentation;
extern size_t tab;
extern bool   show_paren_mode;
extern float  show_paren_delay;
extern int    kill_ring_max;
extern bool   electric_indent_mode;
extern bool   rainbow_mode;
extern bool   crystal_cursor_mode;
extern float  mouse_wheel_scroll_amount;
extern bool   mouse_wheel_lerp_mode;
extern float  mouse_wheel_lerp_speed;
extern bool   minimap;
extern bool   minimap_easing_mode;
/* extern float  minimap_target_width; */
/* extern bool   minimap_lerp_active; */


extern bool   minibuffer_minimap_mode;
extern bool   minimap_cursor;
extern bool   fringe_mode;
extern bool   color_fringe_mode;
extern bool   hide_region_mode;
extern char   *first_theme_name;
/* extern float  minimap_width; */
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
extern bool   scroll_lerp;
extern float  scroll_lerp_speed;
extern bool   clock_mode;
extern float  clockScale;
extern Vec2f  clockPosition;
extern char   *clock_shader;
extern char   *buffer_shader;
extern char   *scratch_buffer_content;
extern bool   mark_mode;
extern bool   region_fg_mode;
extern bool   lerp_line;
extern float  lerp_line_duration;
extern bool   vertico_mode;
extern size_t vertico_max_lines;
extern bool   find_file_focus_existing;
extern size_t max_gemini_redirections;
extern bool   revert_buffer_mode;
/* extern bool   scroll_bar_mode; */
extern bool   scroll_bar;

extern size_t scroll_bar_right_padding;

extern size_t scroll_bar_thickness;

extern bool keep_right_fringe;

extern bool hide_scroll_bar_with_minimap;
extern bool show_scroll_bar_with_minimap;
extern size_t scroll_bar_left_trim;
extern bool scroll_bar_lerp_active;
extern bool prevent_scroll;

extern bool swap_windows_parameters;

extern bool line_move_visual;
extern bool global_visual_line_mode;
extern bool mmm;
extern bool word_wrap;


extern bool show_minimap_with_gemini;
extern bool lerp_minimap_with_gemini;
extern bool global_minimap;

extern bool focus_window_if_buffer_displayed;
extern bool hide_mark_when_region_active;

extern bool shiftPressed;
extern bool ctrlPressed;
extern bool altPressed;
extern bool lerp_minimap_on_startup;



extern bool region_alpha;
extern size_t region_alpha_amount;

#endif

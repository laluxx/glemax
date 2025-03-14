#include <lume.h>
#include <stdbool.h>
#include <stdlib.h>


// NOTE We could parse this file line by line to get the default
// value of globals and their annotation (dumb way)
// TODO Just watch this file and modify it at runtime or extract it into a data structure and update it when we change something ?
// Both approaches allow a mode specified in editing global variables at runtime

// NOTE That this file does not include per buffer modes.

// double mouseX; NOTE Moved
// double mouseY; NOTE Moved

bool   dragging                  = false;
double initialMouseX             = 0;
double initialMouseY             = 0;
double dragThreshold             = 5;     // Threshold in pixels to start dragging
bool   eatchar                   = false; // NOTE Keychords could be implemented in scheme using this very simple approach
bool   electric_pair_mode        = true;  // TODO Wrap selection for () [] {} '' ""
bool   mark_electric_pairs_mode  = true;  // Automatically set-mark when inserting an electric pair

bool   auto_text_scale_mode      = false;
bool   blink_cursor_mode         = true;
float  blink_cursor_delay        = 0.5;   // Seconds of idle time before the first blink of the cursor.
float  blink_cursor_interval     = 0.5;   // Lenght of cursor blink interval in seconds.
int    blink_cursor_blinks       = 10;    // How many times to blink before stopping.
double lastBlinkTime             = 0.0;   // Last time the cursor state changed
bool   cursorVisible             = true;  // Initial state of the cursor visibility
int    blinkCount                = 0;     // Counter for number of blinks
size_t indentation               = 4;
size_t tab                       = 4;
bool   show_paren_mode           = true;
float  show_paren_delay          = 0.125; // TODO Time in seconds to delay before showing a matching paren.
int    kill_ring_max             = 120;   // Maximum length of kill ring before oldest elements are thrown away.
bool   electric_indent_mode      = true;  // TRUE if you want glemax to electric your indent style
bool   rainbow_mode              = true;  // TRUE if you are gae
bool   crystal_cursor_mode       = true;  // Make the cursor crystal clear
bool   hide_region_mode          = false;

bool   minimap_mode              = false;  
bool   minimap_cursor            = true;  
float  minimap_width             = 110;
bool   minimap_padding_mode      = true;  // Weather the minimap should have padding
size_t minimap_left_padding      = 8;     // Width in pexels

bool   fringe_mode               = true; 
size_t fringe                    = 8;     // Width in pexels
char   *first_theme_name         = "dark";
bool   color_fringe_mode         = true;

bool   hl_scope_mode             = true; 
float  hl_scope_base_brightness  = 0.8f;
float  hl_scope_brightness_step  = 0.1f;  // How much lighter/darker per level
// TODO 
/* bool   hl_scope_alpha            = true;  */
/* float  hl_scope_alpha_step       = 0.1f; */


bool   theme_lerp_mode           = true;
float  theme_lerp_speed          = 0.02f;
float  theme_lerp_threshold      = 1.0f;

bool   diff_hl_mode              = true;
bool   diff_hl_bg                = true; 
float  diff_hl_bg_alpha          = 0.5; 
bool   diff_hl_text              = true; 
bool   diff_hl_cursor            = true;

bool   scroll_lerp_mode          = true;  // Enable/disable animated scrolling && recentering
float  scroll_lerp_speed         = 0.06f; // Lerp scrolling Speed (0.0 to 1.0)


bool   mouse_wheel_lerp_mode     = true;
float  mouse_wheel_scroll_amount = 10;    // 2 TODO make it a vec2f for vertical and horizontal, and should it be size_t?
float  mouse_wheel_lerp_speed    = 0.06f; // (0.0 to 1.0)

bool   clock_mode                = true;
float  clockScale                = 3.0;
Vec2f  clockPosition             = {1860, 26};
char   *clock_shader             = "wave";

char   *buffer_shader            = "text";

char   *scratch_buffer_content   = ";; This buffer is for text that is not saved, and for Lisp evaluation.\n;; To create a file, visit it with 'C-x C-f' and enter text in its buffer.\n\n";

bool   mark_mode                 = true;

bool   region_fg_mode            = true;
bool   region_alpa_mode          = true;  // TODO

bool   lerp_line_mode            = false; // TODO It's garbage
float  lerp_line_duration        = 1.0;

bool   vertico_mode              = true;  // TODO
size_t vertico_max_lines         = 10;

bool   find_file_focus_existing  = false; // If true, focus the window that already contains the buffer you are trying to find


size_t max_gemini_redirections = 5;
/* bool highlight_nonselected_windows = false; // TODO each window highlights its own region. */

bool revert_buffer_mode = true; // TODO a watcher for buffer, if that buffer has a path
// TODO Buffers that don't "" have a path should *not* be watched






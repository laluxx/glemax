#include <stdbool.h>
#include <stdlib.h>

// NOTE We could parse this file line by line to get the default
// value of globals and their annotation (dumb way)

// double mouseX; NOTE Moved
// double mouseY; NOTE Moved

bool   dragging                  = false;
double initialMouseX             = 0;
double initialMouseY             = 0;
double dragThreshold             = 5;     // Threshold in pixels to start dragging
bool   eatchar                   = false; // NOTE Keychords could be implemented in scheme using this very simple approach
bool   electric_pair_mode        = true;  // TODO Wrap selection for () [] {} '' ""
bool   auto_text_scale_mode      = false;
bool   blink_cursor_mode         = true;
float  blink_cursor_delay        = 0.5;   // Seconds of idle time before the first blink of the cursor.
float  blink_cursor_interval     = 0.5;   // Lenght of cursor blink interval in seconds.
int    blink_cursor_blinks       = 10;    // How many times to blink before stopping.
double lastBlinkTime             = 0.0;   // Last time the cursor state changed
bool   cursorVisible             = true;  // Initial state of the cursor visibility
int    blinkCount                = 0;     // Counter for number of blinks
int    indentation               = 4;     
bool   show_paren_mode           = true;
float  show_paren_delay          = 0.125; // TODO Time in seconds to delay before showing a matching paren.
int    kill_ring_max             = 120;   // Maximum length of kill ring before oldest elements are thrown away.
bool   electric_indent_mode      = true;  // TRUE if you want glemax to electric your indent style
bool   rainbow_mode              = true;  // TRUE if you are gae
bool   crystal_cursor_mode       = true;  // Make the cursor crystal clear
float  mouse_wheel_scroll_amount = 2;     // TODO make it a vec2f for vertical and horizontal scrolling
bool   color_fringe_mode         = true;  
bool   hide_region_mode          = false;
bool   minimap_mode              = true;  
bool   minimap_cursor            = true;  
float  minimap_width             = 110;
bool   minimap_padding_mode      = true;  // Weather the minimap should have padding
size_t minimap_left_padding      = 8;     // Width in pexels
bool   fringe_mode               = true; 
size_t fringe                    = 8;     // Width in pexels
char   *first_theme_name         = "dark";
bool   hl_scope_mode             = true; 
float  hl_scope_base_brightness  = 0.8f;
float  hl_scope_brightness_step  = 0.1f;  // How muchg lighter/darker per level

bool   theme_lerp_mode           = true;
float  theme_lerp_speed          = 0.008f;
float  theme_lerp_threshold      = 1.0f;

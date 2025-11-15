#include <libguile.h>
#include <cglm/types.h>
#include <obsidian/font.h>
#include <obsidian/obsidian.h>
#include <obsidian/renderer.h>
#include "buffer.h"

#define ROPE_IMPLEMENTATION
#include "rope.h"


uint32_t sw = 1920;
uint32_t sh = 1080;

Font *jetbrains;
Font *lilex;

void text_callback(unsigned int codepoint) {
    if (codepoint >= 32 && codepoint < 127) {  // Printable ASCII
        insert(codepoint);
    }
}

static bool is_vertical_motion(KeyChordAction action) {
    return action == next_line || action == previous_line;
}

static bool is_argument_function(KeyChordAction action) {
    return action == universal_argument || 
           action == negative_argument || 
           action == digit_argument;
}


void before_keychord_hook(const char *notation, KeyChordBinding *binding) {
    // Clear minibuffer on any key press
    if (!wm.minibuffer_active) {
        Buffer *minibuf = wm.minibuffer_window->buffer;
        if (minibuf) {
            size_t len = rope_char_length(minibuf->rope);
            if (len > 0) {
                minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
                wm.minibuffer_window->point = 0;
            }
        }
    }
}


void after_keychord_hook(const char *notation, KeyChordBinding *binding) {
    reset_cursor_blink(buffer);
    update_window_scroll(wm.selected);
    if (binding->action != recenter_top_bottom) recenter_positions = 0;
    // Handle digit argument
    if (binding->action == digit_argument) {
        argument_manually_set = true;
        bool was_negative = false;
        
        // Remember current sign
        was_negative = (arg < 0);
        
        // If the last command was negative_argument and arg is still -1 or 1,
        // reset to 0 but preserve the sign (we're starting fresh)
        if (last_command == negative_argument && (arg == -1 || arg == 1)) {
            arg = 0;
        }
        // If the last command wasn't a digit or negative argument, reset completely
        else if (last_command != digit_argument && last_command != negative_argument) {
            arg = 0;
            was_negative = false;
        }
        
        if (was_negative) arg = -arg;  // Make it positive temporarily
        
        // Extract the digit from notation (e.g., "C-5" -> 5)
        const char *p = notation;
        while (*p) {
            if (*p >= '0' && *p <= '9') {
                int digit = *p - '0';
                
                // Check if multiplying by 10 would overflow
                if (arg <= INT_MAX / 10) {
                    int new_arg = arg * 10;
                    // Check if adding the digit would overflow
                    if (new_arg <= INT_MAX - digit) {
                        arg = new_arg + digit;
                    }
                    // else: silently ignore the digit (overflow would occur)
                }
                // else: silently ignore the digit (overflow would occur)
                break;
            }
            p++;
        }
        
        // Restore the sign
        if (was_negative) arg = -arg;
        
        // Display the current arg value
        char msg[32];
        snprintf(msg, sizeof(msg), "C-u %d", arg);
        message(msg);
        
        raw_prefix_arg = false;    
        
    } else if (!is_argument_function(binding->action)) {
        arg = 1;
        argument_manually_set = false;
        raw_prefix_arg = false;
    }
    
    if (!is_vertical_motion(binding->action)) update_goal_column();
    last_command_was_kill = is_kill_command(binding->action);
    last_command = binding->action;
}

void key_callback(int key, int action, int mods) {
    shift = mods & MOD_SHIFT;
    ctrl  = mods & MOD_CONTROL;
    alt   = mods & MOD_ALT;
    
    
    if (action == PRESS || action == REPEAT) {
        switch (key) {
        }
        
        reset_cursor_blink(buffer);
    }
}



double lastX = WIDTH / 2.0f, lastY = HEIGHT / 2.0f;

bool middleMousePressed = false;
bool rightMousePressed = false;
bool shiftMiddleMousePressed = false;
double lastPanX = 0.0, lastPanY = 0.0;
double lastOrbitX = 0.0, lastOrbitY = 0.0; 
vec3 orbitPivot = {0.0f, 0.0f, 0.0f};  // The point we're orbiting around
float orbitDistance = 10.0f;           // Distance from pivot


void mouse_button_callback(int button, int action, int mods) {
    // Only handle mouse buttons in editor mode (camera inactive)
    if (!camera.active) {
        // Middle mouse button - orbit/pan
        if (button == MOUSE_BUTTON_MIDDLE) {
            if (action == PRESS) {
                middleMousePressed = true;
                getCursorPos(context.window, &lastPanX, &lastPanY);
                getCursorPos(context.window, &lastOrbitX, &lastOrbitY);
                
                shiftMiddleMousePressed = (mods & MOD_SHIFT);
                
                // 1If not panning, calculate the pivot point for orbiting
                if (!shiftMiddleMousePressed) {
                    vec3 hitPoint;
                    bool hitGround = raycast_to_ground(&camera, hitPoint);
                    glm_vec3_copy(hitPoint, orbitPivot);
                    
                    // Calculate distance from camera to pivot
                    vec3 toPivot;
                    glm_vec3_sub(orbitPivot, camera.position, toPivot);
                    orbitDistance = glm_vec3_norm(toPivot);
                    
                    printf("Orbit pivot set to: (%.2f, %.2f, %.2f)\n", orbitPivot[0], orbitPivot[1], orbitPivot[2]);
                    printf("Orbit distance: %.2f\n", orbitDistance);
                }
                
            } else if (action == RELEASE) {
                middleMousePressed = false;
                shiftMiddleMousePressed = false;
                
                camera.use_look_at = false;
                printf("Orbit mode disabled - returning to normal camera control\n");
            }
        }
        
        // Right mouse button - freelook with WASD
        if (button == MOUSE_BUTTON_RIGHT) {
            if (action == PRESS) {
                rightMousePressed = true;
                // Get initial position BEFORE disabling cursor
                getCursorPos(context.window, &lastX, &lastY);
                setInputMode(context.window, CURSOR, CURSOR_DISABLED);
            } else if (action == RELEASE) {
                rightMousePressed = false;
                setInputMode(context.window, CURSOR, CURSOR_NORMAL);
            }
        }
    }
}

void cursor_pos_callback(double xpos, double ypos) {
    double xoffset = xpos - lastX;
    double yoffset = lastY - ypos;
    
    if (camera.active) {
        // Normal FPS camera control - always update lastX/lastY
        lastX = xpos;
        lastY = ypos;
        camera_process_mouse(&camera, xoffset, yoffset);
    }
    else if (rightMousePressed) {
        // Right mouse: freelook in editor mode - always update lastX/lastY
        lastX = xpos;
        lastY = ypos;
        camera_process_mouse(&camera, xoffset, yoffset);
    }
    else if (middleMousePressed) {
        if (shiftMiddleMousePressed) {
            if (camera.use_look_at) {
                camera_disable_orbit_mode(&camera);
                printf("Panning - orbit mode disabled\n");
            }
            // SHIFT + MIDDLE MOUSE: PAN
            
            float panSpeed = 0.005f;
            
            vec3 right, up;
            glm_vec3_cross(camera.front, camera.up, right);
            glm_vec3_normalize(right);
            glm_vec3_copy(camera.up, up);
            glm_vec3_normalize(up);
            
            vec3 panMovement = {0.0f, 0.0f, 0.0f};
            
            vec3 rightMove;
            glm_vec3_scale(right, (float)-xoffset * panSpeed, rightMove);
            glm_vec3_add(panMovement, rightMove, panMovement);
            
            vec3 upMove;
            glm_vec3_scale(up, (float)-yoffset * panSpeed, upMove);
            glm_vec3_add(panMovement, upMove, panMovement);
            
            glm_vec3_add(camera.position, panMovement, camera.position);
            
        } else {
            // MIDDLE MOUSE ONLY: ORBIT
            float orbitSpeed = 0.01f;
            camera_orbit_around_point(&camera, orbitPivot,
                                      (float)(xoffset * orbitSpeed),
                                      (float)(yoffset * orbitSpeed));
        }
        
        lastX = xpos;
        lastY = ypos;
    }
    else {
        // Not doing anything - just update last position to avoid jumps
        lastX = xpos;
        lastY = ypos;
    }
}



#include "wm.h"



static void inner_main (void *data, int argc, char **argv) {
    initWindow(sw, sh, "Kink");
    
    jetbrains = load_font("./assets/fonts/JetBrainsMonoNerdFont-Regular.ttf", 22);
    /* jetbrains = load_font("./assets/fonts/JetBrainsMonoNerdFont-Regular.ttf", 122); */
    /* lilex = load_font("./assets/fonts/LilexNerdFont-Regular.ttf", 22); */
    /* lilex = load_font("./assets/fonts/DejaVuMathTeXGyre.ttf", 100); */
    
    buffer = buffer_create(jetbrains);
    
    sh = context.swapChainExtent.height; // TODO move into
    sw = context.swapChainExtent.width;  // Resize callback
    wm_init(buffer, 0, 0, sw, sh);
    
    
    registerKeyCallback(key_callback);
    registerTextCallback(text_callback);
    registerMouseButtonCallback(mouse_button_callback);
    registerCursorPosCallback(cursor_pos_callback);
    
    register_after_keychord_hook(after_keychord_hook);
    register_before_keychord_hook(before_keychord_hook);
    
    
    loadThemeByName("molokai");
    
    keychord_bind(&keymap, "M--",           previousTheme,            "Previous theme",           PRESS | REPEAT);
    keychord_bind(&keymap, "M-=",           nextTheme,                "Next theme",               PRESS | REPEAT);
    
    keychord_bind(&keymap, "C-b",           backward_char,            "Backward char",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-f",           forward_char,             "Forward char",             PRESS | REPEAT);
    keychord_bind(&keymap, "C-n",           next_line,                "Next line",                PRESS | REPEAT);
    keychord_bind(&keymap, "C-p",           previous_line,            "Previous line",            PRESS | REPEAT);
    keychord_bind(&keymap, "<left>",        backward_char,            "Backward char",            PRESS | REPEAT);
    keychord_bind(&keymap, "<right>",       forward_char,             "Forward char",             PRESS | REPEAT);
    keychord_bind(&keymap, "<down>",        next_line,                "Next line",                PRESS | REPEAT);
    keychord_bind(&keymap, "<up>",          previous_line,            "Previous line",            PRESS | REPEAT);
    keychord_bind(&keymap, "M-f",           forward_word,             "Forward word",             PRESS | REPEAT);
    keychord_bind(&keymap, "M-b",           backward_word,            "Backward word",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-<right>",     forward_word,             "Forward word",             PRESS | REPEAT);
    keychord_bind(&keymap, "C-<left>",      backward_word,            "Backward word",            PRESS | REPEAT);
    keychord_bind(&keymap, "M-d",           kill_word,                "Kill word",                PRESS | REPEAT);
    keychord_bind(&keymap, "C-e",           end_of_line,              "End of line",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-a",           beginning_of_line,        "Beginning of line",        PRESS | REPEAT);
    keychord_bind(&keymap, "M-<",           beginning_of_buffer,      "Beginning of buffer",      PRESS | REPEAT);
    keychord_bind(&keymap, "C-c p",         beginning_of_buffer,      "Beginning of buffer",      PRESS | REPEAT);
    keychord_bind(&keymap, "M->",           end_of_buffer,            "End of buffer",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-c n",         end_of_buffer,            "End of buffer",            PRESS | REPEAT);
    keychord_bind(&keymap, "RET",           newline,                  "Newline",                  PRESS | REPEAT);
    keychord_bind(&keymap, "C-j",           newline,                  "Newline",                  PRESS | REPEAT);
    keychord_bind(&keymap, "C-m",           newline,                  "Newline",                  PRESS | REPEAT);
    keychord_bind(&keymap, "<backspace>",   delete_backward_char,     "Delete backward char",     PRESS | REPEAT);
    keychord_bind(&keymap, "C-<backspace>", backward_kill_word,       "Backward kill word",       PRESS | REPEAT);
    keychord_bind(&keymap, "C-d",           delete_char,              "Delete char",              PRESS | REPEAT);
    keychord_bind(&keymap, "<delete>",           delete_char,              "Delete char",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-o",           open_line,                "Open line",                PRESS | REPEAT);
    keychord_bind(&keymap, "C-M-o",         split_line,               "Split line",               PRESS | REPEAT);
    keychord_bind(&keymap, "C-SPC",         set_mark_command,         "Set mark command",         PRESS | REPEAT);
    keychord_bind(&keymap, "S-<backspace>", delete_region,            "Delete region",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-w",           kill_region,              "Kill region",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-y",           yank,                     "Yank",                     PRESS | REPEAT);
    keychord_bind(&keymap, "C-k",           kill_line,                "Kill line",                PRESS | REPEAT);
    keychord_bind(&keymap, "C-x C-x",       exchange_point_and_mark,  "Excange point and mark",   PRESS | REPEAT);
    
    keychord_bind(&keymap, "M-}",           forward_paragraph,        "Forward paragraph",        PRESS | REPEAT);
    keychord_bind(&keymap, "M-{",           backward_paragraph,       "Backward paragraph",       PRESS | REPEAT);
    keychord_bind(&keymap, "C-<down>",      forward_paragraph,        "Forward paragraph",        PRESS | REPEAT);
    keychord_bind(&keymap, "C-<up>",        backward_paragraph,       "Backward paragraph",       PRESS | REPEAT);
    
    
    keychord_bind(&keymap, "C-x 2",         split_window_below,       "Split window below",       PRESS | REPEAT);
    keychord_bind(&keymap, "C-x 3",         split_window_right,       "Split window right",       PRESS | REPEAT);
    keychord_bind(&keymap, "C-x 0",         delete_window,            "Delete window",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-x 1",         delete_other_windows,     "Delete other windows",     PRESS | REPEAT);
    keychord_bind(&keymap, "C-x o",         other_window,             "Other window",             PRESS | REPEAT);
    keychord_bind(&keymap, "C-x +",         balance_windows,          "Balance windows",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-x ^",         enlarge_window,           "Enlarge window",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-l",           recenter_top_bottom,      "Recenter top bottom",      PRESS | REPEAT);
    keychord_bind(&keymap, "C-v",           recenter,                 "Recenter",                 PRESS | REPEAT);
    
    keychord_bind(&keymap, "C-u",           universal_argument,       "Universal argument",       PRESS | REPEAT);
    keychord_bind(&keymap, "C--",           negative_argument,        "Negative argument",        PRESS | REPEAT);
    keychord_bind(&keymap, "C-0",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-1",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-2",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-3",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-4",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-5",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-6",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-7",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-8",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-9",           digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-x M-x",       digit_argument,           "Digit argument",           PRESS | REPEAT);
    keychord_bind(&keymap, "M-x",           execute_extended_command, "Execute extended command", PRESS | REPEAT);
    keychord_bind(&keymap, "C-g",           keyboard_quit,            "Keyboard quit",            PRESS | REPEAT);
    
    /* load_gltf("./assets/puta.glb", &scene); */
    /* load_gltf("./assets/floppy.glb", &scene); */
    
    
    while (!windowShouldClose()) {
        beginFrame();
        
        clear_background(CT.bg);
        
        fps(jetbrains, 500, 500, RED);
        
        wm_draw();
        
        endFrame();
    }
    
    wm_cleanup();
    buffer_destroy(buffer);
    destroy_font(jetbrains);
    destroy_font(lilex);
    
    
    cleanup(&context);
}


int main(int argc, char **argv) {
    scm_boot_guile (argc, argv, inner_main, 0);
    return 0; // never reached
}

#include <libguile.h>
#include <cglm/types.h>
#include <obsidian/font.h>
#include <obsidian/obsidian.h>
#include <obsidian/renderer.h>
#include "buffer.h"
#include "faces.h"
#include "wm.h"
#include "lisp.h"
#include "edit.h"

#define ROPE_IMPLEMENTATION
#include "rope.h"


uint32_t sw = 1920;
uint32_t sh = 1080;

/* Font *jetbrains; */
/* Font *lilex; */

void text_callback(unsigned int codepoint) {
    bool electric_pair_mode = scm_get_bool("electric-pair-mode", false);
    
    // Check if this is an opening pair character
    uint32_t closing_char = 0;
    bool should_pair = false;
    
    if (electric_pair_mode && codepoint < 128) {
        switch (codepoint) {
            case '(': closing_char = ')'; should_pair = true; break;
            case '[': closing_char = ']'; should_pair = true; break;
            case '{': closing_char = '}'; should_pair = true; break;
            case '<': closing_char = '>'; should_pair = true; break;
            case '"': closing_char = '"'; should_pair = true; break;
            case '\'': closing_char = '\''; should_pair = true; break;
            case '`': closing_char = '`'; should_pair = true; break;
        }
    }
    
    if (should_pair) {
        // Insert both characters as a single string
        char pair[3] = {(char)codepoint, (char)closing_char, 0};
        
        if (current_buffer->pt < current_buffer->region.mark) current_buffer->region.mark += 2;
        current_buffer->rope = rope_insert_chars(current_buffer->rope, current_buffer->pt, pair, 2);
        adjust_all_window_points_after_modification(current_buffer->pt, 2);
        set_point(current_buffer->pt + 1);  // Move to between the pair
        update_goal_column();
    } else {
        // No pair, just insert normally
        insert(codepoint);
    }
}

/* void text_callback(unsigned int codepoint) { */
/*     /\* if (codepoint >= 32 && codepoint < 127) {  // Printable ASCII *\/ */
/*         insert(codepoint); */
/*     /\* } *\/ */
/* } */


bool is_vertical_motion(SCM proc) {
    return is_scm_proc(proc, "next-line") ||
           is_scm_proc(proc, "previous-line");
}

bool is_argument_function(SCM proc) {
    return is_scm_proc(proc, "universal-argument") ||
           is_scm_proc(proc, "negative-argument") ||
           is_scm_proc(proc, "digit-argument");
}



void before_keychord_hook(const char *notation, KeyChordBinding *binding) {
    // Clear minibuffer on any key press
    if (!wm.minibuffer_active && binding->action.c_action != go_inside_minibuffer) {
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
    reset_cursor_blink(current_buffer);
    update_window_scroll(wm.selected);
    int arg = get_prefix_arg();



    if (!is_scm_proc(binding->action.scheme_proc, "recenter-top-bottom") &&
        !is_scm_proc(binding->action.scheme_proc, "move-to-window-line-top-bottom"))
        recenter_positions = 0;

    // Handle digit argument
    if (is_scm_proc(binding->action.scheme_proc, "digit-argument")) {
        argument_manually_set = true;
        bool was_negative = false;
        
        // Remember current sign
        was_negative = (arg < 0);
        
        // If the last command was negative_argument and arg is still -1 or 1,
        // reset to 0 but preserve the sign (we're starting fresh)
        if (is_scm_proc(last_command, "negative-argument") && (arg == -1 || arg == 1)) {
            arg = 0;
            set_prefix_arg(arg);
        }

        // If the last command wasn't a digit or negative argument, reset completely
        else if (!is_scm_proc(last_command, "digit-argument") && 
                 !is_scm_proc(last_command, "negative-argument")) {
            arg = 0;
            set_prefix_arg(arg);
            was_negative = false;
        }


        if (was_negative) arg = -arg;  // Make it positive temporarily
        set_prefix_arg(arg);
        
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
                        set_prefix_arg(arg);
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
        set_prefix_arg(arg);

        // Display the current arg value
        char msg[32];
        snprintf(msg, sizeof(msg), "C-u %d", arg);
        message(msg);
        
        set_raw_prefix_arg(false);
    } else if (!is_argument_function(binding->action.scheme_proc)) {
        arg = 1;
        set_prefix_arg(arg);
        argument_manually_set = false;
        set_raw_prefix_arg(false);
    }
    
    if (!is_vertical_motion(binding->action.scheme_proc)) update_goal_column();
    last_command_was_kill = is_kill_command(binding->action.scheme_proc);
    last_command = binding->action.scheme_proc;
}

void key_callback(int key, int action, int mods) {
    shift = mods & MOD_SHIFT;
    ctrl  = mods & MOD_CONTROL;
    alt   = mods & MOD_ALT;
    
    
    if (action == PRESS || action == REPEAT) {
        switch (key) {
        }
        
        reset_cursor_blink(current_buffer);
    }
}


void mouse_button_callback(int button, int action, int mods) {
}

double lastX = WIDTH / 2.0f, lastY = HEIGHT / 2.0f;

void cursor_pos_callback(double xpos, double ypos) {
    double xoffset = xpos - lastX;
    double yoffset = lastY - ypos;
}

static void inner_main (void *data, int argc, char **argv) {
    initWindow(sw, sh, "Kink");
    
    /* jetbrains = load_font("./assets/fonts/JetBrainsMonoNerdFont-Regular.ttf", 22); */

    /* jetbrains = load_font("./assets/fonts/JetBrainsMonoNerdFont-Medium.ttf", 22); */

    /* jetbrains = load_font("./assets/fonts/JetBrainsMonoNerdFont-Regular.ttf", 122); */
    /* lilex = load_font("./assets/fonts/LilexNerdFont-Regular.ttf", 22); */
    /* lilex = load_font("./assets/fonts/DejaVuMathTeXGyre.ttf", 100); */
    
    Buffer *scratch_buffer = buffer_create("*scratch*");
    Buffer *minibuf = buffer_create("minibuf");
    Buffer *messages = buffer_create("*Messages*");
    
    sh = context.swapChainExtent.height; // TODO move into
    sw = context.swapChainExtent.width;  // Resize callback
    wm_init(scratch_buffer, minibuf, 0, 0, sw, sh);

    init_faces();
    lisp_init(); // IMPORTANT After initializing the windowManager

    
    
    registerKeyCallback(key_callback);
    registerTextCallback(text_callback);
    registerMouseButtonCallback(mouse_button_callback);
    registerCursorPosCallback(cursor_pos_callback);
    
    register_after_keychord_hook(after_keychord_hook);
    register_before_keychord_hook(before_keychord_hook);
    
    
    keychord_bind(&keymap, "M--",           previousTheme,            "Previous theme",           PRESS | REPEAT);
    keychord_bind(&keymap, "M-=",           nextTheme,                "Next theme",               PRESS | REPEAT);
    keychord_bind(&keymap, "C-M-j",         go_inside_minibuffer,     "Go inside minibuffer",     PRESS | REPEAT);

    /* load_gltf("./assets/puta.glb", &scene); */
    /* load_gltf("./assets/floppy.glb", &scene); */
    
    while (!windowShouldClose()) {
        beginFrame();
        
        clear_background(face_cache->faces[FACE_DEFAULT]->bg);
        
        fps(face_cache->faces[FACE_DEFAULT]->font, sw - 400, 200, RED);

        wm_draw();
       
        endFrame();
    }
    
    wm_cleanup();
    buffer_destroy(scratch_buffer);
    /* destroy_font(jetbrains); */
    /* destroy_font(lilex); */
    
    
    cleanup(&context);
}


int main(int argc, char **argv) {
    scm_boot_guile (argc, argv, inner_main, 0);
    return 0; // never reached
}

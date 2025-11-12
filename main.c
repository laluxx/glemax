#include <obsidian/font.h>
#include <obsidian/keychords.h>
#include <obsidian/obsidian.h>
#include <obsidian/renderer.h>
#include <obsidian/theme.h>
#include <obsidian/vertico.h>
#include <obsidian/vulkan_setup.h>
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


void after_keychord_hook(const char *notation, KeyChordBinding *binding) {
    reset_cursor_blink(buffer);

    // Handle digit argument
    if (binding->action == digit_argument) {
        // Remember if arg was negative
        bool was_negative = (arg < 0);
        
        // If the last command wasn't a digit argument, reset to 0
        if (last_command != digit_argument && last_command != negative_argument) {
            arg = 0;
        } else if (last_command == negative_argument) {
            // If we just did C--, reset to 0 but keep the sign
            arg = 0;
        }
        
        if (was_negative) arg = -arg;  // Make it positive temporarily
        
        // Extract the digit from notation (e.g., "C-5" -> 5)
        const char *p = notation;
        while (*p) {
            if (*p >= '0' && *p <= '9') {
                int digit = *p - '0';
                // Multiply existing arg by 10 and add the digit
                arg = arg * 10 + digit;
                break;
            }
            p++;
        }
        
        // Restore the sign
        if (was_negative) arg = -arg;

        raw_prefix_arg = false;    
        
    } else if (!is_argument_function(binding->action)) {
        arg = 1;
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

        /* printf("ARG: %i\n", arg); */
        reset_cursor_blink(buffer);
    }
}

#include "wm.h"


int main() {
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
    register_after_keychord_hook(after_keychord_hook);


    loadThemeByName("modus-vivendi");

    keychord_bind(&keymap, "M--",           previousTheme,           "Previous theme",         PRESS | REPEAT);
    keychord_bind(&keymap, "M-=",           nextTheme,               "Next theme",             PRESS | REPEAT);

    keychord_bind(&keymap, "C-b",           backward_char,           "Backward char",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-f",           forward_char,            "Forward char",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-n",           next_line,               "Next line",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-p",           previous_line,           "Previous line",          PRESS | REPEAT);
    keychord_bind(&keymap, "<left>",        backward_char,           "Backward char",          PRESS | REPEAT);
    keychord_bind(&keymap, "<right>",       forward_char,            "Forward char",           PRESS | REPEAT);
    keychord_bind(&keymap, "<down>",        next_line,               "Next line",              PRESS | REPEAT);
    keychord_bind(&keymap, "<up>",          previous_line,           "Previous line",          PRESS | REPEAT);
    keychord_bind(&keymap, "M-f",           forward_word,            "Forward word",           PRESS | REPEAT);
    keychord_bind(&keymap, "M-b",           backward_word,           "Backward word",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-<right>",     forward_word,            "Forward word",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-<left>",      backward_word,           "Backward word",          PRESS | REPEAT);
    keychord_bind(&keymap, "M-d",           kill_word,               "Kill word",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-e",           end_of_line,             "End of line",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-a",           beginning_of_line,       "Beginning of line",      PRESS | REPEAT);
    keychord_bind(&keymap, "M-<",           beginning_of_buffer,     "Beginning of buffer",    PRESS | REPEAT);
    keychord_bind(&keymap, "C-c p",         beginning_of_buffer,     "Beginning of buffer",    PRESS | REPEAT);
    keychord_bind(&keymap, "M->",           end_of_buffer,           "End of buffer",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-c n",         end_of_buffer,           "End of buffer",          PRESS | REPEAT);
    keychord_bind(&keymap, "RET",           newline,                 "Newline",                PRESS | REPEAT);
    keychord_bind(&keymap, "C-j",           newline,                 "Newline",                PRESS | REPEAT);
    keychord_bind(&keymap, "C-m",           newline,                 "Newline",                PRESS | REPEAT);
    keychord_bind(&keymap, "<backspace>",   delete_backward_char,    "Delete backward char",   PRESS | REPEAT);
    keychord_bind(&keymap, "C-<backspace>", backward_kill_word,      "Backward kill word",     PRESS | REPEAT);
    keychord_bind(&keymap, "C-d",           delete_char,             "Delete char",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-o",           open_line,               "Open line",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-M-o",         split_line,              "Split line",             PRESS | REPEAT);
    keychord_bind(&keymap, "C-SPC",         set_mark_command,        "Set mark command",       PRESS | REPEAT);
    keychord_bind(&keymap, "S-<backspace>", delete_region,           "Delete region",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-w",           kill_region,             "Kill region",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-y",           yank,                    "Yank",                   PRESS | REPEAT);
    keychord_bind(&keymap, "C-k",           kill_line,               "Kill line",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-x C-x",       exchange_point_and_mark, "Excange point and mark", PRESS | REPEAT);

    keychord_bind(&keymap, "M-}",           forward_paragraph,       "Forward paragraph",      PRESS | REPEAT);
    keychord_bind(&keymap, "M-{",           backward_paragraph,      "Backward paragraph",     PRESS | REPEAT);
    keychord_bind(&keymap, "C-<down>",      forward_paragraph,       "Forward paragraph",      PRESS | REPEAT);
    keychord_bind(&keymap, "C-<up>",        backward_paragraph,      "Backward paragraph",     PRESS | REPEAT);


    keychord_bind(&keymap, "C-x 2",         split_window_below,      "Split window below",     PRESS | REPEAT);
    keychord_bind(&keymap, "C-x 3",         split_window_right,      "Split window right",     PRESS | REPEAT);
    keychord_bind(&keymap, "C-x 0",         delete_window,           "Delete window",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-x 1",         delete_other_windows,    "Delete other windows",   PRESS | REPEAT);
    keychord_bind(&keymap, "C-x o",         other_window,            "Other window",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-x +",         balance_windows,         "Balance windows",        PRESS | REPEAT);
    keychord_bind(&keymap, "C-x ^",         enlarge_window,          "Enlarge window",         PRESS | REPEAT);

    keychord_bind(&keymap, "C-u",           universal_argument,      "Universal argument",     PRESS | REPEAT);
    keychord_bind(&keymap, "C--",           negative_argument,       "Negative argument",      PRESS | REPEAT);
    keychord_bind(&keymap, "C-0",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-1",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-2",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-3",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-4",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-5",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-6",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-7",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-8",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-9",           digit_argument,          "Digit argument",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-x M-x",       digit_argument,          "Digit argument",         PRESS | REPEAT);


    while (!windowShouldClose()) {
        beginFrame();
        
        clear_background(CT.bg);

        wm_draw();

        quad2D((vec2){0,100}, (vec2){1,10}, BLUE);
        quad2D((vec2){0,0}, (vec2){1,1}, BLUE);
        
        endFrame();
    }
    
    wm_cleanup();
    buffer_destroy(buffer);
    destroy_font(jetbrains);
    destroy_font(lilex);
    
    cleanup(&context);
    
    return 0;
}

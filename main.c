#include <obsidian/font.h>
#include <obsidian/keychords.h>
#include <obsidian/obsidian.h>
#include <obsidian/theme.h>
#include <obsidian/vertico.h>
#include <obsidian/vulkan_setup.h>
#include "buffer.h"


#define ROPE_IMPLEMENTATION
#include "rope.h"


uint32_t sw = 1920;
uint32_t sh = 1080;

Font *jetbrains;

void text_callback(unsigned int codepoint) {
    if (codepoint >= 32 && codepoint < 127) {  // Printable ASCII
        insert(codepoint);
    }
}

void keychord_callback(const char *notation, KeyChordBinding *binding) {
    reset_cursor_blink(buffer);
    /* printf("Keychord: %s\n", notation); */
}


void key_callback(int key, int action, int mods) {
    bool shiftPressed = mods & MOD_SHIFT;
    bool ctrlPressed  = mods & MOD_CONTROL;
    bool altPressed   = mods & MOD_ALT;
    
    if (action == PRESS || action == REPEAT) {
        switch (key) {
        }
        
        reset_cursor_blink(buffer);
    }
}


int main() {
    initWindow(sw, sh, "Kink");

    jetbrains = load_font("./assets/fonts/JetBrainsMono-Regular.ttf", 100);
    /* jetbrains = load_font("./assets/fonts/DejaVuMathTeXGyre.ttf", 100); */

    buffer = buffer_create(jetbrains);
    
    registerKeyCallback(key_callback);
    registerTextCallback(text_callback);
    registerKeychordCallback(keychord_callback);

    sh = context.swapChainExtent.height; // TODO move into
    sw = context.swapChainExtent.width;  // Resize callback

    loadThemeByName("doom-one");

    keychord_bind(&keymap, "M--",         previousTheme,        "Previous theme",       PRESS | REPEAT);
    keychord_bind(&keymap, "M-=",         nextTheme,            "Next theme",           PRESS | REPEAT);
    keychord_bind(&keymap, "C-b",         backward_char,        "Backward char",        PRESS | REPEAT);
    keychord_bind(&keymap, "C-f",         forward_char,         "Forward char",         PRESS | REPEAT);
    keychord_bind(&keymap, "C-n",         next_line,            "Next line",            PRESS | REPEAT);
    keychord_bind(&keymap, "C-p",         previous_line,        "Previous line",        PRESS | REPEAT);
    keychord_bind(&keymap, "<left>",      backward_char,        "Backward char",        PRESS | REPEAT);
    keychord_bind(&keymap, "<right>",     forward_char,         "Forward char",         PRESS | REPEAT);
    keychord_bind(&keymap, "<down>",      next_line,            "Next line",            PRESS | REPEAT);
    keychord_bind(&keymap, "<up>",        previous_line,        "Previous line",        PRESS | REPEAT);
    keychord_bind(&keymap, "C-e",         end_of_line,          "End of line",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-a",         beginning_of_line,    "Beginning of line",    PRESS | REPEAT);
    keychord_bind(&keymap, "RET",         newline,              "Newline",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-j",         newline,              "Newline",              PRESS | REPEAT);
    keychord_bind(&keymap, "C-m",         newline,              "Newline",              PRESS | REPEAT);
    keychord_bind(&keymap, "<backspace>", delete_backward_char, "Delete backward char", PRESS | REPEAT);
    keychord_bind(&keymap, "C-d",         delete_char,          "Delete char",          PRESS | REPEAT);
    keychord_bind(&keymap, "C-o",         open_line,            "Open line",            PRESS | REPEAT);

    while (!windowShouldClose()) {
        beginFrame();
        
        clear_background(CT.bg);

        draw_buffer(buffer, 0, sh - buffer->font->ascent + buffer->font->descent);


        endFrame();
    }
    
    buffer_destroy(buffer);
    destroy_font(jetbrains);

    cleanup(&context);

    return 0;
}

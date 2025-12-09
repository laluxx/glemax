#include "minibuf.h"
#include "buffer.h"
#include "wm.h"
#include "rope.h"

void activate_minibuffer() {
    if (wm.minibuffer_active) return;
    
    wm.minibuffer_active = true;
    
    // Save window configuration before switching
    if (wm.saved_config.windows) free_window_configuration(&wm.saved_config);
    wm.saved_config = save_window_configuration();
    
    // Store the current window (for minibuffer highlight)
    wm.previous_window = wm.selected;
    
    // Switch to minibuffer
    wm.selected->is_selected = false;
    wm.minibuffer_window->is_selected = true;
    wm.selected = wm.minibuffer_window;
    current_buffer = wm.minibuffer_window->buffer;
    current_buffer->pt = wm.minibuffer_window->point;
}

void deactivate_minibuffer() {
    if (!wm.minibuffer_active) return;
    wm.minibuffer_active = false;
    
    // Clear minibuffer content
    if (wm.minibuffer_window->buffer) {
        size_t len = rope_char_length(wm.minibuffer_window->buffer->rope);
        if (len > 0) {
            wm.minibuffer_window->buffer->rope = rope_delete_chars(
                wm.minibuffer_window->buffer->rope, 0, len);
        }
        wm.minibuffer_window->point = 0;
    }
    
    wm.minibuffer_window->is_selected = false;
    
    /* // Restore saved window configuration */
    restore_window_configuration(&wm.saved_config);
    free_window_configuration(&wm.saved_config);
    
    wm.previous_window = NULL;
}

#include "faces.h"
void read_from_minibuffer(const char *prompt) {
    activate_minibuffer();
    wm.minibuffer_message_start = 0;  // No message yet
    
    Buffer *mb = wm.minibuffer_window->buffer;
    
    if (prompt) {
        size_t len = strlen(prompt);
        mb->rope = rope_insert_chars(mb->rope, 0, prompt, len);
        
        put_text_property(mb, 0, len, 
                         scm_from_locale_symbol("read-only"), 
                         SCM_BOOL_T);
        

        int msg_face = face_id_from_name("minibuffer-prompt");
        put_text_property(mb, 0, len,
                          scm_from_locale_symbol("face"),
                          scm_from_int(msg_face));

        put_text_property(mb, 0, len,
                          scm_from_locale_symbol("field"),
                          SCM_BOOL_T);


        mb->pt = rope_char_length(mb->rope);
        wm.minibuffer_window->point = mb->pt;
    }
}

void execute_extended_command() {
    read_from_minibuffer("M-x: ");
}

void eval_expression() {
    read_from_minibuffer("Eval: ");
}



void clear_minibuffer() {
    Buffer *minibuf = wm.minibuffer_window->buffer;
    if (!minibuf) return;
    
    if (wm.minibuffer_active) {
        // Clear only the echo area (the [...] message part)
        if (wm.minibuffer_message_start > 0) {
            size_t current_len = rope_char_length(minibuf->rope);
            if (current_len > wm.minibuffer_message_start) {
                size_t msg_len = current_len - wm.minibuffer_message_start;
                remove_text_properties(minibuf, wm.minibuffer_message_start, current_len);
                minibuf->rope = rope_delete_chars(minibuf->rope, 
                                                  wm.minibuffer_message_start, 
                                                  msg_len);
                wm.minibuffer_message_start = 0;  // Reset
            }
        }
    } else {
        // Clear entire minibuffer if not active
        size_t len = rope_char_length(minibuf->rope);
        if (len > 0) {
            clear_text_properties(minibuf);
            minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
            wm.minibuffer_window->point = 0;
        }
    }
}

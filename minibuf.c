#include "minibuf.h"
#include "buffer.h"
#include "edit.h"
#include "wm.h"
#include "rope.h"
#include "frame.h"
#include "faces.h"
#include "lisp.h"
#include <string.h>

// State for blocking minibuffer read
static bool minibuffer_exit_requested = false;
static bool minibuffer_abort_requested = false;
static char *minibuffer_result = NULL;

void activate_minibuffer() {
    if (selected_frame->wm.minibuffer_active) {
        message("Command attempted to use minibuffer while in minibuffer");
        return; // TODO Should we return from the calling function too?
    }
    
    selected_frame->wm.minibuffer_active = true;
    
    // Save window configuration before switching
    if (selected_frame->wm.saved_config.windows) free_window_configuration(&selected_frame->wm.saved_config);
    selected_frame->wm.saved_config = save_window_configuration();
    
    // Store the current window (for minibuffer highlight)
    selected_frame->wm.previous_window = selected_frame->wm.selected;
    
    // Switch to minibuffer
    selected_frame->wm.selected->is_selected = false;
    selected_frame->wm.minibuffer_window->is_selected = true;
    selected_frame->wm.selected = selected_frame->wm.minibuffer_window;
    current_buffer = selected_frame->wm.minibuffer_window->buffer;
    current_buffer->pt = selected_frame->wm.minibuffer_window->point;
    reset_cursor_blink(current_buffer);    
    // TODO Why the RET keybind is set for every mode ?
    SCM minibuffer_mode_func = scm_c_lookup("minibuffer-mode");
    scm_call_0(scm_variable_ref(minibuffer_mode_func));
}

void deactivate_minibuffer() {
    if (!selected_frame->wm.minibuffer_active) return;
    selected_frame->wm.minibuffer_active = false;
    
    // Clear minibuffer content
    if (selected_frame->wm.minibuffer_window->buffer) {
        size_t len = rope_char_length(selected_frame->wm.minibuffer_window->buffer->rope);
        if (len > 0) {
            // Clear text properties BEFORE deleting the text
            clear_text_properties(selected_frame->wm.minibuffer_window->buffer);
            
            selected_frame->wm.minibuffer_window->buffer->rope = rope_delete_chars(
                selected_frame->wm.minibuffer_window->buffer->rope, 0, len);
        }
        selected_frame->wm.minibuffer_window->point = 0;
    }
    
    // Force minibuffer to recalculate and reset to single line height
    selected_frame->wm.minibuffer_window->height = selected_frame->line_height;
    wm_recalculate_layout();
    
    selected_frame->wm.minibuffer_window->is_selected = false;
    
    // Restore saved window configuration
    restore_window_configuration(&selected_frame->wm.saved_config);
    free_window_configuration(&selected_frame->wm.saved_config);
    
    selected_frame->wm.previous_window = NULL;
}

void keyboard_quit() {
    deactivate_mark();
    if (selected_frame->wm.selected == selected_frame->wm.minibuffer_window) {
        // Signal abort from minibuffer
        minibuffer_abort_requested = true;
        minibuffer_exit_requested = true;
        if (minibuffer_result) {
            free(minibuffer_result);
            minibuffer_result = NULL;
        }
    }
}

// Extract minibuffer contents (after the prompt)
static char *get_minibuffer_contents() {
    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;
    if (!mb) return strdup("");
    
    size_t total_len = rope_char_length(mb->rope);
    
    // Find where the prompt ends (field property marks the prompt)
    size_t prompt_end = 0;
    SCM field_sym = scm_from_locale_symbol("field");
    for (size_t i = 0; i < total_len; i++) {
        SCM field = get_text_property(mb, i, field_sym);
        if (scm_is_false(field) || scm_is_null(field)) {
            prompt_end = i;
            break;
        }
    }
    
    // Extract text after prompt
    if (prompt_end >= total_len) {
        return strdup("");
    }
    
    size_t content_len = total_len - prompt_end;
    // Allocate buffer with extra space for UTF-8 (4 bytes per char max)
    size_t buffer_size = content_len * 4 + 1;
    char *buffer = malloc(buffer_size);
    if (!buffer) return strdup("");
    
    // Copy characters from rope
    size_t copied = rope_copy_chars(mb->rope, prompt_end, content_len, buffer, buffer_size - 1);
    buffer[copied] = '\0';
    
    return buffer;
}

void minibuffer_complete_and_exit() {
    if (!selected_frame->wm.minibuffer_active) return;
    
    // Get the user's input
    minibuffer_result = get_minibuffer_contents();
    minibuffer_exit_requested = true;
    minibuffer_abort_requested = false;
}

// Recursive edit - runs event loop until minibuffer exits
static void recursive_edit() {
    // Run the event loop until minibuffer_exit_requested becomes true
    while (!minibuffer_exit_requested && !windowShouldClose()) {
        beginFrame();
        
        // Draw everything
        clear_background(face_cache->faces[FACE_DEFAULT]->bg);
        wm_draw(&selected_frame->wm);
        
        endFrame();
    }
}

char *read_from_minibuffer(const char *prompt) {
    // Reset state
    minibuffer_exit_requested = false;
    minibuffer_abort_requested = false;
    if (minibuffer_result) {
        free(minibuffer_result);
        minibuffer_result = NULL;
    }
    
    // Setup minibuffer
    activate_minibuffer();
    selected_frame->wm.minibuffer_message_start = 0;
    
    Buffer *mb = selected_frame->wm.minibuffer_window->buffer;
    
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
        selected_frame->wm.minibuffer_window->point = mb->pt;
    }
    
    // Enter recursive edit loop - this blocks until user presses RET or C-g
    recursive_edit();
    
    // After exit, deactivate and get result
    deactivate_minibuffer();
    
    char *result;
    if (minibuffer_abort_requested) {
        // User pressed C-g, return empty string
        result = strdup("");
    } else {
        // User pressed RET, return the input
        result = minibuffer_result ? minibuffer_result : strdup("");
    }
    
    // Clean up
    minibuffer_result = NULL;
    minibuffer_exit_requested = false;
    minibuffer_abort_requested = false;
    
    return result;
}

void execute_extended_command() {
    char *input = read_from_minibuffer("M-x: ");
    
    if (input && *input) {
        // TODO: Look up and execute the command
        // For now, just print what was entered
        message("Would execute: %s", input);
    }
    
    free(input);
}

void eval_expression() {
    char *input = read_from_minibuffer("Eval: ");
    
    if (input && *input) {
        // Evaluate with error handling
        SCM result = scm_internal_catch(SCM_BOOL_T,
                                        eval_string_body, input,
                                        error_handler, NULL);
        
        // Display result (either value or error message)
        char *result_str = scm_to_locale_string(scm_object_to_string(result, SCM_UNDEFINED));
        message(result_str);
        free(result_str);
    }
    
    free(input);
}

void clear_minibuffer_message() {
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    if (!minibuf) return;
    
    if (selected_frame->wm.minibuffer_active) {
        // Clear only the echo area (the [...] message part)
        if (selected_frame->wm.minibuffer_message_start > 0) {
            size_t current_len = rope_char_length(minibuf->rope);
            if (current_len > selected_frame->wm.minibuffer_message_start) {
                size_t msg_len = current_len - selected_frame->wm.minibuffer_message_start;
                remove_text_properties(minibuf, selected_frame->wm.minibuffer_message_start, current_len);
                minibuf->rope = rope_delete_chars(minibuf->rope, 
                                                  selected_frame->wm.minibuffer_message_start, 
                                                  msg_len);
                selected_frame->wm.minibuffer_message_start = 0;
            }
        }
    } else {
        // Clear entire minibuffer if not active
        size_t len = rope_char_length(minibuf->rope);
        if (len > 0) {
            clear_text_properties(minibuf);
            minibuf->rope = rope_delete_chars(minibuf->rope, 0, len);
            selected_frame->wm.minibuffer_window->point = 0;
        }
    }
}

/// SCM bindings

static SCM scm_read_from_minibuffer(SCM prompt) {
    if (!scm_is_string(prompt)) {
        scm_wrong_type_arg("read-from-minibuffer", 1, prompt);
    }
    
    char *prompt_str = scm_to_locale_string(prompt);
    char *result = read_from_minibuffer(prompt_str);
    free(prompt_str);
    
    SCM result_scm = scm_from_locale_string(result);
    free(result);
    
    return result_scm;
}

static SCM scm_execute_extended_command(void) {
    execute_extended_command();
    return SCM_UNSPECIFIED;
}

static SCM scm_eval_expression(void) {
    eval_expression();
    return SCM_UNSPECIFIED;
}

static SCM scm_keyboard_quit(void) {
    keyboard_quit();
    return SCM_UNSPECIFIED;
}

static SCM scm_clear_minibuffer_message(void) {
    clear_minibuffer_message();
    return SCM_UNSPECIFIED;
}

static SCM scm_minibuffer_complete_and_exit(void) {
    minibuffer_complete_and_exit();
    return SCM_UNSPECIFIED;
}

void init_minibuf_bindings(void) {
    scm_c_define_gsubr("read-from-minibuffer",            1, 0, 0, scm_read_from_minibuffer);
    scm_c_define_gsubr("execute-extended-command",        0, 0, 0, scm_execute_extended_command);
    scm_c_define_gsubr("eval-expression",                 0, 0, 0, scm_eval_expression);
    scm_c_define_gsubr("keyboard-quit",                   0, 0, 0, scm_keyboard_quit);
    scm_c_define_gsubr("clear-minibuffer-message",        0, 0, 0, scm_clear_minibuffer_message);
    scm_c_define_gsubr("minibuffer-complete-and-exit",    0, 0, 0, scm_minibuffer_complete_and_exit);
}

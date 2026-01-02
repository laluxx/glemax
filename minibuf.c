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
        return;
    }
    
    selected_frame->wm.minibuffer_active = true;
    
    // Save window configuration before switching
    if (selected_frame->wm.saved_config.windows) free_window_configuration(&selected_frame->wm.saved_config);
    selected_frame->wm.saved_config = save_window_configuration();
    
    // Store the current window (for minibuffer highlight)
    selected_frame->wm.previous_window = selected_frame->wm.selected;
    
    // IMPORTANT: Reset all keychord states when entering minibuffer
    keychord_reset_state(&keymap);
    
    // Switch to minibuffer
    selected_frame->wm.selected->is_selected = false;
    selected_frame->wm.minibuffer_window->is_selected = true;
    selected_frame->wm.selected = selected_frame->wm.minibuffer_window;
    current_buffer = selected_frame->wm.minibuffer_window->buffer;
    current_buffer->pt = selected_frame->wm.minibuffer_window->point;
    reset_cursor_blink(current_buffer);    
    
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
    message("Quit");
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

char *read_from_minibuffer(const char *prompt, const char *initial_contents) {
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
    size_t pos = 0;
    
    // Insert prompt (read-only with special face)
    if (prompt) {
        size_t len = strlen(prompt);
        mb->rope = rope_insert_chars(mb->rope, pos, prompt, len);
        
        put_text_property(mb, pos, len, 
                         scm_from_locale_symbol("read-only"), 
                         SCM_BOOL_T);
        
        int msg_face = face_id_from_name("minibuffer-prompt");
        put_text_property(mb, pos, len,
                          scm_from_locale_symbol("face"),
                          scm_from_int(msg_face));

        put_text_property(mb, pos, len,
                          scm_from_locale_symbol("field"),
                          SCM_BOOL_T);
        
        pos += len;
    }
    
    // Insert initial contents (editable, normal face, no field property)
    if (initial_contents && *initial_contents) {
        size_t len = strlen(initial_contents);
        mb->rope = rope_insert_chars(mb->rope, pos, initial_contents, len);
        pos += len;
    }
    
    // Set point after all content
    mb->pt = pos;
    selected_frame->wm.minibuffer_window->point = mb->pt;
    
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
    char *input = read_from_minibuffer("M-x: ", NULL);
    
    if (input && *input) {
        // TODO: Look up and execute the command
        // For now, just print what was entered
        message("Would execute: %s", input);
    }
    
    free(input);
}

void eval_expression() {
    char *input = read_from_minibuffer("Eval: ", NULL);
    
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

#include <sys/stat.h>
#include <libgen.h>  // for dirname/basename

// Helper to check if file exists
static bool file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

// Helper to read file contents
static char* read_file_contents(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    char *contents = malloc(size + 1);
    if (!contents) {
        fclose(f);
        return NULL;
    }
    
    size_t read = fread(contents, 1, size, f);
    contents[read] = '\0';
    fclose(f);
    
    if (out_len) *out_len = read;
    return contents;
}

// Helper to extract directory from path
static char* get_directory_from_path(const char *path) {
    char *path_copy = strdup(path);
    char *dir = dirname(path_copy);
    char *result = strdup(dir);
    free(path_copy);
    return result;
}

// Helper to extract filename from path
static char* get_filename_from_path(const char *path) {
    char *path_copy = strdup(path);
    char *base = basename(path_copy);
    char *result = strdup(base);
    free(path_copy);
    return result;
}

#include <pwd.h>
#include <unistd.h>

// Helper to expand ~ to home directory
static char* expand_tilde(const char *path) {
    if (path[0] != '~') {
        return strdup(path);
    }
    
    const char *home;
    const char *rest = path + 1;
    
    if (path[1] == '/' || path[1] == '\0') {
        // ~/... or just ~
        home = getenv("HOME");
        if (!home) {
            struct passwd *pw = getpwuid(getuid());
            home = pw ? pw->pw_dir : ".";
        }
    } else {
        // ~username/...
        const char *slash = strchr(path, '/');
        size_t username_len = slash ? (slash - path - 1) : strlen(path + 1);
        char username[256];
        strncpy(username, path + 1, username_len);
        username[username_len] = '\0';
        
        struct passwd *pw = getpwnam(username);
        if (!pw) {
            return strdup(path); // Can't expand, return as-is
        }
        home = pw->pw_dir;
        rest = slash ? slash : "";
    }
    
    char *result = malloc(strlen(home) + strlen(rest) + 1);
    sprintf(result, "%s%s", home, rest);
    return result;
}


// Helper to abbreviate home directory to ~
static char* abbreviate_home(const char *path) {
    const char *home = getenv("HOME");
    if (!home) {
        struct passwd *pw = getpwuid(getuid());
        home = pw ? pw->pw_dir : NULL;
    }
    
    if (!home) {
        return strdup(path);
    }
    
    size_t home_len = strlen(home);
    if (strncmp(path, home, home_len) == 0 && 
        (path[home_len] == '/' || path[home_len] == '\0')) {
        // Path starts with home directory
        char *result = malloc(strlen(path) - home_len + 2);
        sprintf(result, "~%s", path + home_len);
        return result;
    }
    
    return strdup(path);
}

Buffer* find_file(const char *filename) {
    char *input = NULL;
    bool should_free_input = false;
    
    if (filename == NULL || *filename == '\0') {
        // Interactive mode - prompt user
        const char *default_dir = current_buffer->directory ? 
                                  current_buffer->directory : ".";
        
        char *display_dir = abbreviate_home(default_dir);
        char initial[PATH_MAX];
        snprintf(initial, PATH_MAX, "%s/", display_dir);
        free(display_dir);
        
        input = read_from_minibuffer("Find file: ", initial);
        should_free_input = true;
        
        if (!input || !*input) {
            if (input) free(input);
            return NULL;
        }
    } else {
        // Programmatic mode - use provided filename
        input = (char*)filename;
    }
    
    // Expand tilde if present
    char *expanded_input = expand_tilde(input);
    if (should_free_input) free(input);
    
    // Combine directory with input if input is relative
    char full_path[PATH_MAX];
    if (expanded_input[0] == '/') {
        snprintf(full_path, PATH_MAX, "%s", expanded_input);
    } else {
        const char *default_dir = current_buffer->directory ? 
                                  current_buffer->directory : ".";
        snprintf(full_path, PATH_MAX, "%s/%s", default_dir, expanded_input);
    }
    free(expanded_input);
    
    // Resolve to absolute path
    char absolute_path[PATH_MAX];
    if (realpath(full_path, absolute_path) == NULL) {
        snprintf(absolute_path, PATH_MAX, "%s", full_path);
    }
    
    // Check if buffer already exists for this file
    Buffer *buf = all_buffers;
    if (buf) {
        do {
            if (buf->filename && strcmp(buf->filename, absolute_path) == 0) {
                switch_to_buffer(buf);
                message("Switched to existing buffer: %s", buf->name);
                return buf;
            }
            buf = buf->next;
        } while (buf != all_buffers);
    }
    
    // Create buffer name from filename
    char *base_name = get_filename_from_path(absolute_path);
    char *buffer_name = strdup(base_name);
    int suffix = 1;
    while (get_buffer(buffer_name)) {
        free(buffer_name);
        buffer_name = malloc(strlen(base_name) + 20);
        sprintf(buffer_name, "%s<%d>", base_name, suffix++);
    }
    
    // Create new buffer
    buf = buffer_create(buffer_name);
    free(buffer_name);
    free(base_name);
    
    // Set filename and directory
    buf->filename = strdup(absolute_path);
    if (buf->directory) free(buf->directory);
    buf->directory = get_directory_from_path(absolute_path);
    
    // Try to read file contents
    if (file_exists(absolute_path)) {
        size_t file_len;
        char *contents = read_file_contents(absolute_path, &file_len);
        
        if (contents) {
            buf->rope = rope_insert_chars(buf->rope, 0, contents, file_len);
            buf->modified = false;
            free(contents);
            message("Read %zu characters from %s", file_len, absolute_path);
        } else {
            message("Error reading file: %s", absolute_path);
        }
    } else {
        message("(New file)");
        buf->modified = false;
    }
    
    // Switch to the new buffer
    switch_to_buffer(buf);
    buf->pt = 0;
    
    // Run find-file-hook (which includes set-auto-mode)
    SCM find_file_hook = scm_c_lookup("find-file-hook");
    if (!scm_is_false(scm_variable_bound_p(find_file_hook))) {
        SCM hooks = scm_variable_ref(find_file_hook);
        if (scm_is_true(scm_list_p(hooks))) {
            // Run each hook function
            while (!scm_is_null(hooks)) {
                SCM hook = scm_car(hooks);
                if (scm_is_true(scm_procedure_p(hook))) {
                    scm_call_0(hook);
                }
                hooks = scm_cdr(hooks);
            }
        }
    }

    return buf;
}


/* void find_file() { */
/*     // Get current directory as default */
/*     const char *default_dir = current_buffer->directory ?  */
/*                               current_buffer->directory : "."; */
    
/*     // Abbreviate home directory for initial contents */
/*     char *display_dir = abbreviate_home(default_dir); */
    
/*     // Create initial contents with trailing slash */
/*     char initial[PATH_MAX]; */
/*     snprintf(initial, PATH_MAX, "%s/", display_dir); */
/*     free(display_dir); */
    
/*     // Simple prompt, directory is in initial contents (editable) */
/*     char *input = read_from_minibuffer("Find file: ", initial); */
    
/*     if (!input || !*input) { */
/*         free(input); */
/*         return; */
/*     } */
    
/*     // Expand tilde if present */
/*     char *expanded_input = expand_tilde(input); */
/*     free(input); */
    
/*     // Combine directory with input if input is relative */
/*     char full_path[PATH_MAX]; */
/*     if (expanded_input[0] == '/') { */
/*         // Absolute path */
/*         snprintf(full_path, PATH_MAX, "%s", expanded_input); */
/*     } else { */
/*         // Relative path - use default_dir */
/*         snprintf(full_path, PATH_MAX, "%s/%s", default_dir, expanded_input); */
/*     } */
/*     free(expanded_input); */
    
/*     // Resolve to absolute path */
/*     char absolute_path[PATH_MAX]; */
/*     if (realpath(full_path, absolute_path) == NULL) { */
/*         // If realpath fails, might be a new file - use full_path as-is */
/*         snprintf(absolute_path, PATH_MAX, "%s", full_path); */
/*     } */
    
/*     // Check if buffer already exists for this file */
/*     Buffer *buf = all_buffers; */
/*     if (buf) { */
/*         do { */
/*             if (buf->filename && strcmp(buf->filename, absolute_path) == 0) { */
/*                 // Buffer already exists, just switch to it */
/*                 switch_to_buffer(buf); */
/*                 message("Switched to existing buffer: %s", buf->name); */
/*                 return; */
/*             } */
/*             buf = buf->next; */
/*         } while (buf != all_buffers); */
/*     } */
    
/*     // Create buffer name from filename */
/*     char *base_name = get_filename_from_path(absolute_path); */
    
/*     // Check for name conflicts and create unique name if needed */
/*     char *buffer_name = strdup(base_name); */
/*     int suffix = 1; */
/*     while (get_buffer(buffer_name)) { */
/*         free(buffer_name); */
/*         buffer_name = malloc(strlen(base_name) + 20); */
/*         sprintf(buffer_name, "%s<%d>", base_name, suffix++); */
/*     } */
    
/*     // Create new buffer */
/*     buf = buffer_create(buffer_name); */
/*     free(buffer_name); */
/*     free(base_name); */
    
/*     // Set filename and directory (always stored as absolute paths) */
/*     buf->filename = strdup(absolute_path); */
/*     if (buf->directory) free(buf->directory); */
/*     buf->directory = get_directory_from_path(absolute_path); */
    
/*     // Try to read file contents */
/*     if (file_exists(absolute_path)) { */
/*         size_t file_len; */
/*         char *contents = read_file_contents(absolute_path, &file_len); */
        
/*         if (contents) { */
/*             // Insert file contents into buffer */
/*             buf->rope = rope_insert_chars(buf->rope, 0, contents, file_len); */
/*             buf->modified = false;  // Just loaded, not modified yet */
/*             free(contents); */
/*             message("Read %zu characters from %s", file_len, absolute_path); */
/*         } else { */
/*             message("Error reading file: %s", absolute_path); */
/*         } */
/*     } else { */
/*         message("(New file)"); */
/*         buf->modified = false;  // New file, not modified yet */
/*     } */
    
/*     // Switch to the new buffer */
/*     switch_to_buffer(buf); */
    
/*     // Set point to beginning */
/*     buf->pt = 0; */
    
/*     // TODO: Run find-file-hook, auto-mode detection, etc. */
/* } */

void clear_minibuffer_message() {
    Buffer *minibuf = selected_frame->wm.minibuffer_window->buffer;
    
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

static SCM scm_read_from_minibuffer(SCM prompt, SCM initial) {
    if (!scm_is_string(prompt)) {
        scm_wrong_type_arg("read-from-minibuffer", 1, prompt);
    }
    
    char *prompt_str = scm_to_locale_string(prompt);
    char *initial_str = NULL;
    
    if (!SCM_UNBNDP(initial) && scm_is_string(initial)) {
        initial_str = scm_to_locale_string(initial);
    }
    
    char *result = read_from_minibuffer(prompt_str, initial_str);
    free(prompt_str);
    if (initial_str) free(initial_str);
    
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


static SCM scm_find_file(SCM filename) {
    char *filename_str = NULL;
    
    // Check if filename argument was provided
    if (!SCM_UNBNDP(filename)) {
        if (!scm_is_string(filename)) {
            scm_wrong_type_arg("find-file", 1, filename);
        }
        filename_str = scm_to_locale_string(filename);
    }
    
    Buffer *buf = find_file(filename_str);
    
    if (filename_str) free(filename_str);
    
    if (buf) {
        return get_or_make_buffer_object(buf);
    }
    
    return SCM_BOOL_F;
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
    scm_c_define_gsubr("read-from-minibuffer",            1, 1, 0, scm_read_from_minibuffer);
    scm_c_define_gsubr("execute-extended-command",        0, 0, 0, scm_execute_extended_command);
    scm_c_define_gsubr("eval-expression",                 0, 0, 0, scm_eval_expression);
    scm_c_define_gsubr("find-file",                       0, 1, 0, scm_find_file);
    scm_c_define_gsubr("keyboard-quit",                   0, 0, 0, scm_keyboard_quit);
    scm_c_define_gsubr("clear-minibuffer-message",        0, 0, 0, scm_clear_minibuffer_message);
    scm_c_define_gsubr("minibuffer-complete-and-exit",    0, 0, 0, scm_minibuffer_complete_and_exit);
}

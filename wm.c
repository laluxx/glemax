#include "wm.h"
#include <stdlib.h>
#include <string.h>

WindowManager wm = {0};

#define DEFAULT_SPLIT_RATIO 0.5f
#define MIN_WINDOW_SIZE 100.0f

static bool debug_them_windows = false;
size_t fringe_width = 8;


Window* window_create(Window *parent, Buffer *buffer) {
    Window *win = (Window*)calloc(1, sizeof(Window));
    if (!win) return NULL;
    
    win->parent = parent;
    win->buffer = buffer;
    win->split_type = SPLIT_NONE;
    win->split_ratio = DEFAULT_SPLIT_RATIO;
    win->is_selected = false;
    win->is_minibuffer = false;
   
    win->point = 0;
    
    return win;
}

void window_destroy(Window *win) {
    if (!win) return;
    
    // Recursively destroy children
    if (win->left) window_destroy(win->left);
    if (win->right) window_destroy(win->right);
    
    // NOTE: We don't destroy the buffer here
    free(win);
}

bool is_leaf_window(Window *win) {
    return win && win->split_type == SPLIT_NONE;
}

bool is_minibuffer_window(Window *win) {
    return win && win->is_minibuffer;
}

void wm_init(Buffer *initial_buffer, Buffer *minibuffer,float x, float y, float width, float height) {
    wm.minibuffer_window = window_create(NULL, minibuffer);
    wm.minibuffer_window->is_minibuffer = true;
    wm.minibuffer_window->x = x;
    wm.minibuffer_window->y = y;
    wm.minibuffer_window->scrollx = x;
    wm.minibuffer_window->scrolly = y;

    wm.minibuffer_window->width = width;
    wm.minibuffer_window->height = 0;
    
    // Create root window
    wm.root = window_create(NULL, initial_buffer);
    if (!wm.root) return;
    
    wm.root->x = x;
    wm.root->y = y;
    wm.root->width = width;
    wm.root->height = height;
    wm.root->is_selected = true;
    
    wm.selected = wm.root;
    wm.window_count = 1;
    wm.minibuffer_active = false;
}

void wm_cleanup() {
    if (wm.root) {
        window_destroy(wm.root);
        wm.root = NULL;
    }
    if (wm.minibuffer_window) {
        buffer_destroy(wm.minibuffer_window->buffer);
        free(wm.minibuffer_window);
        wm.minibuffer_window = NULL;
    }
    wm.selected = NULL;
    wm.window_count = 0;
}

// Collect all leaf windows in left-to-right, top-to-bottom order
// EXCLUDES minibuffer unless it's active
void collect_leaf_windows(Window *win, Window **leaves, int *count) {
    if (!win) return;
    
    if (is_leaf_window(win)) {
        leaves[(*count)++] = win;
    } else {
        collect_leaf_windows(win->left, leaves, count);
        collect_leaf_windows(win->right, leaves, count);
    }
}

int count_windows() {
    if (!wm.root) return 0;
    
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    return count;
}


Window* next_window(Window *current) {
    if (!current || !wm.root) return NULL;
    
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    
    // If minibuffer is active, include it at the end
    if (wm.minibuffer_active && wm.minibuffer_window) {
        leaves[count++] = wm.minibuffer_window;
    }
    
    if (count <= 1) return current;
    
    // Find current window in the list
    for (int i = 0; i < count; i++) {
        if (leaves[i] == current) {
            return leaves[(i + 1) % count];  // Wrap
        }
    }
    
    return leaves[0];  // Fallback
}

Window* previous_window(Window *current) {
    if (!current || !wm.root) return NULL;
    
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    
    // If minibuffer is active, include it at the end
    if (wm.minibuffer_active && wm.minibuffer_window) {
        leaves[count++] = wm.minibuffer_window;
    }
    
    if (count <= 1) return current;
    
    // Find current window in the list
    for (int i = 0; i < count; i++) {
        if (leaves[i] == current) {
            return leaves[(i - 1 + count) % count];  // Wrap
        }
    }
    
    return leaves[0];  // Fallback
}

void other_window() {
    wm.selected->point = current_buffer->pt;
    
    Window *original = wm.selected;
    Window *target = wm.selected;
    
    int count = abs(arg);
    
    if (arg > 0) {
        for (int i = 0; i < count; i++) {
            target = next_window(target);
        }
    } else {
        for (int i = 0; i < count; i++) {
            target = previous_window(target);
        }
    }
    
    if (target == original) {
        message("No other window to select");
        return;
    }
    
    if (target && target != wm.selected) {
        wm.selected->is_selected = false;
        target->is_selected = true;
        wm.selected = target;
        
        // Update global buffer pointer and restore point
        current_buffer = target->buffer;
        current_buffer->pt = target->point;
    }
    
    if (debug_them_windows) debug_print_windows();
}

static void save_window_recursive(Window *win, WindowSnapshot *snapshots, int *count, int my_index) {
    if (!win) return;
    
    snapshots[my_index].split_type = win->split_type;
    snapshots[my_index].split_ratio = win->split_ratio;
    snapshots[my_index].buffer = win->buffer;
    snapshots[my_index].point = win->point;
    snapshots[my_index].is_selected = win->is_selected;
    snapshots[my_index].scrollx = win->scrollx;
    snapshots[my_index].scrolly = win->scrolly;
    
    if (is_leaf_window(win)) {
        snapshots[my_index].left_index = -1;
        snapshots[my_index].right_index = -1;
    } else {
        // Reserve slots for children
        int left_idx = (*count)++;
        int right_idx = (*count)++;
        
        snapshots[my_index].left_index = left_idx;
        snapshots[my_index].right_index = right_idx;
        
        // Recursively save children
        save_window_recursive(win->left, snapshots, count, left_idx);
        save_window_recursive(win->right, snapshots, count, right_idx);
    }
}

static WindowConfiguration save_window_configuration() {
    WindowConfiguration config = {0};
    
    // Allocate space for maximum possible windows (should be enough)
    config.windows = (WindowSnapshot*)calloc(32, sizeof(WindowSnapshot));
    config.count = 1;  // Start with root at index 0
    config.root_index = 0;
    
    // Save root geometry
    config.root_x = wm.root->x;
    config.root_y = wm.root->y;
    config.root_width = wm.root->width;
    config.root_height = wm.root->height;
    
    save_window_recursive(wm.root, config.windows, &config.count, 0);
    
    return config;
}

static Window* restore_window_recursive(WindowSnapshot *snapshots, int index, Window *parent) {
    if (index < 0) return NULL;
    
    WindowSnapshot *snap = &snapshots[index];
    Window *win = window_create(parent, snap->buffer);
    
    win->split_type = snap->split_type;
    win->split_ratio = snap->split_ratio;
    win->point = snap->point;
    win->is_selected = snap->is_selected;
    win->scrollx = snap->scrollx;
    win->scrolly = snap->scrolly;
    
    if (snap->left_index >= 0) {
        win->left = restore_window_recursive(snapshots, snap->left_index, win);
    }
    if (snap->right_index >= 0) {
        win->right = restore_window_recursive(snapshots, snap->right_index, win);
    }
    
    return win;
}


static void restore_window_configuration(WindowConfiguration *config) {
    if (!config->windows) return;
    
    // Destroy current window tree
    if (wm.root) {
        window_destroy(wm.root);
    }
    
    // Restore from snapshot
    wm.root = restore_window_recursive(config->windows, config->root_index, NULL);
    
    // Restore saved root geometry
    wm.root->x = config->root_x;
    wm.root->y = config->root_y;
    wm.root->width = config->root_width;
    wm.root->height = config->root_height;
    
    // Find selected window
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    
    wm.selected = NULL;
    for (int i = 0; i < count; i++) {
        if (leaves[i]->is_selected) {
            wm.selected = leaves[i];
            break;
        }
    }
    
    if (!wm.selected && count > 0) {
        wm.selected = leaves[0];
        wm.selected->is_selected = true;
    }
    
    wm.window_count = count;
    current_buffer = wm.selected->buffer;
    current_buffer->pt = wm.selected->point;
    
    wm_recalculate_layout();
}

static void free_window_configuration(WindowConfiguration *config) {
    if (config->windows) {
        free(config->windows);
        config->windows = NULL;
    }
    config->count = 0;
}

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


void go_inside_minibuffer() {
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

static void recalculate_window_geometry(Window *win) {
    if (!win || is_leaf_window(win)) return;
    
    if (win->split_type == SPLIT_VERTICAL) {
        float split_x = win->x + win->width * win->split_ratio;
        
        if (win->left) {
            win->left->x = win->x;
            win->left->y = win->y;
            win->left->width = win->width * win->split_ratio;
            win->left->height = win->height;
            recalculate_window_geometry(win->left);
        }
        
        if (win->right) {
            win->right->x = split_x;
            win->right->y = win->y;
            win->right->width = win->width * (1.0f - win->split_ratio);
            win->right->height = win->height;
            recalculate_window_geometry(win->right);
        }
    } else if (win->split_type == SPLIT_HORIZONTAL) {
        float split_y = win->y + win->height * win->split_ratio;
        
        if (win->left) {  // Top window
            win->left->x = win->x;
            win->left->y = split_y;
            win->left->width = win->width;
            win->left->height = win->height * (1.0f - win->split_ratio);
            recalculate_window_geometry(win->left);
        }
        
        if (win->right) {  // Bottom window
            win->right->x = win->x;
            win->right->y = win->y;
            win->right->width = win->width;
            win->right->height = win->height * win->split_ratio;
            recalculate_window_geometry(win->right);
        }
    }
}

void wm_recalculate_layout() {
    if (wm.root) {
        recalculate_window_geometry(wm.root);
    }
}

void debug_print_windows() {
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(wm.root, leaves, &count);
    
    printf("=== Window Order (count=%d) ===\n", count);
    for (int i = 0; i < count; i++) {
        printf("  [%d] Window at (%.0f, %.0f) size (%.0f x %.0f) %s\n",
               i, leaves[i]->x, leaves[i]->y,
               leaves[i]->width, leaves[i]->height,
               leaves[i]->is_selected ? "[SELECTED]" : "");
    }
    if (wm.minibuffer_window) {
        printf("  [M] Minibuffer at (%.0f, %.0f) size (%.0f x %.0f) %s %s\n",
               wm.minibuffer_window->x, wm.minibuffer_window->y,
               wm.minibuffer_window->width, wm.minibuffer_window->height,
               wm.minibuffer_window->is_selected ? "[SELECTED]" : "",
               wm.minibuffer_active ? "[ACTIVE]" : "[INACTIVE]");
    }
    printf("Selected window: (%.0f, %.0f)\n\n",
           wm.selected->x, wm.selected->y);
}

void split_window_below() {
    if (!is_leaf_window(wm.selected)) return;
    if (is_minibuffer_window(wm.selected)) {
        message("Attempt to split minibuffer window");
        return;
    }
    
    if (wm.selected->height < MIN_WINDOW_SIZE * 2) return;
    
    Window *top = window_create(wm.selected, wm.selected->buffer);
    Window *bottom = window_create(wm.selected, wm.selected->buffer);
    
    if (!top || !bottom) {
        if (top) free(top);
        if (bottom) free(bottom);
        return;
    }
    
    top->point = wm.selected->point;
    bottom->point = wm.selected->point;
    top->scrollx = wm.selected->scrollx;
    top->scrolly = wm.selected->scrolly;
    bottom->scrollx = wm.selected->scrollx;
    bottom->scrolly = wm.selected->scrolly;
    
    wm.selected->is_selected = false;
    wm.selected->split_type = SPLIT_HORIZONTAL;
    wm.selected->left = top;
    wm.selected->right = bottom;
    
    top->is_selected = true;
    wm.selected = top;
    current_buffer = top->buffer;
    current_buffer->pt = top->point;
    
    wm.window_count++;
    wm_recalculate_layout();

    // NOTE The top window is already scrolled
    // Because it’s the active window
    update_window_scroll(bottom);
    
    if (debug_them_windows) debug_print_windows();
}

// NOTE Emacs seem to check if any new wraped lines occur
// and adds a scroll offset to compesate so the current window doesn’t move down when splitting
void split_window_right() {
    if (!is_leaf_window(wm.selected)) return;
    if (is_minibuffer_window(wm.selected)) {
        message("Attempt to split minibuffer window");
        return;
    }
    
    if (wm.selected->width < MIN_WINDOW_SIZE * 2) return;
    
    Window *left = window_create(wm.selected, wm.selected->buffer);
    Window *right = window_create(wm.selected, wm.selected->buffer);
    
    if (!left || !right) {
        if (left) free(left);
        if (right) free(right);
        return;
    }
    
    left->point = wm.selected->point;
    right->point = wm.selected->point;
    left->scrollx = wm.selected->scrollx;
    left->scrolly = wm.selected->scrolly;
    right->scrollx = wm.selected->scrollx;
    right->scrolly = wm.selected->scrolly;
    
    wm.selected->split_type = SPLIT_VERTICAL;
    wm.selected->left = left;
    wm.selected->right = right;
    
    left->is_selected = true;
    wm.selected = left;
    
    wm.window_count++;
    wm_recalculate_layout();
    if (debug_them_windows) debug_print_windows();
}

void delete_window() {
    if (!wm.selected->parent || wm.window_count <= 1 || is_minibuffer_window(wm.selected)) {
        message("Attempt to delete minibuffer or sole ordinary window");
        return;
    }
    
    Window *parent = wm.selected->parent;
    Window *sibling = (parent->left == wm.selected) ? parent->right : parent->left;
    
    if (!sibling) return;
    
    Window *new_selected = is_leaf_window(sibling) ? sibling : NULL;
    if (!new_selected) {
        Window *leaves[256];
        int count = 0;
        collect_leaf_windows(sibling, leaves, &count);
        if (count > 0) new_selected = leaves[0];
    }
    
    free(wm.selected);
    
    sibling->parent = parent->parent;
    
    if (parent->parent) {
        if (parent->parent->left == parent) {
            parent->parent->left = sibling;
        } else {
            parent->parent->right = sibling;
        }
    } else {
        wm.root = sibling;
    }
    
    sibling->x = parent->x;
    sibling->y = parent->y;
    sibling->width = parent->width;
    sibling->height = parent->height;
    
    free(parent);
    
    if (new_selected) {
        new_selected->is_selected = true;
        wm.selected = new_selected;
        current_buffer = new_selected->buffer;
        current_buffer->pt = new_selected->point;
    }
    
    wm.window_count--;
    wm_recalculate_layout();
}

void delete_other_windows() {
    if (is_minibuffer_window(wm.selected)) {
        message("Can’t expand minibuffer to full frame");
        return;
    }
    if (wm.window_count <= 1) {
        message("No other windows to delete");
        return;
    }
    
    Buffer *current_buffer = wm.selected->buffer;
    float x = wm.root->x;
    float y = wm.root->y;
    float width = wm.root->width;
    float height = wm.root->height;
    
    window_destroy(wm.root);
    
    wm.root = window_create(NULL, current_buffer);
    wm.root->x = x;
    wm.root->y = y;
    wm.root->width = width;
    wm.root->height = height;
    wm.root->is_selected = true;
    
    wm.selected = wm.root;
    wm.window_count = 1;
    current_buffer = current_buffer;
}

static void balance_recursive(Window *win) {
    if (!win || is_leaf_window(win)) return;
    win->split_ratio = 0.5f;
    balance_recursive(win->left);
    balance_recursive(win->right);
}

void balance_windows() {
    balance_recursive(wm.root);
    wm_recalculate_layout();
}

void enlarge_window() {
    if (is_minibuffer_window(wm.selected)) return;
    if (!wm.selected->parent) return;
    
    Window *parent = wm.selected->parent;
    float delta = 0.1f;
    
    if (parent->left == wm.selected) {
        parent->split_ratio = fminf(0.9f, parent->split_ratio + delta);
    } else {
        parent->split_ratio = fmaxf(0.1f, parent->split_ratio - delta);
    }
    
    wm_recalculate_layout();
}

void shrink_window() {
    if (is_minibuffer_window(wm.selected)) return;
    if (!wm.selected || !wm.selected->parent) return;
    
    Window *parent = wm.selected->parent;
    float delta = 0.1f;
    
    if (parent->left == wm.selected) {
        parent->split_ratio = fmaxf(0.1f, parent->split_ratio - delta);
    } else {
        parent->split_ratio = fminf(0.9f, parent->split_ratio + delta);
    }
    
    wm_recalculate_layout();
}

static void draw_modeline(Window *win) {
    if (!win) return;
    
    float line_height = win->buffer->font->ascent + win->buffer->font->descent;
    float modeline_height = line_height;
    float modeline_y = win->y;
    
    // If minibuffer is active and this is the previous window, keep it highlighted
    bool is_active = win->is_selected || 
                    (wm.minibuffer_active && win == wm.previous_window);
    
    Color color = is_active ? CT.mode_line_active_bg : CT.mode_line_inactive_bg;
    
    quad2D((vec2){win->x, modeline_y},
           (vec2){win->width, modeline_height},
           color);
}

int recenter_positions = 0;  // 0=middle, 1=top, 2=bottom

void recenter() {
    if (is_minibuffer_window(wm.selected)) return;
    
    Buffer *buf = wm.selected->buffer;
    float line_height = buf->font->ascent + buf->font->descent;
    float modeline_height = line_height;
    float usable_height = wm.selected->height - modeline_height;
    float visible_lines = usable_height / line_height;
    
    // Calculate cursor's line number
    size_t cursor_line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    float x = 0;
    float max_x = wm.selected->width - 2 * fringe_width;
    
    while (i < wm.selected->point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            cursor_line++;
            x = 0;
        } else {
            float char_width = character_width(buf->font, ch);
            if (x + char_width > max_x) {
                cursor_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(buf->font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    float cursor_y = cursor_line * line_height;
    float target_line;
    
    // Handle different argument cases
    if (!raw_prefix_arg && !argument_manually_set) {
        // No argument: center vertically
        target_line = visible_lines / 2.0f;
    } else if (raw_prefix_arg) {
        // C-u prefix: center vertically (same as no argument)
        target_line = visible_lines / 2.0f;
    } else if (arg >= 0) {
        // Positive ARG: put point on line ARG from top (0-indexed)
        target_line = (float)arg;
    } else {
        // Negative ARG: count from bottom
        // -1 means last visible line, -2 means second-to-last, etc.
        target_line = visible_lines + arg;
        // Don't let it go negative
        if (target_line < 0) target_line = 0;
    }
    
    // Set scroll so cursor appears on target_line
    // target_line is relative to the TOP of the visible window
    // So if we want cursor on target_line, we need:
    // scrolly = cursor_y - (target_line * line_height)
    wm.selected->scrolly = cursor_y - (target_line * line_height);
    
    // Ensure we don't scroll past the top of the buffer
    if (wm.selected->scrolly < 0) wm.selected->scrolly = 0;
}

void recenter_top_bottom() {
    if (is_minibuffer_window(wm.selected)) return;
    
    // If argument was manually set, just call recenter with that argument
    if (argument_manually_set) {
        recenter();
        return;
    }
    
    static Window *last_window = NULL;
    
    // Reset cycle if we're in a different window
    if (last_window != wm.selected) {
        recenter_positions = 0;
        last_window = wm.selected;
    }
    
    // Save current arg state
    int saved_arg = arg;
    bool saved_raw_prefix = raw_prefix_arg;
    bool saved_manually_set = argument_manually_set;
    
    // Set appropriate arg for recenter based on cycle state
    argument_manually_set = true;
    
    switch (recenter_positions) {
        case 0:  // Middle
            raw_prefix_arg = true;  // Use C-u behavior (center)
            argument_manually_set = false;
            recenter();
            recenter_positions = 1;
            break;
            
        case 1:  // Top (line 0)
            raw_prefix_arg = false;
            arg = 0;
            recenter();
            recenter_positions = 2;
            break;
            
        case 2:  // Bottom (last visible line)
            raw_prefix_arg = false;
            arg = -1;
            recenter();
            recenter_positions = 0;  // Cycle back to middle
            break;
    }
    
    // Restore arg state
    arg = saved_arg;
    raw_prefix_arg = saved_raw_prefix;
    argument_manually_set = saved_manually_set;
}

void update_window_scroll(Window *win) {
    if (win->is_minibuffer) return;  // Don't scroll minibuffer
    
    Buffer *buffer = win->buffer;
    float line_height = buffer->font->ascent + buffer->font->descent;
    
    // Account for modeline at bottom (1 line high)
    float modeline_height = line_height;
    float usable_height = win->height - modeline_height;
    
    // Calculate cursor's line number TODO CACHE it
    size_t cursor_line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    float x = 0;
    float max_x = win->width - 2 * fringe_width;
    
    while (i < win->point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            cursor_line++;
            x = 0;
        } else {
            float char_width = character_width(buffer->font, ch);
            if (x + char_width > max_x) {
                cursor_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(buffer->font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    // Calculate cursor position in buffer coordinates
    float cursor_top_y = cursor_line * line_height;
    float cursor_bottom_y = cursor_top_y + line_height;
    
    // Calculate window boundaries in buffer coordinates
    // Window top in buffer coords = scrolly + usable_height
    // Window bottom in buffer coords = scrolly
    float window_top_buffer = win->scrolly + usable_height;
    float window_bottom_buffer = win->scrolly;
    
    // Check if cursor is above visible area (needs scroll up)
    if (cursor_top_y < window_bottom_buffer) {
        // Cursor is above the window - center it
        float visible_lines = usable_height / line_height;
        float half_window_lines = visible_lines / 2.0f;
        win->scrolly = cursor_top_y - (half_window_lines * line_height);
        
        // Don't scroll below 0
        if (win->scrolly < 0) win->scrolly = 0;
        
        // Snap to line boundary for clean alignment
        win->scrolly = floorf(win->scrolly / line_height) * line_height;
    }
    // Check if cursor is below visible area (needs scroll down)
    else if (cursor_bottom_y > window_top_buffer) {
        // Cursor is below the window - center it
        float visible_lines = usable_height / line_height;
        float half_window_lines = visible_lines / 2.0f;
        win->scrolly = cursor_top_y - (half_window_lines * line_height);
        
        // Snap to line boundary for clean alignment
        win->scrolly = floorf(win->scrolly / line_height) * line_height;
    }
}

static void draw_window(Window *win) {
    if (!win) return;
    
    if (is_leaf_window(win)) {
       
        if (win->buffer) {
            draw_buffer(win->buffer, win, win->x + fringe_width,
                       win->y + win->height - win->buffer->font->ascent + win->buffer->font->descent);
        }

        // Left fringe
        quad2D((vec2){win->x, win->y},
               (vec2){fringe_width, win->height}, CT.fringe_bg);
        
        // Right fringe
        quad2D((vec2){win->x + win->width - fringe_width, win->y},
               (vec2){fringe_width, win->height}, CT.fringe_bg);
 
        if (!win->is_minibuffer) draw_modeline(win);

    } else {
        draw_window(win->left);
        draw_window(win->right);
    }
}

static void draw_dividers_recursive(Window *win) {
    if (!win || is_leaf_window(win)) return;
    
    if (win->split_type == SPLIT_VERTICAL) {
        // Draw vertical divider between left and right
        float divider_x = win->x + win->width * win->split_ratio;
        float divider_width = 1.0f;
        
        quad2D((vec2){divider_x - divider_width / 2, win->y},
               (vec2){divider_width, win->height}, CT.window_divider);
    }
    
    // Recursively draw dividers in children
    draw_dividers_recursive(win->left);
    draw_dividers_recursive(win->right);
}

void draw_window_dividers() {
    draw_dividers_recursive(wm.root);
}


static size_t count_buffer_lines(Buffer *buf) {
    if (!buf) return 1;
    
    size_t text_len = rope_char_length(buf->rope);
    if (text_len == 0) return 1;  // Empty buffer = 1 line
    
    size_t line_count = 1;  // At least one line
    
    // Count newlines
    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, 0);
    
    uint32_t ch;
    while (rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            line_count++;
        }
    }
    
    rope_iter_destroy(&iter);
    return line_count;
}

static float calculate_minibuffer_height() {
    if (!wm.minibuffer_window || !wm.minibuffer_window->buffer) return 0.0f;
    
    size_t line_count = count_buffer_lines(wm.minibuffer_window->buffer);
    float line_height = wm.minibuffer_window->buffer->font->ascent + 
                       wm.minibuffer_window->buffer->font->descent;
    
    return line_height * line_count;
}

void wm_draw() {
    // Calculate minibuffer height dynamically each frame
    float minibuffer_height = calculate_minibuffer_height();
    wm.minibuffer_window->height = minibuffer_height;
    
    // Get the full frame height (stored during init or window resize)
    // We need to know the total available height
    static float frame_height = 0;
    if (frame_height == 0) {
        // First time: calculate from root's initial setup
        frame_height = wm.root->y + wm.root->height;
    }
    
    // Adjust root window to account for minibuffer
    wm.root->y = wm.minibuffer_window->y + minibuffer_height;
    wm.root->height = frame_height - wm.root->y;
    
    /* wm_recalculate_layout(); */
    
    // Draw all windows
    draw_window(wm.root);
    draw_window_dividers();
    
    // Draw minibuffer
    draw_window(wm.minibuffer_window);
}


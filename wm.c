#include "wm.h"
#include "buffer.h"
#include "frame.h"
#include <stdlib.h>
#include <string.h>
#include "lisp.h"
#include "faces.h"
#include "modeline.h"


#define DEFAULT_SPLIT_RATIO 0.5f

static bool debug_them_windows = false;

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

void wm_init(WindowManager *wm, Buffer *initial_buffer, Buffer *minibuffer, float x, float y, float width, float height) {
    wm->minibuffer_window = window_create(NULL, minibuffer);
    wm->minibuffer_window->is_minibuffer = true;
    wm->minibuffer_window->x = x;
    wm->minibuffer_window->y = y;
    wm->minibuffer_window->scrollx = x;
    wm->minibuffer_window->scrolly = y;
    wm->minibuffer_window->width = width;
    wm->minibuffer_window->height = 0;
    
    // Create root window
    wm->root = window_create(NULL, initial_buffer);
    if (!wm->root) return;
    
    wm->root->x = x;
    wm->root->y = y;
    wm->root->width = width;
    wm->root->height = height;
    wm->root->is_selected = true;
    
    wm->selected = wm->root;
    wm->window_count = 1;
    wm->minibuffer_active = false;
}

void wm_cleanup(WindowManager *wm) {
    if (wm->root) {
        window_destroy(wm->root);
        wm->root = NULL;
    }
    if (wm->minibuffer_window) {
        buffer_destroy(wm->minibuffer_window->buffer);
        free(wm->minibuffer_window);
        wm->minibuffer_window = NULL;
    }
    wm->selected = NULL;
    wm->window_count = 0;
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
    if (!selected_frame->wm.root) return 0;
    
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    return count;
}


Window* next_window(Window *current) {
    if (!current || !selected_frame->wm.root) return NULL;
    
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    // If minibuffer is active, include it at the end
    if (selected_frame->wm.minibuffer_active && selected_frame->wm.minibuffer_window) {
        leaves[count++] = selected_frame->wm.minibuffer_window;
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
    if (!current || !selected_frame->wm.root) return NULL;
    
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    // If minibuffer is active, include it at the end
    if (selected_frame->wm.minibuffer_active && selected_frame->wm.minibuffer_window) {
        leaves[count++] = selected_frame->wm.minibuffer_window;
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
    selected_frame->wm.selected->point = current_buffer->pt;
    
    Window *original = selected_frame->wm.selected;
    Window *target = selected_frame->wm.selected;
    
    int arg = get_prefix_arg();
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
    
    if (target && target != selected_frame->wm.selected) {
        selected_frame->wm.selected->is_selected = false;
        target->is_selected = true;
        selected_frame->wm.selected = target;
        
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

WindowConfiguration save_window_configuration() {
    WindowConfiguration config = {0};
    
    // Allocate space for maximum possible windows (should be enough)
    config.windows = (WindowSnapshot*)calloc(32, sizeof(WindowSnapshot));
    config.count = 1;  // Start with root at index 0
    config.root_index = 0;
    
    // Save root geometry
    config.root_x = selected_frame->wm.root->x;
    config.root_y = selected_frame->wm.root->y;
    config.root_width = selected_frame->wm.root->width;
    config.root_height = selected_frame->wm.root->height;
    
    save_window_recursive(selected_frame->wm.root, config.windows, &config.count, 0);
    
    return config;
}

Window* restore_window_recursive(WindowSnapshot *snapshots, int index, Window *parent) {
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

void restore_window_configuration(WindowConfiguration *config) {
    if (!config->windows) return;
    
    // Destroy current window tree
    if (selected_frame->wm.root) {
        window_destroy(selected_frame->wm.root);
    }
    
    // Restore from snapshot
    selected_frame->wm.root = restore_window_recursive(config->windows, config->root_index, NULL);
    
    // Restore saved root geometry
    selected_frame->wm.root->x = config->root_x;
    selected_frame->wm.root->y = config->root_y;
    selected_frame->wm.root->width = config->root_width;
    selected_frame->wm.root->height = config->root_height;
    
    // Find selected window
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    selected_frame->wm.selected = NULL;
    for (int i = 0; i < count; i++) {
        if (leaves[i]->is_selected) {
            selected_frame->wm.selected = leaves[i];
            break;
        }
    }
    
    if (!selected_frame->wm.selected && count > 0) {
        selected_frame->wm.selected = leaves[0];
        selected_frame->wm.selected->is_selected = true;
    }
    
    selected_frame->wm.window_count = count;
    
    current_buffer = selected_frame->wm.selected->buffer;
    current_buffer->pt = selected_frame->wm.selected->point;
    
    wm_recalculate_layout();
}

void free_window_configuration(WindowConfiguration *config) {
    if (config->windows) {
        free(config->windows);
        config->windows = NULL;
    }
    config->count = 0;
}

void recalculate_window_geometry(Window *win) {
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
    if (selected_frame->wm.root) {
        recalculate_window_geometry(selected_frame->wm.root);
        /* printf("Recalculated layout\n"); */
    }
}

void debug_print_windows() {
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    printf("=== Window Order (count=%d) ===\n", count);
    for (int i = 0; i < count; i++) {
        printf("  [%d] Window at (%.0f, %.0f) size (%.0f x %.0f) %s\n",
               i, leaves[i]->x, leaves[i]->y,
               leaves[i]->width, leaves[i]->height,
               leaves[i]->is_selected ? "[SELECTED]" : "");
    }
    if (selected_frame->wm.minibuffer_window) {
        printf("  [M] Minibuffer at (%.0f, %.0f) size (%.0f x %.0f) %s %s\n",
               selected_frame->wm.minibuffer_window->x, selected_frame->wm.minibuffer_window->y,
               selected_frame->wm.minibuffer_window->width, selected_frame->wm.minibuffer_window->height,
               selected_frame->wm.minibuffer_window->is_selected ? "[SELECTED]" : "",
               selected_frame->wm.minibuffer_active ? "[ACTIVE]" : "[INACTIVE]");
    }
    printf("Selected window: (%.0f, %.0f)\n\n",
           selected_frame->wm.selected->x, selected_frame->wm.selected->y);
}

// TODO Handle ARG
void split_window_below() {
    if (!is_leaf_window(selected_frame->wm.selected)) return;
    if (is_minibuffer_window(selected_frame->wm.selected)) {
        message("Attempt to split minibuffer window");
        return;
    }
    
    if (selected_frame->wm.selected->height < MIN_WINDOW_SIZE * 2) return;
    
    Window *top = window_create(selected_frame->wm.selected, current_buffer);
    Window *bottom = window_create(selected_frame->wm.selected, current_buffer);
    
    if (!top || !bottom) {
        if (top) free(top);
        if (bottom) free(bottom);
        return;
    }
    
    top->point = selected_frame->wm.selected->point;
    bottom->point = selected_frame->wm.selected->point;
    top->scrollx = selected_frame->wm.selected->scrollx;
    top->scrolly = selected_frame->wm.selected->scrolly;
    bottom->scrollx = selected_frame->wm.selected->scrollx;
    bottom->scrolly = selected_frame->wm.selected->scrolly;
    
    selected_frame->wm.selected->is_selected = false;
    selected_frame->wm.selected->split_type = SPLIT_HORIZONTAL;
    selected_frame->wm.selected->left = top;
    selected_frame->wm.selected->right = bottom;
    
    top->is_selected = true;
    selected_frame->wm.selected = top;
    current_buffer = top->buffer;
    current_buffer->pt = top->point;
    
    selected_frame->wm.window_count++;
    wm_recalculate_layout();

    // NOTE The top window is already scrolled
    // Because it’s the active window
    update_window_scroll(bottom);
    
    if (debug_them_windows) debug_print_windows();
}

// NOTE Emacs seem to check if any new wraped lines occur
// and adds a scroll offset to compesate so the current window doesn’t move down when splitting
// TODO Handle ARG
void split_window_right() {
    if (!is_leaf_window(selected_frame->wm.selected)) return;
    if (is_minibuffer_window(selected_frame->wm.selected)) {
        message("Attempt to split minibuffer window");
        return;
    }
    
    if (selected_frame->wm.selected->width < MIN_WINDOW_SIZE * 2) return;
    
    Window *left = window_create(selected_frame->wm.selected, current_buffer);
    Window *right = window_create(selected_frame->wm.selected, current_buffer);
    
    if (!left || !right) {
        if (left) free(left);
        if (right) free(right);
        return;
    }
    
    left->point = selected_frame->wm.selected->point;
    right->point = selected_frame->wm.selected->point;
    left->scrollx = selected_frame->wm.selected->scrollx;
    left->scrolly = selected_frame->wm.selected->scrolly;
    right->scrollx = selected_frame->wm.selected->scrollx;
    right->scrolly = selected_frame->wm.selected->scrolly;
    
    selected_frame->wm.selected->split_type = SPLIT_VERTICAL;
    selected_frame->wm.selected->left = left;
    selected_frame->wm.selected->right = right;
    
    left->is_selected = true;
    selected_frame->wm.selected = left;
    
    selected_frame->wm.window_count++;
    wm_recalculate_layout();
    if (debug_them_windows) debug_print_windows();
}


// TODO Implement window_splittable_p and use it in those function

bool window_try_horizontal_split(Window *win) {
    if (is_minibuffer_window(win)) {
        return false;
    }
    
    // Get split-height-threshold (for horizontal splits)
    SCM threshold_var = scm_c_lookup("split-height-threshold");
    if (scm_is_false(threshold_var)) {
        return false;
    }
    
    SCM threshold_val = scm_variable_ref(threshold_var);
    if (!scm_is_integer(threshold_val)) {
        return false;
    }
    
    size_t split_height_threshold = scm_to_size_t(threshold_val);
    size_t height_in_lines = (size_t)(win->height / selected_frame->line_height);
    
    if (split_height_threshold > 0 && height_in_lines < split_height_threshold) {
        return false;
    }
    
    if (win->height < MIN_WINDOW_SIZE * 2) {
        return false;
    }
    
    split_window_below();  // Horizontal split = top/bottom
    return true;
}

bool window_try_vertical_split(Window *win) {
    if (is_minibuffer_window(win)) {
        return false;
    }
    
    // Get split-width-threshold (for vertical splits)
    SCM threshold_var = scm_c_lookup("split-width-threshold");
    if (scm_is_false(threshold_var)) {
        return false;
    }
    
    SCM threshold_val = scm_variable_ref(threshold_var);
    if (!scm_is_integer(threshold_val)) {
        return false;
    }
    
    size_t split_width_threshold = scm_to_size_t(threshold_val);
    size_t width_in_cols = (size_t)(win->width / selected_frame->column_width);
    
    if (split_width_threshold > 0 && width_in_cols < split_width_threshold) {
        return false;
    }
    
    if (win->width < MIN_WINDOW_SIZE * 2) {
        return false;
    }
    
    split_window_right();  // Vertical split = left/right
    return true;
}


Window* split_window_sensibly() {
    Window *win = selected_frame->wm.selected;
    
    // Get split-window-preferred-direction
    SCM direction_var = scm_c_lookup("split-window-preferred-direction");
    SCM direction = scm_variable_ref(direction_var);
    
    bool try_horizontal_first = false;  // horizontal = top/bottom
    
    if (scm_is_symbol(direction)) {
        SCM horizontal_sym = scm_from_utf8_symbol("horizontal");
        SCM longest_sym = scm_from_utf8_symbol("longest");
        
        if (scm_is_eq(direction, horizontal_sym)) {
            try_horizontal_first = true;  // Try top/bottom first
        } else if (scm_is_eq(direction, longest_sym)) {
            // For 'longest, split along longest dimension
            // If width > height, split vertically (left/right)
            // If height > width, split horizontally (top/bottom)
            try_horizontal_first = (selected_frame->height > selected_frame->width);
        }
        // Default (vertical) means try_horizontal_first = false
    }
    
    bool success = false;
    
    if (try_horizontal_first) {
        // Try horizontal (top/bottom) first, then vertical (left/right)
        if (window_try_horizontal_split(win)) {
            success = true;
        } else if (window_try_vertical_split(win)) {
            success = true;
        }
    } else {
        // Try vertical (left/right) first, then horizontal (top/bottom)
        if (window_try_vertical_split(win)) {
            success = true;
        } else if (window_try_horizontal_split(win)) {
            success = true;
        }
    }
    
    // Last resort: if this is the only window, try horizontal split (top/bottom)
    // disregarding split-height-threshold
    if (!success && selected_frame->wm.window_count == 1 && !is_minibuffer_window(win)) {
        if (win->height >= MIN_WINDOW_SIZE * 2) {
            split_window_below();
            success = true;
        }
    }
    
    if (!success) {
        message("Window too small to split");
        return NULL;
    }
    
    // Return the newly created window (which is now selected)
    return selected_frame->wm.selected;
}


// TODO Doesn't support display-buffer-alist yet (that's a lot more complex)
// TODO Doesn't support ACTION argument yet
Window* display_buffer(Buffer *buffer) {
    if (!buffer) return NULL;
    
    // 1. If buffer is already displayed in a window, return that window
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);
    
    for (int i = 0; i < count; i++) {
        if (leaves[i]->buffer == buffer) {
            return leaves[i];
        }
    }
    
    // 2. Buffer not displayed, need to find/create a window for it
    Window *original_selected = selected_frame->wm.selected;
    Window *target_window = NULL;
    
    // Get the split-window-preferred-function variable
    SCM split_func_var = scm_c_lookup("split-window-preferred-function");
    SCM split_func_sym = scm_variable_ref(split_func_var);
    
    // The variable contains a quoted symbol, so look up the actual function
    if (scm_is_symbol(split_func_sym)) {
        // Convert symbol to C string
        char *func_name = scm_to_locale_string(scm_symbol_to_string(split_func_sym));
        SCM actual_func_var = scm_c_lookup(func_name);
        SCM actual_func = scm_variable_ref(actual_func_var);
        free(func_name);
        
        if (scm_is_true(scm_procedure_p(actual_func))) {
            // Call the function (e.g., split-window-sensibly)
            SCM result = scm_call_0(actual_func);
            
            // Check if it returned a window (not #f)
            if (scm_is_true(result)) {
                // Split succeeded - use the OTHER window (next-window)
                target_window = next_window(selected_frame->wm.selected);
            }
        }
    }
    
    // If split failed, use next window
    if (!target_window) {
        target_window = next_window(original_selected);
    }
    
    // Display buffer in target window
    target_window->buffer = buffer;
    target_window->point = 0;
    
    // Keep original selection
    current_buffer = original_selected->buffer;
    current_buffer->pt = original_selected->point;
    
    return target_window;
}

void delete_window() {
    if (!selected_frame->wm.selected->parent || selected_frame->wm.window_count <= 1 || is_minibuffer_window(selected_frame->wm.selected)) {
        message("Attempt to delete minibuffer or sole ordinary window");
        return;
    }
    
    Window *parent = selected_frame->wm.selected->parent;
    Window *sibling = (parent->left == selected_frame->wm.selected) ? parent->right : parent->left;
    
    if (!sibling) return;
    
    Window *new_selected = is_leaf_window(sibling) ? sibling : NULL;
    if (!new_selected) {
        Window *leaves[256];
        int count = 0;
        collect_leaf_windows(sibling, leaves, &count);
        if (count > 0) new_selected = leaves[0];
    }
    
    free(selected_frame->wm.selected);
    
    sibling->parent = parent->parent;
    
    if (parent->parent) {
        if (parent->parent->left == parent) {
            parent->parent->left = sibling;
        } else {
            parent->parent->right = sibling;
        }
    } else {
        selected_frame->wm.root = sibling;
    }
    
    sibling->x = parent->x;
    sibling->y = parent->y;
    sibling->width = parent->width;
    sibling->height = parent->height;
    
    free(parent);
    
    if (new_selected) {
        new_selected->is_selected = true;
        selected_frame->wm.selected = new_selected;
        current_buffer = new_selected->buffer;
        current_buffer->pt = new_selected->point;
    }
    
    selected_frame->wm.window_count--;
    wm_recalculate_layout();
}

void delete_other_windows() {
    if (is_minibuffer_window(selected_frame->wm.selected)) {
        message("Can’t expand minibuffer to full frame");
        return;
    }
    if (selected_frame->wm.window_count <= 1) {
        message("No other windows to delete");
        return;
    }
    
    float x = selected_frame->wm.root->x;
    float y = selected_frame->wm.root->y;
    float width = selected_frame->wm.root->width;
    float height = selected_frame->wm.root->height;
    
    window_destroy(selected_frame->wm.root);
    
    selected_frame->wm.root = window_create(NULL, current_buffer);
    selected_frame->wm.root->x = x;
    selected_frame->wm.root->y = y;
    selected_frame->wm.root->width = width;
    selected_frame->wm.root->height = height;
    selected_frame->wm.root->is_selected = true;
    
    selected_frame->wm.selected = selected_frame->wm.root;
    selected_frame->wm.window_count = 1;
    current_buffer = current_buffer;
}

static void balance_recursive(Window *win) {
    if (!win || is_leaf_window(win)) return;
    win->split_ratio = 0.5f;
    balance_recursive(win->left);
    balance_recursive(win->right);
}

void balance_windows() {
    balance_recursive(selected_frame->wm.root);
    wm_recalculate_layout();
}

// TODO Doesn’t behave like emacs and we should support ARG
void enlarge_window() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    if (!selected_frame->wm.selected->parent) return;
    
    Window *parent = selected_frame->wm.selected->parent;
    float delta = 0.1f;
    
    if (parent->left == selected_frame->wm.selected) {
        parent->split_ratio = fminf(0.9f, parent->split_ratio + delta);
    } else {
        parent->split_ratio = fmaxf(0.1f, parent->split_ratio - delta);
    }
    
    wm_recalculate_layout();
}

// TODO Doesn’t behave like emacs and we should support ARG
void shrink_window() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    if (!selected_frame->wm.selected || !selected_frame->wm.selected->parent) return;
    
    Window *parent = selected_frame->wm.selected->parent;
    float delta = 0.1f;
    
    if (parent->left == selected_frame->wm.selected) {
        parent->split_ratio = fmaxf(0.1f, parent->split_ratio - delta);
    } else {
        parent->split_ratio = fminf(0.9f, parent->split_ratio + delta);
    }
    
    wm_recalculate_layout();
}

// NOTE We should use Scrissors to emulate emacs 100%
// Or do weird tricks with the order of drawing
// But we can’t because draw cakks are batched and reordered rn
static void draw_modeline(Window *win) {
    if (!win) return;
    
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    float modeline_height = line_height;
    float modeline_y = win->y;
    
    // If minibuffer is active and this is the previous window, keep it highlighted
    bool is_active = win->is_selected || 
                    (selected_frame->wm.minibuffer_active && win == selected_frame->wm.previous_window);

    Color bg_color = is_active ? face_cache->faces[FACE_MODE_LINE_ACTIVE]->bg :
        face_cache->faces[FACE_MODE_LINE_INACTIVE]->bg;
    
    Color fg_color = is_active ? face_cache->faces[FACE_MODE_LINE_ACTIVE]->fg :
        face_cache->faces[FACE_MODE_LINE_INACTIVE]->fg;
    
    // Draw background over entire window width
    quad2D((vec2){win->x, modeline_y},
           (vec2){win->width, modeline_height},
           bg_color);
    
    // Get formatted mode-line text
    char *mode_line_text = format_mode_line(win);
    
    // Draw mode-line text with clipping
    float text_x = win->x;  // Start at window edge, not after fringe
    float text_y = modeline_y + font->descent * 2;
    
    // Calculate max X position (end of window)
    float max_x = win->x + win->width;
    
    // Draw character by character, stopping at window boundary
    float x = text_x;
    char *ptr = mode_line_text;
    
    while (*ptr && x < max_x) {
        unsigned char c = *ptr;
        
        // Skip non-ASCII for simplicity
        if (c >= 128) {
            ptr++;
            continue;
        }
        
        float char_width = character_width(font, c);
        
        // Check if this character would go past the boundary
        if (x + char_width > max_x) {
            break;
        }
        
        // Draw the character
        character(font, c, x, text_y, fg_color);
        x += char_width;
        ptr++;
    }
    
    free_mode_line_string(mode_line_text);
}

void update_window_scroll(Window *win) {
    if (win->is_minibuffer) return;  // Don't scroll minibuffer
    
    Buffer *buffer = win->buffer;
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    
    // Get truncate-lines buffer-local variable
    SCM truncate_lines_sym = scm_from_utf8_symbol("truncate-lines");
    SCM truncate_lines_val = buffer_local_value(truncate_lines_sym, buffer);
    bool truncate_lines = scm_is_true(truncate_lines_val);
    
    // Account for modeline at bottom (1 line high)
    float modeline_height = line_height;
    float usable_height = win->height - modeline_height;
    /* float usable_width = win->width - 2 * fringe_width; */
    float usable_width = win->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    // Calculate cursor's line number and x position TODO Cache it
    size_t cursor_line = 0;
    float cursor_x = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    float x = 0;
    float max_x = usable_width;
    
    while (i < win->point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            cursor_line++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            
            if (truncate_lines) {
                // With truncate-lines, lines don't wrap
                Character *char_info = font_get_character(font, ch);
                if (char_info) {
                    x += char_info->ax;
                }
            } else {
                // Without truncate-lines, handle wrapping
                if (x + char_width > max_x) {
                    cursor_line++;
                    x = 0;
                }
                
                Character *char_info = font_get_character(font, ch);
                if (char_info) {
                    x += char_info->ax;
                }
            }
        }
        i++;
    }
    
    cursor_x = x;
    rope_iter_destroy(&iter);
    
    // Vertical scrolling
    float cursor_top_y = cursor_line * line_height;
    float cursor_bottom_y = cursor_top_y + line_height;
    
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
    
    // Horizontal scrolling (only when truncate-lines is true)
    if (truncate_lines) {
        float window_left = win->scrollx;
        float window_right = win->scrollx + usable_width;
        
        // Check if cursor is left of visible area
        if (cursor_x < window_left) {
            // Cursor is to the left - scroll to center it
            float half_window_width = usable_width / 2.0f;
            win->scrollx = cursor_x - half_window_width;
            
            // Don't scroll below 0
            if (win->scrollx < 0) win->scrollx = 0;
        }
        // Check if cursor is right of visible area
        else if (cursor_x > window_right) {
            // Cursor is to the right - scroll to center it
            float half_window_width = usable_width / 2.0f;
            win->scrollx = cursor_x - half_window_width;
        }
    } else {
        // When not truncating, reset horizontal scroll
        win->scrollx = 0;
    }
}

void update_windows_scroll() {
    Window *leaves[256];
    int count = 0;
    collect_leaf_windows(selected_frame->wm.root, leaves, &count);

    
    // Update all leaf windows
    for (int i = 0; i < count; i++) {
        update_window_scroll(leaves[i]);
    }
    
    // Also update minibuffer if active
    /* if (wm.minibuffer_active && wm.minibuffer_window) { */
    /*     update_window_scroll(wm.minibuffer_window); */
    /* } */
}

static void draw_window(Window *win) {
    if (!win) return;
    
    if (is_leaf_window(win)) {

        Font *font = face_cache->faces[FACE_DEFAULT]->font;

        if (win->buffer) {
            draw_buffer(win->buffer, win, win->x + selected_frame->left_fringe_width,
                       win->y + win->height - font->ascent + font->descent);
        }

        // Left fringe
        quad2D((vec2){win->x, win->y},
               (vec2){selected_frame->left_fringe_width, win->height}, face_cache->faces[FACE_FRINGE]->bg);
        
        // Right fringe
        quad2D((vec2){win->x + win->width - selected_frame->right_fringe_width, win->y},
               (vec2){selected_frame->right_fringe_width, win->height}, face_cache->faces[FACE_FRINGE]->bg);
 
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
               (vec2){divider_width, win->height}, face_cache->faces[FACE_WINDOW_DIVIDER]->bg);
    }
    
    // Recursively draw dividers in children
    draw_dividers_recursive(win->left);
    draw_dividers_recursive(win->right);
}

void draw_window_dividers(WindowManager wm) {
    draw_dividers_recursive(wm.root);
}


// Count actual visual lines including wraps
float calculate_minibuffer_height() {
    if (!selected_frame->wm.minibuffer_window || !selected_frame->wm.minibuffer_window->buffer) return 0.0f;
    
    Buffer *buf = selected_frame->wm.minibuffer_window->buffer;
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    
    // Calculate usable width for text (excluding fringes)
    float usable_width = selected_frame->wm.minibuffer_window->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    size_t text_len = rope_char_length(buf->rope);
    if (text_len == 0) return line_height;  // Empty buffer = 1 line
    
    size_t visual_line_count = 1;  // At least one line
    float x = 0;
    
    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, 0);
    
    uint32_t ch;
    while (rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            visual_line_count++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            
            // Check if character would exceed line width
            if (x + char_width > usable_width) {
                visual_line_count++;
                x = 0;
            }
            
            Character *char_info = font_get_character(font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
    }
    
    rope_iter_destroy(&iter);
    
    return line_height * visual_line_count;
}

// TODO When minibuffer window height changes we should onliy
// change the height of windows that touch the bottom of the frame
// and we should limit the minibuffer height to ‘max-mini-window-height’


void wm_draw(WindowManager *wm) {
    // Calculate minibuffer height dynamically each frame
    float minibuffer_height = calculate_minibuffer_height();
    
    // Check if minibuffer height changed
    static float prev_minibuffer_height = 0;
    bool minibuffer_height_changed = (minibuffer_height != prev_minibuffer_height);
    prev_minibuffer_height = minibuffer_height;
    
    wm->minibuffer_window->height = minibuffer_height;
    
    // Use current screen height, not cached value
    float frame_height = context.swapChainExtent.height;
    
    // Adjust root window to account for minibuffer
    wm->root->y = wm->minibuffer_window->y + minibuffer_height;
    wm->root->height = frame_height - wm->root->y;
    
    // If minibuffer height changed and we have splits, recalculate layout
    if (minibuffer_height_changed && !is_leaf_window(wm->root)) {
        wm_recalculate_layout();
    }
   
    // Draw all windows
    draw_window(wm->root);
    draw_window_dividers(*wm);
    
    // Draw minibuffer
    draw_window(wm->minibuffer_window);
}

/// Window related editing functions

int recenter_positions = 0;  // 0=middle, 1=top, 2=bottom

void recenter() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    float modeline_height = line_height;
    float usable_height = selected_frame->wm.selected->height - modeline_height;
    float visible_lines = usable_height / line_height;
    
    // Calculate cursor's line number
    size_t cursor_line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, current_buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    float x = 0;
    float max_x = selected_frame->wm.selected->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    while (i < selected_frame->wm.selected->point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            cursor_line++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            if (x + char_width > max_x) {
                cursor_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    
    float cursor_y = cursor_line * line_height;
    float target_line;
    
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();

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
    selected_frame->wm.selected->scrolly = cursor_y - (target_line * line_height);
    
    // Ensure we don't scroll past the top of the buffer
    if (selected_frame->wm.selected->scrolly < 0) selected_frame->wm.selected->scrolly = 0;
}

void recenter_top_bottom() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    
    // If argument was manually set, just call recenter with that argument
    if (argument_manually_set) {
        recenter();
        return;
    }
    
    static Window *last_window = NULL;
    
    // Reset cycle if we're in a different window
    if (last_window != selected_frame->wm.selected) {
        recenter_positions = 0;
        last_window = selected_frame->wm.selected;
    }
    
    // Save current arg state
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();
    int saved_arg = arg;
    bool saved_raw_prefix = raw_prefix_arg;
    bool saved_manually_set = argument_manually_set;
    
    // Set appropriate arg for recenter based on cycle state
    argument_manually_set = true;
    
    switch (recenter_positions) {
        case 0:  // Middle
            raw_prefix_arg = true;  // Use C-u behavior (center)
            set_raw_prefix_arg(true);
            argument_manually_set = false;
            recenter();
            recenter_positions = 1;
            break;
            
        case 1:  // Top (line 0)
            raw_prefix_arg = false;
            set_raw_prefix_arg(false);
            arg = 0;
            set_prefix_arg(0);
            recenter();
            recenter_positions = 2;
            break;
            
        case 2:  // Bottom (last visible line)
            raw_prefix_arg = false;
            set_raw_prefix_arg(false);
            arg = -1;
            set_prefix_arg(-1);
            recenter();
            recenter_positions = 0;  // Cycle back to middle
            break;
    }
    
    // Restore arg state
    arg = saved_arg;
    set_prefix_arg(arg);
    raw_prefix_arg = saved_raw_prefix;
    set_raw_prefix_arg(raw_prefix_arg);
    argument_manually_set = saved_manually_set;
}


int next_screen_context_lines = 2;  // Lines of overlap when scrolling full screen


void scroll_up_command() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    float modeline_height = line_height;
    float usable_height = selected_frame->wm.selected->height - modeline_height;
    float visible_lines = usable_height / line_height;
    
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();
    bool manually_set = argument_manually_set;
    
    int lines_to_scroll;
    
    if (!manually_set && !raw_prefix_arg) {
        // No argument: scroll nearly full screen (leave context lines)
        lines_to_scroll = (int)(visible_lines - next_screen_context_lines);
        if (lines_to_scroll < 1) lines_to_scroll = 1;
    } else if (raw_prefix_arg && arg < 0) {
        // C-u - prefix: scroll down by nearly full screen
        scroll_down_command();
        return;
    } else if (arg < 0) {
        // Negative ARG: scroll down
        set_prefix_arg(-arg);
        scroll_down_command();
        set_prefix_arg(arg);  // Restore
        return;
    } else {
        // Explicit positive ARG: scroll that many lines
        lines_to_scroll = arg;
    }
    
    // Calculate current cursor line
    size_t cursor_line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, current_buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    float x = 0;
    float max_x = selected_frame->wm.selected->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    while (i < selected_frame->wm.selected->point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            cursor_line++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            if (x + char_width > max_x) {
                cursor_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        i++;
    }
    rope_iter_destroy(&iter);
    
    // Calculate total lines in buffer
    size_t total_lines = 0;
    rope_iter_init(&iter, current_buffer->rope, 0);
    x = 0;
    while (rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            total_lines++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            if (x + char_width > max_x) {
                total_lines++;
                x = 0;
            }
            
            Character *char_info = font_get_character(font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
    }
    rope_iter_destroy(&iter);
    
    float cursor_y = cursor_line * line_height;
    float max_scroll = total_lines * line_height - usable_height;
    if (max_scroll < 0) max_scroll = 0;
    
    // Try to scroll
    float new_scroll = selected_frame->wm.selected->scrolly + (lines_to_scroll * line_height);
    
    // Check if we can scroll
    if (new_scroll > max_scroll) {
        // Can't scroll further
        // Check if we're already at end of buffer
        if (selected_frame->wm.selected->point >= rope_char_length(current_buffer->rope)) {
            message("End of buffer");
            return;
        }
        
        // Move point instead
        if (manually_set) {
            // Move by ARG lines
            size_t new_point = selected_frame->wm.selected->point;
            rope_iter_t move_iter;
            rope_iter_init(&move_iter, current_buffer->rope, new_point);
            
            int lines_moved = 0;
            while (lines_moved < lines_to_scroll && rope_iter_next_char(&move_iter, &ch)) {
                if (ch == '\n') {
                    lines_moved++;
                    if (lines_moved == lines_to_scroll) {
                        new_point = move_iter.char_pos;
                        break;
                    }
                }
                new_point = move_iter.char_pos;
            }
            rope_iter_destroy(&move_iter);
            
            set_point(new_point);
        } else {
            // Move to end of buffer
            set_point(rope_char_length(current_buffer->rope));
        }
        
        // Clamp scroll to max
        selected_frame->wm.selected->scrolly = max_scroll;
    } else {
        // Can scroll normally
        selected_frame->wm.selected->scrolly = new_scroll;
        
        // Check if cursor is now off-screen (above the visible area)
        float cursor_relative_y = cursor_y - selected_frame->wm.selected->scrolly;
        
        if (cursor_relative_y < 0) {
            // Cursor scrolled off top, move it to top of window
            size_t top_line = (size_t)(selected_frame->wm.selected->scrolly / line_height);
            
            // Find character position for that line
            rope_iter_init(&iter, current_buffer->rope, 0);
            size_t current_line = 0;
            size_t pos = 0;
            x = 0;
            
            while (rope_iter_next_char(&iter, &ch)) {
                if (current_line == top_line) {
                    break;
                }
                
                if (ch == '\n') {
                    current_line++;
                    x = 0;
                } else {
                    float char_width = character_width(font, ch);
                    if (x + char_width > max_x) {
                        current_line++;
                        x = 0;
                    }
                    
                    Character *char_info = font_get_character(font, ch);
                    if (char_info) {
                        x += char_info->ax;
                    }
                }
                pos = iter.char_pos;
            }
            rope_iter_destroy(&iter);
            
            set_point(pos);
        }
    }
}

void scroll_down_command() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    float modeline_height = line_height;
    float usable_height = selected_frame->wm.selected->height - modeline_height;
    float visible_lines = usable_height / line_height;
    
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();
    bool manually_set = argument_manually_set;
    
    int lines_to_scroll;
    
    if (!manually_set && !raw_prefix_arg) {
        // No argument: scroll nearly full screen (leave context lines)
        lines_to_scroll = (int)(visible_lines - next_screen_context_lines);
        if (lines_to_scroll < 1) lines_to_scroll = 1;
    } else if (raw_prefix_arg && arg < 0) {
        // C-u - prefix: scroll up by nearly full screen
        scroll_up_command();
        return;
    } else if (arg < 0) {
        // Negative ARG: scroll up
        set_prefix_arg(-arg);
        scroll_up_command();
        set_prefix_arg(arg);  // Restore
        return;
    } else {
        // Explicit positive ARG: scroll that many lines
        lines_to_scroll = arg;
    }
    
    // Calculate current cursor line
    size_t cursor_line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, current_buffer->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    float x = 0;
    float max_x = selected_frame->wm.selected->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    while (i < selected_frame->wm.selected->point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            cursor_line++;
            x = 0;
        } else {
            float char_width = character_width(font, ch);
            if (x + char_width > max_x) {
                cursor_line++;
                x = 0;
            }
            
            Character *char_info = font_get_character(font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
        i++;
    }
    rope_iter_destroy(&iter);
    
    float cursor_y = cursor_line * line_height;
    
    // Try to scroll
    float new_scroll = selected_frame->wm.selected->scrolly - (lines_to_scroll * line_height);
    
    // Check if we can scroll
    if (new_scroll < 0) {
        // Can't scroll further
        // Check if we're already at beginning of buffer
        if (selected_frame->wm.selected->point == 0) {
            message("Beginning of buffer");
            return;
        }
        
        // Move point instead
        if (manually_set) {
            // Move by ARG lines backward
            size_t new_point = selected_frame->wm.selected->point;
            rope_iter_t move_iter;
            rope_iter_init(&move_iter, current_buffer->rope, new_point);
            
            int lines_moved = 0;
            while (lines_moved < lines_to_scroll && rope_iter_prev_char(&move_iter, &ch)) {
                if (ch == '\n') {
                    lines_moved++;
                    if (lines_moved == lines_to_scroll) {
                        new_point = move_iter.char_pos;
                        break;
                    }
                }
                new_point = move_iter.char_pos;
            }
            rope_iter_destroy(&move_iter);
            
            set_point(new_point);
        } else {
            // Move to beginning of buffer
            set_point(0);
        }
        
        // Clamp scroll to 0
        selected_frame->wm.selected->scrolly = 0;
    } else {
        // Can scroll normally
        selected_frame->wm.selected->scrolly = new_scroll;
        
        // Check if cursor is now off-screen (below the visible area)
        float cursor_relative_y = cursor_y - selected_frame->wm.selected->scrolly;
        
        if (cursor_relative_y >= usable_height) {
            // Cursor scrolled off bottom, move it to bottom of window
            size_t bottom_line = (size_t)((selected_frame->wm.selected->scrolly + usable_height - line_height) / line_height);
            
            // Find character position for that line
            rope_iter_init(&iter, current_buffer->rope, 0);
            size_t current_line = 0;
            size_t pos = 0;
            x = 0;
            
            while (rope_iter_next_char(&iter, &ch)) {
                if (current_line == bottom_line) {
                    break;
                }
                
                if (ch == '\n') {
                    current_line++;
                    x = 0;
                } else {
                    float char_width = character_width(font, ch);
                    if (x + char_width > max_x) {
                        current_line++;
                        x = 0;
                    }
                    
                    Character *char_info = font_get_character(font, ch);
                    if (char_info) {
                        x += char_info->ax;
                    }
                }
                pos = iter.char_pos;
            }
            rope_iter_destroy(&iter);
            
            set_point(pos);
        }
    }
}

void scroll_other_window() {
    Window *original = selected_frame->wm.selected;
    Window *other = next_window(selected_frame->wm.selected);
    
    if (other == original || !other) {
        message("There is no other window");
        return;
    }
    
    // Temporarily switch to other window
    selected_frame->wm.selected = other;
    current_buffer = other->buffer;
    current_buffer->pt = other->point;
    scroll_up_command();

    // Switch back to original window
    selected_frame->wm.selected = original;
    current_buffer = original->buffer;
    current_buffer->pt = original->point;
}

void scroll_other_window_down() {
    Window *original = selected_frame->wm.selected;
    Window *other = next_window(selected_frame->wm.selected);
    
    if (other == original || !other) {
        message("There is no other window");
        return;
    }
    
    // Temporarily switch to other window
    selected_frame->wm.selected = other;
    current_buffer = other->buffer;
    current_buffer->pt = other->point;
    scroll_down_command();
    
    // Switch back to original window
    selected_frame->wm.selected = original;
    current_buffer = original->buffer;
    current_buffer->pt = original->point;
}

void move_to_window_line() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    
    Font *font = face_cache->faces[FACE_DEFAULT]->font;
    float line_height = font->ascent + font->descent;
    float modeline_height = line_height;
    float usable_height = selected_frame->wm.selected->height - modeline_height;
    float visible_lines = usable_height / line_height;
    
    int arg = get_prefix_arg();
    bool raw_prefix_arg = get_raw_prefix_arg();
    
    float target_line_float;
    
    if (!argument_manually_set && !raw_prefix_arg) {
        // No argument: center of window
        target_line_float = visible_lines / 2.0f;
    } else {
        if (arg < 0) {
            // Negative: count from bottom
            // -1 means last fully visible line
            target_line_float = visible_lines + arg;
            if (target_line_float < 0) target_line_float = 0;
        } else {
            // Positive or zero: count from top
            target_line_float = (float)arg;
        }
    }
    
    // Calculate the target Y position in buffer coordinates
    // This matches how recenter calculates: target_y = scrolly + (target_line * line_height)
    float target_y = selected_frame->wm.selected->scrolly + (target_line_float * line_height) + 1;
    size_t target_visual_line = (size_t)(target_y / line_height);
    
    // Now find the character at the START of that visual line
    rope_iter_t iter;
    rope_iter_init(&iter, current_buffer->rope, 0);
    
    uint32_t ch;
    size_t current_visual_line = 0;
    size_t line_start_pos = 0;
    float x = 0;
    float max_x = selected_frame->wm.selected->width - (selected_frame->left_fringe_width + selected_frame->right_fringe_width);
    
    // Iterate through buffer, tracking visual line starts
    while (rope_iter_next_char(&iter, &ch)) {
        // Check if we've reached the target visual line
        if (current_visual_line == target_visual_line) {
            set_point(line_start_pos);
            rope_iter_destroy(&iter);
            return;
        }
        
        if (ch == '\n') {
            current_visual_line++;
            x = 0;
            line_start_pos = iter.char_pos;  // Next line starts after newline
        } else {
            float char_width = character_width(font, ch);
            if (x + char_width > max_x) {
                // Line wrap
                current_visual_line++;
                x = 0;
                // The current character starts the new wrapped line
                line_start_pos = iter.char_pos - 1;
            }
            
            Character *char_info = font_get_character(font, ch);
            if (char_info) {
                x += char_info->ax;
            }
        }
    }
    
    rope_iter_destroy(&iter);
    
    // If we didn't find the target line (beyond end of buffer),
    // go to end of buffer
    size_t buffer_len = rope_char_length(current_buffer->rope);
    set_point(buffer_len);
}

void move_to_window_line_top_bottom() {
    if (is_minibuffer_window(selected_frame->wm.selected)) return;
    
    // If argument was manually set, just call move_to_window_line with that argument
    if (argument_manually_set) {
        move_to_window_line();
        return;
    }
    
    static Window *last_window = NULL;
    
    // Reset cycle if we're in a different window
    if (last_window != selected_frame->wm.selected) {
        recenter_positions = 0;
        last_window = selected_frame->wm.selected;
    }
    
    // Save current arg state
    int saved_arg = get_prefix_arg();
    bool saved_raw_prefix = get_raw_prefix_arg();
    bool saved_manually_set = argument_manually_set;
    
    // Set appropriate arg for move_to_window_line based on cycle state
    switch (recenter_positions) {
        case 0:  // Middle
            argument_manually_set = false;
            move_to_window_line();
            recenter_positions = 1;
            break;
            
        case 1:  // Top
            set_prefix_arg(0);
            argument_manually_set = true;
            move_to_window_line();
            recenter_positions = 2;
            break;
            
        case 2:  // Bottom (last visible line)
            set_prefix_arg(-1);
            argument_manually_set = true;
            move_to_window_line();
            recenter_positions = 0;  // Cycle back to middle
            break;
    }
    
    // Restore arg state
    set_prefix_arg(saved_arg);
    argument_manually_set = saved_manually_set;
}



/// SCM




// Get minimum window width in pixels (columns + fringes)
SCM scm_window_min_pixel_width(SCM window_obj) {
    // Get window-min-width in columns
    size_t min_cols = scm_get_size_t("window-min-width", 9);
    
    // Get character width
    float char_width = frame_char_width(selected_frame);
    
    // Calculate minimum width: columns + fringes
    float min_width = (float)min_cols * char_width +
                      selected_frame->left_fringe_width +
                      selected_frame->right_fringe_width;
    
    return scm_from_double(min_width);
}

// Get minimum window height in pixels (lines)
SCM scm_window_min_pixel_height(SCM window_obj) {
    // Get window-min-height in lines
    size_t min_lines = scm_get_size_t("window-min-height", 4);
    
    // Calculate minimum height: lines * line_height
    float min_height = (float)min_lines * selected_frame->line_height;
    
    return scm_from_double(min_height);
}

#include "wm.h"
#include <stdlib.h>
#include <string.h>

WindowManager wm = {0};

#define DEFAULT_SPLIT_RATIO 0.5f
#define MIN_WINDOW_SIZE 100.0f

#define MINIBUFFER_HEIGHT 30.0f

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
    
    // NOTE: We don't destroy the buffer here, as buffers are managed separately
    free(win);
}

bool is_leaf_window(Window *win) {
    return win && win->split_type == SPLIT_NONE;
}


bool is_minibuffer_window(Window *win) {
    return win && win->is_minibuffer;
}

void wm_init(Buffer *initial_buffer, float x, float y, float width, float height) {
    // Create minibuffer buffer and window first
    Buffer *minibuf = buffer_create(initial_buffer->font);
    wm.minibuffer_window = window_create(NULL, minibuf);
    wm.minibuffer_window->is_minibuffer = true;
    wm.minibuffer_window->x = x;
    wm.minibuffer_window->y = y;
    wm.minibuffer_window->width = width;
    wm.minibuffer_window->height = MINIBUFFER_HEIGHT;
    
    // Create root window (takes up space above minibuffer)
    wm.root = window_create(NULL, initial_buffer);
    if (!wm.root) return;
    
    wm.root->x = x;
    wm.root->y = y + MINIBUFFER_HEIGHT;  // Shift up to account for minibuffer
    wm.root->width = width;
    wm.root->height = height - MINIBUFFER_HEIGHT;
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
        // Note: You need to destroy the minibuffer's buffer too
        if (wm.minibuffer_window->buffer) {
            buffer_destroy(wm.minibuffer_window->buffer);
        }
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

Window* get_selected_window() {
    return wm.selected;
}

Buffer* get_selected_buffer() {
    return wm.selected ? wm.selected->buffer : NULL;
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
            return leaves[(i + 1) % count];  // Wrap around
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
            return leaves[(i - 1 + count) % count];  // Wrap around
        }
    }
    
    return leaves[0];  // Fallback
}

void other_window() {
    wm.selected->point = buffer->pt;
    
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
        buffer = target->buffer;
        buffer->pt = target->point;
    }
    
    if (debug_them_windows) debug_print_windows();
}


void activate_minibuffer() {
    if (wm.minibuffer_active) return;
    
    wm.minibuffer_active = true;
    
    // Store the current window (before switching to minibuffer)
    wm.previous_window = wm.selected;
    
    // Switch to minibuffer
    wm.selected->is_selected = false;
    wm.minibuffer_window->is_selected = true;
    wm.selected = wm.minibuffer_window;
    buffer = wm.minibuffer_window->buffer;
    buffer->pt = wm.minibuffer_window->point;
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

    
    // Switch back to the previous window (not necessarily root)
    if (wm.selected == wm.minibuffer_window && wm.previous_window) {
        wm.minibuffer_window->is_selected = false;
        wm.previous_window->is_selected = true;
        wm.selected = wm.previous_window;
        buffer = wm.previous_window->buffer;
        buffer->pt = wm.previous_window->point;
    }
    
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
    if (is_minibuffer_window(wm.selected)) return;  // Can't split minibuffer
    
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
    
    wm.selected->is_selected = false;
    wm.selected->split_type = SPLIT_HORIZONTAL;
    wm.selected->left = top;
    wm.selected->right = bottom;
    
    top->is_selected = true;
    wm.selected = top;
    buffer = top->buffer;
    buffer->pt = top->point;
    
    wm.window_count++;
    wm_recalculate_layout();
    if (debug_them_windows) debug_print_windows();
}

void split_window_right() {
    if (!is_leaf_window(wm.selected)) return;
    if (is_minibuffer_window(wm.selected)) return;  // Can't split minibuffer
    
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
        buffer = new_selected->buffer;
        buffer->pt = new_selected->point;
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
    buffer = current_buffer;
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


static void draw_window(Window *win) {
    if (!win) return;
    
    if (is_leaf_window(win)) {
        // Left fringe
        quad2D((vec2){win->x, win->y},
               (vec2){fringe_width, win->height}, CT.fringe_bg);
        
        // Right fringe
        quad2D((vec2){win->x + win->width - fringe_width, win->y},
               (vec2){fringe_width, win->height}, CT.fringe_bg);
        
        if (win->buffer) {
            draw_buffer(win->buffer, win, win->x + fringe_width,
                       win->y + win->height - win->buffer->font->ascent + win->buffer->font->descent);
        }
        
        // Don't draw modeline for minibuffer
        if (!win->is_minibuffer) {
            draw_modeline(win);
        }
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

// Calculate minibuffer height based on its content
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
    
    // Recalculate all window geometry with new root dimensions
    wm_recalculate_layout();
    
    // Draw all windows
    draw_window(wm.root);
    draw_window_dividers();
    
    // Draw minibuffer
    draw_window(wm.minibuffer_window);
}

/* void wm_draw() { */
/*     draw_window(wm.root); */
/*     draw_window_dividers(); */
    
/*     // Draw minibuffer */
/*     draw_window(wm.minibuffer_window); */
/* } */

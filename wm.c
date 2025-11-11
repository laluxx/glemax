#include "wm.h"
#include <stdlib.h>
#include <string.h>

WindowManager wm = {0};

#define DEFAULT_SPLIT_RATIO 0.5f
#define MIN_WINDOW_SIZE 100.0f

static bool debug_them_windows = false;

Window* window_create(Window *parent, Buffer *buffer) {
    Window *win = (Window*)calloc(1, sizeof(Window));
    if (!win) return NULL;
    
    win->parent = parent;
    win->buffer = buffer;
    win->split_type = SPLIT_NONE;
    win->split_ratio = DEFAULT_SPLIT_RATIO;
    win->is_selected = false;
    
    win->point = 0;
    win->goal_column = 0;
    
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

void wm_init(Buffer *initial_buffer, float x, float y, float width, float height) {
    wm.root = window_create(NULL, initial_buffer);
    if (!wm.root) return;
    
    wm.root->x = x;
    wm.root->y = y;
    wm.root->width = width;
    wm.root->height = height;
    wm.root->is_selected = true;
    
    wm.selected = wm.root;
    wm.window_count = 1;
}

void wm_cleanup() {
    if (wm.root) {
        window_destroy(wm.root);
        wm.root = NULL;
    }
    wm.selected = NULL;
    wm.window_count = 0;
}

// Collect all leaf windows in left-to-right, top-to-bottom order
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
    
    Window *leaves[256];  // Max 256 windows
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
    // Save current window's point and goal column
    wm.selected->point = buffer->pt;
    wm.selected->goal_column = buffer->cursor.goal_column;
    
    Window *next = next_window(wm.selected);
    if (next && next != wm.selected) {
        wm.selected->is_selected = false;
        next->is_selected = true;
        wm.selected = next;
        
        // Update global buffer pointer and restore point
        buffer = next->buffer;
        buffer->pt = next->point;
        buffer->cursor.goal_column = next->goal_column;
    }
    if (debug_them_windows) debug_print_windows();
}

static void recalculate_window_geometry(Window *win) {
    if (!win || is_leaf_window(win)) return;
    
    if (win->split_type == SPLIT_VERTICAL) {
        // Vertical split: left and right children
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
        // Horizontal split: top and bottom children
        // IMPORTANT: Y grows UPWARD, so win->y is the BOTTOM of the window
        float split_y = win->y + win->height * win->split_ratio;
        
        if (win->left) {  // Top window
            // Top window starts at split_y and goes up
            win->left->x = win->x;
            win->left->y = split_y;  // FIX: Start from split point
            win->left->width = win->width;
            win->left->height = win->height * (1.0f - win->split_ratio);  // FIX: Top gets (1-ratio)
            recalculate_window_geometry(win->left);
        }
        
        if (win->right) {  // Bottom window
            // Bottom window starts at win->y (the bottom)
            win->right->x = win->x;
            win->right->y = win->y;  // FIX: Start from parent's bottom
            win->right->width = win->width;
            win->right->height = win->height * win->split_ratio;  // FIX: Bottom gets ratio
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
    printf("Selected window: (%.0f, %.0f)\n\n",
           wm.selected->x, wm.selected->y);
}

void split_window_below() {
    if (!is_leaf_window(wm.selected)) return;
    
    // Check minimum size
    if (wm.selected->height < MIN_WINDOW_SIZE * 2) return;
    
    // Create two new child windows
    Window *top = window_create(wm.selected, wm.selected->buffer);
    Window *bottom = window_create(wm.selected, wm.selected->buffer);
    
    if (!top || !bottom) {
        if (top) free(top);
        if (bottom) free(bottom);
        return;
    }
    
    // Both windows start with same point and goal column
    top->point = wm.selected->point;
    top->goal_column = wm.selected->goal_column;
    bottom->point = wm.selected->point;
    bottom->goal_column = wm.selected->goal_column;
    
    // IMPORTANT: Clear the old window's selected flag BEFORE conversion
    wm.selected->is_selected = false;
    
    // Convert current window to a split node
    wm.selected->split_type = SPLIT_HORIZONTAL;
    wm.selected->left = top;    // Top
    wm.selected->right = bottom; // Bottom
    
    // Keep selection in the top window (consistent with split_window_right behavior)
    top->is_selected = true;
    wm.selected = top;
    buffer = top->buffer;
    buffer->pt = top->point;
    buffer->cursor.goal_column = top->goal_column;
    
    wm.window_count++;
    wm_recalculate_layout();
    if (debug_them_windows) debug_print_windows();
}

void split_window_right() {
    if (!is_leaf_window(wm.selected)) return;
    
    // Check minimum size
    if (wm.selected->width < MIN_WINDOW_SIZE * 2) return;
    
    // Create two new child windows
    Window *left = window_create(wm.selected, wm.selected->buffer);
    Window *right = window_create(wm.selected, wm.selected->buffer);
    
    if (!left || !right) {
        if (left) free(left);
        if (right) free(right);
        return;
    }
    
    // Both windows start with same point and goal column
    left->point = wm.selected->point;
    left->goal_column = wm.selected->goal_column;
    right->point = wm.selected->point;
    right->goal_column = wm.selected->goal_column;
    
    // Convert current window to a split node
    wm.selected->split_type = SPLIT_VERTICAL;
    wm.selected->left = left;
    wm.selected->right = right;
    
    // Keep selection in the original (now left) window (Emacs behavior)
    left->is_selected = true;
    wm.selected = left;
    
    wm.window_count++;
    wm_recalculate_layout();
    if (debug_them_windows) debug_print_windows();
}

void delete_window() {
    if (!wm.selected->parent) return;  // Can't delete root
    if (wm.window_count <= 1) return;  // Can't delete last window
    
    Window *parent = wm.selected->parent;
    Window *sibling = (parent->left == wm.selected) ? parent->right : parent->left;
    
    if (!sibling) return;
    
    // Select the sibling
    Window *new_selected = is_leaf_window(sibling) ? sibling : NULL;
    if (!new_selected) {
        // Find first leaf in sibling subtree
        Window *leaves[256];
        int count = 0;
        collect_leaf_windows(sibling, leaves, &count);
        if (count > 0) new_selected = leaves[0];
    }
    
    // Free the selected window (but not its buffer)
    free(wm.selected);
    
    // Replace parent with sibling
    sibling->parent = parent->parent;
    
    if (parent->parent) {
        if (parent->parent->left == parent) {
            parent->parent->left = sibling;
        } else {
            parent->parent->right = sibling;
        }
    } else {
        // Parent was root
        wm.root = sibling;
    }
    
    // Copy parent's geometry to sibling
    sibling->x = parent->x;
    sibling->y = parent->y;
    sibling->width = parent->width;
    sibling->height = parent->height;
    
    // Free the parent node
    free(parent);
    
    // Update selection
    if (new_selected) {
        new_selected->is_selected = true;
        wm.selected = new_selected;
        buffer = new_selected->buffer;
        buffer->pt = new_selected->point;
        buffer->cursor.goal_column = new_selected->goal_column;
    }
    
    wm.window_count--;
    wm_recalculate_layout();
}

void delete_other_windows() {
    if (wm.window_count <= 1) return;
    
    Buffer *current_buffer = wm.selected->buffer;
    float x = wm.root->x;
    float y = wm.root->y;
    float width = wm.root->width;
    float height = wm.root->height;
    
    // Destroy entire tree
    window_destroy(wm.root);
    
    // Create new single window
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
    if (!wm.selected->parent) return;
    
    Window *parent = wm.selected->parent;
    float delta = 0.1f;  // 10% change
    
    if (parent->left == wm.selected) {
        // We're the left/top child, increase our share
        parent->split_ratio = fminf(0.9f, parent->split_ratio + delta);
    } else {
        // We're the right/bottom child, decrease left's share
        parent->split_ratio = fmaxf(0.1f, parent->split_ratio - delta);
    }
    
    wm_recalculate_layout();
}

void shrink_window() {
    if (!wm.selected || !wm.selected->parent) return;
    
    Window *parent = wm.selected->parent;
    float delta = 0.1f;  // 10% change
    
    if (parent->left == wm.selected) {
        // We're the left/top child, decrease our share
        parent->split_ratio = fmaxf(0.1f, parent->split_ratio - delta);
    } else {
        // We're the right/bottom child, increase left's share
        parent->split_ratio = fminf(0.9f, parent->split_ratio + delta);
    }
    
    wm_recalculate_layout();
}


static void draw_modeline(Window *win) {
    if (!win) return;
    
    float line_height = win->buffer->font->ascent + win->buffer->font->descent;
    float modeline_height = line_height;
    float modeline_y = win->y;  // Bottom edge of the window
    
    Color color = win->is_selected ? CT.variable : CT.comment;
    
    quad2D((vec2){win->x, modeline_y},
           (vec2){win->width, modeline_height},
           color);
}

static void draw_window(Window *win) {
    if (!win) return;
    
    if (is_leaf_window(win)) {
        // Draw the buffer content with window's point
        if (win->buffer) {
            draw_buffer(win->buffer, win, win->x, 
                                   win->y + win->height - win->buffer->font->ascent + win->buffer->font->descent);
        }
        draw_modeline(win);        
    } else {
        // Draw children
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
               (vec2){divider_width, win->height}, CT.clock);
    }
    
    // Recursively draw dividers in children
    draw_dividers_recursive(win->left);
    draw_dividers_recursive(win->right);
}

void draw_window_dividers() {
    draw_dividers_recursive(wm.root);
}

void wm_draw() {
    draw_window(wm.root);
    draw_window_dividers();
}

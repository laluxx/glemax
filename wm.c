#include "wm.h"
#include "buffer.h"
#include "faces.h"
#include "globals.h"
#include <stdlib.h>
#include <string.h>

// TODO Make sure that buffer->displayWindows is ALWAYS updated
// By doing it in core functions that every other functionin this module will use.

// TODO i want it to mange X11 windows aswell add it to lume lazy fuck
// TODO Tags M-0..9

// TODO resurrect_last_killed_window()


void initWindowManager(WindowManager *wm, BufferManager *bm, Font *font, int sw, int sh) {
    wm->head = malloc(sizeof(Window));
    wm->head->x = 0;
    wm->head->y = sh - font->ascent + font->descent;  // Adjust for font height
    wm->head->width = sw;
    wm->head->height = wm->head->y;  // Adjust height accordingly
    wm->head->buffer = getActiveBuffer(bm);
    wm->head->prev = NULL;
    wm->head->next = NULL;
    wm->head->isActive = true;
    wm->head->modeline.height = 25.0;
    wm->activeWindow = wm->head;
    wm->activeWindow->splitOrientation = VERTICAL;
    wm->head->scroll = (Vec2f){0, 0}; 
    wm->count = 1;
    wm->head->modeline.window = wm->head;

    wm->graveyard = NULL;

    wm->head->leftPadding = 0;
    wm->head->parameters = (WindowParameters){0};
    wm->head->parameters.noOtherWindow = false;
    if (global_minimap) {
        wm->head->parameters.minimap = true;
        wm->head->parameters.minimap_target_width = 110.0f;  // Set target width
        if (lerp_minimap_on_startup) {
            wm->head->parameters.minimap_width = 0.0f;
        } else {
            wm->head->parameters.minimap_width = 110.0f;
        }
        wm->head->parameters.minimap_lerp_active = true; // Start lerping
    } else {
        wm->head->parameters.minimap = false;
    }
    if (global_visual_line_mode) {
        wm->head->parameters.truncateLines = false;
    } else {
        wm->head->parameters.truncateLines = true;
    }

    wm->head->parameters.scrollBar = true;

    addDisplayWindowToBuffer(wm->head->buffer, wm->head);
}

#include "edit.h"

void split_window_right(WindowManager *wm, WindowParameters *parameters) {
    Window *active = wm->activeWindow;
    Window *newWindow = malloc(sizeof(Window));

    *newWindow = *active; // Copy settings from active window
    newWindow->width /= 2;
    active->width -= newWindow->width;
    newWindow->x += active->width;

    active->splitOrientation = VERTICAL;
    newWindow->splitOrientation = VERTICAL;
    /* newWindow->parameters.noOtherWindow = parameters->noOtherWindow; // FIX */
    newWindow->parameters.noOtherWindow = false; // FIX


    // Insert new window into the list
    newWindow->next = active->next;
    if (active->next) {
        active->next->prev = newWindow;
    }
    active->next = newWindow;
    newWindow->prev = active;

    // NOTE Update window-buffer (displayWindows) and window-modeline relationships
    newWindow->modeline.window = newWindow;
    addDisplayWindowToBuffer(newWindow->buffer, newWindow);


    // Check if the active buffer is a .c or .h file and try to open the
    // corresponding file
    if (active->buffer && active->buffer->path) {
        const char *current_path = active->buffer->path;
        size_t len = strlen(current_path);
        char *target_path = NULL;

        // Check if the current file is a .c file
        if (len >= 2 && strcmp(current_path + len - 2, ".c") == 0) {
            // Construct .h path
            target_path = malloc(len + 1);
            if (target_path) {
                strcpy(target_path, current_path);
                strcpy(target_path + len - 2, ".h"); // Replace .c with .h
            }
        }
        // Check if the current file is a .h file
        else if (len >= 2 && strcmp(current_path + len - 2, ".h") == 0) {
            // Construct .c path
            target_path = malloc(len + 1);
            if (target_path) {
                strcpy(target_path, current_path);
                strcpy(target_path + len - 2, ".c"); // Replace .h with .c
            }
        }

        if (target_path) {
            // Temporarily store the original buffer
            Buffer *original_buffer = active->buffer;

            // Insert the target path into the minibuffer and call find_file
            Buffer *minibuffer = getBuffer(&bm, "minibuffer");
            if (minibuffer) {
                setBufferContent(minibuffer, target_path, true);
                find_file(&bm, wm);

                // After find_file runs, the new buffer will be created and active
                Buffer *target_buffer = getActiveBuffer(&bm);
                if (target_buffer) {
                    // Assign the target buffer to the new window
                    newWindow->buffer = target_buffer;
                    recenter(newWindow, true);
                    // Restore the original buffer to the active window
                    active->buffer = original_buffer;
                    // Switch the active window back to the original window
                    wm->activeWindow = active;
                    active->isActive = true;
                    newWindow->isActive = false;
                }
            }

            free(target_path);
        }
    }

    wm->count++;
}

// Function to update buffer associations when a window changes buffers
void updateWindowBufferAssociation(Window *window, Buffer *oldBuffer,
                                   Buffer *newBuffer) {
    if (oldBuffer == newBuffer)
        return;

    if (oldBuffer) {
        removeDisplayWindowFromBuffer(oldBuffer, window);
    }

    if (newBuffer) {
        addDisplayWindowToBuffer(newBuffer, window);
    }
}

void switch_or_split_window(WindowManager *wm, char *buffer_name, WindowParameters *parameters) {
    // Check if a window already displays the buffer
    Window *current = wm->head;
    while (current != NULL) {
        if (current->buffer && strcmp(current->buffer->name, buffer_name) == 0) {
            // Switch focus to the window that already displays the buffer
            wm->activeWindow->isActive = false;
            wm->activeWindow = current;
            current->isActive = true;
            return;
        }
        current = current->next;
    }

    // If no window displays the buffer, split the window
    split_window_right(wm, parameters);
    other_window(wm, 1);

    recenter(wm->activeWindow, true);

    // Check if the buffer exists in the BufferManager
    Buffer *existing = getBuffer(&bm, buffer_name);
    if (existing) {
        // Assign existing buffer to the new window and switch
        wm->activeWindow->buffer = existing;
        switchToBuffer(&bm, buffer_name);
    } else {
        // Create and switch to the new buffer
        char *fontPath = wm->activeWindow->buffer->fontPath;
        newBuffer(&bm, wm, buffer_name, "~/", fontPath);
        switchToBuffer(&bm, buffer_name);
    }
}

void split_window_below(WindowManager *wm, WindowParameters *parameters) {
    Window *active = wm->activeWindow;
    Window *newWindow = malloc(sizeof(Window));
    
    if (!newWindow) return;

    *newWindow = *active; // Copy settings from active window
    newWindow->height = active->height / 2;
    active->height = newWindow->height; // Make active window also half its original height

    // New window should be positioned directly below the active window
    newWindow->y = active->y - newWindow->height; // Subtract height because y increases upward

    active->splitOrientation = HORIZONTAL; // Set split orientation
    newWindow->splitOrientation = HORIZONTAL; // Set split orientation
    newWindow->parameters.noOtherWindow = &parameters->noOtherWindow;

    // Insert the new window into the list
    newWindow->next = active->next;
    if (active->next) {
        active->next->prev = newWindow;
    }

    // NOTE Update window-buffer (displayWindow) and window-modeline relationships
    newWindow->modeline.window = newWindow;
    addDisplayWindowToBuffer(newWindow->buffer, newWindow);


    active->next = newWindow;
    newWindow->prev = active;

    wm->count++;
}



void redistribute_window_space(WindowManager *wm, Window *win) {
    if (!win) return;

    // Adjust size of adjacent windows based on the orientation
    if (win->splitOrientation == HORIZONTAL) {
        // Combine heights if the split was horizontal
        if (win->prev) {
            win->prev->height += win->height;
        } else if (win->next) {
            win->next->y = win->y;
            win->next->height += win->height;
        }
    } else if (win->splitOrientation == VERTICAL) {
        // Combine widths if the split was vertical
        if (win->prev) {
            win->prev->width += win->width;
        } else if (win->next) {
            win->next->x = win->x;
            win->next->width += win->width;
        }
    }

    // Refresh the layout of remaining windows
    if (wm->head) {
        updateWindowDimensions(wm->head, 0, wm->head->y, wm->head->width, wm->head->height);
    }
}

void delete_window(WindowManager *wm) {
    if (wm->count <= 1) return; // Cannot delete the last window we could go into dashboard

    Window *active = wm->activeWindow;

    // Remove this window from its buffer's display list
    removeDisplayWindowFromBuffer(active->buffer, active);

    // Reassign active window
    if (active->prev) {
        wm->activeWindow = active->prev;
    } else if (active->next) {
        wm->activeWindow = active->next;
    }

    // Remove the window from the list
    if (active->prev) {
        active->prev->next = active->next;
    } else {
        wm->head = active->next; // Update head if the first window is being deleted
    }
    if (active->next) {
        active->next->prev = active->prev;
    }

    // Adjust size of adjacent windows based on the orientation
    if (active->splitOrientation == HORIZONTAL) {
        // Combine heights if the split was horizontal
        if (active->prev) {
            active->prev->height += active->height;
        } else if (active->next) {
            active->next->y = active->y;
            active->next->height += active->height;
        }
    } else if (active->splitOrientation == VERTICAL) {
        // Combine widths if the split was vertical
        if (active->prev) {
            active->prev->width += active->width;
        } else if (active->next) {
            active->next->x = active->x;
            active->next->width += active->width;
        }
    }

    free(active);
    wm->count--;
}




// TODO kill_other_windows()
void kill_window(WindowManager *wm, Window *win) {
    if (wm->count <= 1) return; // Cannot kill the last window

    // Remove from active window list
    if (win->prev) {
        win->prev->next = win->next;
    } else {
        wm->head = win->next; // Update head if this was the first window
    }
    if (win->next) {
        win->next->prev = win->prev;
    }

    // Update active window if necessary
    if (wm->activeWindow == win) {
        wm->activeWindow = (win->next ? win->next : win->prev);
        if (wm->activeWindow) {
            wm->activeWindow->isActive = true;
        }
    }

    // Clear window's list pointers
    win->prev = NULL;
    win->next = NULL;

    // Add to graveyard (inactive hashmap)
    WindowMap *entry = malloc(sizeof(WindowMap));
    if (!entry) {
        fprintf(stderr, "kill_window: Failed to allocate memory for hashmap entry\n");
        return;
    }
    entry->key = strdup(win->buffer->name);
    if (!entry->key) {
        free(entry);
        fprintf(stderr, "kill_window: Failed to duplicate buffer name\n");
        return;
    }
    entry->window = win;
    HASH_ADD_KEYPTR(hh, wm->graveyard, entry->key, strlen(entry->key), entry);

    // Remove from buffer's display windows
    removeDisplayWindowFromBuffer(win->buffer, win);

    // UPDATE wm->lastKilledBufferName
    if (wm->lastKilledBufferName) {
        free(wm->lastKilledBufferName);
    }
    wm->lastKilledBufferName = strdup(win->buffer->name);

    // Redistribute the space of the killed window
    redistribute_window_space(wm, win);

    // Update counts
    wm->count--;
    wm->inactive_count++;
}

Window* resurrect_window(WindowManager *wm, const char *buffer_name) {
    // Find the window in the graveyard (inactive hashmap)
    WindowMap *entry;
    HASH_FIND_STR(wm->graveyard, buffer_name, entry);
    if (!entry) {
        fprintf(stderr, "resurrect_window: No inactive window found for buffer '%s'\n", buffer_name);
        return NULL;
    }

    Window *win = entry->window;

    // Remove from graveyard
    HASH_DEL(wm->graveyard, entry);
    free(entry->key);
    free(entry);

    // Add to active list
    win->next = wm->head;
    if (wm->head) {
        wm->head->prev = win;
    }
    wm->head = win;

    // Add to buffer's display windows
    addDisplayWindowToBuffer(win->buffer, win);

    // Update counts
    wm->count++;
    wm->inactive_count--;

    return win;
}


// TODO Support NoOtherWindow Window parameter.
void other_window(WindowManager *wm, int direction) {
    if (direction == 1) {
        if (wm->activeWindow->next) {
            wm->activeWindow->isActive = false;
            wm->activeWindow = wm->activeWindow->next;
            wm->activeWindow->isActive = true;
        } else if (wm->head) {
            wm->activeWindow->isActive = false;
            wm->activeWindow = wm->head;
            wm->activeWindow->isActive = true;
        }
    } else if (direction == -1) {
        Window *current = wm->head;
        if (current == wm->activeWindow) {
            while (current->next) {
                current = current->next;
            }
            wm->activeWindow->isActive = false;
            wm->activeWindow = current;
            wm->activeWindow->isActive = true;
        } else {
            while (current->next != wm->activeWindow) {
                current = current->next;
            }
            wm->activeWindow->isActive = false;
            wm->activeWindow = current;
            wm->activeWindow->isActive = true;
        }
    }
}

void swap_windows(WindowManager *wm, int direction) {
    Window *tw = NULL; // Target Window
    if (direction == 1) {
        tw = wm->activeWindow->next ? wm->activeWindow->next : wm->head;
    } else {
        Window *current = wm->head;
        if (current == wm->activeWindow) {
            while (current->next) {
                current = current->next; // Go to the last window if current is head
            }
            tw = current;
        } else {
            while (current->next != wm->activeWindow) {
                current = current->next;
            }
            tw = current; // Set the target to the previous window
        }
    }
    
    if (tw) {
        // Swap buffers
        Buffer *tempBuffer = wm->activeWindow->buffer;
        wm->activeWindow->buffer = tw->buffer;
        tw->buffer = tempBuffer;
        
        // Swap window parameters if the global flag is set
        if (swap_windows_parameters) {
            WindowParameters tempParams = wm->activeWindow->parameters;
            wm->activeWindow->parameters = tw->parameters;
            tw->parameters = tempParams;
        }
        
        other_window(wm, direction);
    }
}

// TODO void set_window_parameter(Window *window, WindowParameters parameters, bool value) {}

void freeWindowManager(WindowManager *wm) {
    WindowMap *curr, *tmp;
    // Free graveyard
    HASH_ITER(hh, wm->graveyard, curr, tmp) {
        HASH_DEL(wm->graveyard, curr);
        free(curr->key);
        free(curr);
    }

    Window *current = wm->head;
    while (current != NULL) {
        Window *next = current->next;
        free(current);
        current = next;
    }
    wm->head = NULL;
    wm->activeWindow = NULL;
    wm->count = 0;
}

void updateWindowDimensions(Window *win, int x, int y, int width, int height) {
    if (win == NULL) return;

    // Set the current window dimensions
    win->x = x;
    win->y = y;
    win->width = width;
    win->height = height;

    // If the window has a next window in the same split orientation, recurse into it
    if (win->next && win->splitOrientation == win->next->splitOrientation) {
        if (win->splitOrientation == HORIZONTAL) {
            // Recurse with adjusted y and height
            updateWindowDimensions(win->next, x, y - height / 2, width, height / 2);
        } else if (win->splitOrientation == VERTICAL) {
            // Recurse with adjusted x and half the width
            updateWindowDimensions(win->next, x + width / 2, y, width / 2, height);
        }
    } else if (win->next) {
        // If the orientation changes, reset dimensions based on the orientation
        if (win->splitOrientation == HORIZONTAL) {
            updateWindowDimensions(win->next, x, y - height / 2, width, height / 2);
        } else {
            updateWindowDimensions(win->next, x + width / 2, y, width / 2, height);
        }
    }
}

void updateWindows(WindowManager *wm, Font *font, int newWidth, int newHeight) {
    // Start the recursive adjustment from the head window
    if (wm->head != NULL) {
        updateWindowDimensions(wm->head, 0, newHeight - (font->ascent - font->descent), newWidth, newHeight);
    }
}

void printActiveWindowDetails(WindowManager *wm) {
    Window *win = wm->activeWindow;
    if (!win) {
        printf("No active window.\n");
        return;
    }

    // Print window details in a structured format
    printf("\nActive Window Details:\n");
    printf("{\n");
    printf("  X: %.2f,\n", win->x);
    printf("  Y: %.2f,\n", win->y);
    printf("  Width: %.2f,\n", win->width);
    printf("  Height: %.2f,\n", win->height);
    printf("  Modeline Height: %.2f,\n", win->modeline.height);
    printf("  Active: %s,\n", win->isActive ? "True" : "False");
    printf("  Split Orientation: %s,\n", (win->splitOrientation == HORIZONTAL) ? "Horizontal" : "Vertical");
    printf("  Scroll: { X: %.2f, Y: %.2f },\n", win->scroll.x, win->scroll.y);
    printf("  Previous Window: %p,\n", (void *)win->prev);
    printf("  Next Window: %p,\n", (void *)win->next);
    printf("  Window Count in Manager: %d,\n", wm->count);
    printf("\n");

    // Print buffer details
    Buffer *buf = win->buffer;
    if (buf) {
        printf("  Buffer: {\n");
        printf("    Name: %s,\n", buf->name);
        printf("    Scale: %i,\n", buf->scale.index);
        printf("    Path: %s,\n", buf->path);
        printf("    Size: %zu,\n", buf->size);
        printf("    Capacity: %zu,\n", buf->capacity);
        printf("    Point: %zu,\n", buf->point);
        printf("    Region: { Start: %zu, End: %zu, Active: %s },\n", buf->region.start, buf->region.end, buf->region.active ? "True" : "False");
        printf("    ReadOnly: %s,\n", buf->readOnly ? "True" : "False");

        // Print syntax array details
        /* printf("    SyntaxArray: {\n"); */
        /* printf("      Used: %zu,\n", buf->syntaxArray.used); */
        /* printf("      Size: %zu,\n", buf->syntaxArray.size); */
        /* for (size_t i = 0; i < buf->syntaxArray.used; i++) { */
        /*     Syntax s = buf->syntaxArray.items[i]; */
        /*     printf("      Syntax: { Start: %zu, End: %zu, Color: { R: %u, G: %u, B: %u, A: %u } },\n",  */
        /*            s.start, s.end, s.color.r, s.color.g, s.color.b, s.color.a); */
        /* } */
        /* printf("    },\n"); // Close syntax array */

        // Optional: Include more buffer details as needed
        printf("  },\n"); // Close buffer
    }
    printf("}\n"); // Close window
}

bool isBottomWindow(WindowManager *wm, Window *window) {
    // Assuming vertical stacking of windows:
    for (Window *current = wm->head; current != NULL; current = current->next) {
        // Check if there is another window starting below the current one
        if (current != window && current->y > window->y) {
            return false;
        }
    }
    return true;
}




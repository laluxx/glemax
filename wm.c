#include "wm.h"
#include <stdlib.h>
#include <string.h>

// TODO i want it to mange X11 windows aswell add it to lume lazy fuck
// TODO Tags M-0..9

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
}

#include "edit.h"

// TODO Extract the smart part
void split_window_right(WindowManager *wm, Font *font, int sw, int sh) {
    Window *active = wm->activeWindow;
    Window *newWindow = malloc(sizeof(Window));

    *newWindow = *active; // Copy settings from active window
    newWindow->width /= 2;
    active->width -= newWindow->width;
    newWindow->x += active->width;

    active->splitOrientation = VERTICAL;
    newWindow->splitOrientation = VERTICAL;

    // Insert new window into the list
    newWindow->next = active->next;
    if (active->next) {
        active->next->prev = newWindow;
    }
    active->next = newWindow;
    newWindow->prev = active;

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
                find_file(&bm, wm, sw, sh);

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

void switch_or_split_window(WindowManager *wm, Font *font, char *buffer_name,
                            int sw, int sh) {
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
    split_window_right(wm, font, sw, sh);
    other_window(wm, 1);

    // Check if the buffer exists in the BufferManager
    Buffer *existing = getBuffer(&bm, buffer_name);
    if (existing) {
        // Assign existing buffer to the new window and switch
        wm->activeWindow->buffer = existing;
        switchToBuffer(&bm, buffer_name);
    } else {
        // Create and switch to the new buffer
        char *fontPath = wm->activeWindow->buffer->fontPath;
        newBuffer(&bm, wm, buffer_name, "~/", fontPath, sw, sh);
        switchToBuffer(&bm, buffer_name);
    }
}

// TODO if the buffer is already in the BufferManager just make it the wm->activeBuffer
// NOTE Same fix as find-file
/* void switch_or_split_window(WindowManager *wm, Font *font, char *buffer_name, */
/*                             int sw, int sh) { */
/*     // Check if a window already displays the buffer */
/*     Window *current = wm->head; */
/*     while (current != NULL) { */
/*         if (current->buffer && strcmp(current->buffer->name, buffer_name) == 0) { */
/*             // Switch focus to the window that already displays the buffer */
/*             wm->activeWindow->isActive = false; */
/*             wm->activeWindow = current; */
/*             current->isActive = true; */
/*             return; */
/*         } */
/*         current = current->next; */
/*     } */

/*     // If no window displays the buffer, split the window and create a new buffer */
/*     split_window_right(wm, font, sw, sh); */
/*     other_window(wm, 1); */

/*     // Create and switch to the new buffer */
/*     char *fontPath = wm->activeWindow->buffer->fontPath; */
/*     newBuffer(&bm, wm, buffer_name, "~/", fontPath, sw, sh); // Corrected &bm to bm */
/*     switchToBuffer(&bm, buffer_name); // Ensure the active window uses the new buffer */
/* } */

void split_window_below(WindowManager *wm, Font *font, int sw, int sh) {
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

    // Insert the new window into the list
    newWindow->next = active->next;
    if (active->next) {
        active->next->prev = newWindow;
    }
    active->next = newWindow;
    newWindow->prev = active;

    wm->count++;
}

void delete_window(WindowManager *wm) {
    if (wm->count <= 1) return; // Cannot delete the last window

    Window *active = wm->activeWindow;

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

void swap_window(WindowManager *wm, int direction) {
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
        Buffer *tempBuffer = wm->activeWindow->buffer;
        wm->activeWindow->buffer = tw->buffer;
        tw->buffer = tempBuffer;
        other_window(wm, direction);
    }
}


void freeWindowManager(WindowManager *wm) {
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



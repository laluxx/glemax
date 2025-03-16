#include <lume.h>
#include <ctype.h>
#include <font.h>
#include <stdlib.h>
#include "math.h"
#include "renderer.h"
#include "buffer.h"
#include "screen.h"
#include "theme.h"
#include "globals.h"



// UTILITY

float lerp(float a, float b, float t) {
    return a + t * (b - a);
}


// NOTE We do it on ever keypress seems right, right ? 
// TODO Scope module
void fill_scopes(Buffer *buffer, Scopes *scopes) {
    if (!hl_scope_mode) return;

    scopes->count = 0;
    int braceLevel = 0;

    for (size_t i = 0; i < buffer->size; i++) {
        char c = buffer->content[i];
        if (c == '{') {
            if (scopes->count >= scopes->capacity) {
                scopes->capacity = scopes->capacity * 2 + 1;
                scopes->items = realloc(scopes->items, scopes->capacity * sizeof(Scope));
            }
            scopes->items[scopes->count] = (Scope){i, 0, braceLevel};
            scopes->count++;
            braceLevel++;
        } else if (c == '}') {
            if (braceLevel > 0) {
                braceLevel--;
                // Find the matching opening brace and set its end
                for (int j = scopes->count - 1; j >= 0; j--) {
                    if (scopes->items[j].level == braceLevel) {
                        scopes->items[j].end = i;
                        break;
                    }
                }
            }
        }
    }
}

// TODO Scope syntax highligthing
// is this scope a function definition ?
Color getScopeColor(int level) {
    Color colors[] = {
        CT.bg, // or CT.moderline 
        CT.bg, // or make those colors a config
        CT.bg,
        CT.bg,
        CT.bg,
        CT.bg,
    };
    int numColors = sizeof(colors) / sizeof(colors[0]);
    
    Color scopeColor = colors[level % numColors];
    
    // Adjust brightness for deeper levels
    // Start darker and increase brightness
    float factor = hl_scope_base_brightness + (hl_scope_brightness_step * (level % numColors));
    scopeColor.r *= factor;
    scopeColor.g *= factor;
    scopeColor.b *= factor;
    
    return scopeColor;
}



void draw_scopes(WindowManager *wm, Font *font) {
    if (!hl_scope_mode) return;
    
    Buffer *buffer = wm->activeWindow->buffer;
    Window *activeWindow = wm->activeWindow;
    
    if (buffer->scopes.count == 0 || buffer->size == 0) return;

    float x = fringe + activeWindow->x - activeWindow->scroll.x;
    float y = activeWindow->y + font->ascent - font->descent * 2 + activeWindow->scroll.y;
    
    useShader("simple");
    
    size_t lineStart = 0;
    
    for (size_t i = 0; i <= buffer->size; i++) {
        if (i == buffer->size || buffer->content[i] == '\n') {
            for (size_t s = 0; s < buffer->scopes.count; s++) {
                Scope scope = buffer->scopes.items[s];
                
                if (scope.start >= buffer->size || scope.end > buffer->size) continue;
                
                // Check if the current line intersects with the scope
                if ((lineStart <= scope.end) && (i > scope.start)) {
                    float highlightWidth = activeWindow->width - (x - activeWindow->x);
                    
                    // Calculate the effective minimap width and padding
                    float effective_minimap_width = minimap_width;
                    float effective_minimap_padding = 0.0f;

                    if (minimap_padding_mode) {
                        if (keep_right_fringe) {
                            // Lerp the padding alongside the minimap width
                            effective_minimap_padding = lerp(0.0f, minimap_left_padding, minimap_width / (110.0f + minimap_left_padding));
                        } else {
                            // Apply padding immediately
                            effective_minimap_padding = minimap_left_padding;
                        }
                    }
                    
                    // Total space reserved for minimap (width + padding)
                    float total_minimap_space = effective_minimap_width + effective_minimap_padding;
                    
                    // Calculate the maximum width for the scope highlight
                    float maxWidth = activeWindow->width
                        - total_minimap_space
                        - fringe;
                    
                    if (highlightWidth > maxWidth)
                        highlightWidth = maxWidth;
                    
                    Color scopeColor = getScopeColor(scope.level);
                    Vec2f position = {x, y - font->ascent};
                    Vec2f size = {highlightWidth, font->ascent + font->descent};
                    drawRectangle(position, size, scopeColor);
                }
            }
            y -= (font->ascent + font->descent);
            lineStart = i + 1;
        }
    }
    
    flush();
}

// NOTE Don't keep space for the scrollbar, right fringe and minimap_left_padding 
/* void draw_scopes(WindowManager *wm, Font *font) { */
/*     if (!hl_scope_mode) return; */
    
/*     Buffer *buffer = wm->activeWindow->buffer; */
/*     Window *activeWindow = wm->activeWindow; */
    
/*     if (buffer->scopes.count == 0 || buffer->size == 0) return; */

/*     float x = fringe + activeWindow->x - activeWindow->scroll.x; */
/*     float y = activeWindow->y + font->ascent - font->descent * 2 + activeWindow->scroll.y; */
    
/*     useShader("simple"); */
    
/*     size_t lineStart = 0; */
    
/*     for (size_t i = 0; i <= buffer->size; i++) { */
/*         if (i == buffer->size || buffer->content[i] == '\n') { */
/*             for (size_t s = 0; s < buffer->scopes.count; s++) { */
/*                 Scope scope = buffer->scopes.items[s]; */
                
/*                 if (scope.start >= buffer->size || scope.end > buffer->size) continue; */
                
/*                 // Check if the current line intersects with the scope */
/*                 if ((lineStart <= scope.end) && (i > scope.start)) { */
/*                     float highlightWidth = activeWindow->width - (x - activeWindow->x); */
                    
/*                     // Lerp the padding space alongside the minimap width */
/*                     float minimap_padding = minimap_padding_mode */
/*                         ? lerp(0.0f, minimap_left_padding, minimap_width / 110.0f)  // Lerp the padding */
/*                         : 0; */
                    
/*                     float maxWidth = activeWindow->width */
/*                         - minimap_width  // Lerped width */
/*                         - fringe */
/*                         - minimap_padding;  // Lerped padding */
                    
/*                     if (highlightWidth > maxWidth) */
/*                         highlightWidth = maxWidth; */
                    
/*                     Color scopeColor = getScopeColor(scope.level); */
/*                     /\* scopeColor.a = 0.1f + (float)scope.level * 0.05f; *\/ */
                    
/*                     Vec2f position = {x, y - font->ascent}; */
/*                     Vec2f size = {highlightWidth, font->ascent + font->descent}; */
/*                     drawRectangle(position, size, scopeColor); */
/*                 } */
/*             } */
/*             y -= (font->ascent + font->descent); */
/*             lineStart = i + 1; */
/*         } */
/*     } */
    
/*     flush(); */
/* } */


// NOTE keep space for the scrollbar, right fringe and minimap_left_padding 
/* void draw_scopes(WindowManager *wm, Font *font) { */
/*     if (!hl_scope_mode) return; */
    
/*     Buffer *buffer = wm->activeWindow->buffer; */
/*     Window *activeWindow = wm->activeWindow; */
    
/*     if (buffer->scopes.count == 0 || buffer->size == 0) return; */

/*     float x = fringe + activeWindow->x - activeWindow->scroll.x; */
/*     float y = activeWindow->y + font->ascent - font->descent * 2 + activeWindow->scroll.y; */
    
/*     useShader("simple"); */
    
/*     size_t lineStart = 0; */
    
/*     for (size_t i = 0; i <= buffer->size; i++) { */
/*         if (i == buffer->size || buffer->content[i] == '\n') { */
/*             for (size_t s = 0; s < buffer->scopes.count; s++) { */
/*                 Scope scope = buffer->scopes.items[s]; */
                
/*                 if (scope.start >= buffer->size || scope.end > buffer->size) continue; */
                
/*                 // Check if the current line intersects with the scope */
/*                 if ((lineStart <= scope.end) && (i > scope.start)) { */
/*                     float highlightWidth = activeWindow->width - (x - activeWindow->x); */
                    
/*                     // Apply minimap left padding immediately */
/*                     float minimap_padding = minimap_padding_mode */
/*                         ? minimap_left_padding */
/*                         : 0; */
                    
/*                     // Use the current minimap_width for the rest of the animation */
/*                     float maxWidth = activeWindow->width */
/*                         - minimap_width  // Lerped width */
/*                         - fringe */
/*                         - minimap_padding;  // Immediate padding */
                    
/*                     if (highlightWidth > maxWidth) */
/*                         highlightWidth = maxWidth; */
                    
/*                     Color scopeColor = getScopeColor(scope.level); */
/*                     /\* scopeColor.a = 0.1f + (float)scope.level * 0.05f; *\/ */
                    
/*                     Vec2f position = {x, y - font->ascent}; */
/*                     Vec2f size = {highlightWidth, font->ascent + font->descent}; */
/*                     drawRectangle(position, size, scopeColor); */
/*                 } */
/*             } */
/*             y -= (font->ascent + font->descent); */
/*             lineStart = i + 1; */
/*         } */
/*     } */
    
/*     flush(); */
/* } */

// ORIGINAL
// TODO Go over the the minimap padding space when backslash is presse immidiatly
// not after the animation has finished
/* void draw_scopes(WindowManager *wm, Font *font) { */
/*     if (!hl_scope_mode) return; */
    
/*     Buffer *buffer = wm->activeWindow->buffer; */
/*     Window *activeWindow = wm->activeWindow; */
    
/*     if (buffer->scopes.count == 0 || buffer->size == 0) return; */

/*     float x = fringe + activeWindow->x - activeWindow->scroll.x; */
/*     float y = activeWindow->y + font->ascent - font->descent * 2 + activeWindow->scroll.y; */
    
/*     useShader("simple"); */
    
/*     size_t lineStart = 0; */
    
/*     for (size_t i = 0; i <= buffer->size; i++) { */
/*         if (i == buffer->size || buffer->content[i] == '\n') { */
/*             for (size_t s = 0; s < buffer->scopes.count; s++) { */
/*                 Scope scope = buffer->scopes.items[s]; */
                
/*                 if (scope.start >= buffer->size || scope.end > buffer->size) continue; */
                
/*                 // Check if the current line intersects with the scope */
/*                 if ((lineStart <= scope.end) && (i > scope.start)) { */
/*                     float highlightWidth = activeWindow->width - (x - activeWindow->x); */
                    
/*                     if (minimap_mode) { */
/*                         float minimap_padding = minimap_padding_mode */
/*                             ? minimap_left_padding */
/*                             : 0; */
/*                         float maxWidth = activeWindow->width */
/*                             - minimap_width */
/*                             - fringe */
/*                             - minimap_padding; */
/*                         if (highlightWidth > maxWidth) */
/*                             highlightWidth = maxWidth; */
/*                     } */
                    
/*                     Color scopeColor = getScopeColor(scope.level); */
/*                     /\* scopeColor.a = 0.1f + (float)scope.level * 0.05f; *\/ */
                    
/*                     Vec2f position = {x, y - font->ascent}; */
/*                     Vec2f size = {highlightWidth, font->ascent + font->descent}; */
/*                     drawRectangle(position, size, scopeColor); */
/*                 } */
/*             } */
/*             y -= (font->ascent + font->descent); */
/*             lineStart = i + 1; */
/*         } */
/*     } */
    
/*     flush(); */
/* } */



void drawScrollbar(Window *win, Color *color, size_t thickness) {
    if (!scroll_bar_mode) return;
    // I would *really* like to handle the mouse interactions inside this function
    
    // Calculate total content height
    int lineCount = 1;
    for (size_t i = 0; i < win->buffer->size; i++) {
        if (win->buffer->content[i] == '\n') {
            lineCount++;
        }
    }

    float lineHeight = (win->buffer->font->ascent + win->buffer->font->descent);
    float contentHeight = lineHeight * lineCount;

    // Only draw the scrollbar if the content height exceeds the window height
    if (contentHeight > win->height) {
        // Calculate the proportion of the visible area to the total content height
        float viewportRatio = win->height / contentHeight;
        float scrollbarHeight = fmaxf(viewportRatio * win->height, thickness);

        // Calculate the maximum scrollable distance
        float maxScrollY = contentHeight - win->height;

        // Calculate the scroll position (normalized to [0, 1])
        float scrollPosition = (maxScrollY > 0) ? (win->scroll.y / maxScrollY) : 0;

        // Position the scrollbar 2 pixels to the right of its original position
        float scrollbarX = minimap_mode ? win->x + win->width - minimap_width -
            thickness - 2 // 2 pixels to the right
            : win->x + win->width - thickness -
            2; // 2 pixels to the right

        // Calculate the scrollbar Y position (y grows upwards, so we invert the calculation)
        float scrollbarY = win->y - scrollbarHeight - (win->height - scrollbarHeight) * scrollPosition;

        // Ensure the scrollbar stays within the window bounds
        scrollbarY = fmaxf(win->y - win->height, fminf(scrollbarY, win->y - scrollbarHeight));

        useShader("simple");
        // Draw the vertical scrollbar
        drawRectangle((Vec2f){scrollbarX, scrollbarY},
                      (Vec2f){thickness, scrollbarHeight}, *color);
        flush();
    }
}

/* void drawScrollbars(Window *win, Color *color,float Thickness) { */
/*     // Calculate total content height and width */
/*     int lineCount = 1; */
/*     float contentWidth = 0; */
/*     float maxLineWidth = 0; */

/*     // Calculate line count and find the widest line */
/*     for (size_t i = 0; i < win->buffer->size; i++) { */
/*         contentWidth += getCharacterWidth(win->buffer->font, win->buffer->content[i]); */

/*         if (win->buffer->content[i] == '\n') { */
/*             lineCount++; */
/*             if (contentWidth > maxLineWidth) { */
/*                 maxLineWidth = contentWidth; */
/*             } */
/*             contentWidth = 0; */
/*         } */
/*     } */

/*     if (contentWidth > maxLineWidth) { */
/*         maxLineWidth = contentWidth; */
/*     } */

/*     float lineHeight = (win->buffer->font->ascent + win->buffer->font->descent); */
/*     float contentHeight = lineHeight * lineCount; */

/*     // Scrollbar padding */
/*     const float scrollbarPadding = 2.0f; */

/*     // Vertical scrollbar (right edge) */
/*     if (contentHeight > win->height) { */
/*         float viewportRatio = win->height / contentHeight; */
/*         float scrollbarHeight = fmaxf(viewportRatio * win->height, Thickness); */
/*         float maxScrollY = contentHeight - win->height; */
/*         float scrollPosition = (maxScrollY > 0) ? (win->scroll.y / maxScrollY) : 0; */

/*         // Position to the left of the minimap if enabled */
/*         float scrollbarX = minimap_mode */
/*             ? win->x + win->width - minimap_width - Thickness - scrollbarPadding */
/*             : win->x + win->width - Thickness - scrollbarPadding; */

/*         // Calculate scrollbar Y position (y grows upwards, so we invert the calculation) */
/*         float scrollbarY = win->y - scrollbarHeight - (win->height - scrollbarHeight) * scrollPosition; */

/*         // Ensure the scrollbar stays within the window bounds */
/*         scrollbarY = fmaxf(win->y - win->height, fminf(scrollbarY, win->y - scrollbarHeight)); */

/*         // Draw the vertical scrollbar */
/*         drawRectangle( */
/*             (Vec2f){scrollbarX, scrollbarY}, */
/*             (Vec2f){Thickness, scrollbarHeight}, */
/*             *color */
/*         ); */
/*     } */

/*     // Horizontal scrollbar (bottom left) */
/*     if (maxLineWidth > win->width) { */
/*         float viewportRatio = win->width / maxLineWidth; */
/*         float scrollbarWidth = fmaxf(viewportRatio * win->width, Thickness); */
/*         float maxScrollX = maxLineWidth - win->width; */
/*         float scrollPosition = (maxScrollX > 0) ? (win->scroll.x / maxScrollX) : 0; */

/*         // Calculate scrollbar X position (start from window->x + yThickness to avoid overlap with vertical scrollbar) */
/*         float scrollbarX = win->x + Thickness + scrollbarPadding + */
/*             (win->width - Thickness - scrollbarWidth - 2 * scrollbarPadding) * scrollPosition; */

/*         // Calculate scrollbar Y position (y grows upwards, so the scrollbar is at the bottom) */
/*         float scrollbarY = win->y - win->height - scrollbarPadding - Thickness; */

/*         // Ensure the scrollbar stays within the window bounds */
/*         scrollbarX = fmaxf(win->x + Thickness, fminf(scrollbarX, win->x + win->width - scrollbarWidth)); */

/*         // Draw the horizontal scrollbar */
/*         drawRectangle( */
/*             (Vec2f){scrollbarX, scrollbarY}, */
/*             (Vec2f){scrollbarWidth, Thickness}, */
/*             *color */
/*         ); */
/*     } */
/* } */


/* void drawScrollbars(Window *win, Color *xcolor, Color *ycolor) { */
/*     if (!win || !xcolor || !ycolor) return; */

/*     // Calculate total content height and width */
/*     int lineCount = 1; */
/*     float contentWidth = 0; */
/*     float maxLineWidth = 0; */

/*     // Calculate line count and find the widest line */
/*     for (size_t i = 0; i < win->buffer->size; i++) { */
/*         contentWidth += getCharacterWidth(win->buffer->font, win->buffer->content[i]); */

/*         if (win->buffer->content[i] == '\n') { */
/*             lineCount++; */
/*             if (contentWidth > maxLineWidth) { */
/*                 maxLineWidth = contentWidth; */
/*             } */
/*             contentWidth = 0; */
/*         } */
/*     } */

/*     if (contentWidth > maxLineWidth) { */
/*         maxLineWidth = contentWidth; */
/*     } */

/*     float lineHeight = (win->buffer->font->ascent + win->buffer->font->descent); */
/*     float contentHeight = lineHeight * lineCount; */

/*     // Scrollbar constants */
/*     const float scrollbarThickness = 8.0f; */
/*     const float scrollbarMinSize = 20.0f; */
/*     const float scrollbarPadding = 2.0f; */

/*     // Vertical scrollbar (right edge) */
/*     if (contentHeight > win->height) { */
/*         float viewportRatio = win->height / contentHeight; */
/*         float scrollbarHeight = fmaxf(viewportRatio * win->height, scrollbarMinSize); */
/*         float maxScrollY = contentHeight - win->height; */
/*         float scrollPosition = (maxScrollY > 0) ? (win->scroll.y / maxScrollY) : 0; */

/*         // Position to the left of the minimap if enabled */
/*         float scrollbarX = minimap_mode */
/*             ? win->x + win->width - minimap_width - scrollbarThickness - scrollbarPadding */
/*             : win->x + win->width - scrollbarThickness - scrollbarPadding; */

/*         // Calculate scrollbar Y position (y grows upwards, so we invert the calculation) */
/*         float scrollbarY = win->y - win->height + (win->height - scrollbarHeight) * scrollPosition; */

/*         // Ensure the scrollbar stays within the window bounds */
/*         scrollbarY = fmaxf(win->y - win->height, fminf(scrollbarY, win->y - scrollbarHeight)); */

/*         // Draw the vertical scrollbar */
/*         drawRectangle( */
/*                       (Vec2f){scrollbarX, scrollbarY}, */
/*                       (Vec2f){scrollbarThickness, scrollbarHeight}, */
/*                       *ycolor */
/*                       ); */
/*     } */

/*     // Horizontal scrollbar (bottom left) */
/*     if (maxLineWidth > win->width) { */
/*         float viewportRatio = win->width / maxLineWidth; */
/*         float scrollbarWidth = fmaxf(viewportRatio * win->width, scrollbarMinSize); */
/*         float maxScrollX = maxLineWidth - win->width; */
/*         float scrollPosition = (maxScrollX > 0) ? (win->scroll.x / maxScrollX) : 0; */

/*         // Calculate scrollbar X position */
/*         float scrollbarX = win->x + scrollbarPadding + */
/*             (win->width - scrollbarWidth - 2 * scrollbarPadding) * scrollPosition; */

/*         // Calculate scrollbar Y position (y grows upwards, so the scrollbar is at the bottom) */
/*         float scrollbarY = win->y - win->height - scrollbarPadding - scrollbarThickness; */

/*         // Ensure the scrollbar stays within the window bounds */
/*         scrollbarX = fmaxf(win->x, fminf(scrollbarX, win->x + win->width - scrollbarWidth)); */

/*         // Draw the horizontal scrollbar */
/*         drawRectangle( */
/*                       (Vec2f){scrollbarX, scrollbarY}, */
/*                       (Vec2f){scrollbarWidth, scrollbarThickness}, */
/*                       *xcolor */
/*                       ); */
/*     } */
/* } */


/// MINIMAP

// TODO Simplify and optimize the minimap fnctions
// We could generate a texture of the minimap and correctly
// update it when we will have an undo system not on every key

void drawMinimapView(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    // TODO
}

void drawMinimapRegion(WindowManager *wm, Window *mainWindow, Buffer *buffer) {

    float minimap_width = 110;
    float minimapX = mainWindow->x + mainWindow->width - minimap_width;
    float lineHeight = 2.0;
    float scale = 1.0;

    useShader("simple");

    size_t start = buffer->region.start;
    size_t end = buffer->region.end;
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    size_t currentLineStart = 0;
    float x = minimapX;
    float y = mainWindow->y;

    // Iterate over each character to find line starts and ends
    for (size_t i = 0; i <= buffer->size; i++) {
        if (buffer->content[i] == '\n' ||
            i == buffer->size) { // End of line or buffer
            if (i >= start &&
                currentLineStart <= end) { // Check if line contains region
                size_t lineStart =
                    (currentLineStart > start) ? currentLineStart : start;
                size_t lineEnd = (i < end) ? i : end;
                size_t lineLength = lineEnd - lineStart;

                if (lineLength > 0 || (lineEnd == i && buffer->content[i] == '\n')) {
                    // Calculate highlight width in minimap scale
                    float highlightWidth = 0;
                    float lineX = x;

                    // Move to start of region
                    for (size_t j = currentLineStart; j < lineStart; j++) {
                        lineX += scale;
                    }

                    // Calculate width of highlighted region
                    for (size_t j = lineStart; j < lineEnd; j++) {
                        highlightWidth += scale;
                    }

                    // Extend highlight to end of minimap if line is fully selected
                    if (buffer->content[i] == '\n' && lineEnd == i &&
                        (end != i || i == buffer->size)) {
                        highlightWidth = minimap_width - (lineX - x);
                    }

                    // Draw the region highlight rectangle
                    Vec2f position = {lineX, y - 1}; // -1 to center in line space
                    Vec2f size = {highlightWidth,
                                  lineHeight + 2}; // Height includes spacing
                    drawRectangle(position, size, CT.region);
                }
            }
            currentLineStart = i + 1; // Move to next line
            y -= (lineHeight + 2); // Move y to next line position including spacing
        }
    }

    flush();
}



void drawMinimapCursor(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    if (!minimap_cursor || !wm || !mainWindow || !buffer)
        return;

    float minimap_width = 110;
    float minimapX = mainWindow->x + mainWindow->width - minimap_width;
    float lineHeight = 2.0;
    float scale = 1.0;

    useShader("simple");

    // Calculate cursor position similar to main cursor
    float cursorX = minimapX;
    float cursorY = mainWindow->y;
    int lineCount = 0;
    size_t lineStart = 0;

    // Calculate cursor position
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            cursorX = minimapX;
            lineStart = i + 1;
        } else {
            // Scale character width to minimap size
            cursorX += scale;
        }
    }

    // Determine cursor width, handling newline and end of buffer cases
    float cursorWidth = scale; // Default to one character width in minimap scale

    // Adjust Y position using the same spacing as minimap
    cursorY -= lineCount * (lineHeight + 2);

    // Create extended cursor that uses the line spacing
    Vec2f cursorPosition = {cursorX, cursorY - 1};    // Offset up by 1 to center
    Vec2f cursorSize = {cursorWidth, lineHeight + 2}; // Height includes spacing

    // Determine cursor color considering crystal_cursor_mode
    Color cursorColor = CT.cursor;
    if (buffer->region.active && hide_region_mode) {
        cursorColor = CT.null;
    } else if (crystal_cursor_mode) {
        for (size_t i = 0; i < buffer->syntaxArray.used; i++) {
            if (buffer->point >= buffer->syntaxArray.items[i].start &&
                buffer->point < buffer->syntaxArray.items[i].end) {
                cursorColor = *buffer->syntaxArray.items[i].color;
                break;
            }
        }
    }

    // Handle cursor blinking logic
    if (blink_cursor_mode && blinkCount < blink_cursor_blinks) {
        double currentTime = getTime();
        if (currentTime - lastBlinkTime >=
            (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) {
            cursorVisible = !cursorVisible;
            lastBlinkTime = currentTime;
            if (cursorVisible) {
                blinkCount++;
            }
        }

        if (cursorVisible) {
            drawRectangle(cursorPosition, cursorSize, cursorColor);
        }
    } else {
        drawRectangle(cursorPosition, cursorSize, cursorColor);
    }

    flush();
}


/**
 * Draws a word in the minimap, applying syntax highlighting if available.
 * Falls back to CT.text if no syntax highlighting is defined.
 */
void drawWordInMinimap(Buffer *buffer, float minimapX, float startY, size_t lineStart, size_t wordStart, size_t wordEnd, float maxTextWidth, float scale, float lineHeight) {
    // If the syntaxArray is empty, draw the word using CT.text
    if (buffer->syntaxArray.used == 0) {
        float wordStartX = minimapX + (wordStart - lineStart) * scale;
        float wordWidth = (wordEnd - wordStart) * scale;
        drawRectangle((Vec2f){wordStartX, startY}, (Vec2f){wordWidth, lineHeight}, CT.text);
        return;
    }

    // Draw syntax highlights for the word
    for (size_t j = 0; j < buffer->syntaxArray.used; j++) {
        Syntax syntax = buffer->syntaxArray.items[j];
        if (syntax.start >= wordStart && syntax.start < wordEnd) {
            float relativeStart = syntax.start - lineStart;
            if (relativeStart < maxTextWidth) {
                float syntaxStartX = minimapX + relativeStart * scale;
                float syntaxWidth = fmin((syntax.end < wordEnd ? syntax.end : wordEnd) - syntax.start, maxTextWidth - relativeStart) * scale;
                drawRectangle((Vec2f){syntaxStartX, startY}, (Vec2f){syntaxWidth, lineHeight}, *syntax.color);
            }
        }
    }
}

/**
 * Draws comments that extend until the end of the line.
 */
void drawCommentUntilEndOfLine(Buffer *buffer, float minimapX, float startY, size_t lineStart, size_t lineEnd, float maxTextWidth, float scale, float lineHeight) {
    for (size_t j = 0; j < buffer->syntaxArray.used; j++) {
        Syntax syntax = buffer->syntaxArray.items[j];
        if (syntax.start >= lineStart && syntax.start < lineEnd && syntax.color == &CT.comment) {
            float relativeStart = syntax.start - lineStart;
            if (relativeStart < maxTextWidth) {
                float syntaxStartX = minimapX + relativeStart * scale;
                float syntaxWidth = fmin(lineEnd - syntax.start, maxTextWidth - relativeStart) * scale;
                drawRectangle((Vec2f){syntaxStartX, startY}, (Vec2f){syntaxWidth, lineHeight}, *syntax.color);
            }
        }
    }
}


/**
 * Draws the minimap for the given buffer in the specified window.
 * - If the buffer's syntaxArray is empty, falls back to drawing all text using CT.text.
 * - Handles comments that extend until the end of the line.
 */
// TODO Wrap lines
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    float minimapX = mainWindow->x + mainWindow->width - minimap_width;  // Positioned on the right edge of the main window
    float lineHeight = 2.0;  // Height of each line in the minimap
    float scale = 1.0;       // Scale factor for text rendering
    float startY = mainWindow->y;  // Starting Y position for drawing
    size_t lineStart = 0;    // Start index of the current line
    size_t wordStart = 0;    // Start index of the current word
    int currentLine = 0;     // Current line number

    // Calculate the maximum width of text that can fit in the minimap
    float maxTextWidth = minimap_width / scale;

    // Use the "simple" shader for drawing
    useShader("simple");

    // Draw the minimap region and view
    drawMinimapRegion(wm, mainWindow, buffer);
    drawMinimapView(wm, mainWindow, buffer);

    // Draw the minimap background
    drawRectangle((Vec2f){sw - minimap_width, sw}, (Vec2f){sw - minimap_width, sw}, CT.modeline);

    // UPDATE LERP
    if (minimap_lerp_active) {
        // Lerp minimap_width towards target_width
        minimap_width = lerp(minimap_width, minimap_target_width, 0.1f);
        printf("Lerping minimap_width: %.2f -> %.2f\n", minimap_width, minimap_target_width);

        // Check if close enough to stop lerping
        if (fabs(minimap_width - minimap_target_width) < 1.0f) {
            minimap_width = minimap_target_width;
            minimap_lerp_active = false;

            // If the target width is 0, disable the minimap
            if (minimap_target_width == 0.0f) {
                minimap_mode = false;
            }
            printf("Lerping complete | Minimap mode: %d | Minimap width: %.2f\n", minimap_mode, minimap_width);
        }
    }

    /* if (minimap_lerp_active) { */
    /*     // Lerp minimap_width towards target_width */
    /*     minimap_width = lerp(minimap_width, minimap_target_width, 0.1f); */
    /*     // Check if close enough to stop lerping */
    /*     if (fabs(minimap_width - minimap_target_width) < 1.0f) { */
    /*         minimap_width = minimap_target_width; */
    /*         minimap_lerp_active = false; */
    /*         minimap_mode = (minimap_width > 0.0f); // Update minimap_mode based on final width */
    /*     } */
    /* } */




    // Iterate through each character in the buffer to draw syntax highlights
    for (size_t i = 0; i <= buffer->size; i++) {
        // Check for word end or line end
        if (buffer->content[i] == ' ' || buffer->content[i] == '\n' || i == buffer->size) {
            // If there's a word to draw
            if (i > wordStart) {
                drawWordInMinimap(buffer, minimapX, startY, lineStart, wordStart, i, maxTextWidth, scale, lineHeight);
            }
            wordStart = i + 1;  // Update start of next word
        }

        // Check for line end
        if (buffer->content[i] == '\n' || i == buffer->size) {
            // Handle comments that extend until the end of the line
            if (buffer->syntaxArray.used > 0) {
                drawCommentUntilEndOfLine(buffer, minimapX, startY, lineStart, i, maxTextWidth, scale, lineHeight);
            }

            startY -= (lineHeight + 2);  // Move up for the next line and add 2 pixel spacing
            lineStart = i + 1;           // Update the start of the next line
            wordStart = lineStart;        // Reset word start to the beginning of the next line
            currentLine++;
        }
    }

    // Draw the minimap cursor
    drawMinimapCursor(wm, mainWindow, buffer);
    flush();
}


void drawHollowPoint(Buffer *buffer, Window *win, size_t position, Color color) {
    // Validate input
    if (!buffer || !win) return;
    if (!buffer->content) return;
    if (buffer->size == 0) return;
    if (position >= buffer->size) return;

    float x = win->x - win->scroll.x; // Start position adjusted for horizontal scroll
    float y = win->y;
    int lineCount = 0;

    // Calculate position
    for (size_t i = 0; i < position; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            x = win->x - win->scroll.x;
        } else {
            x += getCharacterWidth(buffer->font, buffer->content[i]);
        }
    }

    // Compute y position
    y += win->scroll.y -
        lineCount * (buffer->font->ascent + buffer->font->descent) -
        (buffer->font->descent * 2);

    // Calculate width
    float width = (position < buffer->size && buffer->content[position] != '\n')
        ? getCharacterWidth(buffer->font, buffer->content[position])
        : getCharacterWidth(buffer->font, ' ');

    float height = buffer->font->ascent + buffer->font->descent;
    float lineThickness =
        fmax(1.0f, height * 0.03f); // Scale line thickness with font size

    useShader("simple");

    // Top line
    drawRectangle((Vec2f){fringe + x, y + height - lineThickness},
                  (Vec2f){width, lineThickness}, color);
    // Bottom line
    drawRectangle((Vec2f){fringe + x, y},
                  (Vec2f){width, lineThickness}, color);
    // Left line
    drawRectangle((Vec2f){fringe + x, y},
                  (Vec2f){lineThickness, height}, color);
    // Right line
    drawRectangle((Vec2f){fringe + x + width - lineThickness, y},
                  (Vec2f){lineThickness, height}, color);
    flush();
}

void drawMark(Buffer *buffer, Window *win, Color markColor) {
    drawHollowPoint(buffer, win, buffer->region.mark, foregroundColorAtPoint(buffer, buffer->region.mark));
}

void drawHollowCursor(Buffer *buffer, Window *win, Color defaultColor) {
    // Determine the cursor color based on syntax highlighting
    Color cursorColor = defaultColor;
    if (crystal_cursor_mode) {
        for (size_t i = 0; i < buffer->syntaxArray.used; i++) {
            if (buffer->point >= buffer->syntaxArray.items[i].start &&
                buffer->point < buffer->syntaxArray.items[i].end) {
                cursorColor = *buffer->syntaxArray.items[i].color;
                break;
            }
        }
    }

    drawHollowPoint(buffer, win, buffer->point, cursorColor);
}


void drawCursor(Buffer *buffer, Window *win, Color defaultColor) {
    float startX = fringe + win->x - win->scroll.x;
    float cursorX = startX;
    int lineCount = 0;

    // Calculate maximum content width considering minimap and window parameters
    float maxContentWidth = win->width - fringe;
    if (minimap_mode) {
        float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0;
        maxContentWidth -= (minimap_width + minimap_padding);
    }
    float maxX = startX + maxContentWidth;

    // Iterate through characters up to the cursor's position to calculate wraps
    for (size_t i = 0; i < buffer->point; i++) {
        char c = buffer->content[i];
        if (c == '\n') {
            lineCount++;
            cursorX = startX;
        } else {
            float charWidth = getCharacterWidth(buffer->font, c);
            float newX = cursorX + charWidth;
            // Check if wrapping is needed when truncateLines is disabled
            if (!win->parameters.truncateLines && newX > maxX) {
                lineCount++; // Wrap to next line
                cursorX = startX + charWidth;
            } else {
                cursorX = newX;
            }
        }
    }

    // Determine cursor width based on current character or space if at line end
    float cursorWidth;
    if (buffer->point < buffer->size && buffer->content[buffer->point] != '\n') {
        cursorWidth = getCharacterWidth(buffer->font, buffer->content[buffer->point]);
    } else {
        cursorWidth = getCharacterWidth(buffer->font, ' ');
    }

    // Calculate Y position considering line wraps and newlines
    float cursorY = win->y + win->scroll.y 
        - lineCount * (buffer->font->ascent + buffer->font->descent)
        - (buffer->font->descent * 2);

    Vec2f cursorPosition = {cursorX, cursorY};
    Vec2f cursorSize = {cursorWidth, buffer->font->ascent + buffer->font->descent};

    // Determine cursor color
    Color cursorColor = defaultColor;
    if (buffer->region.active && hide_region_mode) {
        cursorColor = CT.null;
    } else {
        cursorColor = foregroundColorAtPoint(buffer, buffer->point);
    }

    // Handle cursor blinking
    if (blink_cursor_mode && blinkCount < blink_cursor_blinks) {
        double currentTime = getTime();
        if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) {
            cursorVisible = !cursorVisible;
            lastBlinkTime = currentTime;
            if (cursorVisible) blinkCount++;
        }
        if (cursorVisible) {
            drawRectangle(cursorPosition, cursorSize, cursorColor);
        }
    } else {
        drawRectangle(cursorPosition, cursorSize, cursorColor);
    }
}

/* void drawCursor(Buffer *buffer, Window *win, Color defaultColor) { */
/*     float cursorX = fringe + win->x - win->scroll.x;  // Start position adjusted for horizontal scroll */
/*     float cursorY = win->y; */
/*     int lineCount = 0; */
    
/*     // Calculate the cursor's x offset within the content of the buffer */
/*     for (size_t i = 0; i < buffer->point; i++) { */
/*         if (buffer->content[i] == '\n') { */
/*             lineCount++;  // Increment line count at each newline */
/*             cursorX = fringe + win->x - win->scroll.x;  // Reset cursor x to the start of the window on new lines, adjusted for horizontal scroll */
/*         } else { */
/*             cursorX += getCharacterWidth(buffer->font, buffer->content[i]);  // Move cursor right by character width */
/*         } */
/*     } */
    
/*     // Determine cursor width, handle newline or end of buffer cases */
/*     float cursorWidth = (buffer->point < buffer->size && buffer->content[buffer->point] != '\n') ? */
/*         getCharacterWidth(buffer->font, buffer->content[buffer->point]) : */
/*         getCharacterWidth(buffer->font, ' ');  // Use a standard width if at the end of a line or buffer */
    
/*     // Compute cursor's y position based on the number of lines, subtract from initial y and add the scroll */
/*     cursorY += win->scroll.y - lineCount * (buffer->font->ascent + buffer->font->descent) - (buffer->font->descent * 2); */
    
/*     // Create a rectangle representing the cursor */
/*     Vec2f cursorPosition = {cursorX, cursorY}; */
/*     Vec2f cursorSize = {cursorWidth, buffer->font->ascent + buffer->font->descent}; */
    
/*     // Determine the cursor color */
/*     Color cursorColor = defaultColor; */
/*     if (buffer->region.active && hide_region_mode) { */
/*         cursorColor = CT.null; */
/*     } else { */
/*         cursorColor = foregroundColorAtPoint(buffer, buffer->point); */
/*     } */
    
/*     // Handle cursor blinking logic */
/*     if (blink_cursor_mode && blinkCount < blink_cursor_blinks) { */
/*         double currentTime = getTime(); */
/*         if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) { */
/*             cursorVisible = !cursorVisible; */
/*             lastBlinkTime = currentTime; */
/*             if (cursorVisible) { */
/*                 blinkCount++; */
/*             } */
/*         } */
        
/*         if (cursorVisible) { */
/*             drawRectangle(cursorPosition, cursorSize, cursorColor);  // Draw the cursor if visible */
/*         } */
/*     } else { */
/*         drawRectangle(cursorPosition, cursorSize, cursorColor);  // Always draw cursor with syntax color if not blinking or crystal_cursor_mode is true */
/*     } */
/* } */

// TODO once we support the minibuffer as a window remove this function
void drawMiniCursor(Buffer *buffer, Font *font, float x, float y, Color color) {
    int lineCount = 0;
    float cursorX = x;
    
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            cursorX = x;
        } else {
            cursorX += getCharacterWidth(font, buffer->content[i]);
        }
    }
    
    float cursorWidth = buffer->point < buffer->size && buffer->content[buffer->point] != '\n'
        ? getCharacterWidth(font, buffer->content[buffer->point])
        : getCharacterWidth(font, ' ');
    
    Vec2f cursorPosition = {cursorX, y - lineCount * (font->ascent + font->descent) - font->descent};
    Vec2f cursorSize = {cursorWidth, font->ascent + font->descent};
    
    if (blink_cursor_mode && blinkCount < blink_cursor_blinks) {
        double currentTime = getTime();
        if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) {
            cursorVisible = !cursorVisible;
            lastBlinkTime = currentTime;
            if (cursorVisible) {
                blinkCount++;  // Increment only on visibility toggle to visible
            }
        }
        
        if (cursorVisible) {
            drawRectangle(cursorPosition, cursorSize, color);
        }
    } else {
        drawRectangle(cursorPosition, cursorSize, color);  // Always draw cursor if not blinking
    }
}

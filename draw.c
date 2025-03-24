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
#include "edit.h" // for find_visual_line maybe a line module would be nice



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
                    float effective_minimap_width = activeWindow->parameters.minimap_width;
                    float effective_minimap_padding = 0.0f;

                    if (minimap_padding_mode) {
                        if (!keep_right_fringe) {
                            // Lerp the padding alongside the minimap width
                            effective_minimap_padding = lerp(0.0f, minimap_left_padding, activeWindow->parameters.minimap_width / (110.0f + minimap_left_padding));
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



// decent 
void drawScrollbar(Window *win, Color *color, size_t thickness) {
    if (!scroll_bar) return;
    
    if (!win->parameters.minimap) scroll_bar_right_padding = 0;
    
    // Calculate content height
    int lineCount = 1;
    for (size_t i = 0; i < win->buffer->size; i++) {
        if (win->buffer->content[i] == '\n') lineCount++;
    }
    
    float lineHeight = win->buffer->font->ascent + win->buffer->font->descent;
    float contentHeight = lineHeight * lineCount;
    if (contentHeight <= win->height) return;
    
    // Calculate scrollbar dimensions
    float viewportRatio = win->height / contentHeight;
    float scrollbarHeight = fmaxf(viewportRatio * win->height, thickness);
    float maxScrollY = contentHeight - win->height;
    float scrollPosition = (maxScrollY > 0) ? (win->scroll.y / maxScrollY) : 0;
    
    // Calculate base X position and width
    float scrollbarX, scrollbarWidth;
    if (win->parameters.minimap) {
        scrollbarX = win->x + win->width - win->parameters.minimap_width - thickness;
    } else {
        scrollbarX = win->x + win->width - thickness;
    }
    
    // Apply padding and trimming
    scrollbarX -= scroll_bar_right_padding;
    scrollbarX += scroll_bar_left_trim;  // Trim from left
    scrollbarWidth = thickness - scroll_bar_left_trim;
    
    // Handle animations - either minimap easing or dedicated scrollbar easing
    if (minimap_easing_mode && (win->parameters.minimap_lerp_active || scroll_bar_lerp_active)) {
        float lerpFactor = win->parameters.minimap_width / (110.0f + minimap_left_padding);
        
        if (win->parameters.minimap) {
            // Showing minimap
            if (show_scroll_bar_with_minimap && scroll_bar_lerp_active) {
                scrollbarWidth = lerp(0.0f, scrollbarWidth, lerpFactor);
                scrollbarX = win->x + win->width - win->parameters.minimap_width - scrollbarWidth - scroll_bar_right_padding;
            }
        } else {
            // Hiding minimap
            if (hide_scroll_bar_with_minimap && scroll_bar_lerp_active) {
                scrollbarWidth = lerp(scrollbarWidth, 0.0f, lerpFactor);
                scrollbarX = win->x + win->width - thickness + (thickness - scrollbarWidth) - scroll_bar_right_padding;
            } else if (!hide_scroll_bar_with_minimap) {
                // Keep scrollbar visible when hiding minimap and hide_scroll_bar_with_minimap is false
                scrollbarX = win->x + win->width - thickness - scroll_bar_right_padding;
            }
        }
    }
    
    // Calculate Y position (0,0 is bottom-left)
    float windowBottom = win->y - win->height;
    float scrollbarY = windowBottom + (win->height - scrollbarHeight) * (1.0f - scrollPosition);
    scrollbarY = fmaxf(windowBottom, fminf(scrollbarY, win->y - scrollbarHeight));
    
    // Draw scrollbar
    useShader("simple");
    drawRectangle((Vec2f){scrollbarX, scrollbarY},
                  (Vec2f){scrollbarWidth, scrollbarHeight}, *color);
    flush();
}

// TODO Simplify and optimize the minimap fnctions
// We could generate a texture of the minimap and correctly
// update it when we will have an undo system not on every key
void drawMinimapRegion(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    if (mainWindow->buffer->region.active == false) return;
    
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
// TODO Draw rectangles starting from begin-of-defun and ends at end-of-defun
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    float minimapX = mainWindow->x + mainWindow->width - mainWindow->parameters.minimap_width;  // Positioned on the right edge of the main window
    float lineHeight = 2.0;  // Height of each line in the minimap
    float scale = 1.0;       // Scale factor for text rendering
    float startY = mainWindow->y;  // Starting Y position for drawing
    size_t lineStart = 0;    // Start index of the current line
    size_t wordStart = 0;    // Start index of the current word
    int currentLine = 0;     // Current line number
    // Calculate the maximum width of text that can fit in the minimap
    float maxTextWidth = mainWindow->parameters.minimap_width / scale;

    useShader("simple");
    drawMinimapRegion(wm, mainWindow, buffer);

    /* // Draw the minimap background */
    /* drawRectangle((Vec2f){sw - mainWindow->parameters.minimap_width, sw}, (Vec2f){sw - mainWindow->parameters.minimap_width, sw}, CT.modeline); */

    // UPDATE LERP
    if (mainWindow->parameters.minimap_lerp_active) {
        // Lerp minimap_width towards target_width
        mainWindow->parameters.minimap_width = lerp(mainWindow->parameters.minimap_width, mainWindow->parameters.minimap_target_width, 0.1f);

        // Check if close enough to stop lerping
        if (fabs(mainWindow->parameters.minimap_width - mainWindow->parameters.minimap_target_width) < 1.0f) {
            mainWindow->parameters.minimap_width = mainWindow->parameters.minimap_target_width;
            mainWindow->parameters.minimap_lerp_active = false;

            // If the target width is 0, disable the minimap
            if (mainWindow->parameters.minimap_target_width == 0.0f) {
                mainWindow->parameters.minimap = false;
            }
        }
    }

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
    float x = win->x - win->scroll.x; // Start position adjusted for horizontal scroll
    float y = win->y;
    int lineCount = 0;
    
    // Calculate max X position for wrapping/truncation
    float baseX = win->x - win->scroll.x;
    float maxContentWidth = win->width - fringe;
    if (win->parameters.minimap) {
        float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0;
        maxContentWidth -= (win->parameters.minimap_width + minimap_padding);
    }
    float maxX = baseX + maxContentWidth;
    
    // Calculate position
    size_t i = 0;
    while (i < position) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            x = win->x - win->scroll.x;
            i++;
        } else {
            float charWidth = getCharacterWidth(buffer->font, buffer->content[i]);
            
            // Handle line wrapping or truncation
            if (!win->parameters.truncateLines && x + charWidth > maxX) {
                // Visual line wrapping
                lineCount++;
                x = win->x - win->scroll.x;
            } else if (win->parameters.truncateLines && x >= maxX) {
                // Skip to the end of logical line when truncating
                while (i < position && buffer->content[i] != '\n') {
                    i++;
                }
                continue;
            }
            
            x += charWidth;
            i++;
        }
    }
    
    // Compute y position exactly as in the original function
    y += win->scroll.y - 
        lineCount * (buffer->font->ascent + buffer->font->descent) - 
        (buffer->font->descent * 2);
    
    // Calculate width
    float width = (position < buffer->size && buffer->content[position] != '\n')
        ? getCharacterWidth(buffer->font, buffer->content[position])
        : getCharacterWidth(buffer->font, ' ');
    
    // Check if point needs to wrap to the next line
    if (!win->parameters.truncateLines && x + width > maxX) {
        x = win->x - win->scroll.x;
        y -= (buffer->font->ascent + buffer->font->descent);
    }
    
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
    if (hide_mark_when_region_active) {if (buffer->region.active) return;}
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
    if (win->parameters.minimap) {
        float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0;
        maxContentWidth -= (win->parameters.minimap_width + minimap_padding);
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
                lineCount++; // Wrap to next visual line
                cursorX = startX + charWidth;
            } else {
                cursorX = newX;
            }
        }
    }

    // Determine if the cursor is at the end of a wrapped line
    if (buffer->point < buffer->size && buffer->content[buffer->point] != '\n') {
        float charWidth = getCharacterWidth(buffer->font, buffer->content[buffer->point]);
        if (cursorX + charWidth > maxX) {
            // Cursor is at the end of a wrapped line, move to the start of the next visual line
            lineCount++;
            cursorX = startX;
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
/*     float startX = fringe + win->x - win->scroll.x; */
/*     float cursorX = startX; */
/*     int lineCount = 0; */

/*     // Calculate maximum content width considering minimap and window parameters */
/*     float maxContentWidth = win->width - fringe; */
/*     if (minimap) { */
/*         float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0; */
/*         maxContentWidth -= (minimap_width + minimap_padding); */
/*     } */
/*     float maxX = startX + maxContentWidth; */

/*     // Iterate through characters up to the cursor's position to calculate wraps */
/*     for (size_t i = 0; i < buffer->point; i++) { */
/*         char c = buffer->content[i]; */
/*         if (c == '\n') { */
/*             lineCount++; */
/*             cursorX = startX; */
/*         } else { */
/*             float charWidth = getCharacterWidth(buffer->font, c); */
/*             float newX = cursorX + charWidth; */
/*             // Check if wrapping is needed when truncateLines is disabled */
/*             if (!win->parameters.truncateLines && newX > maxX) { */
/*                 lineCount++; // Wrap to next line */
/*                 cursorX = startX + charWidth; */
/*             } else { */
/*                 cursorX = newX; */
/*             } */
/*         } */
/*     } */

/*     // Determine cursor width based on current character or space if at line end */
/*     float cursorWidth; */
/*     if (buffer->point < buffer->size && buffer->content[buffer->point] != '\n') { */
/*         cursorWidth = getCharacterWidth(buffer->font, buffer->content[buffer->point]); */
/*     } else { */
/*         cursorWidth = getCharacterWidth(buffer->font, ' '); */
/*     } */

/*     // Calculate Y position considering line wraps and newlines */
/*     float cursorY = win->y + win->scroll.y  */
/*         - lineCount * (buffer->font->ascent + buffer->font->descent) */
/*         - (buffer->font->descent * 2); */

/*     Vec2f cursorPosition = {cursorX, cursorY}; */
/*     Vec2f cursorSize = {cursorWidth, buffer->font->ascent + buffer->font->descent}; */

/*     // Determine cursor color */
/*     Color cursorColor = defaultColor; */
/*     if (buffer->region.active && hide_region_mode) { */
/*         cursorColor = CT.null; */
/*     } else { */
/*         cursorColor = foregroundColorAtPoint(buffer, buffer->point); */
/*     } */

/*     // Handle cursor blinking */
/*     if (blink_cursor_mode && blinkCount < blink_cursor_blinks) { */
/*         double currentTime = getTime(); */
/*         if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) { */
/*             cursorVisible = !cursorVisible; */
/*             lastBlinkTime = currentTime; */
/*             if (cursorVisible) blinkCount++; */
/*         } */
/*         if (cursorVisible) { */
/*             drawRectangle(cursorPosition, cursorSize, cursorColor); */
/*         } */
/*     } else { */
/*         drawRectangle(cursorPosition, cursorSize, cursorColor); */
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

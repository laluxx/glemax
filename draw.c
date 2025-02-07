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


// NOTE We do it on ever keypress seems right, right ? 
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

Color getScopeColor(int level) {
    Color colors[] = {
        CT.bg, // or CT.moderline
        CT.bg,
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
                    
                    if (minimap_mode) {
                        float minimap_padding = minimap_padding_mode
                            ? minimap_left_padding
                            : 0;
                        float maxWidth = activeWindow->width
                            - minimap_width
                            - fringe
                            - minimap_padding;
                        if (highlightWidth > maxWidth)
                            highlightWidth = maxWidth;
                    }
                    
                    Color scopeColor = getScopeColor(scope.level);
                    scopeColor.a = 0.1f + (float)scope.level * 0.05f;
                    
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


/* ALMOST BETTER */
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    float minimapX = mainWindow->x + mainWindow->width - minimap_width;  // Positioned on the right edge of the main window
    
    float lineHeight = 2.0;
    float scale = 1.0;
    
    useShader("simple");
    drawMinimapRegion(wm, mainWindow, buffer);
    drawMinimapView(wm, mainWindow, buffer);

    drawRectangle((Vec2f){sw-minimap_width, sw}, (Vec2f){sw-minimap_width, sw}, CT.modeline);

    float startY = mainWindow->y;
    size_t lineStart = 0;
    size_t wordStart = 0;
    int currentLine = 0;
    
    // Calculate the maximum width of text that can fit in the minimap
    float maxTextWidth = minimap_width / scale;

    // Iterate through each character in the buffer to draw syntax highlights
    for (size_t i = 0; i <= buffer->size; i++) {
        // Check for word end or line end
        if (buffer->content[i] == ' ' || buffer->content[i] == '\n' || i == buffer->size) {
            if (i > wordStart) {  // There's a word to draw
                for (size_t j = 0; j < buffer->syntaxArray.used; j++) {
                    Syntax syntax = buffer->syntaxArray.items[j];
                    if (syntax.start >= wordStart && syntax.start < i) {
                        // Calculate the start position relative to the current line
                        float relativeStart = syntax.start - lineStart;
                        // Ensure the syntax highlight doesn't extend beyond the minimap width
                        if (relativeStart < maxTextWidth) {
                            float syntaxStartX = minimapX + relativeStart * scale;
                            float syntaxWidth = fmin((syntax.end < i ? syntax.end : i) - syntax.start, maxTextWidth - relativeStart) * scale;
                            
                            drawRectangle((Vec2f){syntaxStartX, startY}, (Vec2f){syntaxWidth, lineHeight}, *syntax.color);
                        }
                    }
                }
            }
            wordStart = i + 1;  // Update start of next word
        }
        
        // Check for line end
        if (buffer->content[i] == '\n' || i == buffer->size) {
            startY -= (lineHeight + 2);  // Move up for the next line and add 2 pixel spacing (doubled from 1)
            lineStart = i + 1;  // Update the start of the next line
            wordStart = lineStart;  // Reset word start to the beginning of the next line
            currentLine++;
        }
    }

    drawMinimapCursor(wm, mainWindow, buffer);

    flush();
}

void drawHollowPoint(Buffer *buffer, Window *win, size_t position, Color color) {
    float x = win->x - win->scroll.x;  // Start position adjusted for horizontal scroll
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
    y += win->scroll.y - lineCount * (buffer->font->ascent + buffer->font->descent) - (buffer->font->descent * 2);
    
    // Calculate width
    float width = (position < buffer->size && buffer->content[position] != '\n') ?
        getCharacterWidth(buffer->font, buffer->content[position]) :
        getCharacterWidth(buffer->font, ' ');
    
    float height = buffer->font->ascent + buffer->font->descent;
    float lineThickness = fmax(1.0f, height * 0.03f);  // Scale line thickness with font size
    
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
    drawHollowPoint(buffer, win, buffer->region.mark, markColor);
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
    float cursorX = fringe + win->x - win->scroll.x;  // Start position adjusted for horizontal scroll
    float cursorY = win->y;
    int lineCount = 0;
    
    // Calculate the cursor's x offset within the content of the buffer
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;  // Increment line count at each newline
            cursorX = fringe + win->x - win->scroll.x;  // Reset cursor x to the start of the window on new lines, adjusted for horizontal scroll
        } else {
            cursorX += getCharacterWidth(buffer->font, buffer->content[i]);  // Move cursor right by character width
        }
    }
    
    // Determine cursor width, handle newline or end of buffer cases
    float cursorWidth = (buffer->point < buffer->size && buffer->content[buffer->point] != '\n') ?
        getCharacterWidth(buffer->font, buffer->content[buffer->point]) :
        getCharacterWidth(buffer->font, ' ');  // Use a standard width if at the end of a line or buffer
    
    // Compute cursor's y position based on the number of lines, subtract from initial y and add the scroll
    cursorY += win->scroll.y - lineCount * (buffer->font->ascent + buffer->font->descent) - (buffer->font->descent * 2);
    
    // Create a rectangle representing the cursor
    Vec2f cursorPosition = {cursorX, cursorY};
    Vec2f cursorSize = {cursorWidth, buffer->font->ascent + buffer->font->descent};
    
    // Determine the cursor color considering crystal_cursor_mode
    Color cursorColor = defaultColor;

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
        if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) {
            cursorVisible = !cursorVisible;
            lastBlinkTime = currentTime;
            if (cursorVisible) {
                blinkCount++;
            }
        }
        
        if (cursorVisible) {
            drawRectangle(cursorPosition, cursorSize, cursorColor);  // Draw the cursor if visible
        }
    } else {
        drawRectangle(cursorPosition, cursorSize, cursorColor);  // Always draw cursor with syntax color if not blinking or crystal_cursor_mode is true
    }
}

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



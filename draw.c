#include <font.h>
#include <stdlib.h>
#include "math.h"
#include "renderer.h"
#include "buffer.h"

size_t fringe = 8; // Width in pexels

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

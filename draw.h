#ifndef DRAW_H
#define DRAW_H

#include <lume.h>
#include "buffer.h"
#include "wm.h"


void drawMinimapCursor(WindowManager *wm, Window *mainWindow, Buffer *buffer);
void drawWordInMinimap(Buffer *buffer, float minimapX, float startY, size_t lineStart, size_t wordStart, size_t wordEnd, float maxTextWidth, float scale, float lineHeight);
void drawCommentUntilEndOfLine(Buffer *buffer, float minimapX, float startY, size_t lineStart, size_t lineEnd, float maxTextWidth, float scale, float lineHeight);
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer);

void drawHollowPoint(Buffer *buffer, Window *win, size_t position, Color color);
void drawHollowCursor(Buffer *buffer, Window *win, Color defaultColor);
void drawMark(Buffer *buffer, Window *win, Color markColor);
void drawMiniCursor(Buffer *buffer, Font *font, float x, float y, Color color);
void drawCursor(Buffer *buffer, Window *win, Color defaultColor);

/* void drawScrollbars(Window *win, Color *xcolor, Color *ycolor); */
/* void drawScrollbars(Window *win, Color *xcolor, Color *ycolor, float xThickness, float yThickness); */
void drawScrollbar(Window *win, Color *color, size_t thickness);

// SCOPES
Color getScopeColor(int level);
void fill_scopes(Buffer *buffer, Scopes *scopes);
void draw_scopes(WindowManager *wm, Font *font);

// CURSOR 
extern double lastBlinkTime;
extern bool cursorVisible;
extern int blinkCount;

// UTILITY
float lerp(float a, float b, float t);



#endif // DRAW_H

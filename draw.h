#ifndef DRAW_H
#define DRAW_H

#include <lume.h>
#include "buffer.h"
#include "wm.h"


void drawMinimapCursor(WindowManager *wm, Window *mainWindow, Buffer *buffer);
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer);
void drawHollowPoint(Buffer *buffer, Window *win, size_t position, Color color);
void drawHollowCursor(Buffer *buffer, Window *win, Color defaultColor);
void drawMark(Buffer *buffer, Window *win, Color markColor);
void drawMiniCursor(Buffer *buffer, Font *font, float x, float y, Color color);
void drawCursor(Buffer *buffer, Window *win, Color defaultColor);

extern size_t fringe; // Width in pexels

// SCOPES
Color getScopeColor(int level);
void fill_scopes(Buffer *buffer, Scopes *scopes);
void draw_scopes(WindowManager *wm, Font *font);

// CURSOR 
extern double lastBlinkTime;
extern bool cursorVisible;
extern int blinkCount;


#endif // DRAW_H

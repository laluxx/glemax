#ifndef DRAW_H
#define DRAW_H

#include <lume.h>
#include "buffer.h"
#include "wm.h"

void drawHollowPoint(Buffer *buffer, Window *win, size_t position, Color color);
void drawMark(Buffer *buffer, Window *win, Color markColor);

extern size_t fringe; // Width in pexels

#endif // DRAW_H

#ifndef CLOCK_H
#define CLOCK_H

#include <lume.h>


void drawClockDigit(int digit, Vec2f position, float squareSize, Color color);
void drawClockColon(Vec2f position, float squareSize, Color color);
void drawClock(int hours, int minutes);

#endif // CLOCK_H

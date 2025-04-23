#ifndef MODELINE_H
#define MODELINE_H

#include <stdint.h>
#include <stdlib.h>
#include "buffer.h"
#include "wm.h"


void addSegment(Segments *segments, const char *name, const char *content);
void initSegments(Segments *segments);
void updateSegmentCodepoints(Segment *segment);
void updateSegments(Modeline *modeline, Buffer *buffer);
void drawModelines(WindowManager *wm, Font *font, float minibufferHeight, Color color);


#endif

// Generated from rgb.txt - DO NOT EDIT
#ifndef X11_COLORS_H
#define X11_COLORS_H

typedef struct {
    const char *name;
    float r, g, b;
} X11Color;

const X11Color *lookup_x11_color(const char *name);

#endif // X11_COLORS_H

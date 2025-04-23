#ifndef FACES_H
#define FACES_H

#include "font.h"
#include "buffer.h"

extern Font *font;
extern int fontsize;
extern char *fontPath;

extern Font *commentfont;
extern char *commentfontPath;
extern int  commentfontsize;


extern Font *minifont;

extern int fontincrement; // NOTE unused
extern int minifontsize;


extern NerdFont* nerdFont;
extern int nerdFontSize;
extern char* nerdFontPath;


#define MAX_FONT_SCALE 27
#define MIN_FONT_SCALE -8
#define MAX_FONT_SCALE_INDEX 36
#define SCALE_ZERO_INDEX 8

extern Font *globalFontCache[MAX_FONT_SCALE_INDEX]; // NOTE Global cache of rasterized fonts

void initScale(Scale *scale);
Font* updateFont(Scale *scale, int newIndex, char *fontPath);
void text_scale_decrease(BufferManager *bm, char *fontPath, WindowManager *wm, int sh, int arg);
void text_scale_increase(BufferManager *bm, char *fontPath, WindowManager *wm, int sh, int arg);
void text_scale_increase_by_buffer(BufferManager *bm, char *bufferName, char *fontPath, WindowManager *wm, int sh, int arg);
void text_scale_decrease_by_buffer(BufferManager *bm, char *bufferName, char *fontPath, WindowManager *wm, int sh, int arg);

#endif

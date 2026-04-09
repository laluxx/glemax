#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <libguile.h>

/// Alignment

typedef enum {
    ALIGN_BITMAP_CENTER = 0,
    ALIGN_BITMAP_TOP,
    ALIGN_BITMAP_BOTTOM,
} FringeAlign;


/// Bitmap

// One row = one uint8_t, 8 pixels, MSB is leftmost pixel.
// Matches Emacs fringe_bitmap layout exactly.
// period > 0 means tile: repeat the bitmap every period rows to fill
// the line height (used by empty-line which has period=3).
typedef struct FringeBitmap {
    char        *name;
    uint8_t     *bits;    // height bytes, one per row
    int          height;
    int          width;   // pixel columns used (always 8 for standard bitmaps)
    int          period;  // 0 = no tiling; N = tile every N rows
    FringeAlign  align;
    int          face_id; // -1 = FACE_FRINGE
} FringeBitmap;


/// Indicators

typedef enum {
    FRINGE_IND_NONE = 0,
    FRINGE_IND_CONTINUATION,
    FRINGE_IND_TRUNCATION,
    FRINGE_IND_EMPTY_LINE,
    FRINGE_IND_OVERLAY_ARROW,
    FRINGE_IND_TOP,
    FRINGE_IND_BOTTOM,
    FRINGE_IND_UP,
    FRINGE_IND_DOWN,
} FringeIndicatorKind;

typedef struct {
    FringeBitmap *left;
    FringeBitmap *right;
} FringeIndicatorPair;


/// Forward declarations

struct Buffer;
struct Window;


/// Registry lifecycle

void fringe_init(void);
void fringe_shutdown(void);


/// Bitmap registry

FringeBitmap *fringe_define_bitmap(const char *name,
                                   const uint8_t *bits, int height, int width,
                                   int period, FringeAlign align, int face_id);
FringeBitmap *fringe_lookup_bitmap(SCM name_sym);
void          fringe_destroy_bitmap(SCM name_sym);
void          fringe_set_bitmap_face(SCM name_sym, int face_id);


/// Indicator resolution

FringeIndicatorPair fringe_resolve_indicator(struct Buffer *buf,
                                             FringeIndicatorKind kind);


/// Rendering

// Draw one bitmap into the left or right fringe of win.
// line_y is the draw_y value (baseline) passed by draw_buffer.
// line_height is current_line_height at that point.
void fringe_draw_bitmap(FringeBitmap *bm, bool is_left,
                        struct Window *win, float line_y, float line_height);

void fringe_draw_pair(FringeIndicatorPair pair, bool has_left, bool has_right,
                      struct Window *win, float line_y, float line_height);


/// Scheme bindings

void init_fringe_bindings(void);

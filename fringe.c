#include "fringe.h"
#include "buffer.h"
#include "wm.h"
#include "faces.h"
#include "frame.h"
#include <libguile.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


/// Internal registry

#define FRINGE_REGISTRY_SIZE 64

typedef struct BitmapEntry {
    struct BitmapEntry *next;
    FringeBitmap        bm;
} BitmapEntry;

static BitmapEntry *g_registry[FRINGE_REGISTRY_SIZE];

static unsigned int registry_hash(const char *s) {
    unsigned int h = 5381;
    for (const unsigned char *p = (const unsigned char *)s; *p; p++)
        h = h * 33 ^ *p;
    return h % FRINGE_REGISTRY_SIZE;
}

static BitmapEntry *registry_find_by_name(const char *name) {
    BitmapEntry *e = g_registry[registry_hash(name)];
    while (e) {
        if (strcmp(e->bm.name, name) == 0) return e;
        e = e->next;
    }
    return NULL;
}


/// Cached indicator symbols

static struct {
    SCM fringe_indicator_alist;
    SCM continuation;
    SCM truncation;
    SCM empty_line;
    SCM overlay_arrow;
    SCM top;
    SCM bottom;
    SCM up;
    SCM down;
} g_sym;

static void intern(SCM *p, const char *name) {
    *p = scm_from_utf8_symbol(name);
    scm_gc_protect_object(*p);
}


/// Fringe bitmaps
//
// Each row is one uint8_t, 8 pixels wide, MSB = leftmost pixel.

/* Undefined bitmap.  A question mark.  */
/*
  ..xxxx..
  .xxxxxx.
  xx....xx
  xx....xx
  ....xx..
  ...xx...
  ...xx...
  ........
  ...xx...
  ...xx...
*/
static uint8_t question_mark_bits[] = {
  0x3c, 0x7e, 0xc3, 0xc3, 0x0c, 0x18, 0x18, 0x00, 0x18, 0x18};

/* An exclamation mark.  */
/*
  ...XX...
  ...XX...
  ...XX...
  ...XX...
  ...XX...
  ...XX...
  ...XX...
  ........
  ...XX...
  ...XX...
*/
static uint8_t exclamation_mark_bits[] = {
  0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 0x18, 0x18};

/* An arrow like this: <-.  */
/*
  ...xx...
  ..xx....
  .xx.....
  xxxxxx..
  xxxxxx..
  .xx.....
  ..xx....
  ...xx...
*/
static uint8_t left_arrow_bits[] = {
   0x18, 0x30, 0x60, 0xfc, 0xfc, 0x60, 0x30, 0x18};


/* Right truncation arrow bitmap ->.  */
/*
  ...xx...
  ....xx..
  .....xx.
  ..xxxxxx
  ..xxxxxx
  .....xx.
  ....xx..
  ...xx...
*/
static uint8_t right_arrow_bits[] = {
   0x18, 0x0c, 0x06, 0x3f, 0x3f, 0x06, 0x0c, 0x18};


/* Up arrow bitmap.  */
/*
  ...xx...
  ..xxxx..
  .xxxxxx.
  xxxxxxxx
  ...xx...
  ...xx...
  ...xx...
  ...xx...
*/
static uint8_t up_arrow_bits[] = {
   0x18, 0x3c, 0x7e, 0xff, 0x18, 0x18, 0x18, 0x18};


/* Down arrow bitmap.  */
/*
  ...xx...
  ...xx...
  ...xx...
  ...xx...
  xxxxxxxx
  .xxxxxx.
  ..xxxx..
  ...xx...
*/
static uint8_t down_arrow_bits[] = {
   0x18, 0x18, 0x18, 0x18, 0xff, 0x7e, 0x3c, 0x18};

/* Marker for continuation lines.  */
/*
  ..xxxx..
  .xxxxx..
  xx......
  xxx..x..
  xxxxxx..
  .xxxxx..
  ..xxxx..
  .xxxxx..
*/
static uint8_t left_curly_arrow_bits[] = {
   0x3c, 0x7c, 0xc0, 0xe4, 0xfc, 0x7c, 0x3c, 0x7c};

/* Marker for continued lines.  */
/*
  ..xxxx..
  ..xxxxx.
  ......xx
  ..x..xxx
  ..xxxxxx
  ..xxxxx.
  ..xxxx..
  ..xxxxx.
*/
static uint8_t right_curly_arrow_bits[] = {
   0x3c, 0x3e, 0x03, 0x27, 0x3f, 0x3e, 0x3c, 0x3e};

/* Large circle bitmap.  */
/*
  ..xxxx..
  .xxxxxx.
  xxxxxxxx
  xxxxxxxx
  xxxxxxxx
  xxxxxxxx
  .xxxxxx.
  ..xxxx..
*/
static uint8_t large_circle_bits[] = {
  0x3c, 0x7e, 0xff, 0xff, 0xff, 0xff, 0x7e, 0x3c};

/* Reverse Overlay arrow bitmap.  A triangular arrow.  */
/*
  ......xx
  ....xxxx
  ...xxxxx
  ..xxxxxx
  ..xxxxxx
  ...xxxxx
  ....xxxx
  ......xx
*/
static uint8_t left_triangle_bits[] = {
   0x03, 0x0f, 0x1f, 0x3f, 0x3f, 0x1f, 0x0f, 0x03};

/* Overlay arrow bitmap.  A triangular arrow.  */
/*
  xx......
  xxxx....
  xxxxx...
  xxxxxx..
  xxxxxx..
  xxxxx...
  xxxx....
  xx......
*/
static uint8_t right_triangle_bits[] = {
   0xc0, 0xf0, 0xf8, 0xfc, 0xfc, 0xf8, 0xf0, 0xc0};

/* First line bitmap.  An top-left angle.  */
/*
  xxxxxx..
  xxxxxx..
  xx......
  xx......
  xx......
  xx......
  xx......
  ........
*/
static uint8_t top_left_angle_bits[] = {
   0xfc, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0x00};

/* First line bitmap.  An right-up angle.  */
/*
  ..xxxxxx
  ..xxxxxx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ........
*/
static uint8_t top_right_angle_bits[] = {
   0x3f, 0x3f, 0x03, 0x03, 0x03, 0x03, 0x03, 0x00};

/* Last line bitmap.  An left-down angle.  */
/*
  ........
  xx......
  xx......
  xx......
  xx......
  xx......
  xxxxxx..
  xxxxxx..
*/
static uint8_t bottom_left_angle_bits[] = {
   0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xfc};

/* Last line bitmap.  An right-down angle.  */
/*
  ........
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ..xxxxxx
  ..xxxxxx
*/
static uint8_t bottom_right_angle_bits[] = {
   0x00, 0x03, 0x03, 0x03, 0x03, 0x03, 0x3f, 0x3f};

/* First/last line bitmap.  An left bracket.  */
/*
  xxxxxx..
  xxxxxx..
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xxxxxx..
  xxxxxx..
*/
static uint8_t left_bracket_bits[] = {
   0xfc, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xfc};

/* First/last line bitmap.  An right bracket.  */
/*
  ..xxxxxx
  ..xxxxxx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ......xx
  ..xxxxxx
  ..xxxxxx
*/
static uint8_t right_bracket_bits[] = {
  0x3f, 0x3f, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x3f, 0x3f};

/* Filled box cursor bitmap.  A filled box; max 13 pixels high.  */
/*
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
  xxxxxxx.
*/
static uint8_t filled_rectangle_bits[] = {
   0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe};

/* Hollow box cursor bitmap.  A hollow box; max 13 pixels high.  */
/*
  xxxxxxx.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  x.....x.
  xxxxxxx.
*/
static uint8_t hollow_rectangle_bits[] = {
   0xfe, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0x82, 0xfe};

/* Hollow square bitmap.  */
/*
  .xxxxxx.
  .x....x.
  .x....x.
  .x....x.
  .x....x.
  .xxxxxx.
*/
static uint8_t hollow_square_bits[] = {
   0x7e, 0x42, 0x42, 0x42, 0x42, 0x7e};

/* Filled square bitmap.  */
/*
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
  .xxxxxx.
*/
static uint8_t filled_square_bits[] = {
   0x7e, 0x7e, 0x7e, 0x7e, 0x7e, 0x7e};

/* Bar cursor bitmap.  A vertical bar; max 13 pixels high.  */
/*
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
  xx......
*/
static uint8_t vertical_bar_bits[] = {
   0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0};

/* HBar cursor bitmap.  A horizontal bar; 2 pixels high.  */
/*
  xxxxxxx.
  xxxxxxx.
*/
static uint8_t horizontal_bar_bits[] = {
  0xfe, 0xfe};

// period=3: repeats every 3 rows to fill the line height
static const uint8_t empty_line_bits[] = {
    0x00, 0x3c, 0x00};

static void register_standard_bitmaps(void) {
#define REG(nm, bits, period, align) \
    fringe_define_bitmap(nm, bits, sizeof(bits), 8, period, align, -1)

    REG("question-mark",        question_mark_bits,      0, ALIGN_BITMAP_CENTER);
    REG("exclamation-mark",     exclamation_mark_bits,   0, ALIGN_BITMAP_CENTER);
    REG("left-arrow",           left_arrow_bits,         0, ALIGN_BITMAP_CENTER);
    REG("right-arrow",          right_arrow_bits,        0, ALIGN_BITMAP_CENTER);
    REG("up-arrow",             up_arrow_bits,           0, ALIGN_BITMAP_TOP);
    REG("down-arrow",           down_arrow_bits,         0, ALIGN_BITMAP_BOTTOM);
    REG("left-curly-arrow",     left_curly_arrow_bits,   0, ALIGN_BITMAP_CENTER);
    REG("right-curly-arrow",    right_curly_arrow_bits,  0, ALIGN_BITMAP_CENTER);
    REG("large-circle",         large_circle_bits,       0, ALIGN_BITMAP_CENTER);
    REG("left-triangle",        left_triangle_bits,      0, ALIGN_BITMAP_CENTER);
    REG("right-triangle",       right_triangle_bits,     0, ALIGN_BITMAP_CENTER);
    REG("top-left-angle",       top_left_angle_bits,     0, ALIGN_BITMAP_TOP);
    REG("top-right-angle",      top_right_angle_bits,    0, ALIGN_BITMAP_TOP);
    REG("bottom-left-angle",    bottom_left_angle_bits,  0, ALIGN_BITMAP_BOTTOM);
    REG("bottom-right-angle",   bottom_right_angle_bits, 0, ALIGN_BITMAP_BOTTOM);
    REG("left-bracket",         left_bracket_bits,       0, ALIGN_BITMAP_CENTER);
    REG("right-bracket",        right_bracket_bits,      0, ALIGN_BITMAP_CENTER);
    REG("filled-rectangle",     filled_rectangle_bits,   0, ALIGN_BITMAP_CENTER);
    REG("hollow-rectangle",     hollow_rectangle_bits,   0, ALIGN_BITMAP_CENTER);
    REG("filled-square",        filled_square_bits,      0, ALIGN_BITMAP_CENTER);
    REG("hollow-square",        hollow_square_bits,      0, ALIGN_BITMAP_CENTER);
    REG("vertical-bar",         vertical_bar_bits,       0, ALIGN_BITMAP_CENTER);
    REG("horizontal-bar",       horizontal_bar_bits,     0, ALIGN_BITMAP_BOTTOM);
    REG("empty-line",           empty_line_bits,         3, ALIGN_BITMAP_TOP);
#undef REG
}


/// Registry lifecycle

void fringe_init(void) {
    memset(g_registry, 0, sizeof(g_registry));

    intern(&g_sym.fringe_indicator_alist, "fringe-indicator-alist");
    intern(&g_sym.continuation,  "continuation");
    intern(&g_sym.truncation,    "truncation");
    intern(&g_sym.empty_line,    "empty-line");
    intern(&g_sym.overlay_arrow, "overlay-arrow");
    intern(&g_sym.top,           "top");
    intern(&g_sym.bottom,        "bottom");
    intern(&g_sym.up,            "up");
    intern(&g_sym.down,          "down");

    register_standard_bitmaps();
}

void fringe_shutdown(void) {
    for (int i = 0; i < FRINGE_REGISTRY_SIZE; i++) {
        BitmapEntry *e = g_registry[i];
        while (e) {
            BitmapEntry *next = e->next;
            free(e->bm.name);
            free(e->bm.bits);
            free(e);
            e = next;
        }
        g_registry[i] = NULL;
    }
}


/// Bitmap registry

FringeBitmap *fringe_define_bitmap(const char *name,
                                   const uint8_t *bits, int height, int width,
                                   int period, FringeAlign align, int face_id) {
    BitmapEntry *e = registry_find_by_name(name);
    if (!e) {
        e = calloc(1, sizeof(BitmapEntry));
        if (!e) return NULL;
        e->bm.name = strdup(name);
        unsigned int slot = registry_hash(name);
        e->next      = g_registry[slot];
        g_registry[slot] = e;
    } else {
        free(e->bm.bits);
        e->bm.bits = NULL;
    }

    uint8_t *copy = malloc(height);
    if (!copy) return NULL;
    memcpy(copy, bits, height);

    e->bm.bits    = copy;
    e->bm.height  = height;
    e->bm.width   = (width < 1) ? 1 : (width > 8) ? 8 : width;
    e->bm.period  = period;
    e->bm.align   = align;
    e->bm.face_id = face_id;
    return &e->bm;
}

FringeBitmap *fringe_lookup_bitmap(SCM name_sym) {
    if (!scm_is_symbol(name_sym)) return NULL;
    char *s = scm_to_utf8_string(scm_symbol_to_string(name_sym));
    BitmapEntry *e = registry_find_by_name(s);
    free(s);
    return e ? &e->bm : NULL;
}

void fringe_destroy_bitmap(SCM name_sym) {
    if (!scm_is_symbol(name_sym)) return;
    char *s = scm_to_utf8_string(scm_symbol_to_string(name_sym));
    unsigned int slot = registry_hash(s);
    BitmapEntry **pp = &g_registry[slot];
    while (*pp) {
        if (strcmp((*pp)->bm.name, s) == 0) {
            BitmapEntry *dead = *pp;
            *pp = dead->next;
            free(dead->bm.name);
            free(dead->bm.bits);
            free(dead);
            break;
        }
        pp = &(*pp)->next;
    }
    free(s);
}

void fringe_set_bitmap_face(SCM name_sym, int face_id) {
    if (!scm_is_symbol(name_sym)) return;
    char *s = scm_to_utf8_string(scm_symbol_to_string(name_sym));
    BitmapEntry *e = registry_find_by_name(s);
    free(s);
    if (e) e->bm.face_id = face_id;
}


/// Indicator resolution

static SCM kind_to_sym(FringeIndicatorKind kind) {
    switch (kind) {
        case FRINGE_IND_CONTINUATION:  return g_sym.continuation;
        case FRINGE_IND_TRUNCATION:    return g_sym.truncation;
        case FRINGE_IND_EMPTY_LINE:    return g_sym.empty_line;
        case FRINGE_IND_OVERLAY_ARROW: return g_sym.overlay_arrow;
        case FRINGE_IND_TOP:           return g_sym.top;
        case FRINGE_IND_BOTTOM:        return g_sym.bottom;
        case FRINGE_IND_UP:            return g_sym.up;
        case FRINGE_IND_DOWN:          return g_sym.down;
        default:                       return SCM_BOOL_F;
    }
}

static FringeBitmap *resolve_side(SCM entry, bool want_right) {
    if (!scm_is_pair(entry)) {
        // bare symbol: only applies to left
        if (want_right) return NULL;
        return scm_is_symbol(entry) ? fringe_lookup_bitmap(entry) : NULL;
    }
    SCM sym = want_right ? scm_cadr(entry) : scm_car(entry);
    if (!scm_is_symbol(sym)) return NULL;
    return fringe_lookup_bitmap(sym);
}

FringeIndicatorPair fringe_resolve_indicator(Buffer *buf,
                                             FringeIndicatorKind kind) {
    FringeIndicatorPair pair = {NULL, NULL};
    if (kind == FRINGE_IND_NONE) return pair;

    SCM key = kind_to_sym(kind);
    if (scm_is_false(key)) return pair;

    SCM alist = buffer_local_value(g_sym.fringe_indicator_alist, buf);
    if (!scm_is_pair(alist)) {
        SCM var = scm_c_lookup("fringe-indicator-alist");
        alist = scm_is_true(scm_variable_bound_p(var))
                ? scm_variable_ref(var) : SCM_EOL;
    }

    SCM cell = scm_assq(key, alist);
    if (scm_is_false(cell)) return pair;

    SCM entry  = scm_cdr(cell);
    pair.left  = resolve_side(entry, false);
    pair.right = resolve_side(entry, true);
    return pair;
}


/// Rendering

void fringe_draw_bitmap(FringeBitmap *bm, bool is_left,
                        Window *win, float line_y, float line_height) {
    if (!bm || !bm->bits || !win || win->is_minibuffer) return;

    float fringe_w = is_left ? selected_frame->left_fringe_width
                             : selected_frame->right_fringe_width;
    if (fringe_w <= 0.f) return;

    int   fid   = (bm->face_id >= 0) ? bm->face_id : FACE_FRINGE;
    Face *face  = get_face(fid);
    Color color = face ? face->fg : (Color){1.f, 1.f, 1.f, 1.f};

    float fringe_x = is_left ? win->x
                             : win->x + win->width - fringe_w;

    // 1 screen pixel per bitmap pixel.
    float px_w = 1.f;
    float px_h = 1.f;

    float slot_bottom = line_y;
    float slot_top    = line_y + line_height;
    float slot_h      = line_height;

    float bm_screen_h = (float)(bm->period > 0 ? bm->period : bm->height);
    float bm_screen_w = (float)bm->width;

    // Center the bitmap horizontally in the fringe.
    float horiz_off = floorf((fringe_w - bm_screen_w) * 0.5f);

    Face *default_face = get_face(FACE_DEFAULT);
    Font *default_font = default_face ? get_face_font(default_face) : NULL;
    float descent = default_font ? default_font->descent : 0.f;

    // Vertical offset based on alignment.
    float vert_off;
    switch (bm->align) {
        case ALIGN_BITMAP_TOP:    vert_off = 0.f;                           break;
        case ALIGN_BITMAP_BOTTOM: vert_off = slot_h - bm_screen_h;         break;
        default:                  vert_off = floorf((slot_h - bm_screen_h) * 0.5f); break;
    }

    // how many rows to draw (tiling if period > 0)
    int draw_rows = bm->period > 0
                    ? (int)ceilf(slot_h / px_h)
                    : bm->height;

    for (int row = 0; row < draw_rows; row++) {
        int src_row = bm->period > 0 ? (row % bm->period) : row;
        if (src_row >= bm->height) break;

        uint8_t mask = bm->bits[src_row];
        if (!mask) continue;

        // row 0 is at the top of the bitmap slot, shifted down by ascent
        float row_y = slot_top - vert_off - (float)(row + 1) - (descent/2) * 2;

        for (int bit = 0; bit < bm->width; bit++) {
            if (!(mask & (0x80u >> bit))) continue;
            float px_x = fringe_x + horiz_off + (float)bit;
            quad2D((vec2){px_x, row_y}, (vec2){1.f, 1.f}, color);
        }
    }
}

void fringe_draw_pair(FringeIndicatorPair pair, bool has_left, bool has_right,
                      Window *win, float line_y, float line_height) {
    if (has_left  && pair.left)
        fringe_draw_bitmap(pair.left,  true,  win, line_y, line_height);
    if (has_right && pair.right)
        fringe_draw_bitmap(pair.right, false, win, line_y, line_height);
}


/// Scheme bindings

static SCM scm_define_fringe_bitmap(SCM sym, SCM bits,
                                    SCM height_s, SCM width_s, SCM align_s) {
    SCM_ASSERT_TYPE(scm_is_symbol(sym),                        sym,  SCM_ARG1, "define-fringe-bitmap", "symbol");
    SCM_ASSERT_TYPE(scm_is_vector(bits) || scm_is_string(bits), bits, SCM_ARG2, "define-fringe-bitmap", "vector or string");

    int height = scm_is_integer(height_s) ? scm_to_int(height_s)
               : scm_is_vector(bits)      ? (int)scm_c_vector_length(bits)
               :                            (int)scm_c_string_length(bits);
    int width  = scm_is_integer(width_s) ? scm_to_int(width_s) : 8;

    uint8_t *rows = calloc(height, 1);
    if (!rows) return SCM_BOOL_F;
    for (int i = 0; i < height; i++) {
        SCM v = scm_is_vector(bits) ? scm_c_vector_ref(bits, i)
                                    : scm_from_uint32((uint32_t)scm_c_string_ref(bits, i));
        rows[i] = scm_is_integer(v) ? (uint8_t)scm_to_uint32(v) : 0;
    }

    int         period   = 0;
    FringeAlign align    = ALIGN_BITMAP_CENTER;
    if (scm_is_pair(align_s)) {
        SCM p = scm_cdr(align_s);
        if (scm_is_integer(p)) period = scm_to_int(p);
        align_s = scm_car(align_s);
    }
    if (scm_is_symbol(align_s)) {
        if (scm_is_eq(align_s, scm_from_utf8_symbol("top")))    align = ALIGN_BITMAP_TOP;
        if (scm_is_eq(align_s, scm_from_utf8_symbol("bottom"))) align = ALIGN_BITMAP_BOTTOM;
    }

    char *name = scm_to_utf8_string(scm_symbol_to_string(sym));
    FringeBitmap *bm = fringe_define_bitmap(name, rows, height, width, period, align, -1);
    free(name);
    free(rows);
    return bm ? sym : SCM_BOOL_F;
}

static SCM scm_destroy_fringe_bitmap(SCM sym) {
    SCM_ASSERT_TYPE(scm_is_symbol(sym), sym, SCM_ARG1, "destroy-fringe-bitmap", "symbol");
    fringe_destroy_bitmap(sym);
    return SCM_UNSPECIFIED;
}

static SCM scm_set_fringe_bitmap_face(SCM sym, SCM face_s) {
    SCM_ASSERT_TYPE(scm_is_symbol(sym), sym, SCM_ARG1, "set-fringe-bitmap-face", "symbol");
    int face_id = -1;
    if (scm_is_symbol(face_s) || scm_is_string(face_s)) {
        char *s  = scm_to_utf8_string(scm_is_symbol(face_s)
                                      ? scm_symbol_to_string(face_s) : face_s);
        Face *f  = get_named_face(s);
        if (f) face_id = f->id;
        free(s);
    }
    fringe_set_bitmap_face(sym, face_id);
    return SCM_UNSPECIFIED;
}

void init_fringe_bindings(void) {
    scm_c_define_gsubr("define-fringe-bitmap",   2, 3, 0, scm_define_fringe_bitmap);
    scm_c_define_gsubr("destroy-fringe-bitmap",  1, 0, 0, scm_destroy_fringe_bitmap);
    scm_c_define_gsubr("set-fringe-bitmap-face", 1, 1, 0, scm_set_fringe_bitmap_face);
}

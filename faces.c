#include "faces.h"
#include "buffer.h"
#include "x11_colors.h"
#include <fontconfig/fontconfig.h>

// TODO Make it dynamic without a limit in size.
#define DYNFACE_INIT_CAP 64

typedef struct {
    char *name;
    int   id;
} DynFaceEntry;

static DynFaceEntry *dynface_table = NULL;
static size_t        dynface_cap   = 0;
static size_t        dynface_count = 0;

static uint32_t dynface_hash(const char *s) {
    uint32_t h = 2166136261u;
    while (*s) { h ^= (uint8_t)*s++; h *= 16777619u; }
    return h;
}

static int dynface_lookup(const char *name) {
    if (!dynface_table || !dynface_cap) return -1;
    uint32_t h = dynface_hash(name);
    size_t   i = h & (dynface_cap - 1);
    for (size_t probe = 0; probe < dynface_cap; probe++) {
        DynFaceEntry *e = &dynface_table[i];
        if (!e->name)                   return -1;
        if (strcmp(e->name, name) == 0) return e->id;
        i = (i + 1) & (dynface_cap - 1);
    }
    return -1;
}

static void dynface_insert_raw(DynFaceEntry *table, size_t cap,
                                char *name, int id) {
    uint32_t h = dynface_hash(name);
    size_t   i = h & (cap - 1);
    while (table[i].name) i = (i + 1) & (cap - 1);
    table[i].name = name;
    table[i].id   = id;
}

static bool dynface_grow(void) {
    size_t new_cap = dynface_cap ? dynface_cap * 2 : DYNFACE_INIT_CAP;
    DynFaceEntry *t = calloc(new_cap, sizeof *t);
    if (!t) return false;
    for (size_t i = 0; i < dynface_cap; i++)
        if (dynface_table[i].name)
            dynface_insert_raw(t, new_cap, dynface_table[i].name, dynface_table[i].id);
    free(dynface_table);
    dynface_table = t;
    dynface_cap   = new_cap;
    return true;
}

static bool dynface_register(const char *name, int id) {
    if (dynface_count + 1 > dynface_cap * 7 / 10)
        if (!dynface_grow()) return false;
    char *key = strdup(name);
    if (!key) return false;
    dynface_insert_raw(dynface_table, dynface_cap, key, id);
    dynface_count++;
    return true;
}

void dynface_free_all(void) {
    for (size_t i = 0; i < dynface_cap; i++)
        free(dynface_table[i].name);
    free(dynface_table);
    dynface_table = NULL;
    dynface_cap = dynface_count = 0;
}

/// Font registry

static FontRegistryEntry *font_registry_head = NULL;

// Global default font spec — may be updated by (set-face-attribute 'default …)
char *g_default_family     = NULL;   // set during init_faces
int   g_default_pixel_size = 22;     // pixel size, not 1/10 pt

// Convert Emacs :height (1/10 pt) → pixel size using a fixed 96 DPI assumption.
// Override this function if you have access to the real frame DPI.
int height_to_pixel_size(int height_10pt) {
    if (height_10pt <= 0) return g_default_pixel_size;
    // height_10pt is in units of 1/10 point.
    // pixels = (pt / 72) * dpi  →  ((height_10pt/10) / 72) * 96
    float pt  = height_10pt / 10.0f;
    float px  = (pt / 72.0f) * 96.0f;
    int   pxi = (int)(px + 0.5f);
    return pxi > 0 ? pxi : 1;
}

// Look up an already-loaded font; NULL if not found.
static Font *font_registry_find(const char *family, int pixel_size,
                                 bool bold, bool italic) {
    for (FontRegistryEntry *e = font_registry_head; e; e = e->next) {
        if (e->pixel_size == pixel_size &&
            e->bold == bold && e->italic == italic &&
            strcmp(e->family, family) == 0)
            return e->font;
    }
    return NULL;
}

// Look up or load a font. Returns NULL only if fontconfig/FreeType fails.
Font *font_registry_get(const char *family, int pixel_size,
                         bool bold, bool italic) {

    if (!family) {
        fprintf(stderr, "font_registry_get: family is NULL\n");
        return NULL;
    }

    Font *existing = font_registry_find(family, pixel_size, bold, italic);
    if (existing) return existing;

    Font *font = fontconfig_load_font(family, pixel_size, bold, italic);
    if (!font) {
        // Fallback: try without bold/italic
        if (bold || italic) {
            font = fontconfig_load_font(family, pixel_size, false, false);
        }
        if (!font) return NULL;
    }

    FontRegistryEntry *entry = calloc(1, sizeof *entry);
    if (!entry) { destroy_font(font); return NULL; }
    entry->family     = strdup(family);
    entry->pixel_size = pixel_size;
    entry->bold       = bold;
    entry->italic     = italic;
    entry->font       = font;
    entry->next       = font_registry_head;
    font_registry_head = entry;
    return font;
}

void font_registry_free_all(void) {
    FontRegistryEntry *e = font_registry_head;
    while (e) {
        FontRegistryEntry *next = e->next;
        destroy_font(e->font);
        free(e->family);
        free(e);
        e = next;
    }
    font_registry_head = NULL;
}

// ─── helpers ──────────────────────────────────────────────────────────────────

// Resolve the family/pixel_size that a face should use, falling back to the
// global defaults when the face has no explicit spec.
static void face_font_spec(const Face *face,
                            const char **out_family,
                            int         *out_pixel_size) {
    *out_family     = (face->family && face->family[0])
                      ? face->family : g_default_family;
    *out_pixel_size = (face->height > 0)
                      ? height_to_pixel_size(face->height)
                      : g_default_pixel_size;
}

/// API

Font *get_face_font(Face *face) {
    if (!face) {
        if (!g_default_family) return NULL;  // init_faces not called yet
        return font_registry_get(g_default_family, g_default_pixel_size,
                                  false, false);
    }

    // If the face has a pre-resolved font pointer, honour it.
    // (This is set by init_faces for the built-in bold/italic faces.)
    if (face->font) return face->font;

    const char *family;
    int         pixel_size;
    face_font_spec(face, &family, &pixel_size);

    Font *f = font_registry_get(family, pixel_size, face->bold, face->italic);
    if (!f) {
        // Last resort: bare default
        f = font_registry_get(g_default_family, g_default_pixel_size,
                               false, false);
    }
    return f;
}

Font *fontconfig_load_font(const char *family, int size, bool bold, bool italic) {
    FcPattern *pattern = FcPatternCreate();
    FcPatternAddString (pattern, FC_FAMILY, (const FcChar8*)family);
    FcPatternAddDouble (pattern, FC_SIZE,   (double)size);
    FcPatternAddInteger(pattern, FC_WEIGHT, bold   ? FC_WEIGHT_BOLD   : FC_WEIGHT_REGULAR);
    FcPatternAddInteger(pattern, FC_SLANT,  italic ? FC_SLANT_ITALIC  : FC_SLANT_ROMAN);

    FcConfigSubstitute(NULL, pattern, FcMatchPattern);
    FcDefaultSubstitute(pattern);

    FcResult   result;
    FcPattern *match = FcFontMatch(NULL, pattern, &result);
    FcPatternDestroy(pattern);

    if (!match) {
        fprintf(stderr, "fontconfig: no match for '%s'\n", family);
        return NULL;
    }

    FcChar8 *file = NULL;
    if (FcPatternGetString(match, FC_FILE, 0, &file) != FcResultMatch) {
        FcPatternDestroy(match);
        return NULL;
    }

    Font *font = load_font((const char*)file, size);
    FcPatternDestroy(match);
    return font;
}

// Face name table (built-in names only)
static struct { const char *name; int id; } face_names[] = {
    {"default",                             FACE_DEFAULT},
    {"mode-line",                           FACE_MODE_LINE},
    {"mode-line-active",                    FACE_MODE_LINE_ACTIVE},
    {"mode-line-inactive",                  FACE_MODE_LINE_INACTIVE},
    {"window-divider",                      FACE_WINDOW_DIVIDER},
    {"fringe",                              FACE_FRINGE},
    {"cursor",                              FACE_CURSOR},
    {"bold",                                FACE_BOLD},
    {"italic",                              FACE_ITALIC},
    {"bold-italic",                         FACE_BOLD_ITALIC},
    {"visible-mark",                        FACE_VISIBLE_MARK},
    {"error",                               FACE_ERROR},
    {"success",                             FACE_SUCCESS},
    {"warning",                             FACE_WARNING},
    {"font-lock-bracket-face",              FACE_FONT_LOCK_BRACKET},
    {"font-lock-builtin-face",              FACE_FONT_LOCK_BUILTIN},
    {"font-lock-comment-delimiter-face",    FACE_FONT_LOCK_COMMENT_DELIMITER},
    {"font-lock-comment-face",              FACE_FONT_LOCK_COMMENT},
    {"font-lock-constant-face",             FACE_FONT_LOCK_CONSTANT},
    {"font-lock-delimiter-face",            FACE_FONT_LOCK_DELIMITER},
    {"font-lock-doc-face",                  FACE_FONT_LOCK_DOC},
    {"font-lock-doc-markup-face",           FACE_FONT_LOCK_DOC_MARKUP},
    {"font-lock-escape-face",               FACE_FONT_LOCK_ESCAPE},
    {"font-lock-function-call-face",        FACE_FONT_LOCK_FUNCTION_CALL},
    {"font-lock-function-name-face",        FACE_FONT_LOCK_FUNCTION_NAME},
    {"font-lock-keyword-face",              FACE_FONT_LOCK_KEYWORD},
    {"font-lock-misc-punctuation-face",     FACE_FONT_LOCK_MISC_PUNCTUATION},
    {"font-lock-negation-char-face",        FACE_FONT_LOCK_NEGATION_CHAR},
    {"font-lock-number-face",               FACE_FONT_LOCK_NUMBER},
    {"font-lock-operator-face",             FACE_FONT_LOCK_OPERATOR},
    {"font-lock-preprocessor-face",         FACE_FONT_LOCK_PREPROCESSOR},
    {"font-lock-property-name-face",        FACE_FONT_LOCK_PROPERTY_NAME},
    {"font-lock-property-use-face",         FACE_FONT_LOCK_PROPERTY_USE},
    {"font-lock-punctuation-face",          FACE_FONT_LOCK_PUNCTUATION},
    {"font-lock-regexp-face",               FACE_FONT_LOCK_REGEXP},
    {"font-lock-regexp-grouping-backslash", FACE_FONT_LOCK_REGEXP_GROUPING_BACKSLASH},
    {"font-lock-regexp-grouping-construct", FACE_FONT_LOCK_REGEXP_GROUPING_CONSTRUCT},
    {"font-lock-string-face",               FACE_FONT_LOCK_STRING},
    {"font-lock-type-face",                 FACE_FONT_LOCK_TYPE},
    {"font-lock-variable-name-face",        FACE_FONT_LOCK_VARIABLE_NAME},
    {"font-lock-variable-use-face",         FACE_FONT_LOCK_VARIABLE_USE},
    {"font-lock-warning-face",              FACE_FONT_LOCK_WARNING},
    {"minibuffer-prompt",                   FACE_MINIBUFFER_PROMPT},
    {"underline",                           FACE_UNDERLINE},
    {"strike-through",                      FACE_STRIKE_THROUGH},
    {"box",                                 FACE_BOX},
    {"region",                              FACE_REGION},
    {"shadow",                              FACE_SHADOW},
    {"highlight",                           FACE_HIGHLIGHT},
    {"help-key-binding",                    FACE_HELP_KEY_BINDING},
    {"completions-highlight",               FACE_COMPLETIONS_HIGHLIGHT},
    {"completions-annotations",             FACE_COMPLETIONS_ANNOTATIONS},
    {"completions-common-part",             FACE_COMPLETIONS_COMMON_PART},
    {"completions-first-difference",        FACE_COMPLETIONS_FIRST_DIFFERENCE},
    {"rainbow-delimiters-depth-1-face",     FACE_RAINBOW_DELIMITERS_DEPTH_1},
    {"rainbow-delimiters-depth-2-face",     FACE_RAINBOW_DELIMITERS_DEPTH_2},
    {"rainbow-delimiters-depth-3-face",     FACE_RAINBOW_DELIMITERS_DEPTH_3},
    {"rainbow-delimiters-depth-4-face",     FACE_RAINBOW_DELIMITERS_DEPTH_4},
    {"rainbow-delimiters-depth-5-face",     FACE_RAINBOW_DELIMITERS_DEPTH_5},
    {"rainbow-delimiters-depth-6-face",     FACE_RAINBOW_DELIMITERS_DEPTH_6},
    {"rainbow-delimiters-depth-7-face",     FACE_RAINBOW_DELIMITERS_DEPTH_7},
    {"rainbow-delimiters-depth-8-face",     FACE_RAINBOW_DELIMITERS_DEPTH_8},
    {"rainbow-delimiters-depth-9-face",     FACE_RAINBOW_DELIMITERS_DEPTH_9},
    {"rainbow-delimiters-unmatched-face",   FACE_RAINBOW_DELIMITERS_UNMATCHED},
    {"rainbow-delimiters-mismatched-face",  FACE_RAINBOW_DELIMITERS_MISMATCHED},
    {"escape-glyph",                        FACE_ESCAPE_GLYPH},
    {"isearch",                             FACE_ISEARCH},
    {"isearch-fail",                        FACE_ISEARCH_FAIL},
    {"lazy-highlight",                      FACE_LAZY_HIGHLIGHT},
    {NULL, -1}
};

FaceCache *face_cache = NULL;

static Face *create_face(int id) {
    Face *face = calloc(1, sizeof(Face));
    if (!face) return NULL;
    face->id           = id;
    face->fg           = (Color){0, 0, 0, 1};
    face->bg           = (Color){1, 1, 1, 1};
    face->font         = NULL;   // resolved lazily via get_face_font()
    face->bold         = false;
    face->italic       = false;
    face->underline    = false;
    face->strike_through = false;
    face->box          = false;
    face->fg_set       = false;
    face->bg_set       = false;
    face->extend       = false;
    face->inherit_from = -1;
    face->family       = NULL;
    face->height       = 0;
    face->underline_color    = (Color){0, 0, 0, 1};
    face->strike_through_color = (Color){0, 0, 0, 1};
    face->box_color          = (Color){0, 0, 0, 1};
    return face;
}

void resolve_face_inheritance(void) {
    if (!face_cache) return;
    Face *default_face = get_face(FACE_DEFAULT);
    if (!default_face) return;

    for (int i = 0; i < face_cache->count; i++) {
        Face *face = face_cache->faces[i];
        if (!face || face->id == FACE_DEFAULT) continue;

        const int MAX_DEPTH = 10;

        if (!face->fg_set) {
            Face *iface = face;
            int depth = 0;
            while (!iface->fg_set && iface->inherit_from >= 0 && depth < MAX_DEPTH) {
                iface = get_face(iface->inherit_from);
                if (!iface) break;
                depth++;
            }
            face->fg = (iface && iface->fg_set) ? iface->fg : default_face->fg;
        }

        if (!face->bg_set) {
            Face *iface = face;
            int depth = 0;
            while (!iface->bg_set && iface->inherit_from >= 0 && depth < MAX_DEPTH) {
                iface = get_face(iface->inherit_from);
                if (!iface) break;
                depth++;
            }
            face->bg = (iface && iface->bg_set) ? iface->bg : default_face->bg;
        }

        if (face->underline &&
            face->underline_color.r == 0 && face->underline_color.g == 0 &&
            face->underline_color.b == 0)
            face->underline_color = face->fg;

        if (face->strike_through &&
            face->strike_through_color.r == 0 && face->strike_through_color.g == 0 &&
            face->strike_through_color.b == 0)
            face->strike_through_color = face->fg;

        if (face->box &&
            face->box_color.r == 0 && face->box_color.g == 0 &&
            face->box_color.b == 0)
            face->box_color = face->fg;
    }
}

// Bootstraps the global default family/size, seeds the font registry with the
// regular face only (all other variants loaded on first use), then creates every
// built-in face without pre-loading any font variant.
void init_faces(void) {
    if (face_cache) return;

    // Establish global default font spec
    g_default_family     = strdup("Adwaita Mono");
    g_default_pixel_size = 22;

    // Warm up the registry for the regular variant only.
    // All other (bold, italic, bold+italic) variants are loaded on first use.
    Font *default_font = font_registry_get(g_default_family,
                                            g_default_pixel_size,
                                            false, false);
    if (!default_font) {
        fprintf(stderr, "Failed to load default font '%s'\n", g_default_family);
        return;
    }

    face_cache = calloc(1, sizeof(FaceCache));
    face_cache->size  = FACE_BUILTIN_COUNT;
    face_cache->count = 0;
    face_cache->faces = calloc(face_cache->size, sizeof(Face*));

    for (int i = 0; i < FACE_BUILTIN_COUNT; i++) {
        Face *face = create_face(i);

        // NOTE: face->font stays NULL for almost all faces.
        //       get_face_font() resolves lazily via the registry.
        //       We only set face->font explicitly for faces where we KNOW
        //       they need a different variant (bold/italic) so that the
        //       variant is requested the first time the face is rendered.
        //       Even that lookup is still lazy inside font_registry_get().

        switch (i) {
            case FACE_DEFAULT:
                face->fg = parse_color("black");
                face->bg = parse_color("white");
                face->fg_set = face->bg_set = true;
                break;

            // ── font variant faces ────────────────────────────────────────────
            // We mark bold/italic here; get_face_font() will pick the right
            // variant from the registry on first use.
            case FACE_BOLD:
                face->bold = true;
                break;
            case FACE_ITALIC:
                face->italic = true;
                break;
            case FACE_BOLD_ITALIC:
                face->bold = face->italic = true;
                break;

            // ── mode line ─────────────────────────────────────────────────────
            case FACE_MODE_LINE:
            case FACE_MODE_LINE_ACTIVE:
                face->fg = parse_color("black");
                face->bg = parse_color("grey75");
                face->fg_set = face->bg_set = true;
                face->box = true;
                break;
            case FACE_MODE_LINE_INACTIVE:
                face->fg = parse_color("grey20");
                face->bg = parse_color("grey90");
                face->fg_set = face->bg_set = true;
                face->box = true;
                face->box_color = parse_color("grey75");
                break;

            case FACE_CURSOR:
                face->bg = parse_color("black");  face->bg_set = true;
                face->fg = parse_color("white");  face->fg_set = true;
                break;
            case FACE_VISIBLE_MARK:
                face->bg = parse_color("grey80"); face->bg_set = true;
                break;
            case FACE_FRINGE:
                face->fg = parse_color("black");  face->fg_set = true;
                face->bg = parse_color("grey95"); face->bg_set = true;
                break;
            case FACE_WINDOW_DIVIDER:
                face->bg = parse_color("black");  face->bg_set = true;
                break;

            case FACE_ERROR:
                face->fg = parse_color("red1");   face->fg_set = true;
                face->bold = true;
                break;
            case FACE_SUCCESS:
                face->fg = parse_color("ForestGreen"); face->fg_set = true;
                face->bold = true;
                break;
            case FACE_WARNING:
                face->fg = parse_color("DarkOrange"); face->fg_set = true;
                face->bold = true;
                break;

            // ── font-lock ─────────────────────────────────────────────────────
            case FACE_FONT_LOCK_BRACKET:
                face->inherit_from = FACE_FONT_LOCK_PUNCTUATION; break;
            case FACE_FONT_LOCK_BUILTIN:
                face->fg = parse_color("dark slate blue"); face->fg_set = true; break;
            case FACE_FONT_LOCK_COMMENT_DELIMITER:
                face->inherit_from = FACE_FONT_LOCK_COMMENT; break;
            case FACE_FONT_LOCK_COMMENT:
                face->fg = parse_color("firebrick"); face->fg_set = true; break;
            case FACE_FONT_LOCK_CONSTANT:
                face->fg = parse_color("dark cyan"); face->fg_set = true; break;
            case FACE_FONT_LOCK_DELIMITER:
                face->inherit_from = FACE_FONT_LOCK_PUNCTUATION; break;
            case FACE_FONT_LOCK_DOC:
                face->inherit_from = FACE_FONT_LOCK_STRING; break;
            case FACE_FONT_LOCK_DOC_MARKUP:
                face->inherit_from = FACE_FONT_LOCK_CONSTANT; break;
            case FACE_FONT_LOCK_ESCAPE:
                face->inherit_from = FACE_FONT_LOCK_REGEXP_GROUPING_BACKSLASH; break;
            case FACE_FONT_LOCK_FUNCTION_CALL:
                face->inherit_from = FACE_FONT_LOCK_FUNCTION_NAME; break;
            case FACE_FONT_LOCK_FUNCTION_NAME:
                face->fg = parse_color("blue1"); face->fg_set = true; break;
            case FACE_FONT_LOCK_KEYWORD:
                face->fg = parse_color("purple"); face->fg_set = true; break;
            case FACE_FONT_LOCK_MISC_PUNCTUATION:
                face->inherit_from = FACE_FONT_LOCK_PUNCTUATION; break;
            case FACE_FONT_LOCK_PREPROCESSOR:
                face->inherit_from = FACE_FONT_LOCK_BUILTIN; break;
            case FACE_FONT_LOCK_PROPERTY_NAME:
                face->inherit_from = FACE_FONT_LOCK_VARIABLE_NAME; break;
            case FACE_FONT_LOCK_PROPERTY_USE:
                face->inherit_from = FACE_FONT_LOCK_PROPERTY_NAME; break;
            case FACE_FONT_LOCK_REGEXP:
                face->inherit_from = FACE_FONT_LOCK_STRING; break;
            case FACE_FONT_LOCK_REGEXP_GROUPING_BACKSLASH:
                face->bold = true; break;
            case FACE_FONT_LOCK_REGEXP_GROUPING_CONSTRUCT:
                face->bold = true; break;
            case FACE_FONT_LOCK_STRING:
                face->fg = parse_color("VioletRed4"); face->fg_set = true; break;
            case FACE_FONT_LOCK_TYPE:
                face->fg = parse_color("ForestGreen"); face->fg_set = true; break;
            case FACE_FONT_LOCK_VARIABLE_NAME:
                face->fg = parse_color("sienna"); face->fg_set = true; break;
            case FACE_FONT_LOCK_VARIABLE_USE:
                face->inherit_from = FACE_FONT_LOCK_VARIABLE_NAME; break;
            case FACE_FONT_LOCK_WARNING:
                face->inherit_from = FACE_ERROR; break;

            case FACE_MINIBUFFER_PROMPT:
                face->fg = parse_color("medium blue"); face->fg_set = true; break;
            case FACE_UNDERLINE:
                face->underline = true; break;
            case FACE_STRIKE_THROUGH:
                face->strike_through = true; break;
            case FACE_BOX:
                face->box = true; break;
            case FACE_REGION:
                face->bg = parse_color("LightGoldenrod2"); face->bg_set = true;
                face->extend = true;
                break;
            case FACE_SHADOW:
                face->fg = parse_color("grey50"); face->fg_set = true; break;
            case FACE_HIGHLIGHT:
                face->bg = parse_color("DarkSeaGreen2"); face->bg_set = true; break;
            case FACE_HELP_KEY_BINDING:
                face->fg = parse_color("DarkBlue");  face->fg_set = true;
                face->bg = parse_color("grey96");    face->bg_set = true;
                face->box = true;
                face->box_color = parse_color("grey80");
                break;
            case FACE_COMPLETIONS_HIGHLIGHT:
                face->inherit_from = FACE_HIGHLIGHT; break;
            case FACE_COMPLETIONS_ANNOTATIONS:
                face->inherit_from = FACE_SHADOW;
                face->italic = true;
                break;
            case FACE_COMPLETIONS_COMMON_PART:
                face->fg = parse_color("blue3"); face->fg_set = true; break;
            case FACE_COMPLETIONS_FIRST_DIFFERENCE:
                face->bold = true; break;

            case FACE_RAINBOW_DELIMITERS_DEPTH_1: face->fg = parse_color("#707183"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_2: face->fg = parse_color("#7388d6"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_3: face->fg = parse_color("#909183"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_4: face->fg = parse_color("#709870"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_5: face->fg = parse_color("#907373"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_6: face->fg = parse_color("#6276ba"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_7: face->fg = parse_color("#858580"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_8: face->fg = parse_color("#80a880"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_DEPTH_9: face->fg = parse_color("#887070"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_UNMATCHED:  face->fg = parse_color("#FF0000"); face->fg_set = true; break;
            case FACE_RAINBOW_DELIMITERS_MISMATCHED: face->fg = parse_color("#BB0000"); face->fg_set = true; break;

            case FACE_ESCAPE_GLYPH:
                face->fg = parse_color("brown"); face->fg_set = true; break;
            case FACE_ISEARCH:
                face->fg = parse_color("LightSkyBlue1"); face->fg_set = true;
                face->bg = parse_color("magenta3");      face->bg_set = true;
                break;
            case FACE_ISEARCH_FAIL:
                face->bg = parse_color("RosyBrown1"); face->bg_set = true; break;
            case FACE_LAZY_HIGHLIGHT:
                face->bg = parse_color("PaleTurquoise"); face->bg_set = true; break;

            default:
                break;
        }

        face_cache->faces[i] = face;
        face_cache->count++;
    }

    resolve_face_inheritance();
}

void free_faces(void) {
    if (!face_cache) return;
    for (int i = 0; i < face_cache->count; i++) {
        if (face_cache->faces[i]) {
            free(face_cache->faces[i]->family);
            free(face_cache->faces[i]);
        }
    }
    free(face_cache->faces);
    free(face_cache);
    face_cache = NULL;

    free(g_default_family);
    g_default_family = NULL;

    dynface_free_all();
    font_registry_free_all();
    FcFini();
}

/// Accessors

Face *get_face(int id) {
    if (!face_cache || id < 0 || id >= face_cache->count)
        return face_cache ? face_cache->faces[FACE_DEFAULT] : NULL;
    return face_cache->faces[id];
}

int face_id_from_name(const char *name) {
    for (int i = 0; face_names[i].name; i++)
        if (strcmp(name, face_names[i].name) == 0)
            return face_names[i].id;
    return dynface_lookup(name);
}

Face *get_named_face(const char *name) {
    int id = face_id_from_name(name);
    if (id < 0) return get_face(FACE_DEFAULT);
    return get_face(id);
}

int face_at_pos(Buffer *buf, size_t pos) {
    if (!buf) return FACE_DEFAULT;
    return get_text_property_face(buf, pos);
}

int register_dynamic_face(const char *name, int inherit_from) {
    int existing = face_id_from_name(name);
    if (existing >= 0) return existing;
    if (!face_cache) return -1;

    if (face_cache->count >= face_cache->size) {
        int new_size = face_cache->size + 64;
        Face **nf = realloc(face_cache->faces, new_size * sizeof(Face*));
        if (!nf) return -1;
        face_cache->faces = nf;
        face_cache->size  = new_size;
    }

    int id = face_cache->count;
    Face *face = create_face(id);
    face->inherit_from = inherit_from;

    face_cache->faces[id] = face;
    face_cache->count++;

    if (!dynface_register(name, id)) {
        free(face);
        face_cache->faces[id] = NULL;
        face_cache->count--;
        return -1;
    }

    resolve_face_inheritance();
    return id;
}

Color parse_color(const char *str) {
    if (!str || !str[0]) return (Color){0, 0, 0, 1};

    if (str[0] == '#') {
        int r, g, b, a = 255;
        size_t len = strlen(str);
        if      (len == 9) sscanf(str, "#%02x%02x%02x%02x", &r, &g, &b, &a);
        else if (len == 7) sscanf(str, "#%02x%02x%02x",     &r, &g, &b);
        else               return (Color){0, 0, 0, 1};

        float rf = r/255.0f, gf = g/255.0f, bf = b/255.0f, af = a/255.0f;
        rf = (rf <= 0.04045f) ? rf/12.92f : powf((rf+0.055f)/1.055f, 2.4f);
        gf = (gf <= 0.04045f) ? gf/12.92f : powf((gf+0.055f)/1.055f, 2.4f);
        bf = (bf <= 0.04045f) ? bf/12.92f : powf((bf+0.055f)/1.055f, 2.4f);
        return (Color){rf, gf, bf, af};
    }

    const X11Color *x = lookup_x11_color(str);
    if (x) return (Color){x->r, x->g, x->b, 1.0f};

    return (Color){1, 0, 0, 1}; // red = error
}

/// SCM

static char *scm_to_c_string(SCM s) {
    if (scm_is_string(s))
        return scm_to_locale_string(s);
    if (scm_is_symbol(s))
        return scm_to_locale_string(scm_symbol_to_string(s));
    return NULL;
}

static SCM scm_make_face(SCM name) {
    if (!scm_is_string(name) && !scm_is_symbol(name))
        scm_wrong_type_arg("make-face", 1, name);
    char *n = scm_to_c_string(name);
    int id = face_id_from_name(n);
    free(n);
    return id >= 0 ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_set_face_foreground(SCM name, SCM color) {
    char *n = scm_to_c_string(name);
    Face *face = get_named_face(n); free(n);
    if (face) {
        char *cs = scm_to_locale_string(color);
        face->fg = parse_color(cs); face->fg_set = true;
        free(cs);
        resolve_face_inheritance();
        return SCM_BOOL_T;
    }
    return SCM_BOOL_F;
}

static SCM scm_set_face_background(SCM name, SCM color) {
    char *n = scm_to_c_string(name);
    Face *face = get_named_face(n); free(n);
    if (face) {
        char *cs = scm_to_locale_string(color);
        face->bg = parse_color(cs); face->bg_set = true;
        free(cs);
        resolve_face_inheritance();
        return SCM_BOOL_T;
    }
    return SCM_BOOL_F;
}

static Color color_to_srgb_string(Color c, char buf[22]) {
    float r = (c.r <= 0.0031308f) ? c.r*12.92f : 1.055f*powf(c.r, 1.f/2.4f)-0.055f;
    float g = (c.g <= 0.0031308f) ? c.g*12.92f : 1.055f*powf(c.g, 1.f/2.4f)-0.055f;
    float b = (c.b <= 0.0031308f) ? c.b*12.92f : 1.055f*powf(c.b, 1.f/2.4f)-0.055f;
    snprintf(buf, 22, "#%02x%02x%02x",
             (int)(r*255), (int)(g*255), (int)(b*255));
    return c;
}

static SCM scm_face_foreground(SCM name) {
    char *n = scm_to_c_string(name);
    Face *face = get_named_face(n); free(n);
    if (!face) return SCM_BOOL_F;
    char buf[22]; color_to_srgb_string(face->fg, buf);
    return scm_from_locale_string(buf);
}

static SCM scm_face_background(SCM name) {
    char *n = scm_to_c_string(name);
    Face *face = get_named_face(n); free(n);
    if (!face) return SCM_BOOL_F;
    char buf[22]; color_to_srgb_string(face->bg, buf);
    return scm_from_locale_string(buf);
}

static SCM scm_face_id_from_name(SCM name) {
    char *n = scm_to_c_string(name);
    if (!n) return scm_from_int(-1);
    int id = face_id_from_name(n); free(n);
    return scm_from_int(id);
}

// ── set-face-attribute ────────────────────────────────────────────────────────
//
// (set-face-attribute FACE FRAME &rest PLIST)
//
// Supported attributes:
//   :foreground STRING
//   :background STRING
//   :family     STRING
//   :height     INTEGER          (1/10 pt, Emacs convention)
//   :weight     'bold | 'normal  (anything non-bold treated as normal)
//   :slant      'italic | 'normal
//   :underline  t | nil
//   :strike-through t | nil
//   :box        t | nil
//   :inherit    FACE-NAME
//
// FRAME is ignored (we have a single frame).
// Unknown attributes are silently skipped for forward compatibility.
// ─────────────────────────────────────────────────────────────────────────────


static char *extract_key(SCM key) {
    if (scm_is_keyword(key))
        return scm_to_locale_string(
            scm_symbol_to_string(scm_keyword_to_symbol(key)));
    if (scm_is_symbol(key)) {
        char *raw = scm_to_locale_string(scm_symbol_to_string(key));
        if (raw && raw[0] == ':') {
            char *stripped = strdup(raw + 1);
            free(raw);
            return stripped;
        }
        return raw;
    }
    return NULL;
}


#include "frame.h"

static SCM scm_set_face_attribute(SCM name, SCM frame, SCM rest) {
    (void)frame;

    char *face_name = scm_to_c_string(name);
    if (!face_name) return SCM_BOOL_F;

    int id = face_id_from_name(face_name);
    if (id < 0) id = register_dynamic_face(face_name, -1);
    free(face_name);
    if (id < 0) return SCM_BOOL_F;

    Face *face = get_face(id);
    if (!face) return SCM_BOOL_F;

    bool font_changed = false;

    SCM pl = rest;
    while (scm_is_pair(pl) && scm_is_pair(scm_cdr(pl))) {
        SCM key = scm_car(pl);
        SCM val = scm_cadr(pl);
        pl = scm_cddr(pl);

        char *k = extract_key(key);
        if (!k) continue;

        if (strcmp(k, "foreground") == 0 && scm_is_string(val)) {
            char *cs = scm_to_locale_string(val);
            face->fg = parse_color(cs);
            face->fg_set = true;
            free(cs);

        } else if (strcmp(k, "background") == 0 && scm_is_string(val)) {
            char *cs = scm_to_locale_string(val);
            face->bg = parse_color(cs);
            face->bg_set = true;
            free(cs);

        } else if (strcmp(k, "family") == 0) {
            char *fam = scm_to_c_string(val);
            if (fam) {
                free(face->family);
                if (strcmp(fam, "unspecified") == 0) {
                    face->family = NULL;
                } else {
                    face->family = strdup(fam);
                    if (id == FACE_DEFAULT) {
                        free(g_default_family);
                        g_default_family = strdup(fam);
                    }
                }
                free(fam);
                font_changed = true;
                face->font = NULL;
            }

        } else if (strcmp(k, "height") == 0 && scm_is_integer(val)) {
            int h = scm_to_int(val);
            face->height = h;
            if (id == FACE_DEFAULT && h > 0)
                g_default_pixel_size = height_to_pixel_size(h);
            font_changed = true;
            face->font = NULL;

        } else if (strcmp(k, "weight") == 0) {
            char *ws = scm_to_c_string(val);
            if (ws) {
                bool new_bold = (strcmp(ws, "bold")       == 0 ||
                                 strcmp(ws, "extra-bold")  == 0 ||
                                 strcmp(ws, "ultra-bold")  == 0);
                if (face->bold != new_bold) {
                    face->bold = new_bold;
                    font_changed = true;
                    face->font = NULL;
                }
                free(ws);
            }

        } else if (strcmp(k, "slant") == 0) {
            char *ss = scm_to_c_string(val);
            if (ss) {
                bool new_italic = (strcmp(ss, "italic")        == 0 ||
                                   strcmp(ss, "oblique")        == 0 ||
                                   strcmp(ss, "reverse-italic") == 0);
                if (face->italic != new_italic) {
                    face->italic = new_italic;
                    font_changed = true;
                    face->font = NULL;
                }
                free(ss);
            }

        } else if (strcmp(k, "underline") == 0) {
            face->underline = scm_is_true(val);

        } else if (strcmp(k, "strike-through") == 0) {
            face->strike_through = scm_is_true(val);

        } else if (strcmp(k, "box") == 0) {
            face->box = scm_is_true(val);

        } else if (strcmp(k, "inherit") == 0) {
            char *iname = scm_to_c_string(val);
            if (iname) {
                face->inherit_from = face_id_from_name(iname);
                free(iname);
            }
        }

        free(k);
    }

    if (font_changed && id == FACE_DEFAULT) {
        // Clear font pointer on all faces so they re-resolve on next render
        for (int i = 0; i < face_cache->count; i++)
            if (face_cache->faces[i])
                face_cache->faces[i]->font = NULL;

        // Update frame metrics immediately
        if (selected_frame) {
            Font *new_font = get_face_font(get_face(FACE_DEFAULT));
            if (new_font) {
                selected_frame->line_height = new_font->ascent + new_font->descent;
                Character *space = font_get_character(new_font, ' ');
                if (space)
                    selected_frame->column_width = space->ax;
            }
        }
    }

    resolve_face_inheritance();
    return SCM_UNSPECIFIED;
}


/// defface

static SCM defface_spec_to_plist(SCM spec) {
    SCM iter = spec;
    while (scm_is_pair(iter)) {
        SCM entry = scm_car(iter);
        if (scm_is_pair(entry)) {
            SCM display = scm_car(entry);
            SCM rest    = scm_cdr(entry);
            if (scm_is_true(display) && scm_is_pair(rest))
                return scm_car(rest);
        }
        iter = scm_cdr(iter);
    }
    return SCM_EOL;
}

static SCM scm_defface_impl(SCM name, SCM spec, SCM docstring) {
    (void)docstring;

    char *face_name = scm_to_c_string(name);
    if (!face_name) return SCM_BOOL_F;

    SCM plist = defface_spec_to_plist(spec);

    int   inherit_from  = -1;
    bool  has_fg = false, has_bg = false;
    bool  bold = false, italic = false, underline = false, strike_through = false;
    Color fg = {0}, bg = {0};

    SCM pl = plist;
    while (scm_is_pair(pl) && scm_is_pair(scm_cdr(pl))) {
        SCM key = scm_car(pl);
        SCM val = scm_cadr(pl);
        pl = scm_cddr(pl);

        char *k = extract_key(key);
        if (!k) continue;

        if (strcmp(k, "inherit") == 0) {
            char *iname = scm_to_c_string(val);
            if (iname) { inherit_from = face_id_from_name(iname); free(iname); }
        } else if (strcmp(k, "foreground") == 0 && scm_is_string(val)) {
            char *cs = scm_to_locale_string(val);
            fg = parse_color(cs); has_fg = true; free(cs);
        } else if (strcmp(k, "background") == 0 && scm_is_string(val)) {
            char *cs = scm_to_locale_string(val);
            bg = parse_color(cs); has_bg = true; free(cs);
        } else if (strcmp(k, "weight") == 0) {
            char *ws = scm_to_c_string(val);
            if (ws) { bold = strcmp(ws, "bold") == 0; free(ws); }
        } else if (strcmp(k, "slant") == 0) {
            char *ss = scm_to_c_string(val);
            if (ss) { italic = strcmp(ss, "italic") == 0; free(ss); }
        } else if (strcmp(k, "underline") == 0) {
            underline = scm_is_true(val);
        } else if (strcmp(k, "strike-through") == 0) {
            strike_through = scm_is_true(val);
        }
        free(k);
    }

    int id = register_dynamic_face(face_name, inherit_from);
    if (id >= 0) {
        Face *face = get_face(id);
        if (face) {
            if (has_fg)         { face->fg = fg; face->fg_set = true; }
            if (has_bg)         { face->bg = bg; face->bg_set = true; }
            if (bold)           { face->bold = true;  face->font = NULL; }
            if (italic)         { face->italic = true; face->font = NULL; }
            if (underline)      { face->underline = true; }
            if (strike_through) { face->strike_through = true; }
            resolve_face_inheritance();
        }
    }

    free(face_name);
    return SCM_UNSPECIFIED;
}

void init_face_bindings(void) {
    scm_c_define_gsubr("make-face",             1, 0, 0, scm_make_face);
    scm_c_define_gsubr("set-face-foreground!",  2, 0, 0, scm_set_face_foreground);
    scm_c_define_gsubr("set-face-background!",  2, 0, 0, scm_set_face_background);
    scm_c_define_gsubr("face-foreground",       1, 0, 0, scm_face_foreground);
    scm_c_define_gsubr("face-background",       1, 0, 0, scm_face_background);
    scm_c_define_gsubr("face-id-from-name",     1, 0, 0, scm_face_id_from_name);
    scm_c_define_gsubr("%defface",              2, 1, 1, scm_defface_impl);
    // 2 required args (face, frame), rest = plist of attributes
    scm_c_define_gsubr("set-face-attribute",    2, 0, 1, scm_set_face_attribute);
}

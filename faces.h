#pragma once
#include <obsidian/obsidian.h>
#include <stdbool.h>
typedef struct Buffer Buffer;
typedef enum {
    FACE_DEFAULT = 0,
    FACE_MODE_LINE,
    FACE_MODE_LINE_ACTIVE,
    FACE_MODE_LINE_INACTIVE,
    FACE_FRINGE,
    FACE_CURSOR,
    FACE_BORDER,
    FACE_WINDOW_DIVIDER,
    FACE_BOLD,
    FACE_ITALIC,
    FACE_BOLD_ITALIC,
    FACE_VISIBLE_MARK,
    FACE_ERROR,
    FACE_SUCCESS,
    FACE_WARNING,
    FACE_FONT_LOCK_BRACKET,
    FACE_FONT_LOCK_BUILTIN,
    FACE_FONT_LOCK_COMMENT_DELIMITER,
    FACE_FONT_LOCK_COMMENT,
    FACE_FONT_LOCK_CONSTANT,
    FACE_FONT_LOCK_DELIMITER,
    FACE_FONT_LOCK_DOC,
    FACE_FONT_LOCK_DOC_MARKUP,
    FACE_FONT_LOCK_ESCAPE,
    FACE_FONT_LOCK_FUNCTION_CALL,
    FACE_FONT_LOCK_FUNCTION_NAME,
    FACE_FONT_LOCK_KEYWORD,
    FACE_FONT_LOCK_MISC_PUNCTUATION,
    FACE_FONT_LOCK_NEGATION_CHAR,
    FACE_FONT_LOCK_NUMBER,
    FACE_FONT_LOCK_OPERATOR,
    FACE_FONT_LOCK_PREPROCESSOR,
    FACE_FONT_LOCK_PROPERTY_NAME,
    FACE_FONT_LOCK_PROPERTY_USE,
    FACE_FONT_LOCK_PUNCTUATION,
    FACE_FONT_LOCK_REGEXP,
    FACE_FONT_LOCK_REGEXP_GROUPING_BACKSLASH,
    FACE_FONT_LOCK_REGEXP_GROUPING_CONSTRUCT,
    FACE_FONT_LOCK_STRING,
    FACE_FONT_LOCK_TYPE,
    FACE_FONT_LOCK_VARIABLE_NAME,
    FACE_FONT_LOCK_VARIABLE_USE,
    FACE_FONT_LOCK_WARNING,
    FACE_MINIBUFFER_PROMPT,
    FACE_UNDERLINE,
    FACE_STRIKE_THROUGH,
    FACE_BOX,
    FACE_REGION,
    FACE_SHADOW,
    FACE_HIGHLIGHT,
    FACE_HELP_KEY_BINDING,
    FACE_COMPLETIONS_HIGHLIGHT,
    FACE_COMPLETIONS_ANNOTATIONS,
    FACE_COMPLETIONS_COMMON_PART,
    FACE_COMPLETIONS_FIRST_DIFFERENCE,
    FACE_RAINBOW_DELIMITERS_DEPTH_1,
    FACE_RAINBOW_DELIMITERS_DEPTH_2,
    FACE_RAINBOW_DELIMITERS_DEPTH_3,
    FACE_RAINBOW_DELIMITERS_DEPTH_4,
    FACE_RAINBOW_DELIMITERS_DEPTH_5,
    FACE_RAINBOW_DELIMITERS_DEPTH_6,
    FACE_RAINBOW_DELIMITERS_DEPTH_7,
    FACE_RAINBOW_DELIMITERS_DEPTH_8,
    FACE_RAINBOW_DELIMITERS_DEPTH_9,
    FACE_RAINBOW_DELIMITERS_UNMATCHED,
    FACE_RAINBOW_DELIMITERS_MISMATCHED,
    FACE_ESCAPE_GLYPH,
    FACE_ISEARCH,
    FACE_ISEARCH_FAIL,
    FACE_LAZY_HIGHLIGHT,
    FACE_BUILTIN_COUNT,
} FaceId;

typedef struct Face {
    int id;
    Color fg;
    Color bg;
    Font *font;         // Resolved font pointer (may be NULL = use default_font)
    bool bold;
    bool italic;
    bool underline;
    Color underline_color;
    bool strike_through;
    Color strike_through_color;
    bool box;
    Color box_color;
    bool fg_set;
    bool bg_set;
    bool extend;
    int  inherit_from;  // face ID to inherit from, -1 for none

    // Explicit font spec (set via set-face-attribute).
    // When family is non-NULL the face owns its own Font* rather than
    // sharing the global default variants.
    char *family;       // heap-allocated, or NULL → use default family
    int   height;       // in 1/10 pt (Emacs convention), 0 → use default

    struct Face *next;
} Face;

typedef struct {
    Face **faces;
    int size;
    int count;
} FaceCache;

// ── Font registry ─────────────────────────────────────────────────────────────
// Lazily-loaded fonts keyed by (family, pixel_size, bold, italic).
// Shared across all faces; never freed until free_faces().
// ─────────────────────────────────────────────────────────────────────────────
typedef struct FontRegistryEntry {
    char *family;
    int   pixel_size;
    bool  bold;
    bool  italic;
    Font *font;
    struct FontRegistryEntry *next;
} FontRegistryEntry;

// Global
extern FaceCache *face_cache;

// ── Font registry API ─────────────────────────────────────────────────────────
// Look up or load a font.  All returned Font* are owned by the registry.
Font *font_registry_get(const char *family, int pixel_size, bool bold, bool italic);
void  font_registry_free_all(void);

// Return the pixel size that corresponds to an Emacs :height value (1/10 pt)
// given the current frame DPI.  Falls back to the default pixel size when
// height == 0.
int   height_to_pixel_size(int height_10pt);

// ── Default font spec ─────────────────────────────────────────────────────────
// These are the family/size used for the DEFAULT face and as fallback for all
// faces that have no explicit :family / :height.  Updated by set-face-attribute
// on the 'default face.
extern char *g_default_family;   // heap-allocated
extern int   g_default_pixel_size;

// ── Core face API ─────────────────────────────────────────────────────────────
void  init_faces(void);
void  free_faces(void);
Face *get_face(int id);

// Return the font that should be used to render a character in `face`.
// Respects face->family / face->height and bold/italic attributes.
// Never returns NULL as long as at least one font could be loaded.
Font *get_face_font(Face *face);

void  resolve_face_inheritance(void);
Face *get_named_face(const char *name);
int   face_id_from_name(const char *name);
int   face_at_pos(Buffer *buf, size_t pos);

Font  *fontconfig_load_font(const char *family, int size, bool bold, bool italic);
Color  parse_color(const char *str);
int    register_dynamic_face(const char *name, int inherit_from);

void   init_face_bindings(void);

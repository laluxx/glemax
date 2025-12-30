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


    FACE_BUILTIN_COUNT,
} FaceId;

typedef struct Face {
    int id;
    Color fg;           // Resolved color (after inheritance)
    Color bg;           // Resolved color (after inheritance)
    Font *font;
    bool bold;
    bool italic;
    bool underline;
    Color underline_color;
    bool strike_through;
    Color strike_through_color;
    bool box;
    Color box_color;
    bool fg_set;        // True if foreground explicitly set
    bool bg_set;        // True if background explicitly set
    bool extend;
    int inherit_from;   // face ID to inherit from, -1 for none
    struct Face *next;
} Face;

typedef struct {
    Face **faces;
    int size;
    int count;
} FaceCache;

// Global
extern FaceCache *face_cache;

void init_faces(void);
void free_faces(void);

Face *get_face(int id);
Font *get_face_font(Face *face);
void resolve_face_inheritance(void);
Face *get_named_face(const char *name);
int face_id_from_name(const char *name);
int face_at_pos(Buffer *buf, size_t pos);
Font *fontconfig_load_font(const char *family, int size, bool bold, bool italic);
Color parse_color(const char *str);

Font *get_font_variant(bool bold, bool italic);
void init_face_bindings(void);

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
    bool fg_set;        // True if foreground explicitly set
    bool bg_set;        // True if background explicitly set
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

// Resolve face inheritance - call after modifying faces or loading themes
void resolve_face_inheritance(void);

Face *get_named_face(const char *name);
int face_id_from_name(const char *name);
int face_at_pos(Buffer *buf, size_t pos);

Font *fontconfig_load_font(const char *family, int size, bool bold, bool italic);
Color parse_color(const char *str);

void init_face_bindings(void);

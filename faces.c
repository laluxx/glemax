#include "faces.h"
#include "buffer.h"
#include <obsidian/common.h>
#include <stdlib.h>
#include <string.h>
#include <fontconfig/fontconfig.h>

FaceCache *face_cache = NULL;

// Font cache for variants
static Font *cached_font_regular = NULL;
static Font *cached_font_bold = NULL;
static Font *cached_font_italic = NULL;
static Font *cached_font_bold_italic = NULL;

static struct {
    const char *name;
    int id;
} face_names[] = {
    {"default",            FACE_DEFAULT},
    {"mode-line",          FACE_MODE_LINE},
    {"mode-line-active",   FACE_MODE_LINE_ACTIVE},
    {"mode-line-inactive", FACE_MODE_LINE_INACTIVE},
    {"window-divider",     FACE_WINDOW_DIVIDER},
    {"fringe",             FACE_FRINGE},
    {"cursor",             FACE_CURSOR},
    {"bold",               FACE_BOLD},
    {"italic",             FACE_ITALIC},
    {"bold-italic",        FACE_BOLD_ITALIC},
    {"visible-mark",       FACE_VISIBLE_MARK},
    {NULL, -1}
};

Font *fontconfig_load_font(const char *family, int size, bool bold, bool italic) {
    FcPattern *pattern = FcPatternCreate();
    FcPatternAddString(pattern, FC_FAMILY, (const FcChar8*)family);
    FcPatternAddDouble(pattern, FC_SIZE, (double)size);
    FcPatternAddInteger(pattern, FC_WEIGHT, bold ? FC_WEIGHT_BOLD : FC_WEIGHT_REGULAR);
    FcPatternAddInteger(pattern, FC_SLANT, italic ? FC_SLANT_ITALIC : FC_SLANT_ROMAN);
    
    FcConfigSubstitute(NULL, pattern, FcMatchPattern);
    FcDefaultSubstitute(pattern);
    
    FcResult result;
    FcPattern *match = FcFontMatch(NULL, pattern, &result);
    FcPatternDestroy(pattern);
    
    if (!match) {
        fprintf(stderr, "Failed to match font: %s\n", family);
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

// Get the appropriate font variant based on bold/italic flags
Font *get_font_variant(bool bold, bool italic) {
    if (bold && italic) {
        if (!cached_font_bold_italic) {
            cached_font_bold_italic = fontconfig_load_font("monospace", 44, true, true);
        }
        return cached_font_bold_italic;
    } else if (bold) {
        if (!cached_font_bold) {
            cached_font_bold = fontconfig_load_font("monospace", 44, true, false);
        }
        return cached_font_bold;
    } else if (italic) {
        if (!cached_font_italic) {
            cached_font_italic = fontconfig_load_font("monospace", 44, false, true);
        }
        return cached_font_italic;
    } else {
        if (!cached_font_regular) {
            cached_font_regular = fontconfig_load_font("monospace", 44, false, false);
        }
        return cached_font_regular;
    }
}

static Face *create_face(int id) {
    Face *face = calloc(1, sizeof(Face));
    if (!face) return NULL;
    
    face->id = id;
    face->fg = (Color){0, 0, 0, 1};  // Default black
    face->bg = (Color){1, 1, 1, 1};  // Default white
    face->font = NULL;
    face->bold = false;
    face->italic = false;
    face->underline = false;
    face->fg_set = false;
    face->bg_set = false;
    face->next = NULL;
    
    return face;
}

// Resolve face inheritance - call this after changing faces or loading themes
void resolve_face_inheritance(void) {
    if (!face_cache) return;
    
    Face *default_face = get_face(FACE_DEFAULT);
    if (!default_face) return;
    
    // Resolve all faces that don't have explicit colors
    for (int i = 0; i < face_cache->count; i++) {
        Face *face = face_cache->faces[i];
        if (!face || face->id == FACE_DEFAULT) continue;
        
        // Inherit foreground from default if not explicitly set
        if (!face->fg_set) {
            face->fg = default_face->fg;
        }
        
        // Inherit background from default if not explicitly set
        if (!face->bg_set) {
            face->bg = default_face->bg;
        }
    }
}

// TODO Manually setting bg_set and fg_set is kinda tedious
void init_faces(void) {
    if (face_cache) return;
    
    face_cache = calloc(1, sizeof(FaceCache));
    face_cache->size = FACE_BUILTIN_COUNT;
    face_cache->count = 0;
    face_cache->faces = calloc(face_cache->size, sizeof(Face*));
    
    // Load the regular font variant
    Font *default_font = get_font_variant(false, false);
    if (!default_font) {
        fprintf(stderr, "Failed to load default font\n");
        return;
    }
    
    // Create all builtin faces
    for (int i = 0; i < FACE_BUILTIN_COUNT; i++) {
        Face *face = create_face(i);
        face->font = default_font;
        
        // Set base theme - clean black on white
        switch (i) {
            case FACE_DEFAULT:
                face->fg = parse_color("#000000");
                face->bg = parse_color("#FFFFFF");
                face->fg_set = true;
                face->bg_set = true;
                break;
            case FACE_BOLD:
                face->bold = true;
                face->font = get_font_variant(true, false);
                break;
            case FACE_ITALIC:
                face->italic = true;
                face->font = get_font_variant(false, true);
                break;
            case FACE_BOLD_ITALIC:
                face->bold = true;
                face->italic = true;
                face->font = get_font_variant(true, true);
                break;
            case FACE_MODE_LINE:
                face->fg = parse_color("#000000");
                face->bg = parse_color("#BFBFBF");
                face->fg_set = true;
                face->bg_set = true;
                break;
            case FACE_MODE_LINE_ACTIVE:
                face->fg = parse_color("#000000");
                face->bg = parse_color("#BFBFBF");
                face->fg_set = true;
                face->bg_set = true;
                break;
            case FACE_MODE_LINE_INACTIVE:
                face->fg = parse_color("#333333");
                face->bg = parse_color("#E5E5E5");
                face->fg_set = true;
                face->bg_set = true;
                break;
            case FACE_CURSOR:
                face->bg = parse_color("#000000");
                face->bg_set = true;
                break;
            case FACE_VISIBLE_MARK:
                face->bg = parse_color("#CCCCCC");
                face->bg_set = true;
                break;
            case FACE_FRINGE:
                face->bg = parse_color("#F2F2F2");
                face->fg = parse_color("#000000");
                face->fg_set = true;
                face->bg_set = true;
                break;
            case FACE_WINDOW_DIVIDER:
                face->bg = parse_color("#000000");
                face->bg_set = true;
                break;
        }
        
        face_cache->faces[i] = face;
        face_cache->count++;
    }
    
    // Resolve inheritance once during initialization
    resolve_face_inheritance();
}

void free_faces(void) {
    if (!face_cache) return;
    
    for (int i = 0; i < face_cache->count; i++) {
        free(face_cache->faces[i]);
    }
    
    free(face_cache->faces);
    free(face_cache);
    face_cache = NULL;
    
    // Free cached font variants
    // Note: Don't free the Font* themselves as they may be managed elsewhere
    cached_font_regular = NULL;
    cached_font_bold = NULL;
    cached_font_italic = NULL;
    cached_font_bold_italic = NULL;
    
    FcFini();
}

Face *get_face(int id) {
    if (!face_cache || id < 0 || id >= face_cache->count) {
        return face_cache ? face_cache->faces[FACE_DEFAULT] : NULL;
    }
    return face_cache->faces[id];
}

// Get the effective font for a face, considering bold/italic attributes
Font *get_face_font(Face *face) {
    if (!face) return get_font_variant(false, false);
    
    // If the face has a specific font set, use it
    if (face->font) {
        return face->font;
    }
    
    // Otherwise, get the appropriate variant based on attributes
    return get_font_variant(face->bold, face->italic);
}

int face_id_from_name(const char *name) {
    for (int i = 0; face_names[i].name != NULL; i++) {
        if (strcmp(name, face_names[i].name) == 0) {
            return face_names[i].id;
        }
    }
    return -1;
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

Color parse_color(const char *str) {
    if (!str || str[0] == '\0') {
        return (Color){0, 0, 0, 1};
    }
    
    // Handle hex colors
    if (str[0] == '#') {
        int r, g, b, a = 255;
        size_t len = strlen(str);
        
        if (len == 9) { // #RRGGBBAA
            sscanf(str, "#%02x%02x%02x%02x", &r, &g, &b, &a);
        } else if (len == 7) { // #RRGGBB
            sscanf(str, "#%02x%02x%02x", &r, &g, &b);
        } else {
            return (Color){0, 0, 0, 1}; // Invalid hex format
        }
        
        // Convert from 0-255 to 0.0-1.0
        float rf = r / 255.0f;
        float gf = g / 255.0f;
        float bf = b / 255.0f;
        float af = a / 255.0f;
        
        // Convert sRGB to linear
        rf = (rf <= 0.04045f) ? rf / 12.92f : powf((rf + 0.055f) / 1.055f, 2.4f);
        gf = (gf <= 0.04045f) ? gf / 12.92f : powf((gf + 0.055f) / 1.055f, 2.4f);
        bf = (bf <= 0.04045f) ? bf / 12.92f : powf((bf + 0.055f) / 1.055f, 2.4f);
        
        return (Color){rf, gf, bf, af};
    }
    
    // Named colors - convert to linear space
    // Pure colors first
    if (strcmp(str, "black")   == 0) return (Color){0, 0, 0, 1};
    if (strcmp(str, "white")   == 0) return (Color){1, 1, 1, 1};
    
    // For other named colors, convert sRGB to linear
    float r = 0, g = 0, b = 0;
    if (strcmp(str, "red")     == 0) { r = 1; g = 0; b = 0; }
    else if (strcmp(str, "green")   == 0) { r = 0; g = 1; b = 0; }
    else if (strcmp(str, "blue")    == 0) { r = 0; g = 0; b = 1; }
    else if (strcmp(str, "yellow")  == 0) { r = 1; g = 1; b = 0; }
    else if (strcmp(str, "cyan")    == 0) { r = 0; g = 1; b = 1; }
    else if (strcmp(str, "magenta") == 0) { r = 1; g = 0; b = 1; }
    else return (Color){0, 0, 0, 1}; // Unknown color
    
    // Convert to linear (these are already 0-1, so apply gamma correction)
    r = (r <= 0.04045f) ? r / 12.92f : powf((r + 0.055f) / 1.055f, 2.4f);
    g = (g <= 0.04045f) ? g / 12.92f : powf((g + 0.055f) / 1.055f, 2.4f);
    b = (b <= 0.04045f) ? b / 12.92f : powf((b + 0.055f) / 1.055f, 2.4f);
    
    return (Color){r, g, b, 1};
}

/// Scheme bindings

static char *scm_to_c_string(SCM str) {
    if (scm_is_string(str)) {
        return scm_to_locale_string(str);
    } else if (scm_is_symbol(str)) {
        SCM str_scm = scm_symbol_to_string(str);
        return scm_to_locale_string(str_scm);
    }
    return NULL;
}

static SCM scm_make_face(SCM name) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("make-face", 1, name);
    }
    
    char *face_name = scm_to_c_string(name);
    int id = face_id_from_name(face_name);
    free(face_name);
    
    return id >= 0 ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_set_face_foreground(SCM name, SCM color) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("set-face-foreground", 1, name);
    }
    if (!scm_is_string(color)) {
        scm_wrong_type_arg("set-face-foreground", 2, color);
    }
    
    char *face_name = scm_to_c_string(name);
    Face *face = get_named_face(face_name);
    free(face_name);
    
    if (face) {
        char *color_str = scm_to_locale_string(color);
        face->fg = parse_color(color_str);
        face->fg_set = true;
        free(color_str);
        
        // Resolve inheritance for dependent faces
        resolve_face_inheritance();
        
        return SCM_BOOL_T;
    }
    
    return SCM_BOOL_F;
}

static SCM scm_set_face_background(SCM name, SCM color) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("set-face-background", 1, name);
    }
    if (!scm_is_string(color)) {
        scm_wrong_type_arg("set-face-background", 2, color);
    }
    
    char *face_name = scm_to_c_string(name);
    Face *face = get_named_face(face_name);
    free(face_name);
    
    if (face) {
        char *color_str = scm_to_locale_string(color);
        face->bg = parse_color(color_str);
        face->bg_set = true;
        free(color_str);
        
        // Resolve inheritance for dependent faces
        resolve_face_inheritance();
        
        return SCM_BOOL_T;
    }
    
    return SCM_BOOL_F;
}

static SCM scm_face_foreground(SCM name) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("face-foreground", 1, name);
    }
    
    char *face_name = scm_to_c_string(name);
    Face *face = get_named_face(face_name);
    free(face_name);
    
    if (face) {
        // Return the resolved color (after inheritance)
        Color fg = face->fg;
        
        // Convert back from linear to sRGB for display
        float r = (fg.r <= 0.0031308f) ? fg.r * 12.92f : 1.055f * powf(fg.r, 1.0f/2.4f) - 0.055f;
        float g = (fg.g <= 0.0031308f) ? fg.g * 12.92f : 1.055f * powf(fg.g, 1.0f/2.4f) - 0.055f;
        float b = (fg.b <= 0.0031308f) ? fg.b * 12.92f : 1.055f * powf(fg.b, 1.0f/2.4f) - 0.055f;
        
        char color_str[32];
        snprintf(color_str, sizeof(color_str), "#%02x%02x%02x",
                (int)(r * 255),
                (int)(g * 255),
                (int)(b * 255));
        return scm_from_locale_string(color_str);
    }
    
    return SCM_BOOL_F;
}

static SCM scm_face_background(SCM name) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("face-background", 1, name);
    }
    
    char *face_name = scm_to_c_string(name);
    Face *face = get_named_face(face_name);
    free(face_name);
    
    if (face) {
        // Return the resolved color (after inheritance)
        Color bg = face->bg;
        
        // Convert back from linear to sRGB for display
        float r = (bg.r <= 0.0031308f) ? bg.r * 12.92f : 1.055f * powf(bg.r, 1.0f/2.4f) - 0.055f;
        float g = (bg.g <= 0.0031308f) ? bg.g * 12.92f : 1.055f * powf(bg.g, 1.0f/2.4f) - 0.055f;
        float b = (bg.b <= 0.0031308f) ? bg.b * 12.92f : 1.055f * powf(bg.b, 1.0f/2.4f) - 0.055f;
        
        char color_str[32];
        snprintf(color_str, sizeof(color_str), "#%02x%02x%02x",
                (int)(r * 255),
                (int)(g * 255),
                (int)(b * 255));
        return scm_from_locale_string(color_str);
    }
    
    return SCM_BOOL_F;
}

void init_face_bindings(void) {
    scm_c_define_gsubr("make-face",            1, 0, 0, scm_make_face);
    scm_c_define_gsubr("set-face-foreground!", 2, 0, 0, scm_set_face_foreground);
    scm_c_define_gsubr("set-face-background!", 2, 0, 0, scm_set_face_background);
    scm_c_define_gsubr("face-foreground",      1, 0, 0, scm_face_foreground);
    scm_c_define_gsubr("face-background",      1, 0, 0, scm_face_background);
}

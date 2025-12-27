#pragma once
#include <obsidian/obsidian.h>
#include <stdbool.h>

// Face specification for a theme
typedef struct FaceSpec {
    char *face_name;
    Color fg;
    Color bg;
    bool has_fg;
    bool has_bg;
    bool bold;
    bool italic;
    bool underline;
    Color underline_color;
    bool strike_through;
    Color strike_through_color;
    bool box;
    Color box_color;
    bool has_underline_color;
    bool has_strike_through_color;
    bool has_box_color;
    int inherit_from;
    bool has_inherit;
    struct FaceSpec *next;
} FaceSpec;

// A theme is a collection of face specifications
typedef struct Theme {
    char *name;
    char *description;
    FaceSpec *face_specs;
    struct Theme *next;
} Theme;

typedef struct {
    Theme *themes;
    SCM enabled_themes;  // List of currently enabled theme names
    Color base_bg;       // Background from default face of active theme
    Color base_fg;       // Foreground from default face of active theme
} ThemeCache;

extern ThemeCache *theme_cache;

void init_themes(void);
void free_themes(void);

Theme *get_theme(const char *name);
void register_theme(const char *name, const char *description);
void load_theme(const char *name, bool no_confirm, bool no_enable);
void enable_theme(const char *name);
void disable_theme(const char *name);
void disable_all_themes(void);

static inline bool color_equals(Color a, Color b) {
    return a.r == b.r && a.g == b.g && a.b == b.b && a.a == b.a;
}

void init_theme_bindings(void);

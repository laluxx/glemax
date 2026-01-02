#include "theme.h"
#include "faces.h"
#include <stdlib.h>
#include <string.h>

ThemeCache *theme_cache = NULL;

// Base theme face snapshot - saved at initialization
typedef struct {
    Color fg;
    Color bg;
    bool fg_set;
    bool bg_set;
    bool bold;
    bool italic;
    bool underline;
    bool strike_through;
    bool box;
    Color underline_color;
    Color strike_through_color;
    Color box_color;
    int inherit_from;
} BaseFace;

static BaseFace *base_faces = NULL;

void init_themes(void) {
    if (theme_cache) return;
    
    theme_cache = calloc(1, sizeof(ThemeCache));
    theme_cache->themes = NULL;
    theme_cache->enabled_themes = SCM_EOL;
    
    // Save base theme snapshot
    if (face_cache) {
        base_faces = calloc(face_cache->count, sizeof(BaseFace));
        for (int i = 0; i < face_cache->count; i++) {
            Face *face = face_cache->faces[i];
            if (face) {
                base_faces[i].fg = face->fg;
                base_faces[i].bg = face->bg;
                base_faces[i].fg_set = face->fg_set;
                base_faces[i].bg_set = face->bg_set;
                base_faces[i].bold = face->bold;
                base_faces[i].italic = face->italic;
                base_faces[i].underline = face->underline;
                base_faces[i].strike_through = face->strike_through;
                base_faces[i].box = face->box;
                base_faces[i].underline_color = face->underline_color;
                base_faces[i].strike_through_color = face->strike_through_color;
                base_faces[i].box_color = face->box_color;
                base_faces[i].inherit_from = face->inherit_from;
            }
        }
    }
    
    // Initialize base colors from default face
    Face *default_face = get_face(FACE_DEFAULT);
    if (default_face) {
        theme_cache->base_bg = default_face->bg;
        theme_cache->base_fg = default_face->fg;
    } else {
        // Fallback
        theme_cache->base_bg = (Color){1, 1, 1, 1};
        theme_cache->base_fg = (Color){0, 0, 0, 1};
    }
    
    // Protect enabled_themes from GC
    scm_gc_protect_object(theme_cache->enabled_themes);
}

void free_themes(void) {
    if (!theme_cache) return;
    
    scm_gc_unprotect_object(theme_cache->enabled_themes);
    
    Theme *theme = theme_cache->themes;
    while (theme) {
        Theme *next = theme->next;
        
        // Free face specs
        FaceSpec *spec = theme->face_specs;
        while (spec) {
            FaceSpec *next_spec = spec->next;
            free(spec->face_name);
            free(spec);
            spec = next_spec;
        }
        
        free(theme->name);
        if (theme->description) free(theme->description);
        free(theme);
        theme = next;
    }
    
    free(theme_cache);
    theme_cache = NULL;
    
    if (base_faces) {
        free(base_faces);
        base_faces = NULL;
    }
}

Theme *get_theme(const char *name) {
    if (!theme_cache || !name) return NULL;
    
    Theme *theme = theme_cache->themes;
    while (theme) {
        if (strcmp(theme->name, name) == 0) {
            return theme;
        }
        theme = theme->next;
    }
    
    return NULL;
}

void register_theme(const char *name, const char *description) {
    if (!theme_cache) init_themes();
    
    // Check if theme already exists
    Theme *existing = get_theme(name);
    if (existing) {
        // Clear existing face specs
        FaceSpec *spec = existing->face_specs;
        while (spec) {
            FaceSpec *next = spec->next;
            free(spec->face_name);
            free(spec);
            spec = next;
        }
        existing->face_specs = NULL;
        
        if (existing->description) free(existing->description);
        existing->description = description ? strdup(description) : NULL;
        return;
    }
    
    // Create new theme
    Theme *theme = calloc(1, sizeof(Theme));
    theme->name = strdup(name);
    theme->description = description ? strdup(description) : NULL;
    theme->face_specs = NULL;
    
    // Add to linked list
    theme->next = theme_cache->themes;
    theme_cache->themes = theme;
}

// Reset all faces to base theme, then apply all enabled themes in order
static void reapply_all_themes(void) {
    if (!theme_cache || !face_cache || !base_faces) return;
    
    // Step 1: Reset all faces to base theme
    for (int i = 0; i < face_cache->count; i++) {
        Face *face = face_cache->faces[i];
        if (face) {
            face->fg = base_faces[i].fg;
            face->bg = base_faces[i].bg;
            face->fg_set = base_faces[i].fg_set;
            face->bg_set = base_faces[i].bg_set;
            face->bold = base_faces[i].bold;
            face->italic = base_faces[i].italic;
            face->underline = base_faces[i].underline;
            face->strike_through = base_faces[i].strike_through;
            face->box = base_faces[i].box;
            face->underline_color = base_faces[i].underline_color;
            face->strike_through_color = base_faces[i].strike_through_color;
            face->box_color = base_faces[i].box_color;
            face->inherit_from = base_faces[i].inherit_from;
        }
    }
    
    // Step 2: Apply each enabled theme in order (from oldest to newest)
    // We need to reverse the list since it's stored newest-first
    SCM reversed = SCM_EOL;
    SCM themes = theme_cache->enabled_themes;
    while (!scm_is_null(themes)) {
        reversed = scm_cons(scm_car(themes), reversed);
        themes = scm_cdr(themes);
    }
    
    // Now apply themes from oldest to newest
    while (!scm_is_null(reversed)) {
        SCM theme_name_scm = scm_car(reversed);
        
        // Convert symbol to string
        SCM theme_name_str = scm_symbol_to_string(theme_name_scm);
        char *theme_name = scm_to_locale_string(theme_name_str);
        
        Theme *theme = get_theme(theme_name);
        if (theme) {
            FaceSpec *spec = theme->face_specs;
            while (spec) {
                Face *face = get_named_face(spec->face_name);
                if (face) {
                    // Apply inheritance first (if specified)
                    if (spec->has_inherit) {
                        face->inherit_from = spec->inherit_from;
                        // When inherit is set, clear explicit color flags
                        // so inheritance resolution can work
                        if (!spec->has_fg) {
                            face->fg_set = false;
                        }
                        if (!spec->has_bg) {
                            face->bg_set = false;
                        }
                    }
                    
                    // Then apply explicit properties (these override inheritance)
                    if (spec->has_fg) {
                        face->fg = spec->fg;
                        face->fg_set = true;
                    }
                    if (spec->has_bg) {
                        face->bg = spec->bg;
                        face->bg_set = true;
                    }
                    
                    // Apply text attributes
                    if (spec->bold)
                        face->bold = true;
                    if (spec->italic)
                        face->italic = true;

                    if (spec->underline) {
                        face->underline = true;
                        if (spec->has_underline_color) {
                            face->underline_color = spec->underline_color;
                        }
                    }
                    if (spec->strike_through) {
                        face->strike_through = true;
                        if (spec->has_strike_through_color) {
                            face->strike_through_color = spec->strike_through_color;
                        }
                    }
                    if (spec->box) {
                        face->box = true;
                        if (spec->has_box_color) {
                            face->box_color = spec->box_color;
                        }
                    }

                    if (spec->has_extend) {
                        face->extend = spec->extend;
                    }
                    
                    // UPDATE THE FONT if bold or italic changed
                    if (spec->bold || spec->italic) {
                        face->font = get_font_variant(face->bold, face->italic);
                    }
                }
                spec = spec->next;
            }
        }

        free(theme_name);
        reversed = scm_cdr(reversed);
    }

    // Step 3: Resolve inheritance
    resolve_face_inheritance();
}

#include "minibuf.h"
#include "buffer.h"

bool load_theme(const char *name) {
    char *theme_name = NULL;
    bool should_free = false;
    
    if (name == NULL || *name == '\0') {
        // Interactive mode - prompt user for theme name
        // TODO: This could show a list of available themes
        theme_name = read_from_minibuffer("Load theme: ", NULL);
        should_free = true;
        
        if (!theme_name || !*theme_name) {
            if (theme_name) free(theme_name);
            return false;
        }
    } else {
        // Programmatic mode - use provided name
        theme_name = (char*)name;
    }
    
    if (!theme_cache) init_themes();
    
    Theme *theme = get_theme(theme_name);
    if (!theme) {
        message("Unknown theme: %s", theme_name);
        if (should_free) free(theme_name);
        return false;
    }
    
    // Enable the theme (applies it and tracks it)
    enable_theme(theme_name);
    
    if (should_free) free(theme_name);
    return true;
}

void enable_theme(const char *name) {
    if (!theme_cache) init_themes();
    
    // Use symbol, not string - matches Emacs behavior
    SCM name_scm = scm_from_locale_symbol(name);
    
    // Check if already enabled
    SCM pos = scm_member(name_scm, theme_cache->enabled_themes);
    if (scm_is_true(pos)) {
        return;  // Already enabled
    }
    
    // Add to enabled themes list (newest at front)
    scm_gc_unprotect_object(theme_cache->enabled_themes);
    theme_cache->enabled_themes = scm_cons(name_scm, theme_cache->enabled_themes);
    scm_gc_protect_object(theme_cache->enabled_themes);
    
    // Reapply all themes in correct order
    reapply_all_themes();
    
    // Update base colors from default face
    Face *default_face = get_face(FACE_DEFAULT);
    if (default_face) {
        theme_cache->base_bg = default_face->bg;
        theme_cache->base_fg = default_face->fg;
    }
}

bool disable_theme(const char *name) {
    char *theme_name = NULL;
    bool should_free = false;
    
    if (name == NULL || *name == '\0') {
        // Interactive mode - prompt user for theme name
        theme_name = read_from_minibuffer("Disable theme: ", NULL);
        should_free = true;
        
        if (!theme_name || !*theme_name) {
            if (theme_name) free(theme_name);
            return false;
        }
    } else {
        // Programmatic mode - use provided name
        theme_name = (char*)name;
    }
    
    if (!theme_cache) {
        if (should_free) free(theme_name);
        return false;
    }
    
    // Use symbol to match enable_theme and Emacs behavior
    SCM name_scm = scm_from_locale_symbol(theme_name);
    
    // Check if theme is actually enabled before disabling
    SCM was_enabled = scm_member(name_scm, theme_cache->enabled_themes);
    
    if (scm_is_false(was_enabled)) {
        message("Theme '%s' is not currently enabled", theme_name);
        if (should_free) free(theme_name);
        return false;
    }
    
    scm_gc_unprotect_object(theme_cache->enabled_themes);
    theme_cache->enabled_themes = scm_delete(name_scm, theme_cache->enabled_themes);
    scm_gc_protect_object(theme_cache->enabled_themes);
    
    // Reapply remaining themes (or reset to base if none left)
    reapply_all_themes();
    
    // Update base colors from default face
    Face *default_face = get_face(FACE_DEFAULT);
    if (default_face) {
        theme_cache->base_bg = default_face->bg;
        theme_cache->base_fg = default_face->fg;
    }
    
    message("Disabled theme: %s", theme_name);
    if (should_free) free(theme_name);
    return true;
}

void disable_all_themes(void) {
    if (!theme_cache) return;
    
    scm_gc_unprotect_object(theme_cache->enabled_themes);
    theme_cache->enabled_themes = SCM_EOL;
    scm_gc_protect_object(theme_cache->enabled_themes);
    
    // Reset to base theme
    reapply_all_themes();
    
    // Update base colors from default face
    Face *default_face = get_face(FACE_DEFAULT);
    if (default_face) {
        theme_cache->base_bg = default_face->bg;
        theme_cache->base_fg = default_face->fg;
    }
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

static SCM scm_deftheme(SCM name, SCM description) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("deftheme", 1, name);
    }
    
    char *theme_name = scm_to_c_string(name);
    char *theme_desc = NULL;
    
    if (!SCM_UNBNDP(description) && scm_is_string(description)) {
        theme_desc = scm_to_locale_string(description);
    }
    
    register_theme(theme_name, theme_desc);
    
    free(theme_name);
    if (theme_desc) free(theme_desc);
    
    return SCM_UNSPECIFIED;
}

static SCM scm_custom_theme_set_faces(SCM theme_name, SCM rest) {
    if (!scm_is_string(theme_name) && !scm_is_symbol(theme_name)) {
        scm_wrong_type_arg("custom-theme-set-faces", 1, theme_name);
    }
    
    char *name = scm_to_c_string(theme_name);
    
    Theme *theme = get_theme(name);
    if (!theme) {
        register_theme(name, NULL);
        theme = get_theme(name);
    }
    free(name);
    
    if (!theme) return SCM_BOOL_F;
    
    // Clear existing face specs
    FaceSpec *spec = theme->face_specs;
    while (spec) {
        FaceSpec *next = spec->next;
        free(spec->face_name);
        free(spec);
        spec = next;
    }
    theme->face_specs = NULL;
    
    // Process each face specification
    while (!scm_is_null(rest)) {
        SCM face_spec = scm_car(rest);
        rest = scm_cdr(rest);
        
        if (!scm_is_pair(face_spec)) {
            continue;
        }
        
        // Extract face name
        SCM face_name_scm = scm_car(face_spec);
        char *face_name = scm_to_c_string(face_name_scm);
        if (!face_name) {
            continue;
        }
        
        // Allocate FaceSpec
        FaceSpec *fspec = calloc(1, sizeof(FaceSpec));
        if (!fspec) {
            free(face_name);
            continue;
        }
        
        fspec->face_name = strdup(face_name);
        fspec->has_fg = false;
        fspec->has_bg = false;
        fspec->bold = false;
        fspec->italic = false;
        fspec->underline = false;
        fspec->inherit_from = -1;
        fspec->has_inherit = false;
        fspec->extend = false;
        fspec->has_extend = false;

        // Navigate: (face-name ((t (:foreground ...))))
        SCM rest_of_spec = scm_cdr(face_spec);
        
        if (scm_is_pair(rest_of_spec)) {
            SCM outer_list = scm_car(rest_of_spec);
            
            if (scm_is_pair(outer_list)) {
                SCM display_spec = scm_car(outer_list);
                
                if (scm_is_pair(display_spec)) {
                    SCM properties_list = scm_cdr(display_spec);
                    
                    if (scm_is_pair(properties_list)) {
                        SCM plist = scm_car(properties_list);
                        
                        // Parse property list
                        while (scm_is_pair(plist)) {
                            SCM key = scm_car(plist);
                            plist = scm_cdr(plist);
                            
                            if (!scm_is_pair(plist)) {
                                break;
                            }
                            
                            SCM value = scm_car(plist);
                            plist = scm_cdr(plist);
                            
                            // Extract key name
                            char *key_name = NULL;
                            
                            if (scm_is_keyword(key)) {
                                SCM key_sym = scm_keyword_to_symbol(key);
                                SCM key_str = scm_symbol_to_string(key_sym);
                                key_name = scm_to_locale_string(key_str);
                            } else if (scm_is_symbol(key)) {
                                SCM key_str = scm_symbol_to_string(key);
                                char *raw_name = scm_to_locale_string(key_str);
                                
                                if (raw_name && raw_name[0] == ':') {
                                    key_name = strdup(raw_name + 1);
                                    free(raw_name);
                                } else {
                                    key_name = raw_name;
                                }
                            }
                            
                            if (!key_name) {
                                continue;
                            }
                            
                            // Handle properties
                            if (strcmp(key_name, "foreground") == 0) {
                                if (scm_is_string(value)) {
                                    char *color_str = scm_to_locale_string(value);
                                    fspec->fg = parse_color(color_str);
                                    fspec->has_fg = true;
                                    free(color_str);
                                }
                            }
                            else if (strcmp(key_name, "background") == 0) {
                                if (scm_is_string(value)) {
                                    char *color_str = scm_to_locale_string(value);
                                    fspec->bg = parse_color(color_str);
                                    fspec->has_bg = true;
                                    free(color_str);
                                }
                            }
                            else if (strcmp(key_name, "weight") == 0) {
                                if (scm_is_symbol(value)) {
                                    SCM val_str = scm_symbol_to_string(value);
                                    char *weight = scm_to_locale_string(val_str);
                                    if (strcmp(weight, "bold") == 0) {
                                        fspec->bold = true;
                                    }
                                    free(weight);
                                }
                            }
                            else if (strcmp(key_name, "slant") == 0) {
                                if (scm_is_symbol(value)) {
                                    SCM val_str = scm_symbol_to_string(value);
                                    char *slant = scm_to_locale_string(val_str);
                                    if (strcmp(slant, "italic") == 0) {
                                        fspec->italic = true;
                                    }
                                    free(slant);
                                }
                            }
                            /* else if (strcmp(key_name, "underline") == 0) { */
                            /*     fspec->underline = scm_is_true(value); */
                            /* } */
                            else if (strcmp(key_name, "underline") == 0) {
                                if (scm_is_true(value)) {
                                    fspec->underline = true;
                                    if (scm_is_string(value)) {
                                        // :underline "green"
                                        char *color_str = scm_to_locale_string(value);
                                        fspec->underline_color = parse_color(color_str);
                                        fspec->has_underline_color = true;
                                        free(color_str);
                                    }
                                }
                            }
                            else if (strcmp(key_name, "strike-through") == 0) {
                                if (scm_is_true(value)) {
                                    fspec->strike_through = true;
                                    if (scm_is_string(value)) {
                                        // :strike-through "maroon"
                                        char *color_str = scm_to_locale_string(value);
                                        fspec->strike_through_color = parse_color(color_str);
                                        fspec->has_strike_through_color = true;
                                        free(color_str);
                                    }
                                }
                            }
                            else if (strcmp(key_name, "box") == 0) {
                                if (scm_is_true(value)) {
                                    fspec->box = true;
                                    if (scm_is_string(value)) {
                                        // :box "blue"
                                        char *color_str = scm_to_locale_string(value);
                                        fspec->box_color = parse_color(color_str);
                                        fspec->has_box_color = true;
                                        free(color_str);
                                    }
                                }
                            }


                            else if (strcmp(key_name, "inherit") == 0) {
                                if (scm_is_symbol(value) || scm_is_string(value)) {
                                    char *inherit_name = scm_to_c_string(value);
                                    int inherit_id = face_id_from_name(inherit_name);
                                    if (inherit_id >= 0) {
                                        fspec->inherit_from = inherit_id;
                                        fspec->has_inherit = true;
                                    }
                                    free(inherit_name);
                                }
                            }

                            else if (strcmp(key_name, "extend") == 0) {
                                fspec->extend = scm_is_true(value);
                                fspec->has_extend = true;
                            }
                            
                            free(key_name);
                        }
                    }
                }
            }
        }
        
        // Add to theme
        fspec->next = theme->face_specs;
        theme->face_specs = fspec;
        
        free(face_name);
    }
    
    return SCM_UNSPECIFIED;
}

static SCM scm_load_theme(SCM name) {
    char *theme_name = NULL;
    
    // Check if name argument was provided
    if (!SCM_UNBNDP(name)) {
        if (!scm_is_symbol(name)) {
            scm_wrong_type_arg("load-theme", 1, name);
        }
        theme_name = scm_to_locale_string(scm_symbol_to_string(name));
    }
    
    bool success = load_theme(theme_name);
    
    if (theme_name) free(theme_name);
    
    return scm_from_bool(success);
}

static SCM scm_enable_theme(SCM name) {
    if (!scm_is_string(name) && !scm_is_symbol(name)) {
        scm_wrong_type_arg("enable-theme", 1, name);
    }
    
    char *theme_name = scm_to_c_string(name);
    enable_theme(theme_name);
    free(theme_name);
    
    return SCM_UNSPECIFIED;
}

static SCM scm_disable_theme(SCM name) {
    char *theme_name = NULL;
    
    // Check if name argument was provided
    if (!SCM_UNBNDP(name)) {
        // Only accept symbols
        if (!scm_is_symbol(name)) {
            scm_wrong_type_arg("disable-theme", 1, name);
        }
        // Convert symbol to string
        SCM name_str = scm_symbol_to_string(name);
        theme_name = scm_to_locale_string(name_str);
    }
    
    bool success = disable_theme(theme_name);
    
    if (theme_name) free(theme_name);
    
    return scm_from_bool(success);
}

static SCM scm_custom_available_themes(void) {
    if (!theme_cache) return SCM_EOL;
    
    SCM result = SCM_EOL;
    Theme *theme = theme_cache->themes;
    
    while (theme) {
        SCM name = scm_from_locale_symbol(theme->name);
        result = scm_cons(name, result);
        theme = theme->next;
    }
    
    return result;
}

static SCM scm_custom_enabled_themes(void) {
    if (!theme_cache) return SCM_EOL;
    return theme_cache->enabled_themes;
}

void init_theme_bindings(void) {
    scm_c_define_gsubr("deftheme",                1, 1, 0, scm_deftheme);
    scm_c_define_gsubr("custom-theme-set-faces",  1, 0, 1, scm_custom_theme_set_faces);
    scm_c_define_gsubr("load-theme",              0, 1, 0, scm_load_theme);
    scm_c_define_gsubr("enable-theme",            1, 0, 0, scm_enable_theme);
    scm_c_define_gsubr("disable-theme",           0, 1, 0, scm_disable_theme);
    scm_c_define_gsubr("custom-available-themes", 0, 0, 0, scm_custom_available_themes);
    scm_c_define_gsubr("custom-enabled-themes",   0, 0, 0, scm_custom_enabled_themes);
}

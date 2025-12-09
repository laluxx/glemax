#include "textprop.h"
#include "buffer.h"
#include "faces.h"
#include <stdlib.h>
#include <stdio.h>

// Helper: Find a text property interval containing pos
static TextProp* find_interval(Buffer *buf, size_t pos) {
    if (!buf) return NULL;
    
    TextProp *prop = buf->props;
    while (prop) {
        if (pos >= prop->start && pos < prop->end) {
            return prop;
        }
        prop = prop->next;
    }
    return NULL;
}

// Helper: Get a specific property value from a property list
static SCM get_prop_value(PropValue *props, SCM key) {
    while (props) {
        if (scm_is_eq(props->key, key)) {
            return props->value;
        }
        props = props->next;
    }
    return SCM_BOOL_F;
}


// Helper: Set a property value in a property list (or add it)
static PropValue* set_prop_value(PropValue *props, SCM key, SCM value) {
    PropValue **curr = &props;
    
    // Find existing property
    while (*curr) {
        if (scm_is_eq((*curr)->key, key)) {
            // Protect the new value from GC
            scm_gc_protect_object(value);
            // Unprotect the old value
            scm_gc_unprotect_object((*curr)->value);
            (*curr)->value = value;
            return props;
        }
        curr = &(*curr)->next;
    }
    
    // Add new property
    PropValue *new_prop = malloc(sizeof(PropValue));
    if (!new_prop) return props;
    
    // Protect both key and value from GC
    scm_gc_protect_object(key);
    scm_gc_protect_object(value);
    
    new_prop->key = key;
    new_prop->value = value;
    new_prop->next = props;
    
    return new_prop;
}

// Helper: Free property list
static void free_prop_list(PropValue *props) {
    while (props) {
        PropValue *next = props->next;
        
        // Unprotect from GC before freeing
        scm_gc_unprotect_object(props->key);
        scm_gc_unprotect_object(props->value);
        
        free(props);
        props = next;
    }
}

// Helper: Copy property list
static PropValue* copy_prop_list(PropValue *props) {
    PropValue *new_list = NULL;
    PropValue **tail = &new_list;
    
    while (props) {
        PropValue *new_prop = malloc(sizeof(PropValue));
        if (!new_prop) break;
        
        // Protect the copied values from GC
        scm_gc_protect_object(props->key);
        scm_gc_protect_object(props->value);
        
        new_prop->key = props->key;
        new_prop->value = props->value;
        new_prop->next = NULL;
        
        *tail = new_prop;
        tail = &new_prop->next;
        
        props = props->next;
    }
    
    return new_list;
}

void put_text_property(Buffer *buf, size_t start, size_t end, SCM prop, SCM value) {
    if (!buf || start >= end) return;
    
    // Find all overlapping intervals
    TextProp **curr = &buf->props;
    
    while (*curr) {
        TextProp *interval = *curr;
        
        // No overlap - skip
        if (interval->end <= start || interval->start >= end) {
            curr = &interval->next;
            continue;
        }
        
        // Exact match - just update property
        if (interval->start == start && interval->end == end) {
            interval->props = set_prop_value(interval->props, prop, value);
            return;
        }
        
        // Split or adjust as needed
        
        // Interval completely inside [start, end] - update property
        if (interval->start >= start && interval->end <= end) {
            interval->props = set_prop_value(interval->props, prop, value);
            curr = &interval->next;
            continue;
        }
        
        // Interval spans [start, end] - split into three
        if (interval->start < start && interval->end > end) {
            TextProp *middle = malloc(sizeof(TextProp));
            TextProp *right = malloc(sizeof(TextProp));
            if (!middle || !right) {
                free(middle);
                free(right);
                return;
            }
            
            middle->start = start;
            middle->end = end;
            middle->props = copy_prop_list(interval->props);
            middle->props = set_prop_value(middle->props, prop, value);
            
            right->start = end;
            right->end = interval->end;
            right->props = copy_prop_list(interval->props);
            
            interval->end = start;
            middle->next = right;
            right->next = interval->next;
            interval->next = middle;
            
            return;
        }
        
        // Partial overlap at start
        if (interval->start < start && interval->end > start) {
            TextProp *right = malloc(sizeof(TextProp));
            if (!right) return;
            
            right->start = start;
            right->end = interval->end;
            right->props = copy_prop_list(interval->props);
            right->props = set_prop_value(right->props, prop, value);
            
            interval->end = start;
            right->next = interval->next;
            interval->next = right;
            
            curr = &right->next;
            continue;
        }
        
        // Partial overlap at end
        if (interval->start < end && interval->end > end) {
            TextProp *right = malloc(sizeof(TextProp));
            if (!right) return;
            
            right->start = end;
            right->end = interval->end;
            right->props = copy_prop_list(interval->props);
            
            interval->end = end;
            interval->props = set_prop_value(interval->props, prop, value);
            
            right->next = interval->next;
            interval->next = right;
            
            return;
        }
        
        curr = &interval->next;
    }
    
    // No existing interval covers this range - create new one
    TextProp *new_interval = malloc(sizeof(TextProp));
    if (!new_interval) return;
    
    new_interval->start = start;
    new_interval->end = end;
    new_interval->props = NULL;
    new_interval->props = set_prop_value(new_interval->props, prop, value);
    new_interval->next = buf->props;
    buf->props = new_interval;
}

SCM get_text_property(Buffer *buf, size_t pos, SCM prop) {
    TextProp *interval = find_interval(buf, pos);
    if (!interval) return SCM_BOOL_F;
    
    return get_prop_value(interval->props, prop);
}

int get_text_property_face(Buffer *buf, size_t pos) {
    if (!buf) return FACE_DEFAULT;
    
    SCM face = get_text_property(buf, pos, scm_from_locale_symbol("face"));
    
    // No face property set
    if (scm_is_false(face)) {
        return FACE_DEFAULT;
    }
    
    // Face is an integer ID
    if (scm_is_integer(face)) {
        return scm_to_int(face);
    }
    
    // Face is a symbol or string name
    if (scm_is_symbol(face) || scm_is_string(face)) {
        char *face_name;
        if (scm_is_string(face)) {
            face_name = scm_to_locale_string(face);
        } else {
            SCM str = scm_symbol_to_string(face);
            face_name = scm_to_locale_string(str);
        }
        int face_id = face_id_from_name(face_name);
        free(face_name);
        return (face_id < 0) ? FACE_DEFAULT : face_id;
    }
    
    return FACE_DEFAULT;
}

SCM get_text_property_field(Buffer *buf, size_t pos) {
    if (!buf) return SCM_BOOL_F;
    return get_text_property(buf, pos, scm_from_locale_symbol("field"));
}

void remove_text_properties(Buffer *buf, size_t start, size_t end) {
    if (!buf) return;
    
    TextProp **curr = &buf->props;
    while (*curr) {
        TextProp *prop = *curr;
        
        // Complete overlap - remove
        if (prop->start >= start && prop->end <= end) {
            *curr = prop->next;
            free_prop_list(prop->props);
            free(prop);
            continue;
        }
        
        // Property spans the removal region - split
        if (prop->start < start && prop->end > end) {
            TextProp *new_prop = malloc(sizeof(TextProp));
            if (new_prop) {
                new_prop->start = end;
                new_prop->end = prop->end;
                new_prop->props = copy_prop_list(prop->props);
                new_prop->next = prop->next;
                prop->end = start;
                prop->next = new_prop;
                curr = &new_prop->next;
            }
            continue;
        }
        
        // Partial overlap - truncate
        if (prop->start >= start && prop->start < end && prop->end > end) {
            prop->start = end;
        }
        
        if (prop->start < start && prop->end > start && prop->end <= end) {
            prop->end = start;
        }
        
        curr = &prop->next;
    }
}

void clear_text_properties(Buffer *buf) {
    if (!buf) return;
    
    TextProp *prop = buf->props;
    while (prop) {
        TextProp *next = prop->next;
        free_prop_list(prop->props);
        free(prop);
        prop = next;
    }
    buf->props = NULL;
}

void adjust_text_properties(Buffer *buf, size_t pos, int delta) {
    if (!buf || delta == 0) return;
    
    TextProp **curr = &buf->props;
    while (*curr) {
        TextProp *prop = *curr;
        
        if (delta > 0) {
            if (prop->start >= pos) {
                prop->start += delta;
            }
            if (prop->end > pos) {
                prop->end += delta;
            }
        } else {
            size_t abs_delta = -delta;
            size_t delete_end = pos + abs_delta;
            
            if (prop->end <= pos) {
                curr = &prop->next;
                continue;
            }
            
            if (prop->start >= delete_end) {
                prop->start -= abs_delta;
                prop->end -= abs_delta;
                curr = &prop->next;
                continue;
            }
            
            if (prop->start >= pos && prop->end <= delete_end) {
                *curr = prop->next;
                free_prop_list(prop->props);
                free(prop);
                continue;
            }
            
            if (prop->start < pos && prop->end > delete_end) {
                prop->end -= abs_delta;
                curr = &prop->next;
                continue;
            }
            
            if (prop->start < pos && prop->end > pos && prop->end <= delete_end) {
                prop->end = pos;
                curr = &prop->next;
                continue;
            }
            
            if (prop->start >= pos && prop->start < delete_end && prop->end > delete_end) {
                prop->start = pos;
                prop->end = pos + (prop->end - delete_end);
                curr = &prop->next;
                continue;
            }
        }
        
        curr = &prop->next;
    }
}

// More efficient version - check intervals instead of every character
bool is_range_readonly(Buffer *buf, size_t start, size_t end) {
    if (!buf) return false;
    
    TextProp *prop = buf->props;
    SCM readonly_sym = scm_from_locale_symbol("read-only");
    
    while (prop) {
        // Check if this interval overlaps with [start, end)
        if (prop->start < end && prop->end > start) {
            // Check if it has read-only property set to true
            SCM readonly = get_prop_value(prop->props, readonly_sym);
            if (scm_is_true(readonly)) {
                return true;
            }
        }
        prop = prop->next;
    }
    
    return false;
}

void debug_text_properties(Buffer *buf) {
    if (!buf) {
        printf("DEBUG: Buffer is NULL\n");
        return;
    }
    
    if (!buf->props) {
        printf("DEBUG: No text properties\n");
        return;
    }
    
    printf("=== Text Properties Debug ===\n");
    
    int interval_count = 0;
    TextProp *prop = buf->props;
    
    while (prop) {
        interval_count++;
        printf("Interval #%d: [%zu, %zu)\n", interval_count, prop->start, prop->end);
        
        if (!prop->props) {
            printf("  (no properties)\n");
        } else {
            PropValue *pv = prop->props;
            while (pv) {
                printf("  ");
                
                // Print key
                if (scm_is_symbol(pv->key)) {
                    char *key_str = scm_to_locale_string(scm_symbol_to_string(pv->key));
                    printf("%s: ", key_str);
                    free(key_str);
                } else if (scm_is_string(pv->key)) {
                    char *key_str = scm_to_locale_string(pv->key);
                    printf("\"%s\": ", key_str);
                    free(key_str);
                } else {
                    printf("(key): ");
                }
                
                // Print value
                if (scm_is_integer(pv->value)) {
                    printf("%ld", scm_to_long(pv->value));
                } else if (scm_is_bool(pv->value)) {
                    printf("%s", scm_is_true(pv->value) ? "#t" : "#f");
                } else if (scm_is_symbol(pv->value)) {
                    char *val_str = scm_to_locale_string(scm_symbol_to_string(pv->value));
                    printf("'%s", val_str);
                    free(val_str);
                } else if (scm_is_string(pv->value)) {
                    char *val_str = scm_to_locale_string(pv->value);
                    printf("\"%s\"", val_str);
                    free(val_str);
                } else if (scm_is_real(pv->value)) {
                    printf("%g", scm_to_double(pv->value));
                } else {
                    // For other types, use Guile's display
                    printf("<");
                    char *type_str = scm_to_locale_string(
                        scm_symbol_to_string(
                            scm_class_name(scm_class_of(pv->value))
                        )
                    );
                    printf("%s", type_str);
                    free(type_str);
                    printf(">");
                }
                
                printf("\n");
                pv = pv->next;
            }
        }
        
        prop = prop->next;
    }
    
    printf("=== Total intervals: %d ===\n\n", interval_count);
}

// Scheme bindings

static SCM scm_put_text_property(SCM start_scm, SCM end_scm, SCM prop_name, SCM value) {
    if (!scm_is_integer(start_scm)) {
        scm_wrong_type_arg("put-text-property", 1, start_scm);
    }
    if (!scm_is_integer(end_scm)) {
        scm_wrong_type_arg("put-text-property", 2, end_scm);
    }
    
    size_t start = scm_to_size_t(start_scm);
    size_t end = scm_to_size_t(end_scm);
    
    if (current_buffer) {
        put_text_property(current_buffer, start, end, prop_name, value);
    }
    
    return SCM_UNSPECIFIED;
}

static SCM scm_get_text_property(SCM pos_scm, SCM prop_name) {
    if (!scm_is_integer(pos_scm)) {
        scm_wrong_type_arg("get-text-property", 1, pos_scm);
    }
    
    size_t pos = scm_to_size_t(pos_scm);
    
    if (current_buffer) {
        return get_text_property(current_buffer, pos, prop_name);
    }
    
    return SCM_BOOL_F;
}

static SCM scm_remove_text_properties(SCM start_scm, SCM end_scm, SCM props) {
    (void)props; // TODO: implement selective removal
    
    if (!scm_is_integer(start_scm)) {
        scm_wrong_type_arg("remove-text-properties", 1, start_scm);
    }
    if (!scm_is_integer(end_scm)) {
        scm_wrong_type_arg("remove-text-properties", 2, end_scm);
    }
    
    size_t start = scm_to_size_t(start_scm);
    size_t end = scm_to_size_t(end_scm);
    
    if (current_buffer) {
        remove_text_properties(current_buffer, start, end);
    }
    
    return SCM_BOOL_T;
}

static SCM scm_debug_text_properties(void) {
    if (current_buffer) {
        debug_text_properties(current_buffer);
    } else {
        printf("DEBUG: No current buffer\n");
    }
    return SCM_UNSPECIFIED;
}

void init_textprop_bindings(void) {
    scm_c_define_gsubr("put-text-property",      4, 0, 0, scm_put_text_property);
    scm_c_define_gsubr("get-text-property",      2, 0, 0, scm_get_text_property);
    scm_c_define_gsubr("remove-text-properties", 3, 0, 0, scm_remove_text_properties);
    scm_c_define_gsubr("debug-text-properties",  0, 0, 0, scm_debug_text_properties);
}


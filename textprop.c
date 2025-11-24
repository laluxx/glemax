#include "textprop.h"
#include "buffer.h"
#include "faces.h"
#include <stdlib.h>
#include <stdio.h>

void put_text_property(Buffer *buf, size_t start, size_t end, int face_id) {
    if (!buf || start >= end) return;
    
    // Remove any overlapping properties first
    remove_text_properties(buf, start, end);
    
    // Create new property
    TextProp *prop = malloc(sizeof(TextProp));
    if (!prop) return;
    
    prop->start = start;
    prop->end = end;
    prop->face_id = face_id;
    prop->next = buf->props;
    buf->props = prop;
}

int get_text_property_face(Buffer *buf, size_t pos) {
    if (!buf) return FACE_DEFAULT;
    
    TextProp *prop = buf->props;
    while (prop) {
        if (pos >= prop->start && pos < prop->end) {
            return prop->face_id;
        }
        prop = prop->next;
    }
    
    return FACE_DEFAULT;
}

void remove_text_properties(Buffer *buf, size_t start, size_t end) {
    if (!buf) return;
    
    TextProp **curr = &buf->props;
    while (*curr) {
        TextProp *prop = *curr;
        
        // Complete overlap - remove
        if (prop->start >= start && prop->end <= end) {
            *curr = prop->next;
            free(prop);
            continue;
        }
        
        // Property spans the removal region - split
        if (prop->start < start && prop->end > end) {
            TextProp *new_prop = malloc(sizeof(TextProp));
            if (new_prop) {
                new_prop->start = end;
                new_prop->end = prop->end;
                new_prop->face_id = prop->face_id;
                new_prop->next = prop->next;
                prop->end = start;
                prop->next = new_prop;
                curr = &new_prop->next;
            }
            continue;
        }
        
        // Partial overlap - truncate start
        if (prop->start >= start && prop->start < end && prop->end > end) {
            prop->start = end;
        }
        
        // Partial overlap - truncate end
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
            // Insertion: shift properties after insertion point
            if (prop->start >= pos) {
                prop->start += delta;
            }
            if (prop->end > pos) {
                prop->end += delta;
            }
        } else {
            // Deletion
            size_t abs_delta = -delta;
            size_t delete_end = pos + abs_delta;
            
            // Property completely before deletion
            if (prop->end <= pos) {
                curr = &prop->next;
                continue;
            }
            
            // Property completely after deletion
            if (prop->start >= delete_end) {
                prop->start -= abs_delta;
                prop->end -= abs_delta;
                curr = &prop->next;
                continue;
            }
            
            // Property overlaps deletion - handle cases
            
            // Completely inside deletion - remove
            if (prop->start >= pos && prop->end <= delete_end) {
                *curr = prop->next;
                free(prop);
                continue;
            }
            
            // Spans deletion - shrink
            if (prop->start < pos && prop->end > delete_end) {
                prop->end -= abs_delta;
                curr = &prop->next;
                continue;
            }
            
            // Starts before, ends inside deletion
            if (prop->start < pos && prop->end > pos && prop->end <= delete_end) {
                prop->end = pos;
                curr = &prop->next;
                continue;
            }
            
            // Starts inside deletion, ends after
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
    
    // Only handle 'face property for now
    if (scm_is_eq(prop_name, scm_from_locale_symbol("face"))) {
        int face_id = FACE_DEFAULT;
        
        if (scm_is_integer(value)) {
            face_id = scm_to_int(value);
        } else if (scm_is_string(value) || scm_is_symbol(value)) {
            char *face_name;
            if (scm_is_string(value)) {
                face_name = scm_to_locale_string(value);
            } else {
                SCM str = scm_symbol_to_string(value);
                face_name = scm_to_locale_string(str);
            }
            face_id = face_id_from_name(face_name);
            if (face_id < 0) face_id = FACE_DEFAULT;
            free(face_name);
        }
        
        if (current_buffer) {
            put_text_property(current_buffer, start, end, face_id);
        }
    }
    
    return SCM_UNSPECIFIED;
}

static SCM scm_get_text_property(SCM pos_scm, SCM prop_name) {
    if (!scm_is_integer(pos_scm)) {
        scm_wrong_type_arg("get-text-property", 1, pos_scm);
    }
    
    size_t pos = scm_to_size_t(pos_scm);
    
    if (scm_is_eq(prop_name, scm_from_locale_symbol("face"))) {
        if (current_buffer) {
            int face_id = get_text_property_face(current_buffer, pos);
            return scm_from_int(face_id);
        }
    }
    
    return SCM_BOOL_F;
}

static SCM scm_remove_text_properties(SCM start_scm, SCM end_scm, SCM props) {
    (void)props; // Unused for now
    
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

void init_textprop_bindings(void) {
    scm_c_define_gsubr("put-text-property",      4, 0, 0, scm_put_text_property);
    scm_c_define_gsubr("get-text-property",      2, 0, 0, scm_get_text_property);
    scm_c_define_gsubr("remove-text-properties", 3, 0, 0, scm_remove_text_properties);
}

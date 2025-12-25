#include "frame.h"
#include "lisp.h"
#include <obsidian/window.h>
#include <libguile.h>

static SCM frame_type;
static SCM frame_object_cache;

// NOTE Currently we only support one frame
Frame *selected_frame = NULL;

static SCM frame_to_scm(Frame *frame) {
    if (!frame) return SCM_BOOL_F;
    
    SCM key = scm_from_uintptr_t((uintptr_t)frame);
    SCM cached = scm_hashq_ref(frame_object_cache, key, SCM_BOOL_F);
    
    if (scm_is_true(cached)) {
        return cached;
    }
    
    SCM obj = scm_make_foreign_object_1(frame_type, frame);
    scm_hashq_set_x(frame_object_cache, key, obj);
    return obj;
}

static Frame* scm_to_frame(SCM obj) {
    scm_assert_foreign_object_type(frame_type, obj);
    return scm_foreign_object_ref(obj, 0);
}

static SCM frame_p(SCM obj) {
    return scm_from_bool(SCM_IS_A_P(obj, frame_type));
}

void update_frame_resize_mode(Frame *frame) {
    if (!frame) return;
    
    bool resize_pixelwise = scm_get_bool("frame-resize-pixelwise", false);
    
    if (resize_pixelwise) {
        // Pixelwise: set increments to 1
        setWindowResizeIncrements(1, 1, 0, 0);
    } else {
        // Charwise: use actual character dimensions
        setWindowResizeIncrements(
            frame->column_width, 
            frame->line_height, 
            frame->left_fringe_width + frame->right_fringe_width, 
            0
        );
    }
}

// NOTE This won't work on Wayland (Future of Desktop btw)
void set_frame_position(Frame *frame, int x, int y) {
    frame->x = x;
    frame->y = y;
    setWindowPos(context.window, x, y);
}


void debug_print_frame(Frame *frame) {
    printf("Frame {\n");
    printf("  x:       %d\n", frame->x);
    printf("  y:       %d\n", frame->y);
    printf("  width:   %d\n", frame->width);
    printf("  height:  %d\n", frame->height);
    printf("  focused: %s\n", frame->focused ? "true" : "false");
    printf("}\n");
}

size_t frame_char_height(Frame *frame) {
    return frame->line_height;
}

size_t frame_char_width(Frame *frame) {
    return frame->column_width;
}


// SCM

static SCM scm_debug_print_frame(SCM frame_obj) {
    Frame *frame;
    
    if (SCM_UNBNDP(frame_obj)) {
        frame = selected_frame;
    } else {
        if (!scm_is_true(frame_p(frame_obj))) {
            scm_wrong_type_arg("debug-print-frame", 1, frame_obj);
        }
        frame = scm_to_frame(frame_obj);
    }
    
    if (!frame) return SCM_BOOL_F;
    
    debug_print_frame(frame);
    return SCM_UNSPECIFIED;
}

static SCM scm_set_frame_position(SCM frame_obj, SCM x_scm, SCM y_scm) {
    Frame *frame;
    
    // If no frame specified, use selected frame
    if (SCM_UNBNDP(frame_obj)) {
        frame = selected_frame;
    } else {
        if (!scm_is_true(frame_p(frame_obj))) {
            scm_wrong_type_arg("set-frame-position", 1, frame_obj);
        }
        frame = scm_to_frame(frame_obj);
    }
    
    if (!frame) return SCM_BOOL_F;
    
    int x = scm_to_int(x_scm);
    int y = scm_to_int(y_scm);
    set_frame_position(frame, x, y);
    return SCM_BOOL_T;
}

static SCM scm_selected_frame(void) {
    return frame_to_scm(selected_frame);
}

static SCM scm_frame_focused_p(SCM frame_obj) {
    Frame *frame;
    
    if (SCM_UNBNDP(frame_obj)) {
        frame = selected_frame;
    } else {
        if (!scm_is_true(frame_p(frame_obj))) {
            scm_wrong_type_arg("frame-focused?", 1, frame_obj);
        }
        frame = scm_to_frame(frame_obj);
    }
    
    if (!frame) return SCM_BOOL_F;
    return scm_from_bool(frame->focused);
}

static SCM scm_frame_char_height(SCM frame_obj) {
    Frame *frame;
    
    if (SCM_UNBNDP(frame_obj)) {
        frame = selected_frame;
    } else {
        if (!scm_is_true(frame_p(frame_obj))) {
            scm_wrong_type_arg("frame-char-height", 1, frame_obj);
        }
        frame = scm_to_frame(frame_obj);
    }
    
    if (!frame) return SCM_BOOL_F;
    
    return scm_from_size_t(frame_char_height(frame));
}

static SCM scm_frame_char_width(SCM frame_obj) {
    Frame *frame;
    
    if (SCM_UNBNDP(frame_obj)) {
        frame = selected_frame;
    } else {
        if (!scm_is_true(frame_p(frame_obj))) {
            scm_wrong_type_arg("frame-char-width", 1, frame_obj);
        }
        frame = scm_to_frame(frame_obj);
    }
    
    if (!frame) return SCM_BOOL_F;
    
    return scm_from_size_t(frame_char_width(frame));
}

void init_frame_bindings(void) {
    // Initialize frame foreign object type
    SCM name = scm_from_utf8_symbol("frame");
    SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
    frame_type = scm_make_foreign_object_type(name, slots, NULL);
    
    // Initialize frame object cache
    frame_object_cache = scm_make_hash_table(scm_from_int(4));
    scm_gc_protect_object(frame_object_cache);

    // Register Scheme functions
    scm_c_define_gsubr("frame?",             1, 0, 0, frame_p);
    scm_c_define_gsubr("selected-frame",     0, 0, 0, scm_selected_frame);
    scm_c_define_gsubr("frame-focused?",     0, 1, 0, scm_frame_focused_p);
    scm_c_define_gsubr("set-frame-position", 2, 1, 0, scm_set_frame_position);
    scm_c_define_gsubr("frame-char-height",  0, 1, 0, scm_frame_char_height);
    scm_c_define_gsubr("frame-char-width",   0, 1, 0, scm_frame_char_width);
    scm_c_define_gsubr("debug-print-frame",  0, 1, 0, scm_debug_print_frame);
}

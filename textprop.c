#include "textprop.h"
#include "buffer.h"
#include "faces.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/// PropValue helpers

static SCM prop_get(PropValue *pv, SCM key) {
    while (pv) {
        if (scm_is_eq(pv->key, key) || scm_is_true(scm_equal_p(pv->key, key)))
            return pv->value;
        pv = pv->next;
    }
    return SCM_BOOL_F;
}

static PropValue *prop_set(PropValue *pv, SCM key, SCM value) {
    for (PropValue *p = pv; p; p = p->next) {
        if (scm_is_eq(p->key, key) || scm_is_true(scm_equal_p(p->key, key))) {
            scm_gc_protect_object(value);
            scm_gc_unprotect_object(p->value);
            p->value = value;
            return pv;
        }
    }
    PropValue *n = malloc(sizeof(PropValue));
    if (!n) return pv;
    scm_gc_protect_object(key);
    scm_gc_protect_object(value);
    n->key   = key;
    n->value = value;
    n->next  = pv;
    return n;
}

static void prop_free(PropValue *pv) {
    while (pv) {
        PropValue *next = pv->next;
        scm_gc_unprotect_object(pv->key);
        scm_gc_unprotect_object(pv->value);
        free(pv);
        pv = next;
    }
}

static PropValue *prop_copy(PropValue *pv) {
    PropValue *head = NULL, **tail = &head;
    while (pv) {
        PropValue *n = malloc(sizeof(PropValue));
        if (!n) break;
        scm_gc_protect_object(pv->key);
        scm_gc_protect_object(pv->value);
        n->key   = pv->key;
        n->value = pv->value;
        n->next  = NULL;
        *tail = n;
        tail  = &n->next;
        pv    = pv->next;
    }
    return head;
}


static PropValue *prop_copy_local(PropValue *pv) {
    PropValue *head = NULL, **tail = &head;
    while (pv) {
        PropValue *n = malloc(sizeof(PropValue));
        if (!n) break;
        // Do NOT call scm_gc_protect_object — the live tree node still owns
        // these SCM values and they are already protected there.
        n->key   = pv->key;
        n->value = pv->value;
        n->next  = NULL;
        *tail = n;
        tail  = &n->next;
        pv    = pv->next;
    }
    return head;
}

static void prop_free_local(PropValue *pv) {
    while (pv) {
        PropValue *next = pv->next;
        // Do NOT call scm_gc_unprotect_object — we never protected these.
        free(pv);
        pv = next;
    }
}


/// AVL interval tree
// nodes sorted by start, augmented with max_end

static inline int node_height(ITreeNode *n) {
    return n ? n->height : 0;
}

static inline size_t node_max_end(ITreeNode *n) {
    return n ? n->max_end : 0;
}

static inline void node_update(ITreeNode *n) {
    if (!n) return;
    int lh = node_height(n->left);
    int rh = node_height(n->right);
    n->height  = 1 + (lh > rh ? lh : rh);
    n->max_end = n->end;
    if (node_max_end(n->left)  > n->max_end) n->max_end = node_max_end(n->left);
    if (node_max_end(n->right) > n->max_end) n->max_end = node_max_end(n->right);
}

static inline int node_balance(ITreeNode *n) {
    return n ? node_height(n->left) - node_height(n->right) : 0;
}

static ITreeNode *rotate_right(ITreeNode *y) {
    ITreeNode *x  = y->left;
    ITreeNode *t2 = x->right;
    x->right = y;
    y->left  = t2;
    node_update(y);
    node_update(x);
    return x;
}

static ITreeNode *rotate_left(ITreeNode *x) {
    ITreeNode *y  = x->right;
    ITreeNode *t2 = y->left;
    y->left  = x;
    x->right = t2;
    node_update(x);
    node_update(y);
    return y;
}

static ITreeNode *avl_balance(ITreeNode *n) {
    node_update(n);
    int bal = node_balance(n);

    if (bal > 1) {
        if (node_balance(n->left) < 0)
            n->left = rotate_left(n->left);
        return rotate_right(n);
    }
    if (bal < -1) {
        if (node_balance(n->right) > 0)
            n->right = rotate_right(n->right);
        return rotate_left(n);
    }
    return n;
}

static ITreeNode *node_new(size_t start, size_t end, PropValue *props) {
    ITreeNode *n = malloc(sizeof(ITreeNode));
    if (!n) return NULL;
    n->start   = start;
    n->end     = end;
    n->max_end = end;
    n->height  = 1;
    n->props   = props;
    n->left    = NULL;
    n->right   = NULL;
    return n;
}

// Insert or merge into AVL tree, returns new root
static ITreeNode *avl_insert(ITreeNode *node, size_t start, size_t end,
                              SCM prop, SCM value) {
    if (!node) {
        PropValue *pv = prop_set(NULL, prop, value);
        return node_new(start, end, pv);
    }

    if (start < node->start) {
        node->left  = avl_insert(node->left,  start, end, prop, value);
    } else if (start > node->start) {
        node->right = avl_insert(node->right, start, end, prop, value);
    } else {
        // Same start — if same end too, just update prop
        if (end == node->end) {
            node->props = prop_set(node->props, prop, value);
            node_update(node);
            return node;
        }
        // Different end at same start — insert to right by end
        if (end > node->end)
            node->right = avl_insert(node->right, start, end, prop, value);
        else
            node->left  = avl_insert(node->left,  start, end, prop, value);
    }

    return avl_balance(node);
}

// Find the leftmost node in a subtree
static ITreeNode *avl_min(ITreeNode *n) {
    while (n && n->left) n = n->left;
    return n;
}

// Delete node with given start+end, return new root
static ITreeNode *avl_delete(ITreeNode *node, size_t start, size_t end) {
    if (!node) return NULL;

    if (start < node->start) {
        node->left  = avl_delete(node->left,  start, end);
    } else if (start > node->start) {
        node->right = avl_delete(node->right, start, end);
    } else if (end != node->end) {
        if (end > node->end)
            node->right = avl_delete(node->right, start, end);
        else
            node->left  = avl_delete(node->left,  start, end);
    } else {
        // Found — delete this node
        if (!node->left || !node->right) {
            ITreeNode *child = node->left ? node->left : node->right;
            prop_free(node->props);
            free(node);
            return child;
        }
        // Two children — replace with in-order successor
        ITreeNode *succ = avl_min(node->right);
        node->start  = succ->start;
        node->end    = succ->end;
        PropValue *old = node->props;
        node->props  = prop_copy(succ->props);
        prop_free(old);
        node->right  = avl_delete(node->right, succ->start, succ->end);
    }

    return avl_balance(node);
}

// Stab query: find first node whose interval contains pos — O(log n)
static ITreeNode *avl_stab(ITreeNode *n, size_t pos) {
    if (!n) return NULL;
    // Prune: if max_end <= pos, no interval in this subtree can contain pos
    if (n->max_end <= pos) return NULL;

    // Try left first (leftmost match for stability)
    ITreeNode *left_result = avl_stab(n->left, pos);
    if (left_result) return left_result;

    // Check this node
    if (n->start <= pos && pos < n->end) return n;

    // Try right only if this node starts <= pos
    if (n->start <= pos) return avl_stab(n->right, pos);

    return NULL;
}

// Next boundary after pos — smallest start or end that is > pos
static void avl_next_boundary(ITreeNode *n, size_t pos, size_t *nearest) {
    if (!n) return;
    // No need to descend if the entire subtree starts after *nearest
    if (n->start >= *nearest) return;

    avl_next_boundary(n->left, pos, nearest);

    if (n->start > pos && n->start < *nearest) *nearest = n->start;
    if (n->end   > pos && n->end   < *nearest) *nearest = n->end;

    avl_next_boundary(n->right, pos, nearest);
}

// Next boundary after pos for a specific property key
static void avl_next_boundary_prop(ITreeNode *n, size_t pos, SCM prop, size_t *nearest) {
    if (!n) return;
    if (n->start >= *nearest) return;

    avl_next_boundary_prop(n->left, pos, prop, nearest);

    SCM val = prop_get(n->props, prop);
    if (!scm_is_false(val)) {
        if (n->start > pos && n->start < *nearest) *nearest = n->start;
        if (n->end   > pos && n->end   < *nearest) *nearest = n->end;
    }

    avl_next_boundary_prop(n->right, pos, prop, nearest);
}

// Previous boundary before pos
static void avl_prev_boundary(ITreeNode *n, size_t pos, size_t *nearest) {
    if (!n) return;
    avl_prev_boundary(n->left,  pos, nearest);
    if (n->start < pos && n->start > *nearest) *nearest = n->start;
    if (n->end   < pos && n->end   > *nearest) *nearest = n->end;
    avl_prev_boundary(n->right, pos, nearest);
}

static void avl_prev_boundary_prop(ITreeNode *n, size_t pos, SCM prop, size_t *nearest) {
    if (!n) return;
    avl_prev_boundary_prop(n->left, pos, prop, nearest);
    SCM val = prop_get(n->props, prop);
    if (!scm_is_false(val)) {
        if (n->start < pos && n->start > *nearest) *nearest = n->start;
        if (n->end   < pos && n->end   > *nearest) *nearest = n->end;
    }
    avl_prev_boundary_prop(n->right, pos, prop, nearest);
}

// Free entire tree
static void avl_free(ITreeNode *n) {
    if (!n) return;
    avl_free(n->left);
    avl_free(n->right);
    prop_free(n->props);
    free(n);
}

// Collect all nodes overlapping [start, end) into a flat array for manipulation
typedef struct { ITreeNode **nodes; size_t count; size_t cap; } NodeList;

static void nodelist_push(NodeList *list, ITreeNode *n) {
    if (list->count >= list->cap) {
        list->cap  = list->cap ? list->cap * 2 : 8;
        list->nodes = realloc(list->nodes, list->cap * sizeof(ITreeNode*));
    }
    list->nodes[list->count++] = n;
}

static void avl_collect_overlapping(ITreeNode *n, size_t start, size_t end, NodeList *out) {
    if (!n) return;
    if (n->max_end <= start) return; // prune
    avl_collect_overlapping(n->left, start, end, out);
    if (n->start < end && n->end > start)
        nodelist_push(out, n);
    if (n->start < end)
        avl_collect_overlapping(n->right, start, end, out);
}

// Shift all positions in tree by delta after pos
static void avl_adjust(ITreeNode *n, size_t pos, int delta) {
    if (!n) return;
    avl_adjust(n->left,  pos, delta);
    avl_adjust(n->right, pos, delta);

    if (delta > 0) {
        if (n->start >= pos) n->start += delta;
        if (n->end   >  pos) n->end   += delta;
    } else {
        size_t abs_delta  = (size_t)(-delta);
        size_t delete_end = pos + abs_delta;
        if (n->start >= pos && n->start < delete_end) n->start = pos;
        else if (n->start >= delete_end)               n->start -= abs_delta;
        if (n->end > pos && n->end <= delete_end)      n->end = pos;
        else if (n->end > delete_end)                  n->end -= abs_delta;
    }
    node_update(n);
}

/// API

typedef struct {
    size_t     start;
    size_t     end;
    PropValue *props;   // local (unprotected) snapshot
} NodeSnapshot;

void put_text_property(Buffer *buf, size_t start, size_t end, SCM prop, SCM value) {
    if (!buf || start >= end) return;

    // ── collect ──────────────────────────────────────────────────────────────
    NodeList overlapping = {0};
    avl_collect_overlapping(buf->intervals.root, start, end, &overlapping);

    if (overlapping.count == 0) {
        buf->intervals.root = avl_insert(buf->intervals.root, start, end, prop, value);
        buf->intervals.count++;
        free(overlapping.nodes);
        return;
    }

    // ── phase 1: snapshot then delete ALL overlapping nodes ──────────────────
    NodeSnapshot *snaps = malloc(overlapping.count * sizeof(NodeSnapshot));
    if (!snaps) {
        free(overlapping.nodes);
        return;
    }

    for (size_t i = 0; i < overlapping.count; i++) {
        ITreeNode *n = overlapping.nodes[i];
        snaps[i].start = n->start;
        snaps[i].end   = n->end;
        // Borrow the prop pointers — no extra protect.
        // avl_delete will prop_free (unprotect) these when it deletes the node.
        snaps[i].props = prop_copy_local(n->props);
    }

    // Delete all at once — after this loop every snapshot's SCM values have
    // been unprotected exactly once by prop_free inside avl_delete.
    for (size_t i = 0; i < overlapping.count; i++) {
        buf->intervals.root = avl_delete(buf->intervals.root,
                                          snaps[i].start, snaps[i].end);
        buf->intervals.count--;
    }

    free(overlapping.nodes);

    // ── phase 2: re-insert remainders ────────────────────────────────────────
    // For each original node we re-insert the portions that fall OUTSIDE
    // [start, end) with their original props intact, and the portion INSIDE
    // [start, end) with their original props minus the one being overwritten.
    //
    // avl_insert → prop_set → scm_gc_protect_object for every key/value that
    // goes back into the tree, so protection counts are correct again.

    for (size_t i = 0; i < overlapping.count; i++) {
        size_t     ns     = snaps[i].start;
        size_t     ne     = snaps[i].end;
        PropValue *nprops = snaps[i].props;

        // Left remainder: [ns, start)
        if (ns < start) {
            bool first = true;
            for (PropValue *pv = nprops; pv; pv = pv->next) {
                buf->intervals.root = avl_insert(buf->intervals.root,
                                                  ns, start,
                                                  pv->key, pv->value);
                if (first) { buf->intervals.count++; first = false; }
            }
        }

        // Inner portion: [max(ns,start), min(ne,end))
        {
            size_t is = ns > start ? ns : start;
            size_t ie = ne < end   ? ne : end;
            if (is < ie) {
                bool first = true;
                for (PropValue *pv = nprops; pv; pv = pv->next) {
                    // Drop the key we are overwriting — caller writes it below.
                    if (scm_is_eq(pv->key, prop) ||
                        scm_is_true(scm_equal_p(pv->key, prop)))
                        continue;
                    buf->intervals.root = avl_insert(buf->intervals.root,
                                                      is, ie,
                                                      pv->key, pv->value);
                    if (first) { buf->intervals.count++; first = false; }
                }
            }
        }

        // Right remainder: [end, ne)
        if (ne > end) {
            bool first = true;
            for (PropValue *pv = nprops; pv; pv = pv->next) {
                buf->intervals.root = avl_insert(buf->intervals.root,
                                                  end, ne,
                                                  pv->key, pv->value);
                if (first) { buf->intervals.count++; first = false; }
            }
        }

        // Free snapshot structs — SCM values already unprotected by phase 1.
        prop_free_local(nprops);
    }

    free(snaps);

    // ── write the caller's prop onto [start, end) ────────────────────────────
    buf->intervals.root = avl_insert(buf->intervals.root, start, end, prop, value);
    buf->intervals.count++;
}

SCM get_text_property(Buffer *buf, size_t pos, SCM prop) {
    if (!buf) return SCM_BOOL_F;
    ITreeNode *n = avl_stab(buf->intervals.root, pos);
    if (!n) return SCM_BOOL_F;
    return prop_get(n->props, prop);
}

int get_text_property_face(Buffer *buf, size_t pos) {
    if (!buf) return FACE_DEFAULT;

    ITreeNode *n = avl_stab(buf->intervals.root, pos);
    if (!n) return FACE_DEFAULT;

    SCM face = prop_get(n->props, scm_from_utf8_symbol("face"));
    if (scm_is_false(face)) return FACE_DEFAULT;

    if (scm_is_integer(face)) return scm_to_int(face);

    if (scm_is_symbol(face) || scm_is_string(face)) {
        SCM str = scm_is_string(face) ? face : scm_symbol_to_string(face);
        char *name = scm_to_locale_string(str);
        int id = face_id_from_name(name);
        free(name);
        return id < 0 ? FACE_DEFAULT : id;
    }

    return FACE_DEFAULT;
}

SCM get_text_property_field(Buffer *buf, size_t pos) {
    if (!buf) return SCM_BOOL_F;
    return get_text_property(buf, pos, scm_from_utf8_symbol("field"));
}

size_t next_property_change(Buffer *buf, size_t pos) {
    if (!buf) return pos;
    size_t nearest = rope_char_length(buf->rope);
    avl_next_boundary(buf->intervals.root, pos, &nearest);
    return nearest;
}

size_t previous_property_change(Buffer *buf, size_t pos) {
    if (!buf || pos == 0) return 0;
    size_t nearest = 0;
    avl_prev_boundary(buf->intervals.root, pos, &nearest);
    return nearest;
}

size_t next_single_property_change(Buffer *buf, size_t pos, SCM prop) {
    if (!buf) return pos;
    size_t nearest = rope_char_length(buf->rope);
    avl_next_boundary_prop(buf->intervals.root, pos, prop, &nearest);
    return nearest;
}

size_t previous_single_property_change(Buffer *buf, size_t pos, SCM prop) {
    if (!buf || pos == 0) return 0;
    size_t nearest = 0;
    avl_prev_boundary_prop(buf->intervals.root, pos, prop, &nearest);
    return nearest;
}

void remove_text_properties(Buffer *buf, size_t start, size_t end) {
    if (!buf) return;

    NodeList overlapping = {0};
    avl_collect_overlapping(buf->intervals.root, start, end, &overlapping);

    for (size_t i = 0; i < overlapping.count; i++) {
        ITreeNode *n = overlapping.nodes[i];
        size_t n_start = n->start;
        size_t n_end   = n->end;
        PropValue *n_props = prop_copy(n->props);

        buf->intervals.root = avl_delete(buf->intervals.root, n_start, n_end);
        buf->intervals.count--;

        // Preserve portions outside [start, end)
        if (n_start < start) {
            PropValue *pv = n_props;
            while (pv) {
                buf->intervals.root = avl_insert(buf->intervals.root, n_start, start, pv->key, pv->value);
                pv = pv->next;
            }
            buf->intervals.count++;
        }
        if (n_end > end) {
            PropValue *pv = n_props;
            while (pv) {
                buf->intervals.root = avl_insert(buf->intervals.root, end, n_end, pv->key, pv->value);
                pv = pv->next;
            }
            buf->intervals.count++;
        }

        prop_free(n_props);
    }
    free(overlapping.nodes);
}

void clear_text_properties(Buffer *buf) {
    if (!buf) return;
    avl_free(buf->intervals.root);
    buf->intervals.root  = NULL;
    buf->intervals.count = 0;
}

void adjust_text_properties(Buffer *buf, size_t pos, int delta) {
    if (!buf || delta == 0) return;
    avl_adjust(buf->intervals.root, pos, delta);
}

bool is_range_readonly(Buffer *buf, size_t start, size_t end) {
    if (!buf) return false;

    NodeList overlapping = {0};
    avl_collect_overlapping(buf->intervals.root, start, end, &overlapping);

    SCM readonly_sym = scm_from_utf8_symbol("read-only");
    bool result = false;

    for (size_t i = 0; i < overlapping.count; i++) {
        SCM val = prop_get(overlapping.nodes[i]->props, readonly_sym);
        if (scm_is_true(val)) { result = true; break; }
    }

    free(overlapping.nodes);
    return result;
}

/// SCM

static SCM scm_put_text_property(SCM start_scm, SCM end_scm, SCM prop, SCM value) {
    if (!scm_is_integer(start_scm)) scm_wrong_type_arg("put-text-property", 1, start_scm);
    if (!scm_is_integer(end_scm))   scm_wrong_type_arg("put-text-property", 2, end_scm);
    if (current_buffer)
        put_text_property(current_buffer, scm_to_size_t(start_scm), scm_to_size_t(end_scm), prop, value);
    return SCM_UNSPECIFIED;
}

static SCM scm_get_text_property(SCM pos_scm, SCM prop) {
    if (!scm_is_integer(pos_scm)) scm_wrong_type_arg("get-text-property", 1, pos_scm);
    if (!current_buffer) return SCM_BOOL_F;
    return get_text_property(current_buffer, scm_to_size_t(pos_scm), prop);
}

static SCM scm_remove_text_properties(SCM start_scm, SCM end_scm, SCM props) {
    (void)props;
    if (!scm_is_integer(start_scm)) scm_wrong_type_arg("remove-text-properties", 1, start_scm);
    if (!scm_is_integer(end_scm))   scm_wrong_type_arg("remove-text-properties", 2, end_scm);
    if (current_buffer)
        remove_text_properties(current_buffer, scm_to_size_t(start_scm), scm_to_size_t(end_scm));
    return SCM_BOOL_T;
}

static SCM scm_next_property_change(SCM pos_scm) {
    if (!current_buffer) return SCM_BOOL_F;
    size_t pos = scm_is_integer(pos_scm) ? scm_to_size_t(pos_scm) : current_buffer->pt;
    return scm_from_size_t(next_property_change(current_buffer, pos));
}

static SCM scm_previous_property_change(SCM pos_scm) {
    if (!current_buffer) return SCM_BOOL_F;
    size_t pos = scm_is_integer(pos_scm) ? scm_to_size_t(pos_scm) : current_buffer->pt;
    return scm_from_size_t(previous_property_change(current_buffer, pos));
}

static SCM scm_next_single_property_change(SCM prop, SCM pos_scm) {
    if (!current_buffer) return SCM_BOOL_F;
    size_t pos = scm_is_integer(pos_scm) ? scm_to_size_t(pos_scm) : current_buffer->pt;
    return scm_from_size_t(next_single_property_change(current_buffer, pos, prop));
}

static SCM scm_previous_single_property_change(SCM prop, SCM pos_scm) {
    if (!current_buffer) return SCM_BOOL_F;
    size_t pos = scm_is_integer(pos_scm) ? scm_to_size_t(pos_scm) : current_buffer->pt;
    return scm_from_size_t(previous_single_property_change(current_buffer, pos, prop));
}

void init_textprop_bindings(void) {
    scm_c_define_gsubr("put-text-property",               4, 0, 0, scm_put_text_property);
    scm_c_define_gsubr("get-text-property",               2, 0, 0, scm_get_text_property);
    scm_c_define_gsubr("remove-text-properties",          3, 0, 0, scm_remove_text_properties);
    scm_c_define_gsubr("next-property-change",            0, 1, 0, scm_next_property_change);
    scm_c_define_gsubr("previous-property-change",        0, 1, 0, scm_previous_property_change);
    scm_c_define_gsubr("next-single-property-change",     1, 1, 0, scm_next_single_property_change);
    scm_c_define_gsubr("previous-single-property-change", 1, 1, 0, scm_previous_single_property_change);
}


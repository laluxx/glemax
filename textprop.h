#pragma once
/*
 * textprop.h — Unified text property store.
 *
 * ONE flat sorted array of TextPropSpan covers ALL properties:
 * treesit face spans, read-only, field, display, user-set face, etc.
 * No rope markers. No HLStore. No AVL tree. No GC pressure per span.
 *
 * ── Lazy shift ───────────────────────────────────────────────────────────────
 *
 * On insert/delete at position P:
 *   - Spans entirely within [visible_start, visible_end]: shifted immediately.
 *     Cost: O(visible_spans) — ~200-500 regardless of file size.
 *   - Spans beyond visible_end: NOT touched. Instead we record:
 *       dirty_from    = min(dirty_from, P)
 *       pending_delta += ±count
 *
 * On draw_buffer scroll into new region [old_visible_end, new_visible_end]:
 *   - apply_pending_delta_upto(new_visible_end) shifts newly visible spans.
 *   - Cost: O(newly_visible_spans) — amortized O(1) per char drawn.
 *
 * Result: inserting at position 0 of a 100 000-span file costs O(visible_spans)
 * not O(100 000). File size no longer affects per-keypress latency.
 *
 * ── Array layout ─────────────────────────────────────────────────────────────
 *
 * spans[] is sorted by start asc, end desc (standard interval sort).
 * Queries use binary search: O(log N).
 * treesit replaces ranges atomically via textprop_replace_range():
 *   one memmove + one memcpy, no rebalancing.
 * Persistent props (read-only, field, …) are inserted via put_text_property()
 * which does a small linear scan of overlapping spans — rare and tiny.
 */

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <libguile.h>

typedef struct Buffer Buffer;

// Linked list of key->value pairs on a span
typedef struct PropValue {
    SCM              key;
    SCM              value;
    struct PropValue *next;
} PropValue;

// One interval with its properties
typedef struct {
    size_t     start;   // inclusive, char position
    size_t     end;     // exclusive, char position
    PropValue *props;   // NULL for face-only spans (use face_id directly)
    int        face_id; // FACE_DEFAULT (-1) if not a face span
} TextPropSpan;         // 32 bytes on 64-bit

// The single store per buffer
typedef struct {
    TextPropSpan *spans;
    size_t        count;
    size_t        cap;

    // Lazy-shift state
    size_t   visible_start;  // current viewport top (char pos)
    size_t   visible_end;    // current viewport bottom (char pos)
    size_t   dirty_from;     // spans starting here or later need pending_delta
    int32_t  pending_delta;  // accumulated offset for dirty region

    size_t   highlighted_end;
} TextPropStore;

void textprop_store_init(TextPropStore *s);
void textprop_store_free(TextPropStore *s);
void textprop_store_clear(TextPropStore *s);

// Viewport tracking (called by draw_buffer)

void textprop_set_viewport(Buffer *buf, size_t vis_start, size_t vis_end);

// Edit notifications (called from insert / delete_impl)
// NOTE Must be called AFTER the rope mutation, with the new char counts.
void textprop_adjust_insert(Buffer *buf, size_t pos, size_t count);
void textprop_adjust_delete(Buffer *buf, size_t pos, size_t end);

// Queries

// Returns FACE_DEFAULT if no face span covers pos. O(log N).
int  textprop_get_face(Buffer *buf, size_t pos);

// Combined face + next boundary query. O(log N).
// Sets *next_change to the nearest boundary > pos and <= limit.
int  textprop_get_face_and_next_change(Buffer *buf, size_t pos, size_t limit,
                                        size_t *next_change);

// Returns SCM_BOOL_F if prop not found at pos. O(log N).
SCM  textprop_get(Buffer *buf, size_t pos, SCM prop);

// Boundary scanning.
size_t textprop_next_change(Buffer *buf, size_t pos);
size_t textprop_prev_change(Buffer *buf, size_t pos);
size_t textprop_next_single_change(Buffer *buf, size_t pos, SCM prop);
size_t textprop_prev_single_change(Buffer *buf, size_t pos, SCM prop);

bool   textprop_is_readonly(Buffer *buf, size_t start, size_t end);

// Mutations

// Full put: splits overlapping spans correctly.
void textprop_put(Buffer *buf, size_t start, size_t end, SCM prop, SCM value);

// Fast put: no overlap handling - for treesit bulk emit into a fresh range.
void textprop_put_fast(Buffer *buf, size_t start, size_t end, SCM prop, SCM value);

// Atomic range replacement - for treesit_apply_highlights_range.
// Replaces all spans overlapping [start, end) with new_spans[0..n).
// new_spans must be sorted by start asc, within [start, end).
void textprop_replace_range(Buffer *buf, size_t start, size_t end,
                             const TextPropSpan *new_spans, size_t n);

// Remove all props from [start, end) (splits overlapping spans).
void textprop_remove(Buffer *buf, size_t start, size_t end);


static inline void notify_text_modified(Buffer *buf) { (void)buf; }  // no-op now

// Compatibility wrappers
void    put_text_property(Buffer *buf, size_t start, size_t end, SCM prop, SCM value);
void    put_text_property_fast(Buffer *buf, size_t start, size_t end, SCM prop, SCM value);
SCM     get_text_property(Buffer *buf, size_t pos, SCM prop);
int     get_text_property_face(Buffer *buf, size_t pos);
int     get_face_id_and_next_change(Buffer *buf, size_t pos, size_t limit, size_t *next_change);
SCM     get_text_property_field(Buffer *buf, size_t pos);
size_t  next_property_change(Buffer *buf, size_t pos);
size_t  previous_property_change(Buffer *buf, size_t pos);
size_t  next_single_property_change(Buffer *buf, size_t pos, SCM prop);
size_t  previous_single_property_change(Buffer *buf, size_t pos, SCM prop);
void    remove_text_properties(Buffer *buf, size_t start, size_t end);
void    clear_text_properties(Buffer *buf);
bool    is_range_readonly(Buffer *buf, size_t start, size_t end);

// Low-level helper still needed by a few callers
SCM prop_get(PropValue *pv, SCM key);

void init_textprop_bindings(void);


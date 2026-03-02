#include "textprop.h"
#include "buffer.h"
#include "faces.h"
#include "rope.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/// PropValue helpers

SCM prop_get(PropValue *pv, SCM key) {
    while (pv) {
        if (scm_is_eq(pv->key, key) ||
            scm_is_true(scm_equal_p(pv->key, key)))
            return pv->value;
        pv = pv->next;
    }
    return SCM_BOOL_F;
}

static PropValue *prop_set(PropValue *pv, SCM key, SCM value) {
    for (PropValue *p = pv; p; p = p->next) {
        if (scm_is_eq(p->key, key) ||
            scm_is_true(scm_equal_p(p->key, key))) {
            scm_gc_protect_object(value);
            scm_gc_unprotect_object(p->value);
            p->value = value;
            return pv;
        }
    }
    PropValue *n = malloc(sizeof *n);
    if (!n) return pv;
    scm_gc_protect_object(key);
    scm_gc_protect_object(value);
    n->key = key; n->value = value; n->next = pv;
    return n;
}

static void prop_free(PropValue *pv) {
    while (pv) {
        PropValue *nx = pv->next;
        scm_gc_unprotect_object(pv->key);
        scm_gc_unprotect_object(pv->value);
        free(pv);
        pv = nx;
    }
}

static PropValue *prop_copy(PropValue *src) {
    PropValue *head = NULL, **tail = &head;
    while (src) {
        PropValue *n = malloc(sizeof *n);
        if (!n) break;
        scm_gc_protect_object(src->key);
        scm_gc_protect_object(src->value);
        n->key = src->key; n->value = src->value; n->next = NULL;
        *tail = n; tail = &n->next;
        src = src->next;
    }
    return head;
}

/// Store lifecycle

void textprop_store_init(TextPropStore *s) {
    s->spans           = NULL;
    s->count           = 0;
    s->cap             = 0;
    s->visible_start   = 0;
    s->visible_end     = 0;
    s->dirty_from      = SIZE_MAX;
    s->pending_delta   = 0;
    s->highlighted_end = 0;
}

void textprop_store_free(TextPropStore *s) {
    for (size_t i = 0; i < s->count; i++)
        prop_free(s->spans[i].props);
    free(s->spans);
    textprop_store_init(s);
}

void textprop_store_clear(TextPropStore *s) {
    for (size_t i = 0; i < s->count; i++)
        prop_free(s->spans[i].props);
    s->count           = 0;
    s->dirty_from      = SIZE_MAX;
    s->pending_delta   = 0;
    s->highlighted_end = 0;
}

// Internal: array growth

static bool store_grow(TextPropStore *s, size_t needed) {
    if (needed <= s->cap) return true;
    size_t nc = s->cap ? s->cap * 2 : 256;
    while (nc < needed) nc *= 2;
    TextPropSpan *p = realloc(s->spans, nc * sizeof *p);
    if (!p) return false;
    s->spans = p; s->cap = nc;
    return true;
}

// Internal: binary searches
// (all positions are in TRUE / virtual coords after delta is applied)

/* first index where spans[i].start >= pos */
static size_t lb_start(const TextPropStore *s, size_t pos) {
    size_t lo = 0, hi = s->count;
    while (lo < hi) {
        size_t mid = (lo + hi) >> 1;
        if (s->spans[mid].start < pos) lo = mid + 1;
        else hi = mid;
    }
    return lo;
}

/* first index where spans[i].start > pos */
static size_t ub_start(const TextPropStore *s, size_t pos) {
    size_t lo = 0, hi = s->count;
    while (lo < hi) {
        size_t mid = (lo + hi) >> 1;
        if (s->spans[mid].start <= pos) lo = mid + 1;
        else hi = mid;
    }
    return lo;
}

// Internal: apply pending delta to spans in [from, upto)
//
// This is the lazy flush. Called when we need true coordinates for a region
// that was dirty. Marks dirty_from forward after applying.

static void flush_delta_upto(TextPropStore *s, size_t upto) {
    if (!s->pending_delta || s->dirty_from == SIZE_MAX || s->dirty_from >= upto)
        return;

    size_t first = lb_start(s, s->dirty_from);
    size_t last  = lb_start(s, upto);  /* first span with start >= upto */

    int32_t d = s->pending_delta;
    for (size_t i = first; i < last; i++) {
        s->spans[i].start = (size_t)((ssize_t)s->spans[i].start + d);
        s->spans[i].end   = (size_t)((ssize_t)s->spans[i].end   + d);
    }

    /* Advance dirty_from to upto */
    s->dirty_from = upto;

    /* Check if any dirty spans remain beyond the new dirty_from */
    size_t next_dirty = lb_start(s, s->dirty_from);
    if (next_dirty >= s->count) {
        /* No more spans at or beyond dirty_from — fully clean */
        s->dirty_from    = SIZE_MAX;
        s->pending_delta = 0;
    }
    /* else: keep pending_delta as-is, dirty_from stays at upto */
}


/// Viewport tracking

void textprop_set_viewport(Buffer *buf, size_t vis_start, size_t vis_end) {
    TextPropStore *s = &buf->props;

    /* Flush lazy delta for newly visible region */
    if (s->pending_delta && s->dirty_from != SIZE_MAX &&
        vis_end > s->dirty_from)
        flush_delta_upto(s, vis_end);

    s->visible_start = vis_start;
    s->visible_end   = vis_end;

    /* Only rehighlight if we've scrolled past the previously highlighted end.
       Use a lookahead of 2x the viewport size so scrolling doesn't trigger
       a rehighlight on every frame. */
    if (vis_end > s->highlighted_end &&
        buf->ts_state && buf->ts_state->tree &&
        buf->ts_state->hl_query && !buf->ts_state->needs_reparse) {

        size_t viewport_size = vis_end > vis_start ? vis_end - vis_start : 1000;
        size_t highlight_to  = vis_end + viewport_size; /* 1 viewport lookahead */

        uint32_t sb = (uint32_t)treesit_char_to_byte(buf, s->highlighted_end);
        uint32_t eb = (uint32_t)treesit_char_to_byte(buf, highlight_to);
        size_t   total_bytes = rope_byte_length(buf->rope);
        if (eb > (uint32_t)total_bytes) eb = (uint32_t)total_bytes;

        if (sb < eb)
            treesit_apply_highlights_range(buf, sb, eb);

        s->highlighted_end = highlight_to;
    }
}


/* Convert a true char position to stored coords.
   Visible/clean spans: stored == true.
   Dirty spans (>= dirty_from): stored = true - pending_delta.
   Call this before writing any .start/.end into the array. */
static inline size_t true_to_stored(const TextPropStore *s, size_t pos) {
    if (s->pending_delta && s->dirty_from != SIZE_MAX && pos >= s->dirty_from)
        return (size_t)((ssize_t)pos - s->pending_delta);
    return pos;
}

/// Edit adjustments

void textprop_adjust_insert(Buffer *buf, size_t pos, size_t count) {
    if (!count) return;
    TextPropStore *s = &buf->props;
    if (!s->count) return;

    size_t vis_end = s->visible_end;

    /* Step 1: flush any existing pending delta for the visible region FIRST.
       This ensures visible spans are in true coords before we touch them.
       Without this, a second edit would see stale coords. */
    if (s->pending_delta && s->dirty_from != SIZE_MAX && s->dirty_from < vis_end)
        flush_delta_upto(s, vis_end);

    /* Step 2: eagerly shift spans whose START is in [pos, vis_end).
       These are visible and in true coords (just flushed above). */
    size_t first = lb_start(s, pos);

    /* Handle the straddling span: start < pos, end > pos.
       Its start doesn't move, but its end does. Only do this if its
       start < vis_end (guaranteed since start < pos and pos could be anywhere). */
    if (first > 0) {
        TextPropSpan *sp = &s->spans[first - 1];
        if (sp->start < vis_end && sp->end > pos)
            sp->end += count;
    }

    /* Shift all spans starting in [pos, vis_end) */
    for (size_t i = first; i < s->count; i++) {
        if (s->spans[i].start >= vis_end) break;
        s->spans[i].start += count;
        s->spans[i].end   += count;
    }

    /* Step 3: accumulate lazy delta for everything at or beyond vis_end.
       dirty_from = vis_end (if insert was inside viewport) or pos (if outside). */
    size_t new_dirty = (pos < vis_end) ? vis_end : pos;
    s->pending_delta += (int32_t)count;
    if (s->dirty_from == SIZE_MAX || new_dirty < s->dirty_from)
        s->dirty_from = new_dirty;
}

void textprop_adjust_delete(Buffer *buf, size_t pos, size_t end_pos) {
    if (pos >= end_pos) return;
    TextPropStore *s = &buf->props;
    if (!s->count) return;

    size_t count   = end_pos - pos;
    size_t vis_end = s->visible_end;

    /* Flush existing pending delta for visible region first */
    if (s->pending_delta && s->dirty_from != SIZE_MAX && s->dirty_from < vis_end)
        flush_delta_upto(s, vis_end);

    /* Handle straddling span (start < pos, end > pos) in visible region */
    if (pos < vis_end) {
        size_t si = lb_start(s, pos);
        if (si > 0) {
            TextPropSpan *sp = &s->spans[si - 1];
            if (sp->start < vis_end && sp->end > pos) {
                if (sp->end <= end_pos) sp->end = pos;
                else                   sp->end -= count;
            }
        }
    }

    /* Compaction pass over visible spans [pos, vis_end) */
    size_t rd = lb_start(s, pos);
    size_t w  = rd;

    for (size_t i = rd; i < s->count; i++) {
        size_t sc = s->spans[i].start;

        if (sc >= vis_end) {
            /* Beyond viewport: keep (lazy delta handles it) */
            if (w != i) s->spans[w] = s->spans[i];
            w++; continue;
        }

        size_t ec = s->spans[i].end;

        if (sc >= pos && sc < end_pos) {
            /* Starts inside deleted range */
            if (ec > end_pos) {
                s->spans[i].start = pos;
                s->spans[i].end   = pos + (ec - end_pos);
                if (w != i) s->spans[w] = s->spans[i];
                w++;
            } else {
                prop_free(s->spans[i].props); /* wholly deleted */
            }
        } else if (sc >= end_pos) {
            /* Entirely after deletion — shift back */
            s->spans[i].start = sc - count;
            s->spans[i].end   = (ec > count) ? ec - count : 0;
            if (w != i) s->spans[w] = s->spans[i];
            w++;
        } else {
            /* Entirely before pos — no change */
            if (w != i) s->spans[w] = s->spans[i];
            w++;
        }
    }
    s->count = w;

    /* Accumulate lazy delta for beyond-viewport spans */
    size_t new_dirty = (pos < vis_end) ? vis_end : pos;
    s->pending_delta -= (int32_t)count;
    if (s->dirty_from == SIZE_MAX || new_dirty < s->dirty_from)
        s->dirty_from = new_dirty;
}

/// Queries

int textprop_get_face(Buffer *buf, size_t pos) {
    TextPropStore *s = &buf->props;
    if (!s->count) return FACE_DEFAULT;

    /* Flush delta up to pos+1 if needed */
    if (s->pending_delta && pos >= s->dirty_from)
        flush_delta_upto(s, pos + 1);

    /* Find last span with start <= pos */
    size_t i = ub_start(s, pos);
    /* Walk backwards to find spans containing pos */
    /* Since spans are sorted by start asc, end desc — check i-1, i-2, etc. */
    while (i > 0) {
        i--;
        if (s->spans[i].start > pos) continue;
        if (s->spans[i].end <= pos)  {
            if (s->spans[i].start < pos) break; /* sorted: nothing earlier contains pos */
            continue;
        }
        /* spans[i] contains pos */
        if (s->spans[i].face_id != FACE_DEFAULT)
            return s->spans[i].face_id;
        /* Check props for 'face' key */
        if (s->spans[i].props) {
            static SCM s_face = 0;
            if (!s_face) { s_face = scm_from_utf8_symbol("face"); scm_gc_protect_object(s_face); }
            SCM fv = prop_get(s->spans[i].props, s_face);
            if (!scm_is_false(fv)) {
                if (scm_is_integer(fv)) return scm_to_int(fv);
                if (scm_is_symbol(fv) || scm_is_string(fv)) {
                    SCM str = scm_is_string(fv) ? fv : scm_symbol_to_string(fv);
                    char *nm = scm_to_locale_string(str);
                    int id = face_id_from_name(nm); free(nm);
                    return id < 0 ? FACE_DEFAULT : id;
                }
            }
        }
    }
    return FACE_DEFAULT;
}

int textprop_get_face_and_next_change(Buffer *buf, size_t pos, size_t limit,
                                       size_t *next_change) {
    *next_change = limit;
    TextPropStore *s = &buf->props;
    if (!s->count) return FACE_DEFAULT;

    if (s->pending_delta && pos >= s->dirty_from)
        flush_delta_upto(s, limit);

    static SCM s_face = 0;
    if (!s_face) { s_face = scm_from_utf8_symbol("face"); scm_gc_protect_object(s_face); }

    int result_face = FACE_DEFAULT;

    /* Find the span containing pos */
    size_t i = ub_start(s, pos);
    size_t containing = SIZE_MAX;
    size_t j = i;
    while (j > 0) {
        j--;
        if (s->spans[j].end <= pos) break;
        if (s->spans[j].start <= pos) {
            containing = j;
            break;
        }
    }

    if (containing != SIZE_MAX) {
        /* Extract face */
        if (s->spans[containing].face_id != FACE_DEFAULT)
            result_face = s->spans[containing].face_id;
        else if (s->spans[containing].props) {
            SCM fv = prop_get(s->spans[containing].props, s_face);
            if (!scm_is_false(fv)) {
                if (scm_is_integer(fv)) result_face = scm_to_int(fv);
                else if (scm_is_symbol(fv) || scm_is_string(fv)) {
                    SCM str = scm_is_string(fv) ? fv : scm_symbol_to_string(fv);
                    char *nm = scm_to_locale_string(str);
                    result_face = face_id_from_name(nm); free(nm);
                    if (result_face < 0) result_face = FACE_DEFAULT;
                }
            }
        }
        /* End of this span is a boundary */
        if (s->spans[containing].end < *next_change)
            *next_change = s->spans[containing].end;
    }

    /* Next span starting after pos */
    if (i < s->count && s->spans[i].start < *next_change)
        *next_change = s->spans[i].start;

    return result_face;
}

SCM textprop_get(Buffer *buf, size_t pos, SCM prop) {
    TextPropStore *s = &buf->props;
    if (!s->count) return SCM_BOOL_F;

    if (s->pending_delta && pos >= s->dirty_from)
        flush_delta_upto(s, pos + 1);

    size_t i = ub_start(s, pos);
    while (i > 0) {
        i--;
        if (s->spans[i].end <= pos) break;
        if (s->spans[i].start <= pos && s->spans[i].props) {
            SCM v = prop_get(s->spans[i].props, prop);
            if (!scm_is_false(v)) return v;
        }
    }
    return SCM_BOOL_F;
}

size_t textprop_next_change(Buffer *buf, size_t pos) {
    TextPropStore *s = &buf->props;
    size_t buf_len = rope_char_length(buf->rope);
    if (!s->count) return buf_len;

    if (s->pending_delta && pos >= s->dirty_from)
        flush_delta_upto(s, buf_len);

    size_t nearest = buf_len;
    size_t i = ub_start(s, pos); /* first with start > pos */

    /* Span containing pos may end before nearest */
    if (i > 0 && s->spans[i-1].end > pos && s->spans[i-1].end < nearest)
        nearest = s->spans[i-1].end;
    /* Next span starting after pos */
    if (i < s->count && s->spans[i].start < nearest)
        nearest = s->spans[i].start;

    return nearest;
}

size_t textprop_prev_change(Buffer *buf, size_t pos) {
    TextPropStore *s = &buf->props;
    if (!s->count || pos == 0) return 0;

    if (s->pending_delta && pos > s->dirty_from)
        flush_delta_upto(s, pos);

    size_t nearest = 0;
    size_t i = lb_start(s, pos); /* first with start >= pos */

    if (i > 0) {
        /* Start of previous span */
        if (s->spans[i-1].start > nearest) nearest = s->spans[i-1].start;
        /* End of previous span (if < pos) */
        if (s->spans[i-1].end < pos && s->spans[i-1].end > nearest)
            nearest = s->spans[i-1].end;
    }
    return nearest;
}

size_t textprop_next_single_change(Buffer *buf, size_t pos, SCM prop) {
    TextPropStore *s = &buf->props;
    size_t buf_len = rope_char_length(buf->rope);
    if (!s->count) return buf_len;

    if (s->pending_delta && pos >= s->dirty_from)
        flush_delta_upto(s, buf_len);

    size_t nearest = buf_len;
    /* Linear scan from pos — persistent props are rare (< 20 spans typically) */
    for (size_t i = 0; i < s->count; i++) {
        if (s->spans[i].end <= pos) continue;
        if (!s->spans[i].props) continue;
        if (scm_is_false(prop_get(s->spans[i].props, prop))) continue;
        if (s->spans[i].start > pos && s->spans[i].start < nearest)
            nearest = s->spans[i].start;
        if (s->spans[i].end > pos && s->spans[i].end < nearest)
            nearest = s->spans[i].end;
    }
    return nearest;
}

size_t textprop_prev_single_change(Buffer *buf, size_t pos, SCM prop) {
    TextPropStore *s = &buf->props;
    if (!s->count || pos == 0) return 0;

    if (s->pending_delta && pos > s->dirty_from)
        flush_delta_upto(s, pos);

    size_t nearest = 0;
    for (size_t i = 0; i < s->count; i++) {
        if (s->spans[i].start >= pos) continue;
        if (!s->spans[i].props) continue;
        if (scm_is_false(prop_get(s->spans[i].props, prop))) continue;
        if (s->spans[i].start < pos && s->spans[i].start > nearest)
            nearest = s->spans[i].start;
        if (s->spans[i].end < pos && s->spans[i].end > nearest)
            nearest = s->spans[i].end;
    }
    return nearest;
}

bool textprop_is_readonly(Buffer *buf, size_t start, size_t end) {
    TextPropStore *s = &buf->props;
    if (!s->count) return false;

    if (s->pending_delta && start >= s->dirty_from)
        flush_delta_upto(s, end);

    static SCM s_ro = 0;
    if (!s_ro) { s_ro = scm_from_utf8_symbol("read-only"); scm_gc_protect_object(s_ro); }

    size_t i = ub_start(s, start);
    if (i > 0 && s->spans[i-1].end > start) i--;

    for (; i < s->count && s->spans[i].start < end; i++) {
        if (s->spans[i].end <= start) continue;
        if (!s->spans[i].props) continue;
        if (scm_is_true(prop_get(s->spans[i].props, s_ro))) return true;
    }
    return false;
}

/// Mutations

/*
 * textprop_replace_range — atomic range replacement for treesit.
 *
 * Flushes pending delta for the range first, then replaces all overlapping
 * spans with new_spans. This is the ONLY mutation treesit calls.
 * Cost: one memmove + one memcpy. No rebalancing.
 */
void textprop_replace_range(Buffer *buf, size_t start, size_t end,
                             const TextPropSpan *new_spans, size_t n) {
    TextPropStore *s = &buf->props;

    /*
     * We must NOT flush dirty spans beyond vis_end — that would defeat the
     * entire lazy-shift design.
     *
     * Strategy:
     *   - The range [start, end) may straddle vis_end.
     *   - For the visible part [start, min(end, vis_end)): flush and replace normally.
     *   - For the dirty part [vis_end, end): new_spans that land there must be
     *     stored in PRE-delta coords (i.e. subtract pending_delta) so they're
     *     consistent with other unflushed spans.
     *
     * Since treesitter gives us true char coords, we adjust: stored = true - pending_delta.
     */

    size_t vis_end = s->visible_end;

    /* Flush only the visible portion of the dirty range */
    size_t flush_upto = (end < vis_end) ? end : vis_end;
    if (s->pending_delta && s->dirty_from != SIZE_MAX && start >= s->dirty_from
        && flush_upto > s->dirty_from)
        flush_delta_upto(s, flush_upto);

    /* Find the range of existing spans to replace */
    size_t first = lb_start(s, start);
    if (first > 0 && s->spans[first - 1].end > start) first--;

    size_t last = lb_start(s, end);
    while (last < s->count && s->spans[last].start < end) last++;

    /* Free props of replaced spans */
    for (size_t i = first; i < last; i++)
        prop_free(s->spans[i].props);

    size_t old_n = last - first;
    if (!store_grow(s, s->count - old_n + n)) return;

    if (n != old_n && last < s->count)
        memmove(s->spans + first + n, s->spans + last,
                (s->count - last) * sizeof(TextPropSpan));

    s->count = s->count - old_n + n;

    if (n) {
        memcpy(s->spans + first, new_spans, n * sizeof(TextPropSpan));

        /* For new spans that land beyond vis_end (dirty region), convert from
           true coords to pre-delta stored coords by subtracting pending_delta. */
        if (s->pending_delta) {
            for (size_t i = 0; i < n; i++) {
                if (s->spans[first + i].start >= vis_end) {
                    s->spans[first + i].start =
                        (size_t)((ssize_t)s->spans[first + i].start - s->pending_delta);
                    s->spans[first + i].end =
                        (size_t)((ssize_t)s->spans[first + i].end - s->pending_delta);
                }
            }
        }
    }
}

void textprop_put(Buffer *buf, size_t start, size_t end, SCM prop, SCM value) {
    if (!buf || start >= end) return;
    TextPropStore *s = &buf->props;

    size_t vis_end = s->visible_end;
    if (s->pending_delta && s->dirty_from != SIZE_MAX && s->dirty_from < vis_end)
        flush_delta_upto(s, vis_end);

    size_t start_stored = true_to_stored(s, start);
    size_t end_stored   = true_to_stored(s, end);

    size_t first = lb_start(s, start_stored);
    if (first > 0 && s->spans[first - 1].end > start_stored) first--;
    size_t last = first;
    while (last < s->count && s->spans[last].start < end_stored) last++;

    if (first == last) {
        /* No overlapping spans — simple insert */
        if (!store_grow(s, s->count + 1)) return;
        memmove(s->spans + first + 1, s->spans + first,
                (s->count - first) * sizeof(TextPropSpan));
        s->spans[first] = (TextPropSpan){
            .start   = start_stored,
            .end     = end_stored,
            .props   = prop_set(NULL, prop, value),
            .face_id = FACE_DEFAULT
        };
        s->count++;
        return;
    }

    typedef struct { size_t s, e; PropValue *p; } Snap;
    Snap *snaps = malloc((last - first) * sizeof *snaps);
    if (!snaps) return;
    for (size_t i = first; i < last; i++) {
        snaps[i - first].s = s->spans[i].start;
        snaps[i - first].e = s->spans[i].end;
        snaps[i - first].p = s->spans[i].props;
        s->spans[i].props = NULL;
    }

    s->count -= (last - first);
    memmove(s->spans + first, s->spans + last,
            (s->count - first) * sizeof(TextPropSpan));

    size_t n_snaps = last - first;

    /* Collect merged props from all spans that exactly cover [start, end).
       We'll apply them to the new center span. */
    PropValue *merged_center = NULL;
    for (size_t si = 0; si < n_snaps; si++) {
        size_t ns = snaps[si].s, ne = snaps[si].e;
        /* Only merge props from spans that are fully contained in [start,end)
           or exactly equal — i.e. the "center" portion */
        if (ns >= start_stored && ne <= end_stored) {
            /* Merge all props from this snap into merged_center */
            PropValue *pv = snaps[si].p;
            while (pv) {
                merged_center = prop_set(merged_center, pv->key, pv->value);
                pv = pv->next;
            }
        }
    }
    /* Apply the new prop on top (overrides if key already exists) */
    merged_center = prop_set(merged_center, prop, value);

    for (size_t si = 0; si < n_snaps; si++) {
        size_t ns = snaps[si].s, ne = snaps[si].e;
        PropValue *snap = snaps[si].p;

        if (ns < start_stored) {
            size_t ins = lb_start(s, ns);
            if (!store_grow(s, s->count + 1)) goto done_si;
            memmove(s->spans + ins + 1, s->spans + ins,
                    (s->count - ins) * sizeof(TextPropSpan));
            s->spans[ins] = (TextPropSpan){
                .start = ns, .end = start_stored,
                .props = prop_copy(snap), .face_id = FACE_DEFAULT
            };
            s->count++;
        }

        if (ne > end_stored) {
            size_t ins = lb_start(s, end_stored);
            if (!store_grow(s, s->count + 1)) goto done_si;
            memmove(s->spans + ins + 1, s->spans + ins,
                    (s->count - ins) * sizeof(TextPropSpan));
            s->spans[ins] = (TextPropSpan){
                .start = end_stored, .end = ne,
                .props = prop_copy(snap), .face_id = FACE_DEFAULT
            };
            s->count++;
        }
done_si:
        prop_free(snap);
    }
    free(snaps);

    /* Insert the merged center span */
    size_t ins = lb_start(s, start_stored);
    if (store_grow(s, s->count + 1)) {
        memmove(s->spans + ins + 1, s->spans + ins,
                (s->count - ins) * sizeof(TextPropSpan));
        s->spans[ins] = (TextPropSpan){
            .start   = start_stored,
            .end     = end_stored,
            .props   = merged_center,
            .face_id = FACE_DEFAULT
        };
        s->count++;
    } else {
        prop_free(merged_center);
    }
}

void textprop_put_fast(Buffer *buf, size_t start, size_t end,
                        SCM prop, SCM value) {
    if (!buf || start >= end) return;
    TextPropStore *s = &buf->props;
    if (!store_grow(s, s->count + 1)) return;
    /* Append (treesit emits in order) */
    s->spans[s->count++] = (TextPropSpan){
        .start = start, .end = end,
        .props = prop_set(NULL, prop, value),
        .face_id = FACE_DEFAULT
    };
}

void textprop_remove(Buffer *buf, size_t start, size_t end) {
    if (!buf || start >= end) return;
    TextPropStore *s = &buf->props;

    size_t vis_end = s->visible_end;
    if (s->pending_delta && s->dirty_from != SIZE_MAX && s->dirty_from < vis_end)
        flush_delta_upto(s, vis_end);

    size_t start_stored = true_to_stored(s, start);
    size_t end_stored   = true_to_stored(s, end);

    size_t first = lb_start(s, start_stored);
    if (first > 0 && s->spans[first - 1].end > start_stored) first--;

    typedef struct { size_t s, e; PropValue *p; } Snap;
    size_t cap = 8; Snap *snaps = malloc(cap * sizeof *snaps); size_t n_snaps = 0;
    if (!snaps) return;

    size_t last = first;
    while (last < s->count && s->spans[last].start < end_stored) {
        if (n_snaps >= cap) { cap *= 2; snaps = realloc(snaps, cap * sizeof *snaps); }
        snaps[n_snaps++] = (Snap){ s->spans[last].start, s->spans[last].end,
                                   s->spans[last].props };
        s->spans[last].props = NULL;
        last++;
    }

    s->count -= (last - first);
    memmove(s->spans + first, s->spans + last,
            (s->count - first) * sizeof(TextPropSpan));

    for (size_t si = 0; si < n_snaps; si++) {
        size_t ns = snaps[si].s, ne = snaps[si].e;  /* stored coords */
        PropValue *snap = snaps[si].p;

        if (ns < start_stored) {
            size_t ins = lb_start(s, ns);
            if (store_grow(s, s->count + 1)) {
                memmove(s->spans + ins + 1, s->spans + ins,
                        (s->count - ins) * sizeof(TextPropSpan));
                s->spans[ins] = (TextPropSpan){ .start=ns, .end=start_stored,
                    .props=prop_copy(snap), .face_id=FACE_DEFAULT };
                s->count++;
            }
        }
        if (ne > end_stored) {
            size_t ins = lb_start(s, end_stored);
            if (store_grow(s, s->count + 1)) {
                memmove(s->spans + ins + 1, s->spans + ins,
                        (s->count - ins) * sizeof(TextPropSpan));
                s->spans[ins] = (TextPropSpan){ .start=end_stored, .end=ne,
                    .props=prop_copy(snap), .face_id=FACE_DEFAULT };
                s->count++;
            }
        }
        prop_free(snap);
    }
    free(snaps);
}

/// Compatibility wrappers

void put_text_property(Buffer *buf, size_t start, size_t end, SCM prop, SCM value)
    { textprop_put(buf, start, end, prop, value); }

void put_text_property_fast(Buffer *buf, size_t start, size_t end, SCM prop, SCM value)
    { textprop_put_fast(buf, start, end, prop, value); }

SCM get_text_property(Buffer *buf, size_t pos, SCM prop)
    { return textprop_get(buf, pos, prop); }

int get_text_property_face(Buffer *buf, size_t pos)
    { return textprop_get_face(buf, pos); }

int get_face_id_and_next_change(Buffer *buf, size_t pos, size_t limit, size_t *nc)
    { return textprop_get_face_and_next_change(buf, pos, limit, nc); }

SCM get_text_property_field(Buffer *buf, size_t pos) {
    static SCM s_field = 0;
    if (!s_field) { s_field = scm_from_utf8_symbol("field"); scm_gc_protect_object(s_field); }
    return textprop_get(buf, pos, s_field);
}

size_t next_property_change(Buffer *buf, size_t pos)
    { return textprop_next_change(buf, pos); }

size_t previous_property_change(Buffer *buf, size_t pos)
    { return textprop_prev_change(buf, pos); }

size_t next_single_property_change(Buffer *buf, size_t pos, SCM prop)
    { return textprop_next_single_change(buf, pos, prop); }

size_t previous_single_property_change(Buffer *buf, size_t pos, SCM prop)
    { return textprop_prev_single_change(buf, pos, prop); }

void remove_text_properties(Buffer *buf, size_t start, size_t end)
    { textprop_remove(buf, start, end); }

void clear_text_properties(Buffer *buf) {
    textprop_store_clear(&buf->props);
}

bool is_range_readonly(Buffer *buf, size_t start, size_t end)
    { return textprop_is_readonly(buf, start, end); }

/// SCM

static SCM scm_put_text_property(SCM start, SCM end, SCM prop, SCM value) {
    if (!scm_is_integer(start)) scm_wrong_type_arg("put-text-property", 1, start);
    if (!scm_is_integer(end))   scm_wrong_type_arg("put-text-property", 2, end);
    if (current_buffer)
        put_text_property(current_buffer, scm_to_size_t(start), scm_to_size_t(end), prop, value);
    return SCM_UNSPECIFIED;
}

static SCM scm_get_text_property(SCM pos, SCM prop) {
    if (!scm_is_integer(pos)) scm_wrong_type_arg("get-text-property", 1, pos);
    if (!current_buffer) return SCM_BOOL_F;
    return get_text_property(current_buffer, scm_to_size_t(pos), prop);
}

static SCM scm_remove_text_properties(SCM start, SCM end, SCM props) {
    (void)props;
    if (!scm_is_integer(start)) scm_wrong_type_arg("remove-text-properties", 1, start);
    if (!scm_is_integer(end))   scm_wrong_type_arg("remove-text-properties", 2, end);
    if (current_buffer)
        remove_text_properties(current_buffer, scm_to_size_t(start), scm_to_size_t(end));
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


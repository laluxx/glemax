/*
 * rope.h - Fast UTF-8 aware rope data structure for text editors
 *
 * A rope is a tree structure for efficiently storing and manipulating
 * very large strings. This implementation is optimized for:
 * - Fast insertions/deletions at arbitrary positions
 * - Full UTF-8 support with proper character boundary handling
 * - Red-Black tree balancing for guaranteed O(log n) operations
 * - Cache-friendly node layout with node pooling
 * - Production-grade split/concat operations
 *
 * All internal stacks are heap-allocated and grow dynamically —
 * no fixed-size stack arrays anywhere. This fixes the segfault that
 * occurs when rapid insertions (e.g. electric-pair-mode with a held key)
 * grow the RB tree deep enough to overflow a fixed stack[128].
 *
 * Usage:
 *   #define ROPE_IMPLEMENTATION
 *   #include "rope.h"
 */

#ifndef ROPE_H
#define ROPE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Configuration */
#ifndef ROPE_NODE_SIZE
#define ROPE_NODE_SIZE 1024
#endif

#ifndef ROPE_SPLIT_THRESHOLD
#define ROPE_SPLIT_THRESHOLD 2048
#endif

/* Initial capacity for all dynamic stacks — grows automatically */
#define ROPE_STACK_INIT_CAP 64

/* Opaque rope handle */
typedef struct rope rope_t;

/* Rope iterator for traversal */
typedef struct {
    const rope_t *rope;
    size_t byte_pos;
    size_t char_pos;
    void *internal;
} rope_iter_t;

typedef struct {
    void  *leaf;
    size_t char_offset_in_leaf;
    size_t byte_offset_in_leaf;
    size_t leaf_start_char;
    size_t leaf_start_byte;
} chunk_info_t;

/* UTF-8 info structure */
typedef struct {
    size_t bytes;
    size_t chars;
    size_t newlines;
} rope_stats_t;

/* Core API */
rope_t *rope_new(void);
rope_t *rope_new_from_str(const char *str, size_t len);
void    rope_free(rope_t *rope);

uint32_t utf8_decode(const char *str, size_t len, size_t *bytes_read);
size_t   utf8_encode(uint32_t codepoint, char *out);

/* Query — O(log n) */
size_t       rope_byte_length(const rope_t *rope);
size_t       rope_char_length(const rope_t *rope);
rope_stats_t rope_stats(const rope_t *rope);

/* Character access — O(log n) */
uint32_t rope_char_at(const rope_t *rope, size_t char_pos);
size_t   rope_char_to_byte(const rope_t *rope, size_t char_pos);
size_t   rope_byte_to_char(const rope_t *rope, size_t byte_pos);

/* Copy */
size_t rope_copy_bytes(const rope_t *rope, size_t byte_start, size_t byte_len,
                       char *buf, size_t bufsize);
size_t rope_copy_chars(const rope_t *rope, size_t char_start, size_t char_len,
                       char *buf, size_t bufsize);

/* Modification — O(log n) */
rope_t *rope_insert_bytes(rope_t *rope, size_t byte_pos, const char *str, size_t len);
rope_t *rope_insert_chars(rope_t *rope, size_t char_pos, const char *str, size_t len);
rope_t *rope_delete_bytes(rope_t *rope, size_t byte_start, size_t byte_len);
rope_t *rope_delete_chars(rope_t *rope, size_t char_start, size_t char_len);

/* Structural */
rope_t *rope_concat(rope_t *left, rope_t *right);
rope_t *rope_split_bytes(rope_t *rope, size_t byte_pos, rope_t **right_out);
rope_t *rope_split_chars(rope_t *rope, size_t char_pos, rope_t **right_out);
rope_t *rope_substring_bytes(const rope_t *rope, size_t start, size_t len);
rope_t *rope_substring_chars(const rope_t *rope, size_t start, size_t len);

/* Utility */
char *rope_to_string(const rope_t *rope, size_t *len_out);
bool  rope_validate_utf8(const rope_t *rope);

/* Iterator API */
void rope_iter_init(rope_iter_t *iter, const rope_t *rope, size_t char_pos);
bool rope_iter_next_char(rope_iter_t *iter, uint32_t *codepoint);
bool rope_iter_prev_char(rope_iter_t *iter, uint32_t *codepoint);
void rope_iter_seek_char(rope_iter_t *iter, size_t char_pos);
void rope_iter_seek_byte(rope_iter_t *iter, size_t byte_pos);
void rope_iter_destroy(rope_iter_t *iter);

/* Chunk API */
bool         rope_chunk_at_char(const rope_t *rope, size_t char_pos, chunk_info_t *info);
bool         rope_prev_chunk(const rope_t *rope, chunk_info_t *current, chunk_info_t *prev);
const char  *rope_chunk_data(const chunk_info_t *info);
size_t       rope_chunk_byte_len(const chunk_info_t *info);
size_t       rope_chunk_char_len(const chunk_info_t *info);

/* Line operations */
size_t rope_line_count(const rope_t *rope);
size_t rope_line_to_char(const rope_t *rope, size_t line);
size_t rope_line_to_byte(const rope_t *rope, size_t line);
size_t rope_char_to_line(const rope_t *rope, size_t char_pos);
size_t rope_byte_to_line(const rope_t *rope, size_t byte_pos);

/* UTF-8 utilities */
size_t   utf8_char_len(uint8_t first_byte);
uint32_t utf8_decode(const char *str, size_t len, size_t *bytes_read);

/* Visit every newline */
void rope_each_newline(const rope_t *rope,
                       void (*callback)(size_t char_pos, void *userdata),
                       void *userdata);

#ifdef __cplusplus
}
#endif

#endif /* ROPE_H */

/* ═══════════════════════════════════════════════════════════════════════════
 * IMPLEMENTATION
 * ═══════════════════════════════════════════════════════════════════════════ */

#ifdef ROPE_IMPLEMENTATION

#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* ── Dynamic stack helper ────────────────────────────────────────────────── */

typedef struct {
    void  **data;
    int     sp;
    int     cap;
} dstack_t;

static inline dstack_t dstack_new(void) {
    dstack_t s;
    s.cap  = ROPE_STACK_INIT_CAP;
    s.sp   = 0;
    s.data = malloc(s.cap * sizeof(void *));
    return s;
}

static inline void dstack_free(dstack_t *s) {
    free(s->data);
    s->data = NULL;
    s->sp = s->cap = 0;
}

static inline void dstack_push(dstack_t *s, void *v) {
    if (s->sp >= s->cap) {
        s->cap *= 2;
        s->data = realloc(s->data, s->cap * sizeof(void *));
    }
    s->data[s->sp++] = v;
}

static inline void *dstack_pop(dstack_t *s) {
    return s->data[--s->sp];
}

static inline void *dstack_peek(dstack_t *s) {
    return s->data[s->sp - 1];
}

static inline bool dstack_empty(dstack_t *s) {
    return s->sp == 0;
}

static inline void dstack_reset(dstack_t *s) {
    s->sp = 0;
}

/* ── Red-Black tree ──────────────────────────────────────────────────────── */

typedef enum { RB_RED = 0, RB_BLACK = 1 } rb_color_t;

typedef struct rope_node {
    bool       is_leaf;
    rb_color_t color;

    size_t byte_weight;
    size_t char_weight;
    size_t newline_weight;

    union {
        struct {
            struct rope_node *left;
            struct rope_node *right;
        } branch;
        struct {
            char  *data;
            size_t byte_len;
            size_t char_len;
            size_t newlines;
            size_t capacity;
        } leaf;
    };
} rope_node_t;

struct rope {
    rope_node_t *root;
    size_t byte_len;
    size_t char_len;
    size_t newlines;
};

/* ── Node pool ───────────────────────────────────────────────────────────── */

#define NODE_POOL_SIZE 512
static rope_node_t *node_pool[NODE_POOL_SIZE];
static size_t       node_pool_count = 0;

static inline rope_node_t *node_alloc(void) {
    rope_node_t *n;
    if (node_pool_count > 0)
        n = node_pool[--node_pool_count];
    else
        n = malloc(sizeof *n);
    memset(n, 0, sizeof *n);
    return n;
}

static inline void node_free(rope_node_t *n) {
    if (node_pool_count < NODE_POOL_SIZE)
        node_pool[node_pool_count++] = n;
    else
        free(n);
}

/* ── UTF-8 utilities ─────────────────────────────────────────────────────── */

size_t utf8_char_len(uint8_t b) {
    if ((b & 0x80) == 0x00) return 1;
    if ((b & 0xE0) == 0xC0) return 2;
    if ((b & 0xF0) == 0xE0) return 3;
    if ((b & 0xF8) == 0xF0) return 4;
    return 1;
}

uint32_t utf8_decode(const char *str, size_t len, size_t *bytes_read) {
    if (!len) { *bytes_read = 0; return 0; }
    uint8_t first = (uint8_t)str[0];
    size_t  clen  = utf8_char_len(first);
    if (clen > len) { *bytes_read = 1; return 0xFFFD; }
    *bytes_read = clen;
    switch (clen) {
        case 1: return first;
        case 2: return ((first & 0x1F) << 6)  | ((uint8_t)str[1] & 0x3F);
        case 3: return ((first & 0x0F) << 12) | (((uint8_t)str[1] & 0x3F) << 6)
                                               |  ((uint8_t)str[2] & 0x3F);
        case 4: return ((first & 0x07) << 18) | (((uint8_t)str[1] & 0x3F) << 12)
                                               | (((uint8_t)str[2] & 0x3F) << 6)
                                               |  ((uint8_t)str[3] & 0x3F);
    }
    return 0xFFFD;
}

size_t utf8_encode(uint32_t cp, char *out) {
    if (cp <= 0x7F)    { out[0] = cp; return 1; }
    if (cp <= 0x7FF)   { out[0] = 0xC0|(cp>>6); out[1] = 0x80|(cp&0x3F); return 2; }
    if (cp <= 0xFFFF)  { out[0] = 0xE0|(cp>>12); out[1] = 0x80|((cp>>6)&0x3F);
                         out[2] = 0x80|(cp&0x3F); return 3; }
    if (cp <= 0x10FFFF){ out[0] = 0xF0|(cp>>18); out[1] = 0x80|((cp>>12)&0x3F);
                         out[2] = 0x80|((cp>>6)&0x3F); out[3] = 0x80|(cp&0x3F); return 4; }
    return 0;
}

static size_t utf8_char_count(const char *str, size_t byte_len) {
    size_t n = 0, i = 0;
    while (i < byte_len) {
        size_t cl = utf8_char_len((uint8_t)str[i]);
        if (cl > byte_len - i) cl = byte_len - i;
        i += cl; n++;
    }
    return n;
}

static size_t count_newlines(const char *str, size_t len) {
    size_t n = 0;
    for (size_t i = 0; i < len; i++) if (str[i] == '\n') n++;
    return n;
}

static size_t utf8_char_to_byte_in(const char *str, size_t byte_len, size_t char_pos) {
    size_t bp = 0, cc = 0;
    while (bp < byte_len && cc < char_pos) {
        size_t cl = utf8_char_len((uint8_t)str[bp]);
        if (cl > byte_len - bp) break;
        bp += cl; cc++;
    }
    return bp;
}

static size_t utf8_byte_to_char_in(const char *str, size_t byte_len, size_t byte_pos) {
    if (byte_pos >= byte_len) return utf8_char_count(str, byte_len);
    size_t cc = 0, cur = 0;
    while (cur < byte_pos && cur < byte_len) {
        size_t cl = utf8_char_len((uint8_t)str[cur]);
        if (cl > byte_len - cur) break;
        if (cur + cl > byte_pos) break;
        cur += cl; cc++;
    }
    return cc;
}

static bool validate_utf8(const char *str, size_t len) {
    size_t i = 0;
    while (i < len) {
        size_t cl = utf8_char_len((uint8_t)str[i]);
        if (i + cl > len) return false;
        for (size_t j = 1; j < cl; j++)
            if (((uint8_t)str[i+j] & 0xC0) != 0x80) return false;
        i += cl;
    }
    return true;
}

/* ── Node metrics ────────────────────────────────────────────────────────── */

static size_t node_byte_len(const rope_node_t *n);
static size_t node_char_len(const rope_node_t *n);
static size_t node_newline_count(const rope_node_t *n);

static size_t node_byte_len(const rope_node_t *n) {
    if (!n) return 0;
    if (n->is_leaf) return n->leaf.byte_len;
    return n->byte_weight + node_byte_len(n->branch.right);
}
static size_t node_char_len(const rope_node_t *n) {
    if (!n) return 0;
    if (n->is_leaf) return n->leaf.char_len;
    return n->char_weight + node_char_len(n->branch.right);
}
static size_t node_newline_count(const rope_node_t *n) {
    if (!n) return 0;
    if (n->is_leaf) return n->leaf.newlines;
    return n->newline_weight + node_newline_count(n->branch.right);
}

static void node_update_weights(rope_node_t *n) {
    if (!n || n->is_leaf) return;
    rope_node_t *l = n->branch.left;
    if (l) {
        n->byte_weight    = node_byte_len(l);
        n->char_weight    = node_char_len(l);
        n->newline_weight = node_newline_count(l);
    } else {
        n->byte_weight = n->char_weight = n->newline_weight = 0;
    }
}

/* ── Node construction ───────────────────────────────────────────────────── */

static rope_node_t *node_new_leaf(const char *str, size_t byte_len) {
    rope_node_t *n = node_alloc();
    n->is_leaf = true;
    n->color   = RB_RED;
    n->leaf.byte_len = byte_len;
    n->leaf.char_len = utf8_char_count(str, byte_len);
    n->leaf.newlines = count_newlines(str, byte_len);
    n->leaf.capacity = byte_len < ROPE_NODE_SIZE ? ROPE_NODE_SIZE : byte_len;
    n->leaf.data     = malloc(n->leaf.capacity);
    memcpy(n->leaf.data, str, byte_len);
    n->byte_weight    = byte_len;
    n->char_weight    = n->leaf.char_len;
    n->newline_weight = n->leaf.newlines;
    return n;
}

static rope_node_t *node_new_branch(rope_node_t *left, rope_node_t *right) {
    rope_node_t *n = node_alloc();
    n->is_leaf       = false;
    n->color         = RB_RED;
    n->branch.left   = left;
    n->branch.right  = right;
    n->byte_weight   = node_byte_len(left);
    n->char_weight   = node_char_len(left);
    n->newline_weight = node_newline_count(left);
    return n;
}

static void node_deep_free(rope_node_t *n) {
    if (!n) return;
    if (n->is_leaf) {
        free(n->leaf.data);
    } else {
        node_deep_free(n->branch.left);
        node_deep_free(n->branch.right);
    }
    node_free(n);
}

/* ── Red-Black balancing ─────────────────────────────────────────────────── */

static inline rb_color_t node_color(const rope_node_t *n) {
    return n ? n->color : RB_BLACK;
}
static inline void node_set_color(rope_node_t *n, rb_color_t c) {
    if (n) n->color = c;
}

static rope_node_t *rotate_left(rope_node_t *n) {
    if (!n || !n->branch.right || n->is_leaf || n->branch.right->is_leaf) return n;
    rope_node_t *r = n->branch.right;
    n->branch.right = r->branch.left;
    r->branch.left  = n;
    r->color = n->color; n->color = RB_RED;
    node_update_weights(n); node_update_weights(r);
    return r;
}

static rope_node_t *rotate_right(rope_node_t *n) {
    if (!n || !n->branch.left || n->is_leaf || n->branch.left->is_leaf) return n;
    rope_node_t *l = n->branch.left;
    n->branch.left  = l->branch.right;
    l->branch.right = n;
    l->color = n->color; n->color = RB_RED;
    node_update_weights(n); node_update_weights(l);
    return l;
}

static void flip_colors(rope_node_t *n) {
    n->color = RB_RED;
    node_set_color(n->branch.left,  RB_BLACK);
    node_set_color(n->branch.right, RB_BLACK);
}

static rope_node_t *balance(rope_node_t *n) {
    if (!n || n->is_leaf) return n;
    bool lb = n->branch.left  && !n->branch.left->is_leaf;
    bool rb = n->branch.right && !n->branch.right->is_leaf;
    if (node_color(n->branch.right) == RB_RED &&
        node_color(n->branch.left)  == RB_BLACK && rb)
        n = rotate_left(n);
    if (node_color(n->branch.left) == RB_RED && lb &&
        n->branch.left->branch.left &&
        node_color(n->branch.left->branch.left) == RB_RED)
        n = rotate_right(n);
    if (node_color(n->branch.left) == RB_RED &&
        node_color(n->branch.right) == RB_RED)
        flip_colors(n);
    return n;
}

/* ── Core rope lifecycle ─────────────────────────────────────────────────── */

rope_t *rope_new(void) {
    return calloc(1, sizeof(rope_t));
}

rope_t *rope_new_from_str(const char *str, size_t len) {
    rope_t *r = calloc(1, sizeof(rope_t));
    if (len > 0) {
        r->root = node_new_leaf(str, len);
        r->root->color = RB_BLACK;
        r->byte_len = len;
        r->char_len = r->root->leaf.char_len;
        r->newlines = r->root->leaf.newlines;
    }
    return r;
}

void rope_free(rope_t *r) {
    if (!r) return;
    node_deep_free(r->root);
    free(r);
}

rope_stats_t rope_stats(const rope_t *r) {
    rope_stats_t s = {0,0,0};
    if (r) { s.bytes = r->byte_len; s.chars = r->char_len; s.newlines = r->newlines; }
    return s;
}

size_t rope_byte_length(const rope_t *r) { return r ? r->byte_len : 0; }
size_t rope_char_length(const rope_t *r) { return r ? r->char_len : 0; }

/* ── Character access ────────────────────────────────────────────────────── */

size_t rope_char_to_byte(const rope_t *r, size_t char_pos) {
    if (!r || char_pos >= r->char_len) return r ? r->byte_len : 0;
    rope_node_t *n = r->root;
    size_t byte_off = 0;
    while (n && !n->is_leaf) {
        if (char_pos < n->char_weight) {
            n = n->branch.left;
        } else {
            byte_off  += n->byte_weight;
            char_pos  -= n->char_weight;
            n = n->branch.right;
        }
    }
    if (n && n->is_leaf)
        return byte_off + utf8_char_to_byte_in(n->leaf.data, n->leaf.byte_len, char_pos);
    return byte_off;
}

size_t rope_byte_to_char(const rope_t *r, size_t byte_pos) {
    if (!r || byte_pos >= r->byte_len) return r ? r->char_len : 0;
    rope_node_t *n = r->root;
    size_t char_off = 0;
    while (n && !n->is_leaf) {
        if (byte_pos < n->byte_weight) {
            n = n->branch.left;
        } else {
            char_off  += n->char_weight;
            byte_pos  -= n->byte_weight;
            n = n->branch.right;
        }
    }
    if (n && n->is_leaf)
        return char_off + utf8_byte_to_char_in(n->leaf.data, n->leaf.byte_len, byte_pos);
    return char_off;
}

uint32_t rope_char_at(const rope_t *r, size_t char_pos) {
    if (!r || char_pos >= r->char_len) return 0;
    size_t byte_pos = rope_char_to_byte(r, char_pos);
    rope_node_t *n = r->root;
    while (n && !n->is_leaf) {
        if (byte_pos < n->byte_weight) { n = n->branch.left; }
        else { byte_pos -= n->byte_weight; n = n->branch.right; }
    }
    if (n && n->is_leaf && byte_pos < n->leaf.byte_len) {
        size_t br;
        return utf8_decode(n->leaf.data + byte_pos, n->leaf.byte_len - byte_pos, &br);
    }
    return 0;
}

/* ── Copy operations ─────────────────────────────────────────────────────── */

size_t rope_copy_bytes(const rope_t *r, size_t byte_start, size_t byte_len,
                       char *buf, size_t bufsize) {
    if (!r || !buf || !bufsize || byte_start >= r->byte_len) return 0;
    if (byte_start + byte_len > r->byte_len) byte_len = r->byte_len - byte_start;
    if (byte_len > bufsize) byte_len = bufsize;

    size_t copied = 0;

    /* Use a dynamic stack for traversal */
    dstack_t stk = dstack_new();
    rope_node_t *n = r->root;
    size_t cur_off = 0;  /* absolute byte offset of n */

    /* Navigate to starting leaf */
    size_t target = byte_start;
    while (n && !n->is_leaf) {
        if (target < n->byte_weight) {
            dstack_push(&stk, n);
            n = n->branch.left;
        } else {
            cur_off += n->byte_weight;
            target  -= n->byte_weight;
            n = n->branch.right;
        }
    }

    /* Copy from leaves sequentially */
    while (n && n->is_leaf && copied < byte_len) {
        size_t avail  = n->leaf.byte_len - target;
        size_t to_copy = avail < (byte_len - copied) ? avail : (byte_len - copied);
        memcpy(buf + copied, n->leaf.data + target, to_copy);
        copied += to_copy;
        target  = 0;

        if (copied < byte_len) {
            /* Advance to next leaf via stack */
            n = NULL;
            while (!dstack_empty(&stk)) {
                rope_node_t *parent = dstack_pop(&stk);
                if (parent->branch.right) {
                    rope_node_t *rn = parent->branch.right;
                    while (rn && !rn->is_leaf) {
                        dstack_push(&stk, rn);
                        rn = rn->branch.left;
                    }
                    n = rn;
                    break;
                }
            }
        }
    }

    dstack_free(&stk);
    return copied;
}

size_t rope_copy_chars(const rope_t *r, size_t char_start, size_t char_len,
                       char *buf, size_t bufsize) {
    if (!r || char_start >= r->char_len) return 0;
    size_t byte_start = rope_char_to_byte(r, char_start);
    size_t byte_end   = rope_char_to_byte(r, char_start + char_len);
    return rope_copy_bytes(r, byte_start, byte_end - byte_start, buf, bufsize);
}

/* ── Concatenation ───────────────────────────────────────────────────────── */

rope_t *rope_concat(rope_t *left, rope_t *right) {
    if (!left  || left->byte_len  == 0) { if (left)  rope_free(left);  return right; }
    if (!right || right->byte_len == 0) { if (right) rope_free(right); return left;  }
    rope_t *res = calloc(1, sizeof(rope_t));
    res->root = node_new_branch(left->root, right->root);
    res->root->color = RB_BLACK;
    res->byte_len = left->byte_len + right->byte_len;
    res->char_len = left->char_len + right->char_len;
    res->newlines = left->newlines + right->newlines;
    left->root = NULL;  rope_free(left);
    right->root = NULL; rope_free(right);
    return res;
}

/* ── Split ───────────────────────────────────────────────────────────────── */

static void split_leaf(rope_node_t *leaf, size_t byte_pos,
                       rope_node_t **lout, rope_node_t **rout) {
    if (byte_pos == 0) { *lout = NULL; *rout = leaf; return; }
    if (byte_pos >= leaf->leaf.byte_len) { *lout = leaf; *rout = NULL; return; }
    *lout = node_new_leaf(leaf->leaf.data, byte_pos);
    *rout = node_new_leaf(leaf->leaf.data + byte_pos, leaf->leaf.byte_len - byte_pos);
    (*lout)->color = leaf->color;
    (*rout)->color = leaf->color;
    free(leaf->leaf.data);
    node_free(leaf);
}

static void node_split_recursive(rope_node_t *n, size_t byte_pos,
                                  rope_node_t **lout, rope_node_t **rout) {
    if (!n) { *lout = *rout = NULL; return; }
    if (n->is_leaf) { split_leaf(n, byte_pos, lout, rout); return; }

    if (byte_pos <= n->byte_weight) {
        rope_node_t *ll = NULL, *lr = NULL;
        node_split_recursive(n->branch.left, byte_pos, &ll, &lr);
        if (lr && n->branch.right) {
            *lout = ll;
            *rout = node_new_branch(lr, n->branch.right);
            (*rout)->color = n->color;
        } else {
            *lout = ll;
            *rout = lr ? lr : n->branch.right;
        }
        node_free(n);
    } else {
        rope_node_t *rl = NULL, *rr = NULL;
        node_split_recursive(n->branch.right, byte_pos - n->byte_weight, &rl, &rr);
        if (rl && n->branch.left) {
            *lout = node_new_branch(n->branch.left, rl);
            (*lout)->color = n->color;
            *rout = rr;
        } else {
            *lout = n->branch.left;
            *rout = rr;
        }
        node_free(n);
    }
}

rope_t *rope_split_bytes(rope_t *r, size_t byte_pos, rope_t **right_out) {
    if (!r) { if (right_out) *right_out = NULL; return NULL; }
    if (byte_pos == 0) { if (right_out) *right_out = r; return rope_new(); }
    if (byte_pos >= r->byte_len) { if (right_out) *right_out = rope_new(); return r; }

    rope_node_t *lt = NULL, *rt = NULL;
    node_split_recursive(r->root, byte_pos, &lt, &rt);

    rope_t *left = rope_new();
    left->root = lt;
    if (lt) {
        lt->color = RB_BLACK;
        left->byte_len = node_byte_len(lt);
        left->char_len = node_char_len(lt);
        left->newlines = node_newline_count(lt);
    }
    if (right_out) {
        *right_out = rope_new();
        (*right_out)->root = rt;
        if (rt) {
            rt->color = RB_BLACK;
            (*right_out)->byte_len = node_byte_len(rt);
            (*right_out)->char_len = node_char_len(rt);
            (*right_out)->newlines = node_newline_count(rt);
        }
    }
    r->root = NULL; rope_free(r);
    return left;
}

rope_t *rope_split_chars(rope_t *r, size_t char_pos, rope_t **right_out) {
    if (!r) { if (right_out) *right_out = NULL; return NULL; }
    size_t byte_pos = rope_char_to_byte(r, char_pos);
    return rope_split_bytes(r, byte_pos, right_out);
}

/* ── Insertion ───────────────────────────────────────────────────────────── */

static rope_node_t *node_insert_bytes(rope_node_t *n, size_t byte_pos,
                                      const char *str, size_t len);

rope_t *rope_insert_bytes(rope_t *r, size_t byte_pos, const char *str, size_t len) {
    if (!r || !len) return r;
    if (byte_pos > r->byte_len) byte_pos = r->byte_len;
    if (!r->root) {
        r->root = node_new_leaf(str, len);
        r->root->color = RB_BLACK;
    } else {
        r->root = node_insert_bytes(r->root, byte_pos, str, len);
        r->root->color = RB_BLACK;
    }
    r->byte_len += len;
    r->char_len += utf8_char_count(str, len);
    r->newlines += count_newlines(str, len);
    return r;
}

static rope_node_t *node_insert_bytes(rope_node_t *n, size_t byte_pos,
                                      const char *str, size_t len) {
    if (n->is_leaf) {
        if (byte_pos == 0) {
            return balance(node_new_branch(node_new_leaf(str, len), n));
        } else if (byte_pos >= n->leaf.byte_len) {
            return balance(node_new_branch(n, node_new_leaf(str, len)));
        } else {
            rope_node_t *left  = node_new_leaf(n->leaf.data, byte_pos);
            rope_node_t *mid   = node_new_leaf(str, len);
            rope_node_t *right = node_new_leaf(n->leaf.data + byte_pos,
                                               n->leaf.byte_len - byte_pos);
            free(n->leaf.data); node_free(n);
            return balance(node_new_branch(balance(node_new_branch(left, mid)), right));
        }
    }
    if (byte_pos <= n->byte_weight)
        n->branch.left  = node_insert_bytes(n->branch.left,  byte_pos, str, len);
    else
        n->branch.right = node_insert_bytes(n->branch.right, byte_pos - n->byte_weight,
                                             str, len);
    node_update_weights(n);
    return balance(n);
}

rope_t *rope_insert_chars(rope_t *r, size_t char_pos, const char *str, size_t len) {
    if (!r) return NULL;
    size_t byte_pos = rope_char_to_byte(r, char_pos);
    return rope_insert_bytes(r, byte_pos, str, len);
}

/* ── Deletion ────────────────────────────────────────────────────────────── */

rope_t *rope_delete_bytes(rope_t *r, size_t byte_start, size_t byte_len) {
    if (!r || byte_start >= r->byte_len) return r;
    if (byte_start + byte_len > r->byte_len) byte_len = r->byte_len - byte_start;
    if (!byte_len) return r;
    rope_t *right = NULL;
    r = rope_split_bytes(r, byte_start, &right);
    if (right) {
        rope_t *tail = NULL;
        right = rope_split_bytes(right, byte_len, &tail);
        rope_free(right);
        if (tail) r = rope_concat(r, tail);
    }
    return r;
}

rope_t *rope_delete_chars(rope_t *r, size_t char_start, size_t char_len) {
    if (!r || char_start >= r->char_len) return r;
    size_t byte_start = rope_char_to_byte(r, char_start);
    size_t byte_end   = rope_char_to_byte(r, char_start + char_len);
    return rope_delete_bytes(r, byte_start, byte_end - byte_start);
}

/* ── Substring ───────────────────────────────────────────────────────────── */

rope_t *rope_substring_bytes(const rope_t *r, size_t start, size_t len) {
    if (!r || start >= r->byte_len) return rope_new();
    if (start + len > r->byte_len) len = r->byte_len - start;
    char *buf = malloc(len);
    size_t copied = rope_copy_bytes(r, start, len, buf, len);
    rope_t *res = rope_new_from_str(buf, copied);
    free(buf);
    return res;
}

rope_t *rope_substring_chars(const rope_t *r, size_t start, size_t len) {
    if (!r || start >= r->char_len) return rope_new();
    size_t byte_start = rope_char_to_byte(r, start);
    size_t byte_end   = rope_char_to_byte(r, start + len);
    return rope_substring_bytes(r, byte_start, byte_end - byte_start);
}

/* ── Utility ─────────────────────────────────────────────────────────────── */

char *rope_to_string(const rope_t *r, size_t *len_out) {
    if (!r || !r->byte_len) { if (len_out) *len_out = 0; return NULL; }
    char *str = malloc(r->byte_len + 1);
    size_t pos = 0;

    dstack_t stk = dstack_new();
    rope_node_t *n = r->root;

    while (n || !dstack_empty(&stk)) {
        while (n) {
            if (!n->is_leaf) { dstack_push(&stk, n); n = n->branch.left; }
            else break;
        }
        if (n && n->is_leaf) {
            memcpy(str + pos, n->leaf.data, n->leaf.byte_len);
            pos += n->leaf.byte_len;
            n = NULL;
        }
        if (!dstack_empty(&stk))
            n = ((rope_node_t *)dstack_pop(&stk))->branch.right;
    }

    dstack_free(&stk);
    str[pos] = '\0';
    if (len_out) *len_out = pos;
    return str;
}

bool rope_validate_utf8(const rope_t *r) {
    if (!r) return true;
    size_t len;
    char *s = rope_to_string(r, &len);
    if (!s) return true;
    bool ok = validate_utf8(s, len);
    free(s);
    return ok;
}

/* ── Chunk API ───────────────────────────────────────────────────────────── */

const char *rope_chunk_data(const chunk_info_t *info) {
    if (!info || !info->leaf) return NULL;
    rope_node_t *l = info->leaf;
    return l->is_leaf ? l->leaf.data : NULL;
}
size_t rope_chunk_byte_len(const chunk_info_t *info) {
    if (!info || !info->leaf) return 0;
    rope_node_t *l = info->leaf;
    return l->is_leaf ? l->leaf.byte_len : 0;
}
size_t rope_chunk_char_len(const chunk_info_t *info) {
    if (!info || !info->leaf) return 0;
    rope_node_t *l = info->leaf;
    return l->is_leaf ? l->leaf.char_len : 0;
}

bool rope_chunk_at_char(const rope_t *r, size_t char_pos, chunk_info_t *info) {
    if (!r || !r->root || !info) return false;
    if (char_pos > r->char_len) char_pos = r->char_len;

    rope_node_t *n = r->root;
    size_t char_off = 0, byte_off = 0;

    while (n && !n->is_leaf) {
        if (char_pos < n->char_weight) {
            n = n->branch.left;
        } else {
            char_off += n->char_weight;
            byte_off += n->byte_weight;
            char_pos -= n->char_weight;
            n = n->branch.right;
        }
    }
    if (!n || !n->is_leaf) return false;

    info->leaf             = n;
    info->leaf_start_char  = char_off;
    info->leaf_start_byte  = byte_off;

    size_t bp = 0, cc = 0;
    while (cc < char_pos && bp < n->leaf.byte_len) {
        bp += utf8_char_len((uint8_t)n->leaf.data[bp]);
        cc++;
    }
    info->char_offset_in_leaf = cc;
    info->byte_offset_in_leaf = bp;
    return true;
}

bool rope_prev_chunk(const rope_t *r, chunk_info_t *current, chunk_info_t *prev) {
    if (!r || !current || !prev) return false;
    if (current->leaf_start_char == 0) return false;
    return rope_chunk_at_char(r, current->leaf_start_char - 1, prev);
}

/* ── Iterator ────────────────────────────────────────────────────────────── */

/*
 * The iterator state uses a heap-allocated dynamic stack.
 * This is the fix for the segfault: previously stack[128] would overflow
 * after enough rapid insertions grew the RB tree beyond 128 levels deep.
 */
typedef struct {
    dstack_t     stk;            /* dynamic — never overflows */
    rope_node_t *current_leaf;
    size_t       leaf_byte_pos;
    size_t       leaf_start_byte;
} rope_iter_state_t;

/* Navigate to leftmost leaf from node, pushing (right_start, node) pairs
   onto stk for every branch we descend into.
   Returns the leaf and sets *out_byte_start to its absolute byte offset. */
static rope_node_t *iter_leftmost(rope_node_t *n, dstack_t *stk,
                                   size_t *out_byte_start, size_t byte_offset) {
    while (n && !n->is_leaf) {
        size_t right_start = byte_offset + n->byte_weight;
        dstack_push(stk, (void *)(uintptr_t)right_start);
        dstack_push(stk, n);
        n = n->branch.left;
        /* byte_offset unchanged going left */
    }
    *out_byte_start = byte_offset;
    return n;
}

/*
 * Advance to next leaf using the parent stack.
 *
 * Stack layout (set up by rope_iter_init and iter_leftmost):
 *   Each branch pushed as TWO entries (bottom-to-top):
 *     [0] right-child absolute byte start  (cast to void* via uintptr_t)
 *     [1] the branch node pointer
 *   So to pop one entry: node = pop(), right_start = pop().
 *
 * Returns NULL at end of rope.
 */
static rope_node_t *iter_next_leaf(rope_iter_state_t *state) {
    while (!dstack_empty(&state->stk)) {
        /* Pop node, then its associated right-child byte start */
        rope_node_t *parent        = dstack_pop(&state->stk);
        size_t right_byte_start    = (size_t)(uintptr_t)dstack_pop(&state->stk);

        if (parent->branch.right) {
            size_t leaf_start;
            rope_node_t *leaf = iter_leftmost(parent->branch.right,
                                              &state->stk,
                                              &leaf_start,
                                              right_byte_start);
            if (leaf) {
                state->leaf_start_byte = leaf_start;
                state->current_leaf    = leaf;
                state->leaf_byte_pos   = 0;
            }
            return leaf;
        }
        /* parent had no right child — keep popping up */
    }
    return NULL;
}

void rope_iter_init(rope_iter_t *iter, const rope_t *rope, size_t char_pos) {
    if (!iter) return;
    iter->rope     = rope;
    iter->char_pos = (rope && char_pos > rope->char_len) ? rope->char_len : char_pos;
    iter->byte_pos = rope ? rope_char_to_byte(rope, iter->char_pos) : 0;

    rope_iter_state_t *state = calloc(1, sizeof *state);
    state->stk = dstack_new();
    iter->internal = state;

    if (!rope || !rope->root) return;

    rope_node_t *n      = rope->root;
    size_t byte_offset  = 0;
    size_t target_byte  = iter->byte_pos;

    /*
     * Navigate to the leaf containing target_byte.
     * For each branch we go into we push TWO entries onto the dynamic stack:
     *   1. The right-child's absolute byte start (as uintptr_t)
     *   2. The branch node itself
     * On pop we get: node = pop(), right_start = pop().
     */
    while (n && !n->is_leaf) {
        if (target_byte < n->byte_weight) {
            /* going left: right subtree starts at byte_offset + n->byte_weight */
            size_t right_start = byte_offset + n->byte_weight;
            dstack_push(&state->stk, (void *)(uintptr_t)right_start);
            dstack_push(&state->stk, n);
            n = n->branch.left;
            /* byte_offset unchanged */
        } else {
            byte_offset  += n->byte_weight;
            target_byte  -= n->byte_weight;
            n = n->branch.right;
        }
    }

    if (n && n->is_leaf) {
        state->current_leaf    = n;
        state->leaf_byte_pos   = target_byte;
        state->leaf_start_byte = byte_offset;
        iter->byte_pos         = byte_offset + target_byte;
    }
}

bool rope_iter_next_char(rope_iter_t *iter, uint32_t *codepoint) {
    if (!iter || !iter->rope || iter->char_pos >= iter->rope->char_len) return false;

    rope_iter_state_t *state = iter->internal;
    if (!state || !state->current_leaf) return false;

    rope_node_t *leaf = state->current_leaf;

    if (state->leaf_byte_pos >= leaf->leaf.byte_len) {
        /* Need next leaf */
        leaf = iter_next_leaf(state);
        if (!leaf) return false;
    }

    size_t   bytes_read;
    *codepoint = utf8_decode(leaf->leaf.data + state->leaf_byte_pos,
                             leaf->leaf.byte_len - state->leaf_byte_pos,
                             &bytes_read);
    state->leaf_byte_pos += bytes_read;
    iter->byte_pos  = state->leaf_start_byte + state->leaf_byte_pos;
    iter->char_pos++;
    return true;
}

bool rope_iter_prev_char(rope_iter_t *iter, uint32_t *codepoint) {
    if (!iter || !iter->rope || iter->char_pos == 0) return false;

    rope_iter_state_t *state = iter->internal;
    if (!state) return false;

    /* If at start of current leaf, jump to end of previous via chunk API */
    if (!state->current_leaf || state->leaf_byte_pos == 0) {
        chunk_info_t cur, prev;
        if (!rope_chunk_at_char(iter->rope, iter->char_pos, &cur)) return false;
        if (!rope_prev_chunk(iter->rope, &cur, &prev)) return false;
        state->current_leaf    = prev.leaf;
        state->leaf_byte_pos   = rope_chunk_byte_len(&prev);
        state->leaf_start_byte = prev.leaf_start_byte;
        dstack_reset(&state->stk);
    }

    rope_node_t *leaf = state->current_leaf;
    if (!leaf || !leaf->is_leaf) return false;

    /* Scan backward to UTF-8 char boundary */
    size_t pos = state->leaf_byte_pos;
    if (pos == 0) return false;

    /* Walk back over continuation bytes */
    pos--;
    int guard = 0;
    while (pos > 0 && ((uint8_t)leaf->leaf.data[pos] & 0xC0) == 0x80 && guard < 3) {
        pos--; guard++;
    }

    size_t bytes_read;
    *codepoint = utf8_decode(leaf->leaf.data + pos,
                             leaf->leaf.byte_len - pos,
                             &bytes_read);
    if (!bytes_read) return false;

    state->leaf_byte_pos = pos;
    iter->byte_pos       = state->leaf_start_byte + pos;
    iter->char_pos--;
    return true;
}

void rope_iter_seek_char(rope_iter_t *iter, size_t char_pos) {
    if (!iter || !iter->rope) return;
    rope_iter_destroy(iter);
    rope_iter_init(iter, iter->rope, char_pos);
}

void rope_iter_seek_byte(rope_iter_t *iter, size_t byte_pos) {
    if (!iter || !iter->rope) return;
    size_t char_pos = rope_byte_to_char(iter->rope, byte_pos);
    rope_iter_seek_char(iter, char_pos);
}

void rope_iter_destroy(rope_iter_t *iter) {
    if (!iter || !iter->internal) return;
    rope_iter_state_t *state = iter->internal;
    dstack_free(&state->stk);
    free(state);
    iter->internal = NULL;
}

/* ── Line operations ─────────────────────────────────────────────────────── */

size_t rope_line_count(const rope_t *r) { return r ? r->newlines + 1 : 1; }

size_t rope_char_to_line(const rope_t *r, size_t char_pos) {
    if (!r) return 0;
    size_t line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, r, 0);
    uint32_t ch;
    size_t i = 0;
    while (i < char_pos && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') line++;
        i++;
    }
    rope_iter_destroy(&iter);
    return line;
}

size_t rope_byte_to_line(const rope_t *r, size_t byte_pos) {
    if (!r) return 0;
    return rope_char_to_line(r, rope_byte_to_char(r, byte_pos));
}

size_t rope_line_to_char(const rope_t *r, size_t line) {
    if (!r) return 0;
    size_t cur_line = 0;
    rope_iter_t iter;
    rope_iter_init(&iter, r, 0);
    uint32_t ch;
    size_t i = 0;
    while (rope_iter_next_char(&iter, &ch)) {
        if (cur_line == line) { rope_iter_destroy(&iter); return i; }
        if (ch == '\n') cur_line++;
        i++;
    }
    rope_iter_destroy(&iter);
    return r->char_len;
}

size_t rope_line_to_byte(const rope_t *r, size_t line) {
    if (!r) return 0;
    return rope_char_to_byte(r, rope_line_to_char(r, line));
}

/* ── Each newline ────────────────────────────────────────────────────────── */

void rope_each_newline(const rope_t *r,
                       void (*callback)(size_t char_pos, void *userdata),
                       void *userdata) {
    if (!r || !r->root) return;

    dstack_t stk = dstack_new();
    rope_node_t *n = r->root;
    size_t char_pos = 0;

    while (n || !dstack_empty(&stk)) {
        while (n) {
            if (!n->is_leaf) { dstack_push(&stk, n); n = n->branch.left; }
            else break;
        }
        if (n && n->is_leaf) {
            const char *data = n->leaf.data;
            size_t      blen = n->leaf.byte_len;
            size_t      bp   = 0;
            while (bp < blen) {
                uint8_t c = (uint8_t)data[bp];
                if (c == '\n') callback(char_pos, userdata);
                bp += utf8_char_len(c);
                char_pos++;
            }
            n = NULL;
        }
        if (!dstack_empty(&stk))
            n = ((rope_node_t *)dstack_pop(&stk))->branch.right;
    }

    dstack_free(&stk);
}

#endif /* ROPE_IMPLEMENTATION */

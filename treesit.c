#include "treesit.h"
#include "textprop.h"
#include "buffer.h"
#include "faces.h"
#include "rope.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>

typedef struct LanguageCache {
    char                 *name;
    const TSLanguage     *language;
    void                 *handle;
    struct LanguageCache *next;
} LanguageCache;

static LanguageCache *language_cache = NULL;

typedef struct RegexCache {
    char    *pattern;
    regex_t  regex;
    bool     valid;
    struct RegexCache *next;
} RegexCache;

static RegexCache *regex_cache = NULL;

static regex_t *get_compiled_regex(const char *pattern) {
    for (RegexCache *r = regex_cache; r; r = r->next)
        if (strcmp(r->pattern, pattern) == 0)
            return r->valid ? &r->regex : NULL;

    RegexCache *e = calloc(1, sizeof *e);
    e->pattern = strdup(pattern);
    int rc = regcomp(&e->regex, pattern, REG_EXTENDED | REG_NOSUB);
    e->valid = (rc == 0);
    if (!e->valid) {
        char err[256]; regerror(rc, &e->regex, err, sizeof err);
        fprintf(stderr, "treesit: regex compile failed '%s': %s\n", pattern, err);
    }
    e->next = regex_cache;
    regex_cache = e;
    return e->valid ? &e->regex : NULL;
}

// Static face symbol (interned once)
static SCM s_face_sym = 0;
static inline SCM face_sym(void) {
    if (!s_face_sym) {
        s_face_sym = scm_from_utf8_symbol("face");
        scm_gc_protect_object(s_face_sym);
    }
    return s_face_sym;
}

static const TSCaptureFaceMapping default_capture_mappings[] = {
    {"comment",               FACE_FONT_LOCK_COMMENT},
    {"comment.delimiter",     FACE_FONT_LOCK_COMMENT_DELIMITER},
    {"comment.documentation", FACE_FONT_LOCK_DOC},
    {"string",                FACE_FONT_LOCK_STRING},
    {"string.documentation",  FACE_FONT_LOCK_DOC},
    {"string.regexp",         FACE_FONT_LOCK_REGEXP},
    {"string.escape",         FACE_FONT_LOCK_ESCAPE},
    {"number",                FACE_FONT_LOCK_NUMBER},
    {"constant",              FACE_FONT_LOCK_CONSTANT},
    {"constant.builtin",      FACE_FONT_LOCK_BUILTIN},
    {"keyword",               FACE_FONT_LOCK_KEYWORD},
    {"operator",              FACE_FONT_LOCK_OPERATOR},
    {"punctuation",           FACE_FONT_LOCK_PUNCTUATION},
    {"punctuation.bracket",   FACE_FONT_LOCK_BRACKET},
    {"punctuation.delimiter", FACE_FONT_LOCK_DELIMITER},
    {"function",              FACE_FONT_LOCK_FUNCTION_NAME},
    {"function.builtin",      FACE_FONT_LOCK_BUILTIN},
    {"function.call",         FACE_FONT_LOCK_FUNCTION_CALL},
    {"function.method",       FACE_FONT_LOCK_FUNCTION_NAME},
    {"function.method.call",  FACE_FONT_LOCK_FUNCTION_CALL},
    {"variable",              FACE_FONT_LOCK_VARIABLE_NAME},
    {"variable.builtin",      FACE_FONT_LOCK_BUILTIN},
    {"variable.parameter",    FACE_FONT_LOCK_VARIABLE_NAME},
    {"property",              FACE_FONT_LOCK_PROPERTY_NAME},
    {"type",                  FACE_FONT_LOCK_TYPE},
    {"type.builtin",          FACE_FONT_LOCK_BUILTIN},
    {"constructor",           FACE_FONT_LOCK_FUNCTION_NAME},
    {"label",                 FACE_FONT_LOCK_CONSTANT},
    {"error",                 FACE_FONT_LOCK_WARNING},
    {NULL,                    FACE_DEFAULT}
};

static int get_face_for_capture(const char *name) {
    for (int i = 0; default_capture_mappings[i].capture_name; i++)
        if (strcmp(name, default_capture_mappings[i].capture_name) == 0)
            return default_capture_mappings[i].face_id;
    return FACE_DEFAULT;
}

void init_treesit(void) {}

void cleanup_treesit(void) {
    for (LanguageCache *c = language_cache, *n; c; c = n) {
        n = c->next; free(c->name);
        if (c->handle) dlclose(c->handle);
        free(c);
    }
    language_cache = NULL;
    for (RegexCache *r = regex_cache, *n; r; r = n) {
        n = r->next; free(r->pattern);
        if (r->valid) regfree(&r->regex);
        free(r);
    }
    regex_cache = NULL;
}

const TSLanguage *treesit_load_language(const char *lang_name) {
    if (!lang_name) return NULL;
    for (LanguageCache *c = language_cache; c; c = c->next)
        if (strcmp(c->name, lang_name) == 0) return c->language;

    char path[512];
    snprintf(path, sizeof path, "./etc/tree-sitter/%s.so", lang_name);
    void *handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (!handle) { fprintf(stderr, "treesit: dlopen '%s': %s\n", lang_name, dlerror()); return NULL; }

    char sym[256];
    snprintf(sym, sizeof sym, "tree_sitter_%s", lang_name);
    typedef const TSLanguage *(*LF)(void);
    LF lf = (LF)(uintptr_t)dlsym(handle, sym);
    if (!lf) { fprintf(stderr, "treesit: dlsym '%s': %s\n", sym, dlerror()); dlclose(handle); return NULL; }

    const TSLanguage *lang = lf();
    if (!lang) { dlclose(handle); return NULL; }

    LanguageCache *e = malloc(sizeof *e);
    e->name = strdup(lang_name); e->language = lang; e->handle = handle;
    e->next = language_cache; language_cache = e;
    return lang;
}

bool treesit_language_available_p(const char *n) {
    return treesit_load_language(n) != NULL;
}

TreeSitterState *treesit_parser_create(const char *lang_name) {
    const TSLanguage *lang = treesit_load_language(lang_name);
    if (!lang) return NULL;

    TreeSitterState *s = calloc(1, sizeof *s);
    s->parser = ts_parser_new();
    if (!s->parser) { free(s); return NULL; }
    ts_parser_set_language(s->parser, lang);
    s->language      = lang;
    s->language_name = strdup(lang_name);
    s->hl_cursor     = ts_query_cursor_new();
    s->needs_reparse = true;
    s->point_cache.valid = false;
    return s;
}

void treesit_parser_delete(TreeSitterState *s) {
    if (!s) return;
    if (s->tree)      ts_tree_delete(s->tree);
    if (s->parser)    ts_parser_delete(s->parser);
    if (s->hl_query)  ts_query_delete(s->hl_query);
    if (s->hl_cursor) ts_query_cursor_delete(s->hl_cursor);
    free(s->language_name);
    free(s);
}

void treesit_parser_set_language(TreeSitterState *s, const char *lang_name) {
    if (!s) return;
    const TSLanguage *lang = treesit_load_language(lang_name);
    if (!lang) return;
    ts_parser_set_language(s->parser, lang);
    s->language = lang;
    free(s->language_name); s->language_name = strdup(lang_name);
    if (s->tree) { ts_tree_delete(s->tree); s->tree = NULL; }
    s->needs_reparse = true;
    s->point_cache.valid = false;
}

static const char *ts_read_buffer(void *payload, uint32_t byte_offset,
                                  TSPoint position, uint32_t *bytes_read) {
    (void)position;
    Buffer *buf = payload;
    size_t rope_len = rope_byte_length(buf->rope);
    if (byte_offset >= rope_len) { *bytes_read = 0; return ""; }
    static char chunk[4096];
    size_t to_read = rope_len - byte_offset;
    if (to_read > sizeof chunk) to_read = sizeof chunk;
    *bytes_read = (uint32_t)rope_copy_bytes(buf->rope, byte_offset, to_read, chunk, sizeof chunk);
    return chunk;
}

/// TSPoint cache management

// treesit_char_to_point is O(|char_pos - cache.char_pos|) amortized.
// For a monotone stream of keypresses at position P, P+1, P+2 … this is O(1).
// For random jumps it degrades to O(file_size) in the worst case, same as before
// but we could add a second anchor at line-start if needed.

void treesit_invalidate_point_cache(Buffer *buf) {
    if (buf && buf->ts_state)
        buf->ts_state->point_cache.valid = false;
}

// Internal: walk from known (char_pos, TSPoint) forward or backward by delta.
// Uses rope_iter_t for cache-friendly sequential access.
static TSPoint walk_point_forward(Buffer *buf, size_t from_char, TSPoint from_pt, size_t to_char) {
    // to_char > from_char
    TSPoint pt = from_pt;
    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, from_char);
    uint32_t ch;
    for (size_t i = from_char; i < to_char && rope_iter_next_char(&iter, &ch); i++) {
        if (ch == '\n') { pt.row++; pt.column = 0; }
        else            { pt.column += (ch < 0x80) ? 1 : (ch < 0x800) ? 2 : (ch < 0x10000) ? 3 : 4; }
    }
    rope_iter_destroy(&iter);
    return pt;
}

static TSPoint walk_point_backward(Buffer *buf, size_t from_char, TSPoint from_pt, size_t to_char) {
    // to_char < from_char — walk backward using prev_char iterator
    // Strategy: find the start of from_pt.row (via rope_line_to_char), then
    // walk forward from line-start to to_char. This avoids a full backward scan.
    // For small deltas (< 256 chars) a direct backward iter is cheaper.
    size_t delta = from_char - to_char;
    if (delta <= 512) {
        // Use backward iterator
        rope_iter_t iter;
        rope_iter_init(&iter, buf->rope, from_char);
        TSPoint pt = from_pt;
        uint32_t ch;
        for (size_t i = 0; i < delta; i++) {
            if (!rope_iter_prev_char(&iter, &ch)) break;
            if (ch == '\n') {
                // We crossed a line boundary going backward.
                // We need to figure out what column we're at now.
                // Jump to line-start approach:
                rope_iter_destroy(&iter);
                size_t cur_char = from_char - i - 1;
                // Find start of this line
                size_t line = rope_char_to_line(buf->rope, cur_char);
                size_t line_start = rope_line_to_char(buf->rope, line);
                pt.row = (uint32_t)line;
                // Column = bytes from line_start to to_char
                // Walk forward from line_start to to_char
                rope_iter_t fwd;
                rope_iter_init(&fwd, buf->rope, line_start);
                uint32_t fc;
                pt.column = 0;
                for (size_t j = line_start; j < to_char && rope_iter_next_char(&fwd, &fc); j++) {
                    pt.column += (fc < 0x80) ? 1 : (fc < 0x800) ? 2 : (fc < 0x10000) ? 3 : 4;
                }
                rope_iter_destroy(&fwd);
                return pt;
            } else {
                uint32_t col_bytes = (ch < 0x80) ? 1 : (ch < 0x800) ? 2 : (ch < 0x10000) ? 3 : 4;
                pt.column = (pt.column >= col_bytes) ? pt.column - col_bytes : 0;
            }
        }
        rope_iter_destroy(&iter);
        return pt;
    } else {
        // Large backward jump: recompute from line start
        size_t line = rope_char_to_line(buf->rope, to_char);
        size_t line_start = rope_line_to_char(buf->rope, line);
        TSPoint pt = {(uint32_t)line, 0};
        return walk_point_forward(buf, line_start, pt, to_char);
    }
}

// Public: char_pos -> TSPoint, O(|delta from cache|) amortized
TSPoint treesit_char_to_point(Buffer *buf, size_t char_pos) {
    TSPoint zero = {0, 0};
    if (!buf || !buf->rope) return zero;

    size_t rope_len = rope_char_length(buf->rope);
    if (char_pos > rope_len) char_pos = rope_len;

    TreeSitterState *s = buf->ts_state;

    // Try cached anchor
    if (s && s->point_cache.valid) {
        size_t cached_pos = s->point_cache.char_pos;
        TSPoint cached_pt = s->point_cache.point;

        TSPoint result;
        if (char_pos == cached_pos) {
            result = cached_pt;
        } else if (char_pos > cached_pos) {
            result = walk_point_forward(buf, cached_pos, cached_pt, char_pos);
        } else {
            result = walk_point_backward(buf, cached_pos, cached_pt, char_pos);
        }
        // Update cache to the new position
        s->point_cache.char_pos = char_pos;
        s->point_cache.point    = result;
        return result;
    }

    // Cold start: walk from 0
    TSPoint result = walk_point_forward(buf, 0, zero, char_pos);
    if (s) {
        s->point_cache.char_pos = char_pos;
        s->point_cache.point    = result;
        s->point_cache.valid    = true;
    }
    return result;
}

size_t treesit_char_to_byte(Buffer *buf, size_t char_pos) {
    return (buf && buf->rope) ? rope_char_to_byte(buf->rope, char_pos) : 0;
}

size_t treesit_byte_to_char(Buffer *buf, size_t byte_offset) {
    return (buf && buf->rope) ? rope_byte_to_char(buf->rope, byte_offset) : 0;
}

/// Parsing

bool treesit_parse_buffer(Buffer *buf) {
    if (!buf || !buf->ts_state) return false;
    TreeSitterState *s = buf->ts_state;
    TSInput input = { buf, ts_read_buffer, TSInputEncodingUTF8 };
    TSTree *new_tree = ts_parser_parse(s->parser, s->tree, input);
    if (!new_tree) { fprintf(stderr, "treesit: ts_parser_parse failed\n"); return false; }
    if (s->tree) ts_tree_delete(s->tree);
    s->tree = new_tree;
    s->needs_reparse = false;
    return true;
}

void treesit_update_tree(Buffer *buf,
                         size_t start_byte, size_t old_end_byte, size_t new_end_byte,
                         TSPoint start_point, TSPoint old_end_point, TSPoint new_end_point) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree) {
        if (buf && buf->ts_state) buf->ts_state->needs_reparse = true;
        return;
    }
    TSInputEdit edit = {
        (uint32_t)start_byte, (uint32_t)old_end_byte, (uint32_t)new_end_byte,
        start_point, old_end_point, new_end_point
    };
    ts_tree_edit(buf->ts_state->tree, &edit);
    buf->ts_state->needs_reparse = true;
    buf->ts_state->last_edit_pos     = start_byte;
    buf->ts_state->last_edit_old_end = old_end_byte;
    buf->ts_state->last_edit_new_end = new_end_byte;
}

void treesit_reparse_if_needed(Buffer *buf) {
    if (buf && buf->ts_state && buf->ts_state->needs_reparse)
        treesit_parse_buffer(buf);
}

/// Query

bool treesit_set_highlight_query(TreeSitterState *s, const char *qs) {
    if (!s || !s->language) return false;
    if (s->hl_query) { ts_query_delete(s->hl_query); s->hl_query = NULL; }
    if (!qs || !qs[0]) return true;
    uint32_t err_off; TSQueryError err_type;
    s->hl_query = ts_query_new(s->language, qs, (uint32_t)strlen(qs), &err_off, &err_type);
    if (!s->hl_query) {
        fprintf(stderr, "treesit: query error at %u: %d\n", err_off, err_type);
        return false;
    }
    return true;
}

/// Predicate evaluation

static char *get_node_text(Buffer *buf, TSNode node) {
    uint32_t sb = ts_node_start_byte(node), eb = ts_node_end_byte(node);
    uint32_t len = eb - sb;
    if (!len) return NULL;
    char *t = malloc(len + 1);
    if (!t) return NULL;
    size_t copied = rope_copy_bytes(buf->rope, sb, len, t, len);
    t[copied] = '\0';
    return t;
}

static bool match_satisfies_predicates(Buffer *buf, TSQuery *query, TSQueryMatch *match) {
    uint32_t n_steps;
    const TSQueryPredicateStep *steps =
        ts_query_predicates_for_pattern(query, match->pattern_index, &n_steps);
    if (!n_steps) return true;

    uint32_t i = 0;
    while (i < n_steps) {
        if (steps[i].type != TSQueryPredicateStepTypeString) { i++; continue; }
        uint32_t slen;
        const char *pred = ts_query_string_value_for_id(query, steps[i].value_id, &slen);
        i++;

        bool is_eq    = (strcmp(pred, "eq?")    == 0);
        bool is_match = (strcmp(pred, "match?") == 0);

        if (!is_eq && !is_match) {
            while (i < n_steps && steps[i].type != TSQueryPredicateStepTypeDone) i++;
            if (i < n_steps) i++;
            continue;
        }
        if (i + 1 >= n_steps) return false;
        if (steps[i].type != TSQueryPredicateStepTypeCapture) {
            while (i < n_steps && steps[i].type != TSQueryPredicateStepTypeDone) i++;
            if (i < n_steps) i++;
            continue;
        }
        uint32_t cap_id = steps[i].value_id; i++;
        if (steps[i].type != TSQueryPredicateStepTypeString) {
            while (i < n_steps && steps[i].type != TSQueryPredicateStepTypeDone) i++;
            if (i < n_steps) i++;
            continue;
        }
        const char *expected = ts_query_string_value_for_id(query, steps[i].value_id, &slen);
        i++;
        while (i < n_steps && steps[i].type != TSQueryPredicateStepTypeDone) i++;
        if (i < n_steps) i++;

        TSNode captured = {0}; bool found = false;
        for (uint16_t j = 0; j < match->capture_count; j++) {
            if (match->captures[j].index == cap_id) {
                captured = match->captures[j].node; found = true; break;
            }
        }
        if (!found || ts_node_is_null(captured)) return false;

        char *text = get_node_text(buf, captured);
        if (!text) return false;

        bool ok;
        if (is_eq) {
            ok = strcmp(text, expected) == 0;
        } else {
            regex_t *rx = get_compiled_regex(expected);
            ok = rx && (regexec(rx, text, 0, NULL, 0) == 0);
        }
        free(text);
        if (!ok) return false;
    }
    return true;
}

// ─── Batched byte->char conversion ─────────────────────────────────────────────
//
// Given a sorted array of N unique byte offsets, convert all of them to char
// positions in a SINGLE left-to-right walk through the rope's leaves.
//
// Algorithm:
//   1. Sort byte offsets (they come in roughly sorted from tree-sitter anyway).
//   2. Use rope_chunk_at_char to find the first leaf.
//   3. Scan forward through leaf data, counting chars, advancing to next leaf
//      via rope_chunk_at_char when we overshoot.
//   4. Binary-search the offset table to emit results as we cross each offset.
//
// Cost: O(N log N  +  spans_in_range × leaf_size)
//       Instead of O(N log file_size).
// For N=200 spans in a 1KB changed region: ~200 comparisons vs 200 × 14 tree hops.

typedef struct {
    uint32_t byte_off;
    size_t   char_pos;  // filled in by batch_convert
} ByteCharEntry;

static int bce_cmp(const void *a, const void *b) {
    uint32_t x = ((const ByteCharEntry*)a)->byte_off;
    uint32_t y = ((const ByteCharEntry*)b)->byte_off;
    return (x > y) - (x < y);
}

// Convert all entries[0..n) in-place.  entries MUST be sorted by byte_off ascending.
// Returns false on allocation failure.
static bool batch_byte_to_char(Buffer *buf, ByteCharEntry *entries, size_t n) {
    if (!n) return true;

    // Start at the first byte offset
    uint32_t start_byte = entries[0].byte_off;
    size_t   start_char = rope_byte_to_char(buf->rope, start_byte); // O(log n), once

    // Current position in the rope (in chars and bytes)
    size_t cur_char = start_char;
    size_t cur_byte = start_byte;

    // Get the leaf containing start_byte
    chunk_info_t chunk;
    if (!rope_chunk_at_char(buf->rope, start_char, &chunk)) {
        // Fallback: individual lookups
        for (size_t i = 0; i < n; i++)
            entries[i].char_pos = rope_byte_to_char(buf->rope, entries[i].byte_off);
        return true;
    }

    // Recalibrate: cur_byte should equal the absolute byte for cur_char.
    // rope_chunk_at_char gives us leaf_start_byte + byte_offset_in_leaf.
    cur_byte = chunk.leaf_start_byte + chunk.byte_offset_in_leaf;
    cur_char = chunk.leaf_start_char + chunk.char_offset_in_leaf;

    const char *data     = rope_chunk_data(&chunk);
    size_t      data_len = rope_chunk_byte_len(&chunk);
    size_t      leaf_byte_off = chunk.byte_offset_in_leaf; // position within current leaf

    size_t ei = 0; // entry index

    // Skip entries before cur_byte (shouldn't happen if start_byte is accurate,
    // but guard anyway)
    while (ei < n && entries[ei].byte_off < (uint32_t)cur_byte) {
        entries[ei].char_pos = cur_char;
        ei++;
    }

    while (ei < n) {
        uint32_t target = entries[ei].byte_off;

        // Advance within current leaf
        while (leaf_byte_off < data_len && cur_byte < target) {
            uint8_t  first     = (uint8_t)data[leaf_byte_off];
            uint32_t char_bytes = (first < 0x80) ? 1 :
                                  (first < 0xE0) ? 2 :
                                  (first < 0xF0) ? 3 : 4;
            // Clamp to leaf
            if (leaf_byte_off + char_bytes > data_len)
                char_bytes = (uint32_t)(data_len - leaf_byte_off);

            leaf_byte_off += char_bytes;
            cur_byte      += char_bytes;
            cur_char++;
        }

        if (cur_byte >= target) {
            // Emit all entries at this byte position
            while (ei < n && entries[ei].byte_off <= (uint32_t)cur_byte) {
                entries[ei].char_pos = cur_char - (cur_byte > target ? 1 : 0);
                /* More precisely: if cur_byte == target exactly, char_pos = cur_char.
                   If we overshot, the target was inside the last char we advanced over,
                   so char_pos = cur_char - 1 (tree-sitter byte offsets always point to
                   char boundaries for well-formed source, so overshoot shouldn't happen). */
                entries[ei].char_pos = (cur_byte == target) ? cur_char :
                                       (cur_char > 0 ? cur_char - 1 : 0);
                ei++;
            }
            continue;
        }

        // Reached end of leaf — load next leaf
        // Use rope_chunk_at_char to hop to next leaf in O(log n)
        size_t next_char = chunk.leaf_start_char + rope_chunk_char_len(&chunk);
        if (!rope_chunk_at_char(buf->rope, next_char, &chunk)) {
            // No more leaves — emit remaining entries at current position
            while (ei < n) { entries[ei].char_pos = cur_char; ei++; }
            break;
        }
        data         = rope_chunk_data(&chunk);
        data_len     = rope_chunk_byte_len(&chunk);
        leaf_byte_off = 0;
        cur_byte     = chunk.leaf_start_byte;
        cur_char     = chunk.leaf_start_char;
    }

    return true;
}

/// Span collection

// Raw span in byte coordinates (straight from tree-sitter)
typedef struct {
    uint32_t start_byte, end_byte;
    int      face_id;
    uint32_t pattern_index, capture_index;
} RawSpan;

// Final span in char coordinates (after batch conversion)
typedef struct {
    size_t   start_char, end_char;
    int      face_id;
    uint32_t pattern_index, capture_index;
} HighlightSpan;

static int hs_cmp(const void *a, const void *b) {
    const HighlightSpan *sa = a, *sb = b;
    if (sa->start_char != sb->start_char)
        return sa->start_char < sb->start_char ? -1 : 1;
    // Larger spans first (enclosing before enclosed)
    if (sa->end_char != sb->end_char)
        return sa->end_char > sb->end_char ? -1 : 1;
    // error nodes last
    bool ae = (sa->face_id == FACE_FONT_LOCK_WARNING);
    bool be = (sb->face_id == FACE_FONT_LOCK_WARNING);
    if (ae != be) return ae ? 1 : -1;
    if (sa->pattern_index != sb->pattern_index)
        return sa->pattern_index < sb->pattern_index ? -1 : 1;
    return (int)sa->capture_index - (int)sb->capture_index;
}

void treesit_apply_highlights_range(Buffer *buf, uint32_t start_byte, uint32_t end_byte) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree || !buf->ts_state->hl_query) return;

    TreeSitterState *s = buf->ts_state;

    // Convert range bounds to char positions
    size_t start_char = treesit_byte_to_char(buf, start_byte);
    size_t end_char   = treesit_byte_to_char(buf, end_byte);

    // Collect raw byte spans from tree-sitter
    size_t raw_cap = 256, raw_count = 0;
    RawSpan *raw = malloc(raw_cap * sizeof *raw);
    if (!raw) return;

    TSNode root = ts_tree_root_node(s->tree);
    ts_query_cursor_set_byte_range(s->hl_cursor, start_byte, end_byte);
    ts_query_cursor_exec(s->hl_cursor, s->hl_query, root);

    TSQueryMatch match;
    while (ts_query_cursor_next_match(s->hl_cursor, &match)) {
        if (!match_satisfies_predicates(buf, s->hl_query, &match)) continue;

        for (uint16_t i = 0; i < match.capture_count; i++) {
            TSQueryCapture cap = match.captures[i];
            uint32_t clen;
            const char *cname = ts_query_capture_name_for_id(s->hl_query, cap.index, &clen);
            if (cname[0] == '_') continue;

            int face_id = get_face_for_capture(cname);
            if (face_id == FACE_DEFAULT) continue;

            uint32_t csb = ts_node_start_byte(cap.node);
            uint32_t ceb = ts_node_end_byte(cap.node);
            if (csb >= ceb) continue;

            if (raw_count >= raw_cap) {
                raw_cap *= 2;
                RawSpan *nr = realloc(raw, raw_cap * sizeof *nr);
                if (!nr) { free(raw); return; }
                raw = nr;
            }
            raw[raw_count++] = (RawSpan){ csb, ceb, face_id,
                                          match.pattern_index, cap.index };
        }
    }

    if (raw_count == 0) {
        // Replace range with empty spans (clear highlights)
        textprop_replace_range(buf, start_char, end_char, NULL, 0);
        free(raw);
        return;
    }

    // Batched byte->char conversion — single rope walk
    size_t n_offsets = raw_count * 2;
    ByteCharEntry *bce = malloc(n_offsets * sizeof *bce);
    if (!bce) { free(raw); return; }

    for (size_t i = 0; i < raw_count; i++) {
        bce[i*2+0].byte_off = raw[i].start_byte;
        bce[i*2+1].byte_off = raw[i].end_byte;
    }
    qsort(bce, n_offsets, sizeof *bce, bce_cmp);

    size_t uniq = 0;
    for (size_t i = 0; i < n_offsets; i++)
        if (i == 0 || bce[i].byte_off != bce[i-1].byte_off)
            bce[uniq++] = bce[i];

    batch_byte_to_char(buf, bce, uniq);

    #define LOOKUP_CHAR(byte_val) ({                                    \
        uint32_t _bv = (byte_val);                                      \
        size_t _lo = 0, _hi = uniq;                                     \
        while (_lo + 1 < _hi) {                                         \
            size_t _mid = (_lo + _hi) / 2;                              \
            if (bce[_mid].byte_off <= _bv) _lo = _mid; else _hi = _mid; \
        }                                                               \
        bce[_lo].char_pos;                                              \
    })

    // Build HighlightSpan array
    HighlightSpan *spans = malloc(raw_count * sizeof *spans);
    if (!spans) { free(raw); free(bce); return; }

    size_t span_count = 0;
    for (size_t i = 0; i < raw_count; i++) {
        size_t sc = LOOKUP_CHAR(raw[i].start_byte);
        size_t ec = LOOKUP_CHAR(raw[i].end_byte);
        if (sc >= ec) continue;
        spans[span_count++] = (HighlightSpan){ sc, ec, raw[i].face_id,
                                               raw[i].pattern_index, raw[i].capture_index };
    }
    free(raw); free(bce);

    if (span_count == 0) {
        textprop_replace_range(buf, start_char, end_char, NULL, 0);
        free(spans);
        return;
    }

    // Sort + error-filter
    qsort(spans, span_count, sizeof *spans, hs_cmp);

    size_t fc = 0, err_end = 0;
    for (size_t i = 0; i < span_count; i++) {
        bool is_err = (spans[i].face_id == FACE_FONT_LOCK_WARNING);
        if (is_err) {
            if (spans[i].end_char > err_end) err_end = spans[i].end_char;
            spans[fc++] = spans[i];
        } else if (spans[i].start_char >= err_end) {
            spans[fc++] = spans[i];
        }
    }

    // Convert to TextPropSpan array for textprop_replace_range
    TextPropSpan *prop_spans = malloc(fc * sizeof(TextPropSpan));
    if (prop_spans) {
        for (size_t i = 0; i < fc; i++) {
            prop_spans[i].start   = spans[i].start_char;
            prop_spans[i].end     = spans[i].end_char;
            prop_spans[i].props   = NULL;  // No additional props, just face_id
            prop_spans[i].face_id = spans[i].face_id;
        }

        // Atomic replacement of the entire range
        textprop_replace_range(buf, start_char, end_char, prop_spans, fc);
        free(prop_spans);
    }

    free(spans);
    #undef LOOKUP_CHAR
}


// Full-file highlight (initial load / explicit rehighlight).
void treesit_apply_highlights(Buffer *buf) {
    if (!buf || !buf->ts_state) return;
    treesit_reparse_if_needed(buf);
    if (!buf->ts_state->tree || !buf->ts_state->hl_query) return;
    size_t byte_len = rope_byte_length(buf->rope);
    treesit_apply_highlights_range(buf, 0, (uint32_t)byte_len);
    /* Full highlight covers everything — watermark = end of file in chars */
    buf->props.highlighted_end = rope_char_length(buf->rope);
}

// Called from insert/delete_impl.  Uses ts_tree_get_changed_ranges to find
// exactly what the incremental reparse changed, then highlights only those.
//
// MAX_EXPAND is 512 bytes (down from 4096) — enough to cover any single
// statement/expression while keeping per-keypress work small.
//
void treesit_apply_highlights_after_edit(Buffer *buf, TSTree *old_tree) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree || !buf->ts_state->hl_query) return;
    if (!old_tree) { treesit_apply_highlights(buf); return; }

    uint32_t n_ranges;
    TSRange *changed = ts_tree_get_changed_ranges(old_tree, buf->ts_state->tree, &n_ranges);
    if (n_ranges == 0) { free(changed); return; }

    TSNode root = ts_tree_root_node(buf->ts_state->tree);
    uint32_t root_start = ts_node_start_byte(root);
    uint32_t root_end   = ts_node_end_byte(root);

    /* Clamp all highlight work to the visible viewport.
       Spans beyond vis_end are stored in lazy coords and will be
       re-highlighted on scroll in textprop_set_viewport.
       Reset highlighted_end to vis_end — the edit invalidated everything
       beyond the viewport, so the scroll watermark must restart from there. */
    uint32_t vis_end_byte = (uint32_t)treesit_char_to_byte(buf, buf->props.visible_end);
    buf->props.highlighted_end = buf->props.visible_end;

    const uint32_t MAX_EXPAND = 512;

    for (uint32_t i = 0; i < n_ranges; i++) {
        uint32_t sb = changed[i].start_byte;
        uint32_t eb = changed[i].end_byte;

        /* Skip ranges entirely outside the viewport */
        if (sb >= vis_end_byte) continue;

        if (sb >= eb) {
            uint32_t pad = 64;
            sb = (sb > pad) ? sb - pad : 0;
            eb += pad;
        }

        TSNode containing = ts_node_descendant_for_byte_range(root, sb, eb);
        if (!ts_node_is_null(containing)) {
            TSNode candidate = containing;
            while (!ts_node_is_null(candidate) && !ts_node_is_named(candidate))
                candidate = ts_node_parent(candidate);
            if (!ts_node_is_null(candidate)) {
                uint32_t nsb = ts_node_start_byte(candidate);
                uint32_t neb = ts_node_end_byte(candidate);
                bool is_root   = (nsb == root_start && neb == root_end);
                bool too_large = (!is_root) &&
                                 ((sb > nsb && (sb - nsb) > MAX_EXPAND) ||
                                  (neb > eb && (neb - eb) > MAX_EXPAND));
                if (!is_root && !too_large) {
                    if (nsb < sb) sb = nsb;
                    if (neb > eb) eb = neb;
                } else {
                    sb = (sb > MAX_EXPAND) ? sb - MAX_EXPAND : 0;
                    eb += MAX_EXPAND;
                    size_t total = rope_byte_length(buf->rope);
                    if (eb > (uint32_t)total) eb = (uint32_t)total;
                }
            }
        }

        /* Hard clamp to viewport */
        if (eb > vis_end_byte) eb = vis_end_byte;
        if (sb >= eb) continue;

        treesit_apply_highlights_range(buf, sb, eb);
    }
    free(changed);
}

// ─── Viewport highlight ───────────────────────────────────────────────────────

void treesit_apply_highlights_viewport(Buffer *buf, size_t visible_bytes) {
    if (!buf || !buf->ts_state) return;
    treesit_reparse_if_needed(buf);
    size_t byte_len = rope_byte_length(buf->rope);
    if (visible_bytes > byte_len) visible_bytes = byte_len;
    treesit_apply_highlights_range(buf, 0, (uint32_t)visible_bytes);
    /* Watermark = how far we actually highlighted */
    buf->props.highlighted_end = treesit_byte_to_char(buf, visible_bytes);
}

// ─── Node inspection ─────────────────────────────────────────────────────────

TSNode treesit_root_node(TreeSitterState *s) {
    return (s && s->tree) ? ts_tree_root_node(s->tree) : (TSNode){0};
}

TSNode treesit_node_at_char(Buffer *buf, size_t char_pos) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree) return (TSNode){0};
    TSNode root = ts_tree_root_node(buf->ts_state->tree);
    uint32_t b = (uint32_t)treesit_char_to_byte(buf, char_pos);
    return ts_node_descendant_for_byte_range(root, b, b);
}

char  *treesit_node_type(TSNode n)       { return ts_node_is_null(n) ? NULL : strdup(ts_node_type(n)); }
bool   treesit_node_named_p(TSNode n)    { return !ts_node_is_null(n) && ts_node_is_named(n); }
size_t treesit_node_start_byte(TSNode n) { return ts_node_start_byte(n); }
size_t treesit_node_end_byte(TSNode n)   { return ts_node_end_byte(n); }

/// Debug

static void print_tree_recursive(Buffer *buf, TSNode node, int depth) {
    if (ts_node_is_null(node)) return;
    uint32_t sb = ts_node_start_byte(node), eb = ts_node_end_byte(node);
    for (int i = 0; i < depth; i++) fprintf(stderr, "  ");
    char *text = NULL;
    if (eb - sb < 50) text = get_node_text(buf, node);
    fprintf(stderr, "%s%s [%zu-%zu]: %s\n",
            ts_node_is_named(node) ? "" : "\"", ts_node_type(node),
            treesit_byte_to_char(buf, sb), treesit_byte_to_char(buf, eb),
            text ? text : "");
    free(text);
    uint32_t nc = ts_node_child_count(node);
    for (uint32_t i = 0; i < nc; i++)
        print_tree_recursive(buf, ts_node_child(node, i), depth + 1);
}

void treesit_debug_print_tree(Buffer *buf) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree) {
        fprintf(stderr, "treesit: no tree\n"); return;
    }
    fprintf(stderr, "\n=== SYNTAX TREE ===\n");
    print_tree_recursive(buf, ts_tree_root_node(buf->ts_state->tree), 0);
    fprintf(stderr, "===================\n\n");
}

/// SCM

static SCM scm_treesit_available_p(void) { return SCM_BOOL_T; }

static SCM scm_treesit_language_available_p(SCM lang) {
    char *n = scm_is_string(lang) ? scm_to_locale_string(lang)
                                  : scm_to_locale_string(scm_symbol_to_string(lang));
    bool ok = treesit_language_available_p(n); free(n);
    return ok ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_treesit_parser_create(SCM lang) {
    if (!current_buffer) return SCM_BOOL_F;
    char *n = scm_is_string(lang) ? scm_to_locale_string(lang)
                                  : scm_to_locale_string(scm_symbol_to_string(lang));
    if (current_buffer->ts_state) treesit_parser_delete(current_buffer->ts_state);
    current_buffer->ts_state = treesit_parser_create(n); free(n);
    if (current_buffer->ts_state) { treesit_parse_buffer(current_buffer); return SCM_BOOL_T; }
    return SCM_BOOL_F;
}

static SCM scm_treesit_parser_delete(void) {
    if (!current_buffer || !current_buffer->ts_state) return SCM_BOOL_F;
    treesit_parser_delete(current_buffer->ts_state);
    current_buffer->ts_state = NULL; return SCM_BOOL_T;
}

static SCM scm_treesit_set_highlight_query(SCM qs) {
    if (!current_buffer || !current_buffer->ts_state) return SCM_BOOL_F;
    char *q = scm_to_locale_string(qs);
    bool ok = treesit_set_highlight_query(current_buffer->ts_state, q); free(q);
    return ok ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_treesit_apply_highlights(void) {
    if (!current_buffer) return SCM_BOOL_F;
    treesit_reparse_if_needed(current_buffer);
    treesit_apply_highlights(current_buffer);
    return SCM_BOOL_T;
}

static SCM scm_treesit_debug_tree(void) {
    if (!current_buffer) return SCM_BOOL_F;
    treesit_reparse_if_needed(current_buffer);
    treesit_debug_print_tree(current_buffer);
    return SCM_BOOL_T;
}

void init_treesit_bindings(void) {
    scm_c_define_gsubr("treesit-available?",           0, 0, 0, scm_treesit_available_p);
    scm_c_define_gsubr("treesit-language-available?",  1, 0, 0, scm_treesit_language_available_p);
    scm_c_define_gsubr("treesit-parser-create",        1, 0, 0, scm_treesit_parser_create);
    scm_c_define_gsubr("treesit-parser-delete",        0, 0, 0, scm_treesit_parser_delete);
    scm_c_define_gsubr("treesit-set-highlight-query!", 1, 0, 0, scm_treesit_set_highlight_query);
    scm_c_define_gsubr("treesit-apply-highlights",     0, 0, 0, scm_treesit_apply_highlights);
    scm_c_define_gsubr("treesit-debug-tree",           0, 0, 0, scm_treesit_debug_tree);
}


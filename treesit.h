#pragma once
#include <tree_sitter/api.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct Buffer Buffer;

// We cache the last char_pos → TSPoint mapping so that consecutive edits at
// nearby positions only walk the DELTA of the rope, not the whole thing.
// For a file with the cursor at position N, char_to_point is O(|delta|) not O(N).
typedef struct {
    size_t  char_pos;   // rope char index this cache entry describes
    TSPoint point;      // corresponding (row, column_bytes) TSPoint
    bool    valid;
} TSPointCache;

typedef struct {
    TSParser *parser;
    TSTree *tree;
    const TSLanguage *language;
    char *language_name;

    TSQuery *hl_query;
    TSQueryCursor *hl_cursor;

    // Incremental edit tracking
    size_t  last_edit_pos;
    size_t  last_edit_old_end;
    size_t  last_edit_new_end;
    bool    needs_reparse;

    // avoids full rope walk on every keypress
    TSPointCache point_cache;
} TreeSitterState;

typedef struct {
    char *capture_name;
    int   face_id;
} TSCaptureFaceMapping;

void init_treesit(void);
void cleanup_treesit(void);


const TSLanguage *treesit_load_language(const char *lang_name);
bool treesit_language_available_p(const char *lang_name);


TreeSitterState  *treesit_parser_create(const char *lang_name);
void treesit_parser_delete(TreeSitterState *state);
void treesit_parser_set_language(TreeSitterState *state, const char *lang_name);


bool  treesit_parse_buffer(Buffer *buf);
void  treesit_update_tree(Buffer *buf,
                          size_t start_byte, size_t old_end_byte, size_t new_end_byte,
                          TSPoint start_point, TSPoint old_end_point, TSPoint new_end_point);
void  treesit_reparse_if_needed(Buffer *buf);

// Coordinate conversion
// These are the only two functions that touch the rope for coordinate math.
// All other code calls these instead of rolling its own walks.

// char_pos -> (row, col_bytes) — O(|delta from cache|) amortized
TSPoint treesit_char_to_point(Buffer *buf, size_t char_pos);

// byte_offset -> char_pos — O(log n) rope traversal
size_t  treesit_byte_to_char(Buffer *buf, size_t byte_offset);

// char_pos -> byte_offset — O(log n)
size_t  treesit_char_to_byte(Buffer *buf, size_t char_pos);

// Invalidate the TSPoint cache (call after any rope mutation)
void    treesit_invalidate_point_cache(Buffer *buf);

bool treesit_set_highlight_query(TreeSitterState *state, const char *query_string);
void treesit_apply_highlights(Buffer *buf); // Full file highlight

// Incremental: uses ts_tree_get_changed_ranges, highlights only what changed
void treesit_apply_highlights_after_edit(Buffer *buf, TSTree *old_tree);

// Viewport-only for fast find_file
void treesit_apply_highlights_viewport(Buffer *buf, size_t visible_bytes);

// Core range highlight — flushed gap, batched byte→char, O(spans × log n)
void treesit_apply_highlights_range(Buffer *buf, uint32_t start_byte, uint32_t end_byte);

// Node inspection
TSNode  treesit_root_node(TreeSitterState *state);
TSNode  treesit_node_at_char(Buffer *buf, size_t char_pos);
char   *treesit_node_type(TSNode node);
bool    treesit_node_named_p(TSNode node);
size_t  treesit_node_start_byte(TSNode node);
size_t  treesit_node_end_byte(TSNode node);

void treesit_debug_print_tree(Buffer *buf);
void init_treesit_bindings(void);


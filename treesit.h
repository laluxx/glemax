#pragma once

#include <tree_sitter/api.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct Buffer Buffer;
typedef struct TSParser TSParser;
typedef struct TSTree TSTree;
typedef struct TSNode TSNode;
typedef struct TSQuery TSQuery;
typedef struct TSQueryCursor TSQueryCursor;

// Tree-sitter parser state for a buffer
typedef struct {
    TSParser *parser;
    TSTree *tree;
    const TSLanguage *language;
    char *language_name;
    
    // Highlighting query
    TSQuery *hl_query;
    TSQueryCursor *hl_cursor;
    
    // Track last edit for incremental parsing
    size_t last_edit_pos;
    size_t last_edit_old_end;
    size_t last_edit_new_end;
    bool needs_reparse;
} TreeSitterState;

// Query capture names mapped to face IDs
typedef struct {
    char *capture_name;
    int face_id;
} TSCaptureFaceMapping;

void init_treesit(void);
void cleanup_treesit(void);

// Language loading
const TSLanguage *treesit_load_language(const char *lang_name);
bool treesit_language_available_p(const char *lang_name);

// Parser management
TreeSitterState *treesit_parser_create(const char *lang_name);
void treesit_parser_delete(TreeSitterState *state);
void treesit_parser_set_language(TreeSitterState *state, const char *lang_name);

// Parsing functions
bool treesit_parse_buffer(Buffer *buf);
void treesit_update_tree(Buffer *buf, 
                         size_t start_byte, 
                         size_t old_end_byte, 
                         size_t new_end_byte,
                         TSPoint start_point,
                         TSPoint old_end_point,
                         TSPoint new_end_point);
void treesit_reparse_if_needed(Buffer *buf);

// Query functions
bool treesit_set_highlight_query(TreeSitterState *state, const char *query_string);
void treesit_apply_highlights(Buffer *buf);

// Node inspection (for future use in semantic editing)
TSNode treesit_root_node(TreeSitterState *state);
TSNode treesit_node_at_point(Buffer *buf, size_t pos);
char *treesit_node_type(TSNode node);
bool treesit_node_named_p(TSNode node);
size_t treesit_node_start_byte(TSNode node);
size_t treesit_node_end_byte(TSNode node);

// Scheme bindings
void init_treesit_bindings(void);

// Utility: Convert rope position to byte offset and TSPoint
size_t treesit_point_to_byte(Buffer *buf, size_t point);
size_t treesit_byte_to_point(Buffer *buf, size_t byte_offset);
TSPoint treesit_char_to_point(Buffer *buf, size_t char_pos);

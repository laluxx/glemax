#include "treesit.h"
#include "buffer.h"
#include "faces.h"
#include "textprop.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>

// Cache for loaded languages
typedef struct LanguageCache {
    char *name;
    const TSLanguage *language;
    void *handle;  // dlopen handle
    struct LanguageCache *next;
} LanguageCache;

static LanguageCache *language_cache = NULL;

// Default highlight query capture mappings
static TSCaptureFaceMapping default_capture_mappings[] = {
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
    {NULL, FACE_DEFAULT}
};

void init_treesit(void) {
    // Initialize if needed
}

void cleanup_treesit(void) {
    LanguageCache *curr = language_cache;
    while (curr) {
        LanguageCache *next = curr->next;
        free(curr->name);
        if (curr->handle) {
            dlclose(curr->handle);
        }
        free(curr);
        curr = next;
    }
    language_cache = NULL;
}

const TSLanguage *treesit_load_language(const char *lang_name) {
    if (!lang_name) return NULL;
    
    // Check cache first
    LanguageCache *curr = language_cache;
    while (curr) {
        if (strcmp(curr->name, lang_name) == 0) {
            return curr->language;
        }
        curr = curr->next;
    }
    
    // Build path to shared library
    char path[512];
    snprintf(path, sizeof(path), "./etc/tree-sitter/%s.so", lang_name);
    
    // Load the shared library
    void *handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (!handle) {
        fprintf(stderr, "Failed to load tree-sitter language '%s': %s\n",
                lang_name, dlerror());
        return NULL;
    }
    
    // Get the language constructor function
    char symbol[256];
    snprintf(symbol, sizeof(symbol), "tree_sitter_%s", lang_name);
    
    typedef const TSLanguage *(*LanguageFunc)(void);
    LanguageFunc lang_func = (LanguageFunc)dlsym(handle, symbol);
    
    if (!lang_func) {
        fprintf(stderr, "Failed to find symbol '%s' in %s: %s\n",
                symbol, path, dlerror());
        dlclose(handle);
        return NULL;
    }
    
    const TSLanguage *language = lang_func();
    if (!language) {
        fprintf(stderr, "Language constructor returned NULL for '%s'\n", lang_name);
        dlclose(handle);
        return NULL;
    }
    
    // Add to cache
    LanguageCache *entry = malloc(sizeof(LanguageCache));
    entry->name = strdup(lang_name);
    entry->language = language;
    entry->handle = handle;
    entry->next = language_cache;
    language_cache = entry;
    
    return language;
}

bool treesit_language_available_p(const char *lang_name) {
    return treesit_load_language(lang_name) != NULL;
}

TreeSitterState *treesit_parser_create(const char *lang_name) {
    const TSLanguage *language = treesit_load_language(lang_name);
    if (!language) {
        return NULL;
    }
    
    TreeSitterState *state = calloc(1, sizeof(TreeSitterState));
    if (!state) return NULL;
    
    state->parser = ts_parser_new();
    if (!state->parser) {
        free(state);
        return NULL;
    }
    
    ts_parser_set_language(state->parser, language);
    state->language = language;
    state->language_name = strdup(lang_name);
    state->tree = NULL;
    state->hl_query = NULL;
    state->hl_cursor = ts_query_cursor_new();
    state->needs_reparse = true;
    
    return state;
}

void treesit_parser_delete(TreeSitterState *state) {
    if (!state) return;
    
    if (state->tree) {
        ts_tree_delete(state->tree);
    }
    if (state->parser) {
        ts_parser_delete(state->parser);
    }
    if (state->hl_query) {
        ts_query_delete(state->hl_query);
    }
    if (state->hl_cursor) {
        ts_query_cursor_delete(state->hl_cursor);
    }
    free(state->language_name);
    free(state);
}

void treesit_parser_set_language(TreeSitterState *state, const char *lang_name) {
    if (!state) return;
    
    const TSLanguage *language = treesit_load_language(lang_name);
    if (!language) return;
    
    ts_parser_set_language(state->parser, language);
    state->language = language;
    
    free(state->language_name);
    state->language_name = strdup(lang_name);
    
    // Clear old tree
    if (state->tree) {
        ts_tree_delete(state->tree);
        state->tree = NULL;
    }
    
    state->needs_reparse = true;
}

// Callback for tree-sitter to read buffer content
static const char *ts_read_buffer(void *payload, uint32_t byte_offset,
                                   TSPoint position, uint32_t *bytes_read) {
    (void)position;  // Unused
    Buffer *buf = (Buffer *)payload;
    
    // Get rope content
    size_t rope_len = rope_byte_length(buf->rope);
    
    if (byte_offset >= rope_len) {
        *bytes_read = 0;
        return "";
    }
    
    // Read a chunk from the rope
    static char chunk[4096];
    size_t available = rope_len - byte_offset;
    size_t to_read = available < sizeof(chunk) ? available : sizeof(chunk);
    
    size_t copied = rope_copy_bytes(buf->rope, byte_offset, to_read, chunk, sizeof(chunk));
    *bytes_read = copied;
    
    return chunk;
}

bool treesit_parse_buffer(Buffer *buf) {
    if (!buf || !buf->ts_state) return false;
    
    TreeSitterState *state = buf->ts_state;
    
    TSInput input = {
        .payload = buf,
        .read = ts_read_buffer,
        .encoding = TSInputEncodingUTF8
    };
    
    // Parse with old tree - this enables incremental parsing
    // If state->tree is NULL, this is a full parse
    // If state->tree has edits applied via ts_tree_edit(), this is incremental
    TSTree *new_tree = ts_parser_parse(state->parser, state->tree, input);
    
    if (!new_tree) {
        fprintf(stderr, "Failed to parse buffer\n");
        return false;
    }
    
    // Replace old tree
    if (state->tree) {
        ts_tree_delete(state->tree);
    }
    state->tree = new_tree;
    state->needs_reparse = false;
    
    return true;
}

TSPoint treesit_char_to_point(Buffer *buf, size_t char_pos) {
    TSPoint point = {0, 0};
    
    if (!buf || !buf->rope) {
        return point;
    }
    
    uint32_t row = 0;
    uint32_t column = 0;
    
    size_t rope_len = rope_char_length(buf->rope);
    if (char_pos > rope_len) {
        char_pos = rope_len;
    }
    
    // Count newlines and columns up to char_pos
    for (size_t i = 0; i < char_pos; i++) {
        size_t byte_pos = rope_char_to_byte(buf->rope, i);
        char c;
        if (rope_copy_bytes(buf->rope, byte_pos, 1, &c, 1) == 1) {
            if (c == '\n') {
                row++;
                column = 0;
            } else {
                column++;
            }
        }
    }
    
    point.row = row;
    point.column = column;
    return point;
}


void treesit_update_tree(Buffer *buf,
                         size_t start_byte,
                         size_t old_end_byte,
                         size_t new_end_byte,
                         TSPoint start_point,
                         TSPoint old_end_point,
                         TSPoint new_end_point) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree) {
        if (buf && buf->ts_state) {
            buf->ts_state->needs_reparse = true;
        }
        return;
    }
    
    TSInputEdit edit = {
        .start_byte = (uint32_t)start_byte,
        .old_end_byte = (uint32_t)old_end_byte,
        .new_end_byte = (uint32_t)new_end_byte,
        .start_point = start_point,
        .old_end_point = old_end_point,
        .new_end_point = new_end_point
    };
    
    // Apply edit to tree - this marks ranges for incremental reparse
    ts_tree_edit(buf->ts_state->tree, &edit);
    
    // Mark as needing incremental reparse
    buf->ts_state->needs_reparse = true;
}

void treesit_reparse_if_needed(Buffer *buf) {
    if (!buf || !buf->ts_state) return;
    
    if (buf->ts_state->needs_reparse) {
        treesit_parse_buffer(buf);
    }
}

bool treesit_set_highlight_query(TreeSitterState *state, const char *query_string) {
    if (!state || !state->language) return false;
    
    // Delete old query
    if (state->hl_query) {
        ts_query_delete(state->hl_query);
        state->hl_query = NULL;
    }
    
    if (!query_string || query_string[0] == '\0') {
        return true;  // Successfully cleared query
    }
    
    // Create new query
    uint32_t error_offset;
    TSQueryError error_type;
    
    TSQuery *query = ts_query_new(
        state->language,
        query_string,
        strlen(query_string),
        &error_offset,
        &error_type
    );
    
    if (!query) {
        fprintf(stderr, "Tree-sitter query error at offset %u: %d\n",
                error_offset, error_type);
        return false;
    }
    
    state->hl_query = query;
    return true;
}

static int get_face_for_capture(const char *capture_name) {
    for (int i = 0; default_capture_mappings[i].capture_name != NULL; i++) {
        if (strcmp(capture_name, default_capture_mappings[i].capture_name) == 0) {
            return default_capture_mappings[i].face_id;
        }
    }
    return FACE_DEFAULT;
}


// Helper function to get node text for predicate evaluation
static char* get_node_text(Buffer *buf, TSNode node) {
    uint32_t start_byte = ts_node_start_byte(node);
    uint32_t end_byte = ts_node_end_byte(node);
    uint32_t length = end_byte - start_byte;
    
    if (length == 0) return NULL;
    
    char *text = malloc(length + 1);
    if (!text) return NULL;
    
    size_t copied = rope_copy_bytes(buf->rope, start_byte, length, text, length);
    text[copied] = '\0';
    
    return text;
}

static bool match_satisfies_predicates(Buffer *buf, TSQuery *query, TSQueryMatch *match) {
    uint32_t pattern_index = match->pattern_index;
    uint32_t predicate_step_count;
    
    const TSQueryPredicateStep *predicate_steps = ts_query_predicates_for_pattern(
        query,
        pattern_index,
        &predicate_step_count
    );
    
    // If no predicates, match is valid
    if (predicate_step_count == 0) {
        return true;
    }
    
    // Predicates are structured as: [String(predicate_name), args..., Done]
    // We need to process each complete predicate
    uint32_t i = 0;
    while (i < predicate_step_count) {
        const TSQueryPredicateStep *step = &predicate_steps[i];
        
        // Each predicate starts with a String step (the predicate name)
        if (step->type != TSQueryPredicateStepTypeString) {
            i++;
            continue;
        }
        
        uint32_t length;
        const char *predicate_name = ts_query_string_value_for_id(query, step->value_id, &length);
        i++; // Move past predicate name
        
        // Handle #eq? predicate: (#eq? @capture "expected_value")
        if (strcmp(predicate_name, "eq?") == 0) {
            if (i >= predicate_step_count) return false;
            
            // Next should be a capture
            const TSQueryPredicateStep *capture_step = &predicate_steps[i];
            if (capture_step->type != TSQueryPredicateStepTypeCapture) {
                // Skip malformed predicate
                while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                    i++;
                }
                if (i < predicate_step_count) i++; // Skip Done
                continue;
            }
            i++;
            
            // Find the captured node
            TSNode captured_node = {0};
            bool found = false;
            for (uint16_t j = 0; j < match->capture_count; j++) {
                if (match->captures[j].index == capture_step->value_id) {
                    captured_node = match->captures[j].node;
                    found = true;
                    break;
                }
            }
            
            if (!found || ts_node_is_null(captured_node)) {
                // Skip to end of this predicate
                while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                    i++;
                }
                if (i < predicate_step_count) i++; // Skip Done
                return false;
            }
            
            // Next should be the expected string value
            if (i >= predicate_step_count) return false;
            const TSQueryPredicateStep *string_step = &predicate_steps[i];
            if (string_step->type != TSQueryPredicateStepTypeString) {
                while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                    i++;
                }
                if (i < predicate_step_count) i++; // Skip Done
                return false;
            }
            i++;
            
            const char *expected_text = ts_query_string_value_for_id(query, string_step->value_id, &length);
            char *actual_text = get_node_text(buf, captured_node);
            
            bool matches = false;
            if (actual_text) {
                matches = (strcmp(actual_text, expected_text) == 0);
                free(actual_text);
            }
            
            // Skip to Done marker
            while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                i++;
            }
            if (i < predicate_step_count) i++; // Skip Done
            
            if (!matches) return false;
        }
        // Handle #match? predicate: (#match? @capture "regex_pattern")
        else if (strcmp(predicate_name, "match?") == 0) {
            if (i >= predicate_step_count) return false;
            
            // Next should be a capture
            const TSQueryPredicateStep *capture_step = &predicate_steps[i];
            if (capture_step->type != TSQueryPredicateStepTypeCapture) {
                while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                    i++;
                }
                if (i < predicate_step_count) i++; // Skip Done
                continue;
            }
            i++;
            
            // Find the captured node
            TSNode captured_node = {0};
            bool found = false;
            for (uint16_t j = 0; j < match->capture_count; j++) {
                if (match->captures[j].index == capture_step->value_id) {
                    captured_node = match->captures[j].node;
                    found = true;
                    break;
                }
            }
            
            if (!found || ts_node_is_null(captured_node)) {
                while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                    i++;
                }
                if (i < predicate_step_count) i++; // Skip Done
                return false;
            }
            
            // Next should be the regex pattern
            if (i >= predicate_step_count) return false;
            const TSQueryPredicateStep *pattern_step = &predicate_steps[i];
            if (pattern_step->type != TSQueryPredicateStepTypeString) {
                while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                    i++;
                }
                if (i < predicate_step_count) i++; // Skip Done
                return false;
            }
            i++;
            
            const char *pattern = ts_query_string_value_for_id(query, pattern_step->value_id, &length);
            char *actual_text = get_node_text(buf, captured_node);
            
            bool matches = false;
            if (actual_text) {
                // Compile and execute regex
                regex_t regex;
                int reti = regcomp(&regex, pattern, REG_EXTENDED | REG_NOSUB);
                
                if (reti == 0) {
                    reti = regexec(&regex, actual_text, 0, NULL, 0);
                    matches = (reti == 0);
                    regfree(&regex);
                } else {
                    char error_buf[256];
                    regerror(reti, &regex, error_buf, sizeof(error_buf));
                    fprintf(stderr, "Regex compilation failed: '%s': %s\n", pattern, error_buf);
                }
                
                free(actual_text);
            }
            
            // Skip to Done marker
            while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                i++;
            }
            if (i < predicate_step_count) i++; // Skip Done
            
            if (!matches) return false;
        }
        else {
            // Unknown predicate - skip to Done marker
            while (i < predicate_step_count && predicate_steps[i].type != TSQueryPredicateStepTypeDone) {
                i++;
            }
            if (i < predicate_step_count) i++; // Skip Done
        }
    }
    
    return true;
}

typedef struct {
    size_t start_pos;
    size_t end_pos;
    int face_id;
    uint32_t pattern_index;  // Higher index = higher precedence
    uint32_t capture_index;  // For tie-breaking within same pattern
} HighlightSpan;


static int compare_spans(const void *a, const void *b) {
    const HighlightSpan *span_a = (const HighlightSpan *)a;
    const HighlightSpan *span_b = (const HighlightSpan *)b;
    
    // First by start position (left to right)
    if (span_a->start_pos != span_b->start_pos) {
        return (span_a->start_pos < span_b->start_pos) ? -1 : 1;
    }
    
    // At same start position: longer spans first (so they get applied before shorter ones)
    // This way ERROR nodes that contain other nodes get applied first
    if (span_a->end_pos != span_b->end_pos) {
        return (span_a->end_pos > span_b->end_pos) ? -1 : 1;
    }
    
    // CRITICAL: ERROR face comes LAST among same-length spans
    // This ensures errors override non-errors of the same span
    bool a_is_error = (span_a->face_id == FACE_FONT_LOCK_WARNING);
    bool b_is_error = (span_b->face_id == FACE_FONT_LOCK_WARNING);
    
    if (a_is_error != b_is_error) {
        return a_is_error ? 1 : -1;
    }
    
    // Then by pattern index (lower first, so higher can override)
    if (span_a->pattern_index != span_b->pattern_index) {
        return (span_a->pattern_index < span_b->pattern_index) ? -1 : 1;
    }
    
    // Then by capture index
    if (span_a->capture_index != span_b->capture_index) {
        return (span_a->capture_index < span_b->capture_index) ? -1 : 1;
    }
    
    return 0;
}

void treesit_apply_highlights(Buffer *buf) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree || !buf->ts_state->hl_query) {
        return;
    }
    
    TreeSitterState *state = buf->ts_state;
    clear_text_properties(buf);
    
    size_t span_capacity = 1024;
    size_t span_count = 0;
    HighlightSpan *spans = malloc(span_capacity * sizeof(HighlightSpan));
    if (!spans) return;
    
    TSNode root = ts_tree_root_node(state->tree);
    ts_query_cursor_exec(state->hl_cursor, state->hl_query, root);
    
    TSQueryMatch match;
    while (ts_query_cursor_next_match(state->hl_cursor, &match)) {
        if (!match_satisfies_predicates(buf, state->hl_query, &match)) {
            continue;
        }
        
        for (uint16_t i = 0; i < match.capture_count; i++) {
            TSQueryCapture capture = match.captures[i];
            
            uint32_t length;
            const char *capture_name = ts_query_capture_name_for_id(
                state->hl_query,
                capture.index,
                &length
            );
            
            if (capture_name[0] == '_') {
                continue;
            }
            
            int face_id = get_face_for_capture(capture_name);
            if (face_id == FACE_DEFAULT) continue;
            
            uint32_t start_byte = ts_node_start_byte(capture.node);
            uint32_t end_byte = ts_node_end_byte(capture.node);
            
            size_t start_pos = treesit_byte_to_point(buf, start_byte);
            size_t end_pos = treesit_byte_to_point(buf, end_byte);
            
            if (start_pos >= end_pos) continue;
            
            if (span_count >= span_capacity) {
                span_capacity *= 2;
                HighlightSpan *new_spans = realloc(spans, span_capacity * sizeof(HighlightSpan));
                if (!new_spans) {
                    free(spans);
                    return;
                }
                spans = new_spans;
            }
            
            spans[span_count].start_pos = start_pos;
            spans[span_count].end_pos = end_pos;
            spans[span_count].face_id = face_id;
            spans[span_count].pattern_index = match.pattern_index;
            spans[span_count].capture_index = capture.index;
            span_count++;
        }
    }
    
    // Sort: left-to-right, longer-first, errors-last
    qsort(spans, span_count, sizeof(HighlightSpan), compare_spans);
    
    // Optimized filtering: Skip spans contained in active ERROR spans
    // We track the rightmost end of any active ERROR span
    size_t filtered_count = 0;
    size_t active_error_end = 0;
    
    for (size_t i = 0; i < span_count; i++) {
        bool is_error = (spans[i].face_id == FACE_FONT_LOCK_WARNING);
        
        // If this is an ERROR, it becomes the new active error boundary
        if (is_error) {
            if (spans[i].end_pos > active_error_end) {
                active_error_end = spans[i].end_pos;
            }
            spans[filtered_count++] = spans[i];
        }
        // If this span is beyond the active error boundary, keep it
        else if (spans[i].start_pos >= active_error_end) {
            spans[filtered_count++] = spans[i];
        }
        // Otherwise it's contained in an error span, skip it
    }
    
    span_count = filtered_count;
    
    // Apply filtered spans
    /* for (size_t i = 0; i < span_count; i++) { */
    /*     put_text_property(buf, spans[i].start_pos, spans[i].end_pos, spans[i].face_id); */
    /* } */

    for (size_t i = 0; i < span_count; i++) {
        put_text_property(buf, spans[i].start_pos, spans[i].end_pos, 
                          scm_from_locale_symbol("face"), 
                          scm_from_int(spans[i].face_id));
    }

    
    free(spans);
}

TSNode treesit_root_node(TreeSitterState *state) {
    if (!state || !state->tree) {
        return (TSNode){0};
    }
    return ts_tree_root_node(state->tree);
}

TSNode treesit_node_at_point(Buffer *buf, size_t pos) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree) {
        return (TSNode){0};
    }
    
    TSNode root = ts_tree_root_node(buf->ts_state->tree);
    uint32_t byte_offset = treesit_point_to_byte(buf, pos);
    
    return ts_node_descendant_for_byte_range(root, byte_offset, byte_offset);
}

char *treesit_node_type(TSNode node) {
    if (ts_node_is_null(node)) return NULL;
    const char *type = ts_node_type(node);
    return type ? strdup(type) : NULL;
}

bool treesit_node_named_p(TSNode node) {
    return ts_node_is_named(node);
}

size_t treesit_node_start_byte(TSNode node) {
    return ts_node_start_byte(node);
}

size_t treesit_node_end_byte(TSNode node) {
    return ts_node_end_byte(node);
}

size_t treesit_point_to_byte(Buffer *buf, size_t point) {
    if (!buf || !buf->rope) return 0;
    
    // Use rope's built-in conversion
    return rope_char_to_byte(buf->rope, point);
}

size_t treesit_byte_to_point(Buffer *buf, size_t byte_offset) {
    if (!buf || !buf->rope) return 0;
    
    // Use rope's built-in conversion
    return rope_byte_to_char(buf->rope, byte_offset);
}

static void print_tree_recursive(Buffer *buf, TSNode node, int depth) {
    if (ts_node_is_null(node)) return;
    
    uint32_t start_byte = ts_node_start_byte(node);
    uint32_t end_byte = ts_node_end_byte(node);
    size_t start_pos = treesit_byte_to_point(buf, start_byte);
    size_t end_pos = treesit_byte_to_point(buf, end_byte);
    
    const char *type = ts_node_type(node);
    bool is_named = ts_node_is_named(node);
    bool has_error = ts_node_has_error(node);
    
    // Print indentation
    for (int i = 0; i < depth; i++) {
        fprintf(stderr, "  ");
    }
    
    // Get text for small nodes
    char *text = NULL;
    if (end_byte - start_byte < 50) {
        text = get_node_text(buf, node);
    }
    
    fprintf(stderr, "%s%s [%zu-%zu]%s: %s\n",
            is_named ? "" : "\"",
            type,
            start_pos, end_pos,
            is_named ? "" : "\"",
            text ? text : "");
    
    if (text) free(text);
    
    // Print children
    uint32_t child_count = ts_node_child_count(node);
    for (uint32_t i = 0; i < child_count; i++) {
        TSNode child = ts_node_child(node, i);
        print_tree_recursive(buf, child, depth + 1);
    }
}

void treesit_debug_print_tree(Buffer *buf) {
    if (!buf || !buf->ts_state || !buf->ts_state->tree) {
        fprintf(stderr, "No tree-sitter tree available\n");
        return;
    }
    
    TSNode root = ts_tree_root_node(buf->ts_state->tree);
    fprintf(stderr, "\n=== FULL SYNTAX TREE ===\n");
    print_tree_recursive(buf, root, 0);
    fprintf(stderr, "========================\n\n");
}

// Scheme bindings

static SCM scm_treesit_available_p(void) {
    return SCM_BOOL_T;
}

static SCM scm_treesit_language_available_p(SCM lang) {
    if (!scm_is_string(lang) && !scm_is_symbol(lang)) {
        scm_wrong_type_arg("treesit-language-available-p", 1, lang);
    }
    
    char *lang_name;
    if (scm_is_string(lang)) {
        lang_name = scm_to_locale_string(lang);
    } else {
        SCM str = scm_symbol_to_string(lang);
        lang_name = scm_to_locale_string(str);
    }
    
    bool available = treesit_language_available_p(lang_name);
    free(lang_name);
    
    return available ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_treesit_parser_create(SCM lang) {
    if (!scm_is_string(lang) && !scm_is_symbol(lang)) {
        scm_wrong_type_arg("treesit-parser-create", 1, lang);
    }
    
    if (!current_buffer) {
        return SCM_BOOL_F;
    }
    
    char *lang_name;
    if (scm_is_string(lang)) {
        lang_name = scm_to_locale_string(lang);
    } else {
        SCM str = scm_symbol_to_string(lang);
        lang_name = scm_to_locale_string(str);
    }
    
    // Delete old parser if exists
    if (current_buffer->ts_state) {
        treesit_parser_delete(current_buffer->ts_state);
    }
    
    current_buffer->ts_state = treesit_parser_create(lang_name);
    free(lang_name);
    
    if (current_buffer->ts_state) {
        treesit_parse_buffer(current_buffer);
        return SCM_BOOL_T;
    }
    
    return SCM_BOOL_F;
}

static SCM scm_treesit_parser_delete(void) {
    if (!current_buffer || !current_buffer->ts_state) {
        return SCM_BOOL_F;
    }
    
    treesit_parser_delete(current_buffer->ts_state);
    current_buffer->ts_state = NULL;
    
    return SCM_BOOL_T;
}

static SCM scm_treesit_set_highlight_query(SCM query_str) {
    if (!scm_is_string(query_str)) {
        scm_wrong_type_arg("treesit-set-highlight-query", 1, query_str);
    }
    
    if (!current_buffer || !current_buffer->ts_state) {
        return SCM_BOOL_F;
    }
    
    char *query = scm_to_locale_string(query_str);
    bool success = treesit_set_highlight_query(current_buffer->ts_state, query);
    free(query);
    
    return success ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM scm_treesit_apply_highlights(void) {
    if (!current_buffer) {
        return SCM_BOOL_F;
    }
    
    treesit_reparse_if_needed(current_buffer);
    treesit_apply_highlights(current_buffer);
    
    return SCM_BOOL_T;
}

static SCM scm_treesit_debug_tree(void) {
    if (!current_buffer) {
        return SCM_BOOL_F;
    }
    
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

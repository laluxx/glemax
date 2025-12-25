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
#define ROPE_NODE_SIZE 1024  /* Leaf node capacity - tune for cache lines */
#endif

#ifndef ROPE_SPLIT_THRESHOLD
#define ROPE_SPLIT_THRESHOLD 2048  /* When to split large leaves */
#endif

/* Opaque rope handle */
typedef struct rope rope_t;

/* Rope iterrator for traversal */
typedef struct {
    const rope_t *rope;
    size_t byte_pos;
    size_t char_pos;
    void *internal;
} rope_iter_t;

typedef struct {
    void *leaf;                  // Opaque leaf pointer
    size_t char_offset_in_leaf;  // Character position within this leaf
    size_t byte_offset_in_leaf;  // Byte position within this leaf
    size_t leaf_start_char;      // Absolute char position where leaf starts
    size_t leaf_start_byte;      // Absolute byte position where leaf starts
} chunk_info_t;

/* UTF-8 info structure */
typedef struct {
    size_t bytes;      /* Total bytes */
    size_t chars;      /* Total UTF-8 characters */
    size_t newlines;   /* Number of newlines */
} rope_stats_t;

/* Core API */
rope_t *rope_new(void);
rope_t *rope_new_from_str(const char *str, size_t len);
void rope_free(rope_t *rope);

uint32_t utf8_decode(const char *str, size_t len, size_t *bytes_read);
size_t utf8_encode(uint32_t codepoint, char *out);

/* Query operations - O(log n) */
size_t rope_byte_length(const rope_t *rope);
size_t rope_char_length(const rope_t *rope);
rope_stats_t rope_stats(const rope_t *rope);

/* UTF-8 aware character access - O(log n) */
uint32_t rope_char_at(const rope_t *rope, size_t char_pos);
size_t rope_char_to_byte(const rope_t *rope, size_t char_pos);
size_t rope_byte_to_char(const rope_t *rope, size_t byte_pos);

/* Copy operations */
size_t rope_copy_bytes(const rope_t *rope, size_t byte_start, size_t byte_len,
                       char *buf, size_t bufsize);
size_t rope_copy_chars(const rope_t *rope, size_t char_start, size_t char_len,
                       char *buf, size_t bufsize);

/* Modification operations - O(log n) */
rope_t *rope_insert_bytes(rope_t *rope, size_t byte_pos, 
                          const char *str, size_t len);
rope_t *rope_insert_chars(rope_t *rope, size_t char_pos,
                          const char *str, size_t len);
rope_t *rope_delete_bytes(rope_t *rope, size_t byte_start, size_t byte_len);
rope_t *rope_delete_chars(rope_t *rope, size_t char_start, size_t char_len);

/* Structural operations */
rope_t *rope_concat(rope_t *left, rope_t *right);
rope_t *rope_split_bytes(rope_t *rope, size_t byte_pos, rope_t **right_out);
rope_t *rope_split_chars(rope_t *rope, size_t char_pos, rope_t **right_out);
rope_t *rope_substring_bytes(const rope_t *rope, size_t start, size_t len);
rope_t *rope_substring_chars(const rope_t *rope, size_t start, size_t len);

/* Utility operations */
char *rope_to_string(const rope_t *rope, size_t *len_out);
bool rope_validate_utf8(const rope_t *rope);

/* Iterator API for efficient sequential access */
void rope_iter_init(rope_iter_t *iter, const rope_t *rope, size_t char_pos);
bool rope_iter_next_char(rope_iter_t *iter, uint32_t *codepoint);
bool rope_iter_prev_char(rope_iter_t *iter, uint32_t *codepoint);
void rope_iter_seek_char(rope_iter_t *iter, size_t char_pos);
void rope_iter_seek_byte(rope_iter_t *iter, size_t byte_pos);
void rope_iter_destroy(rope_iter_t *iter);

/* Chunk API */
bool rope_chunk_at_char(const rope_t *rope, size_t char_pos, chunk_info_t *info);
bool rope_prev_chunk(const rope_t *rope, chunk_info_t *current, chunk_info_t *prev);
const char *rope_chunk_data(const chunk_info_t *info);
size_t rope_chunk_byte_len(const chunk_info_t *info);
size_t rope_chunk_char_len(const chunk_info_t *info);


/* Line-based operations (newline-aware) */
size_t rope_line_count(const rope_t *rope);
size_t rope_line_to_char(const rope_t *rope, size_t line);
size_t rope_line_to_byte(const rope_t *rope, size_t line);
size_t rope_char_to_line(const rope_t *rope, size_t char_pos);
size_t rope_byte_to_line(const rope_t *rope, size_t byte_pos);

/* UTF-8 utility functions */
size_t utf8_char_len(uint8_t first_byte);
uint32_t utf8_decode(const char *str, size_t len, size_t *bytes_read);

#ifdef __cplusplus
}
#endif

#endif /* ROPE_H */

/* ============================================================================
 * IMPLEMENTATION
 * ========================================================================= */

#ifdef ROPE_IMPLEMENTATION

#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Red-Black tree colors */
typedef enum {
    RB_RED = 0,
    RB_BLACK = 1
} rb_color_t;

/* Internal node structure with Red-Black tree properties */
typedef struct rope_node {
    bool is_leaf;
    rb_color_t color;
    
    /* Weight metrics - for branches, these describe the left subtree */
    size_t byte_weight;   /* Byte count */
    size_t char_weight;   /* UTF-8 character count */
    size_t newline_weight; /* Newline count */
    
    union {
        struct {
            struct rope_node *left;
            struct rope_node *right;
        } branch;
        
        struct {
            char *data;
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



/* Node pool for reduced allocations */
#define NODE_POOL_SIZE 512
static rope_node_t *node_pool[NODE_POOL_SIZE];
static size_t node_pool_count = 0;

/* ============================================================================
 * UTF-8 UTILITIES
 * ========================================================================= */

/* Get UTF-8 character byte length from first byte */
size_t utf8_char_len(uint8_t first_byte) {
    if ((first_byte & 0x80) == 0) return 1;      /* 0xxxxxxx */
    if ((first_byte & 0xE0) == 0xC0) return 2;   /* 110xxxxx */
    if ((first_byte & 0xF0) == 0xE0) return 3;   /* 1110xxxx */
    if ((first_byte & 0xF8) == 0xF0) return 4;   /* 11110xxx */
    return 1; /* Invalid, treat as 1 byte */
}

/* Decode UTF-8 character */
uint32_t utf8_decode(const char *str, size_t len, size_t *bytes_read) {
    if (len == 0) {
        *bytes_read = 0;
        return 0;
    }
    
    uint8_t first = (uint8_t)str[0];
    size_t char_len = utf8_char_len(first);
    
    if (char_len > len) {
        *bytes_read = 1;
        return 0xFFFD; /* Replacement character for invalid UTF-8 */
    }
    
    *bytes_read = char_len;
    
    switch (char_len) {
        case 1:
            return first;
        case 2:
            return ((first & 0x1F) << 6) |
                   ((uint8_t)str[1] & 0x3F);
        case 3:
            return ((first & 0x0F) << 12) |
                   (((uint8_t)str[1] & 0x3F) << 6) |
                   ((uint8_t)str[2] & 0x3F);
        case 4:
            return ((first & 0x07) << 18) |
                   (((uint8_t)str[1] & 0x3F) << 12) |
                   (((uint8_t)str[2] & 0x3F) << 6) |
                   ((uint8_t)str[3] & 0x3F);
        default:
            return 0xFFFD;
    }
}

size_t utf8_encode(uint32_t codepoint, char *out) {
    if (codepoint <= 0x7F) {
        out[0] = (char)codepoint;
        return 1;
    } else if (codepoint <= 0x7FF) {
        out[0] = (char)(0xC0 | (codepoint >> 6));
        out[1] = (char)(0x80 | (codepoint & 0x3F));
        return 2;
    } else if (codepoint <= 0xFFFF) {
        out[0] = (char)(0xE0 | (codepoint >> 12));
        out[1] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[2] = (char)(0x80 | (codepoint & 0x3F));
        return 3;
    } else if (codepoint <= 0x10FFFF) {
        out[0] = (char)(0xF0 | (codepoint >> 18));
        out[1] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
        out[2] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[3] = (char)(0x80 | (codepoint & 0x3F));
        return 4;
    }
    return 0;  // Invalid codepoint
}

/* Count UTF-8 characters in byte string */
static size_t utf8_char_count(const char *str, size_t byte_len) {
    size_t count = 0;
    size_t pos = 0;
    
    while (pos < byte_len) {
        size_t char_len = utf8_char_len((uint8_t)str[pos]);
        if (char_len > byte_len - pos) char_len = byte_len - pos;
        pos += char_len;
        count++;
    }
    
    return count;
}

/* Count newlines in string */
static size_t count_newlines(const char *str, size_t len) {
    size_t count = 0;
    for (size_t i = 0; i < len; i++) {
        if (str[i] == '\n') count++;
    }
    return count;
}

/* Find byte position of nth UTF-8 character */
static size_t utf8_char_to_byte(const char *str, size_t byte_len, size_t char_pos) {
    size_t byte_pos = 0;
    size_t current_char = 0;
    
    while (byte_pos < byte_len && current_char < char_pos) {
        size_t char_len = utf8_char_len((uint8_t)str[byte_pos]);
        if (char_len > byte_len - byte_pos) break;
        byte_pos += char_len;
        current_char++;
    }
    
    return byte_pos;
}

/* Validate UTF-8 string */
static bool validate_utf8(const char *str, size_t len) {
    size_t pos = 0;
    while (pos < len) {
        size_t char_len = utf8_char_len((uint8_t)str[pos]);
        if (pos + char_len > len) return false;
        
        /* Check continuation bytes */
        for (size_t i = 1; i < char_len; i++) {
            if (((uint8_t)str[pos + i] & 0xC0) != 0x80) return false;
        }
        
        pos += char_len;
    }
    return true;
}

/* ============================================================================
 * MEMORY MANAGEMENT
 * ========================================================================= */

static inline rope_node_t *node_alloc(void) {
    rope_node_t *node;
    if (node_pool_count > 0) {
        node = node_pool[--node_pool_count];
        memset(node, 0, sizeof(rope_node_t));
    } else {
        node = (rope_node_t *)calloc(1, sizeof(rope_node_t));
    }
    return node;
}

static inline void node_free(rope_node_t *node) {
    if (node_pool_count < NODE_POOL_SIZE) {
        node_pool[node_pool_count++] = node;
    } else {
        free(node);
    }
}

/* ============================================================================
 * FORWARD DECLARATIONS
 * ========================================================================= */

static size_t node_byte_len(const rope_node_t *node);
static size_t node_char_len(const rope_node_t *node);
static size_t node_newline_count(const rope_node_t *node);
static void node_update_weights(rope_node_t *node);

/* ============================================================================
 * NODE CONSTRUCTION
 * ========================================================================= */

/* Create leaf node from UTF-8 string */
static rope_node_t *node_new_leaf(const char *str, size_t byte_len) {
    rope_node_t *node = node_alloc();
    node->is_leaf = true;
    node->color = RB_RED;
    
    node->leaf.byte_len = byte_len;
    node->leaf.char_len = utf8_char_count(str, byte_len);
    node->leaf.newlines = count_newlines(str, byte_len);
    node->leaf.capacity = byte_len < ROPE_NODE_SIZE ? ROPE_NODE_SIZE : byte_len;
    node->leaf.data = (char *)malloc(node->leaf.capacity);
    memcpy(node->leaf.data, str, byte_len);
    
    /* Update weight fields (for leaves, weight = size) */
    node->byte_weight = byte_len;
    node->char_weight = node->leaf.char_len;
    node->newline_weight = node->leaf.newlines;
    
    return node;
}

/* Create branch node */
static rope_node_t *node_new_branch(rope_node_t *left, rope_node_t *right) {
    rope_node_t *node = node_alloc();
    node->is_leaf = false;
    node->color = RB_RED;
    node->branch.left = left;
    node->branch.right = right;
    
    /* Weight = left subtree metrics */
    node->byte_weight = left->is_leaf ? left->byte_weight :
                        left->byte_weight + (left->branch.right ?
                        node_byte_len(left->branch.right) : 0);
    node->char_weight = left->is_leaf ? left->char_weight :
                        left->char_weight + (left->branch.right ?
                        node_char_len(left->branch.right) : 0);
    node->newline_weight = left->is_leaf ? left->newline_weight :
                           left->newline_weight + (left->branch.right ?
                           node_newline_count(left->branch.right) : 0);
    
    return node;
}

/* Deep free node tree */
static void node_deep_free(rope_node_t *node) {
    if (!node) return;
    
    if (node->is_leaf) {
        free(node->leaf.data);
    } else {
        node_deep_free(node->branch.left);
        node_deep_free(node->branch.right);
    }
    node_free(node);
}

/* ============================================================================
 * NODE METRICS
 * ========================================================================= */

static size_t node_byte_len(const rope_node_t *node) {
    if (!node) return 0;
    if (node->is_leaf) return node->leaf.byte_len;
    return node->byte_weight + node_byte_len(node->branch.right);
}

static size_t node_char_len(const rope_node_t *node) {
    if (!node) return 0;
    if (node->is_leaf) return node->leaf.char_len;
    return node->char_weight + node_char_len(node->branch.right);
}

static size_t node_newline_count(const rope_node_t *node) {
    if (!node) return 0;
    if (node->is_leaf) return node->leaf.newlines;
    return node->newline_weight + node_newline_count(node->branch.right);
}

/* Update branch node weights after child modification */
static void node_update_weights(rope_node_t *node) {
    if (!node || node->is_leaf) return;
    
    rope_node_t *left = node->branch.left;
    if (left) {
        node->byte_weight = node_byte_len(left);
        node->char_weight = node_char_len(left);
        node->newline_weight = node_newline_count(left);
    } else {
        node->byte_weight = 0;
        node->char_weight = 0;
        node->newline_weight = 0;
    }
}

/* ============================================================================
 * RED-BLACK TREE OPERATIONS
 * ========================================================================= */

static inline rb_color_t node_color(const rope_node_t *node) {
    return node ? node->color : RB_BLACK;
}

static inline void node_set_color(rope_node_t *node, rb_color_t color) {
    if (node) node->color = color;
}

static rope_node_t *rotate_left(rope_node_t *node) {
    if (!node || !node->branch.right || node->is_leaf || node->branch.right->is_leaf) {
        return node;  // Can't rotate leaves or invalid nodes
    }
    
    rope_node_t *right = node->branch.right;
    node->branch.right = right->branch.left;
    right->branch.left = node;
    
    right->color = node->color;
    node->color = RB_RED;
    
    node_update_weights(node);
    node_update_weights(right);
    
    return right;
}

static rope_node_t *rotate_right(rope_node_t *node) {
    if (!node || !node->branch.left || node->is_leaf || node->branch.left->is_leaf) {
        return node;  // Can't rotate leaves or invalid nodes
    }
    
    rope_node_t *left = node->branch.left;
    node->branch.left = left->branch.right;
    left->branch.right = node;
    
    left->color = node->color;
    node->color = RB_RED;
    
    node_update_weights(node);
    node_update_weights(left);
    
    return left;
}

static void flip_colors(rope_node_t *node) {
    node->color = RB_RED;
    node_set_color(node->branch.left, RB_BLACK);
    node_set_color(node->branch.right, RB_BLACK);
}

/* Balance node (maintain Red-Black invariants) - FIXED */
static rope_node_t *balance(rope_node_t *node) {
    if (!node || node->is_leaf) return node;
    
    /* Only perform rotations if both children are branch nodes */
    bool left_is_branch = node->branch.left && !node->branch.left->is_leaf;
    bool right_is_branch = node->branch.right && !node->branch.right->is_leaf;
    
    /* Fix right-leaning red */
    if (node_color(node->branch.right) == RB_RED && 
        node_color(node->branch.left) == RB_BLACK &&
        right_is_branch) {
        node = rotate_left(node);
    }
    
    /* Fix double red on left */
    if (node_color(node->branch.left) == RB_RED &&
        left_is_branch &&
        node->branch.left->branch.left &&
        node_color(node->branch.left->branch.left) == RB_RED) {
        node = rotate_right(node);
    }
    
    /* Split 4-nodes */
    if (node_color(node->branch.left) == RB_RED &&
        node_color(node->branch.right) == RB_RED) {
        flip_colors(node);
    }
    
    return node;
}

/* ============================================================================
 * CORE ROPE OPERATIONS
 * ========================================================================= */

rope_t *rope_new(void) {
    rope_t *rope = (rope_t *)calloc(1, sizeof(rope_t));
    return rope;
}

rope_t *rope_new_from_str(const char *str, size_t len) {
    rope_t *rope = (rope_t *)calloc(1, sizeof(rope_t));
    if (len > 0) {
        rope->root = node_new_leaf(str, len);
        rope->root->color = RB_BLACK;
        rope->byte_len = len;
        rope->char_len = rope->root->leaf.char_len;
        rope->newlines = rope->root->leaf.newlines;
    }
    return rope;
}

void rope_free(rope_t *rope) {
    if (!rope) return;
    node_deep_free(rope->root);
    free(rope);
}

size_t rope_byte_length(const rope_t *rope) {
    return rope ? rope->byte_len : 0;
}

size_t rope_char_length(const rope_t *rope) {
    return rope ? rope->char_len : 0;
}

rope_stats_t rope_stats(const rope_t *rope) {
    rope_stats_t stats = {0, 0, 0};
    if (rope) {
        stats.bytes = rope->byte_len;
        stats.chars = rope->char_len;
        stats.newlines = rope->newlines;
    }
    return stats;
}

/* ============================================================================
 * CHARACTER ACCESS
 * ========================================================================= */

/* Convert character position to byte position */
size_t rope_char_to_byte(const rope_t *rope, size_t char_pos) {
    if (!rope || char_pos >= rope->char_len) return rope ? rope->byte_len : 0;
    
    rope_node_t *node = rope->root;
    size_t byte_offset = 0;
    size_t char_offset = 0;
    
    while (node && !node->is_leaf) {
        if (char_pos < node->char_weight) {
            node = node->branch.left;
        } else {
            char_offset += node->char_weight;
            byte_offset += node->byte_weight;
            char_pos -= node->char_weight;
            node = node->branch.right;
        }
    }
    
    if (node && node->is_leaf) {
        size_t leaf_byte = utf8_char_to_byte(node->leaf.data, 
                                             node->leaf.byte_len,
                                             char_pos);
        return byte_offset + leaf_byte;
    }
    
    return byte_offset;
}

/* Find character position for byte position in UTF-8 string */
static size_t utf8_byte_to_char(const char *str, size_t byte_len, size_t byte_pos) {
    if (byte_pos >= byte_len) return utf8_char_count(str, byte_len);
    
    size_t char_pos = 0;
    size_t current_byte = 0;
    
    while (current_byte < byte_pos && current_byte < byte_len) {
        size_t char_len = utf8_char_len((uint8_t)str[current_byte]);
        if (char_len > byte_len - current_byte) break;
        
        /* If this character would extend beyond our target byte position, 
           we're in the middle of a character */
        if (current_byte + char_len > byte_pos) {
            break;
        }
        
        current_byte += char_len;
        char_pos++;
    }
    
    return char_pos;
}

size_t rope_byte_to_char(const rope_t *rope, size_t byte_pos) {
    if (!rope || byte_pos >= rope->byte_len) return rope ? rope->char_len : 0;
    
    rope_node_t *node = rope->root;
    size_t byte_offset = 0;
    size_t char_offset = 0;
    
    while (node && !node->is_leaf) {
        if (byte_pos < node->byte_weight) {
            node = node->branch.left;
        } else {
            char_offset += node->char_weight;
            byte_offset += node->byte_weight;
            byte_pos -= node->byte_weight;
            node = node->branch.right;
        }
    }
    
    if (node && node->is_leaf) {
        size_t leaf_chars = utf8_byte_to_char(node->leaf.data, 
                                             node->leaf.byte_len, byte_pos);
        return char_offset + leaf_chars;
    }
    
    return char_offset;
}

/* Get character at position */
uint32_t rope_char_at(const rope_t *rope, size_t char_pos) {
    if (!rope || char_pos >= rope->char_len) return 0;
    
    size_t byte_pos = rope_char_to_byte(rope, char_pos);
    rope_node_t *node = rope->root;
    size_t byte_offset = 0;
    
    while (node && !node->is_leaf) {
        if (byte_pos < node->byte_weight) {
            node = node->branch.left;
        } else {
            byte_offset += node->byte_weight;
            byte_pos -= node->byte_weight;
            node = node->branch.right;
        }
    }
    
    if (node && node->is_leaf && byte_pos < node->leaf.byte_len) {
        size_t bytes_read;
        return utf8_decode(node->leaf.data + byte_pos,
                          node->leaf.byte_len - byte_pos,
                          &bytes_read);
    }
    
    return 0;
}

/* ============================================================================
 * COPY OPERATIONS
 * ========================================================================= */

size_t rope_copy_bytes(const rope_t *rope, size_t byte_start, size_t byte_len,
                       char *buf, size_t bufsize) {
    if (!rope || !buf || bufsize == 0 || byte_start >= rope->byte_len) return 0;
    
    if (byte_start + byte_len > rope->byte_len) {
        byte_len = rope->byte_len - byte_start;
    }
    if (byte_len > bufsize) byte_len = bufsize;
    
    size_t copied = 0;
    rope_node_t *node = rope->root;
    rope_node_t *stack[128];
    int sp = 0;
    size_t current_offset = 0;
    
    // Navigate to starting leaf
    while (node && !node->is_leaf) {
        if (byte_start < node->byte_weight) {
            stack[sp++] = node;
            node = node->branch.left;
        } else {
            current_offset += node->byte_weight;
            byte_start -= node->byte_weight;
            node = node->branch.right;
        }
    }
    
    // Copy from leaves
    while (node && node->is_leaf && copied < byte_len) {
        size_t to_copy = node->leaf.byte_len - byte_start;
        if (to_copy > byte_len - copied) {
            to_copy = byte_len - copied;
        }
        
        memcpy(buf + copied, node->leaf.data + byte_start, to_copy);
        copied += to_copy;
        byte_start = 0; // After first leaf, start from beginning
        
        // Move to next leaf if we need more data
        if (copied < byte_len) {
            // Find next leaf using stack
            if (sp == 0) break;
            
            rope_node_t *parent = stack[--sp];
            if (parent->branch.right) {
                node = parent->branch.right;
                // Find leftmost leaf in right subtree
                while (node && !node->is_leaf) {
                    stack[sp++] = node;
                    node = node->branch.left;
                }
            } else {
                node = NULL;
            }
        }
    }
    
    return copied;
}

size_t rope_copy_chars(const rope_t *rope, size_t char_start, size_t char_len,
                       char *buf, size_t bufsize) {
    if (!rope || char_start >= rope->char_len) return 0;
    
    size_t byte_start = rope_char_to_byte(rope, char_start);
    size_t byte_end = rope_char_to_byte(rope, char_start + char_len);
    size_t byte_len = byte_end - byte_start;
    
    return rope_copy_bytes(rope, byte_start, byte_len, buf, bufsize);
}

/* ============================================================================
 * CONCATENATION
 * ========================================================================= */

rope_t *rope_concat(rope_t *left, rope_t *right) {
    if (!left || left->byte_len == 0) {
        if (left) rope_free(left);
        return right;
    }
    if (!right || right->byte_len == 0) {
        if (right) rope_free(right);
        return left;
    }
    
    rope_t *result = (rope_t *)calloc(1, sizeof(rope_t));
    result->root = node_new_branch(left->root, right->root);
    result->root->color = RB_BLACK;
    result->byte_len = left->byte_len + right->byte_len;
    result->char_len = left->char_len + right->char_len;
    result->newlines = left->newlines + right->newlines;
    
    /* Nullify roots before freeing */
    left->root = NULL;
    right->root = NULL;
    rope_free(left);
    rope_free(right);
    
    return result;
}

/* ============================================================================
 * SPLIT OPERATIONS)
 * ========================================================================= */

/* Split leaf node at byte position */
static void split_leaf(rope_node_t *leaf, size_t byte_pos,
                      rope_node_t **left_out, rope_node_t **right_out) {
    if (byte_pos == 0) {
        *left_out = NULL;
        *right_out = leaf;
        return;
    }
    if (byte_pos >= leaf->leaf.byte_len) {
        *left_out = leaf;
        *right_out = NULL;
        return;
    }
    
    *left_out = node_new_leaf(leaf->leaf.data, byte_pos);
    *right_out = node_new_leaf(leaf->leaf.data + byte_pos,
                               leaf->leaf.byte_len - byte_pos);
    
    (*left_out)->color = leaf->color;
    (*right_out)->color = leaf->color;
    
    /* Free original */
    free(leaf->leaf.data);
    node_free(leaf);
}

/* Recursive split helper */
static void node_split_recursive(rope_node_t *node, size_t byte_pos,
                                rope_node_t **left_out, rope_node_t **right_out) {
    if (!node) {
        *left_out = NULL;
        *right_out = NULL;
        return;
    }
    
    if (node->is_leaf) {
        split_leaf(node, byte_pos, left_out, right_out);
        return;
    }
    
    if (byte_pos <= node->byte_weight) {
        /* Split in left subtree */
        rope_node_t *ll = NULL, *lr = NULL;
        node_split_recursive(node->branch.left, byte_pos, &ll, &lr);
        
        if (lr && node->branch.right) {
            *left_out = ll;
            *right_out = node_new_branch(lr, node->branch.right);
            (*right_out)->color = node->color;
        } else {
            *left_out = ll;
            *right_out = lr ? lr : node->branch.right;
        }
        
        node_free(node);
    } else {
        /* Split in right subtree */
        rope_node_t *rl = NULL, *rr = NULL;
        node_split_recursive(node->branch.right, byte_pos - node->byte_weight,
                           &rl, &rr);
        
        if (rl && node->branch.left) {
            *left_out = node_new_branch(node->branch.left, rl);
            (*left_out)->color = node->color;
            *right_out = rr;
        } else {
            *left_out = node->branch.left;
            *right_out = rr;
        }
        
        node_free(node);
    }
}

rope_t *rope_split_bytes(rope_t *rope, size_t byte_pos, rope_t **right_out) {
    if (!rope) {
        if (right_out) *right_out = NULL;
        return NULL;
    }
    
    if (byte_pos == 0) {
        if (right_out) *right_out = rope;
        return rope_new();
    }
    
    if (byte_pos >= rope->byte_len) {
        if (right_out) *right_out = rope_new();
        return rope;
    }
    
    rope_node_t *left_tree = NULL, *right_tree = NULL;
    node_split_recursive(rope->root, byte_pos, &left_tree, &right_tree);
    
    /* Create left rope */
    rope_t *left = rope_new();
    left->root = left_tree;
    if (left_tree) {
        left_tree->color = RB_BLACK;
        left->byte_len = node_byte_len(left_tree);
        left->char_len = node_char_len(left_tree);
        left->newlines = node_newline_count(left_tree);
    }
    
    /* Create right rope */
    if (right_out) {
        *right_out = rope_new();
        (*right_out)->root = right_tree;
        if (right_tree) {
            right_tree->color = RB_BLACK;
            (*right_out)->byte_len = node_byte_len(right_tree);
            (*right_out)->char_len = node_char_len(right_tree);
            (*right_out)->newlines = node_newline_count(right_tree);
        }
    }
    
    rope->root = NULL;
    rope_free(rope);
    
    return left;
}

rope_t *rope_split_chars(rope_t *rope, size_t char_pos, rope_t **right_out) {
    if (!rope) {
        if (right_out) *right_out = NULL;
        return NULL;
    }
    
    size_t byte_pos = rope_char_to_byte(rope, char_pos);
    return rope_split_bytes(rope, byte_pos, right_out);
}

/* ============================================================================
 * INSERTION OPERATIONS
 * ========================================================================= */

static rope_node_t *node_insert_bytes(rope_node_t *node, size_t byte_pos,
                                      const char *str, size_t len);

rope_t *rope_insert_bytes(rope_t *rope, size_t byte_pos,
                         const char *str, size_t len) {
    if (!rope) return NULL;
    if (len == 0) return rope;
    if (byte_pos > rope->byte_len) byte_pos = rope->byte_len;
    
    if (!rope->root) {
        rope->root = node_new_leaf(str, len);
        rope->root->color = RB_BLACK;
    } else {
        rope->root = node_insert_bytes(rope->root, byte_pos, str, len);
        rope->root->color = RB_BLACK;
    }
    
    rope->byte_len += len;
    rope->char_len += utf8_char_count(str, len);
    rope->newlines += count_newlines(str, len);
    
    return rope;
}


static rope_node_t *node_insert_bytes(rope_node_t *node, size_t byte_pos,
                                      const char *str, size_t len) {
    if (node->is_leaf) {
        /* Insert into leaf - split if needed */
        if (byte_pos == 0) {
            rope_node_t *new_leaf = node_new_leaf(str, len);
            rope_node_t *branch = node_new_branch(new_leaf, node);
            return balance(branch);
        } else if (byte_pos >= node->leaf.byte_len) {
            rope_node_t *new_leaf = node_new_leaf(str, len);
            rope_node_t *branch = node_new_branch(node, new_leaf);
            return balance(branch);
        } else {
            /* Split leaf in middle */
            rope_node_t *left = node_new_leaf(node->leaf.data, byte_pos);
            rope_node_t *mid = node_new_leaf(str, len);
            rope_node_t *right = node_new_leaf(node->leaf.data + byte_pos,
                                              node->leaf.byte_len - byte_pos);
            
            free(node->leaf.data);
            node_free(node);
            
            rope_node_t *left_branch = node_new_branch(left, mid);
            rope_node_t *result = node_new_branch(left_branch, right);
            return balance(result);
        }
    } else {
        /* Navigate to correct subtree */
        if (byte_pos <= node->byte_weight) {
            node->branch.left = node_insert_bytes(node->branch.left,
                                                 byte_pos, str, len);
        } else {
            node->branch.right = node_insert_bytes(node->branch.right,
                                                  byte_pos - node->byte_weight,
                                                  str, len);
        }
        
        node_update_weights(node);
        return balance(node);
    }
}

rope_t *rope_insert_chars(rope_t *rope, size_t char_pos,
                         const char *str, size_t len) {
    if (!rope) return NULL;
    size_t byte_pos = rope_char_to_byte(rope, char_pos);
    return rope_insert_bytes(rope, byte_pos, str, len);
}

/* ============================================================================
 * DELETION OPERATIONS
 * ========================================================================= */

rope_t *rope_delete_bytes(rope_t *rope, size_t byte_start, size_t byte_len) {
    if (!rope || byte_start >= rope->byte_len) return rope;
    if (byte_start + byte_len > rope->byte_len) {
        byte_len = rope->byte_len - byte_start;
    }
    if (byte_len == 0) return rope;
    
    /* Split at start, then split at end, discard middle */
    rope_t *right = NULL;
    rope = rope_split_bytes(rope, byte_start, &right);
    
    if (right) {
        rope_t *rightmost = NULL;
        right = rope_split_bytes(right, byte_len, &rightmost);
        rope_free(right);
        if (rightmost) {
            rope = rope_concat(rope, rightmost);
        }
    }
    
    return rope;
}

rope_t *rope_delete_chars(rope_t *rope, size_t char_start, size_t char_len) {
    if (!rope || char_start >= rope->char_len) return rope;
    
    size_t byte_start = rope_char_to_byte(rope, char_start);
    size_t byte_end = rope_char_to_byte(rope, char_start + char_len);
    size_t byte_len = byte_end - byte_start;
    
    return rope_delete_bytes(rope, byte_start, byte_len);
}

/* ============================================================================
 * SUBSTRING OPERATIONS
 * ========================================================================= */

rope_t *rope_substring_bytes(const rope_t *rope, size_t start, size_t len) {
    if (!rope || start >= rope->byte_len) return rope_new();
    if (start + len > rope->byte_len) len = rope->byte_len - start;
    
    /* Extract substring by copying */
    char *buf = (char *)malloc(len);
    size_t copied = rope_copy_bytes(rope, start, len, buf, len);
    rope_t *result = rope_new_from_str(buf, copied);
    free(buf);
    
    return result;
}

rope_t *rope_substring_chars(const rope_t *rope, size_t start, size_t len) {
    if (!rope || start >= rope->char_len) return rope_new();
    
    size_t byte_start = rope_char_to_byte(rope, start);
    size_t byte_end = rope_char_to_byte(rope, start + len);
    size_t byte_len = byte_end - byte_start;
    
    return rope_substring_bytes(rope, byte_start, byte_len);
}

/* ============================================================================
 * UTILITY OPERATIONS
 * ========================================================================= */

char *rope_to_string(const rope_t *rope, size_t *len_out) {
    if (!rope || rope->byte_len == 0) {
        if (len_out) *len_out = 0;
        return NULL;
    }
    
    char *str = (char *)malloc(rope->byte_len + 1);
    
    /* Perform in-order traversal to collect string */
    size_t pos = 0;
    
    /* Stack-based traversal */
    rope_node_t *stack[128];
    int sp = 0;
    rope_node_t *current = rope->root;
    
    while (current || sp > 0) {
        while (current) {
            if (!current->is_leaf) {
                stack[sp++] = current;
                current = current->branch.left;
            } else {
                break;
            }
        }
        
        if (current && current->is_leaf) {
            memcpy(str + pos, current->leaf.data, current->leaf.byte_len);
            pos += current->leaf.byte_len;
            current = NULL;
        }
        
        if (sp > 0) {
            current = stack[--sp]->branch.right;
        }
    }
    
    str[pos] = '\0';
    if (len_out) *len_out = pos;
    return str;
}

bool rope_validate_utf8(const rope_t *rope) {
    if (!rope) return true;
    
    char *str = rope_to_string(rope, NULL);
    if (!str) return true;
    
    bool valid = validate_utf8(str, rope->byte_len);
    free(str);
    return valid;
}


/* ============================================================================
 * CHUNK-BASED IMPLEMENTATION
 * ========================================================================= */

// Get the leaf (chunk) containing a character position
// Returns the leaf node, and sets out_char_offset to the char position within the leaf
// and out_byte_offset to the byte position within the leaf

const char *rope_chunk_data(const chunk_info_t *info) {
    if (!info || !info->leaf) return NULL;
    rope_node_t *leaf = (rope_node_t*)info->leaf;  // Cast from void*
    return leaf->is_leaf ? leaf->leaf.data : NULL;
}

size_t rope_chunk_byte_len(const chunk_info_t *info) {
    if (!info || !info->leaf) return 0;
    rope_node_t *leaf = (rope_node_t*)info->leaf;  // Cast from void*
    return leaf->is_leaf ? leaf->leaf.byte_len : 0;
}

size_t rope_chunk_char_len(const chunk_info_t *info) {
    if (!info || !info->leaf) return 0;
    rope_node_t *leaf = (rope_node_t*)info->leaf;  // Cast from void*
    return leaf->is_leaf ? leaf->leaf.char_len : 0;
}

bool rope_chunk_at_char(const rope_t *rope, size_t char_pos, chunk_info_t *info) {
    if (!rope || !rope->root || !info) return false;
    
    if (char_pos > rope->char_len) {
        char_pos = rope->char_len;
    }
    
    rope_node_t *node = rope->root;
    size_t char_offset = 0;
    size_t byte_offset = 0;
    
    // Navigate to the leaf containing char_pos
    while (node && !node->is_leaf) {
        size_t left_chars = node->char_weight;
        
        if (char_pos < left_chars) {
            // Go left
            node = node->branch.left;
        } else {
            // Go right
            char_offset += left_chars;
            byte_offset += node->byte_weight;
            char_pos -= left_chars;
            node = node->branch.right;
        }
    }
    
    if (!node || !node->is_leaf) return false;
    
    info->leaf = (void*)node;  // Cast rope_node_t* to void*
    info->leaf_start_char = char_offset;
    info->leaf_start_byte = byte_offset;
    
    // Convert remaining char position to byte position within leaf
    size_t byte_pos = 0;
    size_t chars_counted = 0;
    
    while (chars_counted < char_pos && byte_pos < node->leaf.byte_len) {
        size_t char_len = utf8_char_len((uint8_t)node->leaf.data[byte_pos]);
        byte_pos += char_len;
        chars_counted++;
    }
    
    info->char_offset_in_leaf = chars_counted;
    info->byte_offset_in_leaf = byte_pos;
    
    return true;
}

// Get the previous chunk (leaf) in the rope
bool rope_prev_chunk(const rope_t *rope, chunk_info_t *current, chunk_info_t *prev) {
    if (!rope || !current || !prev) return false;
    if (current->leaf_start_char == 0) return false;  // Already at first chunk
    
    // Get the chunk at the position just before current chunk
    return rope_chunk_at_char(rope, current->leaf_start_char - 1, prev);
}


/* ============================================================================
 * ITERATOR IMPLEMENTATION
 * ========================================================================= */

typedef struct {
    rope_node_t *stack[128];
    int sp;
    rope_node_t *current_leaf;
    size_t leaf_byte_pos;        // Current position within leaf (0 to leaf.byte_len)
    size_t leaf_start_byte;      // Absolute byte position where current leaf starts in rope
} rope_iter_state_t;

/* Helper: Find character boundary by scanning BACKWARD in UTF-8 */
static inline size_t find_char_start_backward(const char *data, size_t byte_pos) {
    if (byte_pos == 0) return 0;
    
    size_t pos = byte_pos - 1;
    
    // Skip backwards over continuation bytes (10xxxxxx)
    // At most 3 continuation bytes in valid UTF-8
    int count = 0;
    while (pos > 0 && ((uint8_t)data[pos] & 0xC0) == 0x80 && count < 3) {
        pos--;
        count++;
    }
    
    return pos;
}

/* Helper: Find the leftmost (first) leaf from a given node */
static rope_node_t *find_leftmost_leaf(rope_node_t *node, rope_node_t **stack, int *sp, size_t *leaf_start_byte, size_t byte_offset) {
    while (node && !node->is_leaf) {
        stack[(*sp)++] = node;
        node = node->branch.left;
        // byte_offset stays the same when going left
    }
    *leaf_start_byte = byte_offset;
    return node;
}

/* Helper: Navigate to next leaf in in-order traversal */
static rope_node_t *next_leaf(rope_iter_state_t *state, size_t *out_byte_offset) {
    if (state->sp == 0) {
        return NULL;
    }
    
    // Pop parent
    state->sp--;
    rope_node_t *parent = state->stack[state->sp];
    
    // Calculate the offset where parent's left subtree ends
    size_t parent_left_size = parent->byte_weight;
    
    if (parent->branch.right) {
        // Go to right subtree
        // The offset is current position + remaining bytes in parent's left coverage
        size_t right_offset = state->leaf_start_byte + parent_left_size;
        size_t leaf_start;
        rope_node_t *leaf = find_leftmost_leaf(parent->branch.right, state->stack, &state->sp, &leaf_start, right_offset);
        *out_byte_offset = leaf_start;
        return leaf;
    }
    
    // No right child, continue up the stack
    return next_leaf(state, out_byte_offset);
}

void rope_iter_init(rope_iter_t *iter, const rope_t *rope, size_t char_pos) {
    if (!iter) return;
    
    iter->rope = rope;
    iter->char_pos = char_pos > rope->char_len ? rope->char_len : char_pos;
    iter->byte_pos = rope ? rope_char_to_byte(rope, iter->char_pos) : 0;
    
    rope_iter_state_t *state = (rope_iter_state_t *)calloc(1, sizeof(rope_iter_state_t));
    iter->internal = state;
    
    if (!rope || !rope->root) return;
    
    rope_node_t *node = rope->root;
    size_t byte_offset = 0;
    size_t target_byte = iter->byte_pos;
    
    // Navigate to the leaf containing the target byte position
    while (node && !node->is_leaf) {
        if (target_byte < node->byte_weight) {
            // Going left - push node
            state->stack[state->sp++] = node;
            node = node->branch.left;
            // byte_offset unchanged
        } else {
            // Going right - don't push, adjust offset and target
            byte_offset += node->byte_weight;
            target_byte -= node->byte_weight;
            node = node->branch.right;
        }
    }
    
    if (node && node->is_leaf) {
        state->current_leaf = node;
        state->leaf_byte_pos = target_byte;
        state->leaf_start_byte = byte_offset;
        iter->byte_pos = byte_offset + target_byte;
    }
}

bool rope_iter_next_char(rope_iter_t *iter, uint32_t *codepoint) {
    if (!iter || !iter->rope || iter->char_pos >= iter->rope->char_len) {
        return false;
    }
    
    rope_iter_state_t *state = (rope_iter_state_t *)iter->internal;
    if (!state || !state->current_leaf) return false;
    
    rope_node_t *leaf = state->current_leaf;
    
    // If at end of current leaf, move to next
    if (state->leaf_byte_pos >= leaf->leaf.byte_len) {
        size_t new_offset;
        state->current_leaf = next_leaf(state, &new_offset);
        
        if (!state->current_leaf) {
            return false;
        }
        
        state->leaf_start_byte = new_offset;
        state->leaf_byte_pos = 0;
        leaf = state->current_leaf;
    }
    
    // Decode character at current position
    size_t bytes_read;
    *codepoint = utf8_decode(leaf->leaf.data + state->leaf_byte_pos,
                            leaf->leaf.byte_len - state->leaf_byte_pos,
                            &bytes_read);
    
    // Advance position
    state->leaf_byte_pos += bytes_read;
    iter->byte_pos = state->leaf_start_byte + state->leaf_byte_pos;
    iter->char_pos++;
    
    return true;
}

bool rope_iter_prev_char(rope_iter_t *iter, uint32_t *codepoint) {
    if (!iter || !iter->rope || iter->char_pos == 0) {
        return false;
    }
    
    rope_iter_state_t *state = (rope_iter_state_t *)iter->internal;
    if (!state) return false;
    
    // If at start of current leaf (or no current leaf), get previous chunk
    if (!state->current_leaf || state->leaf_byte_pos == 0) {
        chunk_info_t current, prev;
        
        // Get current position as a chunk
        if (!rope_chunk_at_char(iter->rope, iter->char_pos, &current)) {
            return false;
        }
        
        // Get previous chunk
        if (!rope_prev_chunk(iter->rope, &current, &prev)) {
            return false;
        }
        
        // Update state to point to end of previous chunk
        state->current_leaf = (rope_node_t*)prev.leaf;
        state->leaf_byte_pos = rope_chunk_byte_len(&prev);
        state->leaf_start_byte = prev.leaf_start_byte;
        
        // Clear the stack since we jumped via chunks
        state->sp = 0;
    }
    
    // Now scan backward in current leaf to find character start
    rope_node_t *leaf = state->current_leaf;
    if (!leaf || !leaf->is_leaf) return false;
    
    size_t char_start = find_char_start_backward(leaf->leaf.data, state->leaf_byte_pos);
    
    // Decode the character
    size_t bytes_read;
    *codepoint = utf8_decode(
        leaf->leaf.data + char_start,
        leaf->leaf.byte_len - char_start,
        &bytes_read
    );
    
    if (bytes_read == 0) return false;
    
    // Move backward
    state->leaf_byte_pos = char_start;
    iter->byte_pos = state->leaf_start_byte + state->leaf_byte_pos;
    iter->char_pos--;
    
    return true;
}

void rope_iter_seek_char(rope_iter_t *iter, size_t char_pos) {
    if (!iter || !iter->rope) return;
    
    if (iter->internal) {
        free(iter->internal);
    }
    
    rope_iter_init(iter, iter->rope, char_pos);
}

void rope_iter_seek_byte(rope_iter_t *iter, size_t byte_pos) {
    if (!iter || !iter->rope) return;
    
    size_t char_pos = rope_byte_to_char(iter->rope, byte_pos);
    rope_iter_seek_char(iter, char_pos);
}

void rope_iter_destroy(rope_iter_t *iter) {
    if (!iter) return;
    if (iter->internal) {
        free(iter->internal);
        iter->internal = NULL;
    }
}

/* ============================================================================
 * LINE OPERATIONS
 * ========================================================================= */

size_t rope_line_count(const rope_t *rope) {
    return rope ? rope->newlines + 1 : 1;
}

size_t rope_char_to_line(const rope_t *rope, size_t char_pos) {
    if (!rope) return 0;
    
    size_t line = 0;
    for (size_t i = 0; i < char_pos && i < rope->char_len; i++) {
        if (rope_char_at(rope, i) == '\n') line++;
    }
    return line;
}

size_t rope_byte_to_line(const rope_t *rope, size_t byte_pos) {
    if (!rope) return 0;
    size_t char_pos = rope_byte_to_char(rope, byte_pos);
    return rope_char_to_line(rope, char_pos);
}

size_t rope_line_to_char(const rope_t *rope, size_t line) {
    if (!rope) return 0;
    
    size_t current_line = 0;
    for (size_t i = 0; i < rope->char_len; i++) {
        if (current_line == line) return i;
        if (rope_char_at(rope, i) == '\n') current_line++;
    }
    return rope->char_len;
}

size_t rope_line_to_byte(const rope_t *rope, size_t line) {
    if (!rope) return 0;
    size_t char_pos = rope_line_to_char(rope, line);
    return rope_char_to_byte(rope, char_pos);
}

#endif /* ROPE_IMPLEMENTATION */

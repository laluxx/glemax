#pragma once
#include <stddef.h>
#include <libguile.h>
#include <stdbool.h>

typedef struct Buffer Buffer;

typedef struct PropValue {
    SCM key;
    SCM value;
    struct PropValue *next;
} PropValue;

// Augmented BST interval tree node
// Sorted by start position, augmented with max_end for O(log n) stabbing queries
typedef struct ITreeNode {
    size_t start;
    size_t end;
    size_t max_end;       // Maximum end in this subtree — the augmentation key
    int    height;        // AVL height for balancing
    PropValue *props;
    struct ITreeNode *left;
    struct ITreeNode *right;
} ITreeNode;

typedef struct {
    ITreeNode *root;
    size_t     count;
} IntervalTree;

void put_text_property(Buffer *buf, size_t start, size_t end, SCM prop, SCM value);
SCM get_text_property(Buffer *buf, size_t pos, SCM prop);
int get_text_property_face(Buffer *buf, size_t pos);
SCM get_text_property_field(Buffer *buf, size_t pos);

size_t next_property_change(Buffer *buf, size_t pos);
size_t previous_property_change(Buffer *buf, size_t pos);
size_t next_single_property_change(Buffer *buf, size_t pos, SCM prop);
size_t previous_single_property_change(Buffer *buf, size_t pos, SCM prop);

void remove_text_properties(Buffer *buf, size_t start, size_t end);
void clear_text_properties(Buffer *buf);
void adjust_text_properties(Buffer *buf, size_t pos, int delta);
bool is_range_readonly(Buffer *buf, size_t start, size_t end);

void init_textprop_bindings(void);

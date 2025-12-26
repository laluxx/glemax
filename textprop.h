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

// TODO We might want to use an interval tree [[https://en.wikipedia.org/wiki/Interval_tree][Interval tree]]
// https://en.wikipedia.org/wiki/Interval_tree

typedef struct TextProp {
    size_t start;
    size_t end;
    PropValue *props;
    struct TextProp *next;
} TextProp;

void put_text_property(Buffer *buf, size_t start, size_t end, SCM prop, SCM value);
SCM get_text_property(Buffer *buf, size_t pos, SCM prop);

int get_text_property_face(Buffer *buf, size_t pos);
SCM get_text_property_field(Buffer *buf, size_t pos);

void remove_text_properties(Buffer *buf, size_t start, size_t end);
void clear_text_properties(Buffer *buf);
void adjust_text_properties(Buffer *buf, size_t pos, int delta);
void init_textprop_bindings(void);
void protect_text_properties(Buffer *buf);
void unprotect_text_properties(Buffer *buf);
bool is_range_readonly(Buffer *buf, size_t start, size_t end);


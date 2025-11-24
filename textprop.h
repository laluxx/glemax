#pragma once
#include <stddef.h>

typedef struct Buffer Buffer;

typedef struct TextProp {
    size_t start;
    size_t end;
    int face_id;
    struct TextProp *next;
} TextProp;

void put_text_property(Buffer *buf, size_t start, size_t end, int face_id);
int get_text_property_face(Buffer *buf, size_t pos);
void remove_text_properties(Buffer *buf, size_t start, size_t end);
void clear_text_properties(Buffer *buf);
void adjust_text_properties(Buffer *buf, size_t pos, int delta);

void init_textprop_bindings(void);

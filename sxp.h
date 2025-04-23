#ifndef SXP_H
#define SXP_H

#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>

typedef struct {
    char *data;
    size_t size;
    size_t capacity;
    int indent;
    bool at_line_start;
    bool in_list;
    bool needs_space;
    size_t list_start_pos;
    int list_item_count;
    int parent_list_item_count;
} SxpBuffer;

typedef enum {
    SXP_NIL,
    SXP_LIST,
    SXP_SYMBOL,
    SXP_STRING
} SxpType;

typedef struct SxpValue SxpValue;
typedef struct SxpList SxpList;

struct SxpList {
    SxpValue **items;
    size_t count;
    size_t capacity;
};

struct SxpValue {
    SxpType type;
    union {
        SxpList list;
        char *symbol;
        char *string;
    };
};

extern SxpBuffer sxpBuffer;

// Writer functions
void sxp_init(SxpBuffer *buf);
void sxp_free(SxpBuffer *buf);
void sxp_write(SxpBuffer *buf, const char *text);
void sxp_start_list(SxpBuffer *buf);
void sxp_end_list(SxpBuffer *buf);
void sxp_add_symbol(SxpBuffer *buf, const char *sym);
void sxp_add_string(SxpBuffer *buf, const char *str);
void sxp_newline(SxpBuffer *buf);
void sxp_write_to_file(SxpBuffer *buf, FILE *fp);

// Parser functions
SxpValue* sxp_nil();
SxpValue* sxp_make_list();
SxpValue* sxp_make_symbol(const char *symbol);
SxpValue* sxp_make_string(const char *string);
void sxp_list_add(SxpValue *list, SxpValue *value);
void sxp_free_value(SxpValue *value);
SxpValue* sxp_parse_file(const char *filename);
SxpValue* sxp_parse_string(const char *str, size_t len);
SxpValue* sxp_get_list_item(SxpValue *list, size_t index);
const char* sxp_get_symbol(SxpValue *value);
const char* sxp_get_string(SxpValue *value);
size_t sxp_list_length(SxpValue *list);

#endif // SXP_H

#include "sxp.h"
#include "globals.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#define INITIAL_CAPACITY 256
#define INITIAL_LIST_CAPACITY 8

SxpBuffer sxpBuffer;

static void ensure_capacity(SxpBuffer *buf, size_t needed) {
    if (buf->size + needed >= buf->capacity) {
        buf->capacity = buf->capacity ? buf->capacity * 2 : INITIAL_CAPACITY;
        buf->data = realloc(buf->data, buf->capacity);
        assert(buf->data);
    }
}

static void write_indent(SxpBuffer *buf) {
    for (int i = 0; i < buf->indent; i++) {
        sxp_write(buf, " ");
    }
}

void sxp_init(SxpBuffer *buf) {
    memset(buf, 0, sizeof(*buf));
    buf->list_start_pos = 0;
    buf->list_item_count = 0;
}

void sxp_free(SxpBuffer *buf) {
    free(buf->data);
    memset(buf, 0, sizeof(*buf));
}

void sxp_write(SxpBuffer *buf, const char *text) {
    size_t len = strlen(text);
    ensure_capacity(buf, len);
    memcpy(buf->data + buf->size, text, len);
    buf->size += len;
    buf->at_line_start = false;
    buf->needs_space = true;
}

void sxp_start_list(SxpBuffer *buf) {
    buf->parent_list_item_count = buf->list_item_count;
    buf->list_item_count = 0;
    
    if (buf->at_line_start) {
        write_indent(buf);
    } else if (buf->needs_space && !buf->at_line_start) {
        sxp_write(buf, " ");
    }
    
    buf->list_start_pos = buf->size;
    sxp_write(buf, "(");
    buf->in_list = true;
    buf->indent += sxp_indentation;
    buf->needs_space = false;
}

void sxp_end_list(SxpBuffer *buf) {
    buf->indent -= sxp_indentation;
    
    if (buf->list_item_count == 0) {
        sxp_write(buf, ")");
    } else if (buf->list_item_count > 0 && buf->at_line_start) {
        write_indent(buf);
        sxp_write(buf, ")");
    } else {
        sxp_write(buf, ")");
    }
    
    buf->list_item_count = buf->parent_list_item_count;
    buf->in_list = (buf->indent > 0);
}

void sxp_add_symbol(SxpBuffer *buf, const char *sym) {
    if (buf->at_line_start) {
        write_indent(buf);
    } else if (buf->needs_space) {
        sxp_write(buf, " ");
    }
    
    sxp_write(buf, sym);
    buf->list_item_count++;
}

void sxp_add_string(SxpBuffer *buf, const char *str) {
    if (buf->at_line_start) {
        write_indent(buf);
    } else if (buf->needs_space) {
        sxp_write(buf, " ");
    }
    
    sxp_write(buf, "\"");
    for (const char *c = str; *c; c++) {
        switch (*c) {
            case '"': sxp_write(buf, "\\\""); break;
            case '\\': sxp_write(buf, "\\\\"); break;
            case '\n': sxp_write(buf, "\\n"); break;
            default: {
                char ch[2] = {*c, '\0'};
                sxp_write(buf, ch);
            }
        }
    }
    sxp_write(buf, "\"");
    
    buf->list_item_count++;
}

void sxp_newline(SxpBuffer *buf) {
    sxp_write(buf, "\n");
    buf->at_line_start = true;
    buf->needs_space = false;
}

void sxp_write_to_file(SxpBuffer *buf, FILE *fp) {
    fwrite(buf->data, 1, buf->size, fp);
}

/* Parser Implementation */

static char* sxp_strdup(const char *str) {
    size_t len = strlen(str);
    char *dup = malloc(len + 1);
    if (dup) {
        memcpy(dup, str, len + 1);
    }
    return dup;
}

SxpValue* sxp_nil() {
    SxpValue *val = calloc(1, sizeof(SxpValue));
    val->type = SXP_NIL;
    return val;
}

SxpValue* sxp_make_list() {
    SxpValue *val = calloc(1, sizeof(SxpValue));
    val->type = SXP_LIST;
    val->list.capacity = INITIAL_LIST_CAPACITY;
    val->list.items = calloc(val->list.capacity, sizeof(SxpValue*));
    return val;
}

SxpValue* sxp_make_symbol(const char *symbol) {
    SxpValue *val = calloc(1, sizeof(SxpValue));
    val->type = SXP_SYMBOL;
    val->symbol = sxp_strdup(symbol);
    return val;
}

SxpValue* sxp_make_string(const char *string) {
    SxpValue *val = calloc(1, sizeof(SxpValue));
    val->type = SXP_STRING;
    val->string = sxp_strdup(string);
    return val;
}

void sxp_list_add(SxpValue *list, SxpValue *value) {
    if (list->type != SXP_LIST) return;
    
    if (list->list.count >= list->list.capacity) {
        list->list.capacity *= 2;
        list->list.items = realloc(list->list.items, list->list.capacity * sizeof(SxpValue*));
    }
    
    list->list.items[list->list.count++] = value;
}

void sxp_free_value(SxpValue *value) {
    if (!value) return;
    
    switch (value->type) {
        case SXP_LIST:
            for (size_t i = 0; i < value->list.count; i++) {
                sxp_free_value(value->list.items[i]);
            }
            free(value->list.items);
            break;
        case SXP_SYMBOL:
            free(value->symbol);
            break;
        case SXP_STRING:
            free(value->string);
            break;
        default:
            break;
    }
    
    free(value);
}

static void skip_whitespace(const char **ptr, size_t *remaining) {
    while (*remaining > 0) {
        if (isspace((unsigned char)**ptr)) {
            (*ptr)++;
            (*remaining)--;
        } else if (**ptr == ';') {
            while (*remaining > 0 && **ptr != '\n') {
                (*ptr)++;
                (*remaining)--;
            }
            if (*remaining > 0) {
                (*ptr)++;
                (*remaining)--;
            }
        } else {
            break;
        }
    }
}

static SxpValue* parse_value(const char **ptr, size_t *remaining);

static SxpValue* parse_list(const char **ptr, size_t *remaining) {
    (*ptr)++; // Skip opening paren
    (*remaining)--;
    
    SxpValue *list = sxp_make_list();
    
    skip_whitespace(ptr, remaining);
    
    while (*remaining > 0 && **ptr != ')') {
        SxpValue *item = parse_value(ptr, remaining);
        if (item) {
            sxp_list_add(list, item);
        }
        skip_whitespace(ptr, remaining);
    }
    
    if (*remaining > 0 && **ptr == ')') {
        (*ptr)++;
        (*remaining)--;
    }
    
    return list;
}

static SxpValue* parse_string(const char **ptr, size_t *remaining) {
    (*ptr)++; // Skip opening quote
    (*remaining)--;
    
    const char *start = *ptr;
    size_t len = 0;
    bool escaped = false;
    
    while (*remaining > 0) {
        char c = **ptr;
        
        if (escaped) {
            escaped = false;
        } else if (c == '\\') {
            escaped = true;
        } else if (c == '"') {
            break;
        }
        
        (*ptr)++;
        (*remaining)--;
        len++;
    }
    
    if (*remaining > 0 && **ptr == '"') {
        (*ptr)++;
        (*remaining)--;
    }
    
    char *str = malloc(len + 1);
    if (!str) return sxp_nil();
    
    const char *src = start;
    char *dst = str;
    size_t i = 0;
    
    escaped = false;
    while (i < len) {
        if (escaped) {
            switch (*src) {
                case 'n': *dst = '\n'; break;
                case 't': *dst = '\t'; break;
                case 'r': *dst = '\r'; break;
                default: *dst = *src; break;
            }
            escaped = false;
        } else if (*src == '\\') {
            escaped = true;
            src++;
            i++;
            continue;
        } else {
            *dst = *src;
        }
        
        dst++;
        src++;
        i++;
    }
    
    *dst = '\0';
    
    SxpValue *value = sxp_make_string(str);
    free(str);
    
    return value;
}

static SxpValue* parse_symbol(const char **ptr, size_t *remaining) {
    const char *start = *ptr;
    size_t len = 0;
    
    while (*remaining > 0 && !isspace((unsigned char)**ptr) && **ptr != '(' && **ptr != ')') {
        (*ptr)++;
        (*remaining)--;
        len++;
    }
    
    char *symbol = malloc(len + 1);
    if (!symbol) return sxp_nil();
    
    memcpy(symbol, start, len);
    symbol[len] = '\0';
    
    SxpValue *value = sxp_make_symbol(symbol);
    free(symbol);
    
    return value;
}

static SxpValue* parse_value(const char **ptr, size_t *remaining) {
    skip_whitespace(ptr, remaining);
    
    if (*remaining == 0) return sxp_nil();
    
    if (**ptr == '(') {
        return parse_list(ptr, remaining);
    } else if (**ptr == '"') {
        return parse_string(ptr, remaining);
    } else {
        return parse_symbol(ptr, remaining);
    }
}

SxpValue* sxp_parse_string(const char *str, size_t len) {
    const char *ptr = str;
    size_t remaining = len;
    
    skip_whitespace(&ptr, &remaining);
    
    if (remaining == 0) return sxp_nil();
    
    return parse_value(&ptr, &remaining);
}

SxpValue* sxp_parse_file(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) return sxp_nil();
    
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    char *buffer = malloc(size);
    if (!buffer) {
        fclose(fp);
        return sxp_nil();
    }
    
    size_t bytes_read = fread(buffer, 1, size, fp);
    fclose(fp);
    
    SxpValue *result = sxp_parse_string(buffer, bytes_read);
    free(buffer);
    
    return result;
}

SxpValue* sxp_get_list_item(SxpValue *list, size_t index) {
    if (!list || list->type != SXP_LIST || index >= list->list.count) {
        return NULL;
    }
    return list->list.items[index];
}

const char* sxp_get_symbol(SxpValue *value) {
    if (!value || value->type != SXP_SYMBOL) return NULL;
    return value->symbol;
}

const char* sxp_get_string(SxpValue *value) {
    if (!value || value->type != SXP_STRING) return NULL;
    return value->string;
}

size_t sxp_list_length(SxpValue *list) {
    if (!list || list->type != SXP_LIST) return 0;
    return list->list.count;
}

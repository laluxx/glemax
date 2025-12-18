#include "modeline.h"
#include "faces.h"
#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Helper to append a string to a dynamic buffer
typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} StrBuf;

static void strbuf_init(StrBuf *buf) {
    buf->capacity = 256;
    buf->length = 0;
    buf->data = malloc(buf->capacity);
    buf->data[0] = '\0';
}

static void strbuf_append(StrBuf *buf, const char *str) {
    if (!str) return;
    
    size_t str_len = strlen(str);
    while (buf->length + str_len + 1 > buf->capacity) {
        buf->capacity *= 2;
        buf->data = realloc(buf->data, buf->capacity);
    }
    
    strcpy(buf->data + buf->length, str);
    buf->length += str_len;
}

static void strbuf_append_char(StrBuf *buf, char c) {
    if (buf->length + 2 > buf->capacity) {
        buf->capacity *= 2;
        buf->data = realloc(buf->data, buf->capacity);
    }
    
    buf->data[buf->length++] = c;
    buf->data[buf->length] = '\0';
}

// Count lines in buffer up to position
static size_t count_lines_to_point(Buffer *buf, size_t point) {
    size_t line = 1;
    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, 0);
    
    uint32_t ch;
    size_t i = 0;
    while (i < point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') line++;
        i++;
    }
    
    rope_iter_destroy(&iter);
    return line;
}

// Calculate current column
static size_t calculate_column(Buffer *buf, size_t point) {
    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, 0);
    
    size_t col = 0;
    uint32_t ch;
    size_t i = 0;
    
    while (i < point && rope_iter_next_char(&iter, &ch)) {
        if (ch == '\n') {
            col = 0;
        } else if (ch == '\t') {
            size_t tab_width = scm_get_size_t("tab-width", 8);
            col = ((col / tab_width) + 1) * tab_width;
        } else {
            col++;
        }
        i++;
    }
    
    rope_iter_destroy(&iter);
    return col;
}

// Process %-constructs in a string
static void process_percent_constructs(StrBuf *out, const char *str, Window *win) {
    Buffer *buf = win->buffer;
    size_t i = 0;
    
    while (str[i]) {
        if (str[i] == '%') {
            i++;
            if (!str[i]) break;
            
            // Handle field width (digits after %)
            int field_width = 0;
            while (isdigit(str[i])) {
                field_width = field_width * 10 + (str[i] - '0');
                i++;
            }
            
            char temp[256];
            switch (str[i]) {
                case 'b':  // Buffer name
                    strbuf_append(out, buf->name);
                    break;
                    
                case 'f':  // Visited file name (we don't have this yet)
                    strbuf_append(out, buf->name);
                    break;
                    
                case 'c':  // Column number (0-based)
                    snprintf(temp, sizeof(temp), "%zu", calculate_column(buf, win->point));
                    strbuf_append(out, temp);
                    break;
                    
                case 'C':  // Column number (1-based)
                    snprintf(temp, sizeof(temp), "%zu", calculate_column(buf, win->point) + 1);
                    strbuf_append(out, temp);
                    break;
                    
                case 'l':  // Line number
                    snprintf(temp, sizeof(temp), "%zu", count_lines_to_point(buf, win->point));
                    strbuf_append(out, temp);
                    break;
                    
                case 'p':  // Percent above top of window
                    // TODO: Calculate actual percentage
                    strbuf_append(out, "Top");
                    break;
                    
                case 'P':  // Percent above bottom of window
                    // TODO: Calculate actual percentage
                    strbuf_append(out, "Top");
                    break;
                    
                case 'i':  // Buffer size
                    snprintf(temp, sizeof(temp), "%zu", rope_char_length(buf->rope));
                    strbuf_append(out, temp);
                    break;
                    
                case '*':  // Modified flag: %, * or -
                    if (buf->read_only) {
                        strbuf_append_char(out, '%');
                    } else if (buf->modified) {  // You'll need to add a 'modified' field
                        strbuf_append_char(out, '*');
                    } else {
                        strbuf_append_char(out, '-');
                    }
                    break;
                    
                case '+':  // Modified/read-only: *, % or -
                    if (buf->modified && buf->read_only) {
                        strbuf_append_char(out, '*');
                    } else if (buf->read_only) {
                        strbuf_append_char(out, '%');
                    } else if (buf->modified) {
                        strbuf_append_char(out, '*');
                    } else {
                        strbuf_append_char(out, '-');
                    }
                    break;
                    
                case '&':  // Modified: * or -
                    strbuf_append_char(out, buf->modified ? '*' : '-');
                    break;
                    
                case '-':  // Fill with dashes
                    // Calculate remaining width and fill with dashes
                    // This is complex - for now just add some dashes
                    for (int j = 0; j < 10; j++) {
                        strbuf_append_char(out, '-');
                    }
                    break;
                    
                case '%':  // Literal %
                    strbuf_append_char(out, '%');
                    break;
                    
                default:
                    // Unknown construct, just output it literally
                    strbuf_append_char(out, '%');
                    strbuf_append_char(out, str[i]);
                    break;
            }
            i++;
        } else {
            strbuf_append_char(out, str[i]);
            i++;
        }
    }
}

static void process_mode_line_construct(StrBuf *out, SCM construct, Window *win);

// Process a mode-line list
static void process_mode_line_list(StrBuf *out, SCM list, Window *win) {
    if (!scm_is_pair(list)) return;
    
    SCM car = scm_car(list);
    
    // Check for special forms
    if (scm_is_symbol(car)) {
        char *sym_name = scm_to_locale_string(scm_symbol_to_string(car));
        
        if (strcmp(sym_name, ":eval") == 0) {
            // (:eval FORM) - evaluate FORM
            if (scm_is_pair(scm_cdr(list))) {
                SCM form = scm_cadr(list);
                SCM result = scm_eval(form, scm_current_module());
                process_mode_line_construct(out, result, win);
            }
            free(sym_name);
            return;
        } else if (strcmp(sym_name, ":propertize") == 0) {
            // (:propertize ELT PROPS...) - process ELT, ignore PROPS for now
            if (scm_is_pair(scm_cdr(list))) {
                SCM elt = scm_cadr(list);
                process_mode_line_construct(out, elt, win);
            }
            free(sym_name);
            return;
        }
        
        free(sym_name);
        
        // Symbol as car: if symbol's value is non-nil, process cadr, else process caddr
        SCM value = scm_variable_ref(scm_c_lookup(sym_name));
        if (scm_is_true(value)) {
            if (scm_is_pair(scm_cdr(list))) {
                process_mode_line_construct(out, scm_cadr(list), win);
            }
        } else {
            if (scm_is_pair(scm_cdr(list)) && scm_is_pair(scm_cddr(list))) {
                process_mode_line_construct(out, scm_caddr(list), win);
            }
        }
        return;
    }
    
    if (scm_is_integer(car)) {
        // (INTEGER ELT) - process ELT with padding/truncation
        int width = scm_to_int(car);
        if (scm_is_pair(scm_cdr(list))) {
            // TODO: Implement padding/truncation
            process_mode_line_construct(out, scm_cadr(list), win);
        }
        return;
    }
    
    // Otherwise, process each element of the list
    while (scm_is_pair(list)) {
        process_mode_line_construct(out, scm_car(list), win);
        list = scm_cdr(list);
    }
}

// Main recursive processor for mode-line constructs
static void process_mode_line_construct(StrBuf *out, SCM construct, Window *win) {
    if (scm_is_false(construct) || scm_is_null(construct)) {
        // nil means don't display
        return;
    }
    
    if (scm_is_string(construct)) {
        char *str = scm_to_locale_string(construct);
        process_percent_constructs(out, str, win);
        free(str);
        return;
    }
    
    if (scm_is_symbol(construct)) {
        // Look up symbol's value
        SCM value = scm_variable_ref(scm_c_lookup(scm_to_locale_string(scm_symbol_to_string(construct))));
        if (scm_is_string(value)) {
            char *str = scm_to_locale_string(value);
            strbuf_append(out, str);  // Don't process %-constructs for symbol values
            free(str);
        } else {
            process_mode_line_construct(out, value, win);
        }
        return;
    }
    
    if (scm_is_pair(construct)) {
        process_mode_line_list(out, construct, win);
        return;
    }
}

// Main entry point
char* format_mode_line(Window *win) {
    if (!win || !win->buffer) return strdup("");
    
    // Get buffer-local mode-line-format
    SCM mode_line_sym = scm_from_utf8_symbol("mode-line-format");
    SCM format = buffer_local_value(mode_line_sym, win->buffer);
    
    StrBuf buf;
    strbuf_init(&buf);
    
    process_mode_line_construct(&buf, format, win);
    
    return buf.data;
}

void free_mode_line_string(char *str) {
    free(str);
}

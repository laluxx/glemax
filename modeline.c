#include "modeline.h"
#include "faces.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    char  *data;
    size_t length;
    size_t capacity;
} StrBuf;

static inline void strbuf_init(StrBuf *b) {
    b->capacity = 256;
    b->length   = 0;
    b->data     = malloc(b->capacity);
    b->data[0]  = '\0';
}

static inline void strbuf_ensure(StrBuf *b, size_t extra) {
    if (b->length + extra + 1 <= b->capacity) return;
    while (b->length + extra + 1 > b->capacity) b->capacity *= 2;
    b->data = realloc(b->data, b->capacity);
}

static inline void strbuf_append(StrBuf *b, const char *s) {
    if (!s) return;
    size_t n = strlen(s);
    strbuf_ensure(b, n);
    memcpy(b->data + b->length, s, n + 1);
    b->length += n;
}

static inline void strbuf_append_char(StrBuf *b, char c) {
    strbuf_ensure(b, 1);
    b->data[b->length++] = c;
    b->data[b->length]   = '\0';
}

// (computed once per format_mode_line, lazily filled)
typedef struct {
    Window *win;
    Buffer *buf;
    size_t  tab_width;
    // Lazy fields — 0/SIZE_MAX means "not yet computed"
    size_t  line;   // 1-based; 0 = not computed
    size_t  col;    // 0-based; SIZE_MAX = not computed
} Ctx;

// Column: walk from line_start to point — only the current line, not whole buffer
static size_t compute_col(Ctx *ctx) {
    Buffer *buf  = ctx->buf;
    size_t  pos  = ctx->win->point;
    size_t  ls   = line_at_char(buf, pos);   // char pos of line start (buffer.c)
    size_t  col  = 0;
    size_t  tw   = ctx->tab_width;

    rope_iter_t iter;
    rope_iter_init(&iter, buf->rope, ls);
    uint32_t ch;
    size_t i = ls;
    while (i < pos && rope_iter_next_char(&iter, &ch)) {
        col = (ch == '\t') ? ((col / tw) + 1) * tw : col + 1;
        i++;
    }
    rope_iter_destroy(&iter);
    return col;
}

// %-construct expander
static void ml_construct(StrBuf *out, SCM construct, Ctx *ctx);

static void ml_percent(StrBuf *out, const char *str, Ctx *ctx) {
    Buffer *buf = ctx->buf;
    char tmp[32];

    for (size_t i = 0; str[i]; ) {
        if (str[i] != '%') { strbuf_append_char(out, str[i++]); continue; }
        i++;
        if (!str[i]) break;

        // Optional numeric field width (consumed but only used for '%-')
        int fw = 0;
        while (isdigit((unsigned char)str[i])) fw = fw * 10 + (str[i++] - '0');

        switch (str[i]) {
            case 'b': case 'f':
                strbuf_append(out, buf->name);
                break;
            case 'l':
                if (!ctx->line) ctx->line = (size_t)line_number_at_pos(buf, ctx->win->point);
                snprintf(tmp, sizeof tmp, "%zu", ctx->line);
                strbuf_append(out, tmp);
                break;
            case 'c':
                if (ctx->col == (size_t)-1) ctx->col = compute_col(ctx);
                snprintf(tmp, sizeof tmp, "%zu", ctx->col);
                strbuf_append(out, tmp);
                break;
            case 'C':
                if (ctx->col == (size_t)-1) ctx->col = compute_col(ctx);
                snprintf(tmp, sizeof tmp, "%zu", ctx->col + 1);
                strbuf_append(out, tmp);
                break;
            case 'i':
                snprintf(tmp, sizeof tmp, "%zu", rope_char_length(buf->rope));
                strbuf_append(out, tmp);
                break;
            case '*':
                strbuf_append_char(out, buf->read_only ? '%' : buf->modified ? '*' : '-');
                break;
            case '+':
                strbuf_append_char(out, (buf->modified && buf->read_only) ? '*'
                                      : buf->read_only                    ? '%'
                                      : buf->modified                     ? '*' : '-');
                break;
            case '&':
                strbuf_append_char(out, buf->modified ? '*' : '-');
                break;
            case 'p': case 'P':
                strbuf_append(out, "Top"); // TODO: real scroll %
                break;
            case '-': {
                int n = fw > 0 ? fw : 10;
                strbuf_ensure(out, n);
                memset(out->data + out->length, '-', n);
                out->length += n;
                out->data[out->length] = '\0';
                break;
            }
            case '%':
                strbuf_append_char(out, '%');
                break;
            default:
                strbuf_append_char(out, '%');
                strbuf_append_char(out, str[i]);
                break;
        }
        i++;
    }
}

static void ml_list(StrBuf *out, SCM list, Ctx *ctx) {
    if (!scm_is_pair(list)) return;
    SCM car = scm_car(list);

    if (scm_is_symbol(car)) {
        // Intern once — zero-alloc comparisons on every call after first
        static SCM s_eval = 0, s_prop = 0;
        if (!s_eval) {
            s_eval = scm_from_utf8_symbol(":eval");
            s_prop = scm_from_utf8_symbol(":propertize");
            scm_gc_protect_object(s_eval);
            scm_gc_protect_object(s_prop);
        }

        if (scm_is_eq(car, s_eval)) {
            if (scm_is_pair(scm_cdr(list)))
                ml_construct(out, scm_eval(scm_cadr(list), scm_current_module()), ctx);
            return;
        }
        if (scm_is_eq(car, s_prop)) {
            if (scm_is_pair(scm_cdr(list)))
                ml_construct(out, scm_cadr(list), ctx);
            return;
        }

        // (SYMBOL then-form else-form) conditional
        SCM var = scm_module_variable(scm_current_module(), car);
        bool truthy = scm_is_true(var)
                   && scm_is_true(scm_variable_bound_p(var))
                   && scm_is_true(scm_variable_ref(var));
        if (truthy) {
            if (scm_is_pair(scm_cdr(list))) ml_construct(out, scm_cadr(list), ctx);
        } else {
            if (scm_is_pair(scm_cdr(list)) && scm_is_pair(scm_cddr(list)))
                ml_construct(out, scm_caddr(list), ctx);
        }
        return;
    }

    if (scm_is_integer(car)) {
        // (N form) — width hint, just process form
        if (scm_is_pair(scm_cdr(list))) ml_construct(out, scm_cadr(list), ctx);
        return;
    }

    // Plain list — recurse each element
    while (scm_is_pair(list)) {
        ml_construct(out, scm_car(list), ctx);
        list = scm_cdr(list);
    }
}

static void ml_construct(StrBuf *out, SCM c, Ctx *ctx) {
    if (scm_is_false(c) || scm_is_null(c)) return;

    if (scm_is_string(c)) {
        char *s = scm_to_locale_string(c);
        ml_percent(out, s, ctx);
        free(s);
        return;
    }

    if (scm_is_symbol(c)) {
        SCM var = scm_module_variable(scm_current_module(), c);
        if (scm_is_false(var) || scm_is_false(scm_variable_bound_p(var))) return;
        SCM val = scm_variable_ref(var);
        if (scm_is_string(val)) {
            char *s = scm_to_locale_string(val);
            ml_percent(out, s, ctx);
            free(s);
        } else {
            ml_construct(out, val, ctx);
        }
        return;
    }

    if (scm_is_pair(c)) { ml_list(out, c, ctx); return; }
}

/// API

char *format_mode_line(Window *win) {
    if (!win || !win->buffer) return strdup("");

    static SCM s_fmt = 0, s_tab_width = 0;
    if (!s_fmt) {
        s_fmt      = scm_from_utf8_symbol("mode-line-format");
        s_tab_width = scm_from_utf8_symbol("tab-width");
        scm_gc_protect_object(s_fmt);
        scm_gc_protect_object(s_tab_width);
    }

    SCM tw = buffer_local_value(s_tab_width, win->buffer);

    Ctx ctx = {
        .win       = win,
        .buf       = win->buffer,
        .tab_width = scm_is_integer(tw) ? scm_to_size_t(tw) : 8,
        .line      = 0,
        .col       = (size_t)-1,
    };

    StrBuf buf;
    strbuf_init(&buf);
    ml_construct(&buf, buffer_local_value(s_fmt, win->buffer), &ctx);
    return buf.data;
}

void free_mode_line_string(char *str) {
    free(str);
}

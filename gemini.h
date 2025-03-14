#ifndef GEMINI_H
#define GEMINI_H

#include <stddef.h>
#include <openssl/ssl.h>
#include <stdint.h>
#include "buffer.h"

// 20 Success
// 30 Temporary redirect
// 31 Permanent redirect
// 51 Not found

typedef struct {
    char host[256];
    char port[16];
    char path[1024];
} GeminiUrl;

typedef struct {
    char* content;
    size_t size;
} GeminiOutput;


void gemini_free_content(GeminiOutput* content);
void initOpenssl(void);
int gemini_parse_url(const char* url, GeminiUrl* result);
GeminiOutput gemini_fetch(const char *url, Buffer *buffer);
bool gemini_redirect(Buffer *buffer);
bool gemini_redirect_other_window(Buffer *buffer);

#endif

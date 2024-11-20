#ifndef ISEARCH_H
#define ISEARCH_H

#include "buffer.h"
#include <stdbool.h>

typedef struct {
    Buffer *searchBuffer;
    size_t lastMatchIndex;
    size_t startIndex;
    size_t count;  // How many times C-s was pressed while searching
    bool searching;
    char *lastSearch;
    bool wrap;
} ISearch;

extern ISearch isearch;

void isearch_backward(Buffer *buffer, Buffer *minibuffer, bool updateStartIndex);
void isearch_forward(Buffer *buffer, BufferManager *bm, Buffer *minibuffer, bool updateStartIndex);
void jumpLastOccurrence(Buffer *buffer, const char *word);

#endif

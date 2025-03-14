#include "isearch.h"
#include "edit.h"
#include <ctype.h>

ISearch isearch = {0};

// TODO It should show in Red the ending substring that doesn't match
// Like 'isearch' does in emacs

// TODO C-r should search on the history in shell-command
// TODO C-r shoudl wrap

void isearch_forward(Buffer *buffer, BufferManager *bm, Buffer *minibuffer, bool updateStartIndex) {
    const char *start = buffer->content + isearch.startIndex;
    const char *found = strstr(start, minibuffer->content);

    if (found) {
        size_t matchIndex = found - buffer->content;
        buffer->point = matchIndex + strlen(minibuffer->content);
        isearch.lastMatchIndex = matchIndex + 1;
        if (updateStartIndex) {
            isearch.startIndex = buffer->point;
        }
        isearch.wrap = false;
    } else {
        if (!isearch.wrap) {
            message("Reached end of buffer");
            isearch.wrap = true;
        } else {
            if (updateStartIndex) {
                isearch.startIndex = 0;
                isearch.wrap = false;
                isearch_forward(buffer, bm, minibuffer, false);
            }
        }
    }
}

void isearch_backward(Buffer *buffer, Buffer *minibuffer, bool updateStartIndex) {
    if (isearch.lastMatchIndex == 0 || isearch.lastMatchIndex > buffer->size) {
        // No previous match or out of bounds, start from end of buffer
        isearch.lastMatchIndex = buffer->size;
    }

    char *searchEnd = buffer->content + isearch.lastMatchIndex - 1;
    char *found = NULL;

    // Ensure we do not start a search from a negative index
    if (searchEnd < buffer->content) {
        searchEnd = buffer->content;
    }

    for (char *p = searchEnd; p >= buffer->content; --p) {
        if (strncmp(p, minibuffer->content, strlen(minibuffer->content)) == 0) {
            found = p;
            break;
        }
    }

    if (found) {
        size_t matchIndex = found - buffer->content;
        buffer->point = matchIndex;
        if (updateStartIndex) {
            isearch.lastMatchIndex = matchIndex;  // Set for next potential search update
        }
        isearch.wrap = false;
    } else {
        if (!isearch.wrap) {
            printf("Reached beginning of buffer. Press Ctrl+R again to wrap search.\n");
            isearch.wrap = true;
        } else {
            if (updateStartIndex) {
                isearch.lastMatchIndex = buffer->size; // Reset lastMatchIndex for wrapping
                isearch.wrap = false;
                isearch_backward(buffer, minibuffer, false); // Attempt to wrap around
            }
        }
    }
}

void jumpLastOccurrence(Buffer *buffer, const char *word) {
    int wordLength = strlen(word);
    int startIndex = buffer->point - wordLength - 1;  // start from the left of the current word

    // Ensure the starting index does not go below 0
    if (startIndex < 0) startIndex = 0;

    for (int i = startIndex; i >= 0; i--) {
        if (strncmp(buffer->content + i, word, wordLength) == 0) {
            buffer->point = i + wordLength;
            return;
        }
    }
}

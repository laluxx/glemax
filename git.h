#ifndef GIT_H
#define GIT_H

typedef enum {
    DIFF_NONE,
    DIFF_ADDED,
    DIFF_CHANGED,
    DIFF_DELETED
} DiffType;

typedef struct {
    int line;
    DiffType type;
} DiffInfo;

typedef struct {
    DiffInfo *array;
    int count;
    int capacity;
} Diffs;


Diffs getDiffInfo(const char *filePath);
char* getGitBranch(const char* relativePath);

#endif


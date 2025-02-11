#ifndef BUFFER_H
#define BUFFER_H

#include <tree_sitter/api.h>
#include <stddef.h>
#include <stdbool.h>
#include "font.h"
#include "git.h"

typedef struct {
    size_t start;
    size_t end;
    /* Color color; */
    Color *color;
} Syntax;

// TODO keep syntaxes in a tree
typedef struct {
    Syntax *items;
    size_t used;
    size_t size;
} SyntaxArray;

typedef struct {
    int index;
    int fontSizes[36]; // NOTE from -8 to +27
} Scale;

typedef struct {
    size_t start;   // Start position of the region
    size_t end;     // End position of the region
    size_t mark;    // NOTE Could become a dynamic array of marks
    bool active;    // Whether the region is currently active
    bool marked;    // HACK Whether the region was activated by pressing C-SPC
} Region;

typedef struct {
    size_t start;
    size_t end;
    int level;
} Scope;

typedef struct {
    Scope *items;
    size_t count;
    size_t capacity;
} Scopes;

typedef struct {
    char *content;   // Text content
    size_t size;     // Current size of content
    size_t capacity; // Allocated capacity
    size_t point;    // Cursor position
    char *name;      // Buffer name
    bool readOnly;   // Read-only flag
    char *path;      // Normalized as "~/"
    Region region;   // NOTE Each buffer has its region
    Scale scale;     // Scale struct for managing font sizes
    Font *font;      // NOTE Each buffer has its fonts
    TSTree *tree;    // Tree sitter tree
    SyntaxArray syntaxArray; // Array of syntax highlighting ranges
    char *major_mode;
    char* fontPath;
    Scopes scopes;
    Diffs diffs;
    char *originalContent; // Store the original file content
    size_t originalSize;   // Size of the original content
} Buffer;

typedef struct {
    Buffer **buffers;    // Array of buffer pointers
    int count;           // Number of buffers
    int capacity;        // Capacity of the buffer list
    int activeIndex;     // Index of the active buffer
    char *activeName;    // Name of the active buffer
    Buffer *lastBuffer;  // For going back from the minibuffer
} BufferManager;

typedef enum {
    VERTICAL,
    HORIZONTAL
} SplitOrientation;

typedef struct {
    char *name;     // Name or identifier of the segment
    char *content;  // Content to be displayed in this segment
    // TODO Width
} Segment;

typedef struct {
    Segment *segment;
    size_t count;
} Segments;

// TODO track the width too (when splitting or resiging the GLFW window)
typedef struct {
    float height;
    Segments segments;
} Modeline;

typedef struct Window {
    float x;              // X position
    float y;              // Y position
    float width;          // Width of the window
    float height;         // Height of the window
    Modeline modeline;    // Modeline for the window
    Vec2f scroll;
    Buffer *buffer;      // Buffer displayed in this window
    /* struct Window *parent;   // NOTE This could be implemented later on.. */
    struct Window *prev; // Previous window in the list
    struct Window *next; // Next window in the list
    bool isActive;       // Is this the active window?
    bool hasFringe;      // Weather the window has a left fringe
    SplitOrientation splitOrientation;
    float targetScrollY;
    bool isScrolling;
} Window;

typedef struct {
    Window *head;         // Head of the window list
    Window *activeWindow; // Currently active window
    int count;            // Number of windows
} WindowManager;


extern WindowManager wm;
extern BufferManager bm;

extern double mouseX;
extern double mouseY;


void initBuffer(Buffer *buffer, const char *name, const char *path);

void newBuffer(BufferManager *manager, WindowManager *wm,
               const char *name, const char *path, char *fontPath,
               int sw, int sh);


void freeBuffer(Buffer *buffer);
void initBufferManager(BufferManager *manager);
void freeBufferManager(BufferManager *manager);

void switchToBuffer(BufferManager *manager, const char *name);
Buffer *getActiveBuffer(BufferManager *manager);
Buffer *getBuffer(BufferManager *manager, const char *name);
bool isCurrentBuffer(BufferManager *manager, const char *bufferName);
void nextBuffer(BufferManager *manager);
void previousBuffer(BufferManager *manager);

void activateRegion(Buffer *buffer);
void updateRegion(Buffer *buffer, size_t new_point);
void deactivateRegion(Buffer *buffer);

void setBufferContent(Buffer *buffer, const char *newContent);
void message(BufferManager *bm, const char *message);
void cleanBuffer(BufferManager *bm, char *name);


Buffer *getBufferUnderCursor(WindowManager *wm);

void setMajorMode(Buffer *buffer);


// DIFF-HL
void updateDiffs(Buffer *buffer);
void initDiffs(BufferManager *bm);



// MODELINE
void addSegment(Segments *segments, const char *name, const char *content);
void initSegments(Segments *segments);
/* void updateSegments(Modeline *modeline); */
void updateSegments(Modeline *modeline, Buffer *buffer);

// UTILITY FUNCTIONS
int getLineNumber(Buffer *buffer);




#endif

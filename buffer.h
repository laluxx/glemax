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
    /* Color bg/fg; */
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
    bool marked;    // HACK Whether  the region was activated by pressing C-SPC
} Region;

// NOTE we could attach more informations to Scopes like
// what is inside that scope is it a function definition ?
// But it would be redundant since we have TreeSitter.
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
    char *name;
    int line_number;
    size_t start_byte;
    size_t end_byte;
} Function;

// Keep track of functions in each buffer
typedef struct {
    Function *items;
    size_t used;
    size_t size;
} Functions;


// TODO Mode(s)
/* typedef struct { */
/*     char *name; */
/*     bool major; */
/*     bool prog; */
/*     Mode *parent; */
/* } Mode; */

/* typedef struct { */
/*     Mode *array; */
/*     size_t count; */
/*     size_t capacity; */
/* } Modes; */


typedef struct {
    struct Window **windows;   // Array of pointers to windows displaying this buffer
    int windowCount;           // Current number of windows
    int windowCapacity;        // Allocated capacity for the array
} BufferWindows;




/* typedef struct { */
    
/* } Undo; */

/* typedef struct { */
    
/* } Undos; */

typedef struct {
    char *content;   // Text content
    size_t size;     // Current size of content
    size_t capacity; // Allocated capacity
    size_t point;    // Cursor position
    char *name;      // Buffer name
    bool readOnly;   // Read-only flag TODO
    char *path;      // Normalized as "~/"
    Region region;   // NOTE Each buffer has its region
    Scale scale;     // Scale struct for managing font sizes
    TSTree *tree;    // Tree sitter tree
    SyntaxArray syntaxArray; // Array of syntax highlighting ranges
    char *major_mode;

    Font *font; // NOTE Each buffer has its fonts
    char *fontPath;

    Scopes scopes;
    Diffs diffs;
    char *originalContent;     // Store the original file content
    size_t originalSize;       // Size of the original content
    int goal_column;           // -1 is unset
    Functions functions;       // TODO
    int animatedLineNumber;    // Line number being animated
    double animationStartTime; // Start time of the animation
    char *url;                 // Gemini url TODO History
    BufferWindows displayWindows; // Windows where this buffer is displayed
} Buffer;

typedef struct {
    Buffer **buffers;   // Array of buffer pointers
    int    count;       // Number of buffers
    int    capacity;    // Capacity of the buffer list
    int    activeIndex; // Index of the active buffer
    char   *activeName; // Name of the active buffer
    /* Buffer *lastBuffer; // For going back from the minibuffer */
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
    struct Window *window; // Pointer to the owning window TODO
} Modeline;



#include "uthash.h"

typedef struct {
    bool  noOtherWindow;
    bool  scrollBar;
    bool  truncateLines;
    bool  minimap;              // TODO
    float minimap_width;        // Current width of the minimap
    float minimap_target_width; // Target width for easing
    bool  minimap_lerp_active;  // Whether easing is active
} WindowParameters;

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
    float targetScrollX;
    bool isScrolling;
    bool isMouseWheelScrolling;
    size_t leftPadding; // TODO
    WindowParameters parameters; // Window parameters
} Window;

typedef struct WindowMap {
    char *key;         // Buffer name
    Window *window;
    UT_hash_handle hh;
} WindowMap;

typedef struct {
    Window *head;
    Window *activeWindow;       // Currently active window
    int count;                  // Number of active windows
    WindowMap *graveyard;       // Pool of inactive windows
    int inactive_count;         // Number of inactive windows
    char *lastKilledBufferName; // Buffers are keys to windows
} WindowManager;

/* typedef struct { */
/*     Window *head;         // Head of the rendered window list */
/*     Window *inactive;     // Pool of unused windows */
/*     Window *activeWindow;  */
/*     int count;            // Number of windows */
/*     int inactive_count;   // Number of inactive windows */
/*     WindowMap *name_map; */
/* } WindowManager; */


extern WindowManager wm;
extern BufferManager bm;

extern double mouseX;
extern double mouseY;


void initBuffer(Buffer *buffer, const char *name, const char *path);

/* void newBuffer(BufferManager *manager, WindowManager *wm, */
/*                const char *name, const char *path, char *fontPath, */
/*                int sw, int sh); */

void newBuffer(BufferManager *manager, WindowManager *wm,
               const char *name, const char *path, char *fontPath);



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

void setBufferContent(Buffer *buffer, const char *newContent, bool pointAtSize);
void appendToBuffer(Buffer *buffer, const char *content);

void message(const char *format, ...);
void cleanBuffer(BufferManager *bm, char *name);

Buffer *getBufferUnderCursor(WindowManager *wm);

void inferMajorMode(Buffer *buffer);
void setMajorMode(Buffer *buffer, char *mode);
bool major_mode_is(Buffer *buffer, char *mode);

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
int lineNumberAtPoint(Buffer *buffer, size_t point);
Color foregroundColorAtPoint(Buffer *buffer, size_t point);
char* getCurrentLine(Buffer *buffer);


// Display Windows
void addDisplayWindowToBuffer(Buffer *buffer, Window *window);
void removeDisplayWindowFromBuffer(Buffer *buffer, Window *window);


void abort_recursive_edit();

Buffer *getPreviousBuffer(BufferManager *bm);
Buffer *getNextBuffer(BufferManager *bm);

char *getPreviousBufferName(BufferManager *bm);
char *getNextBufferName(BufferManager *bm);

char *getPreviousBufferPath(BufferManager *bm);
char *getNextBufferPath(BufferManager *bm);

bool major_mode_supported(Buffer *buffer);

#endif

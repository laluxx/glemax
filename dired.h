#ifndef DIRED_H
#define DIRED_H

#include <stdbool.h>
#include <sys/stat.h>
#include "buffer.h"

typedef enum {
    DIRED_FILE,
    DIRED_DIRECTORY,
    DIRED_SYMLINK,
    DIRED_SPECIAL // For devices, pipes, sockets, etc.
} DiredEntryType;

typedef struct {
    char *name;           // File/directory name
    char *symlink_target; // For symlinks only
    char *permissions;    // Human-readable permissions (like "drwxr-xr-x")
    char *owner;          // Owner name
    char *group;          // Group name
    char *size;           // Human-readable size
    char *time;           // Modification time
    DiredEntryType type;
    bool marked;          // Whether the entry is marked for operation
    bool hidden;          // Whether the entry is hidden (starts with '.')
    ino_t inode;          // Inode number
    nlink_t nlink;        // Number of hard links
    off_t raw_size;       // Raw size in bytes
    time_t mtime;         // Raw modification time
} DiredEntry;

typedef struct {
    char *directory;      // Current directory being displayed
    DiredEntry *entries;  // Array of directory entries
    size_t count;         // Number of entries
    size_t capacity;      // Allocated capacity
    bool show_hidden;     // Whether to show hidden files
    bool human_readable;  // Whether to show human-readable sizes
    int sort_field;       // Which field to sort by
    bool sort_reverse;    // Whether to sort in reverse order
    Buffer *buffer;       // Associated buffer

    bool details_hidden;
    struct {
        size_t permissions;
        size_t links;
        size_t owner;
        size_t group;
        size_t size;
        size_t date;
        size_t name;
    } columns;

} Dired;

typedef struct {
    Dired **entries;      // Array of Dired pointers
    size_t count;         // Number of entries
    size_t capacity;      // Allocated capacity
} Direds;

extern Direds direds; // NOTE Global


// Direds management functions
void direds_init(void);
void direds_free(void);
Dired* direds_get(const char *path);
Dired* direds_get_or_create(const char *path, Buffer *buffer);
void direds_remove(const char *path);
void direds_update_buffer(Buffer *buffer);

// Initialization and cleanup
Dired *dired_new(const char *path, Buffer *buffer);
void dired_free(Dired *dired);

// Directory operations
bool dired_read_directory(Dired *dired, const char *path);
bool dired_revert(Dired *dired);

// Entry manipulation
void dired_mark(Dired *dired, size_t index);
void dired_unmark(Dired *dired, size_t index);
void dired_toggle_mark(Dired *dired, size_t index);
void dired_mark_all(Dired *dired);
void dired_unmark_all(Dired *dired);

// File operations
bool dired_delete_marked(Dired *dired);
bool dired_rename(Dired *dired, size_t index, const char *new_name);
bool dired_create_directory(Dired *dired, const char *name);

// Sorting
void dired_sort(Dired *dired, int field, bool reverse);

// Formatting
char *dired_format_entry(const DiredEntry *entry, bool human_readable,
                         bool details_hidden);
char* dired_format_header(const Dired *dired);

// Buffer integration
void dired_insert_into_buffer(Buffer *buffer, const Dired *dired);
void dired_update_buffer(Buffer *buffer, Dired *dired);
char* dired_format_all_entries(const Dired *dired);

//-- Next day

void dired_update_column_widths(Dired *dired);
void dired_toggle_details(Dired *dired);

void dired_update_column_widths(Dired *dired);
bool dired_jump(WindowManager *wm);
bool dired_poke(Dired *dired, Buffer *buffer, char c); // TODO

Dired *dired_for_buffer(Buffer *buffer);
void parse_and_push_dired_syntax(Buffer *buffer);
void position_point_at_first_entry(Buffer *buffer);


char* dired_get_current_entry_path(Dired *dired, Buffer *buffer);
void dired_find_file(BufferManager *bm, WindowManager *wm, Dired *dired);

char* get_parent_directory(const char *path);
void dired_up_directory(BufferManager *bm, WindowManager *wm, Dired *dired);
void dired_find_file_other_window(BufferManager *bm, WindowManager *wm, Dired *dired);
void dired_toggle(WindowManager *wm);

#endif // DIRED_H

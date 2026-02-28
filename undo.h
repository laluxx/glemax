#pragma once

#include <libguile.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct Buffer Buffer;

extern bool last_command_was_undo;

// Undo record types
typedef enum {
    UNDO_INSERT,      // (BEG . END)                           - insertion from BEG to END
    UNDO_DELETE,      // (TEXT . POSITION)                     - deletion of TEXT at abs(POSITION)
    UNDO_BOUNDARY,    // nil                                   - separates command sequences
    UNDO_POSITION,    // integer                               - point was at POSITION
    UNDO_MODTIME,     // (t . TIMESTAMP)                       - buffer unmodified marker
    UNDO_PROPERTY,    // (nil PROPERTY VALUE BEG . END)        - text property change
    UNDO_APPLY,       // (apply FUN-NAME . ARGS)               - custom undo function
    UNDO_APPLY_SEL,   // (apply DELTA BEG END FUN-NAME . ARGS) - selective undo
    UNDO_MARKER       // (MARKER . DISTANCE)                   - marker adjustment
} UndoType;

// Individual undo record
typedef struct UndoRecord {
    UndoType type;
    union {
        // For UNDO_INSERT: (BEG . END)
        struct {
            size_t beg;
            size_t end;
        } insert;

        // For UNDO_DELETE: (TEXT . POSITION)
        // POSITION > 0 means point was at front
        // POSITION < 0 means point was at end
        struct {
            char *text;
            size_t len;
            ssize_t position;  // signed!
        } del;  // CHANGED FROM 'delete' to 'del'

        // For UNDO_POSITION: just the position
        struct {
            size_t pos;  // CHANGED FROM 'position' to 'pos' (wrapped in struct)
        } position_marker;

        // For UNDO_MODTIME: (t . TIMESTAMP)
        // TIMESTAMP = 0 means unknown time
        // TIMESTAMP = -1 means file did not exist
        struct {
            time_t timestamp;
        } modtime;

        // For UNDO_PROPERTY: (nil PROPERTY VALUE BEG . END)
        struct {
            SCM property;
            SCM value;
            size_t beg;
            size_t end;
        } property;

        // For UNDO_APPLY: (apply FUN-NAME . ARGS)
        struct {
            SCM fun_name;
            SCM args;
        } apply;

        // For UNDO_APPLY_SEL: (apply DELTA BEG END FUN-NAME . ARGS)
        struct {
            ssize_t delta;
            size_t beg;
            size_t end;
            SCM fun_name;
            SCM args;
        } apply_sel;

        // For UNDO_MARKER: (MARKER . DISTANCE)
        struct {
            SCM marker;
            ssize_t distance;
        } marker;
    };

    struct UndoRecord *next;
} UndoRecord;

// Undo state for buffer
typedef struct {
    UndoRecord *list;              // Current undo list (most recent first)
    UndoRecord *list_tail;         // For efficient appending
    size_t list_length;            // Number of records
    size_t list_byte_size;         // Total bytes in deleted text
    bool in_progress;              // True when executing undo
    bool enabled;                  // Can be disabled (buffer-undo-list = t)
    bool boundary_needed;          // True if next edit needs boundary
    size_t last_boundary_position; // Point at last boundary
    time_t last_modtime_recorded;  // Last modification time recorded
} UndoState;

// Core undo functions
void undo_init(Buffer *buf);
void undo_cleanup(UndoState *state);

// Recording undo information (exactly as Emacs does)
void undo_record_insert(Buffer *buf, size_t beg, size_t end);
void undo_record_delete(Buffer *buf, const char *text, size_t len, ssize_t position);
void undo_record_position(Buffer *buf, size_t position);
void undo_record_modtime(Buffer *buf, time_t timestamp);
void undo_record_property(Buffer *buf, SCM prop, SCM value, size_t beg, size_t end);
void undo_record_apply(Buffer *buf, SCM fun_name, SCM args);
void undo_record_apply_selective(Buffer *buf, ssize_t delta, size_t beg, size_t end,
                                 SCM fun_name, SCM args);
void undo_record_marker(Buffer *buf, SCM marker, ssize_t distance);

// Undo boundaries
void undo_boundary(Buffer *buf);
void undo_boundary_if_needed(Buffer *buf);

// Executing undo - the primitive-undo function
SCM primitive_undo(int count, SCM list, Buffer *buf);

// High-level undo commands
void undo_command(int arg);
void undo_only_command(int arg);

// Undo list management
void undo_truncate(Buffer *buf);
void undo_amalgamate(Buffer *buf);
SCM undo_list_to_scm(Buffer *buf);
void undo_list_from_scm(Buffer *buf, SCM list);

// Undo state queries
bool undo_list_empty(Buffer *buf);
bool undo_in_region(Buffer *buf);
SCM buffer_undo_list_scm(Buffer *buf);

// Disable/enable undo
void undo_disable(Buffer *buf);
void undo_enable(Buffer *buf);
bool undo_enabled(Buffer *buf);

// Helpers
size_t get_undo_limit(void);
size_t get_undo_strong_limit(void);
size_t get_undo_outer_limit(void);
bool get_undo_ask_before_discard(void);

// Guile bindings initialization
void init_undo_bindings(void);

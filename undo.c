#define _GNU_SOURCE
#include "undo.h"
#include "buffer.h"
#include "rope.h"
#include "lisp.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <sys/stat.h>

bool last_command_was_undo = false;

#define DEFAULT_UNDO_LIMIT 80000
#define DEFAULT_UNDO_STRONG_LIMIT 120000
#define DEFAULT_UNDO_OUTER_LIMIT 12000000

// Get limits from Scheme variables
size_t get_undo_limit(void) {
    return scm_get_size_t("undo-limit", DEFAULT_UNDO_LIMIT);
}

size_t get_undo_strong_limit(void) {
    return scm_get_size_t("undo-strong-limit", DEFAULT_UNDO_STRONG_LIMIT);
}

size_t get_undo_outer_limit(void) {
    return scm_get_size_t("undo-outer-limit", DEFAULT_UNDO_OUTER_LIMIT);
}

bool get_undo_ask_before_discard(void) {
    return scm_get_bool("undo-ask-before-discard", false);
}

/// INTERNAL HELPERS

static UndoRecord *undo_record_alloc(UndoType type) {
    UndoRecord *rec = calloc(1, sizeof(UndoRecord));
    rec->type = type;
    rec->next = NULL;
    return rec;
}

static void undo_record_free(UndoRecord *rec) {
    if (!rec) return;

    switch (rec->type) {
        case UNDO_DELETE:
            free(rec->del.text);
            break;
        case UNDO_PROPERTY:
        case UNDO_APPLY:
        case UNDO_APPLY_SEL:
        case UNDO_MARKER:
            // SCM values are garbage collected
            break;
        default:
            break;
    }

    free(rec);
}

static void undo_list_free(UndoRecord *list) {
    while (list) {
        UndoRecord *next = list->next;
        undo_record_free(list);
        list = next;
    }
}

// Prepend a record to the HEAD of the list (newest-first).
static void undo_prepend(Buffer *buf, UndoRecord *rec) {
    if (!buf->undo_state) return;

    rec->next = buf->undo_state->list;
    buf->undo_state->list = rec;

    if (!buf->undo_state->list_tail) {
        buf->undo_state->list_tail = rec;
    }

    buf->undo_state->list_length++;

    if (rec->type == UNDO_DELETE) {
        buf->undo_state->list_byte_size += rec->del.len;
    }
}

// Append a record to the TAIL of the list.
static void undo_append(Buffer *buf, UndoRecord *rec) {
    if (!buf->undo_state) return;

    if (!buf->undo_state->list) {
        buf->undo_state->list = rec;
        buf->undo_state->list_tail = rec;
    } else {
        buf->undo_state->list_tail->next = rec;
        buf->undo_state->list_tail = rec;
    }

    buf->undo_state->list_length++;

    if (rec->type == UNDO_DELETE) {
        buf->undo_state->list_byte_size += rec->del.len;
    }
}

/// INITIALIZATION

void undo_init(Buffer *buf) {
    if (!buf) return;

    buf->undo_state = calloc(1, sizeof(UndoState));
    buf->undo_state->list = NULL;
    buf->undo_state->list_tail = NULL;
    buf->undo_state->list_length = 0;
    buf->undo_state->list_byte_size = 0;
    buf->undo_state->in_progress = false;
    buf->undo_state->enabled = true;
    buf->undo_state->boundary_needed = false;
    buf->undo_state->last_boundary_position = 0;
    buf->undo_state->last_modtime_recorded = 0;
}

void undo_cleanup(UndoState *state) {
    if (!state) return;

    undo_list_free(state->list);
    free(state);
}

/// RECORDING UNDO ENTRIES

// All record_* functions PREPEND to the list so that the list is
// always newest-first.

// (BEG . END)
void undo_record_insert(Buffer *buf, size_t beg, size_t end) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    undo_boundary_if_needed(buf);

    UndoRecord *rec = undo_record_alloc(UNDO_INSERT);
    rec->insert.beg = beg;
    rec->insert.end = end;

    undo_prepend(buf, rec);
    undo_truncate(buf);
}

// (TEXT . POSITION)
void undo_record_delete(Buffer *buf, const char *text, size_t len, ssize_t position) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    undo_boundary_if_needed(buf);

    UndoRecord *rec = undo_record_alloc(UNDO_DELETE);
    rec->del.text = malloc(len + 1);
    memcpy(rec->del.text, text, len);
    rec->del.text[len] = '\0';
    rec->del.len = len;
    rec->del.position = position;

    undo_prepend(buf, rec);
    undo_truncate(buf);
}

// POSITION (integer)
void undo_record_position(Buffer *buf, size_t position) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    UndoRecord *rec = undo_record_alloc(UNDO_POSITION);
    rec->position_marker.pos = position;

    undo_prepend(buf, rec);
}

// (t . TIMESTAMP)
void undo_record_modtime(Buffer *buf, time_t timestamp) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    UndoRecord *rec = undo_record_alloc(UNDO_MODTIME);
    rec->modtime.timestamp = timestamp;
    buf->undo_state->last_modtime_recorded = timestamp;

    undo_prepend(buf, rec);
}

// (nil PROPERTY VALUE BEG . END)
void undo_record_property(Buffer *buf, SCM prop, SCM value, size_t beg, size_t end) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    UndoRecord *rec = undo_record_alloc(UNDO_PROPERTY);
    rec->property.property = prop;
    rec->property.value = value;
    rec->property.beg = beg;
    rec->property.end = end;

    scm_gc_protect_object(prop);
    scm_gc_protect_object(value);

    undo_prepend(buf, rec);
}

// (apply FUN-NAME . ARGS)
void undo_record_apply(Buffer *buf, SCM fun_name, SCM args) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    UndoRecord *rec = undo_record_alloc(UNDO_APPLY);
    rec->apply.fun_name = fun_name;
    rec->apply.args = args;

    scm_gc_protect_object(fun_name);
    scm_gc_protect_object(args);

    undo_prepend(buf, rec);
}

// (apply DELTA BEG END FUN-NAME . ARGS)
void undo_record_apply_selective(Buffer *buf, ssize_t delta, size_t beg, size_t end,
                                  SCM fun_name, SCM args) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    UndoRecord *rec = undo_record_alloc(UNDO_APPLY_SEL);
    rec->apply_sel.delta = delta;
    rec->apply_sel.beg = beg;
    rec->apply_sel.end = end;
    rec->apply_sel.fun_name = fun_name;
    rec->apply_sel.args = args;

    scm_gc_protect_object(fun_name);
    scm_gc_protect_object(args);

    undo_prepend(buf, rec);
}

// (MARKER . DISTANCE)
void undo_record_marker(Buffer *buf, SCM marker, ssize_t distance) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    UndoRecord *rec = undo_record_alloc(UNDO_MARKER);
    rec->marker.marker = marker;
    rec->marker.distance = distance;

    scm_gc_protect_object(marker);

    undo_prepend(buf, rec);
}

/// UNDO BOUNDARIES

void undo_boundary(Buffer *buf) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->enabled) return;
    if (buf->undo_state->in_progress) return;

    // Don't add duplicate boundaries at the head
    if (buf->undo_state->list && buf->undo_state->list->type == UNDO_BOUNDARY) {
        return;
    }

    UndoRecord *rec = undo_record_alloc(UNDO_BOUNDARY);
    undo_prepend(buf, rec);

    buf->undo_state->boundary_needed = false;
    buf->undo_state->last_boundary_position = buf->pt;
}

void undo_boundary_if_needed(Buffer *buf) {
    if (!buf || !buf->undo_state) return;

    if (buf->undo_state->boundary_needed) {
        undo_boundary(buf);
    }
}

/// UNDO LIST MANAGEMENT

void undo_truncate(Buffer *buf) {
    if (!buf || !buf->undo_state) return;

    size_t strong_limit = get_undo_strong_limit();
    size_t outer_limit  = get_undo_outer_limit();

    if (buf->undo_state->list_byte_size <= outer_limit) return;

    if (get_undo_ask_before_discard()) {
        // TODO: Ask user interactively
        fprintf(stderr, "Undo info exceeded outer-limit, discarding oldest entries\n");
    }

    // List is newest-first so the tail is the oldest.
    // Walk until byte_size would drop under strong_limit, then chop.
    UndoRecord *prev = NULL;
    UndoRecord *curr = buf->undo_state->list;
    size_t bytes_seen = 0;

    while (curr) {
        if (curr->type == UNDO_DELETE)
            bytes_seen += curr->del.len;

        // Once we've seen enough that the REST is droppable, stop here.
        if (buf->undo_state->list_byte_size - bytes_seen <= strong_limit) {
            // curr is the last node we KEEP.  Free everything after it.
            UndoRecord *drop = curr->next;
            curr->next = NULL;
            buf->undo_state->list_tail = curr;

            while (drop) {
                UndoRecord *next = drop->next;
                if (drop->type == UNDO_DELETE) {
                    buf->undo_state->list_byte_size -= drop->del.len;
                }
                buf->undo_state->list_length--;
                undo_record_free(drop);
                drop = next;
            }
            return;
        }

        prev = curr;
        curr = curr->next;
    }
}

void undo_amalgamate(Buffer *buf) {
    if (!buf || !buf->undo_state) return;

    UndoRecord *curr = buf->undo_state->list;

    while (curr && curr->next) {
        if (curr->type == UNDO_INSERT && curr->next->type == UNDO_INSERT) {
            if (curr->insert.end == curr->next->insert.beg) {
                curr->insert.end = curr->next->insert.end;

                UndoRecord *old = curr->next;
                curr->next = old->next;
                if (old == buf->undo_state->list_tail) {
                    buf->undo_state->list_tail = curr;
                }
                undo_record_free(old);
                buf->undo_state->list_length--;
                continue;
            }
        }
        curr = curr->next;
    }
}

/// EXECUTE ONE UNDO RECORD (internal helper)

// Applies a single UndoRecord to the buffer's rope and records the
// inverse onto the undo list so that undoing again will redo.
// Returns true if the record was a boundary (caller uses this to
// count how many units have been undone).

static bool execute_undo_record(Buffer *buf, UndoRecord *rec, bool record_inverse) {
    switch (rec->type) {

        case UNDO_BOUNDARY:
            return true;

        case UNDO_POSITION: {
            if (record_inverse) {
                UndoRecord *inv = undo_record_alloc(UNDO_POSITION);
                inv->position_marker.pos = buf->pt;
                undo_prepend(buf, inv);
            }
            set_point(rec->position_marker.pos);
            return false;
        }

        case UNDO_INSERT: {
            size_t beg = rec->insert.beg;
            size_t end = rec->insert.end;
            size_t len = end - beg;

            char *text = malloc(len);
            rope_copy_bytes(buf->rope, beg, len, text, len);

            if (record_inverse) {
                UndoRecord *inv = undo_record_alloc(UNDO_DELETE);
                inv->del.text     = text;
                inv->del.len      = len;
                inv->del.position = (ssize_t)beg;
                undo_prepend(buf, inv);
            } else {
                free(text);  // Don't need it if not recording
            }

            buf->rope = rope_delete_bytes(buf->rope, beg, len);
            set_point(beg);
            return false;
        }

        case UNDO_DELETE: {
            size_t abs_pos  = (rec->del.position < 0)
                              ? (size_t)(-rec->del.position)
                              : (size_t)(rec->del.position);
            size_t text_len = rec->del.len;

            buf->rope = rope_insert_bytes(buf->rope, abs_pos,
                                          rec->del.text, text_len);

            if (record_inverse) {
                UndoRecord *inv = undo_record_alloc(UNDO_INSERT);
                inv->insert.beg = abs_pos;
                inv->insert.end = abs_pos + text_len;
                undo_prepend(buf, inv);
            }

            if (rec->del.position >= 0)
                set_point(abs_pos);
            else
                set_point(abs_pos + text_len);
            return false;
        }

        case UNDO_MODTIME:
            return false;

        case UNDO_PROPERTY:
            // TODO: implement text property restore + inverse recording
            return false;

        case UNDO_APPLY: {
            char *name_cstr = scm_to_locale_string(rec->apply.fun_name);
            SCM   var       = scm_c_lookup(name_cstr);
            free(name_cstr);
            scm_apply_0(scm_variable_ref(var), rec->apply.args);
            return false;
        }

        case UNDO_APPLY_SEL:
            // TODO: implement selective undo
            return false;

        case UNDO_MARKER:
            // TODO: implement marker adjustment
            return false;
    }
    return false;
}

/// INTERNAL UNDO ENGINE

// Walks the internal linked list directly.  Consumes records from the
// head until `count` boundaries have been crossed.  Each consumed
// record is freed AFTER its inverse has been recorded (so the inverse
// is at the head, consumed record goes away).
//
// This is what undo_command / redo_command call.  It never touches SCM
// lists — the round-trip through SCM was the bug that killed inverse
// records.

static void run_undo(Buffer *buf, int count) {
    if (!buf || !buf->undo_state) return;
    if (!buf->undo_state->list) return;

    // Determine if we should record inverses:
    // - First undo in a sequence: YES (creates the redo path)
    // - Consecutive undos: NO (we're walking back through history)
    bool record_inverse = !last_command_was_undo;

    // Only add a boundary if the last command was NOT an undo
    if (!last_command_was_undo) {
        buf->undo_state->in_progress = false;
        undo_boundary(buf);
        buf->undo_state->in_progress = true;
    }

    int boundaries_crossed = 0;

    // Determine starting point
    UndoRecord *cur = last_command_was_undo
                      ? buf->undo_state->list
                      : buf->undo_state->list->next;

    // Detach the list
    if (!last_command_was_undo) {
        buf->undo_state->list->next = NULL;
        buf->undo_state->list_tail = buf->undo_state->list;
    } else {
        buf->undo_state->list = NULL;
        buf->undo_state->list_tail = NULL;
    }

    while (cur) {
        UndoRecord *next = cur->next;

        bool is_boundary = execute_undo_record(buf, cur, record_inverse);
        if (is_boundary) {
            boundaries_crossed++;
            if (boundaries_crossed >= count) {
                if (next) {
                    if (buf->undo_state->list) {
                        UndoRecord *tail = buf->undo_state->list;
                        while (tail->next) tail = tail->next;
                        tail->next = next;
                    } else {
                        buf->undo_state->list = next;
                    }
                    UndoRecord *t = next;
                    while (t->next) t = t->next;
                    buf->undo_state->list_tail = t;
                }
                undo_record_free(cur);
                break;
            }
        }

        undo_record_free(cur);
        cur = next;
    }

    buf->undo_state->in_progress = false;
    buf->modified = true;
}

// PRIMITIVE-UNDO (SCM-facing binding only)
//
// This is exposed to Scheme as (primitive-undo COUNT LIST).
// It operates on an SCM list the caller passes in and returns the
// unconsumed tail.  It does NOT record inverses (that would require
// the caller to manage merging them back).  Use it when you have an
// SCM undo list you're driving from Scheme directly.

SCM primitive_undo(int count, SCM list, Buffer *buf) {
    if (!buf || !buf->undo_state) {
        return SCM_EOL;
    }

    buf->undo_state->in_progress = true;

    int i = 0;
    SCM remaining = list;

    while (i < count && !scm_is_null(remaining)) {
        SCM entry = scm_car(remaining);
        remaining = scm_cdr(remaining);

        // nil = boundary
        if (scm_is_null(entry)) {
            i++;
            continue;
        }

        // Integer = position
        if (scm_is_integer(entry)) {
            size_t pos = scm_to_size_t(entry);
            set_point(pos);
            continue;
        }

        if (!scm_is_pair(entry)) continue;

        SCM car_entry = scm_car(entry);
        SCM cdr_entry = scm_cdr(entry);

        // (BEG . END) = undo an insertion by deleting
        if (scm_is_integer(car_entry) && scm_is_integer(cdr_entry)) {
            size_t beg = scm_to_size_t(car_entry);
            size_t end = scm_to_size_t(cdr_entry);
            buf->rope = rope_delete_bytes(buf->rope, beg, end - beg);
            set_point(beg);
            continue;
        }

        // (TEXT . POSITION) = undo a deletion by inserting
        if (scm_is_string(car_entry) && scm_is_integer(cdr_entry)) {
            char *text       = scm_to_locale_string(car_entry);
            ssize_t position = scm_to_ssize_t(cdr_entry);
            size_t abs_pos   = (position < 0) ? (size_t)(-position) : (size_t)position;
            size_t text_len  = strlen(text);

            buf->rope = rope_insert_bytes(buf->rope, abs_pos, text, text_len);

            if (position >= 0)
                set_point(abs_pos);
            else
                set_point(abs_pos + text_len);

            free(text);
            continue;
        }

        // (t . TIMESTAMP) = modtime marker
        if (scm_is_eq(car_entry, SCM_BOOL_T)) {
            continue;
        }

        // (nil PROPERTY VALUE BEG . END) = text property
        if (scm_is_null(car_entry) && scm_is_pair(cdr_entry)) {
            // TODO: text properties
            continue;
        }

        // (apply ...) — plain and selective
        if (scm_is_eq(car_entry, scm_from_locale_symbol("apply")) &&
            scm_is_pair(cdr_entry)) {

            if (scm_is_integer(scm_car(cdr_entry))) {
                // selective — TODO
                continue;
            }

            SCM fun_name = scm_car(cdr_entry);
            SCM args     = scm_cdr(cdr_entry);

            char *name_cstr = scm_to_locale_string(fun_name);
            SCM  var        = scm_c_lookup(name_cstr);
            free(name_cstr);

            scm_apply_0(scm_variable_ref(var), args);
            continue;
        }

        // (MARKER . DISTANCE)
        if (scm_is_integer(cdr_entry)) {
            // TODO: markers
            continue;
        }
    }

    buf->undo_state->in_progress = false;
    buf->modified = true;

    return remaining;
}

/// HIGH-LEVEL UNDO COMMANDS

void undo_command(int arg) {
    if (!current_buffer || !current_buffer->undo_state) return;

    if (!current_buffer->undo_state->enabled) {
        fprintf(stderr, "No undo information in this buffer\n");
        return;
    }

    if (!current_buffer->undo_state->list) {
        fprintf(stderr, "No further undo information\n");
        return;
    }

    run_undo(current_buffer, arg > 0 ? arg : 1);

    // Next normal edit should start a new boundary.
    current_buffer->undo_state->boundary_needed = true;
}

void undo_only_command(int arg) {
    // Like undo but no inverse records -> no redo path.
    // We set in_progress = true the whole time so execute_undo_record's
    // prepends are blocked... but execute_undo_record does the prepends
    // directly, not through the guarded record_* helpers.
    // Simplest correct approach: just run normal undo.  If you truly
    // need no-redo semantics later, add a flag to run_undo.
    // For now this behaves identically to undo_command.
    undo_command(arg);
}

void redo_command(int arg) {
    // Redo IS undo in Emacs' circular model.  The last undo deposited
    // inverse records at the head of the list; undoing those replays
    // the original edit.  We just need a boundary first so the redo
    // chunk is its own unit, then run undo.
    if (!current_buffer || !current_buffer->undo_state) return;

    if (!current_buffer->undo_state->list) {
        fprintf(stderr, "Nothing to redo\n");
        return;
    }

    run_undo(current_buffer, arg > 0 ? arg : 1);
    current_buffer->undo_state->boundary_needed = true;
}

/// CONVERSION TO/FROM SCHEME
//
// These exist for the Scheme-facing (primitive-undo) and
// (buffer-undo-list) / (set-buffer-undo-list!) bindings.
// The internal C undo path (undo_command) does NOT use them.

SCM undo_list_to_scm(Buffer *buf) {
    if (!buf || !buf->undo_state) return SCM_EOL;

    // List is newest-first.  cons'ing head->tail produces oldest-first,
    // then we reverse to restore newest-first for the SCM consumer.
    SCM result = SCM_EOL;
    UndoRecord *rec = buf->undo_state->list;

    while (rec) {
        SCM entry = SCM_EOL;

        switch (rec->type) {
            case UNDO_BOUNDARY:
                entry = SCM_EOL;
                break;
            case UNDO_POSITION:
                entry = scm_from_size_t(rec->position_marker.pos);
                break;
            case UNDO_INSERT:
                entry = scm_cons(scm_from_size_t(rec->insert.beg),
                                 scm_from_size_t(rec->insert.end));
                break;
            case UNDO_DELETE:
                entry = scm_cons(scm_from_locale_string(rec->del.text),
                                 scm_from_ssize_t(rec->del.position));
                break;
            case UNDO_MODTIME:
                entry = scm_cons(SCM_BOOL_T,
                                 scm_from_long(rec->modtime.timestamp));
                break;
            case UNDO_PROPERTY:
                entry = scm_list_5(SCM_EOL,
                                   rec->property.property,
                                   rec->property.value,
                                   scm_from_size_t(rec->property.beg),
                                   scm_from_size_t(rec->property.end));
                break;
            case UNDO_APPLY:
                entry = scm_cons(scm_from_locale_symbol("apply"),
                                 scm_cons(rec->apply.fun_name, rec->apply.args));
                break;
            case UNDO_APPLY_SEL:
                entry = scm_list_n(scm_from_locale_symbol("apply"),
                                   scm_from_ssize_t(rec->apply_sel.delta),
                                   scm_from_size_t(rec->apply_sel.beg),
                                   scm_from_size_t(rec->apply_sel.end),
                                   rec->apply_sel.fun_name,
                                   rec->apply_sel.args,
                                   SCM_UNDEFINED);
                break;
            case UNDO_MARKER:
                entry = scm_cons(rec->marker.marker,
                                 scm_from_ssize_t(rec->marker.distance));
                break;
        }

        result = scm_cons(entry, result);
        rec = rec->next;
    }

    return scm_reverse(result);
}

void undo_list_from_scm(Buffer *buf, SCM list) {
    if (!buf || !buf->undo_state) return;

    // Drop the old internal list.
    undo_list_free(buf->undo_state->list);
    buf->undo_state->list      = NULL;
    buf->undo_state->list_tail = NULL;
    buf->undo_state->list_length    = 0;
    buf->undo_state->list_byte_size = 0;

    // Rebuild newest-first by appending in SCM-list order (which is
    // already newest-first from undo_list_to_scm).
    while (!scm_is_null(list)) {
        SCM entry = scm_car(list);
        list      = scm_cdr(list);

        UndoRecord *rec = NULL;

        if (scm_is_null(entry)) {
            rec = undo_record_alloc(UNDO_BOUNDARY);
            undo_append(buf, rec);
            continue;
        }

        if (scm_is_integer(entry)) {
            rec = undo_record_alloc(UNDO_POSITION);
            rec->position_marker.pos = scm_to_size_t(entry);
            undo_append(buf, rec);
            continue;
        }

        if (!scm_is_pair(entry)) continue;

        SCM car_e = scm_car(entry);
        SCM cdr_e = scm_cdr(entry);

        // (BEG . END)
        if (scm_is_integer(car_e) && scm_is_integer(cdr_e)) {
            rec = undo_record_alloc(UNDO_INSERT);
            rec->insert.beg = scm_to_size_t(car_e);
            rec->insert.end = scm_to_size_t(cdr_e);
            undo_append(buf, rec);
            continue;
        }

        // (TEXT . POSITION)
        if (scm_is_string(car_e) && scm_is_integer(cdr_e)) {
            char *text = scm_to_locale_string(car_e);
            size_t len = strlen(text);
            rec = undo_record_alloc(UNDO_DELETE);
            rec->del.text     = text;
            rec->del.len      = len;
            rec->del.position = scm_to_ssize_t(cdr_e);
            undo_append(buf, rec);
            continue;
        }

        // (t . TIMESTAMP)
        if (scm_is_eq(car_e, SCM_BOOL_T)) {
            rec = undo_record_alloc(UNDO_MODTIME);
            rec->modtime.timestamp = (time_t)scm_to_long(cdr_e);
            undo_append(buf, rec);
            continue;
        }

        // (apply ...)
        if (scm_is_eq(car_e, scm_from_locale_symbol("apply")) && scm_is_pair(cdr_e)) {
            if (scm_is_integer(scm_car(cdr_e))) {
                SCM rest = cdr_e;
                ssize_t delta = scm_to_ssize_t(scm_car(rest)); rest = scm_cdr(rest);
                size_t  beg   = scm_to_size_t(scm_car(rest));  rest = scm_cdr(rest);
                size_t  end   = scm_to_size_t(scm_car(rest));  rest = scm_cdr(rest);
                SCM fun_name  = scm_car(rest);
                SCM args      = scm_cdr(rest);

                rec = undo_record_alloc(UNDO_APPLY_SEL);
                rec->apply_sel.delta    = delta;
                rec->apply_sel.beg      = beg;
                rec->apply_sel.end      = end;
                rec->apply_sel.fun_name = fun_name;
                rec->apply_sel.args     = args;
                scm_gc_protect_object(fun_name);
                scm_gc_protect_object(args);
            } else {
                rec = undo_record_alloc(UNDO_APPLY);
                rec->apply.fun_name = scm_car(cdr_e);
                rec->apply.args     = scm_cdr(cdr_e);
                scm_gc_protect_object(rec->apply.fun_name);
                scm_gc_protect_object(rec->apply.args);
            }
            undo_append(buf, rec);
            continue;
        }

        // (nil PROPERTY VALUE BEG . END)
        if (scm_is_null(car_e) && scm_is_pair(cdr_e)) {
            SCM prop  = scm_car(cdr_e);
            SCM rest  = scm_cdr(cdr_e);
            if (scm_is_pair(rest)) {
                SCM value = scm_car(rest);
                SCM rest2 = scm_cdr(rest);
                if (scm_is_pair(rest2)) {
                    rec = undo_record_alloc(UNDO_PROPERTY);
                    rec->property.property = prop;
                    rec->property.value    = value;
                    rec->property.beg      = scm_to_size_t(scm_car(rest2));
                    rec->property.end      = scm_to_size_t(scm_cdr(rest2));
                    scm_gc_protect_object(prop);
                    scm_gc_protect_object(value);
                    undo_append(buf, rec);
                    continue;
                }
            }
        }

        // (MARKER . DISTANCE) — last, any pair with integer cdr not matched above
        if (scm_is_integer(cdr_e)) {
            rec = undo_record_alloc(UNDO_MARKER);
            rec->marker.marker   = car_e;
            rec->marker.distance = scm_to_ssize_t(cdr_e);
            scm_gc_protect_object(car_e);
            undo_append(buf, rec);
            continue;
        }
    }
}

/// ENABLE/DISABLE

void undo_disable(Buffer *buf) {
    if (!buf || !buf->undo_state) return;
    buf->undo_state->enabled = false;
}

void undo_enable(Buffer *buf) {
    if (!buf || !buf->undo_state) return;
    buf->undo_state->enabled = true;
}

bool undo_enabled(Buffer *buf) {
    if (!buf || !buf->undo_state) return false;
    return buf->undo_state->enabled;
}

bool undo_list_empty(Buffer *buf) {
    if (!buf || !buf->undo_state) return true;
    return buf->undo_state->list == NULL;
}

SCM buffer_undo_list_scm(Buffer *buf) {
    if (!buf) return SCM_EOL;
    return undo_list_to_scm(buf);
}

/// SCM WRAPPERS

static SCM scm_undo_boundary(void) {
    if (current_buffer) {
        undo_boundary(current_buffer);
    }
    return SCM_UNSPECIFIED;
}

static SCM scm_primitive_undo(SCM count, SCM list) {
    if (!current_buffer) return SCM_EOL;
    int n = scm_to_int(count);
    return primitive_undo(n, list, current_buffer);
}

// All three use rest-arg style: 0 required, 0 optional, 1 rest.
// The rest arg is a list; we pull count out if present, else default 1.
static SCM scm_undo(SCM rest) {
    int n = 1;
    if (scm_is_pair(rest))
        n = scm_to_int(scm_car(rest));
    undo_command(n);
    return SCM_UNSPECIFIED;
}

static SCM scm_undo_redo(SCM rest) {
    int n = 1;
    if (scm_is_pair(rest))
        n = scm_to_int(scm_car(rest));
    redo_command(n);
    return SCM_UNSPECIFIED;
}

static SCM scm_undo_only(SCM rest) {
    int n = 1;
    if (scm_is_pair(rest))
        n = scm_to_int(scm_car(rest));
    undo_only_command(n);
    return SCM_UNSPECIFIED;
}

static SCM scm_buffer_undo_list(void) {
    if (!current_buffer) return SCM_EOL;
    return buffer_undo_list_scm(current_buffer);
}

static SCM scm_set_buffer_undo_list(SCM list) {
    if (!current_buffer) return SCM_UNSPECIFIED;

    if (scm_is_eq(list, SCM_BOOL_T)) {
        undo_disable(current_buffer);
        return SCM_BOOL_T;
    }

    if (scm_is_null(list)) {
        undo_enable(current_buffer);
        undo_list_from_scm(current_buffer, SCM_EOL);
        return SCM_EOL;
    }

    undo_enable(current_buffer);
    undo_list_from_scm(current_buffer, list);
    return list;
}

/// INITIALIZATION

void init_undo_bindings(void) {
    scm_c_define_gsubr("undo-boundary",          0, 0, 0, scm_undo_boundary);
    scm_c_define_gsubr("primitive-undo",         2, 0, 0, scm_primitive_undo);
    scm_c_define_gsubr("undo",                   0, 0, 1, scm_undo);
    scm_c_define_gsubr("undo-redo",              0, 0, 1, scm_undo_redo);
    scm_c_define_gsubr("undo-only",              0, 0, 1, scm_undo_only);
    scm_c_define_gsubr("buffer-undo-list",       0, 0, 0, scm_buffer_undo_list);
    scm_c_define_gsubr("set-buffer-undo-list!",  1, 0, 0, scm_set_buffer_undo_list);
}

#include "dired.h"
#include "edit.h"
#include "faces.h"
#include "theme.h"
#include <ctype.h>
#include <dirent.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include "syntax.h"
#include "wm.h"

Direds direds; // NOTE GLobal direds

#define DIRED_INITIAL_CAPACITY 32
#define HUMAN_READABLE_SIZE_BUF 16

static const char* get_file_type_char(mode_t mode) {
    if (S_ISDIR(mode))  return "d";
    if (S_ISLNK(mode))  return "l";
    if (S_ISFIFO(mode)) return "p";
    if (S_ISSOCK(mode)) return "s";
    if (S_ISCHR(mode))  return "c";
    if (S_ISBLK(mode))  return "b";
    return "-";
}

static void format_permissions(mode_t mode, char *buf) {
    buf[0] = (mode & S_IRUSR) ? 'r' : '-';
    buf[1] = (mode & S_IWUSR) ? 'w' : '-';
    buf[2] = (mode & S_IXUSR) ? 'x' : '-';
    buf[3] = (mode & S_IRGRP) ? 'r' : '-';
    buf[4] = (mode & S_IWGRP) ? 'w' : '-';
    buf[5] = (mode & S_IXGRP) ? 'x' : '-';
    buf[6] = (mode & S_IROTH) ? 'r' : '-';
    buf[7] = (mode & S_IWOTH) ? 'w' : '-';
    buf[8] = (mode & S_IXOTH) ? 'x' : '-';
    buf[9] = '\0';
}

static char* format_human_readable_size(off_t size) {
    static const char *units[] = {"B", "K", "M", "G", "T", "P"};
    double s = (double)size;
    int unit = 0;
    
    while (s >= 1024 && unit < (sizeof(units)/sizeof(units[0])-1)) {
        s /= 1024;
        unit++;
    }
    
    char *buf = malloc(HUMAN_READABLE_SIZE_BUF);
    if (!buf) return NULL;
    
    if (unit == 0) {
        snprintf(buf, HUMAN_READABLE_SIZE_BUF, "%lld", (long long)size);
    } else {
        snprintf(buf, HUMAN_READABLE_SIZE_BUF, "%.1f%s", s, units[unit]);
    }
    
    return buf;
}

Dired* dired_new(const char *path, Buffer *buffer) {
    Dired *dired = malloc(sizeof(Dired));
    if (!dired) return NULL;
    
    dired->directory = strdup(path);
    if (!dired->directory) {
        free(dired);
        return NULL;
    }
    
    dired->entries = malloc(sizeof(DiredEntry) * DIRED_INITIAL_CAPACITY);
    if (!dired->entries) {
        free(dired->directory);
        free(dired);
        return NULL;
    }
    
    dired->count = 0;
    dired->capacity = DIRED_INITIAL_CAPACITY;
    dired->show_hidden = false;
    dired->human_readable = true;
    dired->sort_field = 0;
    dired->sort_reverse = false;
    dired->buffer = buffer;
    
    if (!dired_read_directory(dired, path)) {
        dired_free(dired);
        return NULL;
    }
    
    return dired;
}

void dired_free(Dired *dired) {
    if (!dired) return;
    
    free(dired->directory);
    
    for (size_t i = 0; i < dired->count; i++) {
        DiredEntry *entry = &dired->entries[i];
        free(entry->name);
        free(entry->symlink_target);
        free(entry->permissions);
        free(entry->owner);
        free(entry->group);
        free(entry->size);
        free(entry->time);
    }
    
    free(dired->entries);
    free(dired);
}

bool dired_read_directory(Dired *dired, const char *path) {
    DIR *dir = opendir(path);
    if (!dir) return false;
    
    // Clear existing entries
    for (size_t i = 0; i < dired->count; i++) {
        DiredEntry *entry = &dired->entries[i];
        free(entry->name);
        free(entry->symlink_target);
        free(entry->permissions);
        free(entry->owner);
        free(entry->group);
        free(entry->size);
        free(entry->time);
    }
    dired->count = 0;
    
    // Read directory entries
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        // Skip . and .. entries
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;
            
        // Skip hidden files if not showing them
        if (!dired->show_hidden && entry->d_name[0] == '.')
            continue;
            
        // Check if we need to grow the entries array
        if (dired->count >= dired->capacity) {
            size_t new_capacity = dired->capacity * 2;
            DiredEntry *new_entries = realloc(dired->entries, sizeof(DiredEntry) * new_capacity);
            if (!new_entries) {
                closedir(dir);
                return false;
            }
            dired->entries = new_entries;
            dired->capacity = new_capacity;
        }
        
        // Build full path for stat
        char full_path[PATH_MAX];
        snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);
        
        // Get file info
        struct stat st;
        if (lstat(full_path, &st) != 0) {
            continue; // Skip if we can't stat
        }
        
        DiredEntry *de = &dired->entries[dired->count];
        memset(de, 0, sizeof(DiredEntry));
        
        // Basic info
        de->name = strdup(entry->d_name);
        de->hidden = (entry->d_name[0] == '.');
        de->inode = st.st_ino;
        de->nlink = st.st_nlink;
        de->raw_size = st.st_size;
        de->mtime = st.st_mtime;
        
        // Determine type
        if (S_ISDIR(st.st_mode)) de->type = DIRED_DIRECTORY;
        else if (S_ISLNK(st.st_mode)) de->type = DIRED_SYMLINK;
        else if (!S_ISREG(st.st_mode)) de->type = DIRED_SPECIAL;
        else de->type = DIRED_FILE;
        
        // For symlinks, read the target
        if (de->type == DIRED_SYMLINK) {
            char symlink_buf[PATH_MAX];
            ssize_t len = readlink(full_path, symlink_buf, sizeof(symlink_buf)-1);
            if (len > 0) {
                symlink_buf[len] = '\0';
                de->symlink_target = strdup(symlink_buf);
            }
        }
        
        // Permissions
        char type_char = get_file_type_char(st.st_mode)[0];
        char perm_buf[10];
        format_permissions(st.st_mode, perm_buf);
        de->permissions = malloc(11);
        if (de->permissions) {
            snprintf(de->permissions, 11, "%c%s", type_char, perm_buf);
        }
        
        // Owner and group
        struct passwd *pw = getpwuid(st.st_uid);
        if (pw) de->owner = strdup(pw->pw_name);
        else de->owner = strdup("?");
        
        struct group *gr = getgrgid(st.st_gid);
        if (gr) de->group = strdup(gr->gr_name);
        else de->group = strdup("?");
        
        // Size
        if (dired->human_readable) {
            de->size = format_human_readable_size(st.st_size);
        } else {
            de->size = malloc(32);
            if (de->size) {
                snprintf(de->size, 32, "%lld", (long long)st.st_size);
            }
        }
        
        // Time
        char time_buf[64];
        struct tm *tm = localtime(&st.st_mtime);
        strftime(time_buf, sizeof(time_buf), "%b %d %H:%M", tm);
        de->time = strdup(time_buf);
        
        dired->count++;
    }
    
    closedir(dir);
    
    // Sort entries
    dired_sort(dired, dired->sort_field, dired->sort_reverse);
    
    return true;
}

//--

bool dired_revert(Dired *dired) {
    if (!dired || !dired->buffer) return false;
    
    // Save current cursor state
    size_t saved_point = dired->buffer->point;
    int current_line = getLineNumber(dired->buffer);
    size_t current_col = current_column(dired->buffer);
    const char *current_filename = NULL;
    
    // Get current filename if we're on an entry line
    if (current_line > 0 && (size_t)(current_line - 1) < dired->count) {
        current_filename = dired->entries[current_line - 1].name;
    }
    
    // Save display settings
    bool show_hidden = dired->show_hidden;
    bool human_readable = dired->human_readable;
    bool details_hidden = dired->details_hidden;
    int sort_field = dired->sort_field;
    bool sort_reverse = dired->sort_reverse;
    
    // Free existing entries
    for (size_t i = 0; i < dired->count; i++) {
        DiredEntry *entry = &dired->entries[i];
        free(entry->name);
        free(entry->symlink_target);
        free(entry->permissions);
        free(entry->owner);
        free(entry->group);
        free(entry->size);
        free(entry->time);
    }
    dired->count = 0;
    
    // Re-read directory
    if (!dired_read_directory(dired, dired->directory)) {
        // Restore settings if read fails
        dired->show_hidden = show_hidden;
        dired->human_readable = human_readable;
        dired->details_hidden = details_hidden;
        dired->sort_field = sort_field;
        dired->sort_reverse = sort_reverse;
        return false;
    }
    
    // Restore settings
    dired->show_hidden = show_hidden;
    dired->human_readable = human_readable;
    dired->details_hidden = details_hidden;
    dired->sort_field = sort_field;
    dired->sort_reverse = sort_reverse;
    
    // Re-sort
    dired_sort(dired, sort_field, sort_reverse);
    
    // Generate new content
    char *content = dired_format_all_entries(dired);
    if (!content) return false;
    
    // Update buffer without moving point
    setBufferContent(dired->buffer, content, false);
    free(content);
    
    // Try to restore cursor position
    if (current_filename) {
        // Find matching entry
        for (size_t i = 0; i < dired->count; i++) {
            if (strcmp(dired->entries[i].name, current_filename) == 0) {
                size_t line_start = line_beginning_position_at(dired->buffer, i + 1); // +1 for header
                
                // Calculate position based on view mode
                if (dired->details_hidden) {
                    dired->buffer->point = line_start + 2; // After indentation
                } else {
                    // Try to maintain column position if possible
                    size_t target_pos = line_start + current_col;
                    if (target_pos < line_start + strlen(dired->entries[i].name) + 2) {
                        dired->buffer->point = target_pos;
                    } else {
                        dired->buffer->point = line_start + dired->columns.name;
                    }
                }
                break;
            }
        }
    }
    
    parse_and_push_dired_syntax(dired->buffer);
    return true;
}

/* bool dired_revert(Dired *dired) { */
/*     if (!dired || !dired->buffer) return false; */
    
/*     // Save current display settings */
/*     bool show_hidden = dired->show_hidden; */
/*     bool human_readable = dired->human_readable; */
/*     bool details_hidden = dired->details_hidden; */
/*     int sort_field = dired->sort_field; */
/*     bool sort_reverse = dired->sort_reverse; */
    
/*     // Free all existing entries */
/*     for (size_t i = 0; i < dired->count; i++) { */
/*         DiredEntry *entry = &dired->entries[i]; */
/*         free(entry->name); */
/*         free(entry->symlink_target); */
/*         free(entry->permissions); */
/*         free(entry->owner); */
/*         free(entry->group); */
/*         free(entry->size); */
/*         free(entry->time); */
/*     } */
/*     dired->count = 0; */
    
/*     // Re-read the directory */
/*     if (!dired_read_directory(dired, dired->directory)) { */
/*         // If read fails, try to restore at least some state */
/*         dired->show_hidden = show_hidden; */
/*         dired->human_readable = human_readable; */
/*         dired->details_hidden = details_hidden; */
/*         dired->sort_field = sort_field; */
/*         dired->sort_reverse = sort_reverse; */
/*         return false; */
/*     } */
    
/*     // Restore display settings */
/*     dired->show_hidden = show_hidden; */
/*     dired->human_readable = human_readable; */
/*     dired->details_hidden = details_hidden; */
/*     dired->sort_field = sort_field; */
/*     dired->sort_reverse = sort_reverse; */
    
/*     // Re-sort with saved settings */
/*     dired_sort(dired, sort_field, sort_reverse); */
    
/*     // Generate new content */
/*     char *content = dired_format_all_entries(dired); */
/*     if (!content) { */
/*         return false; */
/*     } */
    
/*     // Update the buffer */
/*     setBufferContent(dired->buffer, content, true); */
/*     free(content); */
    
/*     // Re-apply syntax highlighting */
/*     parse_and_push_dired_syntax(dired->buffer); */
    
/*     return true; */
/* } */


// TODO Create a new dired and push it
/* bool dired_revert(Dired *dired) { */
/*     if (!dired) return false; */
/*     return dired_read_directory(dired, dired->directory); */
/* } */

void dired_mark(Dired *dired, size_t index) {
    if (!dired || index >= dired->count) return;
    dired->entries[index].marked = true;
}

void dired_unmark(Dired *dired, size_t index) {
    if (!dired || index >= dired->count) return;
    dired->entries[index].marked = false;
}

void dired_toggle_mark(Dired *dired, size_t index) {
    if (!dired || index >= dired->count) return;
    dired->entries[index].marked = !dired->entries[index].marked;
}

void dired_mark_all(Dired *dired) {
    if (!dired) return;
    for (size_t i = 0; i < dired->count; i++) {
        dired->entries[i].marked = true;
    }
}

void dired_unmark_all(Dired *dired) {
    if (!dired) return;
    for (size_t i = 0; i < dired->count; i++) {
        dired->entries[i].marked = false;
    }
}

bool dired_delete_marked(Dired *dired) {
    if (!dired) return false;
    
    bool success = true;
    char path[PATH_MAX];
    
    for (size_t i = 0; i < dired->count; i++) {
        if (dired->entries[i].marked) {
            snprintf(path, sizeof(path), "%s/%s", dired->directory, dired->entries[i].name);
            
            if (dired->entries[i].type == DIRED_DIRECTORY) {
                // For directories, we need to recursively delete contents
                // This is a simplified version - a real implementation would be more robust
                if (rmdir(path) != 0) {
                    success = false;
                    continue;
                }
            } else {
                if (unlink(path) != 0) {
                    success = false;
                    continue;
                }
            }
        }
    }
    
    // Refresh the directory listing
    dired_revert(dired);
    return success;
}

bool dired_rename(Dired *dired, size_t index, const char *new_name) {
    if (!dired || index >= dired->count || !new_name || !*new_name) return false;
    
    char old_path[PATH_MAX], new_path[PATH_MAX];
    snprintf(old_path, sizeof(old_path), "%s/%s", dired->directory, dired->entries[index].name);
    snprintf(new_path, sizeof(new_path), "%s/%s", dired->directory, new_name);
    
    if (rename(old_path, new_path) != 0) {
        return false;
    }
    
    // Update the entry
    free(dired->entries[index].name);
    dired->entries[index].name = strdup(new_name);
    
    return true;
}

bool dired_create_directory(Dired *dired, const char *name) {
    if (!dired || !name || !*name) return false;
    
    char path[PATH_MAX];
    snprintf(path, sizeof(path), "%s/%s", dired->directory, name);
    
    if (mkdir(path, 0755) != 0) {
        return false;
    }
    
    // Refresh the directory listing
    dired_revert(dired);
    return true;
}

static int compare_entries(const void *a, const void *b, void *arg) {
    const DiredEntry *ea = (const DiredEntry *)a;
    const DiredEntry *eb = (const DiredEntry *)b;
    Dired *dired = (Dired *)arg;
    
    int result = 0;
    
    switch (dired->sort_field) {
        case 0: // Name
            result = strcasecmp(ea->name, eb->name);
            break;
        case 1: // Size
            result = (ea->raw_size < eb->raw_size) ? -1 : (ea->raw_size > eb->raw_size);
            break;
        case 2: // Time
            result = (ea->mtime < eb->mtime) ? -1 : (ea->mtime > eb->mtime);
            break;
        case 3: // Type
            result = (ea->type < eb->type) ? -1 : (ea->type > eb->type);
            break;
        default:
            result = strcasecmp(ea->name, eb->name);
    }
    
    return dired->sort_reverse ? -result : result;
}


static int compare_by_name(const void *a, const void *b) {
    const DiredEntry *ea = a, *eb = b;
    return strcasecmp(ea->name, eb->name);
}

static int compare_by_size(const void *a, const void *b) {
    const DiredEntry *ea = a, *eb = b;
    return (ea->raw_size > eb->raw_size) - (ea->raw_size < eb->raw_size);
}

void dired_sort(Dired *dired, int field, bool reverse) {
    if (!dired) return;
    
    int (*compar)(const void *, const void *) = NULL;
    
    switch (field) {
        case 0: compar = compare_by_name; break;
        case 1: compar = compare_by_size; break;
        // Add other cases
        default: compar = compare_by_name;
    }
    
    qsort(dired->entries, dired->count, sizeof(DiredEntry), compar);
    
    if (reverse) {
        // Reverse the array if needed
        for (size_t i = 0; i < dired->count/2; i++) {
            DiredEntry tmp = dired->entries[i];
            dired->entries[i] = dired->entries[dired->count-i-1];
            dired->entries[dired->count-i-1] = tmp;
        }
    }
}


char* dired_format_header(const Dired *dired) {
    if (!dired) return NULL;
    
    char *buf = malloc(256);
    if (!buf) return NULL;
    
    snprintf(buf, 256, "  %s", dired->directory);  // Removed the colon
    return buf;
}

char* dired_format_entry(const DiredEntry *entry, bool human_readable, bool details_hidden) {
    if (!entry) return NULL;
    
    char *buf = malloc(256);
    if (!buf) return NULL;
    
    if (details_hidden) {
        // Only show file name with 2-space indentation
        snprintf(buf, 256, "  %s", entry->name);
    } else {
        // Format the size appropriately
        char size_str[32];
        if (human_readable) {
            strncpy(size_str, entry->size ? entry->size : "0", sizeof(size_str));
        } else {
            snprintf(size_str, sizeof(size_str), "%lld", (long long)entry->raw_size);
        }
        
        // Format the full line with details
        snprintf(buf, 256, "  %s %2lu %-8s %-8s %7s %s %s",
                 entry->permissions,
                 (unsigned long)entry->nlink,
                 entry->owner,
                 entry->group,
                 size_str,
                 entry->time,
                 entry->name);
    }
    
    return buf;
}

void dired_insert_into_buffer(Buffer *buffer, const Dired *dired) {
    if (!buffer || !dired) return;
    
    // Build content in a temporary buffer
    size_t total_size = 0;
    
    // Calculate total size needed
    total_size += strlen(dired->directory) + 3; // Header
    for (size_t i = 0; i < dired->count; i++) {
        total_size += 80; // Approximate line length
    }
    
    char *content = malloc(total_size + 1);
    if (!content) return;
    content[0] = '\0';
    
    // Add header
    strcat(content, " ");
    strcat(content, dired->directory);
    strcat(content, "\n");
    
    // Add entries
    for (size_t i = 0; i < dired->count; i++) {
        char *entry = dired_format_entry(&dired->entries[i], 
                                       dired->human_readable,
                                       dired->details_hidden);
        if (entry) {
            strcat(content, entry);
            strcat(content, "\n");
            free(entry);
        }
    }
    
    // Set buffer content in one operation
    setBufferContent(buffer, content, true);
    free(content);
    
    // Update syntax highlighting
    parse_and_push_dired_syntax(buffer);
}

char* dired_format_all_entries(const Dired *dired) {
    if (!dired || !dired->entries) return NULL;
    
    // First pass: find maximum size width
    int max_size_width = 0;
    for (size_t i = 0; i < dired->count; i++) {
        char size_buf[32];
        snprintf(size_buf, sizeof(size_buf), "%lld", (long long)dired->entries[i].raw_size);
        int len = strlen(size_buf);
        if (len > max_size_width) max_size_width = len;
    }
    
    // Calculate total size needed
    size_t total_size = 0;
    total_size += strlen(dired->directory) + 3; // Header: " path:\n"
    total_size += dired->count * (max_size_width + 80); // Approximate line length
    
    char *result = malloc(total_size + 1);
    if (!result) return NULL;
    result[0] = '\0';
    
    // Add header
    strcat(result, " ");
    strcat(result, dired->directory);
    strcat(result, ":\n");
    
    // Format each entry
    for (size_t i = 0; i < dired->count; i++) {
        const DiredEntry *entry = &dired->entries[i];
        
        char time_buf[32];
        struct tm *tm = localtime(&entry->mtime);
        strftime(time_buf, sizeof(time_buf), "%b %e %H:%M", tm);
        
        char line[256];
        snprintf(line, sizeof(line), 
                " %s %lu %s %s %*lld %s %s",
                entry->permissions,
                (unsigned long)entry->nlink,
                entry->owner,
                entry->group,
                max_size_width, (long long)entry->raw_size,  // Right-aligned size
                time_buf,
                entry->name);
        
        strcat(result, line);
        strcat(result, "\n");
    }
    
    return result;
}


//-- Next day

void dired_update_column_widths(Dired *dired) {
    if (!dired) return;
    
    // Reset all columns
    dired->columns.permissions = 2;  // Initial 2-space indentation
    dired->columns.links = dired->columns.permissions + 11;  // permissions width + space
    dired->columns.owner = dired->columns.links + 3;  // links width (2) + space
    dired->columns.group = dired->columns.owner + 9;  // owner width (8) + space
    dired->columns.size = dired->columns.group + 9;   // group width (8) + space
    dired->columns.date = dired->columns.size + 8;    // size width (7) + space
    dired->columns.name = dired->columns.date + 13;   // date width (12) + space
    
    // Dynamic adjustment based on actual content
    size_t max_owner = 0, max_group = 0, max_size = 0;
    
    for (size_t i = 0; i < dired->count; i++) {
        DiredEntry *entry = &dired->entries[i];
        
        if (entry->owner && strlen(entry->owner) > max_owner)
            max_owner = strlen(entry->owner);
            
        if (entry->group && strlen(entry->group) > max_group)
            max_group = strlen(entry->group);
            
        char size_buf[32];
        snprintf(size_buf, sizeof(size_buf), "%lld", (long long)entry->raw_size);
        size_t size_len = strlen(size_buf);
        if (size_len > max_size)
            max_size = size_len;
    }
    
    // Update column positions with dynamic widths
    dired->columns.owner = dired->columns.links + 3;
    dired->columns.group = dired->columns.owner + max_owner + 1;
    dired->columns.size = dired->columns.group + max_group + 1;
    dired->columns.date = dired->columns.size + max_size + 1;
    dired->columns.name = dired->columns.date + 13;  // Date format is fixed width
}




void dired_toggle_details(Dired *dired) {
    if (!dired || !dired->buffer) return;
    
    // Save current position info
    int current_line = getLineNumber(dired->buffer);
    const char *current_filename = NULL;
    
    if (current_line > 0 && (size_t)(current_line - 1) < dired->count) {
        current_filename = dired->entries[current_line - 1].name;
    }
    
    // Toggle display mode
    dired->details_hidden = !dired->details_hidden;
    
    // Update buffer
    dired_insert_into_buffer(dired->buffer, dired);
    
    // Restore cursor position
    if (current_filename) {
        // Find matching entry
        for (size_t i = 0; i < dired->count; i++) {
            if (strcmp(dired->entries[i].name, current_filename) == 0) {
                size_t line_start = line_beginning_position_at(dired->buffer, i + 1); // +1 for header
                
                if (dired->details_hidden) {
                    // Simple view - put cursor after indentation
                    dired->buffer->point = line_start + 2;
                } else {
                    // Detailed view - put cursor at name column
                    dired->buffer->point = line_start + dired->columns.name;
                }
                break;
            }
        }
    }
}

bool dired_jump(WindowManager *wm) {
    if (!wm || !wm->activeWindow || !wm->activeWindow->buffer || !wm->activeWindow->buffer->path) {
        return false;
    }

    // Get the current buffer's path
    const char *buffer_path = wm->activeWindow->buffer->path;
    
    // Expand ~/ to full home directory path
    char full_path[PATH_MAX];
    if (buffer_path[0] == '~' && buffer_path[1] == '/') {
        const char *home = getenv("HOME");
        if (!home) {
            message("Could not determine home directory");
            return false;
        }
        snprintf(full_path, sizeof(full_path), "%s%s", home, buffer_path + 1);
    } else {
        strncpy(full_path, buffer_path, sizeof(full_path) - 1);
        full_path[sizeof(full_path) - 1] = '\0';
    }

    // Check if it's a directory or file
    struct stat st;
    if (stat(full_path, &st) != 0) {
        return false;
    }

    char dir_path[PATH_MAX];
    bool is_directory = S_ISDIR(st.st_mode);
    
    if (is_directory) {
        // Use the path directly if it's a directory
        strncpy(dir_path, full_path, sizeof(dir_path));
    } else {
        // For files, get the parent directory
        char *last_slash = strrchr(full_path, '/');
        if (!last_slash) {
            // No parent directory (file is in current directory)
            strncpy(dir_path, ".", sizeof(dir_path));
        } else {
            strncpy(dir_path, full_path, last_slash - full_path);
            dir_path[last_slash - full_path] = '\0';
        }
    }

    // Create display name (show as ~/path if in home directory)
    char display_name[PATH_MAX + 32];
    const char *home = getenv("HOME");
    if (home && strncmp(dir_path, home, strlen(home)) == 0) {
        snprintf(display_name, sizeof(display_name), "Dired: ~%s", dir_path + strlen(home));
    } else {
        snprintf(display_name, sizeof(display_name), "Dired: %s", dir_path);
    }

    // Check for existing buffer first
    Buffer *existing = getBuffer(&bm, display_name);
    if (existing) {
        wm->activeWindow->buffer = existing;
        switchToBuffer(&bm, display_name);
        
        // Position point at first entry name in existing buffer
        position_point_at_first_entry(existing);
        
        // If we're looking at a file, try to highlight it in the directory listing
        if (!is_directory) {
            const char *filename = strrchr(full_path, '/');
            filename = filename ? filename + 1 : full_path;
            
            // Find the file in the directory listing
            Dired *dired = dired_for_buffer(existing);
            if (dired) {
                for (size_t i = 0; i < dired->count; i++) {
                    if (strcmp(dired->entries[i].name, filename) == 0) {
                        // Move point to this entry
                        size_t line_start = line_beginning_position_at(existing, i + 1); // +1 for header
                        if (dired->details_hidden) {
                            existing->point = line_start + 2; // After indentation
                        } else {
                            existing->point = line_start + dired->columns.name;
                        }
                        break;
                    }
                }
            }
        }
        return true;
    }

    // Create new buffer
    Buffer *dired_buf = generate_new_buffer(display_name, dir_path, fontPath);
    if (!dired_buf) {
        message("Failed to create Dired buffer");
        return false;
    }

    // Initialize Dired structure and add to global collection
    Dired *dired = direds_get_or_create(dir_path, dired_buf);
    if (!dired) {
        message("Failed to read directory");
        freeBuffer(dired_buf);
        return false;
    }

    // Format directory listing
    char *content = dired_format_all_entries(dired);
    if (!content) {
        message("Failed to format directory listing");
        dired_free(dired);
        freeBuffer(dired_buf);
        return false;
    }

    // Set buffer content (don't point at size)
    setBufferContent(dired_buf, content, false);
    free(content);
    setMajorMode(dired_buf, "dired");
    dired_buf->readOnly = true;

    // Position point at first entry name
    position_point_at_first_entry(dired_buf);

    // If we're looking at a file, try to highlight it in the directory listing
    if (!is_directory) {
        const char *filename = strrchr(full_path, '/');
        filename = filename ? filename + 1 : full_path;
        
        for (size_t i = 0; i < dired->count; i++) {
            if (strcmp(dired->entries[i].name, filename) == 0) {
                // Move point to this entry
                size_t line_start = line_beginning_position_at(dired_buf, i + 1); // +1 for header
                if (dired->details_hidden) {
                    dired_buf->point = line_start + 2; // After indentation
                } else {
                    dired_buf->point = line_start + dired->columns.name;
                }
                break;
            }
        }
    }

    // Switch to the new buffer
    wm->activeWindow->buffer = dired_buf;
    switchToBuffer(&bm, display_name);
    parse_and_push_dired_syntax(wm->activeWindow->buffer);

    return true;
}

/* bool dired_jump(WindowManager *wm) { */
/*     if (!wm || !wm->activeWindow || !wm->activeWindow->buffer || !wm->activeWindow->buffer->path) { */
/*         return false; */
/*     } */

/*     // Get the current buffer's path */
/*     const char *buffer_path = wm->activeWindow->buffer->path; */
    
/*     // Expand ~/ to full home directory path */
/*     char full_path[PATH_MAX]; */
/*     if (buffer_path[0] == '~' && buffer_path[1] == '/') { */
/*         const char *home = getenv("HOME"); */
/*         if (!home) { */
/*             message("Could not determine home directory"); */
/*             return false; */
/*         } */
/*         snprintf(full_path, sizeof(full_path), "%s%s", home, buffer_path + 1); */
/*     } else { */
/*         strncpy(full_path, buffer_path, sizeof(full_path) - 1); */
/*         full_path[sizeof(full_path) - 1] = '\0'; */
/*     } */

/*     // Check if it's a directory */
/*     struct stat st; */
/*     if (stat(full_path, &st) != 0 || !S_ISDIR(st.st_mode)) { */
/*         return false; */
/*     } */

/*     // Create display name (show as ~/path) */
/*     char display_name[PATH_MAX + 32]; */
/*     const char *home = getenv("HOME"); */
/*     if (home && strncmp(full_path, home, strlen(home)) == 0) { */
/*         snprintf(display_name, sizeof(display_name), "Dired: ~%s", full_path + strlen(home)); */
/*     } else { */
/*         snprintf(display_name, sizeof(display_name), "Dired: %s", full_path); */
/*     } */

/*     // Check for existing buffer first */
/*     Buffer *existing = getBuffer(&bm, display_name); */
/*     if (existing) { */
/*         wm->activeWindow->buffer = existing; */
/*         switchToBuffer(&bm, display_name); */
        
/*         // Position point at first entry name in existing buffer */
/*         position_point_at_first_entry(existing); */
/*         return true; */
/*     } */

/*     // Create new buffer */
/*     Buffer *dired_buf = generate_new_buffer(display_name, full_path, fontPath); */
/*     if (!dired_buf) { */
/*         message("Failed to create Dired buffer"); */
/*         return false; */
/*     } */

/*     // Initialize Dired structure and add to global collection */
/*     Dired *dired = direds_get_or_create(full_path, dired_buf);  // Changed from dired_new to direds_get_or_create */
/*     if (!dired) { */
/*         message("Failed to read directory"); */
/*         freeBuffer(dired_buf); */
/*         return false; */
/*     } */

/*     // Format directory listing */
/*     char *content = dired_format_all_entries(dired); */
/*     if (!content) { */
/*         message("Failed to format directory listing"); */
/*         dired_free(dired); */
/*         freeBuffer(dired_buf); */
/*         return false; */
/*     } */

/*     // Set buffer content (don't point at size) */
/*     setBufferContent(dired_buf, content, false); */
/*     free(content); */
/*     setMajorMode(dired_buf, "dired"); */
/*     dired_buf->readOnly = true; */

/*     // Position point at first entry name */
/*     position_point_at_first_entry(dired_buf); */

/*     // Switch to the new buffer */
/*     wm->activeWindow->buffer = dired_buf; */
/*     switchToBuffer(&bm, display_name); */
/*     parse_and_push_dired_syntax(wm->activeWindow->buffer); */

/*     return true; */
/* } */


// Helper function to position point at first entry name
void position_point_at_first_entry(Buffer *buffer) {
    if (!buffer || !buffer->content) return;

    // Skip header line (first line)
    size_t pos = 0;
    while (pos < buffer->size && buffer->content[pos] != '\n') {
        pos++;
    }
    if (pos < buffer->size) {
        pos++; // Move past newline
    }

    // Skip empty lines
    while (pos < buffer->size && (buffer->content[pos] == '\n' || isspace(buffer->content[pos]))) {
        pos++;
    }

    // Find start of first entry's name (after permissions, links, owner, etc.)
    if (pos < buffer->size) {
        // Skip permissions (10 chars)
        size_t name_start = pos + 10;
        
        // Skip link count (variable whitespace + digits)
        while (name_start < buffer->size && isspace(buffer->content[name_start])) {
            name_start++;
        }
        while (name_start < buffer->size && isdigit(buffer->content[name_start])) {
            name_start++;
        }
        
        // Skip owner (variable whitespace + non-whitespace)
        while (name_start < buffer->size && isspace(buffer->content[name_start])) {
            name_start++;
        }
        while (name_start < buffer->size && !isspace(buffer->content[name_start])) {
            name_start++;
        }
        
        // Skip group (variable whitespace + non-whitespace)
        while (name_start < buffer->size && isspace(buffer->content[name_start])) {
            name_start++;
        }
        while (name_start < buffer->size && !isspace(buffer->content[name_start])) {
            name_start++;
        }
        
        // Skip size (variable whitespace + digits)
        while (name_start < buffer->size && isspace(buffer->content[name_start])) {
            name_start++;
        }
        while (name_start < buffer->size && (isdigit(buffer->content[name_start]) || 
               buffer->content[name_start] == ',')) {
            name_start++;
        }
        
        // Skip timestamp (variable whitespace + date/time)
        while (name_start < buffer->size && isspace(buffer->content[name_start])) {
            name_start++;
        }
        
        // Skip month, day, time (3 space-separated fields)
        int spaces = 0;
        while (name_start < buffer->size && spaces < 3) {
            if (isspace(buffer->content[name_start])) {
                spaces++;
                if (spaces < 3) {
                    name_start++;
                    while (name_start < buffer->size && isspace(buffer->content[name_start])) {
                        name_start++;
                    }
                }
            } else {
                name_start++;
            }
        }
        
        // Skip any remaining whitespace before filename
        while (name_start < buffer->size && isspace(buffer->content[name_start])) {
            name_start++;
        }

        // Set point to start of filename
        if (name_start < buffer->size) {
            buffer->point = name_start;
            
            // Set goal column
            if (buffer->goal_column < 0) { // WRONG
                set_goal_column(buffer);
            }
            set_mark(buffer, buffer->point);
        }
    }
}

bool dired_poke(Dired *dired, Buffer *buffer, char c) {
    if (!dired || !buffer) return false;
    
    // Convert to lowercase for case-insensitive search
    c = tolower(c);
    
    // Get current line (1-based)
    int current_line = getLineNumber(buffer);
    
    // Adjust for header line (entries start at line 2)
    if (current_line < 1) current_line = 1;
    size_t current_entry = (size_t)(current_line - 1);
    
    // First try to find entries below current position
    for (size_t i = current_entry; i < dired->count; i++) {
        if (tolower(dired->entries[i].name[0]) == c) {
            // Found a match - calculate new point position
            size_t line_start = line_beginning_position(buffer) + (i + 1) * (/* approximate line length */ 80);
            
            if (dired->details_hidden) {
                buffer->point = line_start + 2; // After indentation
            } else {
                buffer->point = line_start + dired->columns.name;
            }
            return true;
        }
    }
    
    // If not found below, wrap around and try from the top
    for (size_t i = 0; i < current_entry; i++) {
        if (tolower(dired->entries[i].name[0]) == c) {
            // Found a match - calculate new point position
            size_t line_start = line_beginning_position(buffer) + (i + 1) * (/* approximate line length */ 80);
            
            if (dired->details_hidden) {
                buffer->point = line_start + 2; // After indentation
            } else {
                buffer->point = line_start + dired->columns.name;
            }
            return true;
        }
    }
    
    return false;  // No match found
}

//-- Direds

void direds_init(void) {
    direds.entries = malloc(sizeof(Dired*) * DIRED_INITIAL_CAPACITY);
    direds.count = 0;
    direds.capacity = DIRED_INITIAL_CAPACITY;
}

void direds_free(void) {
    for (size_t i = 0; i < direds.count; i++) {
        dired_free(direds.entries[i]);
    }
    free(direds.entries);
    direds.count = 0;
    direds.capacity = 0;
}

Dired* dired_for_buffer(Buffer *buffer) {
    return direds_get(buffer->path);
}


char* normalize_path_for_comparison(const char *path) {
    if (!path) return NULL;
    
    const char *home = getenv("HOME");
    if (!home) return strdup(path);
    
    size_t home_len = strlen(home);
    if (strncmp(path, home, home_len) == 0) {
        // Allocate space for "~" + remaining path + null terminator
        char *normalized = malloc(1 + strlen(path + home_len) + 1);
        if (!normalized) return NULL;
        
        normalized[0] = '~';
        strcpy(normalized + 1, path + home_len);
        return normalized;
    }
    
    return strdup(path);
}

Dired* direds_get(const char *path) {
    if (!path) return NULL;
    
    char *normalized_path = normalize_path_for_comparison(path);
    if (!normalized_path) return NULL;
    
    for (size_t i = 0; i < direds.count; i++) {
        char *normalized_dir = normalize_path_for_comparison(direds.entries[i]->directory);
        if (!normalized_dir) {
            free(normalized_path);
            return NULL;
        }
        
        bool match = (strcmp(normalized_dir, normalized_path) == 0);
        free(normalized_dir);
        
        if (match) {
            free(normalized_path);
            return direds.entries[i];
        }
    }
    
    free(normalized_path);
    return NULL;
}

/* Dired* direds_get(const char *path) { */
/*     for (size_t i = 0; i < direds.count; i++) { */
/*         if (strcmp(direds.entries[i]->directory, path) == 0) { */
/*             return direds.entries[i]; */
/*         } */
/*     } */
/*     return NULL; */
/* } */

Dired* direds_get_or_create(const char *path, Buffer *buffer) {
    // Check if we already have this dired
    Dired *existing = direds_get(path);
    if (existing) {
        return existing;
    }

    // Check if we need to grow the array
    if (direds.count >= direds.capacity) {
        size_t new_capacity = direds.capacity * 2;
        Dired **new_entries = realloc(direds.entries, sizeof(Dired*) * new_capacity);
        if (!new_entries) {
            return NULL;
        }
        direds.entries = new_entries;
        direds.capacity = new_capacity;
    }

    // Create new Dired instance
    Dired *dired = dired_new(path, buffer);
    if (!dired) {
        return NULL;
    }

    // Add to our collection
    direds.entries[direds.count++] = dired;
    return dired;
}

void direds_remove(const char *path) {
    for (size_t i = 0; i < direds.count; i++) {
        if (strcmp(direds.entries[i]->directory, path) == 0) {
            dired_free(direds.entries[i]);
            // Shift remaining entries
            for (size_t j = i; j < direds.count - 1; j++) {
                direds.entries[j] = direds.entries[j + 1];
            }
            direds.count--;
            return;
        }
    }
}

void direds_update_buffer(Buffer *buffer) {
    for (size_t i = 0; i < direds.count; i++) {
        if (direds.entries[i]->buffer == buffer) {
            dired_insert_into_buffer(buffer, direds.entries[i]);
            return;
        }
    }
}

void parse_and_push_dired_syntax(Buffer *buffer) {
    if (!buffer || !buffer->content) return;
    
    clearSyntaxArray(buffer);
    
    const char *text = buffer->content;
    size_t length = buffer->size;
    size_t line_number = 0;
    
    size_t pos = 0;
    while (pos < length) {
        // Find next line
        size_t lineStart = pos;
        while (pos < length && text[pos] != '\n') pos++;
        size_t lineEnd = pos;
        if (pos < length) pos++;

        // Skip empty lines
        if (lineStart == lineEnd) continue;
        
        // Skip leading whitespace
        size_t contentStart = lineStart;
        while (contentStart < lineEnd && isspace(text[contentStart])) contentStart++;
        
        // Header line (colored as success)
        if (line_number == 0) {
            Syntax headerSyntax = {contentStart, lineEnd, &CT.success};
            insertSyntax(&buffer->syntaxArray, headerSyntax);
            line_number++;
            continue;
        }
        
        // Skip empty lines after whitespace
        if (contentStart == lineEnd) continue;

        // 1. Parse permissions (10 chars)
        size_t permEnd = contentStart + 10;
        if (permEnd > lineEnd) permEnd = lineEnd;
        
        for (size_t i = contentStart; i < permEnd; i++) {
            Color *color = NULL;
            char c = text[i];
            
            if (i == contentStart) { // File type
                switch (c) {
                    case 'd': color = &CT.diredfl_dir_priv; break;
                    case 'l': color = &CT.diredfl_dir_priv; break;
                    case '-': color = &CT.diredfl_no_priv; break;
                    default: break;
                }
            } else { // Permissions
                switch (c) {
                    case 'r': color = &CT.diredfl_read_priv; break;
                    case 'w': color = &CT.diredfl_write_priv; break;
                    case 'x': color = &CT.diredfl_exec_priv; break;
                    case '-': color = &CT.diredfl_no_priv; break;
                    default: break;
                }
            }
            
            if (color) {
                Syntax s = {i, i+1, color};
                insertSyntax(&buffer->syntaxArray, s);
            }
        }

        // 2. Parse link count
        size_t linksStart = permEnd;
        while (linksStart < lineEnd && text[linksStart] == ' ') linksStart++;
        
        size_t linksEnd = linksStart;
        while (linksEnd < lineEnd && isdigit(text[linksEnd])) linksEnd++;
        
        if (linksEnd > linksStart) {
            Syntax s = {linksStart, linksEnd, &CT.diredfl_number};
            insertSyntax(&buffer->syntaxArray, s);
        }

        // 3. Parse owner
        size_t ownerStart = linksEnd;
        while (ownerStart < lineEnd && text[ownerStart] == ' ') ownerStart++;
        
        size_t ownerEnd = ownerStart;
        while (ownerEnd < lineEnd && !isspace(text[ownerEnd])) ownerEnd++;
        
        if (ownerEnd > ownerStart) {
            Syntax s = {ownerStart, ownerEnd, &CT.text};
            insertSyntax(&buffer->syntaxArray, s);
        }

        // 4. Parse group
        size_t groupStart = ownerEnd;
        while (groupStart < lineEnd && text[groupStart] == ' ') groupStart++;
        
        size_t groupEnd = groupStart;
        while (groupEnd < lineEnd && !isspace(text[groupEnd])) groupEnd++;
        
        if (groupEnd > groupStart) {
            Syntax s = {groupStart, groupEnd, &CT.text};
            insertSyntax(&buffer->syntaxArray, s);
        }

        // 5. Parse size
        size_t sizeStart = groupEnd;
        while (sizeStart < lineEnd && text[sizeStart] == ' ') sizeStart++;
        
        size_t sizeEnd = sizeStart;
        while (sizeEnd < lineEnd && (isdigit(text[sizeEnd]) || text[sizeEnd] == ',')) sizeEnd++;
        
        if (sizeEnd > sizeStart) {
            Syntax s = {sizeStart, sizeEnd, &CT.diredfl_number};
            insertSyntax(&buffer->syntaxArray, s);
        }

        // 6. Parse timestamp (month day time)
        size_t timeStart = sizeEnd;
        while (timeStart < lineEnd && text[timeStart] == ' ') timeStart++;
        
        size_t timeEnd = timeStart;
        int spaces = 0;
        while (timeEnd < lineEnd && spaces < 3) {
            if (isspace(text[timeEnd])) {
                spaces++;
                if (spaces < 3) {
                    timeEnd++;
                    while (timeEnd < lineEnd && isspace(text[timeEnd])) timeEnd++;
                }
            } else {
                timeEnd++;
            }
        }

        // Handle time format (HH:MM)
        if (timeEnd < lineEnd && timeEnd + 5 <= lineEnd &&
            isdigit(text[timeEnd]) && isdigit(text[timeEnd+1]) &&
            text[timeEnd+2] == ':' &&
            isdigit(text[timeEnd+3]) && isdigit(text[timeEnd+4])) {
            timeEnd += 5;
        }

        if (timeEnd > timeStart) {
            Syntax s = {timeStart, timeEnd, &CT.diredfl_date_time};
            insertSyntax(&buffer->syntaxArray, s);
        }

        // 7. Parse filename - COMPLETE REWRITE FOR PROPER DOT HANDLING
        size_t nameStart = timeEnd;
        while (nameStart < lineEnd && text[nameStart] == ' ') nameStart++;
        
        if (nameStart < lineEnd) {
            bool isDir = (text[contentStart] == 'd');
            bool isLink = (text[contentStart] == 'l');
            bool isRegularFile = (text[contentStart] == '-');
            size_t nameEnd = lineEnd;
            
            // Handle symlink target
            if (isLink) {
                for (size_t i = nameStart; i < lineEnd; i++) {
                    if (i + 4 <= lineEnd && strncmp(text + i, " -> ", 4) == 0) {
                        nameEnd = i;
                        break;
                    }
                }
            }
            
            // Check for ### wrapped filenames
            bool isCommented = (nameEnd - nameStart >= 2 &&
                              text[nameStart] == '#' &&
                              text[nameEnd-1] == '#');
            
            // Determine base color for filename
            Color *baseColor = isCommented ? &CT.comment :
                             isDir ? &CT.diredfl_dir_name : &CT.text;
            
            if (isRegularFile && !isCommented) {
                // Find last dot in filename (search backwards from end)
                size_t lastDot = nameEnd;
                while (lastDot > nameStart && text[lastDot-1] != '.') lastDot--;
                
                if (lastDot > nameStart) { // Found a dot
                    // Color the base name (before dot)
                    if (lastDot > nameStart) {
                        Syntax baseSyntax = {nameStart, lastDot-1, baseColor};
                        insertSyntax(&buffer->syntaxArray, baseSyntax);
                    }
                    
                    // Color the dot and extension (including the dot)
                    Syntax dotSyntax = {lastDot-1, nameEnd, &CT.diredfl_file_suffix};
                    insertSyntax(&buffer->syntaxArray, dotSyntax);
                } else {
                    // No dot found - color entire name with base color
                    Syntax nameSyntax = {nameStart, nameEnd, baseColor};
                    insertSyntax(&buffer->syntaxArray, nameSyntax);
                }
            } else {
                // Not a regular file or is commented - color entire name with base color
                Syntax nameSyntax = {nameStart, nameEnd, baseColor};
                insertSyntax(&buffer->syntaxArray, nameSyntax);
            }
            
            // Handle symlink target if exists
            if (isLink) {
                for (size_t i = nameStart; i < lineEnd; i++) {
                    if (i + 4 <= lineEnd && strncmp(text + i, " -> ", 4) == 0) {
                        size_t targetStart = i + 4;
                        if (targetStart < lineEnd) {
                            Syntax targetSyntax = {targetStart, lineEnd, &CT.string};
                            insertSyntax(&buffer->syntaxArray, targetSyntax);
                        }
                        break;
                    }
                }
            }
        }
        
        line_number++;
    }
}








char* dired_get_current_entry_path(Dired *dired, Buffer *buffer) {
    if (!dired || !buffer) return NULL;
    
    // Get current line (1-based)
    int current_line = getLineNumber(buffer);
    
    // Header is line 1, first entry is line 2
    if (current_line < 2) return NULL; // On header or before first entry
    size_t current_entry = (size_t)(current_line - 2); // Convert to 0-based index
    
    // Check if we're on a valid entry
    if (current_entry >= dired->count) return NULL;
    
    // Build full path
    char *path = malloc(PATH_MAX);
    if (!path) return NULL;
    
    snprintf(path, PATH_MAX, "%s/%s", dired->directory, dired->entries[current_entry].name);
    
    // Normalize path (replace home with ~)
    const char *home = getenv("HOME");
    if (home && strncmp(path, home, strlen(home)) == 0) {
        char *normalized = malloc(PATH_MAX);
        if (!normalized) {
            free(path);
            return NULL;
        }
        snprintf(normalized, PATH_MAX, "~%s", path + strlen(home));
        free(path);
        path = normalized;
    }
    
    return path;
}

void dired_find_file_other_window(BufferManager *bm, WindowManager *wm, Dired *dired) {
    split_window_right(wm, &wm->activeWindow->parameters);
    other_window(wm, 1);
    dired_find_file(bm, wm, dired);
}

void dired_toggle(WindowManager *wm) {
    if (wm->count == 1) {
        split_window_right(wm, &wm->activeWindow->parameters);
        other_window(wm, 1);
        dired_jump(wm);
    } else {
        delete_window(wm);
    }
}

void dired_find_file(BufferManager *bm, WindowManager *wm, Dired *dired) {
    if (!bm || !wm || !dired || !wm->activeWindow || !wm->activeWindow->buffer) {
        return;
    }
    
    // Get the path of the current entry
    char *entry_path = dired_get_current_entry_path(dired, wm->activeWindow->buffer);
    if (!entry_path) {
        message("No file at point");
        return;
    }
    
    // Set up minibuffer
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");
    
    // Clear minibuffer and set its content
    setBufferContent(minibuffer, entry_path, true);
    free(entry_path);
    find_file(bm, wm);
}


char* get_parent_directory(const char *path) {
    if (!path || !*path) return NULL;
    
    // Make a copy we can modify
    char *path_copy = strdup(path);
    if (!path_copy) return NULL;
    
    // Remove trailing slashes
    while (strlen(path_copy) > 1 && path_copy[strlen(path_copy)-1] == '/') {
        path_copy[strlen(path_copy)-1] = '\0';
    }
    
    // Find last slash
    char *last_slash = strrchr(path_copy, '/');
    if (!last_slash) {
        free(path_copy);
        return strdup("/"); // Already at root
    }
    
    // Handle root directory case
    if (last_slash == path_copy) {
        free(path_copy);
        return strdup("/");
    }
    
    // Truncate at last slash
    *last_slash = '\0';
    
    // For paths that are now empty, return root
    if (strlen(path_copy) == 0) {
        free(path_copy);
        return strdup("/");
    }
    
    return path_copy;
}

void dired_up_directory(BufferManager *bm, WindowManager *wm, Dired *dired) {
    if (!bm || !wm || !dired || !wm->activeWindow || !wm->activeWindow->buffer) {
        return;
    }
    
    // Get parent directory path
    char *parent_dir = get_parent_directory(dired->directory);
    if (!parent_dir) {
        message("Couldn't get parent directory");
        return;
    }
    
    // Set up minibuffer
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");
    
    // Clear minibuffer and set its content
    setBufferContent(minibuffer, parent_dir, true);
    free(parent_dir);
    
    // Set prompt
    free(prompt->content);
    prompt->content = strdup("Find file: ");
    
    // Switch to minibuffer and call find_file
    switchToBuffer(bm, "minibuffer");
    find_file(bm, wm);
}


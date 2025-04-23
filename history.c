#include "history.h"
#include "globals.h"
#include "editor.h"
#include "sxp.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Goated module btw
// TODO savehist_mode
// TODO when we close glemax save all the named histories
// into ~/.config/glemax/named_histories file
// or ~/.glemax.d/named_histories
// or .named_histories

static History* get_history(NamedHistories *nh, const char *name) {
    for (int i = 0; i < nh->count; ++i) {
        if (strcmp(nh->names[i], name) == 0) {
            return nh->histories[i];
        }
    }
    return NULL;
}

void add_to_history(NamedHistories *nh, const char *name, const char *input) {
    History *history = get_history(nh, name);
    if (!history) {
        // NOTE If history doesn't exist, create it
        if (nh->count >= nh->capacity) {
            int newCap = nh->capacity > 0 ? nh->capacity * 2 : 2;
            History **newHistories = realloc(nh->histories, newCap * sizeof(History*));
            char **newNames = realloc(nh->names, newCap * sizeof(char*));
            if (!newHistories || !newNames) return;
            nh->histories = newHistories;
            nh->names = newNames;
            nh->capacity = newCap;
        }
        history = calloc(1, sizeof(History));
        nh->histories[nh->count] = history;
        nh->names[nh->count] = strdup(name);
        nh->count++;
    }
    if (history->size > 0 && strcmp(history->entries[history->size - 1], input) == 0) {
        // If the new input is the same as the last entry, do not add it
        return;
    }
    if (history->size >= history->capacity) {
        // Ensure there is enough capacity to add a new entry
        int newCap = history->capacity > 0 ? history->capacity * 2 : 4;
        char **newEntries = realloc(history->entries, newCap * sizeof(char*));
        if (!newEntries) return;
        history->entries = newEntries;
        history->capacity = newCap;
    }
    // Add the new entry
    history->entries[history->size++] = strdup(input);
    history->index = history->size;
}


const char* previous_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm) {
    History *history = get_history(nh, name);
    if (!history || history->size == 0) return NULL;

    if (history->index == history->size) {
        free(history->currentInput);
        history->currentInput = strdup(minibuffer->content);
    }

    if (history->index > 0) {
        history->index--;
        setBufferContent(minibuffer, history->entries[history->index], true);
        return history->entries[history->index];
    }
    
    message("Beginning of history; no preceding item");
    return NULL;
}

const char* next_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm) {
    History *history = get_history(nh, name);
    if (!history || history->size == 0) return NULL;

    if (history->index == history->size) {
        message("End of history; no next item");
        return NULL;
    }

    history->index++;
    if (history->index >= history->size) {
        history->index = history->size;
        if (history->currentInput) {
            setBufferContent(minibuffer, history->currentInput, true);
        } else {
            cleanBuffer(bm, "minibuffer");
        }
        return NULL;
    }

    setBufferContent(minibuffer, history->entries[history->index], true);
    return history->entries[history->index];
}

/* const char* previous_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm) { */
/*     History *history = get_history(nh, name); */
/*     if (!history) return NULL; */

/*     if (history->index == history->size) { */
/*         free(history->currentInput);  // Free the previous input if it exists */
/*         history->currentInput = strdup(minibuffer->content);  // Save the new input */
/*     } */

/*     if (history->index > 0) { */
/*         history->index--; */
/*         setBufferContent(minibuffer, history->entries[history->index], true); */
/*         return history->entries[history->index]; */
/*     } else { */
/*         message("Beginning of history; no preceding item"); */
/*         return NULL; */
/*     } */
/* } */


/* const char* next_history_element(NamedHistories *nh, const char *name, Buffer *minibuffer, BufferManager *bm) { */
/*     History *history = get_history(nh, name); */
/*     if (!history) return NULL; */

/*     if (history->index == history->size) { */
/*         message("End of defaults; no next item"); */
/*         return NULL; */
/*     } */

/*     history->index++; */
/*     if (history->index >= history->size) { */
/*         history->index = history->size; */
/*         if (history->currentInput) { */
/*             setBufferContent(minibuffer, history->currentInput, true);  // Restore the original content */
/*         } else { */
/*             message("No more next history entries."); */
/*         } */
/*         return NULL; */
/*     } */


/*     setBufferContent(minibuffer, history->entries[history->index], true); */
/*     return history->entries[history->index]; */
/* } */

void resetHistoryIndex(NamedHistories *nh, const char *name) {
    History *history = get_history(nh, name);
    if (history) {
        history->index = history->size;
    }
}


void savehist_load() {
    SxpValue* root = sxp_parse_file(state_path);
    if (!root || root->type != SXP_LIST) {
        if (root) sxp_free_value(root);
        return;
    }
    
    size_t num_histories = sxp_list_length(root);
    for (size_t i = 0; i < num_histories; i++) {
        SxpValue* history_entry = sxp_get_list_item(root, i);
        if (!history_entry || history_entry->type != SXP_LIST) continue;
        
        size_t entry_len = sxp_list_length(history_entry);
        if (entry_len < 1) continue;
        
        SxpValue* name_value = sxp_get_list_item(history_entry, 0);
        if (!name_value || name_value->type != SXP_SYMBOL) continue;
        
        const char* history_name = sxp_get_symbol(name_value);
        
        // Create history if it doesn't exist
        History *history = get_history(&nh, history_name);
        if (!history) {
            add_to_history(&nh, history_name, ""); // Add dummy entry to create history
            history = get_history(&nh, history_name);
            history->size = 0; // Reset size since we added a dummy entry
        }
        
        // Clear any existing entries
        for (int j = 0; j < history->size; j++) {
            free(history->entries[j]);
        }
        history->size = 0;
        
        // Load new entries
        for (size_t j = 1; j < entry_len; j++) {
            SxpValue* item = sxp_get_list_item(history_entry, j);
            if (!item || item->type != SXP_STRING) continue;
            
            const char* history_item = sxp_get_string(item);
            if (history->size >= history->capacity) {
                int newCap = history->capacity > 0 ? history->capacity * 2 : 4;
                char **newEntries = realloc(history->entries, newCap * sizeof(char*));
                if (!newEntries) continue;
                history->entries = newEntries;
                history->capacity = newCap;
            }
            history->entries[history->size++] = strdup(history_item);
        }
        
        // Initialize navigation state
        history->index = history->size;
        free(history->currentInput);
        history->currentInput = NULL;
    }
    
    sxp_free_value(root);
}


/* void savehist_load() { */
/*     SxpValue* root = sxp_parse_file(state_path); */
/*     if (!root || root->type != SXP_LIST) { */
/*         if (root) sxp_free_value(root); */
/*         return; */
/*     } */
    
/*     size_t num_histories = sxp_list_length(root); */
/*     for (size_t i = 0; i < num_histories; i++) { */
/*         SxpValue* history_entry = sxp_get_list_item(root, i); */
/*         if (!history_entry || history_entry->type != SXP_LIST) continue; */
        
/*         size_t entry_len = sxp_list_length(history_entry); */
/*         if (entry_len < 1) continue; */
        
/*         SxpValue* name_value = sxp_get_list_item(history_entry, 0); */
/*         if (!name_value || name_value->type != SXP_SYMBOL) continue; */
        
/*         const char* history_name = sxp_get_symbol(name_value); */
        
/*         for (size_t j = 1; j < entry_len; j++) { */
/*             SxpValue* item = sxp_get_list_item(history_entry, j); */
/*             if (!item || item->type != SXP_STRING) continue; */
            
/*             const char* history_item = sxp_get_string(item); */
/*             add_to_history(&nh, history_name, history_item); */
/*         } */
        
/*         // Reset the history index after loading */
/*         History *history = get_history(&nh, history_name); */
/*         if (history) { */
/*             history->index = history->size; */
/*         } */
/*     } */
    
/*     sxp_free_value(root); */
/* } */

/* void savehist_load() { */
/*     SxpValue* root = sxp_parse_file(state_path); */
/*     if (!root || root->type != SXP_LIST) { */
/*         if (root) sxp_free_value(root); */
/*         return; */
/*     } */
    
/*     size_t num_histories = sxp_list_length(root); */
/*     for (size_t i = 0; i < num_histories; i++) { */
/*         SxpValue* history_entry = sxp_get_list_item(root, i); */
/*         if (!history_entry || history_entry->type != SXP_LIST) continue; */
        
/*         size_t entry_len = sxp_list_length(history_entry); */
/*         if (entry_len < 1) continue; */
        
/*         SxpValue* name_value = sxp_get_list_item(history_entry, 0); */
/*         if (!name_value || name_value->type != SXP_SYMBOL) continue; */
        
/*         const char* history_name = sxp_get_symbol(name_value); */
        
/*         for (size_t j = 1; j < entry_len; j++) { */
/*             SxpValue* item = sxp_get_list_item(history_entry, j); */
/*             if (!item || item->type != SXP_STRING) continue; */
            
/*             const char* history_item = sxp_get_string(item); */
/*             add_to_history(&nh, history_name, history_item); */
/*         } */
/*     } */
    
/*     sxp_free_value(root); */
/* } */


void savehist_save() {
    sxp_init(&sxpBuffer);

    sxp_start_list(&sxpBuffer);
    sxp_newline(&sxpBuffer);

    for (int i = 0; i < nh.count; i++) {
        History *h = nh.histories[i];
        
        // First ensure we're at the correct indentation level
        if (!sxpBuffer.at_line_start) {
            sxp_newline(&sxpBuffer);
        }
        
        // Start a list for this history entry at the correct indentation
        sxp_start_list(&sxpBuffer);
        sxp_add_symbol(&sxpBuffer, nh.names[i]);
        
        if (h->size == 0) {
            // Empty list - compact format
            sxp_end_list(&sxpBuffer);
        } else if (h->size == 1) {
            // Single item - stays on same line
            sxp_add_string(&sxpBuffer, h->entries[0]);
            sxp_end_list(&sxpBuffer);
        } else {
            // Multiple items - each on its own line
            sxp_newline(&sxpBuffer);
            for (int j = 0; j < h->size; j++) {
                sxp_add_string(&sxpBuffer, h->entries[j]);
                sxp_newline(&sxpBuffer);
            }
            sxp_end_list(&sxpBuffer);
        }
        
        sxp_newline(&sxpBuffer);
    }

    sxp_end_list(&sxpBuffer);
    sxp_newline(&sxpBuffer);

    FILE *fp = fopen(state_path, "w");
    if (fp) {
        sxp_write_to_file(&sxpBuffer, fp);
        fclose(fp);
    }

    sxp_free(&sxpBuffer);
}


void savehist_clear() {
    for (int i = 0; i < nh.count; i++) {
        free(nh.names[i]);
        History *h = nh.histories[i];
        if (h) {
            for (int j = 0; j < h->size; j++) {
                free(h->entries[j]);
            }
            free(h->entries);
            free(h->currentInput);
            free(h);
        }
    }
    free(nh.histories);
    free(nh.names);
    nh.histories = NULL;
    nh.names = NULL;
    nh.count = 0;
    nh.capacity = 0;
}


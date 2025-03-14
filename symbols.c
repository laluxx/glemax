#include "symbols.h"
#include "buffer.h"
#include <stdio.h>

Symbols symbols = {0};

void initSymbols() {
    symbols.array = NULL;
    symbols.count = 0;
    symbols.capacity = 0;
}

void addSymbol(Symbol sym) {
    if (symbols.count == symbols.capacity) {
        size_t new_capacity = symbols.capacity == 0 ? 10 : symbols.capacity * 2;
        Symbol *new_array = realloc(symbols.array, new_capacity * sizeof(Symbol));
        if (!new_array) {
            perror("realloc");
            exit(EXIT_FAILURE);
        }
        symbols.array = new_array;
        symbols.capacity = new_capacity;
    }
    symbols.array[symbols.count++] = sym;
}

void freeSymbols() {
    for (size_t i = 0; i < symbols.count; i++) {
        free(symbols.array[i].name);
        free(symbols.array[i].section_name);
        free(symbols.array[i].source);
    }
    free(symbols.array);
    symbols.array = NULL;
    symbols.count = symbols.capacity = 0;
}

static const char *symbolTypeToString(SymbolType type) {
    switch (type) {
    case SYMBOL_FUNCTION: return "FUNC";
    case SYMBOL_VARIABLE: return "VAR";
    case SYMBOL_FILE:     return "FILE";
    case SYMBOL_SECTION:  return "SECTION";
    default:
        return "OTHER";
    }
}

static SymbolType determine_symbol_type(asymbol *sym) {
    if (sym->flags & BSF_FUNCTION) {
        return SYMBOL_FUNCTION;
    } else if (sym->flags & BSF_OBJECT) {
        return SYMBOL_VARIABLE;
    } else if (sym->flags & BSF_FILE) {
        return SYMBOL_FILE;
    } else if (sym->flags & BSF_SECTION_SYM) {
        return SYMBOL_SECTION;
    } else {
        return SYMBOL_OTHER;
    }
}

void load_debug_symbols(const char *filename) {
    // Open the binary file using BFD
    bfd *abfd = bfd_openr(filename, NULL);
    if (!abfd) {
        perror("bfd_openr");
        return;
    }

    // Check if the file is in a valid object format
    if (!bfd_check_format(abfd, bfd_object)) {
        fprintf(stderr, "Invalid format: %s\n", filename);
        bfd_close(abfd);
        return;
    }

    // Get the required storage for the symbol table
    long storage_needed = bfd_get_symtab_upper_bound(abfd);
    if (storage_needed <= 0) {
        fprintf(stderr, "No symbols in %s\n", filename);
        bfd_close(abfd);
        return;
    }

    // Allocate memory for the symbol table
    asymbol **symbol_table = malloc(storage_needed);
    if (!symbol_table) {
        perror("malloc");
        bfd_close(abfd);
        return;
    }

    // Canonicalize the symbol table
    long num_symbols = bfd_canonicalize_symtab(abfd, symbol_table);
    if (num_symbols < 0) {
        fprintf(stderr, "Failed to read symbols from %s\n", filename);
        free(symbol_table);
        bfd_close(abfd);
        return;
    }

    // Process each symbol
    for (long i = 0; i < num_symbols; i++) {
        asymbol *sym = symbol_table[i];
        Symbol new_sym = {0};

        // Copy symbol name
        new_sym.name = strdup(bfd_asymbol_name(sym));
        if (!new_sym.name) {
            perror("strdup");
            continue;
        }

        // Copy symbol value (address)
        new_sym.value = sym->value;

        // Copy symbol size (if available)
        new_sym.size = sym->section ? sym->section->size : 0;

        // Copy symbol flags
        new_sym.flags = sym->flags;

        // Copy section name (if available)
        new_sym.section_name =
            sym->section ? strdup(bfd_section_name(sym->section)) : NULL;

        // Determine symbol type
        new_sym.type = determine_symbol_type(sym);

        // Copy source file/library
        new_sym.source = strdup(filename);
        if (!new_sym.source) {
            perror("strdup");
            free(new_sym.name);
            free(new_sym.section_name);
            continue;
        }

        // Add the symbol to the global array
        addSymbol(new_sym);
    }

    // Clean up
    free(symbol_table);
    bfd_close(abfd);
}

void print_loaded_symbols() {
    for (size_t i = 0; i < symbols.count; i++) {
        printf("Symbol: %s\n", symbols.array[i].name);
        printf("  Address: 0x%lx\n", symbols.array[i].value);
        printf("  Size: %lu\n", symbols.array[i].size);
        printf("  Type: %d\n", symbols.array[i].type);
        printf("  Section: %s\n", symbols.array[i].section_name
               ? symbols.array[i].section_name
               : "N/A");
        printf("  Source: %s\n", symbols.array[i].source);
        printf("\n");
    }
}

char *findSymbolsByName(const char *name_pattern) {
    if (symbols.count == 0) {
        char *result = malloc(64);
        if (!result)
            return NULL;
        snprintf(result, 64,
                 "No symbols loaded. Call load_debug_symbols() first.\n");
        return result;
    }

    // First pass: Determine maximum name length for alignment
    size_t max_name_len = 0;
    for (size_t i = 0; i < symbols.count; i++) {
        if (strstr(symbols.array[i].name, name_pattern)) {
            size_t len = strlen(symbols.array[i].name);
            if (len > max_name_len)
                max_name_len = len;
        }
    }

    if (max_name_len == 0) {
        char *result = malloc(32);
        if (!result)
            return NULL;
        snprintf(result, 32, "No matching symbols found.\n");
        return result;
    }

    // Second pass: Build the result string
    size_t buffer_size = 1024; // Initial buffer size
    char *buffer = malloc(buffer_size);
    if (!buffer)
        return NULL;
    buffer[0] = '\0'; // Initialize as an empty string

    int found = 0;
    for (size_t i = 0; i < symbols.count; i++) {
        if (strstr(symbols.array[i].name, name_pattern)) {
            char addr_buf[20], size_buf[20], flags_str[256] = {0};

            // Format address and size
            snprintf(addr_buf, sizeof(addr_buf), "0x%08lx",
                     (unsigned long)symbols.array[i].value);
            snprintf(size_buf, sizeof(size_buf), "%lu",
                     (unsigned long)symbols.array[i].size);

            // Build flags string
            int pos = 0;
            if (symbols.array[i].flags & BSF_FUNCTION)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "FUNC ");
            if (symbols.array[i].flags & BSF_GLOBAL)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "GLOBAL ");
            if (symbols.array[i].flags & BSF_LOCAL)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "LOCAL ");
            if (symbols.array[i].flags & BSF_WEAK)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "WEAK ");
            if (symbols.array[i].flags & BSF_DEBUGGING)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "DEBUG ");
            if (symbols.array[i].flags & BSF_DYNAMIC)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "DYNAMIC ");
            if (symbols.array[i].flags & BSF_OBJECT)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "OBJECT ");
            if (symbols.array[i].flags & BSF_CONSTRUCTOR)
                pos +=
                    snprintf(flags_str + pos, sizeof(flags_str) - pos, "CONSTRUCTOR ");
            if (symbols.array[i].flags & BSF_WARNING)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "WARNING ");
            if (symbols.array[i].flags & BSF_INDIRECT)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "INDIRECT ");
            if (symbols.array[i].flags & BSF_FILE)
                pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "FILE ");
            if (pos > 0)
                flags_str[pos - 1] = '\0'; // Remove trailing space

            // Format the line
            char line[512];
            snprintf(line, sizeof(line), "%-*s  %10s  %10s  %20s  %s\n",
                     (int)max_name_len, symbols.array[i].name, addr_buf, size_buf,
                     symbols.array[i].section_name ? symbols.array[i].section_name
                     : "",
                     flags_str);

            // Check if we need to resize the buffer
            size_t line_len = strlen(line);
            size_t buffer_len = strlen(buffer);
            if (buffer_len + line_len + 1 > buffer_size) {
                buffer_size = buffer_len + line_len + 1024; // Add extra space
                char *new_buffer = realloc(buffer, buffer_size);
                if (!new_buffer) {
                    free(buffer);
                    return NULL;
                }
                buffer = new_buffer;
            }

            // Append the line to the buffer
            strcat(buffer, line);
            found++;
        }
    }

    // Append summary line
    char summary[64];
    snprintf(summary, sizeof(summary), "Found %d matching symbols.\n", found);
    size_t buffer_len = strlen(buffer);
    size_t summary_len = strlen(summary);
    if (buffer_len + summary_len + 1 > buffer_size) {
        buffer_size = buffer_len + summary_len + 1;
        char *new_buffer = realloc(buffer, buffer_size);
        if (!new_buffer) {
            free(buffer);
            return NULL;
        }
        buffer = new_buffer;
    }
    strcat(buffer, summary);

    return buffer;
}

    // TODO Buffer agnostic vertico system
    /* void findSymbolsByName(const char *name_pattern) { */
    /*     if (symbols.count == 0) { */
    /*         message("No symbols loaded. Call print_debug_symbols() first."); */
    /*         return; */
    /*     } */

    /*     // First pass: find the maximum name length */
    /*     size_t max_name_len = 0; */
    /*     for (size_t i = 0; i < symbols.count; i++) { */
    /*         if (strstr(symbols.array[i].name, name_pattern) != NULL) { */
    /*             size_t len = strlen(symbols.array[i].name); */
    /*             if (len > max_name_len) */
    /*                 max_name_len = len; */
    /*         } */
    /*     } */

    /*     if (max_name_len == 0) { */
    /*         printf("No matching symbols found.\n"); */
    /*         return; */
    /*     } */

    /*     // Second pass: print the symbols */
    /*     int found = 0; */
    /*     for (size_t i = 0; i < symbols.count; i++) { */
    /*         if (strstr(symbols.array[i].name, name_pattern) != NULL) { */
    /*             // Prepare address string */
    /*             char addr_buf[20]; */
    /*             snprintf(addr_buf, sizeof(addr_buf), "0x%08lx", */
    /*                      (unsigned long)symbols.array[i].value); */

    /*             // Prepare size string */
    /*             char size_buf[20]; */
    /*             snprintf(size_buf, sizeof(size_buf), "%lu", */
    /*                      (unsigned long)symbols.array[i].size); */

    /*             // Prepare flags string */
    /*             char flags_str[256] = {0}; */
    /*             int pos = 0; */
    /*             if (symbols.array[i].flags & BSF_FUNCTION) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "FUNC "); */
    /*             if (symbols.array[i].flags & BSF_GLOBAL) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "GLOBAL "); */
    /*             if (symbols.array[i].flags & BSF_LOCAL) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "LOCAL "); */
    /*             if (symbols.array[i].flags & BSF_WEAK) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "WEAK "); */
    /*             if (symbols.array[i].flags & BSF_DEBUGGING) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "DEBUG "); */
    /*             if (symbols.array[i].flags & BSF_DYNAMIC) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "DYNAMIC "); */
    /*             if (symbols.array[i].flags & BSF_OBJECT) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "OBJECT "); */
    /*             if (symbols.array[i].flags & BSF_CONSTRUCTOR) */
    /*                 pos += */
    /*                     snprintf(flags_str + pos, sizeof(flags_str) - pos, "CONSTRUCTOR "); */
    /*             if (symbols.array[i].flags & BSF_WARNING) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "WARNING "); */
    /*             if (symbols.array[i].flags & BSF_INDIRECT) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "INDIRECT "); */
    /*             if (symbols.array[i].flags & BSF_FILE) */
    /*                 pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "FILE "); */
    /*             if (pos > 0) */
    /*                 flags_str[pos - 1] = '\0'; // Remove trailing space */

    /*             // Print the line */
    /*             printf("%-*s  %10s  %10s  %20s  %s\n", (int)max_name_len, */
    /*                    symbols.array[i].name, addr_buf, size_buf, */
    /*                    symbols.array[i].section_name ? symbols.array[i].section_name : "", */
    /*                    flags_str); */
    /*             found++; */
    /*         } */
    /*     } */

    /*     printf("Found %d matching symbols.\n", found); */
    /* } */

    void findSymbolsByAddressRange(bfd_vma start_addr, bfd_vma end_addr) {
        if (symbols.count == 0) {
            message("No symbols loaded. Call print_debug_symbols() first.");
            return;
        }

        // First pass: find the maximum name length
        size_t max_name_len = 0;
        for (size_t i = 0; i < symbols.count; i++) {
            if (symbols.array[i].value >= start_addr &&
                symbols.array[i].value <= end_addr) {
                size_t len = strlen(symbols.array[i].name);
                if (len > max_name_len)
                    max_name_len = len;
            }
        }

        if (max_name_len == 0) {
            printf("No symbols found in this address range.\n");
            return;
        }

        // Second pass: print the symbols
        int found = 0;
        for (size_t i = 0; i < symbols.count; i++) {
            if (symbols.array[i].value >= start_addr &&
                symbols.array[i].value <= end_addr) {
                // Prepare address string
                char addr_buf[20];
                snprintf(addr_buf, sizeof(addr_buf), "0x%08lx",
                         (unsigned long)symbols.array[i].value);

                // Prepare size string
                char size_buf[20];
                snprintf(size_buf, sizeof(size_buf), "%lu",
                         (unsigned long)symbols.array[i].size);

                // Prepare flags string
                char flags_str[256] = {0};
                int pos = 0;
                if (symbols.array[i].flags & BSF_FUNCTION)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "FUNC ");
                if (symbols.array[i].flags & BSF_GLOBAL)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "GLOBAL ");
                if (symbols.array[i].flags & BSF_LOCAL)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "LOCAL ");
                if (symbols.array[i].flags & BSF_WEAK)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "WEAK ");
                if (symbols.array[i].flags & BSF_DEBUGGING)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "DEBUG ");
                if (symbols.array[i].flags & BSF_DYNAMIC)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "DYNAMIC ");
                if (symbols.array[i].flags & BSF_OBJECT)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "OBJECT ");
                if (symbols.array[i].flags & BSF_CONSTRUCTOR)
                    pos +=
                        snprintf(flags_str + pos, sizeof(flags_str) - pos, "CONSTRUCTOR ");
                if (symbols.array[i].flags & BSF_WARNING)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "WARNING ");
                if (symbols.array[i].flags & BSF_INDIRECT)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "INDIRECT ");
                if (symbols.array[i].flags & BSF_FILE)
                    pos += snprintf(flags_str + pos, sizeof(flags_str) - pos, "FILE ");
                if (pos > 0)
                    flags_str[pos - 1] = '\0'; // Remove trailing space

                // Print the line
                printf("%-*s  %10s  %10s  %20s  %s\n", (int)max_name_len,
                       symbols.array[i].name, addr_buf, size_buf,
                       symbols.array[i].section_name ? symbols.array[i].section_name : "",
                       flags_str);
                found++;
            }
        }

        printf("Found %d symbols in this address range.\n", found);
    }

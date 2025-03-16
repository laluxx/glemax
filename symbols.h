#ifndef SYMBOLS_H
#define SYMBOLS_H

#define PACKAGE "your_project"
#define PACKAGE_VERSION "1.0"

#include <bfd.h>

#undef PACKAGE
#undef PACKAGE_VERSION

typedef enum {
    SYMBOL_FUNCTION,
    SYMBOL_VARIABLE,
    SYMBOL_FILE,
    SYMBOL_SECTION,
    SYMBOL_OTHER
} SymbolType;

typedef struct {
    char *name;         // Symbol name
    bfd_vma value;      // Symbol address
    bfd_vma size;       // Symbol size
    unsigned int flags; // Symbol flags
    char *section_name; // Section name
    SymbolType type;    // Symbol type
    char *source;       // Source file/library
} Symbol;

typedef struct {
    Symbol *array;   // Array of symbols
    size_t count;    // Number of symbols
    size_t capacity; // Array capacity
} Symbols;

extern Symbols symbols;

// Those 3 functions could be 1
void initSymbols();
void addSymbol(Symbol sym);
void load_debug_symbols(const char *filename);

void print_loaded_symbols();
char *findSymbolsByName(const char *name_pattern);
void findSymbolsByAddressRange(bfd_vma start_addr, bfd_vma end_addr);

void freeSymbols();


#endif /* SYMBOLS_H */

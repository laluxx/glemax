#ifndef SYNTAX_H
#define SYNTAX_H

#include <tree_sitter/api.h>
#include <uthash.h>
#include "buffer.h"

// NOTE Syntax and SyntaxArray structs are defined inside buffer.h
// to avoid circular header dependency

typedef struct {
    const char *nodeType; // Key
    Color *color;         // Value
    UT_hash_handle hh;    // Hash table handle
} NodeColorMap;

extern NodeColorMap *nodeColorMap;

void addNodeColorMapping(NodeColorMap **map, const char *nodeType, Color *color);

Color *getNodeColorFromMap(const char *nodeType);
void initNodeColorMappings();


typedef struct {
    const char *language; // Language name (e.g., "c", "scheme")
    TSParser *parser;     // Tree-sitter parser for this language
    NodeColorMap *nodeColorMap;
} LanguageParser;

typedef struct {
    LanguageParser *items; // Dynamic array of LanguageParser
    size_t count;          // Number of parsers in the array
    size_t capacity;       // Current capacity of the array
} LanguageParsers;

extern LanguageParsers lps; // NOTE Global ring of LanguageParser(s)

void initLanguageParsers();
TSParser *inferParserForLanguage(const char *language);
void freeLanguageParsers();


void initSyntax(Buffer *buffer);
void parseSyntax(Buffer *buffer);
void updateSyntax(Buffer *buffer, const char *newContent, size_t newContentSize);
void freeSyntax(Buffer *buffer);
void freeSyntaxArray(SyntaxArray *array);

void displaySyntax(Buffer *buffer);
/* Color *getNodeColor(TSNode node); */
Color *getNodeColor(TSNode node, const char *major_mode);
Color *getNodeColor_C(TSNode node);
void printSyntaxTree(TSNode node, const char *source, int depth);
void initSyntaxArray(SyntaxArray *array, size_t initialSize);
/* void processNode(TSNode node, const char *source, SyntaxArray *array); */
void processNode(TSNode node, const char *source, SyntaxArray *array,
                 const char *major_mode);

void insertSyntax(SyntaxArray *array, Syntax syntax);
void initGlobalParser();
void freeGlobalParser();
void printSyntaxInfo(const Buffer *buffer);
TSPoint byteToPoint(const char* text, uint32_t byte);

void updateSyntaxIncremental(Buffer *buffer, TSInputEdit *edit);
TSInputEdit createInputEdit(Buffer *buffer, size_t start_byte, size_t old_end_byte, size_t new_end_byte);

bool isHexColor(const char *text);
void clearSyntaxArray(Buffer *buffer);

void apply_ansi_color_syntax(Buffer *buffer);
Color parseAnsiColor(const char *escapeCode, Color *currentColor);

void clearSyntax(Buffer *buffer, size_t start, size_t end);
void highlightColumns(Buffer *buffer, int numColors, ...);


void msm(Buffer *buffer, int index, int lengthChange);

bool major_mode_is_supported(char *grammar);

Color *getRainbowDelimiterColor(TSNode node);
Color *getIdentifierColor(TSNode node);


// TODO it's bad.
/* typedef struct { */
/*     uint32_t byte_offset; */
/*     int32_t depth; */
/*     Color* color; */
/* } DelimiterCacheEntry; */

/* typedef struct { */
/*     DelimiterCacheEntry* entries; */
/*     size_t count; */
/*     size_t capacity; */
/* } DelimitersCache; */

void parse_and_push_compilation_syntax(Buffer *buffer);

#endif // SYNTAX_H

#ifndef SYNTAX_H
#define SYNTAX_H

#include <tree_sitter/api.h>
#include "buffer.h"

// NOTE Syntax and SyntaxArray structs are defines inside buffer.h
// to avoid circular header dependency

extern TSParser *parser; // NOTE We currently support a single global parser
                         // when implementing new language syntax make it a
                         // global dynamix array of parsers

void initSyntax(Buffer *buffer);
void parseSyntax(Buffer *buffer);
void updateSyntax(Buffer *buffer, const char *newContent, size_t newContentSize);
void freeSyntax(Buffer *buffer);
void freeSyntaxArray(SyntaxArray *array);

void displaySyntax(Buffer *buffer);
/* Color getNodeColor(const char* nodeType); */
Color getNodeColor(TSNode node);
void printSyntaxTree(TSNode node, const char *source, int depth);
void initSyntaxArray(SyntaxArray *array, size_t initialSize);
void processNode(TSNode node, const char *source, SyntaxArray *array);
void insertSyntax(SyntaxArray *array, Syntax syntax);
void initGlobalParser();
void freeGlobalParser();
void printSyntaxInfo(const Buffer *buffer);
TSPoint byteToPoint(const char* text, uint32_t byte);
void adjustSyntaxRanges(Buffer *buffer, int index, int lengthChange);

bool isHexColor(const char *text);
#endif // SYNTAX_H

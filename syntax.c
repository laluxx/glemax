#include "./grammars/tree-sitter-c/bindings/c/tree-sitter-c.h"
#include "./grammars/tree-sitter-scheme/bindings/c/tree-sitter-scheme.h"
#include <stdio.h>
#include <string.h>
#include "syntax.h"
#include "theme.h"


LanguageParsers lps = {0}; // NOTE Global ring of LanguageParser(s)
// TODO language injection for org-mode, md-mode and gemini-mode
static bool printTSNodes = false;


void initLanguageParsers() {
    lps.capacity = 4; // Start with a small capacity
    lps.items = malloc(lps.capacity * sizeof(LanguageParser));
    if (!lps.items) {
        fprintf(stderr, "Failed to allocate memory for language parsers\n");
        exit(EXIT_FAILURE);
    }
    lps.count = 0;
}

void freeLanguageParsers() {
    for (size_t i = 0; i < lps.count; i++) {
        if (lps.items[i].parser) {
            ts_parser_delete(lps.items[i].parser);
            lps.items[i].parser = NULL;
        }
    }
    free(lps.items);
    lps.items = NULL;
    lps.count = 0;
    lps.capacity = 0;
}

void addParserForLanguage(const char *language, TSParser *parser) {
    // Resize the array if necessary
    if (lps.count >= lps.capacity) {
        lps.capacity *= 2;
        LanguageParser *new_items =
            realloc(lps.items, lps.capacity * sizeof(LanguageParser));
        if (!new_items) {
            fprintf(stderr, "Failed to reallocate memory for language parsers\n");
            exit(EXIT_FAILURE);
        }
        lps.items = new_items;
    }

    // Add the new parser
    lps.items[lps.count].language = language;
    lps.items[lps.count].parser = parser;
    lps.count++;
}

TSParser *inferParserForLanguage(const char *language) {
    // Check if the parser already exists
    for (size_t i = 0; i < lps.count; i++) {
        if (strcmp(lps.items[i].language, language) == 0) {
            return lps.items[i].parser;
        }
    }

    // If not, create a new parser
    TSParser *parser = ts_parser_new();
    TSLanguage *lang = NULL;

    // Load the appropriate language grammar
    if (strcmp(language, "c") == 0) {
        lang = tree_sitter_c();
    } else if (strcmp(language, "scheme") == 0) {
        lang = tree_sitter_scheme();
    }
    // Add more languages here as needed

  if (!lang) {
    fprintf(stderr, "Unsupported language: %s\n", language);
    ts_parser_delete(parser);
    return NULL;
  }

  ts_parser_set_language(parser, lang);
  addParserForLanguage(language, parser);
  return parser;
}


void initSyntax(Buffer *buffer) {
  if (!buffer->major_mode) {
    message("initSyntax :: Buffer has no major mode set");
    return;
  }

  TSParser *parser = inferParserForLanguage(buffer->major_mode);
  if (!parser) {
    fprintf(stderr, "Failed to get parser for language: %s\n",
            buffer->major_mode);
    return;
  }

  buffer->tree = ts_parser_parse_string(parser, NULL, buffer->content, buffer->size);
  initSyntaxArray(&buffer->syntaxArray, 10);
}

bool isHexColor(const char *text) {
    if (text[0] == '#' && (strlen(text) == 4 || strlen(text) == 7)) {
        for (int i = 1; i < strlen(text); i++) {
            if (!((text[i] >= '0' && text[i] <= '9') || 
                  (text[i] >= 'A' && text[i] <= 'F') || 
                  (text[i] >= 'a' && text[i] <= 'f'))) {
                return false;
            }
        }
        return true;
    }
    return false;
}


Color *getNodeColor_Scheme(TSNode node) {
    const char *nodeType = ts_node_type(node);

    // Keywords and special forms
    if (strcmp(nodeType, "lambda")   == 0 || strcmp(nodeType, "define")            == 0 ||
        strcmp(nodeType, "if")       == 0 || strcmp(nodeType, "cond")              == 0 ||
        strcmp(nodeType, "let")      == 0 || strcmp(nodeType, "let*")              == 0 ||
        strcmp(nodeType, "letrec")   == 0 || strcmp(nodeType, "begin")             == 0 ||
        strcmp(nodeType, "quote")    == 0 || strcmp(nodeType, "set!")              == 0 ||
        strcmp(nodeType, "and")      == 0 || strcmp(nodeType, "or")                == 0 ||
        strcmp(nodeType, "case")     == 0 || strcmp(nodeType, "do")                == 0 ||
        strcmp(nodeType, "syntax")   == 0 || strcmp(nodeType, "quasisyntax")       == 0 ||
        strcmp(nodeType, "unsyntax") == 0 || strcmp(nodeType, "unsyntax_splicing") == 0) {
        return &CT.keyword; // Keywords and special forms
    }

    // Strings
    if (strcmp(nodeType, "string") == 0) {
        return &CT.string; // String literals
    }

    // Numbers
    if (strcmp(nodeType, "number") == 0) {
        return &CT.number; // Numeric literals
    }

    // Symbols (identifiers)
    if (strcmp(nodeType, "symbol") == 0) {
        return &CT.variable; // Variables and identifiers
    }

    // Comments
    if (strcmp(nodeType, "comment") == 0 || strcmp(nodeType, "block_comment") == 0) {
        return &CT.comment; // Comments
    }

    // Booleans
    if (strcmp(nodeType, "boolean") == 0) {
        return &CT.keyword; // Booleans (treated as keywords)
    }

    // Characters
    if (strcmp(nodeType, "character") == 0) {
        return &CT.string; // Characters (treated like strings)
    }

    // Vectors and byte vectors
    if (strcmp(nodeType, "vector") == 0 || strcmp(nodeType, "byte_vector") == 0) {
        return &CT.type; // Vectors and byte vectors (treated as types)
    }

    // Lists and parentheses
    if (strcmp(nodeType, "list") == 0 || strcmp(nodeType, "(") == 0 ||
        strcmp(nodeType, ")") == 0 || strcmp(nodeType, "[") == 0 ||
        strcmp(nodeType, "]") == 0 || strcmp(nodeType, "{") == 0 ||
        strcmp(nodeType, "}") == 0) {
        return &CT.text; // Lists and parentheses (treated as plain text)
    }

    // Quoting and quasiquoting
    if (strcmp(nodeType, "quote") == 0 || strcmp(nodeType, "quasiquote") == 0 ||
        strcmp(nodeType, "unquote") == 0 || strcmp(nodeType, "unquote_splicing") == 0) {
        return &CT.keyword; // Quoting constructs (treated as keywords)
    }

    // Directives
    if (strcmp(nodeType, "directive") == 0) {
        return &CT.preprocessor; // Directives (treated as preprocessor directives)
    }

    // Keywords (e.g., #:keyword)
    if (strcmp(nodeType, "keyword") == 0) {
        return &CT.keyword; // Keywords (treated as keywords)
    }

    // Escape sequences in strings
    if (strcmp(nodeType, "escape_sequence") == 0) {
        return &CT.string; // Escape sequences (treated as part of strings)
    }

    // Default fallback
    return &CT.text; // Default to plain text for unknown node types
}
 
Color *getNodeColor(TSNode node, const char *major_mode) {
    if (strcmp(major_mode, "c") == 0) {
        return getNodeColor_C(node);
    } else if (strcmp(major_mode, "scheme") == 0) {
        return getNodeColor_Scheme(node);
    }
    // Add more languages here as needed

    // Default to text color for unsupported languages
    return &CT.text;
}

// TODO If this is really the correct solution,
// Sort based on whats most found in a typical *language* buffer
// for a free performance boost.
Color *getNodeColor_C(TSNode node) {
    if (printTSNodes) {
        printf("%s", ts_node_string(node));
        printf("\n\n\n");
    }

    const char *nodeType = ts_node_type(node);

    if (strcmp(nodeType,    "return")   == 0 || strcmp(nodeType, "if")     == 0
        || strcmp(nodeType, "while")    == 0 || strcmp(nodeType, "do")     == 0
        || strcmp(nodeType, "switch")   == 0 || strcmp(nodeType, "break")  == 0
        || strcmp(nodeType, "continue") == 0 || strcmp(nodeType, "goto")   == 0
        || strcmp(nodeType, "typedef")  == 0 || strcmp(nodeType, "extern") == 0
        || strcmp(nodeType, "else")     == 0 || strcmp(nodeType, "struct") == 0
        || strcmp(nodeType, "for")      == 0 || strcmp(nodeType, "const")  == 0 || strcmp(nodeType, "static")  == 0) {
        return &CT.keyword;

    } else if (strcmp(nodeType,    "NULL")  == 0
               || strcmp(nodeType, "true")  == 0
               || strcmp(nodeType, "false") == 0) {
        return &CT.null;

    } else if (strcmp(nodeType, "!") == 0) {
        return &CT.negation;

    } else if (strcmp(nodeType, "type_identifier")      == 0 ||
               strcmp(nodeType, "function_definition")  == 0 ||
               strcmp(nodeType, "sized_type_specifier") == 0 ||
               strcmp(nodeType, "primitive_type")       == 0 ||
               strcmp(nodeType, "primitive_type")       == 0) {
        return &CT.type;

    } else if (strcmp(nodeType, "string_literal")    == 0 ||
               strcmp(nodeType, "char_literal")      == 0 ||
               strcmp(nodeType, "string_content")    == 0 ||
               strcmp(nodeType, "system_lib_string") == 0 ||
               strcmp(nodeType, "\"")                == 0) {
        return &CT.string;

    } else if (strcmp(nodeType, "number_literal") == 0) {
        return &CT.number;

    } else if (strcmp(nodeType, "function_definition")  == 0 ||
               strcmp(nodeType, "function_declaration") == 0) {
        return &CT.function;

    } else if (strcmp(nodeType, "preproc_directive") == 0 ||
               strcmp(nodeType, "preproc_arg")       == 0 ||
               strcmp(nodeType, "preproc_def")       == 0 ||
               strcmp(nodeType, "#define")           == 0 ||
               strcmp(nodeType, "#include")          == 0) {
        return &CT.preprocessor;
       

    } else if (strcmp(nodeType, "assignment_expression") == 0 ||
               strcmp(nodeType, "arithmetic_expression") == 0 ||
               strcmp(nodeType, "unary_expression")      == 0 ||
               strcmp(nodeType, "update_expression")     == 0) {
        return &CT.cursor;

    } else if (strcmp(nodeType, "identifier") == 0) {
        TSNode parent = ts_node_parent(node);
        const char *parentType = ts_node_type(parent);

        if (strcmp(parentType, "function_declarator") == 0 ||
            strcmp(parentType, "function_definition") == 0) {
            return &CT.function;
        } else if (strcmp(parentType, "declaration") == 0 ||
                   strcmp(parentType, "assignment_expression") == 0 ||
                   strcmp(parentType, "init_declarator") == 0) {
            return &CT.variable;
        } else {
            return &CT.text;
        }
    } else if (strcmp(nodeType, "comment") == 0) {
        return &CT.comment;
    } else {
        return &CT.text;
    }
}

void insertSyntax(SyntaxArray *array, Syntax syntax) {
    if (array->used == array->size) {
        array->size *= 2;
        array->items = realloc(array->items, array->size * sizeof(Syntax));
        if (!array->items) {
            fprintf(stderr, "Failed to reallocate memory for syntax array.\n");
            exit(EXIT_FAILURE);
        }
    }

    // Find the correct position to maintain sorted order by 'start'
    size_t pos;
    for (pos = 0; pos < array->used; pos++) {
        if (syntax.start < array->items[pos].start) {
            break;
        }
    }

    // Shift elements to make space for the new entry
    memmove(&array->items[pos + 1], &array->items[pos], 
            (array->used - pos) * sizeof(Syntax));

    // Insert the new syntax entry
    array->items[pos] = syntax;
    array->used++;
}

/* void insertSyntax(SyntaxArray *array, Syntax syntax) { */
/*     if (array->used == array->size) { */
/*         array->size *= 2; */
/*         array->items = realloc(array->items, array->size * sizeof(Syntax)); */
/*     } */
/*     array->items[array->used++] = syntax; */
/* } */

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

// TODO Return a size_t of the max x the highlight as reached
// So we can continue to manually (or not) highlight the rest of the buffer.
void highlightColumns(Buffer *buffer, int numColors, ...) {
    if (!buffer || numColors <= 0) return;

    // Initialize syntax array if not already initialized
    if (buffer->syntaxArray.items == NULL && buffer->syntaxArray.size == 0) {
        initSyntaxArray(&buffer->syntaxArray, 10);
    }

    const char *text = buffer->content;
    size_t textLength = buffer->size;
    size_t start = 0;

    // Process each line
    while (start < textLength) {
        // Find the end of the current line
        size_t lineEnd = start;
        while (lineEnd < textLength && text[lineEnd] != '\n')
            lineEnd++;

        // Reset va_list for each line
        va_list colors;
        va_start(colors, numColors);

        // Process columns in the current line
        size_t columnStart = start;
        int columnIndex = 0;

        while (columnStart < lineEnd) {
            // Skip whitespace greedily
            while (columnStart < lineEnd && isspace(text[columnStart]))
                columnStart++;
            if (columnStart >= lineEnd)
                break;

            // Find end of column (non-whitespace)
            size_t columnEnd = columnStart;
            while (columnEnd < lineEnd && !isspace(text[columnEnd]))
                columnEnd++;

            // Only apply color if the column is non-empty and within the number of
            // colors
            if (columnEnd > columnStart && columnIndex < numColors) {
                Color *color = va_arg(colors, Color *);
                if (color == NULL) {
                    fprintf(stderr, "Error: Invalid color pointer for column %d\n",
                            columnIndex);
                    continue; // Skip this column
                }

                Syntax syntax = {columnStart, columnEnd, color};
                insertSyntax(&buffer->syntaxArray, syntax);
            }

            columnStart = columnEnd; // Move to next column
            columnIndex++;
        }

        va_end(colors);      // Clean up va_list for this line
        start = lineEnd + 1; // Move to the next line (skip '\n')
    }
}

void processFunctionDefinition(TSNode node, const char *source, Buffer *buffer) {
    TSNode declarator = ts_node_child_by_field_name(node, "declarator", 11);
    if (ts_node_is_null(declarator)) return;

    TSNode identifier = ts_node_child_by_field_name(declarator, "declarator", 11);
    if (ts_node_is_null(identifier)) return;
    
    const char *node_type = ts_node_type(identifier);
    if (strcmp(node_type, "identifier") != 0) return;

    uint32_t start_byte = ts_node_start_byte(identifier);
    uint32_t end_byte = ts_node_end_byte(identifier);
    TSPoint start_point = ts_node_start_point(identifier);

    // Extract function name
    char *name = strndup(source + start_byte, end_byte - start_byte);
    
    Function func = {
        .name = name,
        .line_number = start_point.row + 1,
        .start_byte = start_byte,
        .end_byte = end_byte
    };
}

void processNode(TSNode node, const char *source, SyntaxArray *array,
                 const char *major_mode) {
    if (ts_node_child_count(node) == 0) { // Process only leaf nodes
        uint32_t startByte = ts_node_start_byte(node);
        uint32_t endByte = ts_node_end_byte(node);
        Color *color = getNodeColor(node, major_mode); // Pass major_mode here
        Syntax syntax = {startByte, endByte, color};
        insertSyntax(array, syntax);
    } else {
        for (int i = 0; i < ts_node_child_count(node); i++) {
            TSNode child = ts_node_child(node, i);
            processNode(child, source, array, major_mode); // Pass major_mode here
        }
    }
}

/* void processNode(TSNode node, const char *source, SyntaxArray *array) { */
/*     if (ts_node_child_count(node) == 0) { // Process only leaf nodes */
/*         // No need for nodeType here unless you're debugging. */
/*         uint32_t startByte = ts_node_start_byte(node); */
/*         uint32_t endByte = ts_node_end_byte(node); */
/*         Color *color = getNodeColor(node); // getNodeColor returns a pointer */

/*         Syntax syntax = {startByte, endByte, color}; */
/*         insertSyntax(array, syntax); */
/*     } else { */
/*         // Recursively process child nodes */
/*         for (int i = 0; i < ts_node_child_count(node); i++) { */
/*             TSNode child = ts_node_child(node, i); */
/*             processNode(child, source, array); */
/*         } */
/*     } */
/* } */

void displaySyntax(Buffer *buffer) {
    TSNode root_node = ts_tree_root_node(buffer->tree);
    processNode(root_node, buffer->content, &buffer->syntaxArray, buffer->major_mode);
}

/* void displaySyntax(Buffer *buffer) { */
/*     TSNode root_node = ts_tree_root_node(buffer->tree); */
/*     processNode(root_node, buffer->content, &buffer->syntaxArray); */
/* } */

void parseSyntax(Buffer *buffer) {
    TSParser *parser = inferParserForLanguage(buffer->major_mode);
    if (!parser) {
        fprintf(stderr, "Failed to get parser for language: %s\n", buffer->major_mode);
        return;
    }

    buffer->tree = ts_parser_parse_string(parser, NULL, buffer->content, buffer->size);
    displaySyntax(buffer);
}

/* void parseSyntax(Buffer *buffer) { */
/*     buffer->tree = ts_parser_parse_string(parser, NULL, buffer->content, buffer->size); */
/*     displaySyntax(buffer); */
/* } */

void updateSyntax(Buffer *buffer, const char *newContent, size_t newContentSize) {
    TSParser *parser = inferParserForLanguage(buffer->major_mode);
    if (!parser) {
        fprintf(stderr, "Failed to get parser for language: %s\n", buffer->major_mode);
        return;
    }

    TSInputEdit edit;
    edit.start_byte = 0;
    edit.old_end_byte = buffer->size;
    edit.new_end_byte = newContentSize;
    edit.start_point = (TSPoint){0, 0};
    edit.old_end_point = ts_node_end_point(ts_tree_root_node(buffer->tree)); // Corrected
    edit.new_end_point = (TSPoint){0, 0}; // Calculate new end point if necessary

    ts_tree_edit(buffer->tree, &edit);
    buffer->tree = ts_parser_parse_string(parser, buffer->tree, buffer->content, newContentSize);
    displaySyntax(buffer);
}

void freeSyntax(Buffer *buffer) {
    if (buffer->tree) {
        ts_tree_delete(buffer->tree);
        buffer->tree = NULL;
    }
    freeSyntaxArray(&buffer->syntaxArray);
}

static void printIndent(int depth) {
    for (int i = 0; i < depth; i++) {
        printf("    ");
    }
}

void printSyntaxTree(TSNode node, const char *source, int depth) {
    const char *nodeType = ts_node_type(node);
    uint32_t startByte = ts_node_start_byte(node);
    uint32_t endByte = ts_node_end_byte(node);
    TSPoint startPoint = ts_node_start_point(node);
    TSPoint endPoint = ts_node_end_point(node);

    // Print node details in a structured format
    printIndent(depth);
    printf("Node type: %s\n", nodeType);
    printIndent(depth);
    printf("Span: %u-%u, Location: [%u:%u-%u:%u]\n", startByte, endByte,
           startPoint.row + 1, startPoint.column + 1, endPoint.row + 1, endPoint.column + 1);

    // Print text content for leaf nodes
    if (ts_node_child_count(node) == 0 && (endByte - startByte) < 50) { // Short texts for clarity
        printIndent(depth);
        printf("Text: \"%.*s\"\n", (int)(endByte - startByte), source + startByte);
    }

    // Recursively print children, increasing the indentation
    int childCount = ts_node_child_count(node);
    for (int i = 0; i < childCount; i++) {
        TSNode child = ts_node_child(node, i);
        printSyntaxTree(child, source, depth + 1);
    }

    // Separate different nodes visually by a blank line if at root level
    if (depth == 0) {
        printf("\n");
    }
}

void initSyntaxArray(SyntaxArray *array, size_t initialSize) {
    array->items = malloc(initialSize * sizeof(Syntax));
    array->used = 0;
    array->size = initialSize;
}


void freeSyntaxArray(SyntaxArray *array) {
    free(array->items);
    array->items = NULL;
    array->used = array->size = 0;
}

void printSyntaxInfo(const Buffer *buffer) {
    if (!buffer) {
        printf("Buffer is NULL\n");
        return;
    }

    const SyntaxArray *syntaxArray = &buffer->syntaxArray;
    if (!syntaxArray->items) {
        printf("Syntax array is empty or not initialized.\n");
        return;
    }

    printf("Syntax highlights in buffer '%s':\n", buffer->name);
    printf("Total syntax elements: %zu\n", syntaxArray->used);
    for (size_t i = 0; i < syntaxArray->used; i++) {
        const Syntax *syntax = &syntaxArray->items[i];
        printf("Syntax %zu: Start: %zu, End: %zu, Color: %d\n",
               i, syntax->start, syntax->end, syntax->color);
    }
}

TSPoint byteToPoint(const char* text, uint32_t byte) {
    TSPoint point = {0, 0}; // Start at the beginning of the document
    for (uint32_t i = 0; i < byte; ++i) {
        if (text[i] == '\n') { // Newline character moves to the next line
            point.row++;
            point.column = 0;
        } else {
            point.column++; // Move one character to the right
        }
    }
    return point;
}



// INCREMENTAL PARSING
TSInputEdit createInputEdit(Buffer *buffer, size_t start_byte, size_t old_end_byte, size_t new_end_byte) {
    TSInputEdit edit;
    edit.start_byte = start_byte;
    edit.old_end_byte = old_end_byte;
    edit.new_end_byte = new_end_byte;
    edit.start_point = byteToPoint(buffer->content, start_byte);
    edit.old_end_point = byteToPoint(buffer->content, old_end_byte);
    edit.new_end_point = byteToPoint(buffer->content, new_end_byte);
    return edit;
}


void updateSyntaxIncremental(Buffer *buffer, TSInputEdit *edit) {
    TSParser *parser = inferParserForLanguage(buffer->major_mode);
    if (!parser) {
        fprintf(stderr, "Failed to get parser for language: %s\n", buffer->major_mode);
        return;
    }

    if (!buffer->tree) {
        // If there's no existing tree, parse the entire buffer
        parseSyntax(buffer);
        return;
    }

    ts_tree_edit(buffer->tree, edit);

    TSTree *new_tree = ts_parser_parse_string(parser, buffer->tree,
                                              buffer->content, buffer->size);
    if (new_tree) {
        ts_tree_delete(buffer->tree);
        buffer->tree = new_tree;

        // Clear existing syntax array
        buffer->syntaxArray.used = 0;

        // Process the new tree to update syntax highlighting
        TSNode root_node = ts_tree_root_node(buffer->tree);
        processNode(root_node, buffer->content, &buffer->syntaxArray, buffer->major_mode);
    }
}

/**
   Clear Buffer->syntaxArray.used = 0 without freeing the memory.
*/
void clearSyntaxArray(Buffer *buffer) {
    buffer->syntaxArray.used = 0;
}

void clearSyntax(Buffer *buffer, size_t start, size_t end) {
    // TODO
}

// NOTE The only way you should ever touch the syntaxArray memory
// it could be weapped in the MSM macro
void msm(Buffer *buffer, int index, int lengthChange) {
    SyntaxArray *syntaxArray = &buffer->syntaxArray;
    for (size_t i = 0; i < syntaxArray->used; i++) {
        Syntax *syntax = &syntaxArray->items[i];
        if (syntax->start >= index) {
            syntax->start += lengthChange;
        }
        if (syntax->end >= index) {
            syntax->end += lengthChange;
        }
    }
}

void apply_ansi_color_syntax(Buffer *buffer) {
    // Clear existing syntax entries
    clearSyntaxArray(buffer);

    const char *text = buffer->content;
    size_t length = buffer->size;

    // Create a new buffer to store content without escape codes
    char *cleanedContent = malloc(length + 1); // +1 for null terminator
    size_t cleanedIndex = 0; // Tracks position in cleanedContent

    Color currentColor = CT.text; // Default color
    size_t visibleStart = 0; // Tracks visible text positions (excluding escape codes)

    size_t i = 0;
    while (i < length) {
        // Detect ANSI escape sequences
        if (text[i] == '\033' && i + 1 < length && text[i + 1] == '[') {
            size_t escapeStart = i;
            size_t escapeEnd = i + 2; // Skip ESC and [

            // Parse until 'm'
            while (escapeEnd < length && text[escapeEnd] != 'm') {
                escapeEnd++;
            }
            if (escapeEnd >= length) break; // Incomplete escape sequence
            escapeEnd++; // Include 'm'

            // Add syntax entry for text before the escape code
            if (visibleStart < cleanedIndex) {
                Syntax syntax = {
                    .start = visibleStart,
                    .end = cleanedIndex, // Use cleanedIndex for visible text
                    .color = &currentColor
                };
                insertSyntax(&buffer->syntaxArray, syntax);
            }

            // Update color based on escape code
            char escapeCode[32];
            size_t escapeLen = escapeEnd - escapeStart;
            strncpy(escapeCode, text + escapeStart, escapeLen);
            escapeCode[escapeLen] = '\0';
            currentColor = parseAnsiColor(escapeCode, &currentColor);

            // Skip the escape sequence in the cleaned content
            i = escapeEnd;
            visibleStart = cleanedIndex; // Next visible start after escape
        } else {
            // Copy non-escape characters to cleanedContent
            cleanedContent[cleanedIndex++] = text[i++];
        }
    }

    // Add syntax entry for remaining text
    if (visibleStart < cleanedIndex) {
        Syntax syntax = {
            .start = visibleStart,
            .end = cleanedIndex,
            .color = &currentColor
        };
        insertSyntax(&buffer->syntaxArray, syntax);
    }

    // Null-terminate the cleaned content
    cleanedContent[cleanedIndex] = '\0';

    // Replace buffer content with cleaned content
    setBufferContent(buffer, cleanedContent, true);

    // Free the temporary cleaned content buffer
    free(cleanedContent);
}


// TODO
Color parseAnsiColor(const char *escapeCode, Color *currentColor) {
    if (strstr(escapeCode, "[0m"))  return CT.text;
    if (strstr(escapeCode, "[31m")) return CT.string;
    if (strstr(escapeCode, "[32m")) return CT.type;
    if (strstr(escapeCode, "[33m")) return CT.keyword;
    if (strstr(escapeCode, "[34m")) return CT.function;
    if (strstr(escapeCode, "[35m")) return CT.preprocessor;
    if (strstr(escapeCode, "[36m")) return CT.variable;
    return *currentColor; // Default to current color if unknown
}

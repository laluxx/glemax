#include <tree_sitter/tree-sitter-c.h>
#include <stdio.h>
#include <string.h>
#include "syntax.h"
#include "theme.h"

// TODO language injection for org-mode, md-mode and gemini-mode

TSParser *parser; // NOTE Global parser
static bool printTSNodes = false;

void initGlobalParser() {
    parser = ts_parser_new();
    TSLanguage *lang = tree_sitter_c();  // Adjust this for the specific language you need
    if (!ts_parser_set_language(parser, lang)) {
        fprintf(stderr, "Failed to load Tree-sitter language.\n");
        exit(EXIT_FAILURE);
    }
}

void freeGlobalParser() {
    if (parser) {
        ts_parser_delete(parser);
        parser = NULL;
    }
}

void initSyntax(Buffer *buffer) {
    parser = ts_parser_new();
    TSLanguage *lang = tree_sitter_c();  // NOTE Hardcoded to C
    ts_parser_set_language(parser, lang);
    buffer->tree = ts_parser_parse_string(parser, NULL, buffer->content, buffer->size);
    initSyntaxArray(&buffer->syntaxArray, 10);  // Initial size
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

// TODO Sort based on whats most found in a typical c buffer for a free performance boost
Color *getNodeColor(TSNode node) {
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
    }
    array->items[array->used++] = syntax;
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

void processNode(TSNode node, const char *source, SyntaxArray *array) {
    if (ts_node_child_count(node) == 0) { // Process only leaf nodes
        // No need for nodeType here unless you're debugging.
        uint32_t startByte = ts_node_start_byte(node);
        uint32_t endByte = ts_node_end_byte(node);
        Color *color = getNodeColor(node); // getNodeColor returns a pointer

        Syntax syntax = {startByte, endByte, color};
        insertSyntax(array, syntax);
    } else {
        // Recursively process child nodes
        for (int i = 0; i < ts_node_child_count(node); i++) {
            TSNode child = ts_node_child(node, i);
            processNode(child, source, array);
        }
    }
}

void displaySyntax(Buffer *buffer) {
    TSNode root_node = ts_tree_root_node(buffer->tree);
    processNode(root_node, buffer->content, &buffer->syntaxArray);
}

void parseSyntax(Buffer *buffer) {
    buffer->tree = ts_parser_parse_string(parser, NULL, buffer->content, buffer->size);
    displaySyntax(buffer);
}

void updateSyntax(Buffer *buffer, const char *newContent, size_t newContentSize) {
    TSInputEdit edit;
    edit.start_byte = 0;
    edit.old_end_byte = buffer->size;
    edit.new_end_byte = newContentSize;
    edit.start_point = (TSPoint){0, 0};
    edit.old_end_point = ts_node_end_point(ts_tree_root_node(buffer->tree)); // Corrected
    edit.new_end_point = (TSPoint){0, 0};  // Calculate new end point if necessary

    ts_tree_edit(buffer->tree, &edit);
    buffer->tree = ts_parser_parse_string(parser, buffer->tree, buffer->content, newContentSize);
    displaySyntax(buffer);
}

void freeSyntax(Buffer *buffer) {
    if (buffer->tree) {
        ts_tree_delete(buffer->tree);
        buffer->tree = NULL;
    }
    if (parser) {
        ts_parser_delete(parser);
        parser = NULL;
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
        processNode(root_node, buffer->content, &buffer->syntaxArray);
    }
}



// FUNCTIONS

void traverseTreeForFunctions(TSNode node, const char *source, Buffer *buffer) {
    // Check if this node is a function definition or declaration
    const char *nodeType = ts_node_type(node);
    if (strcmp(nodeType, "function_definition") == 0 ||
        strcmp(nodeType, "function_declaration") == 0) {
        // Attempt to locate the declarator containing the identifier
        TSNode declarator = ts_node_child_by_field_name(node, "declarator", 11);
        if (!ts_node_is_null(declarator)) {
            // Further extract the actual identifier node
            TSNode identifier = ts_node_child_by_field_name(declarator, "declarator", 11);
            if (!ts_node_is_null(identifier) && strcmp(ts_node_type(identifier), "identifier") == 0) {
                uint32_t start_byte = ts_node_start_byte(identifier);
                uint32_t end_byte = ts_node_end_byte(identifier);
                TSPoint start_point = ts_node_start_point(identifier);

                // Extract function name from the source text
                char *name = strndup(source + start_byte, end_byte - start_byte);

                // Create a new function record
                Function newFunc;
                newFunc.name = name;
                newFunc.line_number = start_point.row + 1; // 1-indexed line number
                newFunc.start_byte = start_byte;
                newFunc.end_byte = end_byte;

                // Add newFunc to the buffer's functions array
                // (Here you should reallocate buffer->functions.items if necessary)
                if (buffer->functions.used == buffer->functions.size) {
                    buffer->functions.size = buffer->functions.size ? buffer->functions.size * 2 : 4;
                    buffer->functions.items = realloc(buffer->functions.items, buffer->functions.size * sizeof(Function));
                }
                buffer->functions.items[buffer->functions.used++] = newFunc;
            }
        }
    }

    // Recursively traverse child nodes
    uint32_t childCount = ts_node_child_count(node);
    for (uint32_t i = 0; i < childCount; i++) {
        TSNode child = ts_node_child(node, i);
        traverseTreeForFunctions(child, source, buffer);
    }
}

// A helper to start the extraction from the tree's root.
void extractFunctions(Buffer *buffer) {
    if (!buffer->tree) return;
    // Initialize the functions array if necessary.
    buffer->functions.used = 0;
    buffer->functions.size = 4;  // starting size
    buffer->functions.items = malloc(buffer->functions.size * sizeof(Function));

    TSNode root = ts_tree_root_node(buffer->tree);
    traverseTreeForFunctions(root, buffer->content, buffer);
}

// Without freeing the memory
void clearSyntaxArray(Buffer *buffer) {
    buffer->syntaxArray.used = 0;
}

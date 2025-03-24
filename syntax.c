#include "./grammars/tree-sitter-c/bindings/c/tree-sitter-c.h"
#include "./grammars/tree-sitter-scheme/bindings/c/tree-sitter-scheme.h"
#include "./grammars/tree-sitter-html/bindings/c/tree-sitter-html.h"
#include "./grammars/tree-sitter-query/bindings/c/tree-sitter-query.h"
#include "./grammars/tree-sitter-glsl/bindings/c/tree-sitter-glsl.h"
#include "./grammars/tree-sitter-zig/bindings/c/tree-sitter-zig.h"
#include "./grammars/tree-sitter-odin/bindings/c/tree-sitter-odin.h"
#include "./grammars/tree-sitter-make/bindings/c/tree-sitter-make.h"
#include "./grammars/tree-sitter-commonlisp/bindings/c/tree-sitter-commonlisp.h"
#include "./grammars/tree-sitter-scss/bindings/c/tree-sitter-scss.h"
#include "./grammars/tree-sitter-haskell/bindings/c/tree-sitter-haskell.h"
#include "./grammars/tree-sitter-lua/bindings/c/tree-sitter-lua.h"
#include "./grammars/tree-sitter-rust/bindings/c/tree-sitter-rust.h"
#include "./grammars/tree-sitter-bash/bindings/c/tree-sitter-bash.h"
#include "./grammars/tree-sitter-elisp/bindings/c/tree-sitter-elisp.h"
#include "./grammars/tree-sitter-python/bindings/c/tree-sitter-python.h"
#include "./grammars/tree-sitter-ocaml/bindings/c/tree_sitter/tree-sitter-ocaml.h"
#include "./grammars/tree-sitter-css/bindings/c/tree-sitter-css.h"
#include "./grammars/tree-sitter-javascript/bindings/c/tree-sitter-javascript.h"
#include "./grammars/tree-sitter-julia/bindings/c/tree-sitter-julia.h"
#include "./grammars/tree-sitter-cpp/bindings/c/tree-sitter-cpp.h"
#include "./grammars/tree-sitter-go/bindings/c/tree-sitter-go.h"
#include "./grammars/tree-sitter-json/bindings/c/tree-sitter-json.h"
#include "./grammars/tree-sitter-regex/bindings/c/tree-sitter-regex.h"
#include <stdio.h>
#include <string.h>
#include "syntax.h"
#include "buffer.h"
#include "theme.h"

NodeColorMap    *nodeColorMap = {0}; // NOTE Global node->color mapping (for all grammars ?)
LanguageParsers  lps          = {0}; // NOTE Global ring of LanguageParser(s)

// TODO language injection for org-mode, md-mode and gemini-mode
static bool printTSNodes = false;


// Helper function to add a node type to color mapping
void addNodeColorMapping(NodeColorMap **map, const char *nodeType, Color *color) {
    NodeColorMap *entry = malloc(sizeof(NodeColorMap));
    entry->nodeType = nodeType;
    entry->color = color;
    HASH_ADD_KEYPTR(hh, *map, entry->nodeType, strlen(entry->nodeType), entry);
}

void initCNodeColorMappings(NodeColorMap **map) {
    addNodeColorMapping(map, "return",   &CT.keyword);
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "while",    &CT.keyword);
    addNodeColorMapping(map, "do",       &CT.keyword);
    addNodeColorMapping(map, "switch",   &CT.keyword);
    addNodeColorMapping(map, "break",    &CT.keyword);
    addNodeColorMapping(map, "continue", &CT.keyword);
    addNodeColorMapping(map, "goto",     &CT.keyword);
    addNodeColorMapping(map, "typedef",  &CT.keyword);
    addNodeColorMapping(map, "extern",   &CT.keyword);
    addNodeColorMapping(map, "else",     &CT.keyword);
    addNodeColorMapping(map, "struct",   &CT.keyword);
    addNodeColorMapping(map, "for",      &CT.keyword);
    addNodeColorMapping(map, "const",    &CT.keyword);
    addNodeColorMapping(map, "static",   &CT.keyword);
    addNodeColorMapping(map, "NULL",  &CT.null);
    addNodeColorMapping(map, "true",  &CT.null);
    addNodeColorMapping(map, "false", &CT.null);
    addNodeColorMapping(map, "!",     &CT.negation);
    addNodeColorMapping(map, "type_identifier",      &CT.type);
    addNodeColorMapping(map, "function_definition",  &CT.type);
    addNodeColorMapping(map, "sized_type_specifier", &CT.type);
    addNodeColorMapping(map, "primitive_type",       &CT.type);
    addNodeColorMapping(map, "string_literal",    &CT.string);
    addNodeColorMapping(map, "char_literal",      &CT.string);
    addNodeColorMapping(map, "string_content",    &CT.string);
    addNodeColorMapping(map, "system_lib_string", &CT.string);
    addNodeColorMapping(map, "\"",                &CT.string);
    addNodeColorMapping(map, "number_literal", &CT.number);
    addNodeColorMapping(map, "function_definition",  &CT.function);
    addNodeColorMapping(map, "function_declaration", &CT.function);
    addNodeColorMapping(map, "preproc_directive", &CT.preprocessor);
    addNodeColorMapping(map, "preproc_arg",       &CT.preprocessor);
    addNodeColorMapping(map, "preproc_def",       &CT.preprocessor);
    addNodeColorMapping(map, "#define",           &CT.preprocessor);
    addNodeColorMapping(map, "#include",          &CT.preprocessor);
    addNodeColorMapping(map, "assignment_expression", &CT.cursor);
    addNodeColorMapping(map, "arithmetic_expression", &CT.cursor);
    addNodeColorMapping(map, "unary_expression",      &CT.cursor);
    addNodeColorMapping(map, "update_expression",     &CT.cursor);
    // Since we can't dynamically check parent nodes,
    // Map all possible combinations of identifier and its parent node types
    addNodeColorMapping(map, "identifier:function_declarator",   &CT.function);
    addNodeColorMapping(map, "identifier:function_definition",   &CT.function);
    addNodeColorMapping(map, "identifier:declaration",           &CT.variable);
    addNodeColorMapping(map, "identifier:assignment_expression", &CT.variable);
    addNodeColorMapping(map, "identifier:init_declarator",       &CT.variable);
    addNodeColorMapping(map, "identifier",                       &CT.text); //_=> identifier
    addNodeColorMapping(map, "comment", &CT.comment);
    //_=> will use CT.text
}

void initSchemeNodeColorMappings(NodeColorMap **map) {
    addNodeColorMapping(map, "lambda", &CT.keyword);
    addNodeColorMapping(map, "define", &CT.keyword);
    addNodeColorMapping(map, "if", &CT.keyword);
    addNodeColorMapping(map, "cond", &CT.keyword);
    addNodeColorMapping(map, "let", &CT.keyword);
    addNodeColorMapping(map, "let*", &CT.keyword);
    addNodeColorMapping(map, "letrec", &CT.keyword);
    addNodeColorMapping(map, "begin", &CT.keyword);
    addNodeColorMapping(map, "quote", &CT.keyword);
    addNodeColorMapping(map, "set!", &CT.keyword);
    addNodeColorMapping(map, "and", &CT.keyword);
    addNodeColorMapping(map, "or", &CT.keyword);
    addNodeColorMapping(map, "case", &CT.keyword);
    addNodeColorMapping(map, "do", &CT.keyword);
    addNodeColorMapping(map, "syntax", &CT.keyword);
    addNodeColorMapping(map, "quasisyntax", &CT.keyword);
    addNodeColorMapping(map, "unsyntax", &CT.keyword);
    addNodeColorMapping(map, "unsyntax_splicing", &CT.keyword);
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "number", &CT.number);
    addNodeColorMapping(map, "symbol", &CT.variable);
    addNodeColorMapping(map, "comment", &CT.comment);
    addNodeColorMapping(map, "block_comment", &CT.comment);
    addNodeColorMapping(map, "boolean", &CT.keyword);
    addNodeColorMapping(map, "character", &CT.string);
    addNodeColorMapping(map, "vector", &CT.type);
    addNodeColorMapping(map, "byte_vector", &CT.type);
    addNodeColorMapping(map, "list", &CT.text);
    addNodeColorMapping(map, "(", &CT.text);
    addNodeColorMapping(map, ")", &CT.text);
    addNodeColorMapping(map, "[", &CT.text);
    addNodeColorMapping(map, "]", &CT.text);
    addNodeColorMapping(map, "{", &CT.text);
    addNodeColorMapping(map, "}", &CT.text);
    addNodeColorMapping(map, "quote", &CT.keyword);
    addNodeColorMapping(map, "quasiquote", &CT.keyword);
    addNodeColorMapping(map, "unquote", &CT.keyword);
    addNodeColorMapping(map, "unquote_splicing", &CT.keyword);
    addNodeColorMapping(map, "directive", &CT.preprocessor);
    addNodeColorMapping(map, "keyword", &CT.keyword);
    addNodeColorMapping(map, "escape_sequence", &CT.string);
}

void initHtmlNodeColorMappings(NodeColorMap **map) {
    addNodeColorMapping(map, "start_tag", &CT.keyword);
    addNodeColorMapping(map, "end_tag", &CT.keyword);
    addNodeColorMapping(map, "self_closing_tag", &CT.keyword);
    addNodeColorMapping(map, "tag_name", &CT.keyword);
    addNodeColorMapping(map, "doctype", &CT.keyword);
    addNodeColorMapping(map, "attribute", &CT.type);
    addNodeColorMapping(map, "attribute_name", &CT.type);
    addNodeColorMapping(map, "attribute_value", &CT.string);
    addNodeColorMapping(map, "quoted_attribute_value", &CT.string);
    addNodeColorMapping(map, "text", &CT.text);
    addNodeColorMapping(map, "comment", &CT.comment);
    addNodeColorMapping(map, "entity", &CT.variable);
    addNodeColorMapping(map, "script_element", &CT.function);
    addNodeColorMapping(map, "style_element", &CT.function);
    addNodeColorMapping(map, "raw_text", &CT.function);
    addNodeColorMapping(map, "erroneous_end_tag", &CT.error);
    addNodeColorMapping(map, "erroneous_end_tag_name", &CT.error);
    addNodeColorMapping(map, "<", &CT.preprocessor);
    addNodeColorMapping(map, ">", &CT.preprocessor);
    addNodeColorMapping(map, "</", &CT.preprocessor);
    addNodeColorMapping(map, "/>", &CT.preprocessor);
    addNodeColorMapping(map, "=", &CT.preprocessor);
    addNodeColorMapping(map, "\"", &CT.preprocessor);
    addNodeColorMapping(map, "'", &CT.preprocessor);
}

void initQueryNodeColorMappings(NodeColorMap **map) {
    // Named nodes
    addNodeColorMapping(map, "definition", &CT.keyword);
    addNodeColorMapping(map, "anonymous_node", &CT.type);
    addNodeColorMapping(map, "capture", &CT.variable);
    addNodeColorMapping(map, "field_definition", &CT.type);
    addNodeColorMapping(map, "grouping", &CT.keyword);
    addNodeColorMapping(map, "list", &CT.keyword);
    addNodeColorMapping(map, "missing_node", &CT.error);
    addNodeColorMapping(map, "named_node", &CT.type);
    addNodeColorMapping(map, "negated_field", &CT.negation);
    addNodeColorMapping(map, "predicate", &CT.keyword);
    addNodeColorMapping(map, "quantifier", &CT.operator);
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "string_content", &CT.string);
    addNodeColorMapping(map, "escape_sequence", &CT.string);
    addNodeColorMapping(map, "identifier", &CT.variable);
    addNodeColorMapping(map, "comment", &CT.comment);
    addNodeColorMapping(map, "parameters", &CT.variable);
    addNodeColorMapping(map, "program", &CT.keyword);
    addNodeColorMapping(map, "predicate_type", &CT.type);

    // Syntax tokens/operators
    addNodeColorMapping(map, "!", &CT.operator);
    addNodeColorMapping(map, "#", &CT.preprocessor);
    addNodeColorMapping(map, "(", &CT.operator);
    addNodeColorMapping(map, ")", &CT.operator);
    addNodeColorMapping(map, "*", &CT.operator);
    addNodeColorMapping(map, "+", &CT.operator);
    addNodeColorMapping(map, ".", &CT.operator);
    addNodeColorMapping(map, "/", &CT.operator);
    addNodeColorMapping(map, ":", &CT.operator);
    addNodeColorMapping(map, "?", &CT.operator);
    addNodeColorMapping(map, "@", &CT.operator);
    addNodeColorMapping(map, "[", &CT.operator);
    addNodeColorMapping(map, "]", &CT.operator);
    addNodeColorMapping(map, "_", &CT.operator);
    addNodeColorMapping(map, "=", &CT.operator);
    addNodeColorMapping(map, "\"", &CT.string);
    
    // Special cases
    addNodeColorMapping(map, "MISSING", &CT.error);
}

void initGlslNodeColorMappings(NodeColorMap **map) {
    // GLSL keywords
    addNodeColorMapping(map, "void",     &CT.keyword);
    addNodeColorMapping(map, "float",    &CT.keyword);
    addNodeColorMapping(map, "int",      &CT.keyword);
    addNodeColorMapping(map, "bool",     &CT.keyword);
    addNodeColorMapping(map, "vec2",     &CT.keyword);
    addNodeColorMapping(map, "vec3",     &CT.keyword);
    addNodeColorMapping(map, "vec4",     &CT.keyword);
    addNodeColorMapping(map, "mat2",     &CT.keyword);
    addNodeColorMapping(map, "mat3",     &CT.keyword);
    addNodeColorMapping(map, "mat4",     &CT.keyword);
    addNodeColorMapping(map, "sampler2D", &CT.keyword);
    addNodeColorMapping(map, "samplerCube", &CT.keyword);
    
    // Control flow
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "else",     &CT.keyword);
    addNodeColorMapping(map, "for",      &CT.keyword);
    addNodeColorMapping(map, "while",    &CT.keyword);
    addNodeColorMapping(map, "do",       &CT.keyword);
    addNodeColorMapping(map, "switch",   &CT.keyword);
    addNodeColorMapping(map, "case",     &CT.keyword);
    addNodeColorMapping(map, "default",  &CT.keyword);
    addNodeColorMapping(map, "break",    &CT.keyword);
    addNodeColorMapping(map, "continue", &CT.keyword);
    addNodeColorMapping(map, "return",   &CT.keyword);
    addNodeColorMapping(map, "discard",  &CT.keyword);
    
    // Qualifiers
    addNodeColorMapping(map, "in",       &CT.keyword);
    addNodeColorMapping(map, "out",      &CT.keyword);
    addNodeColorMapping(map, "inout",    &CT.keyword);
    addNodeColorMapping(map, "uniform",  &CT.keyword);
    addNodeColorMapping(map, "attribute", &CT.keyword);
    addNodeColorMapping(map, "varying",  &CT.keyword);
    addNodeColorMapping(map, "const",    &CT.keyword);
    addNodeColorMapping(map, "highp",    &CT.keyword);
    addNodeColorMapping(map, "mediump",  &CT.keyword);
    addNodeColorMapping(map, "lowp",     &CT.keyword);
    
    // Built-in values
    addNodeColorMapping(map, "true",     &CT.null);
    addNodeColorMapping(map, "false",    &CT.null);
    addNodeColorMapping(map, "gl_Position", &CT.variable);
    addNodeColorMapping(map, "gl_FragCoord", &CT.variable);
    addNodeColorMapping(map, "gl_FragColor", &CT.variable);
    addNodeColorMapping(map, "gl_PointSize", &CT.variable);
    
    // Operators
    addNodeColorMapping(map, "!",        &CT.negation);
    
    // Types
    addNodeColorMapping(map, "type_identifier",      &CT.type);
    addNodeColorMapping(map, "function_definition",  &CT.type);
    addNodeColorMapping(map, "primitive_type",       &CT.type);
    
    // Literals
    addNodeColorMapping(map, "string_literal",    &CT.string);
    addNodeColorMapping(map, "string_content",    &CT.string);
    addNodeColorMapping(map, "\"",                &CT.string);
    addNodeColorMapping(map, "number_literal",    &CT.number);
    addNodeColorMapping(map, "float_literal",     &CT.number);
    addNodeColorMapping(map, "int_literal",       &CT.number);
    addNodeColorMapping(map, "bool_literal",      &CT.null);
    
    // Functions
    addNodeColorMapping(map, "function_definition",  &CT.function);
    addNodeColorMapping(map, "function_declaration", &CT.function);
    
    // Built-in functions
    addNodeColorMapping(map, "texture2D",   &CT.function);
    addNodeColorMapping(map, "textureCube", &CT.function);
    addNodeColorMapping(map, "normalize",   &CT.function);
    addNodeColorMapping(map, "dot",         &CT.function);
    addNodeColorMapping(map, "cross",       &CT.function);
    addNodeColorMapping(map, "mix",         &CT.function);
    addNodeColorMapping(map, "length",      &CT.function);
    addNodeColorMapping(map, "sin",         &CT.function);
    addNodeColorMapping(map, "cos",         &CT.function);
    
    // Preprocessor
    addNodeColorMapping(map, "preproc_directive", &CT.preprocessor);
    addNodeColorMapping(map, "preproc_arg",       &CT.preprocessor);
    addNodeColorMapping(map, "preproc_def",       &CT.preprocessor);
    addNodeColorMapping(map, "#define",           &CT.preprocessor);
    addNodeColorMapping(map, "#version",          &CT.preprocessor);
    addNodeColorMapping(map, "#ifdef",            &CT.preprocessor);
    addNodeColorMapping(map, "#ifndef",           &CT.preprocessor);
    addNodeColorMapping(map, "#endif",            &CT.preprocessor);
    
    // Expressions
    addNodeColorMapping(map, "assignment_expression", &CT.cursor);
    addNodeColorMapping(map, "arithmetic_expression", &CT.cursor);
    addNodeColorMapping(map, "unary_expression",      &CT.cursor);
    addNodeColorMapping(map, "update_expression",     &CT.cursor);
    
    // Identifiers with context
    addNodeColorMapping(map, "identifier:function_declarator",   &CT.function);
    addNodeColorMapping(map, "identifier:function_definition",   &CT.function);
    addNodeColorMapping(map, "identifier:declaration",           &CT.variable);
    addNodeColorMapping(map, "identifier:assignment_expression", &CT.variable);
    addNodeColorMapping(map, "identifier:init_declarator",       &CT.variable);
    addNodeColorMapping(map, "identifier",                       &CT.text);
    
    // Comments
    addNodeColorMapping(map, "comment", &CT.comment);
    
    // Default fallback will use CT.text
}

void initZigNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "const",    &CT.keyword);
    addNodeColorMapping(map, "var",      &CT.keyword);
    addNodeColorMapping(map, "pub",      &CT.keyword);
    addNodeColorMapping(map, "extern",   &CT.keyword);
    addNodeColorMapping(map, "export",   &CT.keyword);
    addNodeColorMapping(map, "inline",   &CT.keyword);
    addNodeColorMapping(map, "comptime", &CT.keyword);
    addNodeColorMapping(map, "volatile", &CT.keyword);
    addNodeColorMapping(map, "align",    &CT.keyword);
    addNodeColorMapping(map, "linksection", &CT.keyword);
    addNodeColorMapping(map, "threadlocal", &CT.keyword);

    // Control flow
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "else",     &CT.keyword);
    addNodeColorMapping(map, "switch",   &CT.keyword);
    addNodeColorMapping(map, "case",     &CT.keyword);
    addNodeColorMapping(map, "while",    &CT.keyword);
    addNodeColorMapping(map, "for",      &CT.keyword);
    addNodeColorMapping(map, "break",    &CT.keyword);
    addNodeColorMapping(map, "continue", &CT.keyword);
    addNodeColorMapping(map, "return",   &CT.keyword);
    addNodeColorMapping(map, "defer",    &CT.keyword);
    addNodeColorMapping(map, "errdefer", &CT.keyword);
    addNodeColorMapping(map, "try",      &CT.keyword);
    addNodeColorMapping(map, "catch",    &CT.keyword);
    addNodeColorMapping(map, "unreachable", &CT.keyword);
    addNodeColorMapping(map, "suspend",  &CT.keyword);
    addNodeColorMapping(map, "resume",   &CT.keyword);
    addNodeColorMapping(map, "await",    &CT.keyword);

    // Type definitions
    addNodeColorMapping(map, "struct",   &CT.keyword);
    addNodeColorMapping(map, "enum",     &CT.keyword);
    addNodeColorMapping(map, "union",    &CT.keyword);
    addNodeColorMapping(map, "error",    &CT.keyword);
    addNodeColorMapping(map, "packed",   &CT.keyword);
    addNodeColorMapping(map, "fn",       &CT.keyword);
    addNodeColorMapping(map, "usingnamespace", &CT.keyword);

    // Testing and debugging
    addNodeColorMapping(map, "test",     &CT.keyword);
    addNodeColorMapping(map, "debug",    &CT.keyword);

    // Basic types
    addNodeColorMapping(map, "i8",       &CT.type);
    addNodeColorMapping(map, "u8",       &CT.type);
    addNodeColorMapping(map, "i16",      &CT.type);
    addNodeColorMapping(map, "u16",      &CT.type);
    addNodeColorMapping(map, "i32",      &CT.type);
    addNodeColorMapping(map, "u32",      &CT.type);
    addNodeColorMapping(map, "i64",      &CT.type);
    addNodeColorMapping(map, "u64",      &CT.type);
    addNodeColorMapping(map, "i128",     &CT.type);
    addNodeColorMapping(map, "u128",     &CT.type);
    addNodeColorMapping(map, "isize",    &CT.type);
    addNodeColorMapping(map, "usize",    &CT.type);
    addNodeColorMapping(map, "c_short",  &CT.type);
    addNodeColorMapping(map, "c_ushort", &CT.type);
    addNodeColorMapping(map, "c_int",    &CT.type);
    addNodeColorMapping(map, "c_uint",   &CT.type);
    addNodeColorMapping(map, "c_long",   &CT.type);
    addNodeColorMapping(map, "c_ulong",  &CT.type);
    addNodeColorMapping(map, "c_longlong", &CT.type);
    addNodeColorMapping(map, "c_ulonglong", &CT.type);
    addNodeColorMapping(map, "f16",      &CT.type);
    addNodeColorMapping(map, "f32",      &CT.type);
    addNodeColorMapping(map, "f64",      &CT.type);
    addNodeColorMapping(map, "f128",     &CT.type);
    addNodeColorMapping(map, "bool",     &CT.type);
    addNodeColorMapping(map, "void",     &CT.type);
    addNodeColorMapping(map, "noreturn", &CT.type);
    addNodeColorMapping(map, "type",     &CT.type);
    addNodeColorMapping(map, "anytype",  &CT.type);
    addNodeColorMapping(map, "anyframe", &CT.type);

    // Builtin values
    addNodeColorMapping(map, "null",     &CT.null);
    addNodeColorMapping(map, "undefined",&CT.null);
    addNodeColorMapping(map, "true",     &CT.null);
    addNodeColorMapping(map, "false",    &CT.null);

    // Operators
    addNodeColorMapping(map, "!",        &CT.negation);
    
    // Node types
    addNodeColorMapping(map, "type_identifier",     &CT.type);
    addNodeColorMapping(map, "function_definition", &CT.type);
    addNodeColorMapping(map, "primitive_type",      &CT.type);
    addNodeColorMapping(map, "container_declaration", &CT.type);

    // Literals
    addNodeColorMapping(map, "string_literal",  &CT.string);
    addNodeColorMapping(map, "char_literal",    &CT.string);
    addNodeColorMapping(map, "string_content",  &CT.string);
    addNodeColorMapping(map, "multiline_string_literal", &CT.string);
    addNodeColorMapping(map, "\"",              &CT.string);
    addNodeColorMapping(map, "integer_literal", &CT.number);
    addNodeColorMapping(map, "float_literal",   &CT.number);

    // Functions
    addNodeColorMapping(map, "function_definition",  &CT.function);
    addNodeColorMapping(map, "function_declaration", &CT.function);
    addNodeColorMapping(map, "function_prototype",   &CT.function);
    
    // Builtin functions
    addNodeColorMapping(map, "@import",         &CT.function);
    addNodeColorMapping(map, "@cImport",        &CT.function);
    addNodeColorMapping(map, "@cInclude",       &CT.function);
    addNodeColorMapping(map, "@fieldParentPtr", &CT.function);
    addNodeColorMapping(map, "@TypeOf",         &CT.function);
    addNodeColorMapping(map, "@sizeOf",         &CT.function);
    addNodeColorMapping(map, "@alignOf",        &CT.function);
    addNodeColorMapping(map, "@bitSizeOf",      &CT.function);
    addNodeColorMapping(map, "@errorReturnTrace", &CT.function);
    addNodeColorMapping(map, "@panic",          &CT.function);
    addNodeColorMapping(map, "@compileError",   &CT.function);
    addNodeColorMapping(map, "@compileLog",     &CT.function);
    addNodeColorMapping(map, "@embedFile",      &CT.function);

    // Expressions
    addNodeColorMapping(map, "assignment_expression", &CT.cursor);
    addNodeColorMapping(map, "binary_expression",    &CT.cursor);
    addNodeColorMapping(map, "unary_expression",     &CT.cursor);
    addNodeColorMapping(map, "update_expression",    &CT.cursor);
    
    // Identifiers with context
    addNodeColorMapping(map, "identifier:function_declaration", &CT.function);
    addNodeColorMapping(map, "identifier:function_definition",  &CT.function);
    addNodeColorMapping(map, "identifier:declaration",          &CT.variable);
    addNodeColorMapping(map, "identifier:variable_declaration", &CT.variable);
    addNodeColorMapping(map, "identifier:assignment_expression",&CT.variable);
    addNodeColorMapping(map, "identifier:field_declaration",    &CT.variable);
    addNodeColorMapping(map, "identifier",                      &CT.text);
    
    // Comments
    addNodeColorMapping(map, "comment",              &CT.comment);
    addNodeColorMapping(map, "line_comment",         &CT.comment);
    addNodeColorMapping(map, "multiline_comment",    &CT.comment);
    addNodeColorMapping(map, "doc_comment",          &CT.comment);
    addNodeColorMapping(map, "container_doc_comment",&CT.comment);
}

void initOdinNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "import",   &CT.keyword);
    addNodeColorMapping(map, "foreign",  &CT.keyword);
    addNodeColorMapping(map, "package",  &CT.keyword);
    addNodeColorMapping(map, "when",     &CT.keyword);
    addNodeColorMapping(map, "where",    &CT.keyword);
    addNodeColorMapping(map, "const",    &CT.keyword);
    addNodeColorMapping(map, "using",    &CT.keyword);
    addNodeColorMapping(map, "transmute",&CT.keyword);
    addNodeColorMapping(map, "cast",     &CT.keyword);
    addNodeColorMapping(map, "distinct", &CT.keyword);
    addNodeColorMapping(map, "defer",    &CT.keyword);
    addNodeColorMapping(map, "auto_cast",&CT.keyword);
    
    // Control flow
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "else",     &CT.keyword);
    addNodeColorMapping(map, "for",      &CT.keyword);
    addNodeColorMapping(map, "switch",   &CT.keyword);
    addNodeColorMapping(map, "case",     &CT.keyword);
    addNodeColorMapping(map, "break",    &CT.keyword);
    addNodeColorMapping(map, "continue", &CT.keyword);
    addNodeColorMapping(map, "fallthrough", &CT.keyword);
    addNodeColorMapping(map, "return",   &CT.keyword);
    addNodeColorMapping(map, "do",       &CT.keyword);
    
    // Type definitions
    addNodeColorMapping(map, "struct",   &CT.keyword);
    addNodeColorMapping(map, "enum",     &CT.keyword);
    addNodeColorMapping(map, "union",    &CT.keyword);
    addNodeColorMapping(map, "bit_field",&CT.keyword);
    addNodeColorMapping(map, "bit_set",  &CT.keyword);
    addNodeColorMapping(map, "proc",     &CT.keyword);
    
    // Memory keywords
    addNodeColorMapping(map, "new",      &CT.keyword);
    addNodeColorMapping(map, "delete",   &CT.keyword);
    addNodeColorMapping(map, "free",     &CT.keyword);
    addNodeColorMapping(map, "make",     &CT.keyword);
    addNodeColorMapping(map, "alloc",    &CT.keyword);
    
    // Basic types
    addNodeColorMapping(map, "int",      &CT.type);
    addNodeColorMapping(map, "i8",       &CT.type);
    addNodeColorMapping(map, "u8",       &CT.type);
    addNodeColorMapping(map, "i16",      &CT.type);
    addNodeColorMapping(map, "u16",      &CT.type);
    addNodeColorMapping(map, "i32",      &CT.type);
    addNodeColorMapping(map, "u32",      &CT.type);
    addNodeColorMapping(map, "i64",      &CT.type);
    addNodeColorMapping(map, "u64",      &CT.type);
    addNodeColorMapping(map, "i128",     &CT.type);
    addNodeColorMapping(map, "u128",     &CT.type);
    addNodeColorMapping(map, "int",      &CT.type);
    addNodeColorMapping(map, "uint",     &CT.type);
    addNodeColorMapping(map, "uintptr",  &CT.type);
    addNodeColorMapping(map, "rawptr",   &CT.type);
    addNodeColorMapping(map, "f16",      &CT.type);
    addNodeColorMapping(map, "f32",      &CT.type);
    addNodeColorMapping(map, "f64",      &CT.type);
    addNodeColorMapping(map, "complex32",&CT.type);
    addNodeColorMapping(map, "complex64",&CT.type);
    addNodeColorMapping(map, "complex128", &CT.type);
    addNodeColorMapping(map, "quaternion64", &CT.type);
    addNodeColorMapping(map, "quaternion128", &CT.type);
    addNodeColorMapping(map, "quaternion256", &CT.type);
    addNodeColorMapping(map, "bool",     &CT.type);
    addNodeColorMapping(map, "b8",       &CT.type);
    addNodeColorMapping(map, "b16",      &CT.type);
    addNodeColorMapping(map, "b32",      &CT.type);
    addNodeColorMapping(map, "b64",      &CT.type);
    addNodeColorMapping(map, "string",   &CT.type);
    addNodeColorMapping(map, "cstring",  &CT.type);
    addNodeColorMapping(map, "rune",     &CT.type);
    addNodeColorMapping(map, "any",      &CT.type);
    addNodeColorMapping(map, "typeid",   &CT.type);
    
    // Builtin values
    addNodeColorMapping(map, "nil",      &CT.null);
    addNodeColorMapping(map, "null",     &CT.null);
    addNodeColorMapping(map, "true",     &CT.null);
    addNodeColorMapping(map, "false",    &CT.null);
    
    // Operators
    addNodeColorMapping(map, "!",        &CT.negation);
    addNodeColorMapping(map, "in",       &CT.keyword);
    addNodeColorMapping(map, "not_in",   &CT.keyword);
    
    // Node types
    addNodeColorMapping(map, "type_identifier",     &CT.type);
    addNodeColorMapping(map, "procedure_definition", &CT.type);
    addNodeColorMapping(map, "primitive_type",      &CT.type);
    addNodeColorMapping(map, "struct_declaration",  &CT.type);
    
    // Literals
    addNodeColorMapping(map, "string_literal",     &CT.string);
    addNodeColorMapping(map, "raw_string_literal", &CT.string);
    addNodeColorMapping(map, "char_literal",       &CT.string);
    addNodeColorMapping(map, "string_content",     &CT.string);
    addNodeColorMapping(map, "\"",                 &CT.string);
    addNodeColorMapping(map, "integer_literal",    &CT.number);
    addNodeColorMapping(map, "float_literal",      &CT.number);
    addNodeColorMapping(map, "hex_literal",        &CT.number);
    addNodeColorMapping(map, "binary_literal",     &CT.number);
    addNodeColorMapping(map, "octal_literal",      &CT.number);
    
    // Functions/Procedures
    addNodeColorMapping(map, "procedure_definition",  &CT.function);
    addNodeColorMapping(map, "procedure_declaration", &CT.function);
    
    // Attributes
    addNodeColorMapping(map, "@thread_local", &CT.preprocessor);
    addNodeColorMapping(map, "@require",      &CT.preprocessor);
    addNodeColorMapping(map, "@private",      &CT.preprocessor);
    addNodeColorMapping(map, "@file_scope",   &CT.preprocessor);
    addNodeColorMapping(map, "@export",       &CT.preprocessor);
    addNodeColorMapping(map, "@builtin",      &CT.preprocessor);
    addNodeColorMapping(map, "@inline",       &CT.preprocessor);
    addNodeColorMapping(map, "@no_inline",    &CT.preprocessor);
    
    // Built-in procedures
    addNodeColorMapping(map, "len",           &CT.function);
    addNodeColorMapping(map, "cap",           &CT.function);
    addNodeColorMapping(map, "size_of",       &CT.function);
    addNodeColorMapping(map, "align_of",      &CT.function);
    addNodeColorMapping(map, "type_of",       &CT.function);
    addNodeColorMapping(map, "offset_of",     &CT.function);
    addNodeColorMapping(map, "context",       &CT.function);
    
    // Expressions
    addNodeColorMapping(map, "assignment_expression", &CT.cursor);
    addNodeColorMapping(map, "binary_expression",    &CT.cursor);
    addNodeColorMapping(map, "unary_expression",     &CT.cursor);
    addNodeColorMapping(map, "update_expression",    &CT.cursor);
    
    // Identifiers with context
    addNodeColorMapping(map, "identifier:procedure_declaration", &CT.function);
    addNodeColorMapping(map, "identifier:procedure_definition",  &CT.function);
    addNodeColorMapping(map, "identifier:declaration",          &CT.variable);
    addNodeColorMapping(map, "identifier:variable_declaration", &CT.variable);
    addNodeColorMapping(map, "identifier:assignment_expression",&CT.variable);
    addNodeColorMapping(map, "identifier:field_declaration",    &CT.variable);
    addNodeColorMapping(map, "identifier",                      &CT.text);
    
    // Comments
    addNodeColorMapping(map, "comment",           &CT.comment);
    addNodeColorMapping(map, "line_comment",      &CT.comment);
    addNodeColorMapping(map, "multiline_comment", &CT.comment);
    addNodeColorMapping(map, "doc_comment",       &CT.comment);
}

void initMakeNodeColorMappings(NodeColorMap **map) {
    // Makefile directives
    addNodeColorMapping(map, "define",      &CT.keyword);
    addNodeColorMapping(map, "endef",       &CT.keyword);
    addNodeColorMapping(map, "include",     &CT.keyword);
    addNodeColorMapping(map, "-include",    &CT.keyword);
    addNodeColorMapping(map, "sinclude",    &CT.keyword);
    addNodeColorMapping(map, "override",    &CT.keyword);
    addNodeColorMapping(map, "export",      &CT.keyword);
    addNodeColorMapping(map, "unexport",    &CT.keyword);
    addNodeColorMapping(map, "ifdef",       &CT.keyword);
    addNodeColorMapping(map, "ifndef",      &CT.keyword);
    addNodeColorMapping(map, "ifeq",        &CT.keyword);
    addNodeColorMapping(map, "ifneq",       &CT.keyword);
    addNodeColorMapping(map, "else",        &CT.keyword);
    addNodeColorMapping(map, "endif",       &CT.keyword);
    addNodeColorMapping(map, "vpath",       &CT.keyword);

    // Special targets
    addNodeColorMapping(map, ".PHONY",      &CT.preprocessor);
    addNodeColorMapping(map, ".SUFFIXES",   &CT.preprocessor);
    addNodeColorMapping(map, ".DEFAULT",    &CT.preprocessor);
    addNodeColorMapping(map, ".PRECIOUS",   &CT.preprocessor);
    addNodeColorMapping(map, ".INTERMEDIATE", &CT.preprocessor);
    addNodeColorMapping(map, ".SECONDARY",  &CT.preprocessor);
    addNodeColorMapping(map, ".SECONDEXPANSION", &CT.preprocessor);
    addNodeColorMapping(map, ".DELETE_ON_ERROR", &CT.preprocessor);
    addNodeColorMapping(map, ".IGNORE",     &CT.preprocessor);
    addNodeColorMapping(map, ".SILENT",     &CT.preprocessor);
    addNodeColorMapping(map, ".EXPORT_ALL_VARIABLES", &CT.preprocessor);
    addNodeColorMapping(map, ".NOTPARALLEL", &CT.preprocessor);
    addNodeColorMapping(map, ".ONESHELL",   &CT.preprocessor);
    addNodeColorMapping(map, ".POSIX",      &CT.preprocessor);

    // Operators and special characters
    addNodeColorMapping(map, "!",           &CT.negation);
    addNodeColorMapping(map, "=",           &CT.cursor);
    addNodeColorMapping(map, ":=",          &CT.cursor);
    addNodeColorMapping(map, "::=",         &CT.cursor);
    addNodeColorMapping(map, "+=",          &CT.cursor);
    addNodeColorMapping(map, "?=",          &CT.cursor);
    addNodeColorMapping(map, "!=",          &CT.cursor);
    addNodeColorMapping(map, "$",           &CT.variable);
    addNodeColorMapping(map, "$(",          &CT.variable);
    addNodeColorMapping(map, "${",          &CT.variable);
    addNodeColorMapping(map, "$$",          &CT.variable);

    // Rule-related elements
    addNodeColorMapping(map, "rule",         &CT.function);
    addNodeColorMapping(map, "target",       &CT.function);
    addNodeColorMapping(map, "prerequisite", &CT.variable);
    addNodeColorMapping(map, "recipe",       &CT.text);
    addNodeColorMapping(map, "recipe_prefix", &CT.preprocessor);
    
    // Common built-in functions
    addNodeColorMapping(map, "subst",       &CT.function);
    addNodeColorMapping(map, "patsubst",    &CT.function);
    addNodeColorMapping(map, "strip",       &CT.function);
    addNodeColorMapping(map, "findstring",  &CT.function);
    addNodeColorMapping(map, "filter",      &CT.function);
    addNodeColorMapping(map, "filter-out",  &CT.function);
    addNodeColorMapping(map, "sort",        &CT.function);
    addNodeColorMapping(map, "word",        &CT.function);
    addNodeColorMapping(map, "wordlist",    &CT.function);
    addNodeColorMapping(map, "words",       &CT.function);
    addNodeColorMapping(map, "firstword",   &CT.function);
    addNodeColorMapping(map, "lastword",    &CT.function);
    addNodeColorMapping(map, "dir",         &CT.function);
    addNodeColorMapping(map, "notdir",      &CT.function);
    addNodeColorMapping(map, "suffix",      &CT.function);
    addNodeColorMapping(map, "basename",    &CT.function);
    addNodeColorMapping(map, "addsuffix",   &CT.function);
    addNodeColorMapping(map, "addprefix",   &CT.function);
    addNodeColorMapping(map, "join",        &CT.function);
    addNodeColorMapping(map, "wildcard",    &CT.function);
    addNodeColorMapping(map, "realpath",    &CT.function);
    addNodeColorMapping(map, "abspath",     &CT.function);
    addNodeColorMapping(map, "if",          &CT.function);
    addNodeColorMapping(map, "or",          &CT.function);
    addNodeColorMapping(map, "and",         &CT.function);
    addNodeColorMapping(map, "foreach",     &CT.function);
    addNodeColorMapping(map, "call",        &CT.function);
    addNodeColorMapping(map, "value",       &CT.function);
    addNodeColorMapping(map, "eval",        &CT.function);
    addNodeColorMapping(map, "origin",      &CT.function);
    addNodeColorMapping(map, "flavor",      &CT.function);
    addNodeColorMapping(map, "shell",       &CT.function);
    addNodeColorMapping(map, "error",       &CT.function);
    addNodeColorMapping(map, "warning",     &CT.function);
    addNodeColorMapping(map, "info",        &CT.function);

    // Special automatic variables
    addNodeColorMapping(map, "$@",          &CT.variable);
    addNodeColorMapping(map, "$<",          &CT.variable);
    addNodeColorMapping(map, "$^",          &CT.variable);
    addNodeColorMapping(map, "$+",          &CT.variable);
    addNodeColorMapping(map, "$*",          &CT.variable);
    addNodeColorMapping(map, "$?",          &CT.variable);
    addNodeColorMapping(map, "$|",          &CT.variable);
    addNodeColorMapping(map, "$%",          &CT.variable);
    addNodeColorMapping(map, "$D",          &CT.variable);
    addNodeColorMapping(map, "$F",          &CT.variable);

    // Node types
    addNodeColorMapping(map, "variable_name", &CT.variable);
    addNodeColorMapping(map, "variable_reference", &CT.variable);
    addNodeColorMapping(map, "function_call", &CT.function);
    addNodeColorMapping(map, "substitution", &CT.function);
    addNodeColorMapping(map, "expansion",   &CT.variable);
    addNodeColorMapping(map, "string_literal", &CT.string);
    addNodeColorMapping(map, "string_content", &CT.string);
    addNodeColorMapping(map, "\"",          &CT.string);
    addNodeColorMapping(map, "pattern",     &CT.string);
    addNodeColorMapping(map, "wildcard",    &CT.string);

    // Shell commands
    addNodeColorMapping(map, "shell_command", &CT.text);
    addNodeColorMapping(map, "shell_content", &CT.text);
    
    // Identifiers with context
    addNodeColorMapping(map, "identifier:target", &CT.function);
    addNodeColorMapping(map, "identifier:variable_assignment", &CT.variable);
    addNodeColorMapping(map, "identifier",  &CT.text);
    
    // Comments
    addNodeColorMapping(map, "comment",     &CT.comment);
    addNodeColorMapping(map, "line_comment", &CT.comment);
}

void initCommonlispNodeColorMappings(NodeColorMap **map) {
    // Core special forms
    addNodeColorMapping(map, "quote",    &CT.keyword);
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "progn",    &CT.keyword);
    addNodeColorMapping(map, "prog1",    &CT.keyword);
    addNodeColorMapping(map, "prog2",    &CT.keyword);
    addNodeColorMapping(map, "let",      &CT.keyword);
    addNodeColorMapping(map, "let*",     &CT.keyword);
    addNodeColorMapping(map, "lambda",   &CT.keyword);
    addNodeColorMapping(map, "function", &CT.keyword);
    addNodeColorMapping(map, "setq",     &CT.keyword);
    addNodeColorMapping(map, "setf",     &CT.keyword);
    addNodeColorMapping(map, "defun",    &CT.keyword);
    addNodeColorMapping(map, "defvar",   &CT.keyword);
    addNodeColorMapping(map, "defparameter", &CT.keyword);
    addNodeColorMapping(map, "defconstant", &CT.keyword);
    addNodeColorMapping(map, "defmacro", &CT.keyword);
    addNodeColorMapping(map, "defstruct", &CT.keyword);
    addNodeColorMapping(map, "defclass", &CT.keyword);
    addNodeColorMapping(map, "defmethod", &CT.keyword);
    addNodeColorMapping(map, "defgeneric", &CT.keyword);
    
    // Control flow
    addNodeColorMapping(map, "cond",     &CT.keyword);
    addNodeColorMapping(map, "when",     &CT.keyword);
    addNodeColorMapping(map, "unless",   &CT.keyword);
    addNodeColorMapping(map, "case",     &CT.keyword);
    addNodeColorMapping(map, "ecase",    &CT.keyword);
    addNodeColorMapping(map, "typecase", &CT.keyword);
    addNodeColorMapping(map, "etypecase", &CT.keyword);
    addNodeColorMapping(map, "loop",     &CT.keyword);
    addNodeColorMapping(map, "do",       &CT.keyword);
    addNodeColorMapping(map, "do*",      &CT.keyword);
    addNodeColorMapping(map, "dolist",   &CT.keyword);
    addNodeColorMapping(map, "dotimes",  &CT.keyword);
    addNodeColorMapping(map, "block",    &CT.keyword);
    addNodeColorMapping(map, "return",   &CT.keyword);
    addNodeColorMapping(map, "return-from", &CT.keyword);
    addNodeColorMapping(map, "catch",    &CT.keyword);
    addNodeColorMapping(map, "throw",    &CT.keyword);
    addNodeColorMapping(map, "handler-case", &CT.keyword);
    addNodeColorMapping(map, "handler-bind", &CT.keyword);
    addNodeColorMapping(map, "unwind-protect", &CT.keyword);
    
    // Loop keywords
    addNodeColorMapping(map, "for",      &CT.keyword);
    addNodeColorMapping(map, "from",     &CT.keyword);
    addNodeColorMapping(map, "to",       &CT.keyword);
    addNodeColorMapping(map, "downto",   &CT.keyword);
    addNodeColorMapping(map, "by",       &CT.keyword);
    addNodeColorMapping(map, "across",   &CT.keyword);
    addNodeColorMapping(map, "in",       &CT.keyword);
    addNodeColorMapping(map, "on",       &CT.keyword);
    addNodeColorMapping(map, "collect",  &CT.keyword);
    addNodeColorMapping(map, "append",   &CT.keyword);
    addNodeColorMapping(map, "nconc",    &CT.keyword);
    addNodeColorMapping(map, "sum",      &CT.keyword);
    addNodeColorMapping(map, "count",    &CT.keyword);
    addNodeColorMapping(map, "maximize", &CT.keyword);
    addNodeColorMapping(map, "minimize", &CT.keyword);
    addNodeColorMapping(map, "finally",  &CT.keyword);
    addNodeColorMapping(map, "repeat",   &CT.keyword);
    addNodeColorMapping(map, "while",    &CT.keyword);
    addNodeColorMapping(map, "until",    &CT.keyword);
    addNodeColorMapping(map, "always",   &CT.keyword);
    addNodeColorMapping(map, "never",    &CT.keyword);
    addNodeColorMapping(map, "thereis",  &CT.keyword);
    
    // Package system
    addNodeColorMapping(map, "in-package", &CT.keyword);
    addNodeColorMapping(map, "defpackage", &CT.keyword);
    addNodeColorMapping(map, "use-package", &CT.keyword);
    addNodeColorMapping(map, "export",    &CT.keyword);
    addNodeColorMapping(map, "import",    &CT.keyword);
    addNodeColorMapping(map, "require",   &CT.keyword);
    addNodeColorMapping(map, "provide",   &CT.keyword);
    
    // CLOS related
    addNodeColorMapping(map, "make-instance", &CT.function);
    addNodeColorMapping(map, "slot-value", &CT.function);
    addNodeColorMapping(map, "with-slots", &CT.keyword);
    addNodeColorMapping(map, "with-accessors", &CT.keyword);
    
    // Type-related
    addNodeColorMapping(map, "declare",  &CT.keyword);
    addNodeColorMapping(map, "proclaim", &CT.keyword);
    addNodeColorMapping(map, "declaim",  &CT.keyword);
    addNodeColorMapping(map, "type",     &CT.type);
    addNodeColorMapping(map, "the",      &CT.keyword);
    addNodeColorMapping(map, "satisfies", &CT.keyword);
    addNodeColorMapping(map, "typep",    &CT.function);
    
    // Built-in types
    addNodeColorMapping(map, "nil",      &CT.null);
    addNodeColorMapping(map, "t",        &CT.null);
    addNodeColorMapping(map, "integer",  &CT.type);
    addNodeColorMapping(map, "string",   &CT.type);
    addNodeColorMapping(map, "symbol",   &CT.type);
    addNodeColorMapping(map, "keyword",  &CT.type);
    addNodeColorMapping(map, "list",     &CT.type);
    addNodeColorMapping(map, "vector",   &CT.type);
    addNodeColorMapping(map, "array",    &CT.type);
    addNodeColorMapping(map, "character", &CT.type);
    addNodeColorMapping(map, "hash-table", &CT.type);
    addNodeColorMapping(map, "package",  &CT.type);
    addNodeColorMapping(map, "pathname", &CT.type);
    addNodeColorMapping(map, "stream",   &CT.type);
    addNodeColorMapping(map, "function", &CT.type);
    addNodeColorMapping(map, "sequence", &CT.type);
    
    // Reader macros and special syntax
    addNodeColorMapping(map, "#'",       &CT.preprocessor);
    addNodeColorMapping(map, "#\\",      &CT.preprocessor);
    addNodeColorMapping(map, "#(",       &CT.preprocessor);
    addNodeColorMapping(map, "#:",       &CT.preprocessor);
    addNodeColorMapping(map, ":",        &CT.preprocessor); // For keywords
    addNodeColorMapping(map, "'",        &CT.preprocessor);
    addNodeColorMapping(map, "`",        &CT.preprocessor);
    addNodeColorMapping(map, ",",        &CT.preprocessor);
    addNodeColorMapping(map, ",@",       &CT.preprocessor);
    
    // Common functions
    addNodeColorMapping(map, "car",      &CT.function);
    addNodeColorMapping(map, "cdr",      &CT.function);
    addNodeColorMapping(map, "cons",     &CT.function);
    addNodeColorMapping(map, "list",     &CT.function);
    addNodeColorMapping(map, "append",   &CT.function);
    addNodeColorMapping(map, "concatenate", &CT.function);
    addNodeColorMapping(map, "format",   &CT.function);
    addNodeColorMapping(map, "print",    &CT.function);
    addNodeColorMapping(map, "princ",    &CT.function);
    addNodeColorMapping(map, "prin1",    &CT.function);
    addNodeColorMapping(map, "read",     &CT.function);
    addNodeColorMapping(map, "read-line", &CT.function);
    addNodeColorMapping(map, "mapcar",   &CT.function);
    addNodeColorMapping(map, "mapc",     &CT.function);
    addNodeColorMapping(map, "maplist",  &CT.function);
    addNodeColorMapping(map, "find",     &CT.function);
    addNodeColorMapping(map, "find-if",  &CT.function);
    addNodeColorMapping(map, "position", &CT.function);
    addNodeColorMapping(map, "remove",   &CT.function);
    addNodeColorMapping(map, "remove-if", &CT.function);
    addNodeColorMapping(map, "member",   &CT.function);
    addNodeColorMapping(map, "assoc",    &CT.function);
    
    // Node types
    addNodeColorMapping(map, "list",           &CT.text);
    addNodeColorMapping(map, "program",        &CT.text);
    addNodeColorMapping(map, "symbol",         &CT.text);
    addNodeColorMapping(map, "keyword",        &CT.preprocessor);
    addNodeColorMapping(map, "quoted_symbol",  &CT.variable);
    addNodeColorMapping(map, "function_name",  &CT.function);
    addNodeColorMapping(map, "definition_name", &CT.function);
    addNodeColorMapping(map, "variable_name",  &CT.variable);
    addNodeColorMapping(map, "parameter_name", &CT.variable);
    
    // Literals
    addNodeColorMapping(map, "string",        &CT.string);
    addNodeColorMapping(map, "string_content", &CT.string);
    addNodeColorMapping(map, "character",     &CT.string);
    addNodeColorMapping(map, "integer",       &CT.number);
    addNodeColorMapping(map, "float",         &CT.number);
    addNodeColorMapping(map, "ratio",         &CT.number);
    addNodeColorMapping(map, "complex",       &CT.number);
    
    // Comments
    addNodeColorMapping(map, "comment",       &CT.comment);
    addNodeColorMapping(map, "line_comment",  &CT.comment);
    addNodeColorMapping(map, ";",             &CT.comment);
    
    // Special context identifiers
    addNodeColorMapping(map, "symbol:defun", &CT.function);
    addNodeColorMapping(map, "symbol:defmacro", &CT.function);
    addNodeColorMapping(map, "symbol:defmethod", &CT.function);
    addNodeColorMapping(map, "symbol:defvar", &CT.variable);
    addNodeColorMapping(map, "symbol:defparameter", &CT.variable);
    addNodeColorMapping(map, "symbol:defconstant", &CT.variable);
    addNodeColorMapping(map, "symbol:defstruct", &CT.type);
    addNodeColorMapping(map, "symbol:defclass", &CT.type);
}

void initScssNodeColorMappings(NodeColorMap **map) {
    // SCSS Keywords
    addNodeColorMapping(map, "@mixin", &CT.keyword);
    addNodeColorMapping(map, "@include", &CT.keyword);
    addNodeColorMapping(map, "@function", &CT.keyword);
    addNodeColorMapping(map, "@return", &CT.keyword);
    addNodeColorMapping(map, "@if", &CT.keyword);
    addNodeColorMapping(map, "@else", &CT.keyword);
    addNodeColorMapping(map, "@for", &CT.keyword);
    addNodeColorMapping(map, "@each", &CT.keyword);
    addNodeColorMapping(map, "@while", &CT.keyword);
    addNodeColorMapping(map, "@extend", &CT.keyword);
    addNodeColorMapping(map, "@import", &CT.keyword);
    addNodeColorMapping(map, "@use", &CT.keyword);
    addNodeColorMapping(map, "@forward", &CT.keyword);
    addNodeColorMapping(map, "@at-root", &CT.keyword);
    addNodeColorMapping(map, "@debug", &CT.keyword);
    addNodeColorMapping(map, "@warn", &CT.keyword);
    addNodeColorMapping(map, "@error", &CT.keyword);

    // SCSS Variables
    addNodeColorMapping(map, "$", &CT.variable);

    // SCSS Selectors
    addNodeColorMapping(map, ".", &CT.type); // Class selector
    addNodeColorMapping(map, "#", &CT.type); // ID selector
    addNodeColorMapping(map, "&", &CT.type); // Parent selector

    // SCSS Properties and Values
    addNodeColorMapping(map, ":", &CT.keyword); // Property-value separator
    addNodeColorMapping(map, ";", &CT.keyword); // Statement terminator

    // SCSS Strings
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "\"", &CT.string); // Double-quoted string
    addNodeColorMapping(map, "'", &CT.string); // Single-quoted string

    // SCSS Numbers
    addNodeColorMapping(map, "number", &CT.number);

    // SCSS Functions
    addNodeColorMapping(map, "function", &CT.function);
    addNodeColorMapping(map, "calc", &CT.function);
    addNodeColorMapping(map, "rgb", &CT.function);
    addNodeColorMapping(map, "rgba", &CT.function);
    addNodeColorMapping(map, "hsl", &CT.function);
    addNodeColorMapping(map, "hsla", &CT.function);
    addNodeColorMapping(map, "darken", &CT.function);
    addNodeColorMapping(map, "lighten", &CT.function);
    addNodeColorMapping(map, "mix", &CT.function);
    addNodeColorMapping(map, "transparentize", &CT.function);
    addNodeColorMapping(map, "opacify", &CT.function);

    // SCSS Comments
    addNodeColorMapping(map, "//", &CT.comment); // Single-line comment
    addNodeColorMapping(map, "/*", &CT.comment); // Multi-line comment start
    addNodeColorMapping(map, "*/", &CT.comment); // Multi-line comment end

    // SCSS Operators
    addNodeColorMapping(map, "+", &CT.operator);
    addNodeColorMapping(map, "-", &CT.operator);
    addNodeColorMapping(map, "*", &CT.operator);
    addNodeColorMapping(map, "/", &CT.operator);
    addNodeColorMapping(map, "%", &CT.operator);
    addNodeColorMapping(map, "==", &CT.operator);
    addNodeColorMapping(map, "!=", &CT.operator);
    addNodeColorMapping(map, ">", &CT.operator);
    addNodeColorMapping(map, "<", &CT.operator);
    addNodeColorMapping(map, ">=", &CT.operator);
    addNodeColorMapping(map, "<=", &CT.operator);
    addNodeColorMapping(map, "and", &CT.operator);
    addNodeColorMapping(map, "or", &CT.operator);
    addNodeColorMapping(map, "not", &CT.operator);

    // SCSS Interpolation
    addNodeColorMapping(map, "#{", &CT.variable); // Interpolation start
    addNodeColorMapping(map, "}", &CT.variable); // Interpolation end

    // SCSS Placeholders
    addNodeColorMapping(map, "%", &CT.type); // Placeholder selector

    // SCSS At-Rules
    addNodeColorMapping(map, "@media", &CT.keyword);
    addNodeColorMapping(map, "@keyframes", &CT.keyword);
    addNodeColorMapping(map, "@supports", &CT.keyword);
    addNodeColorMapping(map, "@font-face", &CT.keyword);

    // SCSS Error Handling
    addNodeColorMapping(map, "error", &CT.error);

    // SCSS Miscellaneous
    addNodeColorMapping(map, "url", &CT.string); // URL function
    addNodeColorMapping(map, "important", &CT.keyword); // !important
    addNodeColorMapping(map, "default", &CT.keyword); // Default keyword
    addNodeColorMapping(map, "global", &CT.keyword); // Global keyword
    addNodeColorMapping(map, "optional", &CT.keyword); // Optional keyword

    // SCSS Whitespace and Indentation
    addNodeColorMapping(map, "indent", &CT.text);
    addNodeColorMapping(map, "whitespace", &CT.text);
}

void initHaskellNodeColorMappings(NodeColorMap **map) {
    // Haskell Keywords
    addNodeColorMapping(map, "module", &CT.keyword);
    addNodeColorMapping(map, "import", &CT.keyword);
    addNodeColorMapping(map, "where", &CT.keyword);
    addNodeColorMapping(map, "let", &CT.keyword);
    addNodeColorMapping(map, "in", &CT.keyword);
    addNodeColorMapping(map, "if", &CT.keyword);
    addNodeColorMapping(map, "then", &CT.keyword);
    addNodeColorMapping(map, "else", &CT.keyword);
    addNodeColorMapping(map, "case", &CT.keyword);
    addNodeColorMapping(map, "of", &CT.keyword);
    addNodeColorMapping(map, "data", &CT.keyword);
    addNodeColorMapping(map, "type", &CT.keyword);
    addNodeColorMapping(map, "newtype", &CT.keyword);
    addNodeColorMapping(map, "class", &CT.keyword);
    addNodeColorMapping(map, "instance", &CT.keyword);
    addNodeColorMapping(map, "deriving", &CT.keyword);
    addNodeColorMapping(map, "do", &CT.keyword);
    addNodeColorMapping(map, "mdo", &CT.keyword);
    addNodeColorMapping(map, "rec", &CT.keyword);
    addNodeColorMapping(map, "foreign", &CT.keyword);
    addNodeColorMapping(map, "infix", &CT.keyword);
    addNodeColorMapping(map, "infixl", &CT.keyword);
    addNodeColorMapping(map, "infixr", &CT.keyword);

    // Haskell Types
    addNodeColorMapping(map, "Int", &CT.type);
    addNodeColorMapping(map, "Integer", &CT.type);
    addNodeColorMapping(map, "Float", &CT.type);
    addNodeColorMapping(map, "Double", &CT.type);
    addNodeColorMapping(map, "Char", &CT.type);
    addNodeColorMapping(map, "String", &CT.type);
    addNodeColorMapping(map, "Bool", &CT.type);
    addNodeColorMapping(map, "Maybe", &CT.type);
    addNodeColorMapping(map, "Either", &CT.type);
    addNodeColorMapping(map, "IO", &CT.type);
    addNodeColorMapping(map, "List", &CT.type);

    // Haskell Operators
    addNodeColorMapping(map, "->", &CT.operator); // Function arrow
    addNodeColorMapping(map, "=>", &CT.operator); // Class constraint arrow
    addNodeColorMapping(map, "::", &CT.operator); // Type annotation
    addNodeColorMapping(map, "=", &CT.operator);  // Equality/definition
    addNodeColorMapping(map, "==", &CT.operator); // Equality comparison
    addNodeColorMapping(map, "/=", &CT.operator); // Inequality comparison
    addNodeColorMapping(map, "<", &CT.operator);
    addNodeColorMapping(map, ">", &CT.operator);
    addNodeColorMapping(map, "<=", &CT.operator);
    addNodeColorMapping(map, ">=", &CT.operator);
    addNodeColorMapping(map, "+", &CT.operator);
    addNodeColorMapping(map, "-", &CT.operator);
    addNodeColorMapping(map, "*", &CT.operator);
    addNodeColorMapping(map, "/", &CT.operator);
    addNodeColorMapping(map, "&&", &CT.operator); // Logical AND
    addNodeColorMapping(map, "||", &CT.operator); // Logical OR
    addNodeColorMapping(map, "++", &CT.operator); // List concatenation
    addNodeColorMapping(map, "!!", &CT.operator); // List indexing

    // Haskell Strings and Characters
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "\"", &CT.string); // Double-quoted string
    addNodeColorMapping(map, "char", &CT.string);
    addNodeColorMapping(map, "'", &CT.string); // Single-quoted character

    // Haskell Numbers
    addNodeColorMapping(map, "number", &CT.number);

    // Haskell Comments
    addNodeColorMapping(map, "--", &CT.comment); // Single-line comment
    addNodeColorMapping(map, "{-", &CT.comment); // Multi-line comment start
    addNodeColorMapping(map, "-}", &CT.comment); // Multi-line comment end

    // Haskell Functions
    addNodeColorMapping(map, "function", &CT.function);
    addNodeColorMapping(map, "map", &CT.function);
    addNodeColorMapping(map, "filter", &CT.function);
    addNodeColorMapping(map, "foldl", &CT.function);
    addNodeColorMapping(map, "foldr", &CT.function);
    addNodeColorMapping(map, "zip", &CT.function);
    addNodeColorMapping(map, "unzip", &CT.function);
    addNodeColorMapping(map, "head", &CT.function);
    addNodeColorMapping(map, "tail", &CT.function);
    addNodeColorMapping(map, "init", &CT.function);
    addNodeColorMapping(map, "last", &CT.function);

    // Haskell List Comprehensions
    addNodeColorMapping(map, "[", &CT.operator); // List start
    addNodeColorMapping(map, "]", &CT.operator); // List end
    addNodeColorMapping(map, "|", &CT.operator); // List comprehension separator

    // Haskell Type Constructors
    addNodeColorMapping(map, "Just", &CT.type);
    addNodeColorMapping(map, "Nothing", &CT.type);
    addNodeColorMapping(map, "Left", &CT.type);
    addNodeColorMapping(map, "Right", &CT.type);

    // Haskell Error Handling
    addNodeColorMapping(map, "error", &CT.error);

    // Haskell Miscellaneous
    addNodeColorMapping(map, "_", &CT.text); // Wildcard
    addNodeColorMapping(map, "..", &CT.text); // Range
    addNodeColorMapping(map, "\\", &CT.operator); // Lambda
    addNodeColorMapping(map, "`", &CT.operator); // Infix function

    // Haskell Whitespace and Indentation
    addNodeColorMapping(map, "indent", &CT.text);
    addNodeColorMapping(map, "whitespace", &CT.text);
}

void initLuaNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "return",   &CT.keyword);
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "else",     &CT.keyword);
    addNodeColorMapping(map, "elseif",   &CT.keyword);
    addNodeColorMapping(map, "while",    &CT.keyword);
    addNodeColorMapping(map, "repeat",   &CT.keyword);
    addNodeColorMapping(map, "until",    &CT.keyword);
    addNodeColorMapping(map, "for",      &CT.keyword);
    addNodeColorMapping(map, "in",       &CT.keyword);
    addNodeColorMapping(map, "function", &CT.keyword);
    addNodeColorMapping(map, "local",    &CT.keyword);
    addNodeColorMapping(map, "end",      &CT.keyword);
    addNodeColorMapping(map, "do",       &CT.keyword);
    addNodeColorMapping(map, "then",     &CT.keyword);
    addNodeColorMapping(map, "break",    &CT.keyword);
    addNodeColorMapping(map, "goto",     &CT.keyword);
    addNodeColorMapping(map, "nil",      &CT.null);
    addNodeColorMapping(map, "true",     &CT.null);
    addNodeColorMapping(map, "false",    &CT.null);

    // Operators
    addNodeColorMapping(map, "!",        &CT.negation);
    addNodeColorMapping(map, "not",      &CT.negation);
    addNodeColorMapping(map, "and",      &CT.operator);
    addNodeColorMapping(map, "or",       &CT.operator);
    addNodeColorMapping(map, "+",        &CT.operator);
    addNodeColorMapping(map, "-",        &CT.operator);
    addNodeColorMapping(map, "*",        &CT.operator);
    addNodeColorMapping(map, "/",        &CT.operator);
    addNodeColorMapping(map, "%",        &CT.operator);
    addNodeColorMapping(map, "^",        &CT.operator);
    addNodeColorMapping(map, "==",       &CT.operator);
    addNodeColorMapping(map, "~=",       &CT.operator);
    addNodeColorMapping(map, "<",        &CT.operator);
    addNodeColorMapping(map, "<=",       &CT.operator);
    addNodeColorMapping(map, ">",        &CT.operator);
    addNodeColorMapping(map, ">=",       &CT.operator);
    addNodeColorMapping(map, "..",       &CT.operator);
    addNodeColorMapping(map, "#",        &CT.operator);

    // Types and literals
    addNodeColorMapping(map, "string",        &CT.string);
    addNodeColorMapping(map, "string_content",&CT.string);
    addNodeColorMapping(map, "number",        &CT.number);
    addNodeColorMapping(map, "table",         &CT.type);
    addNodeColorMapping(map, "function",      &CT.function);
    addNodeColorMapping(map, "function_definition", &CT.function);
    addNodeColorMapping(map, "function_call", &CT.function);

    // Identifiers
    addNodeColorMapping(map, "identifier", &CT.text); // Default for identifiers
    addNodeColorMapping(map, "identifier:function_definition", &CT.function);
    addNodeColorMapping(map, "identifier:function_call",       &CT.function);
    addNodeColorMapping(map, "identifier:variable_declaration",&CT.variable);
    addNodeColorMapping(map, "identifier:assignment",          &CT.variable);

    // Comments
    addNodeColorMapping(map, "comment", &CT.comment);

    // Other
    addNodeColorMapping(map, "table_constructor", &CT.type);
    addNodeColorMapping(map, "field",            &CT.variable);
    addNodeColorMapping(map, "key",              &CT.variable);
    addNodeColorMapping(map, "value",            &CT.variable);
    addNodeColorMapping(map, "arguments",        &CT.text);
    addNodeColorMapping(map, "parameters",       &CT.text);
    addNodeColorMapping(map, "return_statement", &CT.keyword);
    addNodeColorMapping(map, "break_statement",  &CT.keyword);
    addNodeColorMapping(map, "label",            &CT.text);
}

void initRustNodeColorMappings(NodeColorMap **map) {
    // Keywords and control flow
    addNodeColorMapping(map, "fn", &CT.keyword);
    addNodeColorMapping(map, "let", &CT.keyword);
    addNodeColorMapping(map, "struct", &CT.keyword);
    addNodeColorMapping(map, "enum", &CT.keyword);
    addNodeColorMapping(map, "impl", &CT.keyword);
    addNodeColorMapping(map, "trait", &CT.keyword);
    addNodeColorMapping(map, "match", &CT.keyword);
    addNodeColorMapping(map, "if", &CT.keyword);
    addNodeColorMapping(map, "else", &CT.keyword);
    addNodeColorMapping(map, "loop", &CT.keyword);
    addNodeColorMapping(map, "while", &CT.keyword);
    addNodeColorMapping(map, "for", &CT.keyword);
    addNodeColorMapping(map, "in", &CT.keyword);
    addNodeColorMapping(map, "return", &CT.keyword);
    addNodeColorMapping(map, "break", &CT.keyword);
    addNodeColorMapping(map, "continue", &CT.keyword);
    addNodeColorMapping(map, "pub", &CT.keyword);
    addNodeColorMapping(map, "mod", &CT.keyword);
    addNodeColorMapping(map, "use", &CT.keyword);
    addNodeColorMapping(map, "async", &CT.keyword);
    addNodeColorMapping(map, "await", &CT.keyword);
    addNodeColorMapping(map, "dyn", &CT.keyword);
    addNodeColorMapping(map, "const", &CT.keyword);
    addNodeColorMapping(map, "static", &CT.keyword);
    addNodeColorMapping(map, "mut", &CT.keyword);
    addNodeColorMapping(map, "ref", &CT.keyword);
    addNodeColorMapping(map, "self", &CT.keyword);
    addNodeColorMapping(map, "Self", &CT.keyword);
    addNodeColorMapping(map, "where", &CT.keyword);
    addNodeColorMapping(map, "type", &CT.keyword);
    addNodeColorMapping(map, "as", &CT.keyword);
    addNodeColorMapping(map, "unsafe", &CT.keyword);
    addNodeColorMapping(map, "extern", &CT.keyword);
    addNodeColorMapping(map, "crate", &CT.keyword);

    // Types and type declarations
    addNodeColorMapping(map, "type_identifier", &CT.type);
    addNodeColorMapping(map, "primitive_type", &CT.type);
    addNodeColorMapping(map, "struct_item", &CT.type);
    addNodeColorMapping(map, "enum_item", &CT.type);
    addNodeColorMapping(map, "trait_item", &CT.type);
    addNodeColorMapping(map, "impl_item", &CT.type);
    addNodeColorMapping(map, "Result", &CT.type);
    addNodeColorMapping(map, "Option", &CT.type);
    addNodeColorMapping(map, "Box", &CT.type);
    addNodeColorMapping(map, "Vec", &CT.type);
    addNodeColorMapping(map, "String", &CT.type);
    addNodeColorMapping(map, "char", &CT.type);
    addNodeColorMapping(map, "str", &CT.type);
    addNodeColorMapping(map, "u8", &CT.type);
    addNodeColorMapping(map, "i32", &CT.type);
    addNodeColorMapping(map, "bool", &CT.type);
    addNodeColorMapping(map, "f64", &CT.type);

    // Functions and methods
    addNodeColorMapping(map, "function_item", &CT.function);
    addNodeColorMapping(map, "identifier:call_expression", &CT.function);
    addNodeColorMapping(map, "identifier:method_call", &CT.function);
    addNodeColorMapping(map, "identifier:function_item", &CT.function);

    // Macros
    addNodeColorMapping(map, "macro_invocation", &CT.preprocessor);
    addNodeColorMapping(map, "macro_definition", &CT.preprocessor);
    addNodeColorMapping(map, "println!", &CT.preprocessor);
    addNodeColorMapping(map, "format!", &CT.preprocessor);
    addNodeColorMapping(map, "vec!", &CT.preprocessor);

    // Literals
    addNodeColorMapping(map, "string_literal", &CT.string);
    addNodeColorMapping(map, "raw_string_literal", &CT.string);
    addNodeColorMapping(map, "char_literal", &CT.string);
    addNodeColorMapping(map, "integer_literal", &CT.number);
    addNodeColorMapping(map, "float_literal", &CT.number);
    addNodeColorMapping(map, "boolean_literal", &CT.null);

    // Comments
    addNodeColorMapping(map, "line_comment", &CT.comment);
    addNodeColorMapping(map, "block_comment", &CT.comment);

    // Operators and expressions
    addNodeColorMapping(map, "operator", &CT.operator);
    addNodeColorMapping(map, "binary_expression", &CT.operator);
    addNodeColorMapping(map, "unary_expression", &CT.operator);
    addNodeColorMapping(map, "assignment_expression", &CT.operator);
    addNodeColorMapping(map, "compound_assignment_expr", &CT.operator);

    // Variables and identifiers
    addNodeColorMapping(map, "identifier:let_declaration", &CT.variable);
    addNodeColorMapping(map, "identifier:parameter", &CT.variable);
    addNodeColorMapping(map, "field_identifier", &CT.variable);
    addNodeColorMapping(map, "identifier", &CT.text);

    // Error handling and special constructs
    addNodeColorMapping(map, "panic!", &CT.error);
    addNodeColorMapping(map, "assert!", &CT.error);
    addNodeColorMapping(map, "unreachable!", &CT.error);
    addNodeColorMapping(map, "todo!", &CT.warning);
    addNodeColorMapping(map, "Ok", &CT.success);
    addNodeColorMapping(map, "Err", &CT.error);
    addNodeColorMapping(map, "Some", &CT.success);
    addNodeColorMapping(map, "None", &CT.null);

    // Lifetime and modifiers
    addNodeColorMapping(map, "lifetime", &CT.keyword);
    addNodeColorMapping(map, "modifier", &CT.keyword);
    addNodeColorMapping(map, "visibility_modifier", &CT.keyword);
}


void initBashNodeColorMappings(NodeColorMap **map) {
    // Keywords and shell constructs
    addNodeColorMapping(map, "if", &CT.keyword);
    addNodeColorMapping(map, "then", &CT.keyword);
    addNodeColorMapping(map, "else", &CT.keyword);
    addNodeColorMapping(map, "elif", &CT.keyword);
    addNodeColorMapping(map, "fi", &CT.keyword);
    addNodeColorMapping(map, "for", &CT.keyword);
    addNodeColorMapping(map, "while", &CT.keyword);
    addNodeColorMapping(map, "until", &CT.keyword);
    addNodeColorMapping(map, "do", &CT.keyword);
    addNodeColorMapping(map, "done", &CT.keyword);
    addNodeColorMapping(map, "case", &CT.keyword);
    addNodeColorMapping(map, "esac", &CT.keyword);
    addNodeColorMapping(map, "function", &CT.keyword);
    addNodeColorMapping(map, "select", &CT.keyword);
    addNodeColorMapping(map, "export", &CT.keyword);
    addNodeColorMapping(map, "readonly", &CT.keyword);
    addNodeColorMapping(map, "unset", &CT.keyword);
    addNodeColorMapping(map, "alias", &CT.keyword);
    addNodeColorMapping(map, "local", &CT.keyword);

    // Shebang and preprocessing
    addNodeColorMapping(map, "shebang", &CT.preprocessor);
    addNodeColorMapping(map, "declaration_command", &CT.preprocessor);

    // Comments
    addNodeColorMapping(map, "comment", &CT.comment);

    // Strings and heredocs
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "raw_string", &CT.string);
    addNodeColorMapping(map, "heredoc_start", &CT.string);
    addNodeColorMapping(map, "heredoc_body", &CT.string);
    addNodeColorMapping(map, "string_expansion", &CT.string);
    addNodeColorMapping(map, "ansi_c_string", &CT.string);

    // Variables and parameters
    addNodeColorMapping(map, "variable_name", &CT.variable);
    addNodeColorMapping(map, "special_variable_name", &CT.variable);
    addNodeColorMapping(map, "parameter_expansion", &CT.variable);
    addNodeColorMapping(map, "braced_expansion", &CT.variable);
    addNodeColorMapping(map, "subscript", &CT.variable);

    // Functions and commands
    addNodeColorMapping(map, "function_definition", &CT.function);
    addNodeColorMapping(map, "command_name", &CT.function);
    addNodeColorMapping(map, "builtin_command", &CT.function);
    addNodeColorMapping(map, "identifier:command", &CT.function);

    // Operators and redirections
    addNodeColorMapping(map, "operator", &CT.operator);
    addNodeColorMapping(map, "redirect", &CT.operator);
    addNodeColorMapping(map, "pipe", &CT.operator);
    addNodeColorMapping(map, "file_redirect", &CT.operator);
    addNodeColorMapping(map, "heredoc_redirect", &CT.operator);
    addNodeColorMapping(map, "test_operator", &CT.operator);
    addNodeColorMapping(map, "regex", &CT.operator);

    // Numbers and file descriptors
    addNodeColorMapping(map, "integer", &CT.number);
    addNodeColorMapping(map, "file_descriptor", &CT.number);

    // Process substitution
    addNodeColorMapping(map, "process_substitution", &CT.operator);

    // Command substitution
    addNodeColorMapping(map, "command_substitution", &CT.operator);
    addNodeColorMapping(map, "subshell", &CT.operator);

    // Test constructs
    addNodeColorMapping(map, "test_command", &CT.keyword);
    addNodeColorMapping(map, "conditional_command", &CT.keyword);

    // Arrays
    addNodeColorMapping(map, "array", &CT.type);
    addNodeColorMapping(map, "array_expansion", &CT.type);

    // Exit codes and specials
    addNodeColorMapping(map, "exit_code", &CT.number);
    addNodeColorMapping(map, "bang", &CT.negation);

    // Fallback for generic identifiers
    addNodeColorMapping(map, "identifier", &CT.text);
}

void initElispNodeColorMappings(NodeColorMap **map) {
    // Grouping constructs
    addNodeColorMapping(map, "list",   &CT.text);
    addNodeColorMapping(map, "vector", &CT.text);
    addNodeColorMapping(map, "cons",   &CT.text);

    // Core elements
    addNodeColorMapping(map, "symbol", &CT.text);
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "escape", &CT.string);
    addNodeColorMapping(map, "number", &CT.number);
    addNodeColorMapping(map, "comment", &CT.comment);

    // Special forms / quoting
    addNodeColorMapping(map, "quote",           &CT.keyword);
    addNodeColorMapping(map, "quasiquote",      &CT.keyword);
    addNodeColorMapping(map, "unquote",         &CT.keyword);
    addNodeColorMapping(map, "unquote_splicing",&CT.keyword);
}

void initPythonNodeColorMappings(NodeColorMap **map) {
    // Module and structural nodes
    addNodeColorMapping(map, "module", &CT.text);
    addNodeColorMapping(map, "block", &CT.text);

    // Keywords
    addNodeColorMapping(map, "def",     &CT.keyword);
    addNodeColorMapping(map, "class",   &CT.keyword);
    addNodeColorMapping(map, "if",      &CT.keyword);
    addNodeColorMapping(map, "elif",    &CT.keyword);
    addNodeColorMapping(map, "else",    &CT.keyword);
    addNodeColorMapping(map, "for",     &CT.keyword);
    addNodeColorMapping(map, "while",   &CT.keyword);
    addNodeColorMapping(map, "return",  &CT.keyword);
    addNodeColorMapping(map, "import",  &CT.keyword);
    addNodeColorMapping(map, "from",    &CT.keyword);
    addNodeColorMapping(map, "as",      &CT.keyword);
    addNodeColorMapping(map, "pass",    &CT.keyword);
    addNodeColorMapping(map, "break",   &CT.keyword);
    addNodeColorMapping(map, "continue",&CT.keyword);
    addNodeColorMapping(map, "try",     &CT.keyword);
    addNodeColorMapping(map, "except",  &CT.keyword);
    addNodeColorMapping(map, "finally", &CT.keyword);
    addNodeColorMapping(map, "with",    &CT.keyword);
    addNodeColorMapping(map, "raise",   &CT.keyword);
    addNodeColorMapping(map, "lambda",  &CT.keyword);
    
    // Boolean and None literals
    addNodeColorMapping(map, "True",  &CT.null);
    addNodeColorMapping(map, "False", &CT.null);
    addNodeColorMapping(map, "None",  &CT.null);

    // Identifiers and definitions
    addNodeColorMapping(map, "identifier",         &CT.text);
    addNodeColorMapping(map, "function_definition",&CT.function);
    addNodeColorMapping(map, "class_definition",   &CT.null);

    // Literals: strings and numbers
    addNodeColorMapping(map, "string",         &CT.string);
    addNodeColorMapping(map, "string_literal", &CT.string);
    addNodeColorMapping(map, "integer",        &CT.number);
    addNodeColorMapping(map, "float",          &CT.number);

    // Comments
    addNodeColorMapping(map, "comment", &CT.comment);

    // Decorators
    addNodeColorMapping(map, "decorator", &CT.keyword);

    // Operators and punctuation
    addNodeColorMapping(map, "operator",   &CT.cursor);
    addNodeColorMapping(map, "assignment", &CT.cursor);
}

void initOcamlNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "let",      &CT.keyword);
    addNodeColorMapping(map, "rec",      &CT.keyword);
    addNodeColorMapping(map, "in",       &CT.keyword);
    addNodeColorMapping(map, "match",    &CT.keyword);
    addNodeColorMapping(map, "with",     &CT.keyword);
    addNodeColorMapping(map, "if",       &CT.keyword);
    addNodeColorMapping(map, "then",     &CT.keyword);
    addNodeColorMapping(map, "else",     &CT.keyword);
    addNodeColorMapping(map, "fun",      &CT.keyword);
    addNodeColorMapping(map, "function", &CT.keyword);
    addNodeColorMapping(map, "type",     &CT.keyword);
    addNodeColorMapping(map, "module",   &CT.keyword);
    addNodeColorMapping(map, "open",     &CT.keyword);
    addNodeColorMapping(map, "exception",&CT.keyword);
    addNodeColorMapping(map, "and",      &CT.keyword);
    addNodeColorMapping(map, "mutable",  &CT.keyword);

    // Identifiers and definitions
    addNodeColorMapping(map, "identifier",         &CT.text);
    addNodeColorMapping(map, "value_binding",      &CT.variable);
    addNodeColorMapping(map, "function_definition",&CT.function);

    // Literals
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "char",   &CT.string);
    addNodeColorMapping(map, "int",    &CT.number);
    addNodeColorMapping(map, "float",  &CT.number);

    // Patterns and constructors
    addNodeColorMapping(map, "constructor", &CT.type);
    addNodeColorMapping(map, "pattern",     &CT.text);

    // Comments
    addNodeColorMapping(map, "comment", &CT.comment);

    // Operators and punctuation
    addNodeColorMapping(map, "operator",    &CT.cursor);
    addNodeColorMapping(map, "punctuation", &CT.cursor);

    // Collections and other expressions
    addNodeColorMapping(map, "list",   &CT.text);
    addNodeColorMapping(map, "tuple",  &CT.text);
    addNodeColorMapping(map, "record", &CT.text);
}

void initCssNodeColorMappings(NodeColorMap **map) {
    // Root and stylesheet structure
    addNodeColorMapping(map, "stylesheet", &CT.text);
    
    // At-rules (e.g., @media, @import)
    addNodeColorMapping(map, "at_rule", &CT.keyword);

    // Rule sets and selectors
    addNodeColorMapping(map, "rule_set", &CT.text);
    addNodeColorMapping(map, "selector", &CT.keyword);
    addNodeColorMapping(map, "class_selector", &CT.keyword);
    addNodeColorMapping(map, "id_selector", &CT.keyword);
    addNodeColorMapping(map, "pseudo_class", &CT.keyword);
    addNodeColorMapping(map, "pseudo_element", &CT.keyword);
    addNodeColorMapping(map, "attribute_selector", &CT.keyword);

    // Declaration block
    addNodeColorMapping(map, "declaration", &CT.text);
    addNodeColorMapping(map, "property", &CT.keyword);
    addNodeColorMapping(map, "value", &CT.text);

    // Literals and functions
    addNodeColorMapping(map, "string", &CT.string);
    addNodeColorMapping(map, "number", &CT.number);
    addNodeColorMapping(map, "percentage", &CT.number);
    addNodeColorMapping(map, "dimension", &CT.number);
    addNodeColorMapping(map, "function", &CT.function);
    addNodeColorMapping(map, "url", &CT.string);

    // Comments
    addNodeColorMapping(map, "comment", &CT.comment);

    // Operators and punctuation (e.g., braces, colons, semicolons)
    addNodeColorMapping(map, "operator", &CT.cursor);
    addNodeColorMapping(map, "punctuation", &CT.cursor);
}

void initJavascriptNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "return",      &CT.keyword);
    addNodeColorMapping(map, "if",          &CT.keyword);
    addNodeColorMapping(map, "else",        &CT.keyword);
    addNodeColorMapping(map, "while",       &CT.keyword);
    addNodeColorMapping(map, "do",          &CT.keyword);
    addNodeColorMapping(map, "for",         &CT.keyword);
    addNodeColorMapping(map, "break",       &CT.keyword);
    addNodeColorMapping(map, "continue",    &CT.keyword);
    addNodeColorMapping(map, "switch",      &CT.keyword);
    addNodeColorMapping(map, "case",        &CT.keyword);
    addNodeColorMapping(map, "default",     &CT.keyword);
    addNodeColorMapping(map, "function",    &CT.keyword);
    addNodeColorMapping(map, "const",       &CT.keyword);
    addNodeColorMapping(map, "let",         &CT.keyword);
    addNodeColorMapping(map, "var",         &CT.keyword);
    addNodeColorMapping(map, "class",       &CT.keyword);
    addNodeColorMapping(map, "extends",     &CT.keyword);
    addNodeColorMapping(map, "import",      &CT.keyword);
    addNodeColorMapping(map, "export",      &CT.keyword);
    addNodeColorMapping(map, "try",         &CT.keyword);
    addNodeColorMapping(map, "catch",       &CT.keyword);
    addNodeColorMapping(map, "finally",     &CT.keyword);
    addNodeColorMapping(map, "throw",       &CT.keyword);
    addNodeColorMapping(map, "async",       &CT.keyword);
    addNodeColorMapping(map, "await",       &CT.keyword);
    
    // Constants and booleans
    addNodeColorMapping(map, "null",       &CT.null);
    addNodeColorMapping(map, "undefined",  &CT.null);
    addNodeColorMapping(map, "true",       &CT.null);
    addNodeColorMapping(map, "false",      &CT.null);
    addNodeColorMapping(map, "NaN",        &CT.null);
    addNodeColorMapping(map, "!",          &CT.negation);
    
    // Types and classes
    addNodeColorMapping(map, "class_declaration",     &CT.type);
    addNodeColorMapping(map, "type_identifier",       &CT.type);
    addNodeColorMapping(map, "type_annotation",       &CT.type);
    
    // Strings
    addNodeColorMapping(map, "string",               &CT.string);
    addNodeColorMapping(map, "template_string",      &CT.string);
    addNodeColorMapping(map, "string_fragment",      &CT.string);
    addNodeColorMapping(map, "\"",                   &CT.string);
    addNodeColorMapping(map, "'",                    &CT.string);
    addNodeColorMapping(map, "`",                    &CT.string);
    
    // Numbers
    addNodeColorMapping(map, "number",               &CT.number);
    
    // Functions
    addNodeColorMapping(map, "function_declaration",  &CT.function);
    addNodeColorMapping(map, "method_definition",     &CT.function);
    addNodeColorMapping(map, "arrow_function",        &CT.function);
    addNodeColorMapping(map, "call_expression",       &CT.function);
    
    // Objects and properties
    addNodeColorMapping(map, "property_identifier",   &CT.variable);
    addNodeColorMapping(map, "object",                &CT.variable);
    
    // Expressions and operators
    addNodeColorMapping(map, "assignment_expression", &CT.cursor);
    addNodeColorMapping(map, "binary_expression",     &CT.cursor);
    addNodeColorMapping(map, "unary_expression",      &CT.cursor);
    addNodeColorMapping(map, "update_expression",     &CT.cursor);
    addNodeColorMapping(map, "ternary_expression",    &CT.cursor);
    addNodeColorMapping(map, "spread_element",        &CT.cursor);
    
    // Identifier context mappings
    addNodeColorMapping(map, "identifier:function_declaration", &CT.function);
    addNodeColorMapping(map, "identifier:method_definition",    &CT.function);
    addNodeColorMapping(map, "identifier:variable_declarator",  &CT.variable);
    addNodeColorMapping(map, "identifier:assignment_expression",&CT.variable);
    addNodeColorMapping(map, "identifier:member_expression",    &CT.variable);
    addNodeColorMapping(map, "identifier",                      &CT.text);   //_=> identifier
    
    // Comments
    addNodeColorMapping(map, "comment",               &CT.comment);
    addNodeColorMapping(map, "line_comment",          &CT.comment);
    addNodeColorMapping(map, "block_comment",         &CT.comment);
    
    // JSX and React specific
    addNodeColorMapping(map, "jsx_element",           &CT.type);
    addNodeColorMapping(map, "jsx_attribute",         &CT.variable);
    addNodeColorMapping(map, "jsx_text",              &CT.text);
    
    //_=> will use CT.text for anything not specified
}

void initJuliaNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "function",   &CT.keyword);
    addNodeColorMapping(map, "end",        &CT.keyword);
    addNodeColorMapping(map, "if",         &CT.keyword);
    addNodeColorMapping(map, "else",       &CT.keyword);
    addNodeColorMapping(map, "elseif",     &CT.keyword);
    addNodeColorMapping(map, "while",      &CT.keyword);
    addNodeColorMapping(map, "for",        &CT.keyword);
    addNodeColorMapping(map, "in",         &CT.keyword);
    addNodeColorMapping(map, "return",     &CT.keyword);
    addNodeColorMapping(map, "break",      &CT.keyword);
    addNodeColorMapping(map, "continue",   &CT.keyword);
    addNodeColorMapping(map, "let",        &CT.keyword);
    addNodeColorMapping(map, "const",      &CT.keyword);
    addNodeColorMapping(map, "where",      &CT.keyword);
    addNodeColorMapping(map, "module",     &CT.keyword);
    addNodeColorMapping(map, "import",     &CT.keyword);
    addNodeColorMapping(map, "using",      &CT.keyword);
    addNodeColorMapping(map, "export",     &CT.keyword);
    addNodeColorMapping(map, "struct",     &CT.keyword);
    addNodeColorMapping(map, "mutable",    &CT.keyword);
    addNodeColorMapping(map, "abstract",   &CT.keyword);
    addNodeColorMapping(map, "type",       &CT.keyword);
    addNodeColorMapping(map, "macro",      &CT.keyword);
    addNodeColorMapping(map, "quote",      &CT.keyword);
    addNodeColorMapping(map, "try",        &CT.keyword);
    addNodeColorMapping(map, "catch",      &CT.keyword);
    addNodeColorMapping(map, "finally",    &CT.keyword);
    addNodeColorMapping(map, "do",         &CT.keyword);
    
    // Constants and booleans
    addNodeColorMapping(map, "nothing",    &CT.null);
    addNodeColorMapping(map, "true",       &CT.null);
    addNodeColorMapping(map, "false",      &CT.null);
    addNodeColorMapping(map, "!",          &CT.negation);
    addNodeColorMapping(map, "missing",    &CT.null);
    
    // Types
    addNodeColorMapping(map, "type_declaration",    &CT.type);
    addNodeColorMapping(map, "abstract_type",       &CT.type);
    addNodeColorMapping(map, "struct_definition",   &CT.type);
    addNodeColorMapping(map, "parametric_type",     &CT.type);
    addNodeColorMapping(map, "type_parameter",      &CT.type);
    addNodeColorMapping(map, "typed_parameter",     &CT.type);
    
    // Strings
    addNodeColorMapping(map, "string_literal",      &CT.string);
    addNodeColorMapping(map, "string_content",      &CT.string);
    addNodeColorMapping(map, "char_literal",        &CT.string);
    addNodeColorMapping(map, "triple_string",       &CT.string);
    addNodeColorMapping(map, "command_string",      &CT.string);
    addNodeColorMapping(map, "\"",                  &CT.string);
    addNodeColorMapping(map, "'",                   &CT.string);
    addNodeColorMapping(map, "`",                   &CT.string);
    
    // Numbers
    addNodeColorMapping(map, "integer_literal",     &CT.number);
    addNodeColorMapping(map, "float_literal",       &CT.number);
    addNodeColorMapping(map, "complex_literal",     &CT.number);
    addNodeColorMapping(map, "hex_literal",         &CT.number);
    addNodeColorMapping(map, "binary_literal",      &CT.number);
    addNodeColorMapping(map, "octal_literal",       &CT.number);
    
    // Functions
    addNodeColorMapping(map, "function_definition", &CT.function);
    addNodeColorMapping(map, "function_call",       &CT.function);
    addNodeColorMapping(map, "macro_definition",    &CT.function);
    addNodeColorMapping(map, "macro_call",          &CT.function);
    
    // Special Julia syntax
    addNodeColorMapping(map, "broadcast_call",      &CT.function);
    addNodeColorMapping(map, "pipe_operator",       &CT.operator);
    addNodeColorMapping(map, "range_expression",    &CT.operator);
    addNodeColorMapping(map, "comprehension",       &CT.operator);
    
    // Expressions and operators
    addNodeColorMapping(map, "binary_operation",    &CT.cursor);
    addNodeColorMapping(map, "unary_operation",     &CT.cursor);
    addNodeColorMapping(map, "assignment",          &CT.cursor);
    addNodeColorMapping(map, "conditional",         &CT.cursor);
    addNodeColorMapping(map, "call_expression",     &CT.cursor);
    addNodeColorMapping(map, "splat_expression",    &CT.cursor);
    
    // Identifier context mappings
    addNodeColorMapping(map, "identifier:function_definition", &CT.function);
    addNodeColorMapping(map, "identifier:function_call",       &CT.function);
    addNodeColorMapping(map, "identifier:assignment",          &CT.variable);
    addNodeColorMapping(map, "identifier:struct_definition",   &CT.type);
    addNodeColorMapping(map, "identifier:type_declaration",    &CT.type);
    addNodeColorMapping(map, "identifier:module_definition",   &CT.type);
    addNodeColorMapping(map, "identifier",                     &CT.text);   //_=> identifier
    
    // Comments
    addNodeColorMapping(map, "comment",             &CT.comment);
    addNodeColorMapping(map, "line_comment",        &CT.comment);
    addNodeColorMapping(map, "#",                   &CT.comment);
    
    // Symbols
    addNodeColorMapping(map, "symbol",              &CT.variable);
    addNodeColorMapping(map, ":",                   &CT.variable);
    
    // Array and matrix syntax
    addNodeColorMapping(map, "array_expression",    &CT.variable);
    addNodeColorMapping(map, "matrix_expression",   &CT.variable);
    addNodeColorMapping(map, "vect_expression",     &CT.variable);
    
    //_=> will use CT.text for anything not specified
}

void initCppNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "return",      &CT.keyword);
    addNodeColorMapping(map, "if",          &CT.keyword);
    addNodeColorMapping(map, "else",        &CT.keyword);
    addNodeColorMapping(map, "while",       &CT.keyword);
    addNodeColorMapping(map, "do",          &CT.keyword);
    addNodeColorMapping(map, "for",         &CT.keyword);
    addNodeColorMapping(map, "break",       &CT.keyword);
    addNodeColorMapping(map, "continue",    &CT.keyword);
    addNodeColorMapping(map, "switch",      &CT.keyword);
    addNodeColorMapping(map, "case",        &CT.keyword);
    addNodeColorMapping(map, "default",     &CT.keyword);
    addNodeColorMapping(map, "goto",        &CT.keyword);
    addNodeColorMapping(map, "typedef",     &CT.keyword);
    addNodeColorMapping(map, "extern",      &CT.keyword);
    addNodeColorMapping(map, "static",      &CT.keyword);
    addNodeColorMapping(map, "const",       &CT.keyword);
    addNodeColorMapping(map, "constexpr",   &CT.keyword);
    addNodeColorMapping(map, "volatile",    &CT.keyword);
    addNodeColorMapping(map, "struct",      &CT.keyword);
    addNodeColorMapping(map, "class",       &CT.keyword);
    addNodeColorMapping(map, "namespace",   &CT.keyword);
    addNodeColorMapping(map, "template",    &CT.keyword);
    addNodeColorMapping(map, "typename",    &CT.keyword);
    addNodeColorMapping(map, "using",       &CT.keyword);
    addNodeColorMapping(map, "new",         &CT.keyword);
    addNodeColorMapping(map, "delete",      &CT.keyword);
    addNodeColorMapping(map, "try",         &CT.keyword);
    addNodeColorMapping(map, "catch",       &CT.keyword);
    addNodeColorMapping(map, "throw",       &CT.keyword);
    addNodeColorMapping(map, "public",      &CT.keyword);
    addNodeColorMapping(map, "private",     &CT.keyword);
    addNodeColorMapping(map, "protected",   &CT.keyword);
    addNodeColorMapping(map, "virtual",     &CT.keyword);
    addNodeColorMapping(map, "override",    &CT.keyword);
    addNodeColorMapping(map, "final",       &CT.keyword);
    addNodeColorMapping(map, "explicit",    &CT.keyword);
    addNodeColorMapping(map, "inline",      &CT.keyword);
    addNodeColorMapping(map, "friend",      &CT.keyword);
    addNodeColorMapping(map, "enum",        &CT.keyword);
    addNodeColorMapping(map, "union",       &CT.keyword);
    
    // Constants and booleans
    addNodeColorMapping(map, "NULL",        &CT.null);
    addNodeColorMapping(map, "nullptr",     &CT.null);
    addNodeColorMapping(map, "true",        &CT.null);
    addNodeColorMapping(map, "false",       &CT.null);
    addNodeColorMapping(map, "!",           &CT.negation);
    
    // Types
    addNodeColorMapping(map, "type_identifier",         &CT.type);
    addNodeColorMapping(map, "class_specifier",         &CT.type);
    addNodeColorMapping(map, "struct_specifier",        &CT.type);
    addNodeColorMapping(map, "enum_specifier",          &CT.type);
    addNodeColorMapping(map, "namespace_identifier",    &CT.type);
    addNodeColorMapping(map, "sized_type_specifier",    &CT.type);
    addNodeColorMapping(map, "primitive_type",          &CT.type);
    addNodeColorMapping(map, "template_type",           &CT.type);
    addNodeColorMapping(map, "auto",                    &CT.type);
    
    // Strings
    addNodeColorMapping(map, "string_literal",          &CT.string);
    addNodeColorMapping(map, "raw_string_literal",      &CT.string);
    addNodeColorMapping(map, "char_literal",            &CT.string);
    addNodeColorMapping(map, "string_content",          &CT.string);
    addNodeColorMapping(map, "system_lib_string",       &CT.string);
    addNodeColorMapping(map, "\"",                      &CT.string);
    addNodeColorMapping(map, "'",                       &CT.string);
    
    // Numbers
    addNodeColorMapping(map, "number_literal",          &CT.number);
    addNodeColorMapping(map, "float_literal",           &CT.number);
    addNodeColorMapping(map, "int_literal",             &CT.number);
    
    // Functions
    addNodeColorMapping(map, "function_definition",     &CT.function);
    addNodeColorMapping(map, "function_declaration",    &CT.function);
    addNodeColorMapping(map, "function_template",       &CT.function);
    addNodeColorMapping(map, "constructor_definition",  &CT.function);
    addNodeColorMapping(map, "destructor_definition",   &CT.function);
    addNodeColorMapping(map, "operator_overload",       &CT.function);
    addNodeColorMapping(map, "method_definition",       &CT.function);
    
    // Preprocessor
    addNodeColorMapping(map, "preproc_directive",       &CT.preprocessor);
    addNodeColorMapping(map, "preproc_include",         &CT.preprocessor);
    addNodeColorMapping(map, "preproc_def",             &CT.preprocessor);
    addNodeColorMapping(map, "preproc_function_def",    &CT.preprocessor);
    addNodeColorMapping(map, "preproc_if",              &CT.preprocessor);
    addNodeColorMapping(map, "preproc_else",            &CT.preprocessor);
    addNodeColorMapping(map, "preproc_elif",            &CT.preprocessor);
    addNodeColorMapping(map, "preproc_endif",           &CT.preprocessor);
    addNodeColorMapping(map, "preproc_ifdef",           &CT.preprocessor);
    addNodeColorMapping(map, "preproc_ifndef",          &CT.preprocessor);
    addNodeColorMapping(map, "preproc_arg",             &CT.preprocessor);
    addNodeColorMapping(map, "#define",                 &CT.preprocessor);
    addNodeColorMapping(map, "#include",                &CT.preprocessor);
    addNodeColorMapping(map, "#if",                     &CT.preprocessor);
    addNodeColorMapping(map, "#ifdef",                  &CT.preprocessor);
    addNodeColorMapping(map, "#ifndef",                 &CT.preprocessor);
    addNodeColorMapping(map, "#else",                   &CT.preprocessor);
    addNodeColorMapping(map, "#elif",                   &CT.preprocessor);
    addNodeColorMapping(map, "#endif",                  &CT.preprocessor);
    addNodeColorMapping(map, "#pragma",                 &CT.preprocessor);
    
    // Expressions and operators
    addNodeColorMapping(map, "assignment_expression",   &CT.cursor);
    addNodeColorMapping(map, "binary_expression",       &CT.cursor);
    addNodeColorMapping(map, "unary_expression",        &CT.cursor);
    addNodeColorMapping(map, "update_expression",       &CT.cursor);
    addNodeColorMapping(map, "ternary_expression",      &CT.cursor);
    addNodeColorMapping(map, "expression_statement",    &CT.cursor);
    addNodeColorMapping(map, "lambda_expression",       &CT.cursor);
    
    // C++ specific operators
    addNodeColorMapping(map, "scope_resolution_expression", &CT.operator);
    addNodeColorMapping(map, "::",                         &CT.operator);
    addNodeColorMapping(map, "->",                         &CT.operator);
    addNodeColorMapping(map, "->*",                        &CT.operator);
    addNodeColorMapping(map, ".*",                         &CT.operator);
    
    // Identifier context mappings
    addNodeColorMapping(map, "identifier:function_declarator",  &CT.function);
    addNodeColorMapping(map, "identifier:function_definition",  &CT.function);
    addNodeColorMapping(map, "identifier:method_definition",    &CT.function);
    addNodeColorMapping(map, "identifier:declaration",          &CT.variable);
    addNodeColorMapping(map, "identifier:assignment_expression",&CT.variable);
    addNodeColorMapping(map, "identifier:init_declarator",      &CT.variable);
    addNodeColorMapping(map, "identifier:parameter_declaration",&CT.variable);
    addNodeColorMapping(map, "identifier",                      &CT.text);   //_=> identifier
    
    // Comments
    addNodeColorMapping(map, "comment",                 &CT.comment);
    addNodeColorMapping(map, "line_comment",            &CT.comment);
    addNodeColorMapping(map, "block_comment",           &CT.comment);
    
    // C++ specific 
    addNodeColorMapping(map, "template_argument_list",  &CT.type);
    addNodeColorMapping(map, "template_parameter_list", &CT.type);
    addNodeColorMapping(map, "reference_declarator",    &CT.variable);
    addNodeColorMapping(map, "pointer_declarator",      &CT.variable);
    addNodeColorMapping(map, "namespace_definition",    &CT.type);
    
    //_=> will use CT.text for anything not specified
}

void initGoNodeColorMappings(NodeColorMap **map) {
    // Keywords
    addNodeColorMapping(map, "package",     &CT.keyword);
    addNodeColorMapping(map, "import",      &CT.keyword);
    addNodeColorMapping(map, "func",        &CT.keyword);
    addNodeColorMapping(map, "return",      &CT.keyword);
    addNodeColorMapping(map, "if",          &CT.keyword);
    addNodeColorMapping(map, "else",        &CT.keyword);
    addNodeColorMapping(map, "for",         &CT.keyword);
    addNodeColorMapping(map, "range",       &CT.keyword);
    addNodeColorMapping(map, "break",       &CT.keyword);
    addNodeColorMapping(map, "continue",    &CT.keyword);
    addNodeColorMapping(map, "switch",      &CT.keyword);
    addNodeColorMapping(map, "case",        &CT.keyword);
    addNodeColorMapping(map, "default",     &CT.keyword);
    addNodeColorMapping(map, "type",        &CT.keyword);
    addNodeColorMapping(map, "struct",      &CT.keyword);
    addNodeColorMapping(map, "interface",   &CT.keyword);
    addNodeColorMapping(map, "map",         &CT.keyword);
    addNodeColorMapping(map, "chan",        &CT.keyword);
    addNodeColorMapping(map, "go",          &CT.keyword);
    addNodeColorMapping(map, "defer",       &CT.keyword);
    addNodeColorMapping(map, "select",      &CT.keyword);
    addNodeColorMapping(map, "const",       &CT.keyword);
    addNodeColorMapping(map, "var",         &CT.keyword);
    addNodeColorMapping(map, "fallthrough", &CT.keyword);
    addNodeColorMapping(map, "goto",        &CT.keyword);
    
    // Constants and booleans
    addNodeColorMapping(map, "nil",         &CT.null);
    addNodeColorMapping(map, "true",        &CT.null);
    addNodeColorMapping(map, "false",       &CT.null);
    addNodeColorMapping(map, "iota",        &CT.null);
    addNodeColorMapping(map, "!",           &CT.negation);
    
    // Types
    addNodeColorMapping(map, "type_identifier",       &CT.type);
    addNodeColorMapping(map, "type_spec",             &CT.type);
    addNodeColorMapping(map, "type_declaration",      &CT.type);
    addNodeColorMapping(map, "struct_type",           &CT.type);
    addNodeColorMapping(map, "interface_type",        &CT.type);
    addNodeColorMapping(map, "parameter_declaration", &CT.type);
    addNodeColorMapping(map, "pointer_type",          &CT.type);
    addNodeColorMapping(map, "array_type",            &CT.type);
    addNodeColorMapping(map, "slice_type",            &CT.type);
    addNodeColorMapping(map, "map_type",              &CT.type);
    addNodeColorMapping(map, "channel_type",          &CT.type);
    addNodeColorMapping(map, "function_type",         &CT.type);
    addNodeColorMapping(map, "qualified_type",        &CT.type);
    addNodeColorMapping(map, "int",                   &CT.type);
    addNodeColorMapping(map, "uint",                  &CT.type);
    addNodeColorMapping(map, "byte",                  &CT.type);
    addNodeColorMapping(map, "rune",                  &CT.type);
    addNodeColorMapping(map, "string",                &CT.type);
    addNodeColorMapping(map, "float32",               &CT.type);
    addNodeColorMapping(map, "float64",               &CT.type);
    addNodeColorMapping(map, "bool",                  &CT.type);
    addNodeColorMapping(map, "error",                 &CT.type);
    
    // Strings
    addNodeColorMapping(map, "interpreted_string_literal", &CT.string);
    addNodeColorMapping(map, "raw_string_literal",        &CT.string);
    addNodeColorMapping(map, "rune_literal",              &CT.string);
    addNodeColorMapping(map, "escape_sequence",           &CT.string);
    addNodeColorMapping(map, "\"",                        &CT.string);
    addNodeColorMapping(map, "'",                         &CT.string);
    addNodeColorMapping(map, "`",                         &CT.string);
    
    // Numbers
    addNodeColorMapping(map, "int_literal",               &CT.number);
    addNodeColorMapping(map, "float_literal",             &CT.number);
    addNodeColorMapping(map, "imaginary_literal",         &CT.number);
    
    // Functions
    addNodeColorMapping(map, "function_declaration",      &CT.function);
    addNodeColorMapping(map, "method_declaration",        &CT.function);
    addNodeColorMapping(map, "method_spec",               &CT.function);
    addNodeColorMapping(map, "function_literal",          &CT.function);
    addNodeColorMapping(map, "call_expression",           &CT.function);
    
    // Expressions and operators
    addNodeColorMapping(map, "binary_expression",         &CT.cursor);
    addNodeColorMapping(map, "unary_expression",          &CT.cursor);
    addNodeColorMapping(map, "assignment_statement",      &CT.cursor);
    addNodeColorMapping(map, "expression_switch_statement", &CT.cursor);
    addNodeColorMapping(map, "type_switch_statement",     &CT.cursor);
    addNodeColorMapping(map, "short_var_declaration",     &CT.cursor);
    addNodeColorMapping(map, "expression_list",           &CT.cursor);
    addNodeColorMapping(map, "selector_expression",       &CT.cursor);
    addNodeColorMapping(map, "index_expression",          &CT.cursor);
    addNodeColorMapping(map, "slice_expression",          &CT.cursor);
    addNodeColorMapping(map, "type_assertion_expression", &CT.cursor);
    
    // Go specific operators
    addNodeColorMapping(map, "<-",                        &CT.operator);
    addNodeColorMapping(map, ":=",                        &CT.operator);
    addNodeColorMapping(map, "...",                       &CT.operator);
    
    // Packages and imports
    addNodeColorMapping(map, "package_clause",            &CT.type);
    addNodeColorMapping(map, "import_declaration",        &CT.preprocessor);
    addNodeColorMapping(map, "import_spec",               &CT.preprocessor);
    addNodeColorMapping(map, "package_identifier",        &CT.type);
    
    // Identifier context mappings
    addNodeColorMapping(map, "identifier:function_declaration", &CT.function);
    addNodeColorMapping(map, "identifier:method_declaration",   &CT.function);
    addNodeColorMapping(map, "identifier:parameter_declaration",&CT.variable);
    addNodeColorMapping(map, "identifier:short_var_declaration",&CT.variable);
    addNodeColorMapping(map, "identifier:var_declaration",      &CT.variable);
    addNodeColorMapping(map, "identifier:const_declaration",    &CT.variable);
    addNodeColorMapping(map, "identifier:field_declaration",    &CT.variable);
    addNodeColorMapping(map, "identifier:type_declaration",     &CT.type);
    addNodeColorMapping(map, "identifier:package_clause",       &CT.type);
    addNodeColorMapping(map, "identifier",                      &CT.text);   //_=> identifier
    
    // Comments
    addNodeColorMapping(map, "comment",                   &CT.comment);
    addNodeColorMapping(map, "line_comment",              &CT.comment);
    addNodeColorMapping(map, "block_comment",             &CT.comment);
    
    // Go specific constructs
    addNodeColorMapping(map, "composite_literal",         &CT.variable);
    addNodeColorMapping(map, "keyed_element",             &CT.variable);
    addNodeColorMapping(map, "field_declaration",         &CT.variable);
    addNodeColorMapping(map, "field_identifier",          &CT.variable);
    addNodeColorMapping(map, "labeled_statement",         &CT.variable);
    addNodeColorMapping(map, "receive_statement",         &CT.cursor);
    addNodeColorMapping(map, "send_statement",            &CT.cursor);
    addNodeColorMapping(map, "go_statement",              &CT.keyword);
    addNodeColorMapping(map, "defer_statement",           &CT.keyword);
    
    //_=> will use CT.text for anything not specified
}

void initJsonNodeColorMappings(NodeColorMap **map) {
    // String keys and values
    addNodeColorMapping(map, "string",            &CT.string);
    addNodeColorMapping(map, "string_content",    &CT.string);
    addNodeColorMapping(map, "\"",                &CT.string);
    
    // Property names are typically colored differently than string values
    addNodeColorMapping(map, "property_identifier", &CT.variable);
    addNodeColorMapping(map, "pair",                &CT.variable);
    addNodeColorMapping(map, "object_key",          &CT.variable);
    
    // Numbers
    addNodeColorMapping(map, "number",              &CT.number);
    addNodeColorMapping(map, "integer",             &CT.number);
    addNodeColorMapping(map, "float",               &CT.number);
    
    // Constants
    addNodeColorMapping(map, "true",                &CT.null);
    addNodeColorMapping(map, "false",               &CT.null);
    addNodeColorMapping(map, "null",                &CT.null);
    
    addNodeColorMapping(map, "object",              &CT.text);
    addNodeColorMapping(map, "array",               &CT.text);
    
    // Punctuation (use default text color for these)
    addNodeColorMapping(map, "{",                   &CT.text);
    addNodeColorMapping(map, "}",                   &CT.text);
    addNodeColorMapping(map, "[",                   &CT.text);
    addNodeColorMapping(map, "]",                   &CT.text);
    addNodeColorMapping(map, ":",                   &CT.text);
    addNodeColorMapping(map, ",",                   &CT.text);
    
    addNodeColorMapping(map, "comment",             &CT.comment);
    addNodeColorMapping(map, "line_comment",        &CT.comment);
    addNodeColorMapping(map, "block_comment",       &CT.comment);
    
    addNodeColorMapping(map, "//",                  &CT.comment);
    addNodeColorMapping(map, "/*",                  &CT.comment);
    addNodeColorMapping(map, "*/",                  &CT.comment);
}

void initRegexNodeColorMappings(NodeColorMap **map) {
    addNodeColorMapping(map, "anchor",                  &CT.keyword);       // ^ $
    addNodeColorMapping(map, "boundary",                &CT.keyword);       // \b \B
    addNodeColorMapping(map, "quantifier",              &CT.operator);      // * + ? {n,m}
    addNodeColorMapping(map, "alternation",             &CT.operator);      // |
    addNodeColorMapping(map, "character_class",         &CT.type);          // [a-z]
    addNodeColorMapping(map, "predefined_character_set",&CT.function);      // \d \w \s
    addNodeColorMapping(map, "escape_sequence",         &CT.function);      // \., \*, etc
    addNodeColorMapping(map, "group",                   &CT.preprocessor);  // () (?:) (?=)
    addNodeColorMapping(map, "capture_group",           &CT.preprocessor);
    addNodeColorMapping(map, "non_capturing_group",     &CT.preprocessor);
    
    // Literals and basic elements
    addNodeColorMapping(map, "literal",                 &CT.text);          // Normal characters
    addNodeColorMapping(map, "pattern",                 &CT.text);          // Root node
    addNodeColorMapping(map, "any_character",           &CT.operator);      // .
    
    // Special sequences
    addNodeColorMapping(map, "backreference",           &CT.variable);      // \1 \2 etc
    addNodeColorMapping(map, "named_backreference",     &CT.variable);      // \k<name>
    
    // Assertions and lookarounds
    addNodeColorMapping(map, "lookahead_assertion",     &CT.keyword);
    addNodeColorMapping(map, "lookbehind_assertion",    &CT.keyword);
    addNodeColorMapping(map, "positive_assertion",      &CT.keyword);
    addNodeColorMapping(map, "negative_assertion",      &CT.negation);
    
    // Character class components
    addNodeColorMapping(map, "class_range",             &CT.operator);      // a-z
    addNodeColorMapping(map, "character_class_escape",  &CT.function);      // \d inside []
    
    // Errors
    addNodeColorMapping(map, "error",                   &CT.error);
    
    // Punctuation and symbols
    addNodeColorMapping(map, "(",                       &CT.preprocessor);
    addNodeColorMapping(map, ")",                       &CT.preprocessor);
    addNodeColorMapping(map, "[",                       &CT.preprocessor);
    addNodeColorMapping(map, "]",                       &CT.preprocessor);
    addNodeColorMapping(map, "{",                       &CT.preprocessor);
    addNodeColorMapping(map, "}",                       &CT.preprocessor);
    addNodeColorMapping(map, "|",                       &CT.operator);
    addNodeColorMapping(map, "^",                       &CT.keyword);
    addNodeColorMapping(map, "$",                       &CT.keyword);
    addNodeColorMapping(map, ".",                       &CT.operator);
    addNodeColorMapping(map, "*",                       &CT.operator);
    addNodeColorMapping(map, "+",                       &CT.operator);
    addNodeColorMapping(map, "?",                       &CT.operator);
    
    // Special escapes
    addNodeColorMapping(map, "control_escape",          &CT.function);      // \cX
    addNodeColorMapping(map, "hex_escape",              &CT.function);      // \xhh
    addNodeColorMapping(map, "unicode_escape",          &CT.function);      // \uFFFF
    addNodeColorMapping(map, "octal_escape",            &CT.function);      // \377
    
    // Character class special characters
    addNodeColorMapping(map, "class_intersection",      &CT.operator);      // &&
    addNodeColorMapping(map, "class_complement",        &CT.negation);      // ^ in []
}

void initPhpNodeColorMappings(NodeColorMap **map) {
    // Basic PHP syntax
    addNodeColorMapping(map, "php_tag",                &CT.preprocessor);  // <?php ?>
    addNodeColorMapping(map, "echo_statement",         &CT.keyword);
    addNodeColorMapping(map, "print_statement",        &CT.keyword);
    addNodeColorMapping(map, "exit_statement",         &CT.keyword);
    addNodeColorMapping(map, "expression_statement",   &CT.text);

    // Control structures
    addNodeColorMapping(map, "if_statement",           &CT.keyword);
    addNodeColorMapping(map, "else_statement",         &CT.keyword);
    addNodeColorMapping(map, "elseif_statement",       &CT.keyword);
    addNodeColorMapping(map, "switch_statement",       &CT.keyword);
    addNodeColorMapping(map, "case_statement",         &CT.keyword);
    addNodeColorMapping(map, "default_statement",      &CT.keyword);
    addNodeColorMapping(map, "while_statement",        &CT.keyword);
    addNodeColorMapping(map, "do_statement",           &CT.keyword);
    addNodeColorMapping(map, "for_statement",          &CT.keyword);
    addNodeColorMapping(map, "foreach_statement",      &CT.keyword);
    addNodeColorMapping(map, "break_statement",        &CT.keyword);
    addNodeColorMapping(map, "continue_statement",     &CT.keyword);
    addNodeColorMapping(map, "return_statement",       &CT.keyword);
    addNodeColorMapping(map, "goto_statement",         &CT.keyword);
    addNodeColorMapping(map, "try_statement",          &CT.keyword);
    addNodeColorMapping(map, "catch_statement",        &CT.keyword);
    addNodeColorMapping(map, "finally_statement",      &CT.keyword);
    addNodeColorMapping(map, "throw_statement",        &CT.keyword);

    // Functions and methods
    addNodeColorMapping(map, "function_definition",    &CT.function);
    addNodeColorMapping(map, "method_definition",      &CT.function);
    addNodeColorMapping(map, "function_call",          &CT.function);
    addNodeColorMapping(map, "method_call",            &CT.function);
    addNodeColorMapping(map, "arrow_function",         &CT.function);
    addNodeColorMapping(map, "anonymous_function",     &CT.function);
    addNodeColorMapping(map, "closure",                &CT.function);
    addNodeColorMapping(map, "parameters",             &CT.variable);
    addNodeColorMapping(map, "argument",               &CT.variable);
    addNodeColorMapping(map, "variadic_parameter",     &CT.variable);

    // Classes and objects
    addNodeColorMapping(map, "class_definition",       &CT.type);
    addNodeColorMapping(map, "interface_definition",   &CT.type);
    addNodeColorMapping(map, "trait_definition",       &CT.type);
    addNodeColorMapping(map, "enum_definition",        &CT.type);
    addNodeColorMapping(map, "property_declaration",   &CT.variable);
    addNodeColorMapping(map, "class_constant",         &CT.variable);
    addNodeColorMapping(map, "object_creation",        &CT.type);
    addNodeColorMapping(map, "class_name",             &CT.type);
    addNodeColorMapping(map, "base_clause",            &CT.type);
    addNodeColorMapping(map, "implements_clause",      &CT.type);
    addNodeColorMapping(map, "trait_use_clause",       &CT.type);
    addNodeColorMapping(map, "namespace_definition",   &CT.type);
    addNodeColorMapping(map, "namespace_use_clause",   &CT.type);

    // Variables and constants
    addNodeColorMapping(map, "variable",               &CT.variable);
    addNodeColorMapping(map, "variable_name",          &CT.variable);
    addNodeColorMapping(map, "static_variable",        &CT.variable);
    addNodeColorMapping(map, "global_variable",        &CT.variable);
    addNodeColorMapping(map, "superglobal",            &CT.variable);
    addNodeColorMapping(map, "constant",               &CT.variable);
    addNodeColorMapping(map, "magic_constant",         &CT.variable);
    addNodeColorMapping(map, "class_constant_access",  &CT.variable);
    addNodeColorMapping(map, "object_property_access", &CT.variable);

    // Types and type hints
    addNodeColorMapping(map, "type_identifier",        &CT.type);
    addNodeColorMapping(map, "primitive_type",         &CT.type);
    addNodeColorMapping(map, "union_type",             &CT.type);
    addNodeColorMapping(map, "intersection_type",      &CT.type);
    addNodeColorMapping(map, "nullable_type",          &CT.type);
    addNodeColorMapping(map, "type_parameter",         &CT.type);
    addNodeColorMapping(map, "type_argument",          &CT.type);

    // Arrays
    addNodeColorMapping(map, "array_creation",         &CT.type);
    addNodeColorMapping(map, "array_element",          &CT.variable);
    addNodeColorMapping(map, "array_key",              &CT.variable);
    addNodeColorMapping(map, "array_value",            &CT.variable);
    addNodeColorMapping(map, "short_array",            &CT.type);
    addNodeColorMapping(map, "list_destructuring",     &CT.type);

    // Strings
    addNodeColorMapping(map, "string",                 &CT.string);
    addNodeColorMapping(map, "string_content",         &CT.string);
    addNodeColorMapping(map, "heredoc",                &CT.string);
    addNodeColorMapping(map, "nowdoc",                 &CT.string);
    addNodeColorMapping(map, "string_interpolation",   &CT.string);
    addNodeColorMapping(map, "encapsed_string",        &CT.string);
    addNodeColorMapping(map, "shell_command",          &CT.string);

    // Numbers
    addNodeColorMapping(map, "integer",                &CT.number);
    addNodeColorMapping(map, "float",                  &CT.number);
    addNodeColorMapping(map, "binary",                 &CT.number);
    addNodeColorMapping(map, "octal",                  &CT.number);
    addNodeColorMapping(map, "hexadecimal",            &CT.number);

    // Operators
    addNodeColorMapping(map, "binary_expression",      &CT.operator);
    addNodeColorMapping(map, "unary_expression",       &CT.operator);
    addNodeColorMapping(map, "assignment_expression",  &CT.operator);
    addNodeColorMapping(map, "comparison_expression",  &CT.operator);
    addNodeColorMapping(map, "logical_expression",     &CT.operator);
    addNodeColorMapping(map, "ternary_expression",     &CT.operator);
    addNodeColorMapping(map, "match_expression",       &CT.operator);
    addNodeColorMapping(map, "null_coalescing",        &CT.operator);
    addNodeColorMapping(map, "spaceship_operator",     &CT.operator);
    addNodeColorMapping(map, "instanceof_expression",  &CT.operator);

    // Comments
    addNodeColorMapping(map, "comment",                &CT.comment);
    addNodeColorMapping(map, "line_comment",           &CT.comment);
    addNodeColorMapping(map, "block_comment",          &CT.comment);
    addNodeColorMapping(map, "doc_comment",            &CT.comment);

    // Attributes
    addNodeColorMapping(map, "attribute",              &CT.preprocessor);
    addNodeColorMapping(map, "attribute_group",        &CT.preprocessor);
    addNodeColorMapping(map, "attribute_argument",     &CT.variable);

    // Special syntax
    addNodeColorMapping(map, "yield_expression",       &CT.keyword);
    addNodeColorMapping(map, "yield_from_expression",  &CT.keyword);
    addNodeColorMapping(map, "include_expression",     &CT.keyword);
    addNodeColorMapping(map, "include_once_expression",&CT.keyword);
    addNodeColorMapping(map, "require_expression",     &CT.keyword);
    addNodeColorMapping(map, "require_once_expression",&CT.keyword);
    addNodeColorMapping(map, "declare_statement",      &CT.keyword);
    addNodeColorMapping(map, "empty_expression",       &CT.keyword);
    addNodeColorMapping(map, "eval_expression",        &CT.keyword);
    addNodeColorMapping(map, "isset_expression",       &CT.keyword);
    addNodeColorMapping(map, "unset_expression",       &CT.keyword);

    // Error handling
    addNodeColorMapping(map, "error",                  &CT.error);
    addNodeColorMapping(map, "parse_error",            &CT.error);
    addNodeColorMapping(map, "unterminated_string",    &CT.error);
    addNodeColorMapping(map, "unterminated_comment",   &CT.error);

    // Miscellaneous
    addNodeColorMapping(map, "program",                &CT.text);
    addNodeColorMapping(map, "text_interpolation",     &CT.text);
    addNodeColorMapping(map, "heredoc_start",          &CT.string);
    addNodeColorMapping(map, "heredoc_end",            &CT.string);
    addNodeColorMapping(map, "nowdoc_start",           &CT.string);
    addNodeColorMapping(map, "nowdoc_end",             &CT.string);
}


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
    } else if (strcmp(language, "query") == 0) {
        lang = tree_sitter_query();
    } else if (strcmp(language, "scheme") == 0) {
        lang = tree_sitter_scheme();
    } else if (strcmp(language, "html") == 0) {
        lang = tree_sitter_html();
    } else if (strcmp(language, "glsl") == 0) {
        lang = tree_sitter_glsl();
    } else if (strcmp(language, "zig") == 0) {
        lang = tree_sitter_zig();
    } else if (strcmp(language, "odin") == 0) {
        lang = tree_sitter_odin();
    } else if (strcmp(language, "make") == 0) {
        lang = tree_sitter_make();
    } else if (strcmp(language, "commonlisp") == 0) {
        lang = tree_sitter_commonlisp();
    } else if (strcmp(language, "scss") == 0) {
        lang = tree_sitter_scss();
    } else if (strcmp(language, "haskell") == 0) {
        lang = tree_sitter_haskell();
    } else if (strcmp(language, "lua") == 0) {
        lang = tree_sitter_lua();
    } else if (strcmp(language, "rust") == 0) {
        lang = tree_sitter_rust();
    } else if (strcmp(language, "bash") == 0) {
        lang = tree_sitter_bash();
    } else if (strcmp(language, "elisp") == 0) {
        lang = tree_sitter_elisp();
    } else if (strcmp(language, "python") == 0) {
        lang = tree_sitter_python();
    } else if (strcmp(language, "ocaml") == 0) {
        lang = tree_sitter_ocaml();
    } else if (strcmp(language, "css") == 0) {
        lang = tree_sitter_css();
    } else if (strcmp(language, "javascript") == 0) {
        lang = tree_sitter_javascript();
    } else if (strcmp(language, "julia") == 0) {
        lang = tree_sitter_julia();
    } else if (strcmp(language, "cpp") == 0) {
        lang = tree_sitter_cpp();
    } else if (strcmp(language, "go") == 0) {
        lang = tree_sitter_go();
    } else if (strcmp(language, "json") == 0) {
        lang = tree_sitter_json();
    } else if (strcmp(language, "regex") == 0) {
        lang = tree_sitter_regex();
    }

    if (!lang) {
        fprintf(stderr, "Unsupported language: %s\n", language);
        ts_parser_delete(parser);
        return NULL;
    }

    ts_parser_set_language(parser, lang);

    // Initialize node color mappings for the language
    LanguageParser newParser = {
        .language = language,
        .parser = parser,
        .nodeColorMap = NULL
    };

    if (strcmp(language, "c") == 0) {
        initCNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "scheme") == 0) {
        initSchemeNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "query") == 0) {
        initQueryNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "html") == 0) {
        initHtmlNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "glsl") == 0) {
        initGlslNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "zig") == 0) {
        initZigNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "odin") == 0) {
        initOdinNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "make") == 0) {
        initMakeNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "commonlisp") == 0) {
        initCommonlispNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "scss") == 0) {
        initScssNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "haskell") == 0) {
        initHaskellNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "lua") == 0) {
        initLuaNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "rust") == 0) {
        initRustNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "bash") == 0) {
        initBashNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "elisp") == 0) {
        initElispNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "python") == 0) {
        initPythonNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "ocaml") == 0) {
        initOcamlNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "css") == 0) {
        initCssNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "javascript") == 0) {
        initJavascriptNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "julia") == 0) {
        initJuliaNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "cpp") == 0) {
        initCppNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "go") == 0) {
        initGoNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "json") == 0) {
        initJsonNodeColorMappings(&newParser.nodeColorMap);
    } else if (strcmp(language, "regex") == 0) {
        initRegexNodeColorMappings(&newParser.nodeColorMap);
    }

    // Add the new parser to the array
    addParserForLanguage(language, parser);
    lps.items[lps.count - 1].nodeColorMap = newParser.nodeColorMap;

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

Color *getNodeColor(TSNode node, const char *major_mode) {
    // NOTE At this point we assume that
    // the major mode is already supported

    // Look up the node color based on the node type
    const char *nodeType = ts_node_type(node);
    NodeColorMap *entry;
    HASH_FIND_STR(lps.items[lps.count - 1].nodeColorMap, nodeType, entry);

    // Return the color if found, otherwise return the default text color
    return entry ? entry->color : &CT.warning;
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

void displaySyntax(Buffer *buffer) {
    TSNode root_node = ts_tree_root_node(buffer->tree);
    processNode(root_node, buffer->content, &buffer->syntaxArray, buffer->major_mode);
}

void parseSyntax(Buffer *buffer) {
    TSParser *parser = inferParserForLanguage(buffer->major_mode);
    if (!parser) {
        fprintf(stderr, "Failed to get parser for language: %s\n", buffer->major_mode);
        return;
    }

    buffer->tree = ts_parser_parse_string(parser, NULL, buffer->content, buffer->size);
    displaySyntax(buffer);
}

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

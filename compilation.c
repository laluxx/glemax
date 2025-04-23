#include "compilation.h"
#include "syntax.h"
#include "theme.h"
#include "ctype.h"

void parse_and_push_compilation_syntax(Buffer *buffer) {
    if (!buffer || !buffer->content) return;
    
    // Clear existing syntax highlighting
    clearSyntaxArray(buffer);
    
    const char *text = buffer->content;
    size_t length = buffer->size;
    
    // Keywords to look for
    const char *errorKeyword = "error";
    const char *warningKeyword = "warning";
    
    // Track the last type we found (error or warning) for error indicators
    bool lastWasError = false;
    bool lastWasWarning = false;
    
    size_t pos = 0;
    while (pos < length) {
        // Find next line
        size_t lineStart = pos;
        while (pos < length && text[pos] != '\n') pos++;
        size_t lineEnd = pos;
        if (pos < length) pos++;  // Skip the newline
        
        // Process the current line
        for (size_t i = lineStart; i < lineEnd; i++) {
            // Color specific characters: '/', '.', and ':'
            if (text[i] == '/' || text[i] == '.' || text[i] == ':') {
                Syntax charSyntax = {i, i + 1, &CT.comment};
                insertSyntax(&buffer->syntaxArray, charSyntax);
                
                // Check for filename pattern (something.c)
                if (text[i] == '.' && i > lineStart && i + 1 < lineEnd) {
                    // Check if characters around the dot are valid for filenames
                    if (isalnum(text[i-1]) && isalnum(text[i+1])) {
                        // Highlight the basename (before the dot)
                        size_t nameStart = i;
                        while (nameStart > lineStart && 
                               (isalnum(text[nameStart-1]) || text[nameStart-1] == '_' || text[nameStart-1] == '-')) {
                            nameStart--;
                        }
                        Syntax nameSyntax = {nameStart, i, &CT.text};
                        insertSyntax(&buffer->syntaxArray, nameSyntax);
                        
                        // Highlight the extension (after the dot)
                        size_t extEnd = i + 1;
                        while (extEnd < lineEnd && 
                               (isalnum(text[extEnd]) || text[extEnd] == '_' || text[extEnd] == '-')) {
                            extEnd++;
                        }
                        Syntax extSyntax = {i + 1, extEnd, &CT.variable};
                        insertSyntax(&buffer->syntaxArray, extSyntax);
                        
                        // Skip past the extension we just processed
                        i = extEnd - 1;
                    }
                }
            }
            
            // Check for "error" keyword
            else if (i + strlen(errorKeyword) <= lineEnd &&
                strncmp(text + i, errorKeyword, strlen(errorKeyword)) == 0) {
                
                // Make sure it's "error:" or "error "
                if (i + strlen(errorKeyword) < lineEnd && 
                    (text[i + strlen(errorKeyword)] == ':' || text[i + strlen(errorKeyword)] == ' ')) {
                    
                    // Highlight just the word "error"
                    Syntax errorSyntax = {i, i + strlen(errorKeyword), &CT.error};
                    insertSyntax(&buffer->syntaxArray, errorSyntax);
                    
                    lastWasError = true;
                    lastWasWarning = false;
                    
                    // Skip past the highlighted word
                    i += strlen(errorKeyword) - 1;
                }
            }
            
            // Check for "warning" keyword
            else if (i + strlen(warningKeyword) <= lineEnd &&
                strncmp(text + i, warningKeyword, strlen(warningKeyword)) == 0) {
                
                // Make sure it's "warning:" or "warning "
                if (i + strlen(warningKeyword) < lineEnd && 
                    (text[i + strlen(warningKeyword)] == ':' || text[i + strlen(warningKeyword)] == ' ')) {
                    
                    // Highlight just the word "warning"
                    Syntax warningSyntax = {i, i + strlen(warningKeyword), &CT.warning};
                    insertSyntax(&buffer->syntaxArray, warningSyntax);
                    
                    lastWasError = false;
                    lastWasWarning = true;
                    
                    // Skip past the highlighted word
                    i += strlen(warningKeyword) - 1;
                }
            }
            
            // Check for quoted text (both single and double quotes)
            else if (text[i] == '\'' || text[i] == '"') {
                char quoteChar = text[i];
                size_t quoteStart = i;
                i++; // Skip the opening quote
                
                // Find the closing quote
                while (i < lineEnd && text[i] != quoteChar) i++;
                
                if (i < lineEnd && text[i] == quoteChar) {
                    // Found the closing quote - highlight everything in between
                    Syntax stringSyntax = {quoteStart, i + 1, &CT.string};
                    insertSyntax(&buffer->syntaxArray, stringSyntax);
                }
            }
        }
        
        // Check for error indicators (^ or ~) on this line
        // Look for indicators but exclude lines with '|' at the beginning
        bool lineStartsWithPipe = (lineStart < lineEnd && text[lineStart] == '|');
        
        // Only check for indicators if the line doesn't start with '|'
        if (!lineStartsWithPipe) {
            for (size_t i = lineStart; i < lineEnd; i++) {
                if (text[i] == '^' || text[i] == '~') {
                    // Found an indicator character
                    size_t indicatorStart = i;
                    size_t indicatorEnd = i + 1;
                    
                    // Determine the color based on last seen error/warning
                    Color *color = lastWasError ? &CT.error : 
                                  (lastWasWarning ? &CT.warning : &CT.error);
                    
                    // Highlight each indicator character individually
                    while (i < lineEnd && (text[i] == '^' || text[i] == '~')) {
                        Syntax indicatorSyntax = {i, i + 1, color};
                        insertSyntax(&buffer->syntaxArray, indicatorSyntax);
                        i++;
                    }
                    i--; // Adjust since the loop will increment i
                }
            }
        }
    }
}

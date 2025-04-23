#include "modeline.h"
#include "globals.h"
#include "isearch.h"
#include "theme.h"
#include "editor.h"
#include "stdio.h"
#include "lsp.h"

void addSegment(Segments *segments, const char *name, const char *content) {
    segments->segment = realloc(segments->segment, (segments->count + 1) * sizeof(Segment));
    
    // Initialize the new segment properly
    segments->segment[segments->count].name = strdup(name);
    segments->segment[segments->count].content = strdup(content);
    segments->segment[segments->count].codepoints = NULL;  // <-- CRITICAL!
    segments->segment[segments->count].length = 0;
    
    segments->count++;
}

void initSegments(Segments *segments) {
    segments->segment = NULL;
    segments->count = 0;
    addSegment(segments, "logo",          "NaL");
    addSegment(segments, "modified",      "NaM");
    addSegment(segments, "version",       "NaV");
    addSegment(segments, "changed",       "Nothing");
    addSegment(segments, "readonly",      "NaR");
    addSegment(segments, "noOtherWindow", "NoW");
    addSegment(segments, "name",          "NAME");
    addSegment(segments, "url",           "NaU");
    addSegment(segments, "line-number",   "LINE-NUMBER");
    addSegment(segments, "scroll",        "Top");
    addSegment(segments, "lsp",           "NOLSP");
    addSegment(segments, "path",          "NaP");
    addSegment(segments, "region-chars",  "NaRC");
    addSegment(segments, "region-lines",  "NaRL");
    addSegment(segments, "isearch",       "[NaC]");
    // TODO Right allign segments from
    // here to the right of the modeline
    addSegment(segments, "mode",          "Maybe");
    addSegment(segments, "scale",         "NaN");
    addSegment(segments, "completions",   "NaC");
    addSegment(segments, "branch",        ""); // FIXME
}

void updateSegmentCodepoints(Segment *segment) {
    if (segment == NULL || segment->content == NULL) {
        if (segment) {
            segment->codepoints = NULL;
            segment->length = 0;
        }
        return;
    }
    
    // Count codepoints
    const char *p = segment->content;
    size_t count = 0;
    uint32_t codepoint;
    
    while (*p) {
        p = utf8_to_codepoint(p, &codepoint);
        count++;
    }
    
    // Free old codepoints if they exist
    if (segment->codepoints != NULL) {
        free(segment->codepoints);
        segment->codepoints = NULL;
    }
    
    // Allocate new space only if needed
    if (count > 0) {
        segment->codepoints = malloc(count * sizeof(uint32_t));
        if (segment->codepoints == NULL) {
            // Handle allocation failure
            segment->length = 0;
            return;
        }
        
        // Fill codepoints array
        p = segment->content;
        for (size_t i = 0; i < count; i++) {
            p = utf8_to_codepoint(p, &segment->codepoints[i]);
        }
    }
    
    segment->length = count;
}

// We are in 144hzland here
void updateSegments(Modeline *modeline, Buffer *buffer) {
    for (size_t i = 0; i < modeline->segments.count; i++) {
        Segment *segment = &modeline->segments.segment[i];

        if (strcmp(segment->name, "line-number") == 0) {
            int lineNumber = getLineNumber(buffer);
            free(segment->content);
            char lineNumStr[32];
            snprintf(lineNumStr, sizeof(lineNumStr), "L%d", lineNumber);
            segment->content = strdup(lineNumStr);
        }
        else if (strcmp(segment->name, "branch") == 0) {
            free(segment->content);
            char* branch = getGitBranch(buffer->path);
            segment->content = branch;
        }
        else if (strcmp(segment->name, "name") == 0) {
            free(segment->content);
            segment->content = strdup(buffer->name);
        }
        else if (strcmp(segment->name, "url") == 0) {
            free(segment->content);
            segment->content = strdup(buffer->url);
        }


        else if (strcmp(segment->name, "completions") == 0) {
            // Free existing content if different from what we'll create
            static char last_display[64] = {0};
            char new_display[64] = {0};

            // Empty case
            if (ce.count == 0 || ce.currentIndex < 0) {
                if (!segment->content || strcmp(segment->content, "") != 0) {
                    free(segment->content);
                    segment->content = strdup("");
                }
                return;
            }

            // Configuration
            const int current_pos = ce.currentIndex + 1;
            const int total_items = ce.count;
    
            // Determine how many circles to show (minimum 1, maximum 7, or whatever limit you prefer)
            const int total_circles = (total_items > 7) ? 7 : (total_items < 1 ? 1 : total_items);

            // Track circle position separately from completion index
            static int filled_pos = 0;
            static int last_index = -1;
    
            // Update position based on navigation direction
            if (last_index != -1) {
                if (ce.currentIndex > last_index) { // Forward
                    filled_pos = (filled_pos + 1) % total_circles;
                }
                else if (ce.currentIndex < last_index) { // Backward
                    filled_pos = (filled_pos - 1 + total_circles) % total_circles;
                }
            }
            last_index = ce.currentIndex;

            const char *sep    = "—"; // E2 80 94
            const char *filled = "●"; // E2 97 8F
            const char *empty  = "○"; // E2 97 8B

            // Build display string safely
            int pos = snprintf(new_display, sizeof(new_display), "%d ", current_pos);
    
            for (int i = 0; i < total_circles && pos < sizeof(new_display) - 10; i++) {
                if (i > 0) {
                    strncpy(new_display + pos, sep, sizeof(new_display) - pos);
                    pos += strlen(sep);
                }
                const char *circle = (i == filled_pos) ? filled : empty;
                strncpy(new_display + pos, circle, sizeof(new_display) - pos);
                pos += strlen(circle);
            }

            snprintf(new_display + pos, sizeof(new_display) - pos, " %d", total_items);

            // Only update if changed
            if (strcmp(last_display, new_display) != 0) {
                free(segment->content);
                segment->content = strdup(new_display);
                strncpy(last_display, new_display, sizeof(last_display));
            }
        }

        else if (strcmp(segment->name, "lsp") == 0) {
            free(segment->content);
            char *bufferDir = getBufferDirectory(buffer->path);
            if (lspp()) {
                segment->content = strdup("");
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "path") == 0) {
            free(segment->content);
            if (strcmp(buffer->name, buffer->path) != 0) {
                segment->content = strdup(buffer->path);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "modified") == 0) {
            free(segment->content);
            if (buffer->modified) {
                segment->content = strdup("󰳻");
            } else {
                segment->content = strdup("");
            }
        }
        
        else if (strcmp(segment->name, "version") == 0) {
            free(segment->content);
            char version_str[32]; // size_t
            snprintf(version_str, sizeof(version_str), "%zu", buffer->version);
            if (buffer->version != 0) {
                segment->content = strdup(version_str);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "changed") == 0) {
            free(segment->content);
            if (strstr(buffer->content, buffer->originalContent)) {
                segment->content = strdup("");
            } else {
              segment->content = strdup("C");
            }
        }
        else if (strcmp(segment->name, "mode") == 0) {
            free(segment->content);
            segment->content = strdup(buffer->major_mode);
        }
        else if (strcmp(segment->name, "scale") == 0) {
            free(segment->content);
            char scaleStr[32];
            snprintf(scaleStr, sizeof(scaleStr), "%d", buffer->scale.index);
            segment->content = strdup(scaleStr);
        }
        else if (strcmp(segment->name, "isearch") == 0) {
            free(segment->content);
            if (isearch.count > 0) {
                char countStr[32];
                snprintf(countStr, sizeof(countStr), "[%zu]", isearch.count);
                segment->content = strdup(countStr);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "readonly") == 0) {
            free(segment->content);
            if (buffer->readOnly) {
                segment->content = strdup("󰌾");
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "noOtherWindow") == 0) {
            free(segment->content);
            if (modeline->window && modeline->window->parameters.noOtherWindow) {
                segment->content = strdup("NoW");
            } else {
                segment->content = strdup("");
            }
        }


        else if (strcmp(segment->name, "region-chars") == 0) {
            if (buffer->region.active) {
                free(segment ->content);
                // Calculate the number of characters selected
                size_t chars_selected = buffer->region.end - buffer->region.start;
                char chars_str[32];
                snprintf(chars_str, sizeof(chars_str), "%zu", chars_selected);
                segment->content = strdup(chars_str);
            } else {
                segment->content = strdup("");
            }
            // removed
        }

        else if (strcmp(segment->name, "region-lines") == 0) {
            if (buffer->region.active) {
                free(segment ->content);
                // Calculate the number of lines selected
                size_t lines_selected = 0;
                for (size_t i = buffer->region.start; i < buffer->region.end; i++) {
                    if (buffer->content[i] == '\n') {
                        lines_selected++;
                    }
                }
                char lines_str[32];
                snprintf(lines_str, sizeof(lines_str), "%zu", lines_selected);
                segment->content = strdup(lines_str);
            } else {
                segment->content = strdup("");
            }
        }

        else if (strcmp(segment->name, "logo") == 0) {
            free(segment->content);
            if (strcmp(buffer->major_mode, "c") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->name, "*eshell*") == 0) {
                segment->content = strdup("󱆃");
            } else if (strcmp(buffer->major_mode, "html") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "d") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "scheme") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "glsl") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "eterm") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "gemini") == 0) {
                segment->content = strdup("󰯂");
            } else if (strcmp(buffer->major_mode, "query") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "zig") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "odin") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "make") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "commonlisp") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "scss") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "haskell") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "lua") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "rust") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "bash") == 0) {
                segment->content = strdup("󱆃");
            } else if (strcmp(buffer->major_mode, "elisp") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "python") == 0) {
                segment->content = strdup("󰌠");
            } else if (strcmp(buffer->major_mode, "ocaml") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "css") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "javascript") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "julia") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "cpp") == 0) {
                segment->content = strdup("󰙲");
            } else if (strcmp(buffer->major_mode, "go") == 0) {
                segment->content = strdup("󰟓");
            } else if (strcmp(buffer->major_mode, "json") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "regex") == 0) {
                segment->content = strdup("󰑑");
            } else if (strcmp(buffer->major_mode, "compilation") == 0) {
                segment->content = strdup("");
            } else if (strcmp(buffer->major_mode, "dired") == 0) {
                segment->content = strdup("󰉋");
            } else {
              segment->content = strdup("󰦨");
            }
        }
    }
}

void drawModelines(WindowManager *wm, Font *font, float minibufferHeight, Color color) {
    for (Window *win = wm->head; win != NULL; win = win->next) {
        bool isBottom = true;
        for (Window *checkWin = wm->head; checkWin != NULL; checkWin = checkWin->next) {
            if (win != checkWin && win->x == checkWin->x && win->y - win->height == checkWin->y) {
                isBottom = false;
                break;
            }
        }
        
        // Use win->buffer->font to calculate modelineBaseY
        float modelineBaseY = win->y - win->height + win->buffer->font->ascent - win->buffer->font->descent;
        if (isBottom) {
            modelineBaseY += minibufferHeight;
            modelineBaseY -= win->buffer->font->ascent - win->buffer->font->descent;
        }
        
        useShader("simple");
        float width = win->splitOrientation == VERTICAL ? win->width - 1 : win->width;
        drawRectangle((Vec2f){win->x, modelineBaseY}, (Vec2f){width, win->modeline.height}, color);
        flush();
        
        // Draw each segment in the modeline
        useShader("text");
        float spaceWidth = getCharacterWidth(font, ' ');
        float segmentX = fringe + spaceWidth + win->x;
        // Update modeline segments NOTE We should not do it every frame N times
        // where N is the number of modelines we have
        updateSegments(&win->modeline, win->buffer);
        
        for (size_t i = 0; i < win->modeline.segments.count; i++) {
            Segment *segment = &win->modeline.segments.segment[i];
            if (segment->content == NULL || segment->content[0] == '\0') continue; // Skip drawing if the segment is empty
            
            // Make sure codepoints are up to date
            updateSegmentCodepoints(segment);
            
            float textY = modelineBaseY + (font->ascent - font->descent);
            
            for (size_t j = 0; j < segment->length; j++) {
                uint32_t codepoint = segment->codepoints[j];
                
                // Handle ASCII characters with the regular drawChar
                if (codepoint < 128) {
                    drawChar(font, (char)codepoint, segmentX, textY, 1.0, 1.0, CT.text);
                    segmentX += getCharacterWidth(font, (char)codepoint);
                }
                // Handle Unicode characters
                else {
                    // Ensure the Unicode character is loaded
                    if (!loadUnicodeGlyph(font, codepoint)) {
                        // Fallback if glyph can't be loaded
                        drawChar(font, '?', segmentX, textY, 1.0, 1.0, CT.text);
                        segmentX += getCharacterWidth(font, '?');
                        continue;
                    }
                    
                    Character *ch = findUnicodeCharacter(font, codepoint);
                    if (ch) {
                        drawUnicodeChar(font, ch, segmentX, textY, 1.0, 1.0, CT.text);
                        segmentX += ch->ax; // Use the character's advance
                    } else {
                        // Fallback if character not found
                        drawChar(font, '?', segmentX, textY, 1.0, 1.0, CT.text);
                        segmentX += getCharacterWidth(font, '?');
                    }
                }
            }
            segmentX += spaceWidth;
        }
        flush();
    }
}

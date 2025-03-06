#include "buffer.h"
#include "extension.h"
#include "screen.h"
#include "commands.h"
#include "completion.h"
#include "edit.h"
#include "faces.h"
#include "history.h"
#include "isearch.h"
#include "keychords.h"
#include "syntax.h"
#include "theme.h"
#include "wm.h"
#include <GLFW/glfw3.h>
#include <common.h>
#include <ctype.h>
#include <font.h>
#include <input.h>
#include <libguile.h>
#include <lume.h>
#include <renderer.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "draw.h"
#include "globals.h"
#include "git.h"
#include "clock.h"
#include <string.h>
#include <unistd.h>
#include "gemini.h"

// [/] MAYBE One thing that we really should do, is to
// make the minibuffer just a window, not a special
// case to handle in every function that interact with it or maybe not,
// since we will always have only one minibuffer (even if recursive)

// FIXME many functions calculate the same exact data
// so the same stuff is recalculated like 6 or more times per frame
// i hate it, but i hate refactoring more


// TODO display-line-numbers-mode
//  - Option to color the last line with a different color

// TODO IMPORTANT Don't make xclip a dependencie if you have it in path
// use it, if you don't, don't


// TODO text-scale-increase() and text-scale-decrease() should increase or
// decrease the scale of the text inside the region if the region is active

// TODO Get the list of functions in the current buffer and a size_T pos using tresitter ?

// TODO hide-minibuffer-mode it will appear either lerped or not when message() is called
// it will stay there for N seconds and them hide the minibuffer again either lerped or not
// TODO draw diff-hl-mode fringe rectangles dimmed based on the scope level of the line
// TODO drawBufferRectangle(size_t start, size_t end) it will draw 3 rectanglwees maximum
// TODO diff-hl-mode should not rely on /bing/git and should do it interactively
// not only on save-buffer;
// TODO Option to color diff-hl-text with a gradient from the first line
// of the change/add to the last line of that hunk or per line gradient
// TODO add ARG to next-theme and previous-theme to switch to that theme
// TODO region-alpha global variable


// SPECIAL BUFFER
// TODO Make them *special* fr. I was thinking of a buffer of buffer
// each *special* buffer usually has a major mode dedicated to it,
// we don't have modes yet. each special mode will have a void function pointer
// to a drawing function, a gemini client is a good first *special* buffer.
// Where i can make mistakes on so i don't do them in dired


// SCOPES
// TODO Option to not render scopes on comments

// DIFF-HL
// TODO Option to draw diff-hl-bg ontop of the text

// REGION
// TODO Option to color text inside the region with CT.region_fg

// SCROLL
// TODO in updateScroll if we are going up or down fast, try to lerp 2 halfpages
// at once, scrolling a full page at once not in 2 steps, or ever 3..
// This should make fast lerped scrolling feel better

// WINDOWS 
// TODO Dim other windows
// TODO Shared Window Geometry
// TODO the point should be per window not per buffer that would force me to make the minibuffer
// a window i won't do it btw buffers are better than windows

// CURSOR
// TODO cursor_lerp_mode
// TODO cursor_color_lerp_mode
// TODO bool cursor-lerp-only-on-line-change
// TODO unhardcode the cursor scroll it shoudl use the modelineHeight + minibufferHeight

// NEW
// TODO dabbrev-completion
// TODO rainbow-delimiters-mode (we can do it for free now that we track scopes)
// TODO revert-buffer-mode
// TODO wdired
// TODO iedit
// TODO Option to not let the point over correct indentation

// TODO nextBuffer() and previousBuffer() should update the wm->buffer also switchToBuffer
// Do this when we implement the minibuffer as a window
// NOTE I-searrch for "if (isCurrentBuffer(&bm, "minibuffer")"

// TODO color unmatched characters in isearch
// TODO unhardcode the keybinds
// TODO Object based undo system

// FIXME use setBufferContent() to set the prompt aswell

// TODO rainbow-delimiters-mode

#include "editor.h"

void drawHighlight(WindowManager *wm, Font *font, size_t startPos, size_t length, Color highlightColor);
void highlightMatchingBrackets(WindowManager *wm, Font *font, Color highlightColor);
void highlightAllOccurrences(WindowManager *wm, const char *searchText, Font *font, Color highlightColor);
void drawRegion(WindowManager *wm, Font *font, Color regionColor);
void drawModelines(WindowManager *wm, Font *font, float minibufferHeight, Color color);
void drawBuffer(Window *win, Buffer *buffer, bool cursorVisible, bool colorPoint);
void highlightHexColors(WindowManager *wm, Font *font, Buffer *buffer, bool rm);
int getGlobalArg(Buffer *argBuffer);
void updateScroll(Window *window);
void updateScrollLerp(Window *window);
void scrollCallback(double xOffset, double yOffset);
void cursorPosCallback(double xpos, double ypos);
void mouseButtonCallback(int button, int action, int mods);
float lerp(float a, float b, float t);
void drawFringe(Window *win, Font *font);
void updateMouseWheelLerp(Window *window);
void updateCursorAfterScroll(Window *win);

void updateMouseWheelLerp(Window *window);
void updateBufferAnimations(BufferManager *bm);

// These shoule be in c-mode module
static bool shouldSkipCharacter(Buffer *buffer, unsigned int codepoint);
static bool isIncludeDirectiveAngleBracket(Buffer *buffer, unsigned int codepoint);
static void insertIncludeAngleBrackets(Buffer *buffer);
static void insertCharWithElectricPairs(Buffer *buffer, unsigned int codepoint);

void keyCallback(int key, int action, int mods);
void textCallback(unsigned int codepoint);


static void inner_main(void *closure, int argc, char **argv) {
    init_scheme_environment();  // Initialize Guile
    init_glemax_bindings();    // Set up our module
    
    (void)closure; // Unused parameters
    (void)argc;
    (void)argv;
    
    initWindow(sw, sh, "*scratch* - Glemax");
    registerTextCallback(textCallback);
    registerKeyCallback(keyCallback);
    registerScrollCallback(scrollCallback);
    registerCursorPosCallback(cursorPosCallback);
    registerMouseButtonCallback(mouseButtonCallback);

    font = loadFont(fontPath, fontsize, "name");
    initThemes();
    load_theme(first_theme_name);
    initGlobalParser();
    initKillRing(&kr, kill_ring_max);
    initBufferManager(&bm);
    initCommands();
    initOpenssl();
    newBuffer(&bm, &wm, "minibuffer", "~/", fontPath, sw, sh);
    newBuffer(&bm, &wm, "prompt",     "~/", fontPath, sw, sh);
    newBuffer(&bm, &wm, "message",    "~/", fontPath, sw, sh);
    newBuffer(&bm, &wm, "arg",        "~/", fontPath, sw, sh);
    newBuffer(&bm, &wm, "*scratch*",  "~/", fontPath, sw, sh);

    setBufferContent(getBuffer(&bm, "*scratch*"), scratch_buffer_content, true);

    if (argc > 1) {
        const char *filePath = argv[1];
        newBuffer(&bm, &wm, filePath, filePath, fontPath, sw, sh);
        bm.lastBuffer = getBuffer(&bm, filePath);
        if (bm.lastBuffer) {
            // Use find_file to load the content
            Buffer *minibuffer = getBuffer(&bm, "minibuffer");
            setBufferContent(minibuffer, filePath, true);
            find_file(&bm, &wm, sw, sh);
        }
    } else {
        bm.lastBuffer = getBuffer(&bm, "*scratch*");
    }



    if (!fringe_mode) fringe = 0;

    sw = getScreenWidth();  // NOTE Currently get screen dimentions
    sh = getScreenHeight(); // only once at startup
    
    initWindowManager(&wm, &bm, font, sw, sh);

    initDiffs(&bm);

    initSegments(&wm.activeWindow->modeline.segments);
    updateSegments(&wm.activeWindow->modeline, wm.activeWindow->buffer);
    /* updateWindows(&wm, font, sw, sh); // NOTE We will do it later*/
    
    load_init_file();
    
    while (!windowShouldClose()) {
        sw = getScreenWidth();  // TODO Update in
        sh = getScreenHeight(); // the resize callback

        if (theme_lerp_mode) updateThemeInterpolation();
        
        /* updateWindows(&wm, font, sw, sh); */ 
        /* reloadShaders(); // NOTE Recompile all shaders each frame */
        Buffer *prompt     = getBuffer(&bm, "prompt");
        Buffer *minibuffer = getBuffer(&bm, "minibuffer");
        Buffer *message    = getBuffer(&bm, "message");
        
        Window *win = wm.head;
        Window *activeWindow = wm.activeWindow;
        Buffer *currentBuffer = activeWindow->buffer;


        beginDrawing();
        clearBackground(CT.bg);

        float promptWidth = 0;
        for (size_t i = 0; i < strlen(prompt->content); i++) {
            promptWidth += getCharacterWidth(minibuffer->font, prompt->content[i]);
        }
        
        int lineCount = 1; // Start with 1 to account for content without any newlines
        for (int i = 0; i < strlen(minibuffer->content); i++) {
            if (minibuffer->content[i] == '\n') {
                lineCount++;
            }
        }
        
        float lineHeight = (minibuffer->font->ascent + minibuffer->font->descent);
        
        // Set the height based on the number of lines found
        float minibufferHeight = lineHeight * lineCount;
        
        // PROMPT TEXT
        drawTextEx(minibuffer->font, prompt->content, fringe,
                   minibufferHeight -
                   (minibuffer->font->ascent - minibuffer->font->descent),
                   1.0, 1.0, CT.minibuffer_prompt, CT.bg, -1, cursorVisible,
                   "text");
        
        if (isCurrentBuffer(&bm, "minibuffer")) {
            drawMiniCursor(minibuffer, minibuffer->font, fringe + promptWidth,
                           minibufferHeight - minibuffer->font->ascent, CT.cursor);
        } else {
            // DrawMiniHollowCursor() :(
            /* drawHollowCursor(minibuffer, win, CT.cursor); // And hollow it */
        }
        
        drawModelines(&wm, font, minibufferHeight, CT.modeline);

        for (; win != NULL; win = win->next) {
            Buffer *buffer = win->buffer;
            bool bottom = isBottomWindow(&wm, win);
            float scissorStartY = win->y - win->height + minibufferHeight + win->modeline.height;
            float scissorHeight = sh - scissorStartY;
            if (bottom) {
                scissorHeight += minibufferHeight;
            }

            /* if (scroll_lerp_mode) */ updateScrollLerp(win); // NOTE Do it for every window
            if (mouse_wheel_lerp_mode) updateMouseWheelLerp(win);

            beginScissorMode((Vec2f){win->x, scissorStartY},
                             (Vec2f){win->width, scissorHeight});

            draw_scopes(&wm, buffer->font);

            if (win == wm.activeWindow) {
                highlightHexColors(&wm, buffer->font, buffer, rainbow_mode);
                useShader("simple");
                drawFringe(win, font);
                drawRegion(&wm, buffer->font, CT.region); // TODO ALPHA
                highlightMatchingBrackets(&wm, buffer->font, CT.show_paren_match);
                if (isearch.searching)
                    highlightAllOccurrences(&wm, minibuffer->content, font,
                                            CT.isearch_highlight);


                // Draw cursor after all "overlays"
                if (!isCurrentBuffer(&bm, "minibuffer")) {
                    if (buffer->region.active) {
                        if (hide_region_mode) {
                            drawCursor(currentBuffer, wm.activeWindow, CT.null);
                        } else {
                            drawCursor(currentBuffer, wm.activeWindow, CT.cursor);
                        }
                    } else {
                        drawCursor(currentBuffer, wm.activeWindow, CT.cursor);
                    }
                }

                
                flush();

                if (mark_mode) drawMark(buffer, win, CT.cursor);

                if (isCurrentBuffer(&bm, "minibuffer")) {
                    drawBuffer(win, buffer, cursorVisible, false); // Hide cursor
                    drawHollowCursor(win->buffer, win, CT.cursor); // And hollow it
                } else {
                    drawBuffer(win, buffer, cursorVisible, true);
                    if (minimap_mode) {
                        drawMinimap(&wm, win, buffer);
                    }
                    
                }
                
            } else {
                if (win->buffer != wm.activeWindow->buffer) {
                    drawHollowCursor(win->buffer, win, CT.cursor);
                }
                drawBuffer(win, buffer, cursorVisible, false);
            }
            endScissorMode();
        }
        
        float minibufferWidth = promptWidth;
        
        for (size_t i = 0; i < strlen(minibuffer->content); i++) {
            minibufferWidth +=
                getCharacterWidth(minibuffer->font, minibuffer->content[i]);
        }
        
        // MINIBUFFER TEXT
        drawTextEx(minibuffer->font, minibuffer->content, promptWidth + fringe,
                   minibufferHeight -
                   (minibuffer->font->ascent - minibuffer->font->descent),
                   1.0, 1.0, CT.text, CT.bg, minibuffer->point, cursorVisible,
                   "text");
        
        // MESSAGE TEXT
        float lastLineWidth = getCharacterWidth(minibuffer->font, 32);
        size_t lastLineStart =
            strrchr(minibuffer->content, '\n')
            ? strrchr(minibuffer->content, '\n') - minibuffer->content + 1
            : 0;
        for (size_t i = lastLineStart; i < strlen(minibuffer->content); i++) {
            lastLineWidth +=
                getCharacterWidth(minibuffer->font, minibuffer->content[i]);
        }
        
        float lastLineY = minibufferHeight - (lineHeight * lineCount) + getCharacterWidth(minibuffer->font, 32);
        
        drawTextEx(minibuffer->font, message->content, promptWidth + lastLineWidth,
                   lastLineY, 1.0, 1.0, CT.message, CT.bg, message->point,
                   cursorVisible, "text");

        drawFPS(font, 400.0, 400.0, RED);

        // Draw Clock
        time_t t = time(NULL);
        struct tm *tm = localtime(&t);
        int hours = tm->tm_hour;
        int minutes = tm->tm_min;
        drawClock(hours, minutes);

        endDrawing();
    }
    
    freeGlobalParser();
    freeFont(font);
    freeKillRing(&kr);
    freeBufferManager(&bm);
    freeWindowManager(&wm);
    closeWindow();
}

// Is here where early-init.scm should be evaluated ?
int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, inner_main, 0);
    return 0; // NOTE This line is never reached
}


// TODO comment-dwim M-; or R-c
void keyCallback(int key, int action, int mods) {
    Window * win = wm.activeWindow;
    Buffer *buffer = isCurrentBuffer(&bm, "minibuffer") ? getBuffer(&bm, "minibuffer") : wm.activeWindow->buffer;
    
    Buffer *minibuffer = getBuffer(&bm, "minibuffer");
    Buffer *prompt = getBuffer(&bm, "prompt");
    Buffer *messageBuffer = getBuffer(&bm, "message");
    Buffer *argBuffer = getBuffer(&bm, "arg");
    int arg = getGlobalArg(argBuffer);


    bool shiftPressed = mods & GLFW_MOD_SHIFT;
    bool ctrlPressed = mods & GLFW_MOD_CONTROL;
    bool altPressed = mods & GLFW_MOD_ALT;

    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
        /* // NOTE Handle global arg */
        if (ctrlPressed && key >= KEY_0 && key <= KEY_9) {
            char digit = '0' + (key - KEY_0);  // Convert key code to corresponding character
            
            if (argBuffer->size == 1 && argBuffer->content[0] == '0' && digit != '0') {
                argBuffer->content[0] = digit;
            } else if (!(argBuffer->size == 1 && argBuffer->content[0] == '0' && digit == '0')) {
                insertChar(argBuffer, digit);
            }
            
            message(argBuffer->content);
            return;
        }
        
        if (!isCurrentBuffer(&bm, "minibuffer") && !isearch.searching && (!ctrlPressed && !altPressed)) {
            cleanBuffer(&bm, "minibuffer");
        }
        
        cleanBuffer(&bm, "message");
        
        cleanBuffer(&bm, "arg");
        
        switch (key) {
            
            case KEY_BACKSPACE:
            bool dont;
            if (buffer->region.active && !isearch.searching) {
                kill_region(buffer, &kr);
                
            } else if (isearch.searching) {
                if (altPressed || ctrlPressed) {
                    backward_kill_word(minibuffer, &kr);
                } else if (isearch.count > 0) {
                    jumpLastOccurrence(buffer, minibuffer->content);
                    isearch.startIndex = buffer->point - strlen(minibuffer->content);
                    if (isearch.count == 1) dont = true;
                    isearch.count--;
                } else {
                    backspace(minibuffer, electric_pair_mode);
                }
                if (minibuffer->size > 0 && isearch.count == 0 && !dont) {
                    isearch_forward(buffer, &bm, minibuffer, false);
                } else if (isearch.count == 0 && !dont) {
                    // If the minibuffer is empty, move the cursor back to where the search started
                    buffer->point = isearch.startIndex;
                    // isearch.searching = false;  NOTE Keep searching (like emacs)
                }
            } else {
                if (altPressed || ctrlPressed) {
                    backward_kill_word(buffer, &kr);
                } else {
                    backspace(buffer, electric_pair_mode);
                    if (electric_indent_mode)
                      indent(buffer, indentation, &bm, arg);
                }
            }
            break;
            

            case KEY_BACKSLASH:
                if (ctrlPressed) {
                    minimap_mode = !minimap_mode;
                }
                /* if (shiftPressed) { */
                /*     if (buffer->region.active) { */
                /*         /\* pipethathshit(buffer); *\/ */
                /*     } */
                /* } */
            break;

        case KEY_UNKNOWN:
            scroll(win, 1);
            break;

            
            case KEY_SPACE:
            if (ctrlPressed && shiftPressed) {
                set_mark(buffer, buffer->point);
            } else if (ctrlPressed) {
              set_mark_command(buffer);
            }
            break;
            case KEY_PERIOD:
            if (altPressed && shiftPressed) end_of_buffer(buffer);
            break;
            case KEY_COMMA:
            if (altPressed && shiftPressed) beginning_of_buffer(buffer);
            break;
            case KEY_ENTER:
                enter(buffer, &bm, &wm, minibuffer, prompt, indentation, electric_indent_mode, sw, sh, &nh, arg);
            break;
            case KEY_Y:
            if (ctrlPressed)
                yank(buffer, &kr, arg);
            break;
            case KEY_SEMICOLON:
            if (altPressed && shiftPressed) {
                eval_expression(&bm);
            }
            break;
            
            case KEY_2:
            if (ctrl_x_pressed) {
                split_window_below(&wm, font, getScreenWidth(), getScreenHeight());
                ctrl_x_pressed = false;
                eatchar = true;
            }
            break;
            
            case KEY_3:
            if (ctrl_x_pressed) {
                split_window_right(&wm, font, getScreenWidth(), getScreenHeight());
                ctrl_x_pressed = false;
                eatchar = true;
            }
            break;
            case KEY_0:
            if (ctrl_x_pressed) {
                delete_window(&wm);
                ctrl_x_pressed = false;
                eatchar = true;
                return;
            }
            break;

            case KEY_PAGE_UP:
              scroll_down(win, arg);
              break;

            case KEY_PAGE_DOWN:
              scroll_up(win, arg);
              break;

            case KEY_Z:
            /* printfSyntaxTree() */
            /* moveTo(buffer, 10, 300); */
            /* printSyntaxInfo(buffer); */
            if (ctrlPressed) {
                /* printf("Buffer under cursor: %s\n", getBufferUnderCursor(&wm)->name); */
                /* (insert_guile_symbols(buffer, &bm)); */

                GeminiOutput go = gemini_fetch("gemini://geminiprotocol.net/docs/", buffer);
                setBufferContent(buffer, go.content, false);
                setMajorMode(buffer, "gemini");

                /* buffer->content = go.content; */
                /* buffer->size = go.size; */
                
            } else if (altPressed) {
                keep_lines(&bm, &wm);
            }
            break;
            
            case KEY_X:
            if (ctrlPressed && ctrl_x_pressed) {
                exchange_point_and_mark(buffer);
            } else if (ctrlPressed) {
                ctrl_x_pressed = true;
                /* message("C-x-"); */ // It doesn't work with how i've
                // implemented the minibuffer lmao
                /* printActiveWindowDetails(&wm); */
            } else if (altPressed) {
                execute_extended_command(&bm);
            }
            break;
            
            case KEY_R:
            if (ctrlPressed) {
                if (!isearch.searching) {
                    isearch.searching = true;
                    minibuffer->size = 0;
                    minibuffer->content[0] = '\0';
                    isearch.lastMatchIndex = buffer->point;  // Start backward search from the current point
                    prompt->content = strdup("I-search backward: ");
                } else {
                    // If the minibuffer is empty and there was a previous search, reload it
                    if (minibuffer->size == 0 && isearch.lastSearch) {
                        if (minibuffer->content) free(minibuffer->content);
                        minibuffer->content = strdup(isearch.lastSearch);
                        minibuffer->size = strlen(minibuffer->content);
                        minibuffer->point = minibuffer->size;
                    }
                    isearch.lastMatchIndex = buffer->point;  // Set start point for the next backward search
                    isearch_backward(buffer, minibuffer, true);  // Continue search backward
                }
            } else if (altPressed) {
                reloadShaders();
            }
            break;
            
            
        case KEY_S:
            if (altPressed) {
                hl_scope_mode = !hl_scope_mode;
            } else if (ctrlPressed) {
                if (ctrl_x_pressed) {
                    save_buffer(&bm, buffer);
                    ctrl_x_pressed = false;
                } else if (!isearch.searching) {
                    isearch.searching = true;
                    minibuffer->size = 0;
                    minibuffer->content[0] = '\0';
                    isearch.startIndex = buffer->point;
                    prompt->content = strdup("I-search: ");
                } else {
                    // Ensures that we start the search from the right point even if minibuffer hasn't changed
                    if (minibuffer->size == 0 && isearch.lastSearch) {
                        if (minibuffer->content) free(minibuffer->content);
                        minibuffer->content = strdup(isearch.lastSearch);
                        minibuffer->size = strlen(minibuffer->content);
                        minibuffer->point = minibuffer->size;
                    } else {
                        isearch.count += 1;
                        printf("search count: %i\n", isearch.count);
                    }
                    isearch.startIndex = buffer->point;  // Update to ensure search starts from next position
                    isearch_forward(buffer, &bm, minibuffer, true);  // Continue search from new start index
                }
            }
            break;
            case KEY_W:
            if (ctrlPressed) {
                kill_region(buffer, &kr);
            } else if (altPressed) {
                kill_ring_save(buffer, &kr);                
            }
            break;
            case KEY_M:
            if (ctrlPressed) {
                enter(buffer, &bm, &wm, minibuffer, prompt, indentation, electric_pair_mode, sw, sh, &nh, arg);
            } else if (altPressed) {
                minimap_mode = !minimap_mode;
            }
            break;
            case KEY_C:
                if (ctrlPressed) {
                    ctrl_c_pressed = true;
                    message("C-c-");
                } else if (altPressed) {
                    capitalize_word(buffer);
                }
            break;
            
            case KEY_V:
                if (ctrlPressed) {
                    /* eval_expression(&bm); */
                    scroll(win, arg);
                }
            break;

            case KEY_T:
              if (ctrlPressed) {
                  transpose_words(buffer, arg);
              } else if (altPressed) {
                  transpose_chars(buffer);
              }
              break;

            case KEY_G:
            if (ctrlPressed){
                ctrl_x_pressed = false;
                
                resetHistoryIndex(&nh, prompt->content); // NOTE prompt->content is the history name
                if (isearch.searching) {
                    buffer->point = isearch.startIndex;
                    cleanBuffer(&bm, "minibuffer");
                    cleanBuffer(&bm, "prompt");
                    isearch.searching = false;
                    isearch.count = 0;
                } else {
                    buffer->region.active = false;
                    buffer->region.marked = false;
                    cleanBuffer(&bm, "minibuffer");
                    cleanBuffer(&bm, "prompt");
                    switchToBuffer(&bm, bm.lastBuffer->name);
                    cleanBuffer(&bm, "message");
                }
                
            } else if (altPressed) {
                goto_line(&bm, &wm, sw, sh);
            }
            break;
            
            
            case KEY_I:
            if (ctrlPressed) {
                if (shiftPressed) {
                    removeIndentation(buffer, indentation);                    
                } else {
                    addIndentation(buffer, indentation);                    
                }
            }
            break;
            case KEY_6:
            if (altPressed && shiftPressed) delete_indentation(buffer, &bm, arg);
            break;
            case KEY_TAB:
            if (isCurrentBuffer(&bm, "minibuffer") && strcmp(prompt->content, "Find file: ") == 0) {
                if (!ce.isActive || strcmp(minibuffer->content, ce.items[ce.currentIndex]) != 0) {
                    fetch_completions(minibuffer->content, &ce);
                    ce.currentIndex = 0; // Start from the first ce.
                    insert_completions(minibuffer, &ce);
                } else {
                    if (shiftPressed) {
                        // Move to the previous ce, wrapping around if necessary.
                        if (ce.currentIndex == 0) {
                            ce.currentIndex = ce.count - 1;
                        } else {
                            ce.currentIndex--;
                        }
                    } else {
                        // Cycle through the completions.
                        ce.currentIndex = (ce.currentIndex + 1) % ce.count;
                    }
                }
                
                // Set the minibuffer content to the current ce and update necessary fields.
                if (ce.count > 0) {
                    setBufferContent(minibuffer, ce.items[ce.currentIndex], true);
                }
            } else {
                if (buffer->region.active) {
                    indent_region(buffer, &bm, indentation, arg);
                } else {
                    indent(buffer, indentation, &bm, arg);
                }
            }
            break;
            case KEY_DOWN:
            if (ctrlPressed) {
                forward_paragraph(buffer, shiftPressed);
            } else {
                next_line(buffer, shiftPressed, &bm, buffer->goal_column);
            }
            break;
            case KEY_UP:
            if (ctrlPressed) {
                backward_paragraph(buffer, shiftPressed);
            } else {
                previous_line(buffer, shiftPressed, &bm, buffer->goal_column);
            }
            break;
            case KEY_LEFT:
            if (ctrlPressed) {
                backward_word(buffer, 1, shiftPressed);
            } else {
                left_char(buffer, shiftPressed, &bm, arg);
            }
            break;
            case KEY_RIGHT:
            if (ctrlPressed) {
                forward_word(buffer, 1, shiftPressed);
            } else {
                right_char(buffer, shiftPressed, &bm, arg);
            }
            break;
            case KEY_DELETE:
            delete_char(buffer, &bm);
            break;
            case KEY_N:
            if (ctrlPressed && altPressed) {
                forward_list(buffer, arg);
            } else if (ctrlPressed) {
                if (ctrl_c_pressed) {
                    diff_hl_next_hunk(buffer);
                    ctrl_c_pressed = false;
                } else if (ctrl_x_pressed) {
                    set_goal_column(buffer);
                } else {
                  next_line(buffer, shiftPressed, &bm, buffer->goal_column);
                }
            } else if (altPressed) {
                if (isCurrentBuffer(&bm, "minibuffer")) {
                    next_history_element(&nh, prompt->content, minibuffer, &bm);
                } else {
                    forward_paragraph(buffer, shiftPressed);
                }
            } else if (ctrl_c_pressed) {
                ctrl_c_pressed = false;
                end_of_buffer(buffer);
                eatchar = true;
            }
            break;
            case KEY_P:
            if (ctrlPressed && altPressed) {
                backward_list(buffer, arg);
            } else if (ctrlPressed) {
                if (ctrl_c_pressed) {
                    diff_hl_previous_hunk(buffer);
                    ctrl_c_pressed = false;
                } else {
                    previous_line(buffer, shiftPressed, &bm, buffer->goal_column);
                }
            } else if (altPressed) {
                if (isCurrentBuffer(&bm, "minibuffer")) {
                    previous_history_element(&nh, prompt->content, minibuffer, &bm);
                } else {
                    backward_paragraph(buffer, shiftPressed);
                }
            } else if (ctrl_c_pressed) {
                ctrl_c_pressed = false;
                beginning_of_buffer(buffer);
                eatchar = true;
            }
            break;
            case KEY_F:
            if (ctrlPressed && altPressed) {
                forward_sexp(buffer, 1);
            } else if (ctrlPressed && ctrl_x_pressed) {
                find_file(&bm, &wm, sw, sh);
            } else if (ctrlPressed) {
                right_char(buffer, shiftPressed, &bm, arg);
            } else if (altPressed) {
                forward_word(buffer, 1, shiftPressed);
            }
            break;
            case KEY_B:
            if (ctrlPressed && altPressed) {
                backward_sexp(buffer, arg);
            } else if (ctrlPressed) {
                left_char(buffer, shiftPressed, &bm, arg);
            } else if (altPressed) {
                backward_word(buffer, 1, shiftPressed);
            }
            break;
            case KEY_E:
            if (ctrlPressed) {
                move_end_of_line(buffer, shiftPressed);
            }
            break;
            case KEY_A:
            if (ctrlPressed) move_beginning_of_line(buffer, shiftPressed);
            break;
            case KEY_HOME:
                if (ctrlPressed) {
                    beginning_of_buffer(buffer);
                } else {
                  move_beginning_of_line(buffer, shiftPressed);
                }
            break;
            case KEY_D:
                if (altPressed) {
                    diff_hl_mode = !diff_hl_mode;
                } else if (ctrlPressed) {
                if (buffer->region.active) {
                    kill_region(buffer, &kr);
                } else {
                    delete_char(buffer, &bm);
                }
            }
            break;
            case KEY_Q:
            if (altPressed) {
                delete_window(&wm);
                /* updateWindows(&wm, font, sw, sh); */
            }
            break;
            case KEY_O:
            if (altPressed) {
                other_window(&wm, 1);
            } else if (ctrlPressed && shiftPressed) {
                if (buffer->region.active) buffer->region.active = false;
                duplicate_line(buffer);
            } else if (ctrlPressed) {
              if (ctrl_x_pressed) {
                delete_blank_lines(buffer, arg);
                ctrl_x_pressed = false;
              } else {
                if (buffer->region.active)
                  buffer->region.active = false;
                open_line(buffer);
              }
            } else if (ctrl_x_pressed) {
                other_window(&wm, 1);
                eatchar = true;
            }
            break;
            
            case KEY_1:
            if (altPressed && shiftPressed) shell_command(&bm);
            break;
            
            case KEY_EQUAL:
                if (altPressed && ctrlPressed) {
                    text_scale_increase_by_buffer(&bm, "minibuffer", font->path, &wm, sh, arg);
                } else if (altPressed) {
                    switchToNextTheme();
                } else if (ctrlPressed) {
                    text_scale_increase(&bm, fontPath, &wm, sh, arg);                    
                }
            break;
            case KEY_MINUS:
                if (altPressed && ctrlPressed) {
                  text_scale_decrease_by_buffer(&bm, "minibuffer", font->path, &wm, sh, arg);
                } else if (altPressed) {
                switchToPreviousTheme();
            } else if (ctrlPressed) {
                text_scale_decrease(&bm, fontPath, &wm, sh, arg);                    
            }
            break;
            
            case KEY_L:
                if (ctrlPressed && altPressed) {
                    gemini_redirect(buffer);
                } else if (altPressed) {
                    split_window_right(&wm, font, sw, sh);
                    other_window(&wm, 1);
                } else if (ctrlPressed) {
                    recenter(win, false);
                }
            break;
            case KEY_J:
                if (altPressed && wm.count <= 1)  {
                split_window_below(&wm, font, sw, sw);
                other_window(&wm, 1);
            } else if (altPressed && shiftPressed) {
                swap_window(&wm, 1);
            } else if (altPressed) {
                other_window(&wm, 1);
            } else if (ctrlPressed) {
                /* enter(buffer, &bm, &wm, minibuffer, prompt, indentation, electric_indent_mode, sw, sh, &nh, arg); */
                eval_last_sexp(&bm);
            }
            break;
        case KEY_H:
            if (ctrlPressed && altPressed) {
                mark_scope(buffer);
            } else if (altPressed && wm.count <= 1) {
                split_window_right(&wm, font, sw, sw);
            }
            break;
            
        case KEY_K:
            if (altPressed && ctrlPressed) {
                kill_sexp(buffer, &kr, 1);
            } else if (altPressed && wm.count <= 1) {
                split_window_below(&wm, font, sw, sw);
            } else if (altPressed && shiftPressed) {
                swap_window(&wm, -1);
            } else if (altPressed) {
                other_window(&wm, -1);
            } else if (ctrlPressed) {
                if (buffer->region.active) {
                    kill_region(buffer, &kr);
                } else {
                    kill_line(buffer, &kr);
                }
            }
            break;
            case KEY_LEFT_BRACKET:
            if (altPressed) {
                nextBuffer(&bm);                
            }
            break;
            case KEY_RIGHT_BRACKET:
            if (altPressed) {
                previousBuffer(&bm);                
            }
            break;
        }
        // TODO This is so bad, maybe move it after all the cases without the if
        // just ctrl_x_pressed = false;
        if (ctrl_x_pressed && key != KEY_X && key != KEY_F && key != KEY_O && key != KEY_S) {
            ctrl_x_pressed = false;
        }
        
        updateScroll(win); // handle the scroll after all possbile cursor movements
    }

    fill_scopes(buffer, &buffer->scopes);
    
    updateRegion(buffer, buffer->point);
    updateSegments(&win->modeline, win->buffer);
    
    if (blink_cursor_mode) {
        blinkCount = 0;
        lastBlinkTime = getTime();
        cursorVisible = true;
    }

    // Update syntax for all key presses
    if (!isCurrentBuffer(&bm, "minibuffer") && buffer->tree != NULL) {
        TSInputEdit edit = createInputEdit(buffer, 0, buffer->size, buffer->size);
        updateSyntaxIncremental(buffer, &edit);
    }

    // Reset eatchar after key is processed
    if (action == GLFW_RELEASE) {
        eatchar = false;
    }

}

void textCallback(unsigned int codepoint) {
    if (eatchar) return;

    Window *win = wm.activeWindow;
    Buffer *buffer = win->buffer;
    Buffer *prompt = getBuffer(&bm, "prompt");
    Buffer *minibuffer = getBuffer(&bm, "minibuffer");
    Buffer *argBuffer = getBuffer(&bm, "arg");
    int arg = getGlobalArg(argBuffer);

    if (buffer->region.active && codepoint == 'e') {
        eval_region(&bm);
        buffer->region.active = false;
        return;
    }

    ctrl_x_pressed = false;

    if (buffer != NULL) {
        if (!isearch.searching) {
            buffer->region.active = false;
        }

        if (isearch.searching) {
            if (isprint(codepoint)) {
                insertChar(minibuffer, (char)codepoint);
                if (strcmp(prompt->content, "I-search backward: ") == 0) {
                    isearch_backward(buffer, minibuffer, false);
                } else {
                    isearch_forward(buffer, &bm, minibuffer, false);
                }
            }
        } else {
            size_t old_size = buffer->size;
            size_t insert_position = buffer->point;
            size_t original_point = buffer->point;

            if ((codepoint == ')' || codepoint == ']' || codepoint == '}' ||
                 codepoint == '>' || codepoint == '\'' || codepoint == '\"') &&
                buffer->point < buffer->size &&
                buffer->content[buffer->point] == codepoint) {
                right_char(buffer, false, &bm, arg);
            } else {
                if (isCurrentBuffer(&bm, "minibuffer")) {
                    insertChar(minibuffer, codepoint);
                } else {
                    if (electric_pair_mode) {
                        switch (codepoint) {
                        case '(':
                            insertChar(buffer, '(');
                            insertChar(buffer, ')');
                            buffer->point--;
                            break;
                        case '[':
                            insertChar(buffer, '[');
                            insertChar(buffer, ']');
                            buffer->point--;
                            break;
                        case '{':
                            insertChar(buffer, '{');
                            insertChar(buffer, '}');
                            buffer->point--;
                            break;
                        case '<':
                            // Check for #include context
                            size_t lineStart = buffer->point;
                            while (lineStart > 0 && buffer->content[lineStart - 1] != '\n') {
                                lineStart--;
                            }
                            if (buffer->point - lineStart >= 8 &&
                                strncmp(buffer->content + lineStart, "#include", 8) == 0) {
                                insertChar(buffer, '<');
                                insertChar(buffer, '>');
                                buffer->point--;
                            } else {
                                insertChar(buffer, '<');
                            }
                            break;
                        case '\'':
                            insertChar(buffer, '\'');
                            if (!(buffer->point > 1 &&
                                  buffer->content[buffer->point - 2] == '\'')) {
                                insertChar(buffer, '\'');
                                buffer->point--;
                            }
                            break;
                        case '\"':
                            insertChar(buffer, '\"');
                            if (!(buffer->point > 1 &&
                                  buffer->content[buffer->point - 2] == '\"')) {
                                insertChar(buffer, '\"');
                                buffer->point--;
                            }
                            break;
                        default:
                            insertChar(buffer, codepoint);
                        }
                    } else {
                        insertChar(buffer, codepoint);
                    }

                    // Update syntax and scopes only once after insertion
                    if (buffer->tree != NULL) {
                        size_t inserted_length = buffer->size - old_size;
                        TSInputEdit edit =
                            createInputEdit(buffer, original_point, original_point,
                                            original_point + inserted_length);
                        updateSyntaxIncremental(buffer, &edit);
                    }
                    fill_scopes(buffer, &buffer->scopes);
                }

                if (electric_indent_mode && (codepoint == '}' || codepoint == ';')) {
                    indent(buffer, indentation, &bm, arg);
                }
            }
        }

        updateScroll(win);
    }
}


// TODO use show_paren_delay
// TODO highlifght the 2 characters as well
void highlightMatchingBrackets(WindowManager *wm, Font *font, Color highlightColor) {
    if (!wm || !wm->activeWindow || !wm->activeWindow->buffer) return;
    if (!show_paren_mode) return;
    
    Buffer *buffer = wm->activeWindow->buffer;
    if (buffer->point > buffer->size) return;
    
    char currentChar = buffer->point < buffer->size ? buffer->content[buffer->point] : '\0';
    char prevChar = buffer->point > 0 ? buffer->content[buffer->point - 1] : '\0';
    char matchChar = '\0';
    int direction = 0;
    int searchPos = buffer->point;
    
    // Determine the direction to search based on the current or previous character
    if (buffer->point < buffer->size && strchr("({[", currentChar)) {
        matchChar = currentChar == '(' ? ')' :
        currentChar == '[' ? ']' : '}';
        direction = 1;
    } else if (prevChar == ')' || prevChar == ']' || prevChar == '}') {
        currentChar = prevChar;
        matchChar = currentChar == ')' ? '(' :
        currentChar == ']' ? '[' : '{';
        direction = -1;
        searchPos = buffer->point - 1;
    } else {
        return;  // Not on or immediately after a bracket
    }
    
    int depth = 1;
    searchPos += direction;
    
    // Search for the matching bracket
    while (searchPos >= 0 && searchPos < buffer->size) {
        char c = buffer->content[searchPos];
        if (c == currentChar) {
            depth++;
        } else if (c == matchChar) {
            depth--;
            if (depth == 0) {
                // Draw highlights at the positions of the matching brackets
                drawHighlight(wm, font, buffer->point - (direction == -1 ? 1 : 0), 1, highlightColor);
                drawHighlight(wm, font, searchPos, 1, highlightColor);
                return;
            }
        }
        searchPos += direction;
    }
}


void highlightAllOccurrences(WindowManager *wm, const char *searchText, Font *font, Color highlightColor) {
    if (!wm || !wm->activeWindow || !wm->activeWindow->buffer) return;
    Buffer *buffer = wm->activeWindow->buffer;
    if (!searchText || strlen(searchText) == 0 || !buffer->content) return;
    
    size_t searchLength = strlen(searchText);
    const char *current = buffer->content;
    size_t pos = 0;
    
    while ((current = strstr(current, searchText)) != NULL) {
        pos = current - buffer->content;
        drawHighlight(wm, wm->activeWindow->buffer->font, pos, searchLength, highlightColor);  // Updated to use WindowManager
        current += searchLength; // Move past the current match
        
        // Stop searching if the next search start is beyond buffer content
        if (current >= buffer->content + buffer->size) break;
    }
}

void drawHighlight(WindowManager *wm, Font *font, size_t startPos,
                   size_t length, Color highlightColor) {
    if (!wm || !wm->activeWindow || !wm->activeWindow->buffer)
        return;

    Buffer *buffer = wm->activeWindow->buffer;
    Window *activeWindow = wm->activeWindow;

    float x = fringe + activeWindow->x -
        activeWindow->scroll.x; // Adjust x by horizontal scroll
    float y = activeWindow->y + font->ascent - font->descent * 2;
    int lineCount = 0;

    // Calculate initial x offset and y position up to startPos
    for (size_t i = 0; i < startPos && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            x = fringe + activeWindow->x -
                activeWindow->scroll.x; // Reset x to the start of the line at each
            // new line, adjust for scroll
            y -= (font->ascent + font->descent); // Move up for each new line
        } else {
            x += getCharacterWidth(font, buffer->content[i]); // Accumulate width
        }
    }

    // Calculate the width of the highlighted area
    float highlightWidth = 0;
    for (size_t i = startPos; i < startPos + length && i < buffer->size; i++) {
        if (buffer->content[i] == '\n')
            break; // Stop if newline is encountered within highlight
        highlightWidth += getCharacterWidth(font, buffer->content[i]);
    }

    // Adjust y to be the lower left corner of the line to draw the highlight
    y -= font->ascent;

    // Adjust y for vertical scrolling
    y += activeWindow->scroll.y;

    // Handle minimap mode and padding
    if (minimap_mode) {
        float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0;
        float maxWidth = activeWindow->width - minimap_width -
            (x - (fringe + activeWindow->x - activeWindow->scroll.x)) -
            fringe - minimap_padding;
        if (highlightWidth > maxWidth)
            highlightWidth = maxWidth;
    }

    // Define the position and size of the highlight rectangle
    Vec2f position = {x, y};
    Vec2f size = {highlightWidth, font->ascent + font->descent};

    // Draw the highlight rectangle
    drawRectangle(position, size, highlightColor);
}

// TODO Optimize this, we shoudl call drawRectangle only 2 times
// once for all the lines fully selected, and another for the line where the cursor is
// not one rectangle per line.
void drawRegion(WindowManager *wm, Font *font, Color regionColor) {
    if (hide_region_mode) return;
    Buffer *buffer = wm->activeWindow->buffer;
    if (!buffer->region.active) return;
    Window *activeWindow = wm->activeWindow;
    
    size_t start = buffer->region.start;
    size_t end = buffer->region.end;
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }
    
    size_t currentLineStart = 0;
    float x = fringe + activeWindow->x - activeWindow->scroll.x;
    float y = activeWindow->y + font->ascent - font->descent * 2 + activeWindow->scroll.y;
    float initialY = y;
    
    // Iterate over each character in the buffer to find line starts and ends
    for (size_t i = 0; i <= buffer->size; i++) {
        if (buffer->content[i] == '\n' || i == buffer->size) {  // End of line or buffer
            if (i >= start && currentLineStart <= end) {  // Check if the line contains the region
                size_t lineStart = (currentLineStart > start) ? currentLineStart : start;
                size_t lineEnd = (i < end) ? i : end;
                size_t lineLength = lineEnd - lineStart;
                
                if (lineLength > 0 || (lineEnd == i && buffer->content[i] == '\n')) {
                    // Calculate highlight width from the start of the selection to the end of the line or selection end
                    float highlightWidth = 0;
                    float lineX = x; // Start at the beginning of the line
                    for (size_t j = currentLineStart; j < lineStart; j++) {
                        lineX += getCharacterWidth(font, buffer->content[j]); // Move to the start of the region
                    }
                    
                    for (size_t j = lineStart; j < lineEnd; j++) {
                        highlightWidth += getCharacterWidth(font, buffer->content[j]);
                    }
                    
                    if (buffer->content[i] == '\n' && lineEnd == i && (end != i || i == buffer->size)) {
                        // Extend highlight to the end of the window if the line is completely selected and ends with a newline
                        highlightWidth = activeWindow->width - (lineX - x);
                    }
                    if (minimap_mode) {
                        float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0;
                        float maxWidth = activeWindow->width - minimap_width - (lineX - x) - fringe - minimap_padding;
                        if (highlightWidth > maxWidth) highlightWidth = maxWidth;
                    }
                    
                    Vec2f position = {lineX, y - font->ascent};
                    Vec2f size = {highlightWidth, font->ascent + font->descent};
                    drawRectangle(position, size, regionColor);
                }
            }
            currentLineStart = i + 1;  // Move to the start of the next line
            y -= (font->ascent + font->descent);  // Move y to the next line
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
        
        // Update modeline segments with the correct buffer data
        updateSegments(&win->modeline, win->buffer);
        
        for (size_t i = 0; i < win->modeline.segments.count; i++) {
            Segment segment = win->modeline.segments.segment[i];
            float textY = modelineBaseY + (font->ascent - font->descent);
            
            for (size_t j = 0; j < strlen(segment.content); j++) {
                drawChar(font, segment.content[j], segmentX, textY, 1.0, 1.0, CT.text);
                segmentX += getCharacterWidth(font, segment.content[j]);
            }
            segmentX += spaceWidth;
        }
        flush();
    }
}

// TODO Optimize this we could draw a single rectangle for both the fringe and the
// background if true, not 2 rectangles for lines, 
void drawFringe(Window *win, Font *font) {
    useShader("simple");
    float lineHeight = font->ascent + font->descent;
    int fringeWidth = fringe;
    int rectangleWidth = fringe; // Width for fringe indicator
    int rectangleOffset = 1;

    // Calculate max highlight width considering minimap
    float maxHighlightWidth = win->width - fringe;
    if (minimap_mode) {
        float minimap_padding = minimap_padding_mode ? minimap_left_padding : 0;
        float maxWidthWithMinimap = win->width - minimap_width - fringe - minimap_padding;
        if (maxWidthWithMinimap < maxHighlightWidth) {
            maxHighlightWidth = maxWidthWithMinimap;
        }
    }

    // Draw the fringe background
    Vec2f fringePosition = {win->x, win->y};
    Vec2f fringeSize = {fringeWidth, win->height};
    /* drawRectangle(fringePosition, fringeSize, CT.fringe); */

    Color diff_hl_bg_color = (Color){CT.fringe.r, CT.fringe.g, CT.fringe.b, 0.1f};
    drawRectangle(fringePosition, fringeSize, diff_hl_bg_color);

    if (diff_hl_mode) {
        Diffs diffs = win->buffer->diffs;
        int visibleStartLine = (int)(win->scroll.y / lineHeight);
        int visibleEndLine = visibleStartLine + (int)(win->height / lineHeight) + 1;

        for (int i = 0; i < diffs.count; i++) {
            int diffLine = diffs.array[i].line;
            if (diffLine >= visibleStartLine && diffLine <= visibleEndLine) {
                // Calculate y position
                float y = win->y + win->scroll.y - (diffLine * lineHeight);
                
                // Position for both rectangles
                float rectY = y + font->ascent - font->descent;
                
                // Small fringe rectangle
                Vec2f diffPosition = {win->x + rectangleOffset, rectY};
                Vec2f diffSize = {rectangleWidth - rectangleOffset, lineHeight};
                
                Color diffColor;
                Color bufferColor;
                switch (diffs.array[i].type) {
                    case DIFF_ADDED:
                        diffColor = CT.diff_hl_insert;
                        bufferColor = CT.diff_hl_bg;
                        bufferColor.a = 0.2f; // More subtle for buffer
                        break;
                    case DIFF_CHANGED:
                        diffColor = CT.diff_hl_change;
                        bufferColor = CT.diff_hl_change_bg;
                        bufferColor.a = 0.2f;
                        break;
                    default:
                        continue;
                }
                
                // Draw fringe indicator
                drawRectangle(diffPosition, diffSize, diffColor);
                
                // Draw buffer background highlight only if diff_hl_bg is true
                if (diff_hl_bg) {
                    Vec2f bufferPosition = {win->x + fringe, rectY};
                    Vec2f bufferSize = {maxHighlightWidth, lineHeight};
                    drawRectangle(bufferPosition, bufferSize, bufferColor);
                }
            }
        }
    }
    flush();
}



#include <math.h>

// REGION-FG && LINE LERP
void drawBuffer(Window *win, Buffer *buffer, bool cursorVisible, bool colorPoint) {
    Font *font = buffer->font;
    const char *text = buffer->content;

    float x = fringe + win->x - win->scroll.x;
    float y = win->y + win->scroll.y;
    size_t index = 0;
    size_t charIndex = 0;
    Color currentColor = CT.text;

    // Track current line and diff information
    int currentLine = 1;
    int diffIndex = 0;
    DiffType currentDiffType = DIFF_NONE;

    // Check for diffs in the first line
    while (diffIndex < buffer->diffs.count &&
           buffer->diffs.array[diffIndex].line < currentLine) {
        diffIndex++;
    }
    if (diffIndex < buffer->diffs.count &&
        buffer->diffs.array[diffIndex].line == currentLine) {
        currentDiffType = buffer->diffs.array[diffIndex].type;
        diffIndex++;
    }

    useShader(buffer_shader);

    while (text[charIndex] != '\0') {
        if (text[charIndex] == '\n') {
            x = fringe + win->x - win->scroll.x;
            y -= (font->ascent + font->descent);
            charIndex++;
            currentLine++;

            // Update diff status for new line
            currentDiffType = DIFF_NONE;
            while (diffIndex < buffer->diffs.count &&
                   buffer->diffs.array[diffIndex].line < currentLine) {
                diffIndex++;
            }
            if (diffIndex < buffer->diffs.count &&
                buffer->diffs.array[diffIndex].line == currentLine) {
                currentDiffType = buffer->diffs.array[diffIndex].type;
                diffIndex++;
            }
            continue;
        }

        // Determine character color
        if (cursorVisible && colorPoint && charIndex == buffer->point) {
            currentColor =
                (buffer->region.active && buffer->point == buffer->region.start)
                ? CT.region
                : CT.bg;
        } else if (buffer->region.active && charIndex >= buffer->region.start &&
                   charIndex < buffer->region.end && region_fg_mode) {
            currentColor = CT.region_fg; // Apply region foreground color
        } else if (diff_hl_text && currentDiffType != DIFF_NONE) {
            switch (currentDiffType) {
            case DIFF_ADDED:
                currentColor = CT.diff_hl_insert;
                break;
            case DIFF_CHANGED:
                currentColor = CT.diff_hl_change;
                break;
            default:
                currentColor = CT.text;
                break;
            }
        } else if (index < buffer->syntaxArray.used &&
                   charIndex >= buffer->syntaxArray.items[index].start &&
                   charIndex < buffer->syntaxArray.items[index].end) {
            currentColor = *buffer->syntaxArray.items[index].color;
        } else {
            currentColor = CT.text;
        }

        // Calculate Y offset for animation
        float drawY = y;
        if (lerp_line_mode && buffer->animatedLineNumber != -1 &&
            currentLine > buffer->animatedLineNumber) {
            double elapsed = getTime() - buffer->animationStartTime;
            float duration = lerp_line_duration; // Animation duration in seconds
            if (elapsed < duration) {
                float t = (float)(elapsed / duration);
                t = 1.0f - powf(1.0f - t, 3.0f); // Ease-out cubic
                float lineHeight = font->ascent + font->descent;
                drawY -= lineHeight * t; // Animate downward
            }
        }

        drawChar(font, text[charIndex], x, drawY, 1.0, 1.0, currentColor);

        x += getCharacterWidth(font, text[charIndex]);
        charIndex++;

        if (index < buffer->syntaxArray.used &&
            charIndex == buffer->syntaxArray.items[index].end) {
            index++;
        }
    }

    flush();
}


// NOTE for all buffers, in case we ever want to make things slow
void updateBufferAnimations(BufferManager *bm) {
    if (!lerp_line_mode) return; // Skip if animation is disabled

    double currentTime = getTime();
    for (int i = 0; i < bm->count; ++i) {
        Buffer *buffer = bm->buffers[i];
        if (buffer->animatedLineNumber != -1) { // Check if animation is active
            double elapsed = currentTime - buffer->animationStartTime;
            if (elapsed >= lerp_line_duration) {
                // Animation is complete
                buffer->animatedLineNumber = -1; // Reset animation state
            }
        }
    }
}


// REGION-FG
/* void drawBuffer(Window *win, Buffer *buffer, bool cursorVisible, bool colorPoint) { */
/*     Font *font = buffer->font; */
/*     const char *text = buffer->content; */

/*     float x = fringe + win->x - win->scroll.x; */
/*     float y = win->y + win->scroll.y; */
/*     size_t index = 0; */
/*     size_t charIndex = 0; */
/*     Color currentColor = CT.text; */

/*     // Track current line and diff information */
/*     int currentLine = 1; */
/*     int diffIndex = 0; */
/*     DiffType currentDiffType = DIFF_NONE; */

/*     // Check for diffs in the first line */
/*     while (diffIndex < buffer->diffs.count && */
/*            buffer->diffs.array[diffIndex].line < currentLine) { */
/*         diffIndex++; */
/*     } */
/*     if (diffIndex < buffer->diffs.count && */
/*         buffer->diffs.array[diffIndex].line == currentLine) { */
/*         currentDiffType = buffer->diffs.array[diffIndex].type; */
/*         diffIndex++; */
/*     } */

/*     useShader(buffer_shader); */

/*     while (text[charIndex] != '\0') { */
/*         if (text[charIndex] == '\n') { */
/*             x = fringe + win->x - win->scroll.x; */
/*             y -= (font->ascent + font->descent); */
/*             charIndex++; */
/*             currentLine++; */

/*             // Update diff status for new line */
/*             currentDiffType = DIFF_NONE; */
/*             while (diffIndex < buffer->diffs.count && */
/*                    buffer->diffs.array[diffIndex].line < currentLine) { */
/*                 diffIndex++; */
/*             } */
/*             if (diffIndex < buffer->diffs.count && */
/*                 buffer->diffs.array[diffIndex].line == currentLine) { */
/*                 currentDiffType = buffer->diffs.array[diffIndex].type; */
/*                 diffIndex++; */
/*             } */
/*             continue; */
/*         } */

/*         // Determine character color */
/*         if (cursorVisible && colorPoint && charIndex == buffer->point) { */
/*             currentColor = */
/*                 (buffer->region.active && buffer->point == buffer->region.start) */
/*                 ? CT.region */
/*                 : CT.bg; */
/*         } else if (buffer->region.active && charIndex >= buffer->region.start && charIndex < buffer->region.end && region_fg_mode) { */
/*             currentColor = CT.region_fg; // Apply region foreground color */
/*         } else if (diff_hl_text && currentDiffType != DIFF_NONE) { */
/*             switch (currentDiffType) { */
/*             case DIFF_ADDED: */
/*                 currentColor = CT.diff_hl_insert; */
/*                 break; */
/*             case DIFF_CHANGED: */
/*                 currentColor = CT.diff_hl_change; */
/*                 break; */
/*             default: */
/*                 currentColor = CT.text; */
/*                 break; */
/*             } */
/*         } else if (index < buffer->syntaxArray.used && */
/*                    charIndex >= buffer->syntaxArray.items[index].start && */
/*                    charIndex < buffer->syntaxArray.items[index].end) { */
/*             currentColor = *buffer->syntaxArray.items[index].color; */
/*         } else { */
/*             currentColor = CT.text; */
/*         } */

/*         drawChar(font, text[charIndex], x, y, 1.0, 1.0, currentColor); */

/*         x += getCharacterWidth(font, text[charIndex]); */
/*         charIndex++; */

/*         if (index < buffer->syntaxArray.used && */
/*             charIndex == buffer->syntaxArray.items[index].end) { */
/*             index++; */
/*         } */
/*     } */

/*     flush(); */
/* } */


// NOTE Original
/* void drawBuffer(Window *win, Buffer *buffer, bool cursorVisible, bool colorPoint) { */
/*     Font *font = buffer->font; */
/*     const char *text = buffer->content; */

/*     float x = fringe + win->x - win->scroll.x; */
/*     float y = win->y + win->scroll.y; */
/*     size_t index = 0; */
/*     size_t charIndex = 0; */
/*     Color currentColor = CT.text; */

/*     // Track current line and diff information */
/*     int currentLine = 1; */
/*     int diffIndex = 0; */
/*     DiffType currentDiffType = DIFF_NONE; */

/*     // Check for diffs in the first line */
/*     while (diffIndex < buffer->diffs.count && */
/*            buffer->diffs.array[diffIndex].line < currentLine) { */
/*         diffIndex++; */
/*     } */
/*     if (diffIndex < buffer->diffs.count && */
/*         buffer->diffs.array[diffIndex].line == currentLine) { */
/*         currentDiffType = buffer->diffs.array[diffIndex].type; */
/*         diffIndex++; */
/*     } */

/*     useShader(buffer_shader); */

/*     while (text[charIndex] != '\0') { */
/*         if (text[charIndex] == '\n') { */
/*             x = fringe + win->x - win->scroll.x; */
/*             y -= (font->ascent + font->descent); */
/*             charIndex++; */
/*             currentLine++; */

/*             // Update diff status for new line */
/*             currentDiffType = DIFF_NONE; */
/*             while (diffIndex < buffer->diffs.count && */
/*                    buffer->diffs.array[diffIndex].line < currentLine) { */
/*                 diffIndex++; */
/*             } */
/*             if (diffIndex < buffer->diffs.count && */
/*                 buffer->diffs.array[diffIndex].line == currentLine) { */
/*                 currentDiffType = buffer->diffs.array[diffIndex].type; */
/*                 diffIndex++; */
/*             } */
/*             continue; */
/*         } */

/*         // Determine character color */
/*         if (cursorVisible && colorPoint && charIndex == buffer->point) { */
/*             currentColor = */
/*                 (buffer->region.active && buffer->point == buffer->region.start) */
/*                 ? CT.region */
/*                 : CT.bg; */
/*         } else if (diff_hl_text && currentDiffType != DIFF_NONE) { */
/*             switch (currentDiffType) { */
/*             case DIFF_ADDED: */
/*                 currentColor = CT.diff_hl_insert; */
/*                 break; */
/*             case DIFF_CHANGED: */
/*                 currentColor = CT.diff_hl_change; */
/*                 break; */
/*             default: */
/*                 currentColor = CT.text; */
/*                 break; */
/*             } */
/*         } else if (index < buffer->syntaxArray.used && */
/*                    charIndex >= buffer->syntaxArray.items[index].start && */
/*                    charIndex < buffer->syntaxArray.items[index].end) { */
/*             currentColor = *buffer->syntaxArray.items[index].color; */
/*         } else { */
/*             currentColor = CT.text; */
/*         } */

/*         drawChar(font, text[charIndex], x, y, 1.0, 1.0, currentColor); */

/*         x += getCharacterWidth(font, text[charIndex]); */
/*         charIndex++; */

/*         if (index < buffer->syntaxArray.used && */
/*             charIndex == buffer->syntaxArray.items[index].end) { */
/*             index++; */
/*         } */
/*     } */

/*     flush(); */
/* } */


#include <regex.h>

void highlightHexColors(WindowManager *wm, Font *font, Buffer *buffer, bool rm) {
    if (!rm || !wm || !wm->activeWindow || !buffer) return;
    useShader("simple");
    const char *pattern = "#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})\\b";
    regex_t regex;
    regmatch_t matches[2]; // We expect one full match and one subgroup
    
    if (regcomp(&regex, pattern, REG_EXTENDED)) {
        fprintf(stderr, "Failed to compile regex.\n");
        return;
    }
    
    char *text = buffer->content;
    size_t offset = 0;
    
    while (regexec(&regex, text + offset, 2, matches, 0) == 0) {
        size_t match_start = offset + matches[0].rm_so;
        size_t match_end = offset + matches[0].rm_eo;
        
        char matchedString[8]; // Enough to hold the full pattern plus null terminator
        snprintf(matchedString, sizeof(matchedString), "%.*s", (int)(match_end - match_start), text + match_start);
        
        Color highlightColor = hexToColor(matchedString);
        drawHighlight(wm, font, match_start, match_end - match_start, highlightColor);
        
        offset = match_end; // Move past this match
    }
    flush();
    regfree(&regex);
}



int getGlobalArg(Buffer *argBuffer) {
    if (argBuffer->size == 0 || argBuffer->content[0] == '\0') {
        return 1;
    } else {
        char *endptr;
        int result = (int)strtol(argBuffer->content, &endptr, 10);
        if (*endptr != '\0') {
            // Handle case where non-numeric characters are present
            printf("Non-numeric input in argument buffer. Ignoring non-numeric part.\n");
            return result;
        }
        return result;
    }
}

#include <math.h> // Scrolling is hard

float lerp(float a, float b, float t) {
    return a + t * (b - a);
}


// NOTE It thinks it should scroll until the lerp animation ends
// TODO DIRECTION arg maybe you really want to force it to scroll up or down
void updateScroll(Window *window) {
    Buffer *buffer = window->buffer;
    Font *font = buffer->font;
    Buffer *minibuffer = getBuffer(&bm, "minibuffer");

    float lineHeight = font->ascent + font->descent;
    
    // Calculate cursor position
    int cursorLine = 0;
    float cursorX = 0;
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            cursorLine++;
            cursorX = 0;
        } else {
            cursorX += getCharacterWidth(font, buffer->content[i]);
        }
    }
    float cursorY = cursorLine * lineHeight;
    float cursorWidth = getCharacterWidth(font, buffer->content[buffer->point]);
    float cursorRightEdge = cursorX + cursorWidth;
    
    float viewTop = window->scroll.y;
    float viewBottom = window->scroll.y + window->height - window->modeline.height;
    float viewLeft = window->scroll.x;
    float viewRight = window->scroll.x + window->width;

    // Vertical scrolling logic
    if (cursorY < viewTop || cursorY +  minibuffer->font->ascent + minibuffer->font->descent /* lineHeight */ > viewBottom) {
        float newScrollY = cursorY - window->height / 2 + lineHeight / 2;
        newScrollY = fmax(0, round(newScrollY / lineHeight) * lineHeight);
        newScrollY = fmin(newScrollY, buffer->size * lineHeight - window->height);
        
        window->targetScrollY = newScrollY;
        window->isScrolling = true;
    }
    
    // Horizontal scrolling logic (unchanged)
    if (cursorX < viewLeft || cursorRightEdge > viewRight) {
        if (auto_text_scale_mode && buffer->scale.index > 8) {
            text_scale_decrease(&bm, fontPath, &wm, sh, 1);
        } else {
            float newScrollX = cursorX - window->width / 2;
            newScrollX = fmax(0, fmin(newScrollX, buffer->size * lineHeight - window->width));
            window->scroll.x = newScrollX;
        }
    }
}

void updateScrollLerp(Window *window) {
    if (window->isScrolling) {
        if (scroll_lerp_mode) {
            window->scroll.y = lerp(window->scroll.y, window->targetScrollY, scroll_lerp_speed);
            if (fabs(window->scroll.y - window->targetScrollY) < 0.1) {
                window->scroll.y = window->targetScrollY;
                window->isScrolling = false;
            }
        } else {
            window->scroll.y = window->targetScrollY;
            window->isScrolling = false;
        }
    }
}

void updateCursorAfterScroll(Window *win) {
    float lineHeight = win->buffer->font->ascent + win->buffer->font->descent;
    float cursorY = 0;
    int cursorLine = 0;
    for (size_t i = 0; i < win->buffer->point; i++) {
        if (win->buffer->content[i] == '\n') {
            cursorLine++;
        }
    }
    cursorY = cursorLine * lineHeight;
    float viewTop = win->scroll.y;
    float viewBottom =
        win->scroll.y + win->height - lineHeight - (win->modeline.height) * 2;
    if (cursorY < viewTop || cursorY > viewBottom) {
        size_t newPoint = 0;
        cursorLine = 0;
        for (size_t i = 0; i < win->buffer->size; i++) {
            if (win->buffer->content[i] == '\n') {
                cursorLine++;
                float lineTop = cursorLine * lineHeight;
                if ((cursorY < viewTop && lineTop >= viewTop) ||
                    (cursorY > viewBottom && lineTop >= viewBottom)) {
                    newPoint = i + 1;
                    break;
                }
            }
        }
        win->buffer->region.active = false;
        win->buffer->point = newPoint;
    }
}


void updateMouseWheelLerp(Window *window) {
    if (window->isMouseWheelScrolling && mouse_wheel_lerp_mode) {
        window->scroll.y = lerp(window->scroll.y, window->targetScrollY,
                                mouse_wheel_lerp_speed);
        if (fabs(window->scroll.y - window->targetScrollY) < 0.1) {
            window->scroll.y = window->targetScrollY;
            window->isMouseWheelScrolling = false;
        }
        updateCursorAfterScroll(window);
    }
}


void scrollCallback(double xOffset, double yOffset) {
  Window *win = wm.head;

  yOffset = -yOffset; // Invert scrolling direction

  while (win != NULL) {
    if (isKeyDown(KEY_LEFT_CONTROL)) {
      int arg = (yOffset > 0) ? -1 : 1;
      if (arg > 0) {
        text_scale_increase(&bm, fontPath, &wm, sh, arg);
      } else {
        text_scale_decrease(&bm, fontPath, &wm, sh, -arg);
      }
      updateScroll(win);
      if (blink_cursor_mode) {
        blinkCount = 0;
        lastBlinkTime = getTime();
        cursorVisible = true;
      }
    } else if (mouseX >= win->x && mouseX <= win->x + win->width &&
               mouseY >= win->y - win->height && mouseY <= win->y) {

      float lineHeight = win->buffer->font->ascent + win->buffer->font->descent;
      float scrollAmount = yOffset * mouse_wheel_scroll_amount * lineHeight;

      // Calculate the maximum allowed scroll position
      float maxScroll = win->buffer->size * lineHeight - win->height;

      if (mouse_wheel_lerp_mode) {
        // Accumulate scroll amount and clamp targetScrollY
        win->targetScrollY += scrollAmount;

        // Clamp targetScrollY to prevent scrolling past the top or bottom
        win->targetScrollY = fmax(0, fmin(win->targetScrollY, maxScroll));

        win->isMouseWheelScrolling = true;
      } else {
        // Non-lerp mode: directly update scroll.y and clamp it
        win->scroll.y += scrollAmount;

        // Clamp scroll.y to prevent scrolling past the top or bottom
        win->scroll.y = fmax(0, fmin(win->scroll.y, maxScroll));
      }

      // Handle cursor visibility and position
      float cursorY = 0;
      int cursorLine = 0;
      for (size_t i = 0; i < win->buffer->point; i++) {
        if (win->buffer->content[i] == '\n') {
          cursorLine++;
        }
      }
      cursorY = cursorLine * lineHeight;

      float viewTop = win->scroll.y;
      float viewBottom =
          win->scroll.y + win->height - lineHeight - (win->modeline.height) * 2;

      if (cursorY < viewTop) { // Scrolling up
        // Move point to the first visible line (top-most visible line)
        size_t newPoint = 0;
        cursorLine = 0;

        for (size_t i = 0; i < win->buffer->size; i++) {
          if (win->buffer->content[i] == '\n') {
            cursorLine++;
            float lineTop = cursorLine * lineHeight;
            if (lineTop >= viewTop) {
              newPoint = i + 1; // Set point to the beginning of the first fully
                                // visible line
              break;
            }
          }
        }
        win->buffer->region.active = false;
        win->buffer->point = newPoint;
      } else if (cursorY > viewBottom) { // Scrolling down
        // Move point to the last visible line (bottom-most visible line)
        size_t newPoint = 0;
        cursorLine = 0;

        for (size_t i = 0; i < win->buffer->size; i++) {
          if (win->buffer->content[i] == '\n') {
            cursorLine++;
            float lineTop = cursorLine * lineHeight;
            if (lineTop >= viewBottom) {
              newPoint = i + 1; // Set point to the beginning of the last fully
                                // visible line
              break;
            }
          }
        }
        win->buffer->region.active = false;
        win->buffer->point = newPoint;
      }

      // Refresh cursor visibility
      if (blink_cursor_mode) {
        blinkCount = 0;
        lastBlinkTime = getTime();
        cursorVisible = true;
      }

      break;
    }
    win = win->next;
  }
}

void updateCursorPosition(Window *win, size_t lineStart, size_t lineEnd) {
    float cursorX = win->x;
    if (fringe_mode) cursorX += fringe;

    
    for (size_t j = lineStart; j < lineEnd; j++) {
        float charWidth = getCharacterWidth(win->buffer->font, win->buffer->content[j]);
        cursorX += charWidth;
        if (cursorX > mouseX) {
            win->buffer->point = j;
            return;
        }
    }
    win->buffer->point = lineEnd;
}

void updateCursorPositionFromMouse(Window *win, double mouseX, double mouseY) {
    Font *font = win->buffer->font;
    float lineHeight = font->ascent + font->descent;
    float cursorY = win->y + win->scroll.y - lineHeight + ((font->ascent - font->descent) * 3);
    size_t lineStart = 0;
    
    for (size_t i = 0; i <= win->buffer->size; i++) {
        if (win->buffer->content[i] == '\n' || i == win->buffer->size) {
            if (mouseY > cursorY - lineHeight && mouseY <= cursorY) {
                updateCursorPosition(win, lineStart, i);
                break;
            }
            cursorY -= lineHeight;
            lineStart = i + 1;
        }
    }
}

void cursorPosCallback(double xpos, double ypos) {
    mouseX = xpos;
    mouseY = sh - ypos;  // NOTE invert the y
    
    if (getMouseButton(GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS) {
        double dx = fabs(mouseX - initialMouseX);
        double dy = fabs(mouseY - initialMouseY);
        
        if (!dragging && (dx > dragThreshold || dy > dragThreshold)) {
            dragging = true;
        }
        
        if (dragging) {
            if (blink_cursor_mode) {
                blinkCount = 0;
                lastBlinkTime = getTime();
                cursorVisible = true;
            }
            
            Window *win = wm.activeWindow;
            updateRegion(win->buffer, win->buffer->point);
            updateCursorPositionFromMouse(win, mouseX, mouseY);
        }
    }
}

// TODO Implement buttons and then handle them in here
// TODO its actually off by some pixels vertically idk why
// TODO i can't select the first line if i click on the top half of the fist line
void mouseButtonCallback(int button, int action, int mods) {
    Window *win = wm.head;
    
    if (button == GLFW_MOUSE_BUTTON_LEFT) {
        if (action == GLFW_PRESS) {
            initialMouseX = mouseX;
            initialMouseY = mouseY;
            dragging = false;  // Reset dragging status
            
            // Find the window based on mouse coordinates and make it active
            while (win) {
                if (mouseX >= win->x && mouseX <= win->x + win->width &&
                    mouseY >= win->y - win->height && mouseY <= win->y) {
                    wm.activeWindow = win; // Set this window as the active window
                    
                    // Reset the region before setting a new one
                    win->buffer->region.active = false;  // Reset active state of the region
                    
                    Font *font = win->buffer->font;
                    float lineHeight = font->ascent + font->descent;
                    float offset = ((font->ascent - font->descent) * 3);
                    float cursorY = win->y + win->scroll.y - lineHeight + offset;
                    
                    for (size_t i = 0, lineStart = 0; i <= win->buffer->size; i++) {
                        if (win->buffer->content[i] == '\n' || i == win->buffer->size) {
                            if (mouseY > cursorY - lineHeight && mouseY <= cursorY) {
                                updateCursorPosition(win, lineStart, i);
                                win->buffer->region.mark = win->buffer->point;  // Set mark at new start point
                                win->buffer->region.start = win->buffer->point; // Start new region
                                win->buffer->region.end = win->buffer->point;   // End also starts at the same point
                                win->buffer->region.active = true;              // Activate region at press
                                break;
                            }
                            cursorY -= lineHeight;
                            lineStart = i + 1;
                        }
                    }
                    
                    if (blink_cursor_mode) {
                        blinkCount = 0;
                        lastBlinkTime = getTime();
                        cursorVisible = true;
                    }
                    
                    break;
                }
                win = win->next;
            }
        }
        else if (action == GLFW_RELEASE) {
            if (dragging) {
                win = wm.activeWindow;
                win->buffer->region.end = win->buffer->point;
                win->buffer->region.active = true;
                dragging = false;
                updateRegion(win->buffer, win->buffer->point);
            } else {
                win = wm.activeWindow;
                win->buffer->region.active = false;
                win->buffer->region.start = 0;
                win->buffer->region.end = 0;
            }
        }
    }
}



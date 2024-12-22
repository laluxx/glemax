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
#include <ctype.h>
#include <input.h>
#include <libguile.h>
#include <lume.h>
#include <stdio.h>
#include <stdlib.h>

// [ ] TODO One thing that we really should do is to
// make the minibuffer just a window and not a special
// case to handle in all the functions

// FIXME many functions calculate the same exact data
// so the same stuff is recalculated like 6 or more times per frame
// i hate it, but i hate refactoring more

// TODO COOL an option to automatically make the font smaller if we try to type outisde
// of the window x cordinate

// TODO FIX backward_list when called on the last character of the buffer

// TODO unhardcode the scroll it shoudl use the modelineHeight + minibufferHeight

// TODO we should add cursor struct to each window
// TODO we should add scrollbar struct to each window
// TODO we should add 2 fringes to each window
// TODO the point should be per window not per buffer

// FIXME most functions that take the "Font *font" shoudl take only the wm now
// TODO dabbrev-completion
// TODO rainbow-delimiters-mode
// TODO revert-buffer-mode
// TODO wdired
// TODO iedit
// TODO draw hollow cursor on non active widnows

// TODO nextBuffer() and previousBuffer() should update the wm->buffer also switchToBuffer
// Do this when we implement the minibuffer as a window
// NOTE I-searrch for "if (isCurrentBuffer(&bm, "minibuffer")"

// TODO color unmatched characters in isearch and fix is behaviour like pressing backspaces etc..
// TODO unhardcode the keybinds
// TODO check if the lastmatchindex changes and stop search (like for moving)
// TODO M-x
// TODO undo system

// TODO ) inside () shoudl jumpt to the closing one not move right once
// FIXME use setBufferContent() to set the prompt aswell

// TODO IMPORTANT Don't fetch for the same buffer multiple times per frame,
// do it only once and pass it arround. (we did it by using the wm ?)

CompletionEngine ce = {0};
BufferManager bm = {0};
KillRing kr = {0};
WindowManager wm = {0};
NamedHistories nh = {0};
/* double mouseX; */
/* double mouseY; */
bool dragging = false;
double initialMouseX = 0;
double initialMouseY = 0;
double dragThreshold = 5; // Threshold in pixels to start dragging


bool electric_pair_mode = true; // TODO Wrap selection for () [] {} '' ""
bool blink_cursor_mode = true;
float blink_cursor_delay = 0.5; // Seconds of idle time before the first blink of the cursor.
float blink_cursor_interval = 0.5; // Lenght of cursor blink interval in seconds.
int blink_cursor_blinks = 10; // How many times to blink before stopping.
int indentation = 4;
bool show_paren_mode = true;
float show_paren_delay = 0.125; // TODO Time in seconds to delay before showing a matching paren.
int kill_ring_max = 120; // Maximum length of kill ring before oldest elements are thrown away.
bool draw_region_on_empty_lines = true;
bool electric_indent_mode = true;
bool rainbow_mode = true;
bool crystal_cursor_mode = true; // Make the cursor crystal clear
float mouse_wheel_scroll_amount = 2; // TODO make it a vec2f for vertical and horizontal scrolling


void drawMiniCursor(Buffer *buffer, Font *font, float x, float y, Color color);
void drawCursor(Buffer *buffer, Window *win, Color defaultColor);
void drawHighlight(WindowManager *wm, Font *font, size_t startPos, size_t length, Color highlightColor);
void highlightMatchingBrackets(WindowManager *wm, Font *font, Color highlightColor);
void highlightAllOccurrences(WindowManager *wm, const char *searchText, Font *font, Color highlightColor);
void drawRegion(WindowManager *wm, Font *font, Color regionColor);
void drawModelines(WindowManager *wm, Font *font, float minibufferHeight, Color color);
void drawBuffer(Window *win, Buffer *buffer, bool cursorVisible, bool colorPoint);
void highlightHexColors(WindowManager *wm, Font *font, Buffer *buffer, bool rm);
int getGlobalArg(Buffer *argBuffer);
void updateScroll(Window *window);
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer);
void scrollCallback(double xOffset, double yOffset);
void cursorPosCallback(double xpos, double ypos);
void mouseButtonCallback(int button, int action, int mods);

void keyCallback(int key, int action, int mods);
void textCallback(unsigned int codepoint);
void insertUnicodeCharacter(Buffer * buffer, unsigned int codepoint, int arg);
int encodeUTF8(char *out, unsigned int codepoint);


static double lastBlinkTime = 0.0;  // Last time the cursor state changed
static bool cursorVisible = true;  // Initial state of the cursor visibility
static int blinkCount = 0;  // Counter for number of blinks


void drawCursor(Buffer *buffer, Window *win, Color defaultColor) {
    float cursorX = win->x - win->scroll.x;  // Start position adjusted for horizontal scroll
    float cursorY = win->y;
    int lineCount = 0;

    // Calculate the cursor's x offset within the content of the buffer
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;  // Increment line count at each newline
            cursorX = win->x - win->scroll.x;  // Reset cursor x to the start of the window on new lines, adjusted for horizontal scroll
        } else {
            cursorX += getCharacterWidth(buffer->font, buffer->content[i]);  // Move cursor right by character width
        }
    }

    // Determine cursor width, handle newline or end of buffer cases
    float cursorWidth = (buffer->point < buffer->size && buffer->content[buffer->point] != '\n') ?
        getCharacterWidth(buffer->font, buffer->content[buffer->point]) :
        getCharacterWidth(buffer->font, ' ');  // Use a standard width if at the end of a line or buffer

    // Compute cursor's y position based on the number of lines, subtract from initial y and add the scroll
    cursorY += win->scroll.y - lineCount * (buffer->font->ascent + buffer->font->descent) - (buffer->font->descent * 2);

    // Create a rectangle representing the cursor
    Vec2f cursorPosition = {cursorX, cursorY};
    Vec2f cursorSize = {cursorWidth, buffer->font->ascent + buffer->font->descent};

    // Determine the cursor color considering crystal_cursor_mode
    Color cursorColor = defaultColor;
    if (crystal_cursor_mode) {
        for (size_t i = 0; i < buffer->syntaxArray.used; i++) {
            if (buffer->point >= buffer->syntaxArray.items[i].start && buffer->point < buffer->syntaxArray.items[i].end) {
                cursorColor = buffer->syntaxArray.items[i].color;
                break;
            }
        }
    }

    // Handle cursor blinking logic
    if (blink_cursor_mode && blinkCount < blink_cursor_blinks) {
        double currentTime = getTime();
        if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) {
            cursorVisible = !cursorVisible;
            lastBlinkTime = currentTime;
            if (cursorVisible) {
                blinkCount++;
            }
        }

        if (cursorVisible) {
            drawRectangle(cursorPosition, cursorSize, cursorColor);  // Draw the cursor if visible
        }
    } else {
        drawRectangle(cursorPosition, cursorSize, cursorColor);  // Always draw cursor with syntax color if not blinking or crystal_cursor_mode is true
    }
}

// TODO once we support the minibuffer as a window remove this function
void drawMiniCursor(Buffer *buffer, Font *font, float x, float y, Color color) {
    int lineCount = 0;
    float cursorX = x;

    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            cursorX = x;
        } else {
            cursorX += getCharacterWidth(font, buffer->content[i]);
        }
    }

    float cursorWidth = buffer->point < buffer->size && buffer->content[buffer->point] != '\n'
        ? getCharacterWidth(font, buffer->content[buffer->point])
        : getCharacterWidth(font, ' ');

    Vec2f cursorPosition = {cursorX, y - lineCount * (font->ascent + font->descent) - font->descent};
    Vec2f cursorSize = {cursorWidth, font->ascent + font->descent};

    if (blink_cursor_mode && blinkCount < blink_cursor_blinks) {
        double currentTime = getTime();
        if (currentTime - lastBlinkTime >= (cursorVisible ? blink_cursor_interval : blink_cursor_delay)) {
            cursorVisible = !cursorVisible;
            lastBlinkTime = currentTime;
            if (cursorVisible) {
                blinkCount++;  // Increment only on visibility toggle to visible
            }
        }

        if (cursorVisible) {
            drawRectangle(cursorPosition, cursorSize, color);
        }
    } else {
        drawRectangle(cursorPosition, cursorSize, color);  // Always draw cursor if not blinking
    }
}


static void inner_main(void *closure, int argc, char **argv) {
    /* init_global_environment(); // Guile env initialization */


    // At program start
    init_scheme_environment();  // Initialize Guile
    init_glemax_bindings();    // Set up our module


    initThemes();

    (void)closure; // Unused parameters
    (void)argc;
    (void)argv;


    initWindow(sw, sh, "*scratch* - Glemax");
    registerTextCallback(textCallback);
    registerKeyCallback(keyCallback);
    registerScrollCallback(scrollCallback);
    registerCursorPosCallback(cursorPosCallback);
    registerMouseButtonCallback(mouseButtonCallback);

    font = loadFont(fontname, fontsize);

    initGlobalParser();
    initKillRing(&kr, kill_ring_max);
    initBufferManager(&bm);
    initCommands();
    newBuffer(&bm, &wm, "minibuffer", "~/", fontname, sw, sh);
    newBuffer(&bm, &wm, "prompt", "~/", fontname, sw, sh);
    newBuffer(&bm, &wm, "message", "~/", fontname, sw, sh);
    newBuffer(&bm, &wm, "arg", "~/", fontname, sw, sh);
    newBuffer(&bm, &wm, "*scratch*", "~/", fontname, sw, sh);
    bm.lastBuffer = getBuffer(&bm, "*scratch*");

    
    
    sw = getScreenWidth();  // NOTE Currently get scren dimentions
    sh = getScreenHeight(); // only once at startup

    initWindowManager(&wm, &bm, font, sw, sh);

    initSegments(&wm.activeWindow->modeline.segments);
    updateSegments(&wm.activeWindow->modeline, wm.activeWindow->buffer);
    /* updateWindows(&wm, font, sw, sh); */


    load_init_file();  // Load user config

    while (!windowShouldClose()) {
        sw = getScreenWidth();
        sh = getScreenHeight();

        /* updateWindows(&wm, font, sw, sh); */
        /* reloadShaders(); */
        Buffer *prompt = getBuffer(&bm, "prompt");
        Buffer *minibuffer = getBuffer(&bm, "minibuffer");
        Buffer *message = getBuffer(&bm, "message");

        Window *win = wm.head;
        Window *activeWindow = wm.activeWindow;
        Buffer *currentBuffer = activeWindow->buffer;

        beginDrawing();
        clearBackground(CT.bg);

        drawFPS(font, 400.0, 400.0, RED);

        float promptWidth = 0;
        for (size_t i = 0; i < strlen(prompt->content); i++) {
            promptWidth += getCharacterWidth(minibuffer->font, prompt->content[i]);
        }

        int lineCount =
            1; // Start with 1 to account for content without any newlines
        for (int i = 0; i < strlen(minibuffer->content); i++) {
            if (minibuffer->content[i] == '\n') {
                lineCount++;
            }
        }

        float lineHeight = (minibuffer->font->ascent + minibuffer->font->descent);

        // Set the height based on the number of lines found
        float minibufferHeight = lineHeight * lineCount;

        // PROMPT TEXT
        drawTextEx(minibuffer->font, prompt->content, 0,
                   minibufferHeight -
                   (minibuffer->font->ascent - minibuffer->font->descent),
                   1.0, 1.0, CT.minibuffer_prompt, CT.bg, -1, cursorVisible,
                   "text");

        if (isCurrentBuffer(&bm, "minibuffer")) {
            drawMiniCursor(minibuffer, minibuffer->font, promptWidth,
                           minibufferHeight - minibuffer->font->ascent, CT.cursor);
        }

        drawModelines(&wm, font, minibufferHeight, CT.modeline);

        for (; win != NULL; win = win->next) {
            Buffer *buffer = win->buffer;
            bool bottom = isBottomWindow(&wm, win);
            float scissorStartY =
                win->y - win->height + minibufferHeight + win->modeline.height;
            float scissorHeight = sh - scissorStartY;
            if (bottom) {
                scissorHeight += minibufferHeight;
            }

            beginScissorMode((Vec2f){win->x, scissorStartY},
                             (Vec2f){win->width, scissorHeight});

            if (win == wm.activeWindow) {
                highlightHexColors(&wm, buffer->font, buffer, rainbow_mode);
                useShader("simple");
                drawRegion(&wm, buffer->font, CT.region);
                highlightMatchingBrackets(&wm, buffer->font, CT.show_paren_match);
                if (isearch.searching)
                    highlightAllOccurrences(&wm, minibuffer->content, font,
                                            CT.isearch_highlight);

                // Draw cursor after all "overlays"
                if (!isCurrentBuffer(&bm, "minibuffer")) {
                    drawCursor(currentBuffer, wm.activeWindow, CT.cursor);
                }

                flush();

                if (isCurrentBuffer(&bm, "minibuffer")) {
                    drawBuffer(win, buffer, cursorVisible, false);
                } else {
                    drawBuffer(win, buffer, cursorVisible, true);
                    /* drawMinimap(&wm, win, buffer); */
                }

            } else {
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
        drawTextEx(minibuffer->font, minibuffer->content, promptWidth,
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

        float lastLineY = minibufferHeight - (lineHeight * lineCount) +
            getCharacterWidth(minibuffer->font, 32);
        drawTextEx(minibuffer->font, message->content, promptWidth + lastLineWidth,
                   lastLineY, 1.0, 1.0, CT.message, CT.bg, message->point,
                   cursorVisible, "text");

        endDrawing();
    }

    freeGlobalParser();
    freeFont(font);
    freeKillRing(&kr);
    freeBufferManager(&bm);
    freeWindowManager(&wm);
    closeWindow();
}

int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, inner_main, 0);
    return 0; // This line is never reached
}


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

        // NOTE Handle global arg
        if (ctrlPressed && key >= KEY_0 && key <= KEY_9) {
            char digit = '0' + (key - KEY_0);  // Convert key code to corresponding character

            if (argBuffer->size == 1 && argBuffer->content[0] == '0' && digit != '0') {
                argBuffer->content[0] = digit;
            } else if (!(argBuffer->size == 1 && argBuffer->content[0] == '0' && digit == '0')) {
                insertChar(argBuffer, digit);
            }

            message(&bm, argBuffer->content);
            return;
        }


        if (!isCurrentBuffer(&bm, "minibuffer") && !isearch.searching && (ctrlPressed || altPressed)) {
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
                }
            }
            break;


        case KEY_SPACE:
            if (ctrlPressed) {
                if (!buffer->region.active) {
                    activateRegion(buffer);
                    buffer->region.marked = true;
                } else {
                    deactivateRegion(buffer);
                    buffer->region.marked = false;
                }
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
            if (ctrlPressed) yank(buffer, &kr, arg);
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
            return;
          }
          break;
        case KEY_3:
          if (ctrl_x_pressed) {
            split_window_right(&wm, font, getScreenWidth(), getScreenHeight());
            ctrl_x_pressed = false;
            return;
          }
          break;
        case KEY_0:
          if (ctrl_x_pressed) {
            delete_window(&wm);
            ctrl_x_pressed = false;
            return;
          }
          break;

        case KEY_Z:
            /* printfSyntaxTree() */
            /* moveTo(buffer, 10, 300); */
            /* printSyntaxInfo(buffer); */
            if (ctrlPressed) {
                printf("Buffer under cursor: %s\n", getBufferUnderCursor(&wm)->name);
            }
            break;

        case KEY_X:
            if (ctrlPressed) {
                ctrl_x_pressed = true;
                printActiveWindowDetails(&wm);
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
            if (ctrlPressed) {
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
                    setBufferContent(minibuffer, ce.items[ce.currentIndex]);
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
                next_line(buffer, shiftPressed, &bm);
            }
            break;
        case KEY_UP:
            if (ctrlPressed) {
                backward_paragraph(buffer, shiftPressed);
            } else {
                previous_line(buffer, shiftPressed, &bm);
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
                next_line(buffer, shiftPressed, &bm);
            } else if (altPressed) {
                if (isCurrentBuffer(&bm, "minibuffer")) {
                    next_history_element(&nh, prompt->content, minibuffer, &bm);
                } else {
                    forward_paragraph(buffer, shiftPressed);
                }
            }
            break;
        case KEY_P:
            if (ctrlPressed && altPressed) {
                backward_list(buffer, arg);
            } else if (ctrlPressed) {
                previous_line(buffer, shiftPressed, &bm);
            } else if (altPressed) {
                if (isCurrentBuffer(&bm, "minibuffer")) {
                    previous_history_element(&nh, prompt->content, minibuffer, &bm);
                } else {
                    backward_paragraph(buffer, shiftPressed);
                }
            }
            break;
        case KEY_F:
            if (ctrlPressed && ctrl_x_pressed) {
                find_file(&bm, &wm, sw, sh);
            } else if (ctrlPressed) {
                right_char(buffer, shiftPressed, &bm, arg);
            } else if (altPressed) {
                forward_word(buffer, 1, shiftPressed);
            }
            break;
        case KEY_B:
            if (ctrlPressed) {
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
            move_beginning_of_line(buffer, shiftPressed);
            break;
        case KEY_D:
            if (ctrlPressed) {
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
                    if (buffer->region.active) buffer->region.active = false;
                    open_line(buffer);
                }
            }
            break;

        case KEY_1:
            if (altPressed && shiftPressed) shell_command(&bm);
            break;

        case KEY_EQUAL:
            if (altPressed) {
                nextTheme();
            } else if (ctrlPressed) {
                increaseFontSize(&bm, fontname, &wm, sh, arg);                    
            }
            break;
        case KEY_MINUS:
            if (altPressed) {
                previousTheme();
            } else if (ctrlPressed) {
                decreaseFontSize(&bm, fontname, &wm, sh, arg);                    
            }
            break;

        case KEY_L:
            if (altPressed) {
                split_window_right(&wm, font, sw, sh);
                other_window(&wm, 1);
            } else if (ctrlPressed) {
                /* recenter(win); */
                recenter_top_bottom(win);
            }
            break;
        case KEY_J:
            if (altPressed && wm.count <= 1) {
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
            if (altPressed && wm.count <= 1) {
                split_window_right(&wm, font, sw, sw);
            }
            break;

        case KEY_K:
            if (altPressed && wm.count <= 1) {
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
    
    updateRegion(buffer, buffer->point);
    updateSegments(&win->modeline, win->buffer);

    if (blink_cursor_mode) {
        blinkCount = 0;
        lastBlinkTime = getTime();
        cursorVisible = true;
    }
    
}

void textCallback(unsigned int codepoint) {
    // TODO it shoudl take all those variables as parameters
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
                    isearch_backward(buffer, minibuffer, false);  // Continue search backward
                } else {
                    isearch_forward(buffer, &bm, minibuffer, false);
                }

            }
        } else {
            // Normal behavior when not in search mode
            if ((codepoint == ')' || codepoint == ']' || codepoint == '}' || 
                 codepoint == '\'' || codepoint == '\"') &&
                buffer->point < buffer->size && buffer->content[buffer->point] == codepoint) {
                right_char(buffer, false, &bm, arg);
            } else {

                if (isCurrentBuffer(&bm, "minibuffer")) {
                    insertChar(minibuffer, codepoint);
                } else {
                    insertChar(buffer, codepoint);
                }


                if (electric_pair_mode) {
                    switch (codepoint) {
                    case '(':
                        insertChar(buffer, ')');
                        break;
                    case '[':
                        insertChar(buffer, ']');
                        break;
                    case '{':
                        insertChar(buffer, '}');
                        break;
                    case '\'':
                        if (!(buffer->point > 1 && buffer->content[buffer->point - 2] == '\'')) {
                            insertChar(buffer, '\'');
                        }
                        break;
                    case '\"':
                        if (!(buffer->point > 1 && buffer->content[buffer->point - 2] == '\"')) {
                            insertChar(buffer, '\"');
                        }
                        break;
                    }

                    // Move the cursor back to between the pair of characters
                    if (codepoint == '(' || codepoint == '[' || codepoint == '{' ||
                        codepoint == '\'' || codepoint == '\"') {
                        buffer->point--;
                    }
                }
                if (electric_indent_mode && (codepoint == '}' || codepoint == ';')) {
                    indent(buffer, indentation, &bm, arg);
                }
            }
        }
        updateScroll(win);
    }
}

void insertUnicodeCharacter(Buffer * buffer, unsigned int codepoint, int arg) {
    char utf8[5]; // Buffer to hold UTF-8 encoded character
    int bytes = encodeUTF8(utf8, codepoint); // Function to convert codepoint to UTF-8
    for (int i = 0; i < bytes; i++) {
        insertChar(buffer, utf8[i]);
    }
}

int encodeUTF8(char *out, unsigned int codepoint) {
    if (codepoint <= 0x7F) {
        out[0] = codepoint;
        return 1;
    } else if (codepoint <= 0x7FF) {
        out[0] = 192 + (codepoint >> 6);
        out[1] = 128 + (codepoint & 63);
        return 2;
    } else if (codepoint <= 0xFFFF) {
        out[0] = 224 + (codepoint >> 12);
        out[1] = 128 + ((codepoint >> 6) & 63);
        out[2] = 128 + (codepoint & 63);
        return 3;
    } else {
        out[0] = 240 + (codepoint >> 18);
        out[1] = 128 + ((codepoint >> 12) & 63);
        out[2] = 128 + ((codepoint >> 6) & 63);
        out[3] = 128 + (codepoint & 63);
        return 4;
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



void drawHighlight(WindowManager *wm, Font *font, size_t startPos, size_t length, Color highlightColor) {
    if (!wm || !wm->activeWindow || !wm->activeWindow->buffer) return;

    Buffer *buffer = wm->activeWindow->buffer;
    Window *activeWindow = wm->activeWindow;
    
    float x = activeWindow->x - activeWindow->scroll.x; // Adjust x by horizontal scroll
    float y = activeWindow->y + font->ascent - font->descent * 2;
    int lineCount = 0;
    
    // Calculate initial x offset and y position up to startPos
    for (size_t i = 0; i < startPos && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            lineCount++;
            x = activeWindow->x - activeWindow->scroll.x; // Reset x to the start of the line at each new line, adjust for scroll
            y -= (font->ascent + font->descent); // Move up for each new line
        } else {
            x += getCharacterWidth(font, buffer->content[i]); // Accumulate width
        }
    }

    // Calculate the width of the highlighted area
    float highlightWidth = 0;
    for (size_t i = startPos; i < startPos + length && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') break; // Stop if newline is encountered within highlight
        highlightWidth += getCharacterWidth(font, buffer->content[i]);
    }

    // Adjust y to be the lower left corner of the line to draw the highlight
    y -= font->ascent;

    // Adjust y for vertical scrolling
    y += activeWindow->scroll.y;

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
    Buffer *buffer = wm->activeWindow->buffer;
    Window *activeWindow = wm->activeWindow;

    if (!buffer->region.active) return;

    size_t start = buffer->region.start;
    size_t end = buffer->region.end;
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    size_t currentLineStart = 0;
    float x = activeWindow->x - activeWindow->scroll.x;
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
        float segmentX = win->x + 8.0 + spaceWidth; // Start with a space

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

// ORIGINAL
/* void drawModeline(WindowManager *wm, Font *font, float minibufferHeight, Color color) { */
/*     for (Window *win = wm->head; win != NULL; win = win->next) { */
/*         bool isBottom = true;  // Assume the window is at the bottom unless proven otherwise. */
/*         for (Window *checkWin = wm->head; checkWin != NULL; checkWin = checkWin->next) { */
/*             if (win != checkWin && win->x == checkWin->x && win->y - win->height == checkWin->y) { */
/*                 isBottom = false; */
/*                 break; */
/*             } */
/*         } */

/*         float modelineBaseY = win->y - win->height + wm->activeWindow->buffer->font->ascent - wm->activeWindow->buffer->font->descent; */
/*         if (isBottom) { */
/*             // Adjust for minibuffer height if this window is at the bottom */
/*             modelineBaseY += minibufferHeight; */
/*             modelineBaseY -= wm->activeWindow->buffer->font->ascent - wm->activeWindow->buffer->font->descent; */
/*         } */
        
/*         useShader("simple"); */
/*         float width = win->splitOrientation == VERTICAL ? win->width - 1 : win->width; */
/*         drawRectangle((Vec2f){win->x, modelineBaseY}, (Vec2f){width, win->modeline.height}, color); */
/*         flush(); */

/*         // Draw each segment in the modeline */
/*         useShader("text"); */
/*         float segmentX = win->x + 8.0;  // Start position for the first segment, considering 8 pixels of left fringe */
/*         float spaceWidth = getCharacterWidth(font, ' ');  // Measure the width of a space character */
/*         for (size_t i = 0; i < win->modeline.segments.count; i++) { */
/*             Segment segment = win->modeline.segments.segment[i]; */
/*             float textY = modelineBaseY + (font->ascent - font->descent); */
/*             for (size_t j = 0; j < strlen(segment.content); j++) { */
/*                 drawChar(font, segment.content[j], segmentX, textY, 1.0, 1.0, CT.text); */
/*                 segmentX += getCharacterWidth(font, segment.content[j]); // Move X position for next character */
/*             } */
/*             segmentX += spaceWidth;  // Add a space after each segment */
/*         } */
/*         flush(); */
/*     } */
/* } */


void drawBuffer(Window *win, Buffer *buffer, bool cursorVisible, bool colorPoint) {
    Font *font = buffer->font;
    const char *text = buffer->content;
    // Start from the adjusted x and y based on scroll position
    float x = win->x - win->scroll.x;  // Adjust x starting position based on horizontal scroll
    float y = win->y + win->scroll.y;  // Adjust y starting position based on vertical scroll
    size_t index = 0;
    size_t charIndex = 0;
    Color currentColor = CT.text;

    useShader("text");

    while (text[charIndex] != '\0') {
        if (text[charIndex] == '\n') {
            x = win->x - win->scroll.x;  // Reset to the start of the line, accounting for horizontal scroll
            y -= (font->ascent + font->descent);  // Move up to the next line
            charIndex++;
            continue;
        }

        if (cursorVisible && colorPoint && charIndex == buffer->point) {
            if (buffer->region.active && (buffer->point == buffer->region.start) && buffer->region.end - buffer->region.start != 0) {
                currentColor = CT.region;
            } else {
                currentColor = CT.bg;
            }
        } else if (index < buffer->syntaxArray.used && charIndex >= buffer->syntaxArray.items[index].start &&
                   charIndex < buffer->syntaxArray.items[index].end) {
            currentColor = buffer->syntaxArray.items[index].color;  // Apply syntax coloring
        } else {
            currentColor = CT.text;  // Default text color
        }

        drawChar(font, text[charIndex], x, y, 1.0, 1.0, currentColor);  // Draw each character

        x += getCharacterWidth(font, text[charIndex]);  // Advance x position by character width
        charIndex++;  // Move to the next character

        // Update syntax index when moving past the end of a highlighted section
        if (index < buffer->syntaxArray.used && charIndex == buffer->syntaxArray.items[index].end) {
            index++;  // Move to the next syntax highlight index
        }
    }

    flush();
}


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

#include <math.h>



// TODO if the frame is not selected don't render the cursor or
// render an hollow one, 

void updateScroll(Window *window) {
    Buffer *buffer = window->buffer;
    Font *font = buffer->font;

    float lineHeight = font->ascent + font->descent;

    // Calculate the vertical and horizontal positions of the cursor in the buffer
    int cursorLine = 0;
    float cursorX = 0;
    for (size_t i = 0; i < buffer->point; i++) {
        if (buffer->content[i] == '\n') {
            cursorLine++;
            cursorX = 0; // Reset cursorX at the start of each new line
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
    if (cursorY < viewTop || cursorY + lineHeight > viewBottom) {
        float newScrollY = cursorY - window->height / 2 + lineHeight / 2;
        newScrollY = fmax(0, round(newScrollY / lineHeight) * lineHeight); // NOTE Snap to nearest line
        newScrollY = fmin(newScrollY, buffer->size * lineHeight - window->height); // Don't scroll past the bottom of the buffer
        window->scroll.y = newScrollY;
    }

    // Horizontal scrolling logic
    if (cursorX < viewLeft || cursorRightEdge > viewRight) {
        float newScrollX = cursorX - window->width / 2;
        newScrollX = fmax(0, fmin(newScrollX, buffer->size * lineHeight - window->width)); // Ensure the new scroll is within the width of the longest line
        window->scroll.x = newScrollX;
    }
}




// NOTE This is just an approximation it could be a neat way to render diff files or commits
// TODO it must be much more performant
void drawMinimap(WindowManager *wm, Window *mainWindow, Buffer *buffer) {
    float minimapWidth = 80;  // Fixed width for the minimap
    float minimapX = mainWindow->x + mainWindow->width - minimapWidth;  // Positioned on the right edge of the main window

    float lineHeight = 2;  // Line height set to 2 pixels
    float scale = 1.0;  // Scaling factor so each character width is 1 pixel

    useShader("simple");

    float startY = mainWindow->y;
    size_t lineStart = 0;
    size_t wordStart = 0;
    int currentLine = 0;

    // Iterate through each character in the buffer to draw syntax highlights
    for (size_t i = 0; i <= buffer->size; i++) {
        // Check for word end or line end
        if (buffer->content[i] == ' ' || buffer->content[i] == '\n' || i == buffer->size) {
            if (i > wordStart) {  // There's a word to draw
                for (size_t j = 0; j < buffer->syntaxArray.used; j++) {
                    Syntax syntax = buffer->syntaxArray.items[j];
                    if (syntax.start >= wordStart && syntax.start < i) {
                        float syntaxStartX = minimapX + (syntax.start - lineStart) * scale;
                        float syntaxWidth = (syntax.end < i ? syntax.end : i) - syntax.start;

                        drawRectangle((Vec2f){syntaxStartX, startY}, (Vec2f){syntaxWidth, lineHeight}, syntax.color);
                    }
                }
            }
            wordStart = i + 1;  // Update start of next word
        }

        // Check for line end
        if (buffer->content[i] == '\n' || i == buffer->size) {
            startY -= (lineHeight + 1);  // Move up for the next line and add 1 pixel spacing
            lineStart = i + 1;  // Update the start of the next line
            wordStart = lineStart;  // Reset word start to the beginning of the next line
            currentLine++;
        }
    }

    flush();
}


void scrollCallback(double xOffset, double yOffset) {
    Window *win = wm.head;

    yOffset = -yOffset;  // Invert scrolling direction

    while (win != NULL) {
        if (isKeyDown(KEY_LEFT_CONTROL)) {
            int arg = (yOffset > 0) ? -1 : 1;
            if (arg > 0) {
                increaseFontSize(&bm, fontname, &wm, sh, arg);
            } else {
                decreaseFontSize(&bm, fontname, &wm, sh, -arg);
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
            win->scroll.y += scrollAmount;

            // Clamp the scroll position
            float maxScroll = win->buffer->size * lineHeight - win->height;
            win->scroll.y = fmax(0, fmin(win->scroll.y, maxScroll));

            float cursorY = 0;
            int cursorLine = 0;
            for (size_t i = 0; i < win->buffer->point; i++) {
                if (win->buffer->content[i] == '\n') {
                    cursorLine++;
                }
            }
            cursorY = cursorLine * lineHeight;

            float viewTop = win->scroll.y;
            float viewBottom = win->scroll.y + win->height - lineHeight - (win->modeline.height) * 2;

            if (cursorY < viewTop) {  // Scrolling up
                // Move point to the first visible line (top-most visible line)
                size_t newPoint = 0;
                cursorLine = 0;

                for (size_t i = 0; i < win->buffer->size; i++) {
                    if (win->buffer->content[i] == '\n') {
                        cursorLine++;
                        float lineTop = cursorLine * lineHeight;
                        if (lineTop >= viewTop) {
                            newPoint = i + 1;  // Set point to the beginning of the first fully visible line
                            break;
                        }
                    }
                }
                win->buffer->region.active = false;
                win->buffer->point = newPoint;
            } else if (cursorY > viewBottom) {  // Scrolling down
                // Move point to the last visible line (bottom-most visible line)
                size_t newPoint = 0;
                cursorLine = 0;

                for (size_t i = 0; i < win->buffer->size; i++) {
                    if (win->buffer->content[i] == '\n') {
                        cursorLine++;
                        float lineTop = cursorLine * lineHeight;
                        if (lineTop >= viewBottom) {
                            newPoint = i + 1;  // Set point to the beginning of the last fully visible line
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

// TODO its actually off by some pixels idk why
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



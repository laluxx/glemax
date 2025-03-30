#include "buffer.h"
#include "extension.h"
#include "commands.h"
#include "edit.h"
#include "syntax.h"
#include "theme.h"
#include "wm.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "globals.h"
#include "lsp.h"

// Hope doing this long enough will make me understand something..

// TODO Change this file at compile time
// collect all the functions that are not
// from ./main.c and insert a call to addCommand() for each one

// TODO comment_region()
// NOTE We could make a religion out of that

#define INITIAL_CAPACITY 100

Commands commands = {0};

/**
   Major mode for editing C code.
*/
void c_mode(Buffer *buffer) {
    // NOTE How we don't have to update the modeline
    // because we do it 144 times a second wich i love and hate at the same time.
    setMajorMode(buffer, "c");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Scheme code.
*/
void scheme_mode(Buffer *buffer) {
    setMajorMode(buffer, "scheme");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode based on SGML mode for editing HTML documents.
*/
void html_mode(Buffer *buffer) {
    setMajorMode(buffer, "html");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for Open GLSL shader files.
*/
void glsl_mode(Buffer *buffer) {
    setMajorMode(buffer, "glsl");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for the Zig programming languages.
*/
void zig_mode(Buffer *buffer) {
    setMajorMode(buffer, "zig");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode major mode for the Odin programming language.
*/
void odin_mode(Buffer *buffer) {
    setMajorMode(buffer, "odin");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing standard Makefiles.
*/
void make_mode(Buffer *buffer) {
    setMajorMode(buffer, "make");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing programs in Common Lisp.
*/
void commonlisp_mode(Buffer *buffer) {
    setMajorMode(buffer, "commonlisp");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing SCSS files.
*/
void scss_mode(Buffer *buffer) {
    setMajorMode(buffer, "scss");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Haskell programs.
*/
void haskell_mode(Buffer *buffer) {
    setMajorMode(buffer, "haskell");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Lua code.
*/
void lua_mode(Buffer *buffer) {
    setMajorMode(buffer, "lua");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for Rust code.
*/
void rust_mode(Buffer *buffer) {
    setMajorMode(buffer, "rust");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing bash code.
*/
void bash_mode(Buffer *buffer) {
    setMajorMode(buffer, "bash");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Lisp code to run in Emacs.
*/
void elisp_mode(Buffer *buffer) {
    setMajorMode(buffer, "elisp");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Python files.
*/
void python_mode(Buffer *buffer) {
    setMajorMode(buffer, "python");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Ocaml files.
*/
void ocaml_mode(Buffer *buffer) {
    setMajorMode(buffer, "ocaml");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode to edit Cascading Style Sheets (CSS).
*/
void css_mode(Buffer *buffer) {
    setMajorMode(buffer, "css");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode to editing JavaScript.
*/
void javascript_mode(Buffer *buffer) {
    setMajorMode(buffer, "javascript");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing julia code.
*/
void julia_mode(Buffer *buffer) {
    setMajorMode(buffer, "julia");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing cpp code.
*/
void cpp_mode(Buffer *buffer) {
    setMajorMode(buffer, "cpp");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for the Go programming language.
*/
void go_mode(Buffer *buffer) {
    setMajorMode(buffer, "go");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing JSON files
*/
void json_mode(Buffer *buffer) {
    setMajorMode(buffer, "json");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}

/**
   Major mode for editing Regex.
*/
void regex_mode(Buffer *buffer) {
    setMajorMode(buffer, "regex");
    clearSyntaxArray(buffer);
    parseSyntax(buffer);
}


/**
   Force parsing the syntax of the active buffer if supported;
*/
void parse_systax(Buffer *buffer) {
    parseSyntax(buffer);
}

/**
   Toggle scrollBar parameter for the active WINDOW.
*/
void toggle_scroll_bar() {
    wm.activeWindow->parameters.scrollBar = !wm.activeWindow->parameters.scrollBar;
}

/**
   Toggle truncating of long lines for the current bufffer.
*/
void toggle_truncate_lines() {
    wm.activeWindow->parameters.truncateLines = !wm.activeWindow->parameters.truncateLines;
}

/**
   Toggle show_scroll_bar_with_minimap_mode minor mode.
*/
void show_scroll_bar_with_minimap_mode() {
    show_scroll_bar_with_minimap = !show_scroll_bar_with_minimap;
}


/**
   Toggle hide_scroll_bar_with_minimap_mode minor mode.
*/
void hide_scroll_bar_with_minimap_mode() {
    hide_scroll_bar_with_minimap = !hide_scroll_bar_with_minimap;
}

/**
   Toggle keep-right-fringe-mode minor mode.
*/
void keep_right_fringe_mode() {
    keep_right_fringe = !keep_right_fringe;
}

/**
   Toggle scroll-bar-mode minor mode if true windows *might* have scrollbars.
*/
void scroll_bar_mode() {
    scroll_bar = !scroll_bar;
}

/**
   Toggle scroll-lerp-mode minor mode if true kscrolling will be lerped at scroll_lerp_speed speed.
*/
void scroll_lerp_mode() {
    scroll_lerp = !scroll_lerp;
}


/**
   Toggle move-mark-memory-mode minor mode if true Move Mark the the mark memory if buffer->point < buffer->region.mark.
*/
void move_mark_memory_mode() {
    mmm = !mmm;
}




/**
 * Toggles minimap mode. If `minimap_easing_mode` is true, the minimap will be enabled/disabled with easing.
 * If `show_scroll_bar_with_minimap` is true, the scrollbar will also be toggled accordingly.
 */
void minimap_mode(WindowManager* wm) {
    Window *win = wm->activeWindow;

    if (minimap_easing_mode) {
        if (!win->parameters.minimap) {
            // Enable the minimap with easing
            win->parameters.minimap = true;
            win->parameters.minimap_target_width = 110.0f + minimap_left_padding; // Target width for enabling
            win->parameters.minimap_lerp_active = true;

            // If show_scroll_bar_with_minimap is true, enable the scrollbar and start lerping its width
            if (show_scroll_bar_with_minimap) {
                win->parameters.scrollBar = true;
                // Start lerping the scrollbar width from 0 to scroll_bar_thickness
                // This is handled in the drawScrollbar function using the lerpFactor
            }
        } else {
            // Disable the minimap with easing
            win->parameters.minimap_target_width = 0.0f; // Target width for disabling
            win->parameters.minimap_lerp_active = true;
        }
    } else {
        // Immediate toggle without easing
        win->parameters.minimap = !win->parameters.minimap;
        win->parameters.minimap_width = win->parameters.minimap ? 110.0f + minimap_left_padding : 0.0f;

        // Handle scrollbar visibility immediately
        if (win->parameters.minimap) {
            if (show_scroll_bar_with_minimap) {
                win->parameters.scrollBar = true;
            }
        } else {
            if (hide_scroll_bar_with_minimap) {
                win->parameters.scrollBar = false;
            }
        }
    }
}

/**
   Toggle lerp-line-mode minor mode if true opening lines will be lerped.
*/
void lerp_line_mode() {
    lerp_line = !lerp_line;
}

/**
   Toggle swap-windows-parameters-mode minor mode if true swap-windows will also swap the parameters of the 2 windows.
*/
void swap_window_parameters_mode() {
    swap_windows_parameters = !swap_windows_parameters;
}

/**
   Toggle line-move-visual-mode minor mode
*/
void line_move_visual_mode() {
    line_move_visual = !line_move_visual;
}

/**
   Major mode not specialized for anything in particular.
*/
void fundamental_mode(Buffer *buffer) {
    // NOTE That we still don't have to update the modeline
    setMajorMode(buffer, "fundamental");
    clearSyntaxArray(buffer);
}

// Helper function to resize the commands array if needed
static void ensureCapacity(Commands *cmds) {
    if (cmds->capacity == 0) {
        // Initialize capacity to INITIAL_CAPACITY if it's 0
        cmds->capacity = INITIAL_CAPACITY;
        cmds->commands = (Command *)malloc(cmds->capacity * sizeof(Command));
        if (!cmds->commands) {
            fprintf(stderr, "Error: Failed to allocate memory for commands.\n");
            exit(EXIT_FAILURE);
        }
        return;
    }

    if (cmds->size >= cmds->capacity) {
        size_t new_capacity = cmds->capacity * 2;

        printf("Resizing commands array: capacity=%zu, new_capacity=%zu\n",
               cmds->capacity, new_capacity); // TODO After N Hour(s) of execution..

        Command *newCommands =
            (Command *)realloc(cmds->commands, new_capacity * sizeof(Command));

        if (!newCommands) {
            fprintf(stderr,
                    "Error: Failed to allocate memory for commands. Current "
                    "size=%zu, requested size=%zu\n",
                    cmds->size, new_capacity * sizeof(Command));
            exit(EXIT_FAILURE);
        }

        cmds->commands = newCommands;
        cmds->capacity = new_capacity;
        printf("Resize successful: new capacity=%zu\n", cmds->capacity);
    }
}

static void initCommand(Command *cmd, const char *name, const char *description, CommandType type) {
    cmd->name = strdup(name);
    cmd->description = strdup(description);
    cmd->type = type;

    if (!cmd->name || !cmd->description) {
        fprintf(stderr, "Error: Failed to allocate memory for command name or description.\n");
        exit(EXIT_FAILURE);
    }
}

#include "gemini.h"

/**
   Gemini.
*/
void gemini() {
    switch_or_split_window(&wm, "*gemini*", &wm.activeWindow->parameters);
    wm.activeWindow->parameters.truncateLines = false; // Wrap lines as specified.
    
    if (show_minimap_with_gemini) {
        Buffer *pb = getPreviousBuffer(&bm);
        
        // Check if previous buffer exists and has windows
        if (pb && pb->displayWindows.windowCount > 0) {
            // Iterate through all windows for this buffer to check minimap status
            bool minimap_needed = true;
            for (int i = 0; i < pb->displayWindows.windowCount; i++) {
                if (pb->displayWindows.windows[i]->parameters.minimap) {
                    minimap_needed = false;
                    break;
                }
            }
            
            if (minimap_needed) {
                minimap_mode(&wm);
            }
        }
    }
    
    GeminiOutput go = gemini_fetch("gemini://geminiprotocol.net/docs/", getBuffer(&bm, "*gemini*"));
    setBufferContent(getBuffer(&bm, "*gemini*"), go.content, false);
    setMajorMode(getBuffer(&bm, "*gemini*"), "gemini");
    wm.activeWindow->buffer->readOnly = true;
}

/**
   View the log of recent echo-area messages: the '*Messages*' buffer.
*/
// FIXME CRASH
void view_echo_area_messages() {
    switch_or_split_window(&wm, "messages", false);
}


/**
   Toggle region-alpha minor mode.
 */
void region_alpha_mode() {
    region_alpha = !region_alpha;
}


void initCommands() {
    commands.commands = (Command *)malloc(INITIAL_CAPACITY * sizeof(Command));
    commands.size = 0;
    commands.capacity = INITIAL_CAPACITY;
 
    // Void
    addVoidCommand(         &commands, "lsp-start",                          "Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.",               start_lsp);
    addVoidCommand(         &commands, "lsp-stop",                           "Stop LSP server for PROJECT.",                                                    stop_lsp);
    addVoidCommand(         &commands, "next-theme",                         "Switch to the next theme.",                                                       switchToNextTheme);
    addVoidCommand(         &commands, "previous-theme",                     "Switch to the previous theme.",                                                   switchToPreviousTheme);
    addVoidCommand(         &commands, "gemini",                             "Gemini.",                                                                         gemini);
    addVoidCommand(         &commands, "view-echo-area-messages",            "View the log of recent echo-area messages: the '*Messages*' buffer.",             view_echo_area_messages);
    addVoidCommand(         &commands, "toggle-truncate-lines",              "Toggle truncating of long lines for the current bufffer.",                        toggle_truncate_lines);
    addVoidCommand(         &commands, "toggle-scroll-bar",                  "Toggle scrollBar parameter for the active WINDOW. ",                              toggle_scroll_bar);
    addVoidCommand(         &commands, "scroll-lerp-mode",                   "Toggle scroll-lerp minor mode",                                                   scroll_lerp_mode);
    addVoidCommand(         &commands, "scroll-bar-mode",                    "Toggle scroll-bar-mode minor mode if true windows *might* have scrollbars.",      scroll_bar_mode);
    addVoidCommand(         &commands, "keep-right-fringe-mode",             "Toggle keep-right-fringe-mode minor mode.",                                       keep_right_fringe_mode);
    addVoidCommand(         &commands, "hide-scroll-bar-with-minimap_mode",  "Toggle hide-scroll-bar-with-minimap_mode minor mode.",                            hide_scroll_bar_with_minimap_mode);
    addVoidCommand(         &commands, "show-scroll-bar-with-minimap_mode",  "Toggle show-scroll-bar-with-minimap-mode minor mode.",                            show_scroll_bar_with_minimap_mode);
    addVoidCommand(         &commands, "minimap-mode",                       "Toggle minimap-mode minor mode.",                                                 minimap_mode);
    addVoidCommand(         &commands, "lerp-line-mode",                     "idk",                                                                             lerp_line_mode);
    addVoidCommand(         &commands, "swap-windows-parameters-mode",       "Toggle swap-windows-parameters minor mode if true 'swap-windows' will also swap the parameters of the 2 windows.", swap_window_parameters_mode);
    addVoidCommand(         &commands, "line-move-visual-mode",              "Whatever",                                                                        line_move_visual_mode);
    addVoidCommand(         &commands, "move-mark-memory-mode",              "Toggle move-mark-memory-mode minor mode if true Move Mark the the mark memory if buffer->point < buffer->region.mark.", move_mark_memory_mode);
    addVoidCommand(         &commands, "toggle-vsync",                       "Toggle vsync",                                                                    toggle_vsync);
    addVoidCommand(         &commands, "reload-shaders",                     "Recompile and link all known shader programs",                                    reloadShaders);
    addVoidCommand(         &commands, "region-alpha-mode",                  "Toggle region-alpha minor mode.",                                                 region_alpha_mode);
    addVoidCommand(         &commands, "eval-buffer",                        "Execute the accessible portion of current buffer as Guile scheme code.",          eval_buffer);
    addVoidCommand(         &commands, "eval-region",                        "Execute the region as Guile scheme code.",                                        eval_region);
    addVoidCommand(         &commands, "load-user-init-file",                "Load a user init-file, used at startup.",                                         load_user_init_file);

    // Buffer
    addBufferCommand(       &commands, "fundamental-mode",                   "Major mode not specialized for anything in particular.",                          fundamental_mode);
    addBufferCommand(       &commands, "c-mode",                             "Major mode for editing C code.",                                                  c_mode);
    addBufferCommand(       &commands, "scheme-mode",                        "Major mode for editing Scheme code.",                                             scheme_mode);
    addBufferCommand(       &commands, "html-mode",                          "Major mode based on SGML mode for editing HTML documents.",                       html_mode);
    addBufferCommand(       &commands, "glsl-mode",                          "Major mode for Open GLSL shader files",                                           glsl_mode);
    addBufferCommand(       &commands, "zig-mode",                           "Major mode for the Zig programming languages.",                                   zig_mode);
    addBufferCommand(       &commands, "odin-mode",                          "Major mode major mode for the Odin programming language.",                        odin_mode);
    addBufferCommand(       &commands, "make-mode",                          "Major mode for editing standard Makefiles.",                                      make_mode);
    addBufferCommand(       &commands, "commonlisp-mode",                    "Major mode for editing programs in Common Lisp.",                                 commonlisp_mode);
    addBufferCommand(       &commands, "scss-mode",                          "Major mode for editing SCSS files.",                                              scss_mode);
    addBufferCommand(       &commands, "haskell-mode",                       "Major mode for editing Haskell programs.",                                        haskell_mode);
    addBufferCommand(       &commands, "lua-mode",                           "Major mode for editing Lua code.",                                                lua_mode);
    addBufferCommand(       &commands, "rust-mode",                          "Major mode for Rust code.",                                                       rust_mode);
    addBufferCommand(       &commands, "bash-mode",                          "Major mode for editing bash code.",                                               bash_mode);
    addBufferCommand(       &commands, "elisp-mode",                         "Major mode for editing Lisp code to run in Emacs.",                               elisp_mode);
    addBufferCommand(       &commands, "python-mode",                        "Major mode for editing Python files.",                                            python_mode);
    addBufferCommand(       &commands, "ocaml-mode",                         "Major mode for editing Ocaml files.",                                             ocaml_mode);
    addBufferCommand(       &commands, "css-mode",                           "Major mode to edit Cascading Style Sheets (CSS).",                                css_mode);
    addBufferCommand(       &commands, "javascript-mode",                    "Major mode to editing JavaScript.",                                               javascript_mode);
    addBufferCommand(       &commands, "julia-mode",                         "Major mode for editing julia code.",                                              julia_mode);
    addBufferCommand(       &commands, "cpp-mode",                           "Major mode for editing cpp code.",                                                cpp_mode);
    addBufferCommand(       &commands, "go-mode",                            "Major mode for the Go programming language.",                                     go_mode);
    addBufferCommand(       &commands, "json-mode",                          "Major mode for editing JSON files",                                               json_mode);
    addBufferCommand(       &commands, "regex-mode",                         "Major mode for editing Regex.",                                                   regex_mode);
    addBufferCommand(       &commands, "add-indentation",                    "Add one tab character at Beginning Of Line.",                                     add_indentation);
    addBufferCommand(       &commands, "remove-indentation",                 "Remove one tab character from Beginning Of Line.",                                remove_indentation);
    addBufferCommand(       &commands, "beginning-of-buffer",                "Move point to the beginning of the buffer.",                                      beginning_of_buffer);
    addBufferCommand(       &commands, "end-of-buffer",                      "Move point to the end of the buffer.",                                            end_of_buffer);
    addBufferCommand(       &commands, "set-mark-command",                   "Set the mark where point is, and activate it; or jump to the mark",               set_mark_command);
    addBufferCommand(       &commands, "exchange-point-and-mark",            "Put mark where point is now, and point where the mark is now.",                   exchange_point_and_mark);
    addBufferCommand(       &commands, "set-goal-column",                    "Set the current horizontal position as a goal column.",                           set_goal_column);
    addBufferCommand(       &commands, "delete-char",                        "Delete the following N characters (previous if N is negative).",                  delete_char);
    addBufferCommand(       &commands, "open-line",                          "Insert a newline and leave point before it.",                                     open_line);
    addBufferCommand(       &commands, "duplicate-line",                     "Duplicate the current line N times.",                                             duplicate_line);
    addBufferCommand(       &commands, "clearSyntaxArray",                   "Clear Buffer->syntaxArray.used = 0 without freeing the memory.",                  clearSyntaxArray);
    addBufferCommand(       &commands, "parse-syntax",                       "Parse buffer->content and fill buffer->syntaxArray once.",                        parse_systax);
    addBufferCommand(       &commands, "set-mark-command",                   "Set the mark where point is, and activate the region; or jump to the mark",       set_mark_command);
    addBufferCommand(       &commands, "delete-region",                      "Delete the text between START and END.",                                          delete_region);
    addBufferCommand(       &commands, "kill-line",                          "Kill the rest of the current line; if no nonblanks there, kill thru newline.",    kill_line);
    addBufferCommand(       &commands, "kill-region",                        "Kill (cut) text between point and mark.",                                         kill_region);
    addBufferCommand(       &commands, "kill-word",                          "Kill characters forward until encountering the end of a word.",                   kill_word);
    addBufferCommand(       &commands, "kill-ring-save",                     "Save the region as if killed, but don't kill it.",                                kill_ring_save);
    addBufferCommand(       &commands, "kill-sexp",                          "Kill the sexp (balanced expression) following point.",                            kill_sexp);
    addBufferCommand(       &commands, "backward-kill-word",                 "Kill characters backward until encountering the beginning of a word.",            backward_kill_word);
    addBufferCommand(       &commands, "goto-definition",                    "Find definitions of the symbol under point.",                                     goto_definition);
    addBufferCommand(       &commands, "insert-guile-symbols",               "Find all Scheme symbols and insert them in the active buffer.",                   insert_guile_symbols);

    // BM
    addBufferManagerCommand(&commands, "keep-lines",                         "Delete all lines except those containing matches for REGEXP.",                    keep_lines);
    addBufferManagerCommand(&commands, "shell-command",                      "Execute string COMMAND in inferior shell; display output, if any.",               shell_command);
    addBufferManagerCommand(&commands, "eval-expression",                    "Evaluate EXP and print value in the echo area.",                                  eval_expression);
    addBufferManagerCommand(&commands, "goto-line",                          "Go to LINE, counting from line 1 at beginning of buffer.",                        goto_line);
    addBufferManagerCommand(&commands, "helpful-symbol",                     "Show help for SYMBOL, a variable, function, macro, or face.",                     helpful_symbol);
    addBufferManagerCommand(&commands, "load-font",                          "load a new global font and adjust windows.",                                      load_font);
    addBufferManagerCommand(&commands, "execute-extended-command",           "Read a command name, then read the arguments and call the command.",              execute_extended_command);
    addBufferManagerCommand(&commands, "change-major-mode",                  "Read a major-mode name, then update the active buffer major mode to it.",         change_major_mode);

    // BSA
    addBufferShiftArgCommand(&commands, "forward-word",                      "Move point forward ARG words (backward if ARG is negative).",                     forward_word);
    addBufferShiftArgCommand(&commands, "backward-word",                     "Move backward until encountering the beginning of a word.",                       backward_word);
    addBufferShiftArgCommand(&commands, "right-char",                        "Move point N characters to the right (to the left if N is negative)",             right_char);
    addBufferShiftArgCommand(&commands, "left-char",                         "Move point N characters to the left (to the right if N is negative).",            left_char);
    addBufferShiftArgCommand(&commands, "forward-list",                      "Move forward across one balanced group of parentheses.",                          forward_list);
    addBufferShiftArgCommand(&commands, "backward-list",                     "Move backward across one balanced group of prentheses.",                          backward_list);
    addBufferShiftArgCommand(&commands, "forward-sexp",                      "Move forward across one balanced expression (sexp).",                             forward_sexp);
    addBufferShiftArgCommand(&commands, "backward-sexp",                     "Move backward across one balanced expression (sexp).",                            backward_sexp);
    addBufferShiftArgCommand(&commands, "indent-region",                     "Indent each nonblank line in the region.",                                        indent_region);
    addBufferShiftArgCommand(&commands, "indent-line",                       "Indent current line.",                                                            indent_line);


    // WSA
    addWindowShiftArgCommand(&commands, "next-line",                         "Move cursor vertically down ARG lines.",                                          next_line);
    addWindowShiftArgCommand(&commands, "previous-line",                     "Move cursor vertically up ARG lines.",                                            previous_line);
    addWindowShiftArgCommand(&commands, "move-beginning-of-line",            "Move point to visible beginning of current logical line.",                        move_beginning_of_line);
    addWindowShiftArgCommand(&commands, "move-end-of-line",                  "Move point to end of current line as displayed.",                                 move_end_of_line);

}

void addSchemeCommand(Commands *cmds, const char *name, const char *description, const char *scheme_expr) {
    ensureCapacity(cmds);
    Command *newCommand = &cmds->commands[cmds->size++];
    initCommand(newCommand, name, description, CMD_TYPE_SCHEME);
    newCommand->func.scheme_expr = strdup(scheme_expr);

    if (!newCommand->func.scheme_expr) {
        fprintf(stderr, "Error: Failed to allocate memory for Scheme expression.\n");
        exit(EXIT_FAILURE);
    }
}

void addVoidCommand(Commands *cmds, const char *name, const char *description, VoidFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_VOID, (void *)func);
}

void addBufferCommand(Commands *cmds, const char *name, const char *description, BufferFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_BUFFER, (void *)func);
}

void addBufferManagerCommand(Commands *cmds, const char *name, const char *description, BufferManagerFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_BUFFERMANAGER, (void *)func);
}

void addWindowManagerCommand(Commands *cmds, const char *name, const char *description, WindowManagerFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_WINDOWMANAGER, (void *)func);
}

void addWindowManagerParametersCommand(Commands *cmds, const char *name, const char *description, WindowManagerParametersFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_WINDOWMANAGERPARAMETERS, (void *)func);
}

void addBufferShiftArgCommand(Commands *cmds, const char *name, const char *description, BufferShiftArgFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_BUFFERSHIFTARG, (void *)func);
}

void addWindowShiftArgCommand(Commands *cmds, const char *name, const char *description, WindowShiftArgFunc func) {
    addCommand(cmds, name, description, CMD_TYPE_C_WINDOWSHIFTARG, (void *)func);
}



void addCommand(Commands *cmds, const char *name, const char *description,
                CommandType type, void *func) {
    ensureCapacity(cmds);
    Command *newCommand = &cmds->commands[cmds->size++];
    initCommand(newCommand, name, description, type);

    // Assign the function pointer based on the type
    switch (type) {
    case CMD_TYPE_C_VOID:
        newCommand->func.void_func = (VoidFunc)func;
        break;
    case CMD_TYPE_C_BUFFER:
        newCommand->func.buffer_func = (BufferFunc)func;
        break;
    case CMD_TYPE_C_BUFFERMANAGER:
        newCommand->func.bufferManager_func = (BufferManagerFunc)func;
        break;
    case CMD_TYPE_C_WINDOWMANAGER:
        newCommand->func.windowManager_func = (WindowManagerFunc)func;
        break;
    case CMD_TYPE_C_WINDOWMANAGERPARAMETERS:
        newCommand->func.windowManagerParameters_func = (WindowManagerParametersFunc)func;
    case CMD_TYPE_C_BUFFERSHIFTARG:
        newCommand->func.bufferShiftArg_func = (BufferShiftArgFunc)func;
        break;
    case CMD_TYPE_C_WINDOWSHIFTARG:
        newCommand->func.windowShiftArg_func = (WindowShiftArgFunc)func;
        break;

    case CMD_TYPE_SCHEME:
        // For Scheme commands, `func` should be a `char*` (the Scheme expression)
        newCommand->func.scheme_expr = strdup((char *)func);
        if (!newCommand->func.scheme_expr) {
            fprintf(stderr,
                    "Error: Failed to allocate memory for Scheme expression.\n");
            exit(EXIT_FAILURE);
        }
        break;
    default:
        fprintf(stderr, "Error: Unknown command type.\n");
        exit(EXIT_FAILURE);
    }
}

SCM scm_register_command(SCM name, SCM description, SCM expr) {
    // Check that all arguments are strings
    if (!scm_is_string(name) || !scm_is_string(description) || !scm_is_string(expr)) {
        scm_throw(scm_from_locale_symbol("glemax-error"),
                  scm_list_1(scm_from_locale_string("All arguments must be strings")));
        return SCM_BOOL_F;
    }

    char *c_name = safe_scm_to_string(name);
    char *c_description = safe_scm_to_string(description);
    char *c_expr = safe_scm_to_string(expr);
    
    if (!c_name || !c_description || !c_expr) {
        free(c_name); 
        free(c_description); 
        free(c_expr);
        scm_throw(scm_from_locale_symbol("glemax-error"),
                  scm_list_1(scm_from_locale_string("Failed to convert strings")));
        return SCM_BOOL_F;
    }
    
    addSchemeCommand(&commands, c_name, c_description, c_expr);
    
    free(c_name);
    free(c_description);
    free(c_expr);
    
    return SCM_BOOL_T;
}

void executeBufferCommand(const char *name, Buffer *buffer) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_BUFFER) {
                commands.commands[i].func.buffer_func(buffer);
            } else {
                printf("Error: '%s' is not a buffer command\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void executeBufferShiftArgCommand(const char *name, Buffer *buffer, bool shift, bool arg) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_BUFFERSHIFTARG) {
                commands.commands[i].func.bufferShiftArg_func(buffer, shift, arg);
            } else {
                printf("Error: '%s' is not a bufferShiftArg command\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void executeWindowShiftArgCommand(const char *name, Window *window, bool shift, bool arg) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_WINDOWSHIFTARG) {
                commands.commands[i].func.windowShiftArg_func(window, shift, arg);
            } else {
                printf("Error: '%s' is not a WindowShiftArg command\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void executeBufferArgCommand(const char *name, Buffer *buffer, int arg) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_BUFFER) {
                // Assuming the function can handle the arg; may need adjustment
                commands.commands[i].func.buffer_func(buffer);
            } else {
                printf("Error: '%s' is not a buffer command\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void executeBufferManagerCommand(const char *name, BufferManager *manager) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_BUFFERMANAGER) {
                commands.commands[i].func.bufferManager_func(manager);
            } else {
                printf("Error: '%s' requires a BufferManager\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void executeWindowManagerCommand(const char *name, WindowManager *manager) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_WINDOWMANAGER) {
                commands.commands[i].func.windowManager_func(manager);
            } else {
                printf("Error: '%s' requires a WindowManager\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void executeWindowManagerParametersCommand(const char *name, WindowManager *manager, WindowParameters parameters) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C_WINDOWMANAGER) {
                commands.commands[i].func.windowManager_func(manager);
            } else {
                printf("Error: '%s' requires a WindowManager and WindowParameters\n", name);
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}



// NOTE We still don't use this +
void executeCommand(const char *name) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            switch (commands.commands[i].type) {
            case CMD_TYPE_C_VOID:
                commands.commands[i].func.void_func();
                break;
            case CMD_TYPE_SCHEME: {
                char *result = eval_scheme_string(commands.commands[i].func.scheme_expr);
                if (result) free(result);
                break;
            }
            default:
                printf("Error: '%s' cannot be executed without arguments\n", name);
                break;
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

void freeCommands(Commands *cmds) {
    for (size_t i = 0; i < cmds->size; ++i) {
        free(cmds->commands[i].name);
        free(cmds->commands[i].description);
        if (cmds->commands[i].type == CMD_TYPE_SCHEME) {
            free(cmds->commands[i].func.scheme_expr);
        }
    }
    free(cmds->commands);
    cmds->commands = NULL;
    cmds->size = 0;
    cmds->capacity = 0;
}


#include "buffer.h"
#include "extension.h"
#include "commands.h"
#include "edit.h"
#include "syntax.h"
#include "theme.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// [ ] TODO Change this file at compile time
// collect all the void functions that are not
// from ./main.c and insert a call to addCommand() for each one
// scheme already has this power but C doesn't

// TODO comment_region()

#define INITIAL_CAPACITY 100

Commands commands = {0};

void testCommand() {
    printf("Hello from C!\n");
}

void c_mode(Buffer *buffer) {
    // NOTE How we don't have to update the modeline
    // because we do it 144 times a second wich i love and hate at the same time.
    setMajorMode(buffer, "c");
}


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
               cmds->capacity, new_capacity);

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

#include "faces.h"
#include "gemini.h"

void gemini() {
    switch_or_split_window(&wm, font, "*gemini*", sw, sh);
    GeminiOutput go = gemini_fetch("gemini://geminiprotocol.net/docs/", getBuffer(&bm, "*gemini*"));
    setBufferContent(getBuffer(&bm, "*gemini*"), go.content, false);
    setMajorMode(getBuffer(&bm, "*gemini*"), "gemini");
}

void initCommands() {
    commands.commands = (Command *)malloc(INITIAL_CAPACITY * sizeof(Command));
    commands.size = 0;
    commands.capacity = INITIAL_CAPACITY;
 
    addVoidCommand(         &commands, "c-command",                   "Test C command",                                                       testCommand);
    addVoidCommand(         &commands, "next-theme",                  "Switch to the next theme.",                                            switchToNextTheme);
    addVoidCommand(         &commands, "previous-theme",              "Switch to the previous theme.",                                        switchToPreviousTheme);
    addVoidCommand(         &commands, "gemini",                      "Gemini.",                                                              gemini);

    addBufferCommand(       &commands, "c-mode",                      "Major mode for editing C code.",                                       c_mode);
    addBufferCommand(       &commands, "fundamental-mode",            "Major mode not specialized for anything in particular.",               fundamental_mode);
    addBufferCommand(       &commands, "add-indentation",             "Add one tab character at Beginning Of Line.",                          add_indentation);
    addBufferCommand(       &commands, "remove-indentation",          "Remove one tab character from Beginning Of Line.",                     remove_indentation);
    addBufferCommand(       &commands, "beginning-of-buffer",         "Move point to the beginning of the buffer.",                           beginning_of_buffer);
    addBufferCommand(       &commands, "end-of-buffer",               "Move point to the end of the buffer.",                                 end_of_buffer);
    addBufferCommand(       &commands, "set-mark-command",            "Set the mark where point is, and activate it; or jump to the mark",    set_mark_command);
    addBufferCommand(       &commands, "exchange-point-and-mark",     "Put mark where point is now, and point where the mark is now.",        exchange_point_and_mark);
    addBufferCommand(       &commands, "set-goal-column",             "Set the current horizontal position as a goal column.",                set_goal_column);
    addBufferCommand(       &commands, "delete-char",                 "Delete the following N characters (previous if N is negative).",       delete_char);
    addBufferCommand(       &commands, "open-line",                   "Insert a newline and leave point before it.",                          open_line);
    addBufferCommand(       &commands, "duplicate-line",              "Duplicate the current line N times.",                                  duplicate_line);


    addBufferManagerCommand(&commands, "keep-lines",                  "Delete all lines except those containing matches for REGEXP.",         keep_lines);
    addBufferManagerCommand(&commands, "shell-command",               "Execute string COMMAND in inferior shell; display output, if any.",    shell_command);
    addBufferManagerCommand(&commands, "eval-expression",             "Evaluate EXP and print value in the echo area.",                       eval_expression);
    addBufferManagerCommand(&commands, "goto-line",                   "Go to LINE, counting from line 1 at beginning of buffer.",             goto_line);
    addBufferManagerCommand(&commands, "helpful-symbol",              "Show help for SYMBOL, a variable, function, macro, or face.",          helpful_symbol);
    addBufferManagerCommand(&commands, "load-font",                   "load a new global font and adjust windows.",                           load_font);
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

// NOTE We still don't use this
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


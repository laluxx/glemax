#include "extension.h"
#include "commands.h"
#include "edit.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// [ ] TODO Change this file at compile time
// collect all the void functions that are not
// from ./main.c and insert a call to addCommand() for each one
// scheme already has this power but C don't this is a powerful idea

#define INITIAL_CAPACITY 10

Commands commands = {0};

void testCommand() {
    printf("Sample command executed!\n");
}

void initCommands() {
    commands.commands = (Command *)malloc(INITIAL_CAPACITY * sizeof(Command));
    commands.size = 0;
    commands.capacity = INITIAL_CAPACITY;

    addCommand(&commands, "test", "A test command", testCommand);
    /* addCommand(&commands, "keep-lines", "A test command", keep_lines); // TODO SEGFAULT */
}

// commands.c
void addCommand(Commands *cmds, const char *name, const char *description, CommandFunc func) {
    if (cmds->size >= cmds->capacity) {
        cmds->capacity *= 2;
        cmds->commands = (Command *)realloc(cmds->commands, cmds->capacity * sizeof(Command));
    }
    Command *newCommand = &cmds->commands[cmds->size++];
    newCommand->name = strdup(name);
    newCommand->description = strdup(description);
    newCommand->type = CMD_TYPE_C;
    newCommand->func.c_func = func;
}

void addSchemeCommand(Commands *cmds, const char *name, const char *description, const char *scheme_expr) {
    if (cmds->size >= cmds->capacity) {
        cmds->capacity *= 2;
        cmds->commands = (Command *)realloc(cmds->commands, cmds->capacity * sizeof(Command));
    }
    Command *newCommand = &cmds->commands[cmds->size++];
    newCommand->name = strdup(name);
    newCommand->description = strdup(description);
    newCommand->type = CMD_TYPE_SCHEME;
    newCommand->func.scheme_expr = strdup(scheme_expr);
}

/* void addCommand(Commands *cmds, const char *name, const char *description, CommandFunc func) { */
/*     if (cmds->size >= cmds->capacity) { */
/*         // Double the capacity */
/*         cmds->capacity *= 2; */
/*         cmds->commands = (Command *)realloc(cmds->commands, cmds->capacity * sizeof(Command)); */
/*     } */

/*     // Add new command */
/*     Command *newCommand = &cmds->commands[cmds->size++]; */
/*     newCommand->name = strdup(name); */
/*     newCommand->description = strdup(description); */
/*     newCommand->execute = func; */
/* } */

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

void executeCommand(const char *name) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            if (commands.commands[i].type == CMD_TYPE_C) {
                commands.commands[i].func.c_func();
            } else {
                // Execute Scheme expression
                char *result = eval_scheme_string(commands.commands[i].func.scheme_expr);
                if (result) {
                    free(result);
                }
            }
            return;
        }
    }
    printf("Command '%s' not found.\n", name);
}

/* void executeCommand(const char *name) { */
/*     for (size_t i = 0; i < commands.size; ++i) { */
/*         if (strcmp(commands.commands[i].name, name) == 0) { */
/*             // Execute the command's function */
/*             commands.commands[i].execute(); */
/*             return; */
/*         } */
/*     } */
/*     // If no command matches the name */
/*     printf("Command '%s' not found.\n", name); */
/* } */

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

/* void freeCommands(Commands *cmds) { */
/*     for (size_t i = 0; i < cmds->size; ++i) { */
/*         free(cmds->commands[i].name); */
/*         free(cmds->commands[i].description); */
/*     } */
/*     free(cmds->commands); */
/*     cmds->commands = NULL; */
/*     cmds->size = 0; */
/*     cmds->capacity = 0; */
/* } */


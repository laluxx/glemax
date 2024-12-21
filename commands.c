#include "commands.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
}

void addCommand(Commands *cmds, const char *name, const char *description, CommandFunc func) {
    if (cmds->size >= cmds->capacity) {
        // Double the capacity
        cmds->capacity *= 2;
        cmds->commands = (Command *)realloc(cmds->commands, cmds->capacity * sizeof(Command));
    }

    // Add new command
    Command *newCommand = &cmds->commands[cmds->size++];
    newCommand->name = strdup(name);
    newCommand->description = strdup(description);
    newCommand->execute = func;
}

void executeCommand(const char *name) {
    for (size_t i = 0; i < commands.size; ++i) {
        if (strcmp(commands.commands[i].name, name) == 0) {
            // Execute the command's function
            commands.commands[i].execute();
            return;
        }
    }
    // If no command matches the name
    printf("Command '%s' not found.\n", name);
}

void freeCommands(Commands *cmds) {
    for (size_t i = 0; i < cmds->size; ++i) {
        free(cmds->commands[i].name);
        free(cmds->commands[i].description);
    }
    free(cmds->commands);
    cmds->commands = NULL;
    cmds->size = 0;
    cmds->capacity = 0;
}


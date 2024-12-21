#ifndef COMMANDS_H
#define COMMANDS_H

#include <stddef.h>

// Function pointer type for commands
typedef void (*CommandFunc)();

typedef struct {
    char *name;            // Name of the command
    char *description;     // Description of the command
    CommandFunc execute;   // Pointer to the command function
} Command;

typedef struct {
    Command *commands; // Dynamic array of commands
    size_t size;       // Number of commands in the array
    size_t capacity;   // Current capacity of the array
} Commands;

extern Commands commands;

void initCommands();
void addCommand(Commands *cmds, const char *name, const char *description, CommandFunc func);
void executeCommand(const char *name);
void freeCommands(Commands *cmds);

#endif // COMMANDS_H

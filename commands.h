#ifndef COMMANDS_H
#define COMMANDS_H

#include <stddef.h>
#include <libguile.h>

// Function pointer type for commands
typedef void (*CommandFunc)();

typedef enum {
    CMD_TYPE_C,
    CMD_TYPE_SCHEME
} CommandType;

typedef struct {
    char *name;            
    char *description;     
    CommandType type;      // Whether this is a C or Scheme function
    union {
        CommandFunc c_func;    // C function pointer
        char *scheme_expr;     // Scheme expression to evaluate
    } func;
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
void addSchemeCommand(Commands *cmds, const char *name, const char *description, const char *scheme_expr);

#endif // COMMANDS_H

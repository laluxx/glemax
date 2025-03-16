#ifndef COMMANDS_H
#define COMMANDS_H

#include <stddef.h>
#include <libguile.h>
#include "buffer.h"

// Function pointer types for commands
typedef void (*VoidFunc)();
typedef void (*BufferFunc)(Buffer *);
typedef void (*BufferManagerFunc)(BufferManager*);
typedef void (*WindowManagerFunc)(WindowManager*);
typedef void (*WindowManagerParametersFunc)(WindowManager*, WindowParameters*);

typedef enum {
    CMD_TYPE_C_VOID,                     // C function with no arguments
    CMD_TYPE_C_BUFFER,                   // C function that takes a Buffer*
    CMD_TYPE_C_BUFFERMANAGER,            // C function that takes a BufferManager*
    CMD_TYPE_C_WINDOWMANAGER,            // C function that takes a WindowManager*
    CMD_TYPE_C_WINDOWMANAGERPARAMETERS,  // C function that takes a WindowManager*
    CMD_TYPE_SCHEME
} CommandType;

typedef struct {
    char *name;
    char *description;
    CommandType type;
    union {
        VoidFunc          void_func;          // C function with no arguments
        BufferFunc        buffer_func;        // C function with Buffer* argument
        BufferManagerFunc bufferManager_func; // C function with BufferManager* argument
        WindowManagerFunc windowManager_func; // C function with WindowManager* argument
        WindowManagerParametersFunc windowManagerParameters_func; // C function with WindowManager* argument
        char              *scheme_expr;       // Scheme expression to evaluate
    } func;
} Command;

typedef struct {
    Command *commands; // Dynamic array of commands
    size_t size;       // Number of commands in the array
    size_t capacity;   // Current capacity of the array
} Commands;

extern Commands commands;

void initCommands();

void addSchemeCommand(       Commands *cmds, const char *name, const char *description, const char *scheme_expr);
void addVoidCommand(         Commands *cmds, const char *name, const char *description, VoidFunc func);
void addBufferCommand(       Commands *cmds, const char *name, const char *description, BufferFunc func);
void addBufferManagerCommand(Commands *cmds, const char *name, const char *description, BufferManagerFunc func);
void addWindowManagerCommand(Commands *cmds, const char *name, const char *description, WindowManagerFunc func);
void addWindowManagerParametersCommand(Commands *cmds, const char *name, const char *description, WindowManagerParametersFunc func);



void addCommand(Commands *cmds, const char *name, const char *description,
                CommandType type, void *func);

void executeCommand(const char *name);
void executeBufferCommand(const char *name, Buffer *buffer);
void executeBufferArgCommand(const char *name, Buffer *buffer, int arg);
void executeBufferManagerCommand(const char *name, BufferManager *manager);
void executeWindowManagerCommand(const char *name, WindowManager *manager);
void executeWindowManagerParametersCommand(const char *name, WindowManager *manager, WindowParameters parameters);

void freeCommands(Commands *cmds);

// COMMANDS

void view_echo_area_messages();


#endif // COMMANDS_H

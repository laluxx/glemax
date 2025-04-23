#ifndef COMMANDS_H
#define COMMANDS_H

#include <stddef.h>
#include <libguile.h>
#include "buffer.h"

// Function pointer types
typedef void (*VoidFunc)();
typedef void (*BufferFunc)(Buffer*);
typedef void (*BufferManagerFunc)(BufferManager*);
typedef void (*WindowManagerFunc)(WindowManager*);
typedef void (*WindowManagerParametersFunc)(WindowManager*, WindowParameters*);
typedef void (*BufferShiftArgFunc)(Buffer*, bool, int);
typedef void (*WindowShiftArgFunc)(Window*, bool, int);

/* typedef void (*DiredFunc)(Dired*); // TODO */

typedef enum {
    CMD_TYPE_C_VOID,
    CMD_TYPE_C_BUFFER,
    CMD_TYPE_C_BUFFERMANAGER,
    CMD_TYPE_C_WINDOWMANAGER,
    CMD_TYPE_C_WINDOWMANAGERPARAMETERS,
    CMD_TYPE_C_BUFFERSHIFTARG,
    CMD_TYPE_C_WINDOWSHIFTARG,
    CMD_TYPE_SCHEME
} CommandType;

typedef struct {
    char *name;
    char *description;
    CommandType type;
    union {
        VoidFunc          void_func;
        BufferFunc        buffer_func;
        BufferManagerFunc bufferManager_func;
        WindowManagerFunc windowManager_func;
        WindowManagerParametersFunc windowManagerParameters_func;
        BufferShiftArgFunc bufferShiftArg_func;
        WindowShiftArgFunc windowShiftArg_func;
        char              *scheme_expr;
    } func;
} Command;

typedef struct {
    Command *commands; // Dynamic array of commands
    size_t size;       // Number of commands in the array
    size_t capacity;   // Current capacity of the array
} Commands;

extern Commands commands;

void initCommands();

void addSchemeCommand(          Commands *cmds, const char *name, const char *description, const char *scheme_expr);
void addVoidCommand(            Commands *cmds, const char *name, const char *description, VoidFunc func);
void addBufferCommand(          Commands *cmds, const char *name, const char *description, BufferFunc func);
void addBufferManagerCommand(   Commands *cmds, const char *name, const char *description, BufferManagerFunc func);
void addWindowManagerCommand(   Commands *cmds, const char *name, const char *description, WindowManagerFunc func);
void addWindowManagerParametersCommand(Commands *cmds, const char *name, const char *description, WindowManagerParametersFunc func);
void addBufferShiftArgCommand(  Commands *cmds, const char *name, const char *description, BufferShiftArgFunc func);
void addWindowShiftArgCommand(  Commands *cmds, const char *name, const char *description, WindowShiftArgFunc func);


void addCommand(Commands *cmds, const char *name, const char *description,
                CommandType type, void *func);

void executeCommand(const char *name);
void executeBufferCommand(const char *name, Buffer *buffer);
void executeBufferArgCommand(const char *name, Buffer *buffer, int arg);
void executeBufferManagerCommand(const char *name, BufferManager *manager);
void executeWindowManagerCommand(const char *name, WindowManager *manager);
void executeWindowManagerParametersCommand(const char *name, WindowManager *manager, WindowParameters parameters);
void executeBufferShiftArgCommand(const char *name, Buffer *buffer, bool shift, bool arg);
void executeWindowShiftArgCommand(const char *name, Window *window, bool shift, bool arg);


void freeCommands(Commands *cmds);


// COMMANDS

void view_echo_area_messages();
void minimap_mode(WindowManager* wm);
#endif // COMMANDS_H

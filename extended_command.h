/* #ifndef EXTENDED_COMMAND_H */
/* #define EXTENDED_COMMAND_H */

/* #include <stddef.h> */
/* #include <stdbool.h> */

/* typedef void (*CommandFunction)(void *context); // Generic function pointer with a void* context */

/* #define MAX_COMMANDS 100 */

/* typedef struct { */
/*     char *name;              // Command name */
/*     CommandFunction func;    // Function pointer */
/* } Command; */

/* typedef struct { */
/*     Command commands[MAX_COMMANDS];  // Array of commands */
/*     int count;                       // Number of commands registered */
/* } CommandRegistry; */

/* /\** */
/*  * Initialize the command registry. */
/*  * @param registry The command registry to initialize. */
/*  *\/ */
/* void init_command_registry(CommandRegistry *registry); */

/* /\** */
/*  * Register a command with a name and a function pointer. */
/*  * @param registry The command registry. */
/*  * @param name The name of the command. */
/*  * @param func The function pointer to the command. */
/*  *\/ */
/* void register_command(CommandRegistry *registry, const char *name, CommandFunction func); */

/* /\** */
/*  * Execute a command by its name, passing a generic context. */
/*  * @param registry The command registry. */
/*  * @param name The name of the command to execute. */
/*  * @param context A pointer to the context for the command. */
/*  *\/ */
/* void execute_command(CommandRegistry *registry, const char *name, void *context); */

/* #endif // EXTENDED_COMMAND_H */

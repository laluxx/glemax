/* #include <stdio.h> */
/* #include <string.h> */
/* #include <stdlib.h> */
/* #include "extended_command.h" */

/* void init_command_registry(CommandRegistry *registry) { */
/*     registry->count = 0; */
/* } */

/* void register_command(CommandRegistry *registry, const char *name, CommandFunction func, int arg_count, ArgType arg_types[]) { */
/*     if (registry->count >= MAX_COMMANDS) { */
/*         printf("Command registry is full.\n"); */
/*         return; */
/*     } */

/*     // Copy command name and function */
/*     registry->commands[registry->count].name = strdup(name); */
/*     registry->commands[registry->count].func = func; */
/*     registry->commands[registry->count].arg_count = arg_count; */

/*     // Copy argument types */
/*     for (int i = 0; i < arg_count; i++) { */
/*         registry->commands[registry->count].arg_types[i] = arg_types[i]; */
/*     } */

/*     registry->count++; */
/* } */

/* void execute_command(CommandRegistry *registry, const char *name, void *args[]) { */
/*     for (int i = 0; i < registry->count; i++) { */
/*         if (strcmp(registry->commands[i].name, name) == 0) { */
/*             // Here, we could optionally validate arguments based on arg_types */
/*             // (e.g., checking if args match expected types). */
/*             // For now, just call the function with the provided arguments */
/*             registry->commands[i].func(args);  // Call the function */
/*             return; */
/*         } */
/*     } */
/*     printf("Command not found: %s\n", name); */
/* } */

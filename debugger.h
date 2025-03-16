#ifndef DEBUGGER_H
#define DEBUGGER_H

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>
#include <signal.h>

#define MAX_BREAKPOINTS 256
#define MAX_STACK_FRAMES 128
#define MAX_VAR_NAME 64
#define MAX_BACKTRACE_DEPTH 50

typedef enum {
    DBG_EVENT_NONE,
    DBG_EVENT_BREAKPOINT,
    DBG_EVENT_SIGNAL,
    DBG_EVENT_EXIT,
    DBG_EVENT_ERROR,
    DBG_EVENT_STEP
} DebugEventType;

typedef enum {
    DBG_STATE_RUNNING,
    DBG_STATE_STOPPED,
    DBG_STATE_STEPPING,
    DBG_STATE_TERMINATED
} DebugState;

typedef struct {
    void *address;           // Memory address of the breakpoint
    uintptr_t offset;        // Offset from base of function
    unsigned char orig_byte; // Original byte at the breakpoint address
    bool enabled;            // Whether breakpoint is currently active
    char *function_name;     // Name of function containing breakpoint
    char *file_name;         // Source file of breakpoint
    int line_number;         // Line number in source file
    char *condition;         // Optional breakpoint condition expression
    uint64_t hit_count;      // Number of times breakpoint was hit
} Breakpoint;

#ifdef __x86_64__
typedef struct {
    uint64_t rax, rbx, rcx, rdx;
    uint64_t rsi, rdi, rbp, rsp;
    uint64_t r8, r9, r10, r11;
    uint64_t r12, r13, r14, r15;
    uint64_t rip, rflags;
} RegisterState;
#elif defined(__i386__)
typedef struct {
    uint32_t eax, ebx, ecx, edx;
    uint32_t esi, edi, ebp, esp;
    uint32_t eip, eflags;
} RegisterState;
#else
typedef struct {
    uint64_t registers[32];
    uint64_t pc;
    uint64_t flags;
} RegisterState;
#endif

typedef struct {
    void *pc;                /* Program counter */
    void *frame_pointer;     /* Frame pointer */
    char *function_name;     /* Function name (if available) */
    char *file_name;         /* Source file (if available) */
    int line_number;         /* Line number in source */
} StackFrame;

typedef struct {
    char name[MAX_VAR_NAME]; // Variable name
    void *address;           // Memory address
    enum {
        VAR_TYPE_INT,
        VAR_TYPE_LONG,
        VAR_TYPE_FLOAT,
        VAR_TYPE_DOUBLE,
        VAR_TYPE_CHAR,
        VAR_TYPE_STRING,
        VAR_TYPE_POINTER,
        VAR_TYPE_STRUCT,
        VAR_TYPE_UNKNOWN
    } type;
    union {
           int   int_val;
          long   long_val;
         float   float_val;
        double   double_val;
          char   char_val;
          char * string_val;
          void * pointer_val;
    } value;
    size_t size;             // Size in bytes
    bool is_local;           // Whether it's a local variable
} Variable;

typedef struct {
    DebugEventType type;     // Type of event
    int signal_num;          // Signal number (if signal event)
    Breakpoint *breakpoint;  // Breakpoint hit (if breakpoint event)
    char *error_message;     // Error message (if error event)
    int exit_code;           // Exit code (if exit event)
} DebugEvent;

typedef struct {
    // Core state
    DebugState state;                        // Current execution state
    bool initialized;                        // Whether debugger is initialized
    
    // Breakpoints
    Breakpoint breakpoints[MAX_BREAKPOINTS]; // Array of set breakpoints
    int breakpoint_count;                    // Number of active breakpoints
    
    // Signal handling
    struct sigaction old_sigaction[NSIG];    // Previous signal handlers
    sigset_t old_sigmask;                    // Previous signal mask

    // Registers
    RegisterState registers;                 // Current register state
    void *instruction_pointer;               // Current instruction pointer
    
    // Stack
    StackFrame call_stack[MAX_STACK_FRAMES]; // Current call stack
    int stack_depth;                         // Depth of current call stack
    
    // DSymbols
    bool has_symbols;                        /* Whether debug symbols are available */
    void *symbol_table;                      /* Pointer to symbol table information */
    
    // Variables
    Variable locals[MAX_BACKTRACE_DEPTH];    // Local variables
    int local_count;                         // Number of tracked local variables
    Variable *globals;                       // Global variables
    int global_count;                        // Number of tracked global variables
    
    // Events
    DebugEvent last_event;                   // Most recent debug event
    void (*event_callback)(DebugEvent *);    // Callback for debug events
    
    /* Memory info */
    uintptr_t text_start;                    /* Start of text segment */
    size_t text_size;                        /* Size of text segment */
    
    /* Source information */
    char **source_files;                     /* Array of source files */
    int source_file_count;                   /* Number of source files */
} Debugger;

// Register constants
#ifndef REG_RAX
// x86_64
#define REG_RAX 10
#define REG_RBX 5
#define REG_RCX 11
#define REG_RDX 12
#define REG_RSI 13
#define REG_RDI 14
#define REG_RBP 6
#define REG_RSP 15
#define REG_R8 8
#define REG_R9 9
#define REG_R10 16
#define REG_R11 17
#define REG_R12 18
#define REG_R13 19
#define REG_R14 20
#define REG_R15 21
#define REG_RIP 16
#define REG_EFL 49
#endif

#ifndef REG_EAX
// i386
#define REG_EAX 11
#define REG_EBX 8
#define REG_ECX 10
#define REG_EDX 9
#define REG_ESI 4
#define REG_EDI 5
#define REG_EBP 6
#define REG_ESP 7
#define REG_EIP 14
#define REG_EFL 17
#endif

bool initDebugger            (void);
void shutdownDebugger        (void);
 int setBreakpoint           (void *address);
 int setBreakpointByName     (const char *function_name);
 int setBreakpointByLocation (const char *file, int line);
bool removeBreakpoint        (int id);
bool enableBreakpoint        (int id, bool enabled);
bool setBreakpointCondition  (int id, const char *condition);
bool continueExecution       (void);
bool stepInstruction         (void);
bool stepLine                (void);
bool stepOver                (void);
bool stepOut                 (void);
 int getCallStack            (StackFrame *frames, int max_frames);
bool getVariableByName       (const char *name, Variable *var);
bool readMemory              (void *address, void *buffer, size_t size);
bool writeMemory             (void *address, const void *buffer, size_t size);
 int getLocalVariables       (Variable *vars, int max_vars);
 int getGlobalVariables      (Variable *vars, int max_vars);
bool evaluateExpression      (const char *expression, char *result, size_t size);
void setEventCallback        (void (*callback)(DebugEvent *));
void getDebuggerState        (Debugger *debugger);
bool getSourceLocation       (void *address, char *file, size_t file_size, int *line);
bool loadDebugSymbols        (const char *filename);

#endif /* DEBUGGER_H */

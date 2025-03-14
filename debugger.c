#define _GNU_SOURCE
#include "debugger.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <execinfo.h>
#include <dlfcn.h>        // For dladdr and Dl_info
#include <ucontext.h>     // For ucontext_t
#include <sys/ucontext.h> // For register definitions

static Debugger g_debugger = {0}; // NOTE Global debugger

// Forward declarations for static methods
static void handleSigtrap(int signo, siginfo_t *info, void *context);
static void reinstallBreakpoint(int id);
static bool getSymbolInfo(void *addr, char **function_name, char **file_name, int *line);
static void captureBacktrace(void);
static bool readProcessMemory(void *addr, void *buffer, size_t size);
static bool writeProcessMemory(void *addr, const void *buffer, size_t size);
static void analyzeCurrentPosition(void);
static void fillRegisterState(RegisterState *regs, void *context);
static void collectLocalVariables(void);
static bool isAddressExecutable(void *addr);
static void raiseDebugEvent(DebugEventType type, Breakpoint *bp, int signal_num, const char *error, int exit_code);
static void *findFunctionByName(const char *name);
static void handleDebugSignal(int signo, siginfo_t *info, void *context);
static void calculateTextSegmentBounds(void);
static bool parseDebugLine(const char *line, void **addr, char **func, char **file, int *lineno);

bool initDebugger(void) {
    if (g_debugger.initialized) {
        return true;
    }
    
    memset(&g_debugger, 0, sizeof(Debugger));
    g_debugger.state = DBG_STATE_RUNNING;
    
    /* Save current signal mask */
    sigset_t current_mask;
    if (sigprocmask(SIG_SETMASK, NULL, &current_mask) != 0) {
        return false;
    }
    g_debugger.old_sigmask = current_mask;
    
    /* Install signal handlers */
    struct sigaction act;
    memset(&act, 0, sizeof(act));
    act.sa_sigaction = handleDebugSignal;
    act.sa_flags = SA_SIGINFO | SA_RESTART;
    
    int signals[] = {
        SIGTRAP, SIGSEGV, SIGILL, SIGFPE, SIGABRT, SIGBUS, 0
    };
    
    for (int i = 0; signals[i]; i++) {
        struct sigaction old_act;
        if (sigaction(signals[i], &act, &old_act) != 0) {
            return false;
        }
        g_debugger.old_sigaction[signals[i]] = old_act;
    }
    
    calculateTextSegmentBounds();
    g_debugger.has_symbols = loadDebugSymbols(NULL);
    g_debugger.initialized = true;
    
    return true;
}

void shutdownDebugger(void) {
    if (!g_debugger.initialized) {
        return;
    }
    
    /* Remove all breakpoints */
    for (int i = 0; i < g_debugger.breakpoint_count; i++) {
        if (g_debugger.breakpoints[i].enabled) {
            void *addr = g_debugger.breakpoints[i].address;
            writeProcessMemory(addr, &g_debugger.breakpoints[i].orig_byte, 1);
        }
    }
    
    /* Restore original signal handlers */
    int signals[] = {
        SIGTRAP, SIGSEGV, SIGILL, SIGFPE, SIGABRT, SIGBUS, 0
    };
    
    for (int i = 0; signals[i]; i++) {
        sigaction(signals[i], &g_debugger.old_sigaction[signals[i]], NULL);
    }
    
    sigprocmask(SIG_SETMASK, &g_debugger.old_sigmask, NULL);
    
    /* Free allocated memory */
    for (int i = 0; i < g_debugger.breakpoint_count; i++) {
        free(g_debugger.breakpoints[i].function_name);
        free(g_debugger.breakpoints[i].file_name);
        free(g_debugger.breakpoints[i].condition);
    }
    
    for (int i = 0; i < g_debugger.stack_depth; i++) {
        free(g_debugger.call_stack[i].function_name);
        free(g_debugger.call_stack[i].file_name);
    }
    
    for (int i = 0; i < g_debugger.source_file_count; i++) {
        free(g_debugger.source_files[i]);
    }
    free(g_debugger.source_files);
    free(g_debugger.globals);
    
    memset(&g_debugger, 0, sizeof(Debugger));
}

int setBreakpoint(void *address) {
    if (!g_debugger.initialized || g_debugger.breakpoint_count >= MAX_BREAKPOINTS) {
        return -1;
    }
    
    if (!isAddressExecutable(address)) {
        return -1;
    }
    
    /* Check if we already have a breakpoint at this address */
    for (int i = 0; i < g_debugger.breakpoint_count; i++) {
        if (g_debugger.breakpoints[i].address == address) {
            if (!g_debugger.breakpoints[i].enabled) {
                unsigned char int3 = 0xCC;
                if (writeProcessMemory(address, &int3, 1)) {
                    g_debugger.breakpoints[i].enabled = true;
                    return i;
                }
            }
            return i;
        }
    }
    
    /* Create new breakpoint */
    int id = g_debugger.breakpoint_count++;
    Breakpoint *bp = &g_debugger.breakpoints[id];
    
    bp->address = address;
    bp->enabled = false;
    bp->hit_count = 0;
    
    /* Read the original byte at the address */
    if (!readProcessMemory(address, &bp->orig_byte, 1)) {
        g_debugger.breakpoint_count--;
        return -1;
    }
    
    /* Replace with INT 3 instruction (0xCC) */
    unsigned char int3 = 0xCC;
    if (!writeProcessMemory(address, &int3, 1)) {
        g_debugger.breakpoint_count--;
        return -1;
    }
    
    bp->enabled = true;
    getSymbolInfo(address, &bp->function_name, &bp->file_name, &bp->line_number);
    
    return id;
}

int setBreakpointByName(const char *function_name) {
    if (!g_debugger.initialized || !g_debugger.has_symbols) {
        return -1;
    }
    
    void *addr = findFunctionByName(function_name);
    if (!addr) {
        return -1;
    }
    
    return setBreakpoint(addr);
}

int setBreakpointByLocation(const char *file, int line) {
    if (!g_debugger.initialized || !g_debugger.has_symbols) {
        return -1;
    }
    
    /* Search through our preloaded source location information */
    for (void *addr = (void*)g_debugger.text_start; 
         addr < (void*)(g_debugger.text_start + g_debugger.text_size); 
         addr += 1) {
        
        char file_buffer[256];
        int line_num = 0;
        
        if (getSourceLocation(addr, file_buffer, sizeof(file_buffer), &line_num)) {
            if (line_num == line && strstr(file_buffer, file) != NULL) {
                return setBreakpoint(addr);
            }
        }
    }
    
    return -1;
}

bool removeBreakpoint(int id) {
    if (!g_debugger.initialized || id < 0 || id >= g_debugger.breakpoint_count) {
        return false;
    }
    
    Breakpoint *bp = &g_debugger.breakpoints[id];
    if (bp->enabled) {
        /* Restore original byte */
        if (!writeProcessMemory(bp->address, &bp->orig_byte, 1)) {
            return false;
        }
        bp->enabled = false;
    }
    
    return true;
}

bool enableBreakpoint(int id, bool enabled) {
    if (!g_debugger.initialized || id < 0 || id >= g_debugger.breakpoint_count) {
        return false;
    }
    
    Breakpoint *bp = &g_debugger.breakpoints[id];
    
    if (bp->enabled == enabled) {
        return true;
    }
    
    if (enabled) {
        unsigned char int3 = 0xCC;
        if (!writeProcessMemory(bp->address, &int3, 1)) {
            return false;
        }
    } else {
        if (!writeProcessMemory(bp->address, &bp->orig_byte, 1)) {
            return false;
        }
    }
    
    bp->enabled = enabled;
    return true;
}

bool setBreakpointCondition(int id, const char *condition) {
    if (!g_debugger.initialized || id < 0 || id >= g_debugger.breakpoint_count) {
        return false;
    }
    
    Breakpoint *bp = &g_debugger.breakpoints[id];
    
    free(bp->condition);
    
    if (condition && *condition) {
        bp->condition = strdup(condition);
        if (!bp->condition) {
            return false;
        }
    } else {
        bp->condition = NULL;
    }
    
    return true;
}

bool continueExecution(void) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return false;
    }
    
    for (int i = 0; i < g_debugger.breakpoint_count; i++) {
        Breakpoint *bp = &g_debugger.breakpoints[i];
        if (bp->enabled && bp->address == g_debugger.instruction_pointer) {
            writeProcessMemory(bp->address, &bp->orig_byte, 1);
            reinstallBreakpoint(i);
            break;
        }
    }
    
    g_debugger.state = DBG_STATE_RUNNING;
    return true;
}

bool stepInstruction(void) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return false;
    }
    
#ifdef __x86_64__
    g_debugger.registers.rflags |= 0x100;  /* Set TF bit */
#endif
    
    g_debugger.state = DBG_STATE_STEPPING;
    
    for (int i = 0; i < g_debugger.breakpoint_count; i++) {
        Breakpoint *bp = &g_debugger.breakpoints[i];
        if (bp->enabled && bp->address == g_debugger.instruction_pointer) {
            writeProcessMemory(bp->address, &bp->orig_byte, 1);
            break;
        }
    }
    
    return true;
}

bool stepLine(void) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return false;
    }
    
    if (!g_debugger.has_symbols) {
        return stepInstruction();
    }
    
    char current_file[256];
    int current_line = 0;
    
    if (!getSourceLocation(g_debugger.instruction_pointer, 
                           current_file, sizeof(current_file), &current_line)) {
        return stepInstruction();
    }
    
    g_debugger.state = DBG_STATE_STEPPING;
    return true;
}

bool stepOver(void) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return false;
    }
    
    unsigned char insn;
    if (readProcessMemory(g_debugger.instruction_pointer, &insn, 1)) {
#ifdef __x86_64__
        if (insn == 0xE8) {  /* Call instruction on x86_64 */
            void *next_insn = (void*)((uintptr_t)g_debugger.instruction_pointer + 5);
            int bp_id = setBreakpoint(next_insn);
            if (bp_id >= 0) {
                g_debugger.breakpoints[bp_id].hit_count = 1;
            }
        }
#endif
    }
    
    g_debugger.state = DBG_STATE_RUNNING;
    return true;
}

bool stepOut(void) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return false;
    }
    
    void *return_addr = NULL;
    
#ifdef __x86_64__
    void *frame_ptr = (void*)g_debugger.registers.rbp;
    if (frame_ptr && readProcessMemory(frame_ptr + 8, &return_addr, sizeof(return_addr))) {
        int bp_id = setBreakpoint(return_addr);
        if (bp_id >= 0) {
            g_debugger.breakpoints[bp_id].hit_count = 1;
        }
    }
#endif
    
    g_debugger.state = DBG_STATE_RUNNING;
    return true;
}

int getCallStack(StackFrame *frames, int max_frames) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return 0;
    }
    
    captureBacktrace();
    
    int count = (g_debugger.stack_depth < max_frames) ? 
        g_debugger.stack_depth : max_frames;
    
    for (int i = 0; i < count; i++) {
        frames[i] = g_debugger.call_stack[i];
    }
    
    return count;
}

bool getVariableByName(const char *name, Variable *var) {
    if (!g_debugger.initialized || !name || !var) {
        return false;
    }
    
    /* Look for variable in locals */
    for (int i = 0; i < g_debugger.local_count; i++) {
        if (strcmp(g_debugger.locals[i].name, name) == 0) {
            *var = g_debugger.locals[i];
            return true;
        }
    }
    
    /* Look for variable in globals */
    for (int i = 0; i < g_debugger.global_count; i++) {
        if (strcmp(g_debugger.globals[i].name, name) == 0) {
            *var = g_debugger.globals[i];
            return true;
        }
    }
    
    return false;
}

bool readMemory(void *address, void *buffer, size_t size) {
    return readProcessMemory(address, buffer, size);
}

bool writeMemory(void *address, const void *buffer, size_t size) {
    return writeProcessMemory(address, buffer, size);
}

int getLocalVariables(Variable *vars, int max_vars) {
    if (!g_debugger.initialized || g_debugger.state != DBG_STATE_STOPPED) {
        return 0;
    }
    
    collectLocalVariables();
    
    int count = (g_debugger.local_count < max_vars) ? 
        g_debugger.local_count : max_vars;
    
    for (int i = 0; i < count; i++) {
        vars[i] = g_debugger.locals[i];
    }
    
    return count;
}

int getGlobalVariables(Variable *vars, int max_vars) {
    if (!g_debugger.initialized) {
        return 0;
    }
    
    int count = (g_debugger.global_count < max_vars) ? 
        g_debugger.global_count : max_vars;
    
    for (int i = 0; i < count; i++) {
        vars[i] = g_debugger.globals[i];
    }
    
    return count;
}

bool evaluateExpression(const char *expression, char *result, size_t size) {
    /* This would require a mini-interpreter to implement properly.
       For now, we'll return a stub implementation. */
    if (!g_debugger.initialized || !expression || !result || size == 0) {
        return false;
    }
    
    snprintf(result, size, "Cannot evaluate expression: %s", expression);
    return false;
}

void setEventCallback(void (*callback)(DebugEvent *)) {
    if (!g_debugger.initialized) {
        return;
    }
    
    g_debugger.event_callback = callback;
}

void getDebuggerState(Debugger *debugger) {
    if (!g_debugger.initialized || !debugger) {
        return;
    }
    
    *debugger = g_debugger;
}

bool getSourceLocation(void *address, char *file, size_t file_size, int *line) {
    if (!g_debugger.initialized || !g_debugger.has_symbols || !file || !line) {
        return false;
    }
    
    /* This would typically use debug info (DWARF) to resolve the location.
       For a simplified implementation: */
    char *func_name = NULL;
    char *file_name = NULL;
    
    if (getSymbolInfo(address, &func_name, &file_name, line)) {
        if (file_name) {
            strncpy(file, file_name, file_size - 1);
            file[file_size - 1] = '\0';
            free(func_name);
            free(file_name);
            return true;
        }
        free(func_name);
        free(file_name);
    }
    
    return false;
}

bool loadDebugSymbols(const char *filename) {
    if (!g_debugger.initialized) {
        return false;
    }
    
    /* For a real implementation, this would parse DWARF debug info.
       For a simplified implementation, we'll just check if debug info exists. */
    void *handle = dlopen(NULL, RTLD_LAZY);
    if (!handle) {
        return false;
    }
    
    g_debugger.symbol_table = handle;
    return true;
}

/* Internal helper functions */

static void handleDebugSignal(int signo, siginfo_t *info, void *context) {
    if (!g_debugger.initialized) {
        /* Pass to original handler */
        if (g_debugger.old_sigaction[signo].sa_flags & SA_SIGINFO) {
            g_debugger.old_sigaction[signo].sa_sigaction(signo, info, context);
        } else if (g_debugger.old_sigaction[signo].sa_handler != SIG_IGN &&
                   g_debugger.old_sigaction[signo].sa_handler != SIG_DFL) {
            g_debugger.old_sigaction[signo].sa_handler(signo);
        }
        return;
    }
    
    /* Update register state */
    fillRegisterState(&g_debugger.registers, context);
    
    if (signo == SIGTRAP) {
        /* Handle breakpoint or single-step */
        handleSigtrap(signo, info, context);
    } else {
        /* Other signals (crashes, etc.) */
        g_debugger.state = DBG_STATE_STOPPED;
        g_debugger.instruction_pointer = (void*)g_debugger.registers.rip;
        analyzeCurrentPosition();
        raiseDebugEvent(DBG_EVENT_SIGNAL, NULL, signo, NULL, 0);
    }
}

static void handleSigtrap(int signo, siginfo_t *info, void *context) {
    /* Use the address from the context */
#ifdef __x86_64__
    void *addr = (void*)g_debugger.registers.rip;
#else
    void *addr = info->si_addr;
#endif
    
    g_debugger.instruction_pointer = addr;
    
    /* Check if this is a breakpoint */
    Breakpoint *bp = NULL;
    for (int i = 0; i < g_debugger.breakpoint_count; i++) {
        /* On x86, the IP will point after the int3 instruction */
        void *bp_addr = g_debugger.breakpoints[i].address;
#ifdef __x86_64__
        if (bp_addr == (void*)((uintptr_t)addr - 1)) {
            /* Adjust IP back to breakpoint address */
            g_debugger.registers.rip = (uint64_t)bp_addr;
            g_debugger.instruction_pointer = bp_addr;
            bp = &g_debugger.breakpoints[i];
            bp->hit_count++;
            break;
        }
#else
        if (bp_addr == addr) {
            bp = &g_debugger.breakpoints[i];
            bp->hit_count++;
            break;
        }
#endif
    }
    
    g_debugger.state = DBG_STATE_STOPPED;
    analyzeCurrentPosition();
    
    if (bp) {
        /* Check if breakpoint should be removed after being hit (temporary) */
        if (bp->hit_count == 1 && bp->condition == NULL) {
            removeBreakpoint(bp - g_debugger.breakpoints);
        }
        
        raiseDebugEvent(DBG_EVENT_BREAKPOINT, bp, 0, NULL, 0);
    } else {
        /* Single-step or other trap */
        raiseDebugEvent(DBG_EVENT_STEP, NULL, 0, NULL, 0);
    }
}

static void reinstallBreakpoint(int id) {
    /* This would normally set up single-step mode to get past the breakpoint
       and then reinstall it. Simplified implementation: */
    
    Breakpoint *bp = &g_debugger.breakpoints[id];
    unsigned char int3 = 0xCC;
    
    /* Simulate advancing past the instruction */
    g_debugger.instruction_pointer = (void*)((uintptr_t)bp->address + 1);
    
    /* Reinstall breakpoint */
    writeProcessMemory(bp->address, &int3, 1);
}

static bool getSymbolInfo(void *addr, char **function_name, char **file_name, int *line) {
    /* In a real implementation, this would use debug info.
       For a simplified implementation: */
    Dl_info info;
    if (dladdr(addr, &info) && info.dli_sname) {
        *function_name = strdup(info.dli_sname);
        *file_name = NULL;
        *line = 0;
        return true;
    }
    
    *function_name = NULL;
    *file_name = NULL;
    *line = 0;
    return false;
}

static void captureBacktrace(void) {
    /* Capture the backtrace */
    void *frames[MAX_STACK_FRAMES];
    int frame_count = backtrace(frames, MAX_STACK_FRAMES);
    
    /* Clear existing stack info */
    for (int i = 0; i < g_debugger.stack_depth; i++) {
        free(g_debugger.call_stack[i].function_name);
        free(g_debugger.call_stack[i].file_name);
    }
    
    g_debugger.stack_depth = 0;
    
    for (int i = 0; i < frame_count && i < MAX_STACK_FRAMES; i++) {
        g_debugger.call_stack[i].pc = frames[i];
        g_debugger.call_stack[i].frame_pointer = NULL;
        
        /* Get symbol information for this frame */
        char *func_name = NULL;
        char *file_name = NULL;
        int line_num = 0;
        
        if (getSymbolInfo(frames[i], &func_name, &file_name, &line_num)) {
            g_debugger.call_stack[i].function_name = func_name;
            g_debugger.call_stack[i].file_name = file_name;
            g_debugger.call_stack[i].line_number = line_num;
        } else {
            g_debugger.call_stack[i].function_name = NULL;
            g_debugger.call_stack[i].file_name = NULL;
            g_debugger.call_stack[i].line_number = 0;
        }
        
        g_debugger.stack_depth++;
    }
}

static bool readProcessMemory(void *addr, void *buffer, size_t size) {
    /* For self-debugging, we can directly read memory */
    if (!buffer || !addr) {
        return false;
    }
    
    /* Check if address is valid and accessible */
    if (!isAddressExecutable(addr) && mprotect(
                                               (void *)((uintptr_t)addr & ~(getpagesize() - 1)),
                                               getpagesize(),
                                               PROT_READ) != 0) {
        return false;
    }
    
    memcpy(buffer, addr, size);
    return true;
}

static bool writeProcessMemory(void *addr, const void *buffer, size_t size) {
    /* For self-debugging, we need to make memory writable first */
    if (!buffer || !addr) {
        return false;
    }
    
    void *page_addr = (void *)((uintptr_t)addr & ~(getpagesize() - 1));
    
    /* Make the page writable */
    if (mprotect(page_addr, getpagesize(), PROT_READ | PROT_WRITE | PROT_EXEC) != 0) {
        return false;
    }
    
    /* Write the data */
    memcpy(addr, buffer, size);
    
    /* Restore the original protection */
    if (isAddressExecutable(addr)) {
        mprotect(page_addr, getpagesize(), PROT_READ | PROT_EXEC);
    } else {
        mprotect(page_addr, getpagesize(), PROT_READ);
    }
    
    return true;
}

static void analyzeCurrentPosition(void) {
    captureBacktrace();
    collectLocalVariables();
}

static void fillRegisterState(RegisterState *regs, void *context) {
    if (!regs || !context) {
        return;
    }

#ifdef __x86_64__
    ucontext_t *uc = (ucontext_t *)context;

    /* Warning: These offsets are system-dependent */
    /* It's better to use the system's REG_* definitions if available */
    /* A more portable approach would be to use ptrace or a platform-specific API */

    /* Try to use mcontext directly if the structure is accessible */
#if defined(__linux__)
#if defined(__GLIBC__)
    regs->rax = uc->uc_mcontext.gregs[REG_RAX];
    regs->rbx = uc->uc_mcontext.gregs[REG_RBX];
    regs->rcx = uc->uc_mcontext.gregs[REG_RCX];
    regs->rdx = uc->uc_mcontext.gregs[REG_RDX];
    regs->rsi = uc->uc_mcontext.gregs[REG_RSI];
    regs->rdi = uc->uc_mcontext.gregs[REG_RDI];
    regs->rbp = uc->uc_mcontext.gregs[REG_RBP];
    regs->rsp = uc->uc_mcontext.gregs[REG_RSP];
    regs->r8 = uc->uc_mcontext.gregs[REG_R8];
    regs->r9 = uc->uc_mcontext.gregs[REG_R9];
    regs->r10 = uc->uc_mcontext.gregs[REG_R10];
    regs->r11 = uc->uc_mcontext.gregs[REG_R11];
    regs->r12 = uc->uc_mcontext.gregs[REG_R12];
    regs->r13 = uc->uc_mcontext.gregs[REG_R13];
    regs->r14 = uc->uc_mcontext.gregs[REG_R14];
    regs->r15 = uc->uc_mcontext.gregs[REG_R15];
    regs->rip = uc->uc_mcontext.gregs[REG_RIP];
    regs->rflags = uc->uc_mcontext.gregs[REG_EFL];
#else
    /* Fallback for non-glibc Linux */
    memset(regs, 0, sizeof(RegisterState));
#endif
#else
    /* Fallback for non-Linux systems */
    memset(regs, 0, sizeof(RegisterState));
#endif
#elif defined(__i386__)
    ucontext_t *uc = (ucontext_t *)context;

#if defined(__linux__)
#if defined(__GLIBC__)
    regs->eax = uc->uc_mcontext.gregs[REG_EAX];
    regs->ebx = uc->uc_mcontext.gregs[REG_EBX];
    regs->ecx = uc->uc_mcontext.gregs[REG_ECX];
    regs->edx = uc->uc_mcontext.gregs[REG_EDX];
    regs->esi = uc->uc_mcontext.gregs[REG_ESI];
    regs->edi = uc->uc_mcontext.gregs[REG_EDI];
    regs->ebp = uc->uc_mcontext.gregs[REG_EBP];
    regs->esp = uc->uc_mcontext.gregs[REG_ESP];
    regs->eip = uc->uc_mcontext.gregs[REG_EIP];
    regs->eflags = uc->uc_mcontext.gregs[REG_EFL];
#else
    memset(regs, 0, sizeof(RegisterState));
#endif
#else
    memset(regs, 0, sizeof(RegisterState));
#endif
#else
    /* For other architectures, initialize with zeros */
    memset(regs, 0, sizeof(RegisterState));
#endif
}

static void collectLocalVariables(void) {
    /* In a real implementation, this would use debug info to identify 
       and collect local variables. For our simplified implementation,
       we'll just clear the array. */
    g_debugger.local_count = 0;
    
    /* With full DWARF debug info, we could walk the stack frames,
       and extract variable locations and types */
}

static bool isAddressExecutable(void *addr) {
    if (!addr) {
        return false;
    }
    
    /* Check if the address is within the text segment */
    if ((uintptr_t)addr >= g_debugger.text_start && 
        (uintptr_t)addr < g_debugger.text_start + g_debugger.text_size) {
        return true;
    }
    
    /* We can also check /proc/self/maps for more detailed memory layout */
    FILE *maps = fopen("/proc/self/maps", "r");
    if (!maps) {
        return false;
    }
    
    char line[256];
    bool executable = false;
    
    while (fgets(line, sizeof(line), maps)) {
        unsigned long start, end;
        char permissions[5];
        
        if (sscanf(line, "%lx-%lx %4s", &start, &end, permissions) == 3) {
            if ((uintptr_t)addr >= start && (uintptr_t)addr < end) {
                if (strchr(permissions, 'x')) {
                    executable = true;
                }
                break;
            }
        }
    }
    
    fclose(maps);
    return executable;
}

static void raiseDebugEvent(DebugEventType type, Breakpoint *bp, int signal_num, 
                            const char *error, int exit_code) {
    DebugEvent event;
    
    event.type = type;
    event.signal_num = signal_num;
    event.breakpoint = bp;
    event.exit_code = exit_code;
    
    if (error) {
        event.error_message = strdup(error);
    } else {
        event.error_message = NULL;
    }
    
    if (g_debugger.event_callback) {
        g_debugger.event_callback(&event);
    }
    
    free(event.error_message);
}

static void *findFunctionByName(const char *name) {
    if (!g_debugger.initialized || !name) {
        return NULL;
    }
    
    void *handle = dlopen(NULL, RTLD_LAZY);
    if (!handle) {
        return NULL;
    }
    
    void *addr = dlsym(handle, name);
    dlclose(handle);
    
    return addr;
}

static void calculateTextSegmentBounds(void) {
    /* Read memory map to find text segment */
    FILE *maps = fopen("/proc/self/maps", "r");
    if (!maps) {
        return;
    }
    
    char line[256];
    while (fgets(line, sizeof(line), maps)) {
        unsigned long start, end;
        char permissions[5];
        char path[256] = {0};
        
        /* Try to parse the line */
        int matched = sscanf(line, "%lx-%lx %4s %*s %*s %*s %255s", 
                             &start, &end, permissions, path);
        
        if (matched >= 3 && strchr(permissions, 'x')) {
            /* This is an executable segment */
            if (matched == 4 && (strstr(path, "/lib/") == NULL)) {
                /* This is likely our main executable */
                g_debugger.text_start = start;
                g_debugger.text_size = end - start;
                break;
            } else if (g_debugger.text_start == 0) {
                /* If we don't find a better match, use the first executable segment */
                g_debugger.text_start = start;
                g_debugger.text_size = end - start;
            }
        }
    }
    
    fclose(maps);
}

static bool parseDebugLine(const char *line, void **addr, char **func, char **file, int *lineno) {
    /* This would parse a line containing debug information.
       Format depends on the specific debug info source. */
    if (!line || !addr || !func || !file || !lineno) {
        return false;
    }
    
    /* Parse something like: "0x12345678 functionName at file.c:123" */
    uintptr_t address;
    char function_name[256] = {0};
    char file_name[256] = {0};
    
    if (sscanf(line, "%lx %255s at %255s:%d", 
               &address, function_name, file_name, lineno) == 4) {
        *addr = (void*)address;
        *func = strdup(function_name);
        *file = strdup(file_name);
        return true;
    }
    
    return false;
}


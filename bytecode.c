#include "bytecode.h"
#include "syntax.h" // Compilation.h later..
#include "buffer.h"
#include "wm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <ctype.h>

#define MAX_BYTECODE_LEN 4096 // Bytes

int hex_char_to_val(char c) {
    if ('0' <= c && c <= '9') return c - '0';
    if ('a' <= c && c <= 'f') return 10 + c - 'a';
    if ('A' <= c && c <= 'F') return 10 + c - 'A';
    return -1;
}

int parse_bytecodecode(const char *input, unsigned char *out) {
    int len = 0;
    const char *p = input;
    while (*p && len < MAX_BYTECODE_LEN) {
        // Skip whitespace and common hex notation prefixes
        while (*p && (isspace(*p) || *p == '\\' || *p == 'x' || *p == 'X')) p++;
        
        // Skip '0x' or '0X' prefix if present
        if (*p == '0' && (*(p+1) == 'x' || *(p+1) == 'X')) p += 2;
        
        // Stop if we don't have two hex digits
        if (!isxdigit(p[0]) || !isxdigit(p[1])) break;
        
        // Parse the two hex digits into a byte
        int hi = hex_char_to_val(p[0]);
        int lo = hex_char_to_val(p[1]);
        if (hi < 0 || lo < 0) break;
        
        out[len++] = (hi << 4) | lo;
        p += 2;
    }
    return len;
}

int execute_bytecode_binary(const unsigned char *bytecode, int sc_len, 
                             Buffer *outputBuffer, bool capture) {
    if (!bytecode || sc_len <= 0) return -1;
    
    // If capturing output, we need to set up pipes
    int stdout_pipe[2] = {-1, -1};
    int stderr_pipe[2] = {-1, -1};
    int saved_stdout = -1;
    int saved_stderr = -1;
    
    if (capture && outputBuffer) {
        if (pipe(stdout_pipe) != 0 || pipe(stderr_pipe) != 0) {
            if (outputBuffer) {
                appendToBuffer(outputBuffer, "Failed to create pipes for output capture.\n");
            }
            return -1;
        }
        
        // Save original stdout/stderr
        saved_stdout = dup(STDOUT_FILENO);
        saved_stderr = dup(STDERR_FILENO);
        
        if (saved_stdout == -1 || saved_stderr == -1) {
            if (outputBuffer) {
                appendToBuffer(outputBuffer, "Failed to duplicate file descriptors.\n");
            }
            
            // Clean up
            if (saved_stdout != -1) close(saved_stdout);
            if (saved_stderr != -1) close(saved_stderr);
            close(stdout_pipe[0]);
            close(stdout_pipe[1]);
            close(stderr_pipe[0]);
            close(stderr_pipe[1]);
            
            return -1;
        }
    }
    
    // Allocate executable memory
    void *mem = mmap(NULL, sc_len, PROT_READ | PROT_WRITE | PROT_EXEC,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to allocate executable memory.\n");
        }
        
        // Clean up
        if (capture && outputBuffer) {
            if (saved_stdout != -1) close(saved_stdout);
            if (saved_stderr != -1) close(saved_stderr);
            close(stdout_pipe[0]);
            close(stdout_pipe[1]);
            close(stderr_pipe[0]);
            close(stderr_pipe[1]);
        }
        
        return -1;
    }
    
    // Copy bytecode to executable memory
    memcpy(mem, bytecode, sc_len);
    
    // Execute bytecode
    int status = 0;
    pid_t pid = fork();
    
    if (pid == 0) {
        // Child process
        
        if (capture && outputBuffer) {
            // Redirect stdout/stderr to our pipes in the child
            dup2(stdout_pipe[1], STDOUT_FILENO);
            dup2(stderr_pipe[1], STDERR_FILENO);
            
            // Close all pipe ends in child
            close(stdout_pipe[0]);
            close(stdout_pipe[1]);
            close(stderr_pipe[0]);
            close(stderr_pipe[1]);
            
            if (saved_stdout != -1) close(saved_stdout);
            if (saved_stderr != -1) close(saved_stderr);
        }
        
        // Execute bytecode as a function
        ((void(*)())mem)();
        
        // If we reach here, bytecode didn't exit
        _exit(0);
    } else if (pid < 0) {
        // Fork failed
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to fork process for bytecode execution.\n");
        }
        
        // Clean up
        munmap(mem, sc_len);
        
        if (capture && outputBuffer) {
            if (saved_stdout != -1) close(saved_stdout);
            if (saved_stderr != -1) close(saved_stderr);
            close(stdout_pipe[0]);
            close(stdout_pipe[1]);
            close(stderr_pipe[0]);
            close(stderr_pipe[1]);
        }
        
        return -1;
    }
    
    // Parent process
    
    if (capture && outputBuffer) {
        // Close write ends of pipes in parent
        close(stdout_pipe[1]);
        close(stderr_pipe[1]);
    }
    
    // Wait for child process to complete
    int wait_status;
    waitpid(pid, &wait_status, 0);
    
    if (WIFEXITED(wait_status)) {
        status = WEXITSTATUS(wait_status);
    } else if (WIFSIGNALED(wait_status)) {
        status = 128 + WTERMSIG(wait_status);
        
        if (outputBuffer) {
            char signal_msg[128];
            snprintf(signal_msg, sizeof(signal_msg), 
                     "[Shellcode terminated by signal %d]\n", WTERMSIG(wait_status));
            appendToBuffer(outputBuffer, signal_msg);
        }
    } else {
        status = -1;
    }
    
    // Free executable memory
    munmap(mem, sc_len);
    
    // If capturing output, read from pipes
    if (capture && outputBuffer) {
        // Restore original stdout/stderr
        if (saved_stdout != -1) {
            dup2(saved_stdout, STDOUT_FILENO);
            close(saved_stdout);
        }
        
        if (saved_stderr != -1) {
            dup2(saved_stderr, STDERR_FILENO);
            close(saved_stderr);
        }
        
        // Read from stdout pipe
        char buffer[4096];
        ssize_t bytes_read;
        
        while ((bytes_read = read(stdout_pipe[0], buffer, sizeof(buffer) - 1)) > 0) {
            buffer[bytes_read] = '\0';
            appendToBuffer(outputBuffer, buffer);
        }
        
        // Read from stderr pipe
        while ((bytes_read = read(stderr_pipe[0], buffer, sizeof(buffer) - 1)) > 0) {
            buffer[bytes_read] = '\0';
            appendToBuffer(outputBuffer, buffer);
        }
        
        // Close read ends of pipes
        close(stdout_pipe[0]);
        close(stderr_pipe[0]);
    }
    
    return status;
}

int execute_bytecode(const char *input, Buffer *outputBuffer, bool capture) {
    if (!input) return -1;
    
    unsigned char bytecode[MAX_BYTECODE_LEN];
    int sc_len = parse_bytecodecode(input, bytecode);
    
    if (sc_len <= 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Invalid bytecode format.\n");
        }
        return -1;
    }
    
    return execute_bytecode_binary(bytecode, sc_len, outputBuffer, capture);
}

void eval_bytecode(BufferManager *bm) {
    Buffer *minibuffer = getBuffer(bm, "minibuffer");
    Buffer *prompt = getBuffer(bm, "prompt");
    
    if (minibuffer->size == 0) {
        minibuffer->size = 0;
        minibuffer->point = 0;
        minibuffer->content[0] = '\0';
        free(prompt->content);
        prompt->content = strdup("Eval bytecode: ");
        switchToBuffer(bm, "minibuffer");
        return;
    }
    
    // Execute bytecode with output capture
    Buffer *outputBuffer = wm.activeWindow->buffer;
    int status = execute_bytecode(minibuffer->content, outputBuffer, true);
    
    if (status != 0) {
        char status_msg[128];
        snprintf(status_msg, sizeof(status_msg), 
                 "[Shellcode exited with status: %d]\n", status);
        appendToBuffer(outputBuffer, status_msg);
    }
    
    // Clear minibuffer after operation
    minibuffer->size = 0;
    minibuffer->point = 0;
    minibuffer->content[0] = '\0';
    free(prompt->content);
    prompt->content = strdup("");
    switchToBuffer(bm, outputBuffer->name);
}

int compile_and_execute_region(BufferManager *bm) {
    Buffer *active = wm.activeWindow->buffer;
    if (!active) return -1;

    // Get the region bounds
    size_t start, end;
    if (active->region.active) {
        start = active->region.start;
        end = active->region.end;
    } else {
        // If no active region, use mark and point
        start = active->region.mark;
        end = active->point;
    }

    // Ensure start is before end
    if (start > end) {
        size_t temp = start;
        start = end;
        end = temp;
    }

    // Clamp to buffer size
    if (end > active->size) end = active->size;
    if (start > active->size) start = active->size;

    // Extract the region content
    char *region_content = buffer_substring(active, start, end);
    if (!region_content) return -1;

    // Create output buffer
    Buffer *output = generate_new_buffer("*bytecode-output*", "", 0);
    if (!output) {
        free(region_content);
        return -1;
    }

    /* split_window_right(&wm, &wm.activeWindow->parameters); */
    /* other_window(&wm, 1); */
    /* wm.activeWindow->buffer = output; */

    split_or_take_over_window(&wm, output->name, &wm.activeWindow->parameters);
    wm.activeWindow->buffer->major_mode = strdup("compilation");

    appendToBuffer(output, "Compiling region to bytecode...\n");

    // Compile the region content to bytecode
    unsigned char bytecode[MAX_BYTECODE_LEN];
    int sc_len = compile_c_to_bytecode(region_content, output, bytecode, MAX_BYTECODE_LEN);
    free(region_content);

    if (sc_len <= 0) {
        appendToBuffer(output, "Failed to compile region to bytecode.\n");
        switchToBuffer(bm, output->name);
        return -1;
    }

    appendToBuffer(output, "\nExecuting generated bytecode...\n");

    // Execute the bytecode
    int status = execute_bytecode_binary(bytecode, sc_len, output, true);


    /* parse_and_push_compilation_syntax(output); // NOTE It doesn't work for some reason */

    if (status != 0) {
        char status_msg[128];
        snprintf(status_msg, sizeof(status_msg), 
                 "[Shellcode exited with status: %d]\n", status);
        appendToBuffer(output, status_msg);
    } else {
        appendToBuffer(output, "[Shellcode executed successfully]\n");
    }


    return status;
}

int compile_c_to_bytecode(const char *source_code, Buffer *outputBuffer, 
                           unsigned char *bytecodecode_out, int max_bytecodecode_len) {
    if (!source_code || !bytecodecode_out || max_bytecodecode_len <= 0) {
        return -1;
    }
    
    // Create temporary files
    char c_filename[]   = "/tmp/bytecodecode_XXXXXX.c";
    char obj_filename[] = "/tmp/bytecodecode_XXXXXX.o";
    char asm_filename[] = "/tmp/bytecodecode_XXXXXX.s";
    char bin_filename[] = "/tmp/bytecodecode_XXXXXX.bin";
    
    // Generate unique filenames
    int c_fd = mkstemps(c_filename, 2);
    if (c_fd < 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to create temp C file.\n");
        }
        return -1;
    }
    close(c_fd);
    
    int obj_fd = mkstemps(obj_filename, 2);
    if (obj_fd < 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to create temp object file.\n");
        }
        unlink(c_filename);
        return -1;
    }
    close(obj_fd);
    
    int asm_fd = mkstemps(asm_filename, 2);
    if (asm_fd < 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to create temp assembly file.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        return -1;
    }
    close(asm_fd);
    
    int bin_fd = mkstemps(bin_filename, 4);
    if (bin_fd < 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to create temp binary file.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        return -1;
    }
    close(bin_fd);
    
    // Write C source to file
    FILE *c_file = fopen(c_filename, "w");
    if (!c_file) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to open temp C file for writing.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    fprintf(c_file, "%s", source_code);
    fclose(c_file);
    
    // Compile C to assembly
    char compile_cmd[1024];
    snprintf(compile_cmd, sizeof(compile_cmd), 
             "gcc -S -fPIC -fno-asynchronous-unwind-tables -nostdlib -O2 -o %s %s 2>&1",
             asm_filename, c_filename);
    
    FILE *compile_pipe = popen(compile_cmd, "r");
    if (!compile_pipe) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to compile C to assembly.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Capture compiler output
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), compile_pipe) != NULL) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, buffer);
        }
    }
    
    int compile_status = pclose(compile_pipe);
    if (compile_status != 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Compilation failed.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Assemble to object file
    char assemble_cmd[1024];
    snprintf(assemble_cmd, sizeof(assemble_cmd), 
             "as -o %s %s 2>&1", obj_filename, asm_filename);
    
    FILE *assemble_pipe = popen(assemble_cmd, "r");
    if (!assemble_pipe) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to assemble to object file.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Capture assembler output
    while (fgets(buffer, sizeof(buffer), assemble_pipe) != NULL) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, buffer);
        }
    }
    
    int assemble_status = pclose(assemble_pipe);
    if (assemble_status != 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Assembly failed.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Extract raw binary bytecode from object file
    char extract_cmd[1024];
    snprintf(extract_cmd, sizeof(extract_cmd), 
             "objcopy -O binary -j .text %s %s 2>&1", obj_filename, bin_filename);
    
    FILE *extract_pipe = popen(extract_cmd, "r");
    if (!extract_pipe) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to extract binary bytecode.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Capture objcopy output
    while (fgets(buffer, sizeof(buffer), extract_pipe) != NULL) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, buffer);
        }
    }
    
    int extract_status = pclose(extract_pipe);
    if (extract_status != 0) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Binary extraction failed.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Read binary bytecode
    FILE *bin_file = fopen(bin_filename, "rb");
    if (!bin_file) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Failed to open binary file.\n");
        }
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Get file size
    fseek(bin_file, 0, SEEK_END);
    long bin_size = ftell(bin_file);
    fseek(bin_file, 0, SEEK_SET);
    
    if (bin_size > max_bytecodecode_len) {
        if (outputBuffer) {
            appendToBuffer(outputBuffer, "Generated bytecode is too large.\n");
        }
        fclose(bin_file);
        unlink(c_filename);
        unlink(obj_filename);
        unlink(asm_filename);
        unlink(bin_filename);
        return -1;
    }
    
    // Read bytecode into output buffer
    size_t bytes_read = fread(bytecodecode_out, 1, bin_size, bin_file);
    fclose(bin_file);
    
    // Clean up temp files
    unlink(c_filename);
    unlink(obj_filename);
    unlink(asm_filename);
    unlink(bin_filename);
    
    // Show bytecode bytes in hex format if requested
    if (outputBuffer) {
        appendToBuffer(outputBuffer, "Generated bytecode (");
        char size_str[32];
        snprintf(size_str, sizeof(size_str), "%ld bytes", bytes_read);
        appendToBuffer(outputBuffer, size_str);
        appendToBuffer(outputBuffer, "):\n");
        
        for (size_t i = 0; i < bytes_read; i++) {
            char hex_byte[5];
            snprintf(hex_byte, sizeof(hex_byte), "\\x%02x", bytecodecode_out[i]);
            appendToBuffer(outputBuffer, hex_byte);
            
            // Add newline every 16 bytes for readability
            if ((i + 1) % 16 == 0) {
                appendToBuffer(outputBuffer, "\n");
            }
        }
        
        // Ensure we end with a newline
        if (bytes_read % 16 != 0) {
            appendToBuffer(outputBuffer, "\n");
        }
    }
    
    return (int)bytes_read;
}

void compile_and_execute_bytecodecode(BufferManager *bm) {
    Buffer *active = wm.activeWindow->buffer;
    Buffer *output = generate_new_buffer("*bytecode-output*", "", 0);
    
    if (!active || !output) {
        return;
    }
    
    appendToBuffer(output, "Compiling C code to bytecode...\n");
    
    unsigned char bytecode[MAX_BYTECODE_LEN];
    int sc_len = compile_c_to_bytecode(active->content, output, bytecode, MAX_BYTECODE_LEN);
    
    if (sc_len <= 0) {
        appendToBuffer(output, "Failed to compile C code to bytecode.\n");
        switchToBuffer(bm, output->name);
        return;
    }
    
    appendToBuffer(output, "\nExecuting generated bytecode...\n");
    
    int status = execute_bytecode_binary(bytecode, sc_len, output, true);
    
    if (status != 0) {
        char status_msg[128];
        snprintf(status_msg, sizeof(status_msg), 
                 "[Shellcode exited with status: %d]\n", status);
        appendToBuffer(output, status_msg);
    } else {
        appendToBuffer(output, "[Shellcode executed successfully]\n");
    }
    
    switchToBuffer(bm, output->name);
}

int extract_bytecodecode_from_buffer(Buffer *buffer, 
                                 unsigned char *bytecodecode_out, 
                                 int max_bytecodecode_len) {
    if (!buffer || !buffer->content || !bytecodecode_out || max_bytecodecode_len <= 0) {
        return -1;
    }
    
    // Try to detect the format
    const char *content = buffer->content;
    
    // Check if content starts with assembly directives
    if (strstr(content, ".text") || strstr(content, ".globl") || 
        strstr(content, ".global") || strstr(content, ".section")) {
        
        // Looks like assembly code, compile it
        char asm_filename[] = "/tmp/bytecodecode_asm_XXXXXX.s";
        char obj_filename[] = "/tmp/bytecodecode_obj_XXXXXX.o";
        char bin_filename[] = "/tmp/bytecodecode_bin_XXXXXX.bin";
        
        int asm_fd = mkstemps(asm_filename, 2);
        if (asm_fd < 0) return -1;
        close(asm_fd);
        
        int obj_fd = mkstemps(obj_filename, 2);
        if (obj_fd < 0) {
            unlink(asm_filename);
            return -1;
        }
        close(obj_fd);
        
        int bin_fd = mkstemps(bin_filename, 4);
        if (bin_fd < 0) {
            unlink(asm_filename);
            unlink(obj_filename);
            return -1;
        }
        close(bin_fd);
        
        // Write assembly to file
        FILE *asm_file = fopen(asm_filename, "w");
        if (!asm_file) {
            unlink(asm_filename);
            unlink(obj_filename);
            unlink(bin_filename);
            return -1;
        }
        fprintf(asm_file, "%s", content);
        fclose(asm_file);
        
        // Assemble
        char cmd[1024];
        snprintf(cmd, sizeof(cmd), "as -o %s %s", obj_filename, asm_filename);
        int status = system(cmd);
        if (status != 0) {
            unlink(asm_filename);
            unlink(obj_filename);
            unlink(bin_filename);
            return -1;
        }
        
        // Extract binary
        snprintf(cmd, sizeof(cmd), "objcopy -O binary -j .text %s %s", obj_filename, bin_filename);
        status = system(cmd);
        if (status != 0) {
            unlink(asm_filename);
            unlink(obj_filename);
            unlink(bin_filename);
            return -1;
        }
        
        // Read binary
        FILE *bin_file = fopen(bin_filename, "rb");
        if (!bin_file) {
            unlink(asm_filename);
            unlink(obj_filename);
            unlink(bin_filename);
            return -1;
        }
        
        fseek(bin_file, 0, SEEK_END);
        long bin_size = ftell(bin_file);
        fseek(bin_file, 0, SEEK_SET);
        
        if (bin_size > max_bytecodecode_len) {
            fclose(bin_file);
            unlink(asm_filename);
            unlink(obj_filename);
            unlink(bin_filename);
            return -1;
        }
        
        int bytes_read = (int)fread(bytecodecode_out, 1, bin_size, bin_file);
        fclose(bin_file);
        
        unlink(asm_filename);
        unlink(obj_filename);
        unlink(bin_filename);
        
        return bytes_read;
    }
    else {
        // Try parsing as hex bytecode
        return parse_bytecodecode(content, bytecodecode_out);
    }
}

// TODO SCM_DEFINE
/* #include <libguile.h> */

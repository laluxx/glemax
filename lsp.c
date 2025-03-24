#include "lsp.h"
#include "edit.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>


// NOTE Global lsp config
LspConfig lspConfig = {
    .timeout_ms = 5000,
    .log_path = "/tmp/lsp.log",
    .log_level = LOG_LEVEL_INFO,
    .server_argv = (char*[]){ "clangd", NULL }
};

LspClient *lspClient = NULL;

static volatile sig_atomic_t g_got_sigpipe = 0;

static void handle_sigpipe(int sig) {
    g_got_sigpipe = 1;
}

static void* read_stderr(void* arg) {
    LspClient* client = (LspClient*)arg;
    char buf[256];
    ssize_t n;

    while ((n = read(client->server_stderr, buf, sizeof(buf)-1)) > 0) {
        buf[n] = '\0';
        appendToBuffer(client->stderr_buffer, buf);
        if (client->config->log_path) {
            FILE* log = fopen(client->config->log_path, "a");
            if (log) {
                fprintf(log, "[LSP-ERR] %s", buf);
                fclose(log);
            }
        }
    }
    return NULL;
}


// Helper to duplicate a NULL-terminated string array
static char** copy_string_array(const char** src) {
    if (!src) return NULL;
    
    size_t count = 0;
    while (src[count]) count++;
    
    char** dest = malloc((count + 1) * sizeof(char*));
    if (!dest) return NULL;
    
    for (size_t i = 0; i < count; i++) {
        dest[i] = strdup(src[i]);
        if (!dest[i]) {
            // Cleanup on allocation failure
            for (size_t j = 0; j < i; j++) free(dest[j]);
            free(dest);
            return NULL;
        }
    }
    dest[count] = NULL;
    return dest;
}

// Helper to free a string array
static void free_string_array(char** arr) {
    if (!arr) return;
    for (char** p = arr; *p; p++) free(*p);
    free(arr);
}

LspClient* initLsp(const char* workspace_root, const char* language_id, const LspConfig* config) {
    // Validate essential parameters
    if (!workspace_root || !language_id || !config || !config->server_argv || !config->server_argv[0]) {
        errno = EINVAL;
        return NULL;
    }

    // Allocate client structure
    LspClient* client = calloc(1, sizeof(LspClient));
    if (!client) return NULL;

    // Initialize with safe defaults
    client->server_pid = -1;
    client->server_stdin = -1;
    client->server_stdout = -1;
    client->server_stderr = -1;

    // Duplicate string parameters
    client->workspace_root = strdup(workspace_root);
    client->language_id = strdup(language_id);
    if (!client->workspace_root || !client->language_id) goto error;

    // Copy configuration
    client->config = malloc(sizeof(LspConfig));
    if (!client->config) goto error;
    
    *client->config = (LspConfig){
        .timeout_ms = config->timeout_ms,
        .log_level = config->log_level,
        .log_path = config->log_path ? strdup(config->log_path) : NULL,
        .server_argv = copy_string_array((const char**)config->server_argv)
    };
    
    if ((config->log_path && !client->config->log_path) || !client->config->server_argv) goto error;

    // Create communication pipes
    int stdin_pipe[2], stdout_pipe[2], stderr_pipe[2];
    if (pipe(stdin_pipe))  goto error;
    if (pipe(stdout_pipe)) goto pipe_cleanup;
    if (pipe(stderr_pipe)) goto pipe_cleanup;

    // Fork to start language server
    pid_t pid = fork();
    if (pid < 0) goto pipe_cleanup;

    if (pid == 0) { // Child process - language server
        // Redirect standard streams
        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        dup2(stderr_pipe[1], STDERR_FILENO);

        // Close unused pipe ends
        close(stdin_pipe[0]);
        close(stdin_pipe[1]);
        close(stdout_pipe[0]);
        close(stdout_pipe[1]);
        close(stderr_pipe[0]);
        close(stderr_pipe[1]);

        // Set working directory
        if (chdir(client->workspace_root)) {
            perror("Failed to set working directory");
            exit(EXIT_FAILURE);
        }

        // Execute server
        execvp(client->config->server_argv[0], client->config->server_argv);
        perror("Failed to start language server");
        exit(EXIT_FAILURE);
    } else { // Parent process - text editor
        // Close unused pipe ends
        close(stdin_pipe[0]);
        close(stdout_pipe[1]);
        close(stderr_pipe[1]);

        // Store communication handles
        client->server_pid = pid;
        client->server_stdin = stdin_pipe[1];
        client->server_stdout = stdout_pipe[0];
        client->server_stderr = stderr_pipe[0];

        return client;
    }

pipe_cleanup:
    close(stdin_pipe[0]);
    close(stdin_pipe[1]);
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    close(stderr_pipe[0]);
    close(stderr_pipe[1]);
error:
    freeLsp(client);
    return NULL;
}

void freeLsp(LspClient* client) {
    if (!client) return;

    // Close communication channels
    if (client->server_stdin != -1) close(client->server_stdin);
    if (client->server_stdout != -1) close(client->server_stdout);
    if (client->server_stderr != -1) close(client->server_stderr);

    // Terminate server process
    if (client->server_pid != -1) {
        kill(client->server_pid, SIGTERM);
        waitpid(client->server_pid, NULL, 0);
    }

    // Free allocated memory
    free(client->workspace_root);
    free(client->language_id);
    
    if (client->config) {
        free(client->config->log_path);
        free_string_array(client->config->server_argv);
        free(client->config);
    }
    
    free(client);
}


// Function to check if LSP is running
bool lspp(LspClient* client) {
    if (!client || client->server_pid == -1) return false;
    return kill(client->server_pid, 0) == 0;
}

/**
   Stop LSP server for PROJECT.
*/
void stop_lsp() {
    if (!lspClient || lspClient->server_pid == -1) return;
    kill(lspClient->server_pid, SIGTERM);
    waitpid(lspClient->server_pid, NULL, 0);
    lspClient->server_pid = -1;
}


bool initialize_lsp_session(LspClient* client) {
    if (!client) return false;
    
    // Create initialize request
    json_object* request = json_object_new_object();
    json_object_object_add(request, "jsonrpc", json_object_new_string("2.0"));
    json_object_object_add(request, "id", json_object_new_int(1));
    json_object_object_add(request, "method", json_object_new_string("initialize"));
    
    // Create parameters
    json_object* params = json_object_new_object();
    
    // Process ID
    json_object_object_add(params, "processId", json_object_new_int(getpid()));
    
    // Root URI
    char root_uri[PATH_MAX + 8];
    if (strncmp(client->workspace_root, "file://", 7) != 0) {
        snprintf(root_uri, sizeof(root_uri), "file://%s", client->workspace_root);
        json_object_object_add(params, "rootUri", json_object_new_string(root_uri));
    } else {
        json_object_object_add(params, "rootUri", json_object_new_string(client->workspace_root));
    }
    
    // Client capabilities
    json_object* capabilities = json_object_new_object();
    
    // Text document capabilities
    json_object* textDocument = json_object_new_object();
    
    // Declare support for definition requests
    json_object* definition = json_object_new_object();
    json_object_object_add(definition, "dynamicRegistration", json_object_new_boolean(false));
    json_object_object_add(textDocument, "definition", definition);
    
    // Add more capabilities as needed
    // For example, completion, hover, references, etc.
    
    json_object_object_add(capabilities, "textDocument", textDocument);
    json_object_object_add(params, "capabilities", capabilities);
    
    // Add params to request
    json_object_object_add(request, "params", params);
    
    // Send the request
    if (!lsp_send_request(client, request)) {
        fprintf(stderr, "Failed to send initialization request\n");
        json_object_put(request);
        return false;
    }
    
    // Read the response
    json_object* response = lsp_read_response(client, client->config->timeout_ms);
    if (!response) {
        fprintf(stderr, "No initialization response from LSP server\n");
        json_object_put(request);
        return false;
    }
    
    // Check for successful initialization
    bool success = false;
    json_object* result = NULL;
    json_object* error = NULL;
    
    if (json_object_object_get_ex(response, "result", &result)) {
        // Server capabilities are in the result
        success = true;
        
        // Here you could store server capabilities if needed
        // json_object* serverCapabilities = NULL;
        // if (json_object_object_get_ex(result, "capabilities", &serverCapabilities)) {
        //     // Store capabilities for future use
        // }
    } else if (json_object_object_get_ex(response, "error", &error)) {
        json_object* message_obj = NULL;
        if (json_object_object_get_ex(error, "message", &message_obj)) {
            fprintf(stderr, "LSP initialization error: %s\n", 
                   json_object_get_string(message_obj));
        }
    }
    
    // Send initialized notification
    if (success) {
        json_object* notification = json_object_new_object();
        json_object_object_add(notification, "jsonrpc", json_object_new_string("2.0"));
        json_object_object_add(notification, "method", json_object_new_string("initialized"));
        json_object_object_add(notification, "params", json_object_new_object());
        
        if (!lsp_send_request(client, notification)) {
            fprintf(stderr, "Failed to send initialized notification\n");
            success = false;
        }
        
        json_object_put(notification);
    }
    
    // Cleanup
    json_object_put(request);
    json_object_put(response);
    
    return success;
}

/**
   Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.
*/
void start_lsp() {
    if (!lspClient) return;
    if (lspp(lspClient)) {
        message("LSP Already running");
        return; // Already running
    }

    // Reinitialize the LSP
    LspClient* newClient = initLsp(lspClient->workspace_root, lspClient->language_id, lspClient->config);
    if (!newClient) return;

    // Initialize LSP session BEFORE using
    if (!initialize_lsp_session(newClient)) {
        fprintf(stderr, "LSP initialization failed\n");
        freeLsp(newClient);
        return;
    }


    // Copy the new client's state to the existing client
    lspClient->server_pid = newClient->server_pid;
    lspClient->server_stdin = newClient->server_stdin;
    lspClient->server_stdout = newClient->server_stdout;
    lspClient->server_stderr = newClient->server_stderr;

    // Free the new client structure without freeing its internal data
    free(newClient);

    return;
}

char* uri_to_path(const char* uri) {
    if (!uri) return NULL;
    if (strncmp(uri, "file://", 7) == 0) {
        // Basic URI decoding
        char *decoded = strdup(uri + 7);
        char *p = decoded;
        while (*p) {
            if (*p == '%' && isxdigit(p[1]) && isxdigit(p[2])) {
                *p = (char)strtol(p + 1, NULL, 16);
                memmove(p + 1, p + 3, strlen(p + 3) + 1);
            }
            p++;
        }
        return decoded;
    }
    return strdup(uri);
}

bool lsp_send_request(LspClient* client, json_object* request) {
    if (!client || !request) return false;

    // Install SIGPIPE handler
    struct sigaction sa, old_sa;
    sa.sa_handler = handle_sigpipe;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGPIPE, &sa, &old_sa);
    g_got_sigpipe = 0;

    const char *request_str = json_object_to_json_string_ext(request, JSON_C_TO_STRING_PLAIN);
    size_t request_len = strlen(request_str);
    
    char header[128];
    int header_len = snprintf(header, sizeof(header), 
                              "Content-Length: %zu\r\n\r\n", request_len);
    
    // Write header
    ssize_t written = 0;
    while (written < header_len) {
        ssize_t rc = write(client->server_stdin, header + written, header_len - written);
        if (rc <= 0 || g_got_sigpipe) goto error;
        written += rc;
    }
    
    // Write body
    written = 0;
    while (written < request_len) {
        ssize_t rc = write(client->server_stdin, request_str + written, request_len - written);
        if (rc <= 0 || g_got_sigpipe) goto error;
        written += rc;
    }
    
    sigaction(SIGPIPE, &old_sa, NULL);
    return true;

 error:
    sigaction(SIGPIPE, &old_sa, NULL);
    fprintf(stderr, "Write failed: %s\n", strerror(errno));
    return false;
}

/* bool lsp_send_request(LspClient* client, json_object* request) { */
/*     if (!client || !request) return false; */
    
/*     // Convert request to string */
/*     const char* request_str = json_object_to_json_string(request); */
/*     size_t request_len = strlen(request_str); */
    
/*     // Create LSP header with content length */
/*     char header[128]; */
/*     snprintf(header, sizeof(header), "Content-Length: %zu\r\n\r\n", request_len); */
    
/*     // Send header */
/*     ssize_t written = write(client->server_stdin, header, strlen(header)); */
/*     if (written == -1) return false; */
    
/*     // Send JSON content */
/*     written = write(client->server_stdin, request_str, request_len); */
/*     return (written != -1); */
/* } */

json_object* lsp_read_response(LspClient* client, int timeout_ms) {
    if (!client) return NULL;
    
    // Set up select for timeout
    fd_set read_fds;
    struct timeval timeout = {
        .tv_sec = timeout_ms / 1000,
        .tv_usec = (timeout_ms % 1000) * 1000
    };
    
    // Read header to get content length
    char buffer[4096] = {0};
    size_t buffer_pos = 0;
    size_t content_length = 0;
    bool header_complete = false;
    
    // Read header with timeout
    while (!header_complete) {
        FD_ZERO(&read_fds);
        FD_SET(client->server_stdout, &read_fds);
        
        int ready = select(client->server_stdout + 1, &read_fds, NULL, NULL, &timeout);
        if (ready <= 0) {
            fprintf(stderr, "LSP response timeout or error\n");
            return NULL;
        }
        
        ssize_t bytes_read = read(client->server_stdout, 
                                 buffer + buffer_pos,
                                 sizeof(buffer) - buffer_pos - 1);
        
        if (bytes_read <= 0) {
            fprintf(stderr, "Failed to read from LSP server\n");
            return NULL;
        }
        
        buffer_pos += bytes_read;
        buffer[buffer_pos] = '\0';
        
        // Look for end of headers
        char* header_end = strstr(buffer, "\r\n\r\n");
        if (header_end) {
            // Parse Content-Length header
            char* length_header = strstr(buffer, "Content-Length:");
            if (length_header) {
                content_length = strtoul(length_header + 15, NULL, 10);
                header_complete = true;
                
                // Adjust buffer to remove header
                size_t header_size = (header_end + 4) - buffer;
                memmove(buffer, header_end + 4, buffer_pos - header_size);
                buffer_pos -= header_size;
            } else {
                fprintf(stderr, "No Content-Length in LSP response\n");
                return NULL;
            }
        }
    }
    
    // Allocate buffer for full response
    char* response = malloc(content_length + 1);
    if (!response) {
        fprintf(stderr, "Memory allocation failed\n");
        return NULL;
    }
    
    // Copy what we've already read
    memcpy(response, buffer, buffer_pos);
    
    // Read remaining content
    size_t remaining = content_length - buffer_pos;
    size_t total_read = buffer_pos;
    
    while (remaining > 0) {
        FD_ZERO(&read_fds);
        FD_SET(client->server_stdout, &read_fds);
        
        int ready = select(client->server_stdout + 1, &read_fds, NULL, NULL, &timeout);
        if (ready <= 0) {
            fprintf(stderr, "LSP response timeout or error\n");
            free(response);
            return NULL;
        }
        
        ssize_t bytes_read = read(client->server_stdout, 
                                 response + total_read,
                                 remaining);
        
        if (bytes_read <= 0) {
            fprintf(stderr, "Failed to read from LSP server\n");
            free(response);
            return NULL;
        }
        
        total_read += bytes_read;
        remaining -= bytes_read;
    }
    
    response[content_length] = '\0';
    
    // Parse JSON response
    json_object* json_response = json_tokener_parse(response);
    free(response);
    
    if (!json_response) {
        fprintf(stderr, "Invalid JSON in LSP response\n");
        return NULL;
    }
    
    return json_response;
}

void goto_definition(Buffer* buffer) {
    if (!lspClient || !buffer || !buffer->path) {
        message("LSP client or buffer invalid");
        return;
    }
    
    // Ensure server is running and initialized
    if (!lspp(lspClient)) {
        message("LSP server not running. Attempting to start...");
        start_lsp();
        
        if (!lspp(lspClient)) {
            message("Failed to start LSP server");
            return;
        }
    }
    
    // Configure SIGPIPE handling
    struct sigaction sa;
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    if (sigaction(SIGPIPE, &sa, NULL) == -1) {
        perror("sigaction");
        return;
    }
    
    // Create JSON-RPC request
    json_object* request = json_object_new_object();
    json_object_object_add(request, "jsonrpc", json_object_new_string("2.0"));
    json_object_object_add(request, "id", json_object_new_int(2)); // Use unique IDs for requests
    json_object_object_add(request, "method", json_object_new_string("textDocument/definition"));
    
    // Build text document identifier
    json_object* params = json_object_new_object();
    json_object* textDocument = json_object_new_object();
    
    // Convert buffer path to URI format if needed
    char uri[PATH_MAX + 8]; // Extra space for "file://" prefix
    if (strncmp(buffer->path, "file://", 7) != 0) {
        snprintf(uri, sizeof(uri), "file://%s", buffer->path);
    } else {
        strncpy(uri, buffer->path, sizeof(uri) - 1);
        uri[sizeof(uri) - 1] = '\0';
    }
    
    json_object_object_add(textDocument, "uri", json_object_new_string(uri));
    json_object_object_add(params, "textDocument", textDocument);
    
    // Build position object (0-indexed for LSP)
    json_object* position = json_object_new_object();
    // Convert buffer's point to line and character
    // This depends on your buffer structure, so may need adjustment
    int line = 0;
    int character = 0;
    size_t i = 0;
    
    // Calculate line and character from buffer point
    while (i < buffer->point && i < buffer->size) {
        if (buffer->content[i] == '\n') {
            line++;
            character = 0;
        } else {
            character++;
        }
        i++;
    }
    
    json_object_object_add(position, "line", json_object_new_int(line));
    json_object_object_add(position, "character", json_object_new_int(character));
    json_object_object_add(params, "position", position);
    
    // Add params to request
    json_object_object_add(request, "params", params);
    
    // Send request
    if (!lsp_send_request(lspClient, request)) {
        message("Failed to send request to LSP server");
        json_object_put(request);
        return;
    }
    
    // Read response
    json_object* response = lsp_read_response(lspClient, lspClient->config->timeout_ms);
    if (!response) {
        message("No response from LSP server");
        json_object_put(request);
        return;
    }
    
    // Process result
    json_object* result = NULL;
    if (json_object_object_get_ex(response, "result", &result)) {
        // Handle multiple definitions (result can be an array or a single object)
        if (json_object_get_type(result) == json_type_array) {
            int array_len = json_object_array_length(result);
            if (array_len > 0) {
                // Take the first definition
                json_object* first_def = json_object_array_get_idx(result, 0);
                
                // Extract location information
                json_object* uri_obj = NULL;
                json_object* range = NULL;
                
                if (json_object_object_get_ex(first_def, "uri", &uri_obj) &&
                    json_object_object_get_ex(first_def, "range", &range)) {
                    
                    const char* target_uri = json_object_get_string(uri_obj);
                    char* target_path = uri_to_path(target_uri);
                    
                    if (target_path) {
                        json_object* start = NULL;
                        if (json_object_object_get_ex(range, "start", &start)) {
                            json_object* line_obj = NULL;
                            json_object* char_obj = NULL;
                            
                            if (json_object_object_get_ex(start, "line", &line_obj) &&
                                json_object_object_get_ex(start, "character", &char_obj)) {
                                
                                int target_line = json_object_get_int(line_obj);
                                int target_char = json_object_get_int(char_obj);
                                
                                message("Found definition in %s at line %d, character %d",
                                       target_path, target_line + 1, target_char + 1);
                                
                                // TODO: Open the file and navigate to the position
                                // This depends on your editor's functionality
                                // Example:
                                // Buffer* target_buffer = openBuffer(target_path);
                                // if (target_buffer) {
                                //     moveTo(target_buffer, target_line, target_char);
                                // }
                            }
                        }
                        free(target_path);
                    }
                }
            } else {
                message("No definition found");
            }
        } else if (json_object_get_type(result) == json_type_object) {
            // Handle single definition object
            json_object* uri_obj = NULL;
            json_object* range = NULL;
            
            if (json_object_object_get_ex(result, "uri", &uri_obj) &&
                json_object_object_get_ex(result, "range", &range)) {
                
                const char* target_uri = json_object_get_string(uri_obj);
                char* target_path = uri_to_path(target_uri);
                
                if (target_path) {
                    json_object* start = NULL;
                    if (json_object_object_get_ex(range, "start", &start)) {
                        json_object* line_obj = NULL;
                        json_object* char_obj = NULL;
                        
                        if (json_object_object_get_ex(start, "line", &line_obj) &&
                            json_object_object_get_ex(start, "character", &char_obj)) {
                            
                            int target_line = json_object_get_int(line_obj);
                            int target_char = json_object_get_int(char_obj);
                            
                            message("Found definition in %s at line %d, character %d",
                                   target_path, target_line + 1, target_char + 1);
                            
                            // TODO: Open the file and navigate to the position
                            // This depends on your editor's functionality
                            // Example:
                            // Buffer* target_buffer = openBuffer(target_path);
                            // if (target_buffer) {
                            //     moveTo(target_buffer, target_line, target_char);
                            // }
                        }
                    }
                    free(target_path);
                }
            }
        } else {
            message("No definition found or unexpected result format");
        }
    } else {
        // Check for error
        json_object* error = NULL;
        if (json_object_object_get_ex(response, "error", &error)) {
            json_object* message_obj = NULL;
            if (json_object_object_get_ex(error, "message", &message_obj)) {
                const char* error_msg = json_object_get_string(message_obj);
                message("LSP error: %s", error_msg);
            } else {
                message("Unknown LSP error");
            }
        } else {
            message("Unexpected LSP response format");
        }
    }
    
    // Cleanup
    json_object_put(request);
    json_object_put(response);
}

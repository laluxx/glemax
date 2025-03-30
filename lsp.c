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
#include <pwd.h>
#include <stdarg.h>


// NOTE Global lsp config TODO lspConfig(s)
LspConfig lspConfig = {
    .timeout_ms = 5000,
    .log_path = "/tmp/lsp.log",
    .log_level = LOG_LEVEL_INFO,
    .server_argv = (char*[]){ "clangd", "--log=verbose", NULL }
};


LspClients *lspClients = NULL;
/* LspConfigs *lspConfigs = NULL; // TODO */


LspClients* initLspClients(size_t initial_capacity) {
    if (initial_capacity == 0) {
        initial_capacity = LSP_CLIENTS_INITIAL_CAPACITY;
    }

    LspClients* lsp_clients = malloc(sizeof(LspClients));
    if (!lsp_clients) {
        errno = ENOMEM;
        return NULL;
    }

    lsp_clients->clients = malloc(sizeof(LspClient*) * initial_capacity);
    if (!lsp_clients->clients) {
        free(lsp_clients);
        errno = ENOMEM;
        return NULL;
    }

    lsp_clients->count = 0;
    lsp_clients->capacity = initial_capacity;

    return lsp_clients;
}

void freeLspClients(LspClients* lsp_clients) {
    if (!lsp_clients) return;

    // Free each client
    for (size_t i = 0; i < lsp_clients->count; i++) {
        freeLsp(lsp_clients->clients[i]);
    }

    // Free the array and the structure
    free(lsp_clients->clients);
    free(lsp_clients);
}

LspClient* addLspClient(LspClients* lsp_clients, 
                        const char* workspace_root, 
                        const char* language_id, 
                        const LspConfig* config) {
    if (!lsp_clients || !workspace_root || !language_id || !config) {
        errno = EINVAL;
        return NULL;
    }

    // Resize if needed
    if (lsp_clients->count >= lsp_clients->capacity) {
        size_t new_capacity = lsp_clients->capacity * 2;
        LspClient** new_clients = realloc(lsp_clients->clients, 
                                          sizeof(LspClient*) * new_capacity);
        if (!new_clients) {
            errno = ENOMEM;
            return NULL;
        }
        lsp_clients->clients = new_clients;
        lsp_clients->capacity = new_capacity;
    }

    // Create new LSP client
    LspClient* new_client = initLsp(workspace_root, language_id, config);
    if (!new_client) {
        return NULL;
    }

    // Add to clients array
    lsp_clients->clients[lsp_clients->count++] = new_client;

    return new_client;
}

bool removeLspClient(LspClients* lsp_clients, LspClient* client) {
    if (!lsp_clients || !client) {
        errno = EINVAL;
        return false;
    }

    // Find the client in the array
    for (size_t i = 0; i < lsp_clients->count; i++) {
        if (lsp_clients->clients[i] == client) {
            // Free the client
            freeLsp(client);

            // Move last element to this position if not the last element
            if (i < lsp_clients->count - 1) {
                lsp_clients->clients[i] = lsp_clients->clients[lsp_clients->count - 1];
            }

            lsp_clients->count--;
            return true;
        }
    }

    errno = ENOENT;
    return false;
}

LspClient* findLspClientByWorkspace(LspClients* lsp_clients, const char* workspace_root) {
    if (!lsp_clients || !workspace_root) {
        errno = EINVAL;
        return NULL;
    }

    for (size_t i = 0; i < lsp_clients->count; i++) {
        if (strcmp(lsp_clients->clients[i]->workspace_root, workspace_root) == 0) {
            return lsp_clients->clients[i];
        }
    }

    return NULL;
}


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
        
        // Try to parse as JSON to detect diagnostics
        json_object* json = json_tokener_parse(buf);
        if (json) {
            json_object* method_obj = NULL;
            if (json_object_object_get_ex(json, "method", &method_obj)) {
                const char* method = json_object_get_string(method_obj);
                if (strcmp(method, "textDocument/publishDiagnostics") == 0) {
                    json_object* params_obj = NULL;
                    if (json_object_object_get_ex(json, "params", &params_obj)) {
                        json_object* diagnostics_obj = NULL;
                        if (json_object_object_get_ex(params_obj, "diagnostics", &diagnostics_obj)) {
                            int count = json_object_array_length(diagnostics_obj);
                            if (count > 0) {
                                json_object* diag = json_object_array_get_idx(diagnostics_obj, 0);
                                json_object* message_obj = NULL;
                                if (json_object_object_get_ex(diag, "message", &message_obj)) {
                                    message("LSP: %s", json_object_get_string(message_obj));
                                }
                            }
                        }
                    }
                }
            }
            json_object_put(json);
        }
        
        appendToBuffer(client->stderr_buffer, buf);
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

char* expand_path(const char* path) {
    if (!path) return NULL;

    // Handle ~ expansion
    if (path[0] == '~') {
        const char *home_dir = getenv("HOME");
        if (!home_dir) {
            struct passwd *pw = getpwuid(getuid());
            home_dir = pw->pw_dir;
        }

        if (home_dir) {
            size_t len = strlen(home_dir) + strlen(path);
            char *expanded = malloc(len + 1);
            if (!expanded) return NULL;
            
            strcpy(expanded, home_dir);
            strcat(expanded, path + 1);  // Skip the ~
            return expanded;
        }
    }

    // No expansion needed
    return strdup(path);
}


LspClient* initLsp(const char* workspace_root, const char* language_id, const LspConfig* config) {
    if (!workspace_root || !language_id || !config || !config->server_argv || !config->server_argv[0]) {
        errno = EINVAL;
        return NULL;
    }

    // Allocate and zero-initialize client structure
    LspClient* client = calloc(1, sizeof(LspClient));
    if (!client) {
        return NULL;
    }

    // Initialize with safe defaults
    client->server_pid = -1;
    client->server_stdin = -1;
    client->server_stdout = -1;
    client->server_stderr = -1;
    client->initialized = false;

    // Initialize request counter and mutex first
    client->request_id_counter = 1;
    if (pthread_mutex_init(&client->request_mutex, NULL) != 0) {
        fprintf(stderr, "Failed to initialize request mutex\n");
        goto cleanup_client;
    }

    // Expand path first
    char *expanded_path = expand_path(workspace_root);
    if (!expanded_path) {
        fprintf(stderr, "Failed to expand path: %s\n", workspace_root);
        goto cleanup_mutex;
    }

    // Then resolve realpath
    if (!(client->workspace_root = realpath(expanded_path, NULL))) {
        fprintf(stderr, "Failed to resolve workspace path: %s (expanded: %s)\n",
                workspace_root, expanded_path);
        free(expanded_path);
        goto cleanup_mutex;
    }
    free(expanded_path);

    if (!(client->language_id = strdup(language_id))) {
        fprintf(stderr, "Failed to duplicate language ID\n");
        goto cleanup_workspace;
    }

    // Copy configuration with deep duplication
    client->config = malloc(sizeof(LspConfig));
    if (!client->config) {
        fprintf(stderr, "Failed to allocate config\n");
        goto cleanup_language_id;
    }
    
    *client->config = (LspConfig){
        .timeout_ms = config->timeout_ms,
        .log_level = config->log_level,
        .log_path = config->log_path ? strdup(config->log_path) : NULL,
        .server_argv = copy_string_array((const char**)config->server_argv)
    };
    
    if ((config->log_path && !client->config->log_path) || !client->config->server_argv) {
        fprintf(stderr, "Failed to duplicate config elements\n");
        goto cleanup_config;
    }

    // Create communication pipes with error handling
    int stdin_pipe[2], stdout_pipe[2], stderr_pipe[2];
    if (pipe(stdin_pipe)) {
        fprintf(stderr, "Failed to create stdin pipe: %s\n", strerror(errno));
        goto cleanup_config_data;
    }
    if (pipe(stdout_pipe)) {
        fprintf(stderr, "Failed to create stdout pipe: %s\n", strerror(errno));
        goto cleanup_stdin_pipe;
    }
    if (pipe(stderr_pipe)) {
        fprintf(stderr, "Failed to create stderr pipe: %s\n", strerror(errno));
        goto cleanup_stdout_pipe;
    }

    // Fork to start language server
    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "Failed to fork: %s\n", strerror(errno));
        goto cleanup_all_pipes;
    }

    if (pid == 0) { // Child process - language server
        // Close unused pipe ends
        close(stdin_pipe[1]);
        close(stdout_pipe[0]);
        close(stderr_pipe[0]);

        // Redirect standard streams with error checking
        if (dup2(stdin_pipe[0], STDIN_FILENO) == -1) {
            perror("Failed to redirect stdin");
            exit(EXIT_FAILURE);
        }
        if (dup2(stdout_pipe[1], STDOUT_FILENO) == -1) {
            perror("Failed to redirect stdout");
            exit(EXIT_FAILURE);
        }
        if (dup2(stderr_pipe[1], STDERR_FILENO) == -1) {
            perror("Failed to redirect stderr");
            exit(EXIT_FAILURE);
        }

        // Close original pipe ends
        close(stdin_pipe[0]);
        close(stdout_pipe[1]);
        close(stderr_pipe[1]);

        // Set working directory
        if (chdir(client->workspace_root)) {
            perror("Failed to set working directory");
            exit(EXIT_FAILURE);
        }

        // Execute server with PATH search
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

        // Initialize stderr buffer
        client->stderr_buffer = malloc(sizeof(Buffer));
        if (!client->stderr_buffer) {
            fprintf(stderr, "Failed to allocate stderr buffer\n");
            goto cleanup_all_pipes;
        }
        
        // Initialize buffer fields
        memset(client->stderr_buffer, 0, sizeof(Buffer));
        client->stderr_buffer->name = strdup("*lsp-stderr*");
        if (!client->stderr_buffer->name) {
            fprintf(stderr, "Failed to allocate stderr buffer name\n");
            goto cleanup_stderr_buffer;
        }
        
        client->stderr_buffer->capacity = 4096;  // Initial buffer size
        client->stderr_buffer->content = malloc(client->stderr_buffer->capacity);
        if (!client->stderr_buffer->content) {
            fprintf(stderr, "Failed to allocate stderr buffer content\n");
            goto cleanup_stderr_buffer_name;
        }
        client->stderr_buffer->content[0] = '\0';
        client->stderr_buffer->readOnly = true;

        // Initialize mutex
        if (pthread_mutex_init(&client->stderr_mutex, NULL) != 0) {
            fprintf(stderr, "Failed to initialize stderr mutex\n");
            goto cleanup_stderr_buffer_content;
        }

        // Start stderr reader thread
        if (pthread_create(&client->stderr_thread, NULL, read_stderr, client) != 0) {
            fprintf(stderr, "Failed to create stderr thread\n");
            goto cleanup_stderr_mutex;
        }

        return client;

    // Cleanup labels for stderr buffer initialization
    cleanup_stderr_mutex:
        pthread_mutex_destroy(&client->stderr_mutex);
    cleanup_stderr_buffer_content:
        free(client->stderr_buffer->content);
    cleanup_stderr_buffer_name:
        free(client->stderr_buffer->name);
    cleanup_stderr_buffer:
        free(client->stderr_buffer);
    }

cleanup_all_pipes:
    close(stderr_pipe[0]);
    close(stderr_pipe[1]);
cleanup_stdout_pipe:
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
cleanup_stdin_pipe:
    close(stdin_pipe[0]);
    close(stdin_pipe[1]);
cleanup_config_data:
    if (client->config) {
        if (client->config->log_path) free(client->config->log_path);
        if (client->config->server_argv) free_string_array(client->config->server_argv);
    }
cleanup_config:
    free(client->config);
cleanup_language_id:
    free(client->language_id);
cleanup_workspace:
    free(client->workspace_root);
cleanup_mutex:
    pthread_mutex_destroy(&client->request_mutex);
cleanup_client:
    free(client);
    return NULL;
}

void freeLsp(LspClient* client) {
    if (!client) return;

    // Request stderr thread to exit by closing the pipe
    if (client->server_stderr != -1) {
        close(client->server_stderr);
        client->server_stderr = -1;
    }

    // Wait for stderr thread to finish
    if (client->stderr_thread) {
        pthread_join(client->stderr_thread, NULL);
    }

    // Destroy mutex. Safe if mutex was properly initialized or zeroed
    pthread_mutex_destroy(&client->stderr_mutex); 

    // Free stderr buffer resources
    if (client->stderr_buffer) {
        if (client->stderr_buffer->content) {
            free(client->stderr_buffer->content);
        }
        if (client->stderr_buffer->name) {
            free(client->stderr_buffer->name);
        }
        free(client->stderr_buffer);
    }

    // Close remaining pipes
    if (client->server_stdin != -1) close(client->server_stdin);
    if (client->server_stdout != -1) close(client->server_stdout);

    // Terminate server process if still running
    if (client->server_pid != -1) {
        // Try graceful shutdown first
        kill(client->server_pid, SIGTERM);
        
        // Wait for process to exit
        int status;
        pid_t result = waitpid(client->server_pid, &status, WNOHANG);
        if (result == 0) {
            // Process still running, wait a bit
            struct timespec timeout = {.tv_sec = 0, .tv_nsec = 100000000}; // 100ms
            nanosleep(&timeout, NULL);
            
            result = waitpid(client->server_pid, &status, WNOHANG);
            if (result == 0) {
                // Still running, force kill
                kill(client->server_pid, SIGKILL);
                waitpid(client->server_pid, &status, 0);
            }
        }
    }

    // Free strings and config
    if (client->workspace_root) free(client->workspace_root);
    if (client->language_id) free(client->language_id);

    // Free server capabilities if they exist
    if (client->server_capabilities) {
        json_object_put(client->server_capabilities);
    }

    if (client->config) {
        if (client->config->log_path) free(client->config->log_path);
        if (client->config->server_argv) free_string_array(client->config->server_argv);
        free(client->config);
    }
    
    free(client);
}


LspClient* get_client_for_current_buffer(void) {
    if (!wm.activeWindow || !wm.activeWindow->buffer || !wm.activeWindow->buffer->path) {
        fprintf(stderr, "No active buffer with path\n");
        return NULL;
    }

    Buffer* buffer = wm.activeWindow->buffer;
    
    // Get absolute path of current buffer
    char* abs_path = resolve_file_path(buffer->path);
    if (!abs_path) {
        fprintf(stderr, "Failed to resolve path: %s\n", buffer->path);
        return NULL;
    }

    // Find project root for current file
    char* workspace_root = getProjectRoot(abs_path);
    if (!workspace_root) {
        fprintf(stderr, "Not in a project workspace: %s\n", abs_path);
        free(abs_path);
        return NULL;
    }
    free(abs_path);

    // Expand and resolve the workspace root path
    char* expanded_root = expand_path(workspace_root);
    if (!expanded_root) {
        fprintf(stderr, "Failed to expand path: %s\n", workspace_root);
        free(workspace_root);
        return NULL;
    }

    char* resolved_root = realpath(expanded_root, NULL);
    free(expanded_root);
    free(workspace_root);

    if (!resolved_root) {
        fprintf(stderr, "Failed to resolve workspace path: %s\n", expanded_root);
        return NULL;
    }

    // Find existing client for this workspace
    LspClient* client = NULL;
    if (lspClients) {
        for (size_t i = 0; i < lspClients->count; i++) {
            if (strcmp(lspClients->clients[i]->workspace_root, resolved_root) == 0) {
                client = lspClients->clients[i];
                break;
            }
        }
    }

    free(resolved_root);
    return client;
}

// Check if LSP is running for current buffer
bool lspp(void) {
    LspClient *client = get_client_for_current_buffer();
    return client && client->initialized && client->server_pid > 0;
}

/**
   Stop LSP server for PROJECT.
*/
void stop_lsp(LspClient* client) {
    if (client && lspClients) {
        removeLspClient(lspClients, client);
        message("LSP stopped");
    }
}



/**
 * Initialize LSP session with comprehensive handshake and capability negotiation
 */
bool initialize_lsp_session(LspClient* client) {
    if (!client || !client->workspace_root || !client->language_id) {
        fprintf(stderr, "[LSP] Initialization failed: invalid client parameters\n");
        return false;
    }

    /* Convert workspace path to URI */
    char* root_uri = path_to_uri(client->workspace_root);
    if (!root_uri) {
        fprintf(stderr, "[LSP] Initialization failed: cannot create workspace URI\n");
        return false;
    }

    /* Build initialization request */
    json_object* request = json_object_new_object();
    json_object_object_add(request, "jsonrpc", json_object_new_string("2.0"));
    
    /* Use fixed ID 1 for initialization as per LSP spec */
    json_object_object_add(request, "id", json_object_new_int(1));
    json_object_object_add(request, "method", json_object_new_string("initialize"));

    /* Build parameters object */
    json_object* params = json_object_new_object();
    json_object_object_add(params, "processId", json_object_new_int(getpid()));
    json_object_object_add(params, "rootUri", json_object_new_string(root_uri));
    json_object_object_add(params, "rootPath", json_object_new_string(client->workspace_root));
    json_object_object_add(params, "locale", json_object_new_string("en-US"));
    free(root_uri);

    /* Set client capabilities */
    json_object* capabilities = build_client_capabilities();
    json_object_object_add(params, "capabilities", capabilities);
    json_object_object_add(request, "params", params);

    /* Send request with timeout */
    if (!lsp_send_request(client, request)) {
        fprintf(stderr, "[LSP] Failed to send initialization request\n");
        json_object_put(request);
        return false;
    }

    /* Get response with extended timeout for initialization */
    json_object* response = lsp_read_response(client, 10000); // 10 second timeout for init
    json_object_put(request);

    if (!response) {
        fprintf(stderr, "[LSP] No response to initialization request\n");
        return false;
    }

    /* Verify this is the response to our initialization */
    json_object* response_id = NULL;
    if (json_object_object_get_ex(response, "id", &response_id)) {
        if (json_object_get_int(response_id) != 1) {
            fprintf(stderr, "[LSP] Received response with wrong ID during initialization\n");
            json_object_put(response);
            return false;
        }
    }

    /* Check for error response */
    json_object* error = NULL;
    if (json_object_object_get_ex(response, "error", &error)) {
        json_object* code = NULL;
        json_object* message = NULL;
        
        json_object_object_get_ex(error, "code", &code);
        json_object_object_get_ex(error, "message", &message);
        
        fprintf(stderr, "[LSP] Initialization error (%d): %s\n",
                code ? json_object_get_int(code) : -1,
                message ? json_object_get_string(message) : "Unknown error");
        
        json_object_put(response);
        return false;
    }

    /* Process server capabilities */
    json_object* result = NULL;
    if (!json_object_object_get_ex(response, "result", &result)) {
        fprintf(stderr, "[LSP] No result in initialization response\n");
        json_object_put(response);
        return false;
    }

    /* Store server capabilities */
    json_object* server_caps = NULL;
    if (json_object_object_get_ex(result, "capabilities", &server_caps)) {
        if (client->server_capabilities) {
            json_object_put(client->server_capabilities);
        }
        client->server_capabilities = json_object_get(server_caps);
        
        /* Debug: Print server capabilities */
        fprintf(stderr, "[LSP] Server capabilities:\n%s\n",
                json_object_to_json_string_ext(server_caps, JSON_C_TO_STRING_PRETTY));
    }

    client->initialized = true;
    json_object_put(response);

    /* Send initialized notification */
    json_object* initialized = json_object_new_object();
    json_object_object_add(initialized, "jsonrpc", json_object_new_string("2.0"));
    json_object_object_add(initialized, "method", json_object_new_string("initialized"));
    json_object_object_add(initialized, "params", json_object_new_object());
    
    if (!lsp_send_request(client, initialized)) {
        fprintf(stderr, "[LSP] Warning: Failed to send initialized notification\n");
    }
    json_object_put(initialized);

    /* Wait briefly to ensure server is ready */
    struct timespec delay = {.tv_sec = 0, .tv_nsec = 200000000}; // 200ms
    nanosleep(&delay, NULL);

    return true;
}


/**
   Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.
*/
void start_lsp(void) {
    if (!wm.activeWindow || !wm.activeWindow->buffer || !wm.activeWindow->buffer->path) {
        message("No active buffer with path");
        return;
    }

    // Initialize clients array if needed
    if (!lspClients) {
        lspClients = initLspClients(LSP_CLIENTS_INITIAL_CAPACITY);
        if (!lspClients) {
            message("Failed to initialize LSP clients array");
            return;
        }
    }

    Buffer* buffer = wm.activeWindow->buffer;
    const char* workspace_root = getProjectRoot(buffer->path);
    if (!workspace_root) {
        message("Could not determine project root");
        return;
    }

    const char* language_id = getFileExtension(buffer->path);
    if (!language_id) {
        message("Could not determine language ID");
        return;
    }

    // Check for existing client
    LspClient* client = findLspClientByWorkspace(lspClients, workspace_root);
    if (client) {
        if (client->initialized) {
            message("LSP already running for this workspace");
            return;
        }
        // Clean up existing but uninitialized client
        removeLspClient(lspClients, client);
    }

    // Create new client
    client = addLspClient(lspClients, workspace_root, language_id, &lspConfig);
    if (!client) {
        message("Failed to create LSP client");
        return;
    }

    // Verify server is running
    if (!lsp_client_running(client)) {
        message("Failed to start language server");
        removeLspClient(lspClients, client);
        return;
    }

    // Initialize session
    if (initialize_lsp_session(client)) {
        lsp_notify_did_open(buffer); // And the other files ?
        message("LSP initialized for %s", workspace_root);
    } else {
        message("LSP initialization failed");
        removeLspClient(lspClients, client);
    }
}


char* uri_to_path(const char* uri) {
    if (!uri) return NULL;

    // Handle non-URI paths directly
    if (strncmp(uri, "file://", 7) != 0) {
        return strdup(uri);
    }

    // Handle Windows paths (file:///C:/path)
    bool is_windows_path = false;
    const char* src = uri + 7; // Skip file://
    
    // Check for Windows drive letter pattern
    if (src[0] == '/' && isalpha(src[1])) {
        is_windows_path = true;
        src++; // Skip the extra slash
    }

    // Calculate maximum possible path length
    size_t path_len = strlen(uri) + 1;
    char* path = malloc(path_len);
    if (!path) return NULL;

    char* dest = path;
    
    // For Windows, preserve the drive letter colon
    if (is_windows_path) {
        *dest++ = *src++; // Drive letter
        *dest++ = ':';
    }

    while (*src) {
        if (*src == '%' && isxdigit(src[1]) && isxdigit(src[2])) {
            // Decode percent-encoded sequence
            char hex[3] = {src[1], src[2], '\0'};
            *dest++ = (char)strtol(hex, NULL, 16);
            src += 3;
        } else {
            // Copy character directly (convert slash to backslash on Windows)
            *dest++ = (is_windows_path && *src == '/') ? '\\' : *src;
            src++;
        }
    }
    *dest = '\0';

    return path;
}





/**
 * Convert a filesystem path to a properly formatted file:// URI
 * Handles:
 * - Absolute path resolution
 * - Tilde expansion
 * - Windows path conversion
 * - Proper percent-encoding
 * - UTF-8 characters
 */
char* path_to_uri(const char* path) {
    if (!path) {
        return NULL;
    }

    // Step 1: Expand ~ to home directory if needed
    char* expanded = expand_path(path);
    if (!expanded) {
        return NULL;
    }

    // Step 2: Get absolute canonical path
    char* absolute = realpath(expanded, NULL);
    free(expanded);
    if (!absolute) {
        return NULL;
    }

    // Step 3: Calculate maximum possible URI length
    // (each byte could become %XX, plus 7 for "file://")
    size_t uri_len = 7; // "file://"
    for (const char* p = absolute; *p; p++) {
        if (*p == '\\') {
            uri_len++; // Will be converted to '/'
        } else if (should_encode(*p)) {
            uri_len += 3; // %XX encoding
        } else {
            uri_len++;
        }
    }
    uri_len += 1; // null terminator

    // Step 4: Build URI
    char* uri = malloc(uri_len);
    if (!uri) {
        free(absolute);
        return NULL;
    }

    strcpy(uri, "file://");
    char* dest = uri + 7;

    for (const char* src = absolute; *src; src++) {
        unsigned char c = *src;

        // Convert Windows backslashes to forward slashes
        if (c == '\\') {
            *dest++ = '/';
        }
        // Percent-encode special characters (but leave / unencoded)
        else if (should_encode(c) && c != '/') {
            sprintf(dest, "%%%02X", c);
            dest += 3;
        }
        // Copy regular characters directly
        else {
            *dest++ = c;
        }
    }
    *dest = '\0';

    free(absolute);
    return uri;
}

/**
 * Helper function to determine if a character should be percent-encoded
 */
static bool should_encode(unsigned char c) {
    // Don't encode unreserved characters (RFC 3986)
    if ((c >= 'a' && c <= 'z') || 
        (c >= 'A' && c <= 'Z') || 
        (c >= '0' && c <= '9') ||
        c == '-' || c == '_' || c == '.' || c == '~' || c == '/') {
        return false;
    }
    return true;
}

/**
 * Send an LSP request with proper framing and error handling
 */
bool lsp_send_request(LspClient* client, json_object* request) {
    if (!client || !request || client->server_stdin == -1) {
        fprintf(stderr, "[LSP] Invalid parameters for send_request\n");
        return false;
    }

    /* Install SIGPIPE handler to detect broken pipe */
    struct sigaction sa, old_sa;
    sa.sa_handler = handle_sigpipe;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGPIPE, &sa, &old_sa);
    g_got_sigpipe = 0;

    /* Serialize request to JSON */
    const char *request_str = json_object_to_json_string_ext(request, JSON_C_TO_STRING_PLAIN);
    if (!request_str) {
        fprintf(stderr, "[LSP] Failed to serialize request to JSON\n");
        sigaction(SIGPIPE, &old_sa, NULL);
        return false;
    }

    size_t request_len = strlen(request_str);
    
    /* Prepare Content-Length header */
    char header[128];
    int header_len = snprintf(header, sizeof(header), 
                           "Content-Length: %zu\r\n\r\n", request_len);
    if (header_len < 0 || header_len >= (int)sizeof(header)) {
        fprintf(stderr, "[LSP] Failed to format header\n");
        sigaction(SIGPIPE, &old_sa, NULL);
        return false;
    }

    /* Lock the client mutex for the entire send operation */
    pthread_mutex_lock(&client->request_mutex);

    /* Write header */
    ssize_t written = 0;
    while (written < header_len && !g_got_sigpipe) {
        ssize_t rc = write(client->server_stdin, header + written, header_len - written);
        if (rc <= 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) continue;
            break;
        }
        written += rc;
    }

    /* Write body */
    written = 0;
    while (written < request_len && !g_got_sigpipe) {
        ssize_t rc = write(client->server_stdin, request_str + written, request_len - written);
        if (rc <= 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) continue;
            break;
        }
        written += rc;
    }

    /* Restore signal handler */
    sigaction(SIGPIPE, &old_sa, NULL);
    pthread_mutex_unlock(&client->request_mutex);

    /* Check for errors */
    if (g_got_sigpipe || written < request_len) {
        fprintf(stderr, "[LSP] Write failed: %s\n", 
                g_got_sigpipe ? "broken pipe" : strerror(errno));
        
        /* Mark client as disconnected */
        client->server_stdin = -1;
        return false;
    }

    /* Debug: Log outgoing request */
    fprintf(stderr, "[LSP] Sent request:\n%s\n", 
            json_object_to_json_string_ext(request, JSON_C_TO_STRING_PRETTY));

    return true;
}

json_object* lsp_read_response(LspClient* client, int timeout_ms) {
    if (!client || client->server_stdout == -1) {
        return NULL;
    }

    fd_set read_fds;
    struct timeval timeout = {
        .tv_sec = timeout_ms / 1000,
        .tv_usec = (timeout_ms % 1000) * 1000
    };

    // Read header
    char header[256] = {0};
    size_t header_len = 0;
    size_t content_length = 0;

    while (header_len < sizeof(header) - 1) {
        FD_ZERO(&read_fds);
        FD_SET(client->server_stdout, &read_fds);

        int ready = select(client->server_stdout + 1, &read_fds, NULL, NULL, &timeout);
        if (ready <= 0) {
            fprintf(stderr, "LSP header read timeout\n");
            return NULL;
        }

        ssize_t n = read(client->server_stdout, header + header_len, 1);
        if (n <= 0) {
            fprintf(stderr, "Failed to read from LSP server\n");
            return NULL;
        }

        header_len += n;
        header[header_len] = '\0';

        // Check for end of headers
        if (header_len >= 4 && strcmp(header + header_len - 4, "\r\n\r\n") == 0) {
            break;
        }
    }

    // Parse Content-Length
    char* content_length_start = strstr(header, "Content-Length:");
    if (!content_length_start) {
        fprintf(stderr, "No Content-Length in LSP response\n");
        return NULL;
    }

    content_length = strtoul(content_length_start + 15, NULL, 10);
    if (content_length == 0) {
        fprintf(stderr, "Invalid Content-Length\n");
        return NULL;
    }

    // Read message content
    char* content = malloc(content_length + 1);
    if (!content) {
        fprintf(stderr, "Memory allocation failed\n");
        return NULL;
    }

    size_t bytes_read = 0;
    while (bytes_read < content_length) {
        FD_ZERO(&read_fds);
        FD_SET(client->server_stdout, &read_fds);

        int ready = select(client->server_stdout + 1, &read_fds, NULL, NULL, &timeout);
        if (ready <= 0) {
            fprintf(stderr, "LSP content read timeout\n");
            free(content);
            return NULL;
        }

        ssize_t n = read(client->server_stdout, content + bytes_read, content_length - bytes_read);
        if (n <= 0) {
            fprintf(stderr, "Failed to read content from LSP server\n");
            free(content);
            return NULL;
        }

        bytes_read += n;
    }
    content[content_length] = '\0';

    // Parse JSON
    json_object* json = json_tokener_parse(content);
    free(content);

    if (!json) {
        fprintf(stderr, "Failed to parse LSP response\n");
        return NULL;
    }

    return json;
}

/**
 * Jump to the definition of the symbol at point using LSP.
 * Handles path resolution, client lookup, and error cases robustly.
 */
void goto_definition(Buffer* buffer) {
    if (!buffer || !buffer->path) return;

    LspClient* client = get_client_for_current_buffer();
    if (!client || !client->initialized) return;

    char* uri = path_to_uri(buffer->path);
    if (!uri) return;

    int line, character;
    get_lsp_line_and_char(buffer, buffer->point, &line, &character);

    // Build request
    json_object* request = json_object_new_object();
    json_object_object_add(request, "jsonrpc", json_object_new_string("2.0"));
    json_object_object_add(request, "id", json_object_new_int(get_next_request_id(client)));
    json_object_object_add(request, "method", json_object_new_string("textDocument/definition"));

    // Build params
    json_object* params = json_object_new_object();
    
    // textDocument
    json_object* textDocument = json_object_new_object();
    json_object_object_add(textDocument, "uri", json_object_new_string(uri));
    json_object_object_add(params, "textDocument", textDocument);

    // position
    json_object* position = json_object_new_object();
    json_object_object_add(position, "line", json_object_new_int(line));
    json_object_object_add(position, "character", json_object_new_int(character));
    json_object_object_add(params, "position", position);
    
    json_object_object_add(request, "params", params);
    free(uri);

    // Debug print
    fprintf(stderr, "[LSP DEBUG] Sending definition request:\n%s\n",
            json_object_to_json_string_ext(request, JSON_C_TO_STRING_PRETTY));

    // Send and wait for response
    if (!lsp_send_request(client, request)) {
        json_object_put(request);
        message("Failed to send definition request");
        return;
    }

    json_object* response = lsp_read_response(client, client->config->timeout_ms);
    json_object_put(request);

    if (!response) {
        message("No response from server");
        return;
    }

    // Verify response ID matches our request
    json_object* response_id = NULL;
    if (json_object_object_get_ex(response, "id", &response_id)) {
        int received_id = json_object_get_int(response_id);
        int expected_id = client->request_id_counter - 1;
        if (received_id != expected_id) {
            fprintf(stderr, "[LSP WARNING] Response ID mismatch: expected %d, got %d\n",
                    expected_id, received_id);
        }
    }

    handle_definition_response(response, buffer);
    json_object_put(response);
}

void handle_definition_result(json_object* result, Buffer* buffer) {
    if (!result) return;

    // Handle both array and single location responses
    json_object* locations = result;
    if (json_object_get_type(result) != json_type_array) {
        locations = json_object_new_array();
        json_object_array_add(locations, result);
    }

    int count = json_object_array_length(locations);
    if (count == 0) {
        message("No definition found");
        if (json_object_get_type(result) != json_type_array) {
            json_object_put(locations);
        }
        return;
    }

    // Get first definition (could implement UI for multiple later)
    json_object* first_def = json_object_array_get_idx(locations, 0);
    
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
                    
                    // Open the target file and navigate to position
                    Buffer* target_buffer = getBuffer(&bm, target_path);
                    if (!target_buffer) {
                        newBuffer(&bm, &wm, target_path, target_path, NULL);
                        target_buffer = getBuffer(&bm, target_path);
                    }
                    
                    if (target_buffer) {
                        switchToBuffer(&bm, target_buffer->name);
                        // Convert to 1-indexed for editor (LSP uses 0-indexed)
                        moveTo(target_buffer, target_line + 1, target_char + 1);
                        message("Jumped to definition");
                    } else {
                        message("Could not open file: %s", target_path);
                    }
                }
            }
            free(target_path);
        }
    }

    if (json_object_get_type(result) != json_type_array) {
        json_object_put(locations);
    }
}

// Helper to get character position in line (0-indexed)
int get_character_position(Buffer* buffer, size_t point, int line) {
    int current_line = 0;
    size_t line_start = 0;
    
    for (size_t i = 0; i < point && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            if (current_line == line) {
                return point - line_start;
            }
            current_line++;
            line_start = i + 1;
        }
    }
    
    return point - line_start;
}


/**
 * Check if the LSP server process is running
 * @param client The LSP client to check
 * @return true if the server process is running, false otherwise
 */
bool lsp_client_running(LspClient *client) {
    if (!client || client->server_pid == -1) {
        return false;
    }

    // Check if process exists by sending signal 0
    if (kill(client->server_pid, 0) == -1) {
        if (errno == ESRCH) {
            // Process doesn't exist
            return false;
        }
        // Other error - assume process exists
        return true;
    }
    return true;
}

void lsp_notify_did_open(Buffer* buffer) {
    LspClient* client = get_client_for_current_buffer();
    if (!client || !client->initialized) return;

    char* uri = path_to_uri(buffer->path);
    if (!uri) return;

    json_object* params = json_object_new_object();
    json_object* textDocument = json_object_new_object();
    json_object_object_add(textDocument, "uri", json_object_new_string(uri));
    json_object_object_add(textDocument, "languageId", json_object_new_string(client->language_id));
    json_object_object_add(textDocument, "version", json_object_new_int(1));
    json_object_object_add(textDocument, "text", json_object_new_string(buffer->content));
    json_object_object_add(params, "textDocument", textDocument);

    json_object* notif = json_object_new_object();
    json_object_object_add(notif, "jsonrpc", json_object_new_string("2.0"));
    json_object_object_add(notif, "method", json_object_new_string("textDocument/didOpen"));
    json_object_object_add(notif, "params", params);

    lsp_send_request(client, notif);
    json_object_put(notif);
    free(uri);
}

void lsp_notify_did_change(Buffer* buffer) {
    LspClient* client = get_client_for_current_buffer();
    if (!client || !client->initialized) return;

    char* uri = path_to_uri(buffer->path);
    if (!uri) return;

    json_object* params = json_object_new_object();
    json_object* textDocument = json_object_new_object();
    json_object_object_add(textDocument, "uri", json_object_new_string(uri));
    json_object_object_add(textDocument, "version", json_object_new_int(buffer->version++));

    json_object* contentChanges = json_object_new_array();
    json_object* change = json_object_new_object();
    json_object_object_add(change, "text", json_object_new_string(buffer->content));
    json_object_array_add(contentChanges, change);

    json_object_object_add(params, "textDocument", textDocument);
    json_object_object_add(params, "contentChanges", contentChanges);

    json_object* notif = json_object_new_object();
    json_object_object_add(notif, "jsonrpc", json_object_new_string("2.0"));
    json_object_object_add(notif, "method", json_object_new_string("textDocument/didChange"));
    json_object_object_add(notif, "params", params);

    lsp_send_request(client, notif);
    json_object_put(notif);
    free(uri);
}

/**
 * Calculate the 0-based UTF-16 code unit offset (LSP position) 
 * for a buffer position
 */
int get_lsp_position(Buffer* buffer, size_t point) {
    if (!buffer || point > buffer->size) return 0;
    
    int utf16_offset = 0;
    for (size_t i = 0; i < point; i++) {
        unsigned char c = buffer->content[i];
        
        // Basic ASCII (1 UTF-16 code unit)
        if (c < 0x80) {
            utf16_offset++;
        } 
        // UTF-8 continuation bytes (already counted)
        else if ((c & 0xC0) == 0x80) {
            continue;
        }
        // 2-byte UTF-8 (1 UTF-16 code unit)
        else if ((c & 0xE0) == 0xC0) {
            utf16_offset++;
        }
        // 3-byte UTF-8 (1 UTF-16 code unit unless surrogate)
        else if ((c & 0xF0) == 0xE0) {
            utf16_offset++;
        }
        // 4-byte UTF-8 (2 UTF-16 code units - surrogate pair)
        else if ((c & 0xF8) == 0xF0) {
            utf16_offset += 2;
        }
    }
    
    return utf16_offset;
}

/**
 * Get LSP-style position (0-based line and UTF-16 offset)
 */
void get_lsp_line_and_char(Buffer* buffer, size_t point, 
                          int* out_line, int* out_char) {
    if (!buffer || !out_line || !out_char) return;
    
    *out_line = 0;
    *out_char = 0;
    
    size_t line_start = 0;
    for (size_t i = 0; i < point && i < buffer->size; i++) {
        if (buffer->content[i] == '\n') {
            (*out_line)++;
            line_start = i + 1;
        }
    }
    
    *out_char = get_lsp_position(buffer, point) - 
                get_lsp_position(buffer, line_start);
}

/**
 * Resolve a file path with full error reporting
 */
static char* resolve_file_path(const char* path) {
    if (!path) {
        fprintf(stderr, "resolve_file_path: NULL input path\n");
        return NULL;
    }

    // Expand tilde first
    char* expanded = expand_path(path);
    if (!expanded) {
        fprintf(stderr, "resolve_file_path: expand_path failed for '%s'\n", path);
        return NULL;
    }

    char* abs_path = realpath(expanded, NULL);
    free(expanded);

    if (!abs_path) {
        fprintf(stderr, "resolve_file_path: realpath failed for expanded path '%s': %s\n", 
                path, strerror(errno));
        return NULL;
    }

    /* fprintf(stderr, "resolve_file_path: '%s' -> '%s'\n", path, abs_path); */
    return abs_path;
}

/**
 * Handle the definition response from the LSP server.
 * Processes both single locations and arrays of locations.
 * Provides detailed error messages and navigates to the first definition.
 */
void handle_definition_response(json_object* response, Buffer* buffer) {
    if (!response) {
        message("No response from server");
        return;
    }

    // Debug: Print raw response
    const char* response_str = json_object_to_json_string_ext(response, JSON_C_TO_STRING_PRETTY);
    fprintf(stderr, "[LSP] Definition response:\n%s\n", response_str);

    // Check for error response first
    json_object* error = NULL;
    if (json_object_object_get_ex(response, "error", &error)) {
        handle_lsp_error(error);
        return;
    }

    // Get result object
    json_object* result = NULL;
    if (!json_object_object_get_ex(response, "result", &result)) {
        message("No definition found (empty result)");
        return;
    }

    // Handle both single location and array response
    json_object* locations = NULL;
    enum json_type result_type = json_object_get_type(result);
    
    if (result_type == json_type_array) {
        locations = json_object_get(result);
    } else if (result_type == json_type_object) {
        // Convert single location to array
        locations = json_object_new_array();
        json_object_array_add(locations, json_object_get(result));
    } else {
        message("Invalid definition response format");
        return;
    }

    // Check if we got any locations
    int location_count = json_object_array_length(locations);
    if (location_count == 0) {
        message("No definition locations found");
        if (result_type == json_type_object) {
            json_object_put(locations); // Clean up our temporary array
        }
        return;
    }

    // Process first location (could extend to handle multiple later)
    json_object* first_location = json_object_array_get_idx(locations, 0);
    if (!first_location) {
        message("Invalid location data");
        if (result_type == json_type_object) {
            json_object_put(locations);
        }
        return;
    }

    // Extract location details
    json_object *uri_obj = NULL, *range_obj = NULL;
    if (!json_object_object_get_ex(first_location, "uri", &uri_obj) ||
        !json_object_object_get_ex(first_location, "range", &range_obj)) {
        message("Invalid location format");
        if (result_type == json_type_object) {
            json_object_put(locations);
        }
        return;
    }

    // Convert URI to filesystem path
    const char* uri = json_object_get_string(uri_obj);
    char* file_path = uri_to_path(uri);
    if (!file_path) {
        message("Could not convert URI to path");
        if (result_type == json_type_object) {
            json_object_put(locations);
        }
        return;
    }

    // Get position from range
    json_object *start_obj = NULL;
    if (!json_object_object_get_ex(range_obj, "start", &start_obj)) {
        message("No position in definition");
        free(file_path);
        if (result_type == json_type_object) {
            json_object_put(locations);
        }
        return;
    }

    json_object *line_obj = NULL, *char_obj = NULL;
    if (!json_object_object_get_ex(start_obj, "line", &line_obj) ||
        !json_object_object_get_ex(start_obj, "character", &char_obj)) {
        message("Invalid position format");
        free(file_path);
        if (result_type == json_type_object) {
            json_object_put(locations);
        }
        return;
    }

    // Convert from LSP (0-based) to editor (1-based) line numbers
    int line = json_object_get_int(line_obj) + 1;
    int column = json_object_get_int(char_obj) + 1;

    // Try to open the target file
    Buffer* target_buffer = getBuffer(&bm, file_path);
    if (!target_buffer) {
        // Create new buffer if it doesn't exist
        newBuffer(&bm, &wm, file_path, file_path, NULL);
        target_buffer = getBuffer(&bm, file_path);
    }

    if (target_buffer) {
        // Switch to the buffer and move to definition
        switchToBuffer(&bm, target_buffer->name);
        moveTo(target_buffer, line, column);
        message("Jumped to definition");
    } else {
        message("Could not open file: %s", file_path);
    }

    // Clean up
    free(file_path);
    if (result_type == json_type_object) {
        json_object_put(locations);
    }
}


void handle_lsp_error(json_object* error_obj) {
    const char* error_msg = "Unknown LSP error";
    json_object* message_obj = NULL;
    
    if (json_object_object_get_ex(error_obj, "message", &message_obj)) {
        error_msg = json_object_get_string(message_obj);
    }
    
    fprintf(stderr, "[LSP] Server error: %s\n", error_msg);
    show_user_message("LSP Error: %s", error_msg);
}

json_object* prepare_locations_array(json_object* result_obj) {
    if (json_object_get_type(result_obj) == json_type_array) {
        return json_object_get(result_obj); // Already an array
    }
    
    // Convert single location to array
    json_object* array = json_object_new_array();
    if (array) {
        json_object_array_add(array, json_object_get(result_obj));
    }
    return array;
}

bool extract_location_info(json_object* location_obj, LocationInfo* out) {
    json_object *uri_obj = NULL, *range_obj = NULL;
    
    if (!json_object_object_get_ex(location_obj, "uri", &uri_obj) ||
        !json_object_object_get_ex(location_obj, "range", &range_obj)) {
        return false;
    }

    // Convert URI to filesystem path
    const char* uri = json_object_get_string(uri_obj);
    out->path = uri_to_path(uri);
    if (!out->path) {
        fprintf(stderr, "[LSP] Failed to convert URI: %s\n", uri);
        return false;
    }

    // Extract position
    json_object *start_obj = NULL;
    if (json_object_object_get_ex(range_obj, "start", &start_obj)) {
        json_object *line_obj = NULL, *char_obj = NULL;
        if (json_object_object_get_ex(start_obj, "line", &line_obj) &&
            json_object_object_get_ex(start_obj, "character", &char_obj)) {
            // Convert from 0-based (LSP) to 1-based (editor)
            out->line = json_object_get_int(line_obj) + 1;
            out->character = json_object_get_int(char_obj) + 1;
            return true;
        }
    }
    
    free(out->path);
    return false;
}

/**
 * Navigate to the definition location in editor
 */
void navigate_to_definition(const char* path, int line, int col) {
    fprintf(stderr, "[LSP] Navigating to: %s:%d:%d\n", path, line, col);
    
    Buffer* target = getBuffer(&bm, path);
    if (!target) {
        fprintf(stderr, "[LSP] Opening new buffer for: %s\n", path);
        newBuffer(&bm, &wm, path, path, NULL);
        target = getBuffer(&bm, path);
    }

    if (target) {
        switchToBuffer(&bm, target->name);
        moveTo(target, line, col);
        show_user_message("Jumped to definition");
    } else {
        fprintf(stderr, "[LSP] Failed to open: %s\n", path);
        show_user_message("Could not open file: %s", path);
    }
}

/**
 * Show message to user (wrapper around your message system)
 */
void show_user_message(const char* format, ...) {
    char buffer[256];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    
    message(buffer); // Calls your existing message function
}


bool check_server_capabilities(LspClient* client, json_object* response) {
    json_object* result = NULL;
    if (!json_object_object_get_ex(response, "result", &result)) {
        return false;
    }

    json_object* capabilities = NULL;
    if (!json_object_object_get_ex(result, "capabilities", &capabilities)) {
        return false;
    }

    json_object* textDocument = NULL;
    if (!json_object_object_get_ex(capabilities, "textDocument", &textDocument)) {
        return false;
    }

    json_object* definition = NULL;
    if (!json_object_object_get_ex(textDocument, "definition", &definition)) {
        message("Server doesn't support go-to-definition");
        return false;
    }

    return true;
}


bool lsp_supports_feature(LspClient* client, const char* feature) {
    if (!client || !client->server_capabilities) return false;
    
    // First check textDocument capabilities
    json_object* textDocument = NULL;
    if (json_object_object_get_ex(client->server_capabilities, "textDocument", &textDocument)) {
        json_object* provider = NULL;
        if (json_object_object_get_ex(textDocument, feature, &provider)) {
            return true;
        }
    }
    
    // Then check root capabilities
    json_object* provider = NULL;
    if (json_object_object_get_ex(client->server_capabilities, feature, &provider)) {
        return true;
    }
    
    return false;
}


int get_next_request_id(LspClient* client) {
    pthread_mutex_lock(&client->request_mutex);
    // Start from 2 since 1 is used for initialization
    int id = client->request_id_counter++;
    if (id == 1) id = client->request_id_counter++; // Skip 1
    pthread_mutex_unlock(&client->request_mutex);
    return id;
}

/* int get_next_request_id(LspClient* client) { */
/*     pthread_mutex_lock(&client->request_mutex); */
/*     int id = client->request_id_counter++; */
/*     pthread_mutex_unlock(&client->request_mutex); */
/*     return id; */
/* } */


static json_object* build_client_capabilities() {
    json_object* capabilities = json_object_new_object();
    
    /* Workspace capabilities */
    json_object* workspace = json_object_new_object();
    json_object_object_add(workspace, "applyEdit", json_object_new_boolean(true));
    json_object_object_add(workspace, "workspaceEdit", json_object_new_object());
    json_object_object_add(workspace, "didChangeConfiguration", json_object_new_object());
    json_object_object_add(workspace, "workspaceFolders", json_object_new_boolean(true));
    json_object_object_add(capabilities, "workspace", workspace);

    /* Text Document capabilities */
    json_object* textDocument = json_object_new_object();
    
    /* Synchronization */
    json_object* sync = json_object_new_object();
    json_object_object_add(sync, "dynamicRegistration", json_object_new_boolean(false));
    json_object_object_add(sync, "willSave", json_object_new_boolean(true));
    json_object_object_add(sync, "willSaveWaitUntil", json_object_new_boolean(false));
    json_object_object_add(sync, "didSave", json_object_new_boolean(true));
    json_object_object_add(textDocument, "synchronization", sync);

    /* Definition */
    json_object_object_add(textDocument, "definition", json_object_new_object());
    
    /* Other standard capabilities */
    json_object_object_add(textDocument, "completion", json_object_new_object());
    json_object_object_add(textDocument, "hover", json_object_new_object());
    json_object_object_add(textDocument, "signatureHelp", json_object_new_object());
    json_object_object_add(textDocument, "references", json_object_new_object());
    json_object_object_add(textDocument, "documentHighlight", json_object_new_object());
    json_object_object_add(textDocument, "documentSymbol", json_object_new_object());
    
    json_object_object_add(capabilities, "textDocument", textDocument);

    return capabilities;
}


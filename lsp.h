// lsp.h
#ifndef LSP_H
#define LSP_H

#include <sys/types.h>
#include <pthread.h>
#include "buffer.h"
#include <json-c/json.h>

typedef enum {
    LOG_LEVEL_ERROR,
    LOG_LEVEL_WARN,
    LOG_LEVEL_INFO,
    LOG_LEVEL_DEBUG
} LogLevel;

typedef struct {
    int timeout_ms;
    char* log_path;
    LogLevel log_level;
    char** server_argv;
} LspConfig;

typedef struct {
    pid_t server_pid;
    int server_stdin;
    int server_stdout;
    int server_stderr;
    char* workspace_root;
    char* language_id;
    LspConfig* config;
    pthread_t stderr_thread;
    Buffer* stderr_buffer;
    pthread_mutex_t stderr_mutex;
    bool initialized;
} LspClient;

extern LspConfig lspConfig;
extern LspClient *lspClient;

// Function declarations
LspClient* lsp_client_create(const char* workspace_root, const char* language_id, const LspConfig* config);
void lsp_client_destroy(LspClient* client);
bool lsp_client_running(LspClient *client);
bool lsp_initialize_session(LspClient* client);
void lsp_restart_server();
char* uri_to_path(const char* uri);
bool lsp_send_request(LspClient* client, json_object* request);
json_object* lsp_read_response(LspClient* client, int timeout_ms);
void lsp_goto_definition(Buffer *buffer);
bool initialize_lsp_session(LspClient* client);

LspClient* initLsp(const char* workspace_root, const char* language_id, const LspConfig* config);
void freeLsp(LspClient* client);
bool lspp(LspClient* client);
void goto_definition(Buffer* buffer);
void start_lsp();
void stop_lsp();

#endif // LSP_H

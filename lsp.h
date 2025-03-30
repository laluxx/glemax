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

// TODO
/* typedef struct { */
/*     LspConfig** configs; */
/*     size_t count; */
/*     size_t capacity; */
/* } LspConfigs;  */

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
    pthread_mutex_t request_mutex;  // New mutex for request counter
    int request_id_counter;         // New counter
    bool initialized;
    json_object* server_capabilities
} LspClient;

typedef struct {
    LspClient** clients;
    size_t count;
    size_t capacity;
} LspClients;


extern LspConfig lspConfig;
extern LspClients *lspClients;

#define LSP_CLIENTS_INITIAL_CAPACITY 5

LspClients* initLspClients(size_t initial_capacity);
void freeLspClients(LspClients* lsp_clients);

LspClient* addLspClient(LspClients* lsp_clients, 
                        const char* workspace_root, 
                        const char* language_id, 
                        const LspConfig* config);

bool removeLspClient(LspClients* lsp_clients, LspClient* client);
LspClient* findLspClientByWorkspace(LspClients* lsp_clients, const char* workspace_root);


LspClient* lsp_client_create(const char* workspace_root, const char* language_id, const LspConfig* config);
void lsp_client_destroy(LspClient* client);
bool lsp_client_running(LspClient *client);
bool lsp_initialize_session(LspClient* client);
void lsp_restart_server();
char* uri_to_path(const char* uri);
bool lsp_send_request(LspClient* client, json_object* request);
json_object* lsp_read_response(LspClient* client, int timeout_ms);
bool initialize_lsp_session(LspClient* client);

LspClient* initLsp(const char* workspace_root, const char* language_id, const LspConfig* config);
void freeLsp(LspClient* client);

void goto_definition(Buffer *buffer);
bool initialize_lsp_session(LspClient *client);
void start_lsp(void);
void stop_lsp(LspClient *client);
LspClient *get_client_for_current_buffer(void);
void handle_definition_result(json_object* result, Buffer* buffer);

bool lspp(void);
char* path_to_uri(const char* path);
bool lsp_client_running(LspClient *client);

void lsp_notify_did_open(Buffer *buffer);
void lsp_notify_did_change(Buffer* buffer);
void handle_definition_response(json_object* response, Buffer* buffer);

void lsp_notify_did_open(Buffer* buffer);
void lsp_notify_did_change(Buffer* buffer);

int get_lsp_position(Buffer *buffer, size_t point);
void get_lsp_line_and_char(Buffer *buffer, size_t point, int *out_line, int *out_char);
void handle_definition_response(json_object* response, Buffer* buffer);
static char* resolve_file_path(const char* path);

void handle_lsp_error(json_object *error_obj);
json_object *prepare_locations_array(json_object *result_obj);
void navigate_to_definition(const char* path, int line, int col);
void show_user_message(const char* format, ...);


typedef struct {
    char* path;
    int line;
    int character;
} LocationInfo;

bool extract_location_info(json_object *location_obj, LocationInfo *out);
char* expand_path(const char* path);
bool check_server_capabilities(LspClient* client, json_object* response);
bool lsp_supports_feature(LspClient* client, const char* feature);
static json_object *build_client_capabilities();
static json_object* build_client_capabilities();
static void handle_sigpipe(int sig);
int get_next_request_id(LspClient* client);
static bool should_encode(unsigned char c);
static void handle_diagnostics_notification(LspClient* client, json_object* notification);

#endif // LSP_H

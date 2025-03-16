#include "gemini.h"
#include <netdb.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include "buffer.h"
#include "globals.h"
#include <ctype.h>

#define INITIAL_BUF_SIZE 4096
#define MAX_HEADER_SIZE 1024 // TODO Message if we exceed it ?
#define GEMINI_PORT "1965"

static SSL_CTX *ssl_ctx = NULL;


bool gemini_redirect_other_window(Buffer *buffer) {
    char *line = getCurrentLine(buffer);
    char *url = NULL;
    char *link_text = NULL;

    // Look for "=>" which marks a link in Gemini format
    char *link_marker = strstr(line, "=>");
    if (!link_marker) {
        message("No link found in current line");
        return false;
    }

    // Move past the "=>" marker and any whitespace
    link_text = link_marker + 2;
    while (isspace(*link_text) && *link_text)
        link_text++;

    // Look for tab character which separates link and description
    char *tab = strchr(link_text, '\t');
    if (tab) {
        *tab = '\0'; // Temporarily terminate string at tab

        // Strip trailing whitespace from link
        char *end = tab - 1;
        while (end > link_text && isspace(*end))
            *end-- = '\0';

        if (*link_text) {
            url = strdup(link_text);
            if (!url) {
                message("Memory allocation failed");
                *tab = '\t'; // Restore tab character
                return false;
            }
        }
        *tab = '\t'; // Restore tab character
    } else {
        // No tab - extract first word as the URL
        char *end = link_text;
        while (*end && !isspace(*end))
            end++;

        // Temporarily terminate
        char saved = *end;
        *end = '\0';

        url = strdup(link_text);

        // Restore
        *end = saved;

        if (!url) {
            message("Memory allocation failed");
            return false;
        }
    }

    if (!url) {
        message("No link found in current line");
        return false;
    }

    // Check if this is a relative URL
    char *full_url = NULL;
    if (strstr(url, "://") == NULL) {
        // This is a relative URL, need to combine with base URL
        char *current_url = buffer->url;
        if (!current_url) {
            message("No base URL for relative link");
            free(url);
            return false;
        }

        // Find the last slash in the current URL
        char *last_slash = strrchr(current_url, '/');
        if (!last_slash) {
            // If no slash, just append the relative URL to the current URL
            size_t base_len = strlen(current_url);
            size_t rel_len = strlen(url);
            full_url =
                malloc(base_len + rel_len + 2); // +2 for '/' and null terminator

            if (!full_url) {
                message("Memory allocation failed");
                free(url);
                return false;
            }

            sprintf(full_url, "%s/%s", current_url, url);
        } else {
            // Replace everything after the last slash with the new URL
            size_t base_len = last_slash - current_url + 1; // +1 to include the slash
            size_t full_len = base_len + strlen(url) + 1;   // +1 for null terminator

            full_url = malloc(full_len);
            if (!full_url) {
                message("Memory allocation failed");
                free(url);
                return false;
            }

            strncpy(full_url, current_url, base_len);
            strcpy(full_url + base_len, url);
        }

        free(url);
        url = full_url;
    }

    message("Fetching link...");
    GeminiOutput go = gemini_fetch(url, buffer);

    if (go.size == 0) {
        message("Failed to fetch content");
        free(url);
        return false;
    }

    setBufferContent(buffer, go.content, false);

    // Update the buffer's URL to the new URL
    if (buffer->url) {
        free(buffer->url);
    }
    buffer->url = url; // Transfer ownership of url

    return true;
}


// TODO Extract the is_gemini_link(char *text) function
// We still can't render HTML, so we might want to see the raw HTML.
bool gemini_redirect(Buffer *buffer) {
    char *line = getCurrentLine(buffer);
    char *url = NULL;
    char *link_text = NULL;

    // Look for "=>" which marks a link in Gemini format
    char *link_marker = strstr(line, "=>");
    if (!link_marker) {
        message("No link found in current line");
        return false;
    }

    // Move past the "=>" marker and any whitespace
    link_text = link_marker + 2;
    while (isspace(*link_text) && *link_text)
        link_text++;

    // Look for tab character which separates link and description
    char *tab = strchr(link_text, '\t');
    if (tab) {
        *tab = '\0'; // Temporarily terminate string at tab

        // Strip trailing whitespace from link
        char *end = tab - 1;
        while (end > link_text && isspace(*end))
            *end-- = '\0';

        if (*link_text) {
            url = strdup(link_text);
            if (!url) {
                message("Memory allocation failed");
                *tab = '\t'; // Restore tab character
                return false;
            }
        }
        *tab = '\t'; // Restore tab character
    } else {
        // No tab - extract first word as the URL
        char *end = link_text;
        while (*end && !isspace(*end))
            end++;

        // Temporarily terminate
        char saved = *end;
        *end = '\0';

        url = strdup(link_text);

        // Restore
        *end = saved;

        if (!url) {
            message("Memory allocation failed");
            return false;
        }
    }

    if (!url) {
        message("No link found in current line");
        return false;
    }

    // Check if this is a relative URL
    char *full_url = NULL;
    if (strstr(url, "://") == NULL) {
        // This is a relative URL, need to combine with base URL
        char *current_url = buffer->url;
        if (!current_url) {
            message("No base URL for relative link");
            free(url);
            return false;
        }

        // Find the last slash in the current URL
        char *last_slash = strrchr(current_url, '/');
        if (!last_slash) {
            // If no slash, just append the relative URL to the current URL
            size_t base_len = strlen(current_url);
            size_t rel_len = strlen(url);
            full_url =
                malloc(base_len + rel_len + 2); // +2 for '/' and null terminator

            if (!full_url) {
                message("Memory allocation failed");
                free(url);
                return false;
            }

            sprintf(full_url, "%s/%s", current_url, url);
        } else {
            // Replace everything after the last slash with the new URL
            size_t base_len = last_slash - current_url + 1; // +1 to include the slash
            size_t full_len = base_len + strlen(url) + 1;   // +1 for null terminator

            full_url = malloc(full_len);
            if (!full_url) {
                message("Memory allocation failed");
                free(url);
                return false;
            }

            strncpy(full_url, current_url, base_len);
            strcpy(full_url + base_len, url);
        }

        free(url);
        url = full_url;
    }

    message("Fetching link...");
    GeminiOutput go = gemini_fetch(url, buffer);

    if (go.size == 0) {
        message("Failed to fetch content");
        free(url);
        return false;
    }

    setBufferContent(buffer, go.content, false);

    // Update the buffer's URL to the new URL
    if (buffer->url) {
        free(buffer->url);
    }
    buffer->url = url; // Transfer ownership of url

    return true;
}

void initOpenssl(void) {
    SSL_library_init();
    OpenSSL_add_all_algorithms();
    SSL_load_error_strings();
    ssl_ctx = SSL_CTX_new(TLS_client_method());
    SSL_CTX_set_min_proto_version(ssl_ctx, TLS1_2_VERSION);
    SSL_CTX_set_verify(ssl_ctx, SSL_VERIFY_NONE, NULL);
}

int gemini_parse_url(const char *url, GeminiUrl *result) {
    memset(result, 0, sizeof(*result));

    if (strncmp(url, "gemini://", 9) != 0)
        return 0;
    const char *start = url + 9;

    // Find host boundaries
    const char *host_end = strchrnul(start, '/');
    const char *colon = memchr(start, ':', host_end - start);

    // Extract host
    const char *host_start = start;
    size_t host_len =
        colon ? (size_t)(colon - start) : (size_t)(host_end - start);
    if (host_len >= sizeof(result->host))
        return 0;
    memcpy(result->host, host_start, host_len);
    result->host[host_len] = '\0';

    // Extract port if present
    if (colon && colon < host_end) {
        size_t port_len = (size_t)(host_end - colon - 1);
        if (port_len >= sizeof(result->port))
            return 0;
        memcpy(result->port, colon + 1, port_len);
        result->port[port_len] = '\0';
    } else {
        strncpy(result->port, GEMINI_PORT, sizeof(result->port));
    }

    // Extract path
    const char *path_start = host_end;
    if (*path_start == '\0') {
        strncpy(result->path, "/", sizeof(result->path));
    } else {
        size_t path_len = strcspn(path_start, "\r\n");
        if (path_len >= sizeof(result->path))
            return 0;
        memcpy(result->path, path_start, path_len);
        result->path[path_len] = '\0';
    }

    // Debug: Print parsed URL
    fprintf(stderr, "Parsed URL: host=%s, port=%s, path=%s\n", result->host,
            result->port, result->path);

    return 1;
}

static int connect_socket(const char *host, const char *port) {
    struct addrinfo hints = {.ai_family = AF_UNSPEC, .ai_socktype = SOCK_STREAM}, *res;

    if (getaddrinfo(host, port, &hints, &res)) return -1;
    
    int sockfd = -1;
    for (struct addrinfo* p = res; p; p = p->ai_next) {
        sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (sockfd == -1) continue;
        if (connect(sockfd, p->ai_addr, p->ai_addrlen) == 0) break;
        close(sockfd);
        sockfd = -1;
    }
    
    freeaddrinfo(res);
    return sockfd;
}

static SSL *setup_ssl(int sockfd, const char *host) {
    // Create a new SSL object
    SSL *ssl = SSL_new(ssl_ctx);
    if (!ssl) {
        fprintf(stderr, "Failed to create SSL structure\n");
        return NULL;
    }

    // Associate the socket with the SSL structure
    SSL_set_fd(ssl, sockfd);

    // Set the SNI (Server Name Indication) hostname
    if (SSL_set_tlsext_host_name(ssl, host) != 1) {
        fprintf(stderr, "Failed to set SNI hostname\n");
        SSL_free(ssl);
        return NULL;
    }

    // Perform the SSL/TLS handshake
    if (SSL_connect(ssl) != 1) {
        fprintf(stderr, "SSL handshake failed\n");
        ERR_print_errors_fp(stderr);
        SSL_free(ssl);
        return NULL;
    }

    return ssl;
}

static int send_request(SSL *ssl, const GeminiUrl *url) {
    // Construct the Gemini request URL
    char request[2048];

    // Correctly include the path in the request URL
    int len = snprintf(request, sizeof(request), "gemini://%s%s%s\r\n",
                       url->host,
                       (strcmp(url->port, GEMINI_PORT) ? ":" : "",
                        (strcmp(url->port, GEMINI_PORT) ? url->port : "")),
                       url->path);

    // Check for errors in snprintf
    if (len < 0 || len >= (int)sizeof(request)) {
        fprintf(stderr, "snprintf failed or request too long\n");
        return 0; // Failed to construct request
    }

    // Debug: Print the request
    fprintf(stderr, "Sending request: %s", request);

    // Send the request over SSL
    int total_sent = 0;
    while (total_sent < len) {
        int sent = SSL_write(ssl, request + total_sent, len - total_sent);
        if (sent <= 0) {
            int ssl_err = SSL_get_error(ssl, sent);
            if (ssl_err == SSL_ERROR_WANT_READ || ssl_err == SSL_ERROR_WANT_WRITE) {
                continue; // Retry (blocking mode should not hit this)
            } else {
                fprintf(stderr, "SSL_write failed: %d\n", ssl_err);
                return 0; // Fatal error
            }
        }
        total_sent += sent;
    }

    return 1; // Success
}

static int read_response(SSL *ssl, GeminiOutput *content,
                         GeminiUrl *redirect_url) {
    char header_buf[MAX_HEADER_SIZE + 1] = {0}; // +1 for null terminator
    size_t header_len = 0;
    char *crlf_pos = NULL;

    // Step 1: Read the header until CRLF is found or buffer is full
    while (header_len < MAX_HEADER_SIZE && !crlf_pos) {
        int bytes =
            SSL_read(ssl, header_buf + header_len, MAX_HEADER_SIZE - header_len);
        if (bytes <= 0) {
            int ssl_err = SSL_get_error(ssl, bytes);
            fprintf(stderr, "SSL_read failed: %d\n", ssl_err);
            return 0; // Error or premature close
        }
        header_len += bytes;
        header_buf[header_len] = '\0'; // Null-terminate for strstr
        crlf_pos = strstr(header_buf, "\r\n");
    }

    // Step 2: Validate the header
    if (!crlf_pos) {
        fprintf(stderr, "Header too large or no CRLF found\n");
        return 0; // Invalid header
    }

    // Debug: Print the received header
    fprintf(stderr, "Received header: %s\n", header_buf);

    // Step 3: Check the status code
    if (strncmp(header_buf, "20 ", 3) != 0) {
        // Handle redirects (status codes 30 and 31)
        if (strncmp(header_buf, "30 ", 3) == 0 ||
            strncmp(header_buf, "31 ", 3) == 0) {
            // Step 4: Extract the redirect URL from the META part of the header
            char *meta_start = header_buf + 3; // Skip "30 " or "31 "
            char *meta_end = crlf_pos;         // Points to '\r' in "\r\n"
            size_t meta_len = meta_end - meta_start;

            if (meta_len == 0) {
                fprintf(stderr, "Redirect URL is empty\n");
                return 0;
            }

            // Step 5: Copy the META (URL) into a temporary buffer
            char *url = malloc(meta_len + 1);
            if (!url) {
                fprintf(stderr, "malloc failed for redirect URL\n");
                return 0;
            }
            memcpy(url, meta_start, meta_len);
            url[meta_len] = '\0';

            // Step 6: Parse the redirect URL
            if (!gemini_parse_url(url, redirect_url)) {
                fprintf(stderr, "Failed to parse redirect URL: %s\n", url);
                free(url);
                return 0;
            }

            // Step 7: Do NOT append the original path to the redirect URL
            // The redirect URL's path should be used as-is
            free(url);

            // Debug: Print the redirect URL
            fprintf(stderr, "Redirecting to: %s%s\n", redirect_url->host,
                    redirect_url->path);
            return 2; // Indicate a redirect
        } else {
            // Handle other non-success status codes
            fprintf(stderr, "Invalid status code: %s\n", header_buf);
            return 0; // Non-success status code
        }
    }

    // Step 8: Prepare to read the body
    size_t body_start = crlf_pos + 2 - header_buf; // Skip "\r\n"
    size_t initial_body_len = header_len - body_start;

    // Step 9: Allocate memory for the content
    content->content = malloc(INITIAL_BUF_SIZE);
    if (!content->content) {
        fprintf(stderr, "malloc failed\n");
        return 0; // Memory allocation failed
    }

    content->size = 0;
    size_t capacity = INITIAL_BUF_SIZE;

    // Step 10: Copy initial body data from the header buffer
    if (initial_body_len > 0) {
        if (initial_body_len > capacity) {
            capacity = initial_body_len;
            char *new_data = realloc(content->content, capacity);
            if (!new_data) {
                fprintf(stderr, "realloc failed\n");
                free(content->content);
                content->content = NULL;
                return 0; // Memory allocation failed
            }
            content->content = new_data;
        }
        memcpy(content->content, header_buf + body_start, initial_body_len);
        content->size = initial_body_len;
    }

    // Step 11: Read the remaining body
    char buf[INITIAL_BUF_SIZE];
    while (1) {
        int bytes = SSL_read(ssl, buf, sizeof(buf));
        if (bytes < 0) {
            int ssl_err = SSL_get_error(ssl, bytes);
            fprintf(stderr, "SSL_read failed: %d\n", ssl_err);
            free(content->content);
            content->content = NULL;
            content->size = 0;
            return 0; // Error reading body
        } else if (bytes == 0) {
            // Connection closed
            break;
        } else {
            // Expand buffer if needed
            if (content->size + bytes > capacity) {
                capacity = content->size + bytes;
                char *new_data = realloc(content->content, capacity);
                if (!new_data) {
                    fprintf(stderr, "realloc failed\n");
                    free(content->content);
                    content->content = NULL;
                    content->size = 0;
                    return 0; // Memory allocation failed
                }
                content->content = new_data;
            }
            memcpy(content->content + content->size, buf, bytes);
            content->size += bytes;
        }
    }

    // Step 12: Null-terminate the content (optional)
    char *new_data = realloc(content->content, content->size + 1);
    if (new_data) {
        content->content = new_data;
        content->content[content->size] = '\0';
    }

    return 1; // Success
}

GeminiOutput gemini_fetch(const char *url, Buffer *buffer) {
    GeminiOutput result = {0};
    GeminiUrl parsed;
    GeminiUrl redirect_url;

    if (!gemini_parse_url(url, &parsed)) {
        fprintf(stderr, "Invalid URL: %s\n", url);
        return result;
    }

    int redirect_count = 0;
    while (redirect_count < max_gemini_redirections) { // Limit redirects to avoid infinite loops
        int sockfd = connect_socket(parsed.host, parsed.port);
        if (sockfd == -1) {
            perror("Connection failed");
            return result;
        }

        SSL *ssl = setup_ssl(sockfd, parsed.host);
        if (!ssl) {
            fprintf(stderr, "SSL setup failed\n");
            close(sockfd);
            return result;
        }

        if (!send_request(ssl, &parsed)) {
            SSL_shutdown(ssl);
            SSL_free(ssl);
            close(sockfd);
            gemini_free_content(&result);
            return (GeminiOutput){0};
        }

        int response_status = read_response(ssl, &result, &redirect_url);
        SSL_shutdown(ssl);
        SSL_free(ssl);
        close(sockfd);

        if (response_status == 1) {
            // Success
            buffer->url = strdup(url);
            return result;
        } else if (response_status == 2) {
            // Redirect
            redirect_count++;
            memcpy(&parsed, &redirect_url,
                   sizeof(parsed)); // Update URL for next request
        } else {
            // Error
            gemini_free_content(&result);
            return (GeminiOutput){0};
        }
    }

    fprintf(stderr, "Too many redirects\n");
    gemini_free_content(&result);
    return (GeminiOutput){0};
}

void gemini_free_content(GeminiOutput *content) {
    free(content->content);
    content->content = NULL;
    content->size = 0;
}

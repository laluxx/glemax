#include "git.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>
#include <pwd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// DIFF-HL

Diffs getDiffInfo(const char *filePath) {
    Diffs diffs = {NULL, 0, 0};
    char command[1024];
    FILE *fp;

    snprintf(command, sizeof(command), "git diff --no-color --unified=0 %s",
             filePath);
    fp = popen(command, "r");
    if (fp == NULL) {
        return diffs;
    }

    char line[256];
    int currentLine = 1;
    bool inHunk = false;
    int hunkNewStart = 0;
    int deletionCount = 0;

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (line[0] == '@' && line[1] == '@') {
            sscanf(line, "@@ -%*d,%*d +%d,%*d @@", &hunkNewStart);
            currentLine = hunkNewStart;
            inHunk = true;
            deletionCount = 0;
        } else if (inHunk) {
            if (diffs.count == diffs.capacity) {
                diffs.capacity = diffs.capacity == 0 ? 1 : diffs.capacity * 2;
                diffs.array = realloc(diffs.array, diffs.capacity * sizeof(DiffInfo));
            }

            DiffInfo diff = {currentLine, DIFF_NONE};
            switch (line[0]) {
            case '+':
                if (deletionCount > 0) {
                    diff.type = DIFF_MODIFIED;
                    deletionCount--;
                } else {
                    diff.type = DIFF_ADDED;
                }
                diffs.array[diffs.count++] = diff;
                currentLine++;
                break;
            case '-':
                diff.type = DIFF_DELETED;
                diff.line = currentLine + deletionCount;
                diffs.array[diffs.count++] = diff;
                deletionCount++;
                break;
            case ' ':
                currentLine++;
                deletionCount = 0;
                break;
            default:
                inHunk = false;
                break;
            }
        }
    }

    pclose(fp);
    return diffs;
}




char* getGitBranch(const char* relativePath) {
    char fullPath[1024];
    char* homedir;
    char* projectDir;
    char git_head_path[1024];
    char* branch_name = NULL;
    FILE* head_file;
    char line[256];

    // Expand the ~ to the full home directory path
    if (relativePath[0] == '~' && relativePath[1] == '/') {
        homedir = getenv("HOME");
        if (homedir == NULL) {
            homedir = getpwuid(getuid())->pw_dir;
        }
        snprintf(fullPath, sizeof(fullPath), "%s%s", homedir, relativePath + 1);
    } else {
        strncpy(fullPath, relativePath, sizeof(fullPath));
    }

    // Find the project directory (parent of the file)
    projectDir = strdup(fullPath);
    char* lastSlash = strrchr(projectDir, '/');
    if (lastSlash != NULL) {
        *lastSlash = '\0';  // Truncate at the last slash
    }

    // Construct the path to the .git/HEAD file
    snprintf(git_head_path, sizeof(git_head_path), "%s/.git/HEAD", projectDir);

    // Check if the .git directory exists
    struct stat st;
    if (stat(git_head_path, &st) == -1) {
        free(projectDir);
        return strdup("");  // Not a git repository (we do it like this for the modeline)
    }

    head_file = fopen(git_head_path, "r");
    if (head_file == NULL) {
        free(projectDir);
        return strdup("");  // Unable to open HEAD file
    }

    if (fgets(line, sizeof(line), head_file) != NULL) {
        char* ref_prefix = "ref: refs/heads/";
        if (strncmp(line, ref_prefix, strlen(ref_prefix)) == 0) {
            // Remove newline character if present
            char* newline = strchr(line, '\n');
            if (newline) *newline = '\0';

            branch_name = strdup(line + strlen(ref_prefix));
        }
    }

    fclose(head_file);
    free(projectDir);
    return branch_name ? branch_name : strdup("");
}

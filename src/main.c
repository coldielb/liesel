#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#if defined(_WIN32)
#ifndef PATH_MAX
#define PATH_MAX _MAX_PATH
#endif
#else
#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#endif
#include "liesel.h"

static void print_usage(const char *name) {
    fprintf(stderr, "Usage: %s <script.ls>\n", name);
    fprintf(stderr, "Try '%s --help' for more information.\n", name);
}

static char *resolve_executable_path(const char *path) {
#if defined(_WIN32)
    char buffer[PATH_MAX];
    if (_fullpath(buffer, path, PATH_MAX) != NULL) {
        return strdup(buffer);
    }
    return strdup(path);
#else
    char *resolved = realpath(path, NULL);
    if (resolved != NULL) {
        return resolved;
    }
    return strdup(path);
#endif
}

static void register_default_module_paths(const char *exec_path) {
    char *absolute = resolve_executable_path(exec_path);
    if (absolute == NULL) {
        return;
    }
    char *directory = strdup(absolute);
    free(absolute);
    if (directory == NULL) {
        return;
    }

    char *slash = strrchr(directory, '/');
#if defined(_WIN32)
    char *backslash = strrchr(directory, '\\');
    if (backslash != NULL && (slash == NULL || backslash > slash)) {
        slash = backslash;
    }
#endif
    if (slash == NULL) {
        lie_register_module_path(".");
        free(directory);
        return;
    }

    *slash = '\0';
    if (directory[0] == '\0') {
        lie_register_module_path("/");
    } else {
        lie_register_module_path(directory);
    }

    char *parent = strdup(directory);
    if (parent != NULL) {
        char *parent_slash = strrchr(parent, '/');
#if defined(_WIN32)
        char *parent_backslash = strrchr(parent, '\\');
        if (parent_backslash != NULL && (parent_slash == NULL || parent_backslash > parent_slash)) {
            parent_slash = parent_backslash;
        }
#endif
        if (parent_slash != NULL) {
            if (parent_slash == parent) {
                parent_slash[1] = '\0';
            } else {
                *parent_slash = '\0';
            }
            if (parent[0] != '\0') {
                lie_register_module_path(parent);
            }
        }
        free(parent);
    }

    free(directory);
}

static void print_help(const char *name) {
    printf("Liesel Interpreter\n\n");
    printf("Usage: %s [options] <script.ls> [args]\n\n", name);
    printf("Options:\n");
    printf("  -h, --help        Show this guide and exit.\n\n");
    printf("Getting Started:\n");
    printf("  - Place supporting libraries under 'libs/' and bring them in with 'gather <name>'.\n");
    printf("  - Run bundled samples such as 'examples/hello.ls' to verify your build.\n");
    printf("  - Scripts execute top-to-bottom; define an entry point with 'note main(): ...' and call it.\n\n");
    printf("Runtime Details:\n");
    printf("  - Library search checks the current workspace, the script directory lineage, registered runtime paths, and $LIESEL_HOME.\n");
    printf("  - Built-in diagnostics highlight the error line and offer recovery hints.\n\n");
    printf("Further Reading:\n");
    printf("  - docs/spec.md - authoritative language rules.\n");
    printf("  - docs/syntax-sheet.md - quick reference and idioms.\n");
}

int main(int argc, char **argv) {
    if (argc >= 1 && argv[0] != NULL) {
        register_default_module_paths(argv[0]);
    }

    if (argc >= 2 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0)) {
        print_help(argv[0]);
        return 0;
    }

    if (argc < 2) {
        print_usage(argv[0]);
        return 64;
    }

    const char *path = argv[1];
    int status = lie_run_file(path);
    if (status != 0) {
        fprintf(stderr, "Liesel runtime exited with status %d\n", status);
    }
    return status;
}

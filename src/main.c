#include <stdio.h>
#include "liesel.h"

static void print_usage(const char *name) {
    fprintf(stderr, "Usage: %s <script.ls>\n", name);
}

int main(int argc, char **argv) {
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

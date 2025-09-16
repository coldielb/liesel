#include "liesel/error.h"

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static const char *kind_label(LieErrorKind kind) {
    switch (kind) {
        case LIE_ERROR_LEX:
            return "Lex";
        case LIE_ERROR_PARSE:
            return "Parse";
        case LIE_ERROR_RUNTIME:
            return "Runtime";
        case LIE_ERROR_SYSTEM:
            return "System";
    }
    return "Error";
}

void lie_error_report(LieErrorKind kind, int line, const char *lexeme_start, size_t lexeme_length, const char *fmt, ...) {
    fprintf(stderr, "[line %d] %s error", line, kind_label(kind));
    if (lexeme_start != NULL && lexeme_length > 0) {
        fprintf(stderr, " at '%.*s'", (int)lexeme_length, lexeme_start);
    }
    fputs(": ", stderr);

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
}

void lie_error_system(const char *context, int errnum) {
    const char *message = strerror(errnum);
    fprintf(stderr, "[system] %s: %s\n", context, message ? message : "unknown error");
}

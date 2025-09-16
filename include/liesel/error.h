#ifndef LIE_ERROR_H
#define LIE_ERROR_H

#include <stddef.h>

typedef enum {
    LIE_ERROR_LEX,
    LIE_ERROR_PARSE,
    LIE_ERROR_RUNTIME,
    LIE_ERROR_SYSTEM
} LieErrorKind;

void lie_error_report(LieErrorKind kind, int line, const char *lexeme_start, size_t lexeme_length, const char *fmt, ...);
void lie_error_hint(const char *fmt, ...);
void lie_error_system(const char *context, int errnum);

#endif

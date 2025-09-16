#include "liesel.h"
#include "liesel/error.h"

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <limits.h>
#if defined(_WIN32)
#include <direct.h>
#define getcwd _getcwd
#else
#include <unistd.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define UNUSED(x) (void)(x)
#define CHECK_ALLOC(ptr)                     \
    do {                                         \
        if ((ptr) == NULL) {                     \
            lie_error_report(LIE_ERROR_SYSTEM, 0, NULL, 0,\
                             "Out of memory");                  \
            exit(74);                            \
        }                                        \
    } while (0)

static char *copy_string(const char *start, size_t length) {
    char *buffer = (char *)malloc(length + 1);
    CHECK_ALLOC(buffer);
    memcpy(buffer, start, length);
    buffer[length] = '\0';
    return buffer;
}

static char *duplicate_cstring(const char *text) {
    return copy_string(text, strlen(text));
}

static bool is_path_separator(char c) {
    return c == '/' || c == '\\';
}

static bool path_is_absolute(const char *path) {
    if (path == NULL || path[0] == '\0') {
        return false;
    }
#if defined(_WIN32)
    if (is_path_separator(path[0])) {
        return true;
    }
    return (strlen(path) > 1 && path[1] == ':');
#else
    return path[0] == '/';
#endif
}

static char *resolve_absolute_path(const char *path) {
    if (path == NULL) {
        return NULL;
    }
#if defined(_WIN32)
    if (path_is_absolute(path)) {
        return duplicate_cstring(path);
    }
#endif
#if !defined(_WIN32)
    char *resolved = realpath(path, NULL);
    if (resolved != NULL) {
        return resolved;
    }
#endif
    if (path_is_absolute(path)) {
        return duplicate_cstring(path);
    }

    char cwd[PATH_MAX];
    if (getcwd(cwd, sizeof(cwd)) == NULL) {
        return duplicate_cstring(path);
    }

    size_t cwd_len = strlen(cwd);
    bool need_sep = cwd_len > 0 && !is_path_separator(cwd[cwd_len - 1]);
    size_t path_len = strlen(path);
    size_t total = cwd_len + (need_sep ? 1 : 0) + path_len + 1;
    char *result = (char *)malloc(total);
    CHECK_ALLOC(result);
    memcpy(result, cwd, cwd_len);
    size_t pos = cwd_len;
    if (need_sep) {
        result[pos++] = '/';
    }
    memcpy(result + pos, path, path_len + 1);
    return result;
}

static bool path_is_root(const char *path) {
    if (path == NULL || path[0] == '\0') {
        return false;
    }
#if defined(_WIN32)
    if (path[0] == '\0') {
        return false;
    }
    if ((path[0] == '/' || path[0] == '\\') && path[1] == '\0') {
        return true;
    }
    if (strlen(path) == 2 && path[1] == ':') {
        return true;
    }
    if (strlen(path) == 3 && path[1] == ':' && is_path_separator(path[2])) {
        return true;
    }
    return false;
#else
    return path[0] == '/' && path[1] == '\0';
#endif
}

static char *path_dirname_copy(const char *path) {
    if (path == NULL) {
        return NULL;
    }
    size_t len = strlen(path);
    if (len == 0) {
        return NULL;
    }

    while (len > 1 && is_path_separator(path[len - 1])) {
        len--;
    }
    if (len == 0) {
        return NULL;
    }

    size_t i = len;
    while (i > 0) {
        if (is_path_separator(path[i - 1])) {
            break;
        }
        i--;
    }
    if (i == 0) {
#if defined(_WIN32)
        if (len >= 2 && path[1] == ':') {
            char *drive = (char *)malloc(3);
            CHECK_ALLOC(drive);
            drive[0] = path[0];
            drive[1] = ':';
            drive[2] = '\0';
            return drive;
        }
#endif
        return NULL;
    }

    size_t parent_len = i;
    while (parent_len > 1 && is_path_separator(path[parent_len - 1])) {
        parent_len--;
    }
#if defined(_WIN32)
    if (parent_len == 0 && is_path_separator(path[0])) {
        parent_len = 1;
    }
    if (parent_len == 2 && path[1] == ':') {
        parent_len = 2;
    }
#endif
    if (parent_len == 0) {
        parent_len = 1;
    }
    char *result = (char *)malloc(parent_len + 1);
    CHECK_ALLOC(result);
    memcpy(result, path, parent_len);
    result[parent_len] = '\0';
    return result;
}

/* ========================= LEXER ========================= */

typedef enum {
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    TOKEN_LET,
    TOKEN_SET,
    TOKEN_BE,
    TOKEN_TO,
    TOKEN_GATHER,
    TOKEN_TRUE,
    TOKEN_FALSE,
    TOKEN_NOTHING,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_NOT,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_IS,
    TOKEN_ISNT,
    TOKEN_IF,
    TOKEN_OTHERWISE,
    TOKEN_WHILST,
    TOKEN_NOTE,
    TOKEN_HALT,

    TOKEN_NEWLINE,
    TOKEN_INDENT,
    TOKEN_DEDENT,
    TOKEN_EOF,
    TOKEN_ERROR
} TokenType;

typedef struct {
    TokenType type;
    const char *start;
    size_t length;
    int line;
    const char *message;
} Token;

typedef struct {
    const char *source;
    const char *start;
    const char *current;
    int line;
    bool at_line_start;
    int indent_stack[256];
    size_t indent_depth;
    int pending_dedents;
} Lexer;

static void lexer_init(Lexer *lexer, const char *source) {
    lexer->source = source;
    lexer->start = source;
    lexer->current = source;
    lexer->line = 1;
    lexer->at_line_start = true;
    lexer->indent_stack[0] = 0;
    lexer->indent_depth = 1;
    lexer->pending_dedents = 0;
}

static bool lexer_is_at_end(const Lexer *lexer) {
    return *lexer->current == '\0';
}

static char lexer_advance(Lexer *lexer) {
    lexer->current++;
    return lexer->current[-1];
}

static char lexer_peek(const Lexer *lexer) {
    return *lexer->current;
}

static char lexer_peek_next(const Lexer *lexer) {
    if (lexer_is_at_end(lexer)) return '\0';
    return lexer->current[1];
}

static bool lexer_match(Lexer *lexer, char expected) {
    if (lexer_is_at_end(lexer)) return false;
    if (*lexer->current != expected) return false;
    lexer->current++;
    return true;
}

static Token make_token(Lexer *lexer, TokenType type) {
    Token token;
    token.type = type;
    token.start = lexer->start;
    token.length = (size_t)(lexer->current - lexer->start);
    token.line = lexer->line;
    token.message = NULL;
    return token;
}

static Token make_simple_token(const Lexer *lexer, TokenType type) {
    Token token;
    token.type = type;
    token.start = lexer->current;
    token.length = 0;
    token.line = lexer->line;
    token.message = NULL;
    return token;
}

static Token error_token(Lexer *lexer, const char *message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = lexer->start;
    token.length = 0;
    token.line = lexer->line;
    token.message = message;
    return token;
}

static void skip_comment_to_eol(Lexer *lexer) {
    while (lexer_peek(lexer) != '\n' && !lexer_is_at_end(lexer)) {
        lexer_advance(lexer);
    }
}

static Token string_token(Lexer *lexer) {
    while (lexer_peek(lexer) != '"' && !lexer_is_at_end(lexer)) {
        if (lexer_peek(lexer) == '\n') {
            lexer->line++;
        }
        if (lexer_peek(lexer) == '\\' && lexer_peek_next(lexer) != '\0') {
            lexer_advance(lexer);
        }
        lexer_advance(lexer);
    }

    if (lexer_is_at_end(lexer)) {
        return error_token(lexer, "Unterminated string");
    }

    lexer_advance(lexer);
    return make_token(lexer, TOKEN_STRING);
}

static bool is_identifier_start(char c) {
    return isalpha((unsigned char)c) || c == '_';
}

static bool is_identifier_part(char c) {
    return isalnum((unsigned char)c) || c == '_';
}

static TokenType keyword_type(const char *start, size_t length) {
    struct Keyword {
        const char *text;
        TokenType type;
    };
    static const struct Keyword keywords[] = {
        {"let", TOKEN_LET},
        {"set", TOKEN_SET},
        {"be", TOKEN_BE},
        {"to", TOKEN_TO},
        {"gather", TOKEN_GATHER},
        {"true", TOKEN_TRUE},
        {"false", TOKEN_FALSE},
        {"nothing", TOKEN_NOTHING},
        {"and", TOKEN_AND},
        {"or", TOKEN_OR},
        {"not", TOKEN_NOT},
        {"is", TOKEN_IS},
        {"isnt", TOKEN_ISNT},
        {"if", TOKEN_IF},
        {"otherwise", TOKEN_OTHERWISE},
        {"whilst", TOKEN_WHILST},
        {"note", TOKEN_NOTE},
        {"halt", TOKEN_HALT},
        {"break", TOKEN_BREAK},
        {"continue", TOKEN_CONTINUE},
    };
    for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
        if (length == strlen(keywords[i].text) && strncmp(start, keywords[i].text, length) == 0) {
            return keywords[i].type;
        }
    }
    return TOKEN_IDENTIFIER;
}

static Token identifier_token(Lexer *lexer) {
    while (true) {
        while (is_identifier_part(lexer_peek(lexer))) {
            lexer_advance(lexer);
        }
        if (lexer_peek(lexer) == ':' && lexer_peek_next(lexer) == ':') {
            const char *lookahead = lexer->current + 2;
            if (!is_identifier_start(*lookahead)) {
                break;
            }
            lexer->current += 2;
            while (is_identifier_part(lexer_peek(lexer))) {
                lexer_advance(lexer);
            }
        } else {
            break;
        }
    }

    size_t length = (size_t)(lexer->current - lexer->start);
    if (length == 7 && strncmp(lexer->start, "whisper", 7) == 0) {
        char next = lexer_peek(lexer);
        if (next == ' ' || next == '\t' || next == '\r' || next == '\n' || next == '\0') {
            skip_comment_to_eol(lexer);
            return make_token(lexer, TOKEN_NEWLINE);
        }
    }

    TokenType type = keyword_type(lexer->start, length);
    return make_token(lexer, type);
}

static Token number_token(Lexer *lexer) {
    while (isdigit((unsigned char)lexer_peek(lexer))) {
        lexer_advance(lexer);
    }
    if (lexer_peek(lexer) == '.' && isdigit((unsigned char)lexer_peek_next(lexer))) {
        lexer_advance(lexer);
        while (isdigit((unsigned char)lexer_peek(lexer))) {
            lexer_advance(lexer);
        }
    }
    return make_token(lexer, TOKEN_NUMBER);
}

static void skip_horizontal_space(Lexer *lexer) {
    while (true) {
        char c = lexer_peek(lexer);
        if (c == ' ' || c == '\t' || c == '\r') {
            lexer_advance(lexer);
        } else {
            break;
        }
    }
}

static Token handle_line_start(Lexer *lexer) {
    const char *line_start = lexer->current;
    int indent = 0;
    while (true) {
        char c = lexer_peek(lexer);
        if (c == ' ') {
            lexer_advance(lexer);
            indent++;
        } else if (c == '\t') {
            lexer->start = line_start;
            return error_token(lexer, "Tabs are not allowed for indentation");
        } else {
            break;
        }
    }

    int current = lexer->indent_stack[lexer->indent_depth - 1];
    char next = lexer_peek(lexer);
    if (next == '\n' || next == '\0') {
        lexer->start = line_start;
        lexer->at_line_start = false;
        return make_simple_token(lexer, TOKEN_NEWLINE);
    }
    if (indent > current) {
        lexer->indent_stack[lexer->indent_depth++] = indent;
        lexer->start = line_start;
        lexer->at_line_start = false;
        return make_simple_token(lexer, TOKEN_INDENT);
    }

    if (indent < current) {
        size_t depth = lexer->indent_depth;
        int dedents = 0;
        while (depth > 0 && indent < lexer->indent_stack[depth - 1]) {
            depth--;
            dedents++;
        }
        if (depth == 0 || indent != lexer->indent_stack[depth - 1]) {
            lexer->start = line_start;
            return error_token(lexer, "Inconsistent indentation");
        }
        lexer->indent_depth = depth;
        lexer->pending_dedents = dedents - 1;
        lexer->start = line_start;
        lexer->at_line_start = false;
        return make_simple_token(lexer, TOKEN_DEDENT);
    }

    lexer->start = line_start;
    lexer->at_line_start = false;
    lexer->start = lexer->current;
    return make_simple_token(lexer, TOKEN_NEWLINE); /* placeholder: caller ignores */
}

static Token lexer_next_token(Lexer *lexer) {
    if (lexer->pending_dedents > 0) {
        lexer->pending_dedents--;
        return make_simple_token(lexer, TOKEN_DEDENT);
    }

    if (lexer->at_line_start) {
        Token indent_token = handle_line_start(lexer);
        if (indent_token.type == TOKEN_INDENT || indent_token.type == TOKEN_DEDENT || indent_token.type == TOKEN_ERROR) {
            return indent_token;
        }
        /* if indent unchanged, we emitted a NEWLINE placeholder; fall through */
    }

    skip_horizontal_space(lexer);
    lexer->start = lexer->current;

    if (lexer_is_at_end(lexer)) {
        if (lexer->indent_depth > 1) {
            lexer->indent_depth--;
            return make_simple_token(lexer, TOKEN_DEDENT);
        }
        return make_token(lexer, TOKEN_EOF);
    }

    char c = lexer_advance(lexer);

    if (c == '\n') {
        lexer->line++;
        lexer->at_line_start = true;
        return make_token(lexer, TOKEN_NEWLINE);
    }

    if (is_identifier_start(c)) {
        return identifier_token(lexer);
    }

    if (isdigit((unsigned char)c)) {
        return number_token(lexer);
    }

    switch (c) {
        case '(':
            return make_token(lexer, TOKEN_LEFT_PAREN);
        case ')':
            return make_token(lexer, TOKEN_RIGHT_PAREN);
        case '[':
            return make_token(lexer, TOKEN_LEFT_BRACKET);
        case ']':
            return make_token(lexer, TOKEN_RIGHT_BRACKET);
        case '{':
            return make_token(lexer, TOKEN_LEFT_BRACE);
        case '}':
            return make_token(lexer, TOKEN_RIGHT_BRACE);
        case ',':
            return make_token(lexer, TOKEN_COMMA);
        case ':':
            return make_token(lexer, TOKEN_COLON);
        case '+':
            return make_token(lexer, TOKEN_PLUS);
        case '-':
            return make_token(lexer, TOKEN_MINUS);
        case '*':
            return make_token(lexer, TOKEN_STAR);
        case '/':
            if (lexer_match(lexer, '/')) {
                skip_comment_to_eol(lexer);
                return lexer_next_token(lexer);
            }
            return make_token(lexer, TOKEN_SLASH);
        case '%':
            return make_token(lexer, TOKEN_PERCENT);
        case '>':
            return make_token(lexer, lexer_match(lexer, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '<':
            return make_token(lexer, lexer_match(lexer, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '"':
            return string_token(lexer);
        default:
            break;
    }

    return error_token(lexer, "Unexpected character");
}

/* ========================= TOKEN BUFFER ========================= */

typedef struct {
    Token *data;
    size_t count;
    size_t capacity;
} TokenBuffer;

static void token_buffer_init(TokenBuffer *buffer) {
    buffer->data = NULL;
    buffer->count = 0;
    buffer->capacity = 0;
}

static void token_buffer_push(TokenBuffer *buffer, Token token) {
    if (buffer->count + 1 > buffer->capacity) {
        size_t new_capacity = buffer->capacity < 8 ? 8 : buffer->capacity * 2;
        Token *new_data = (Token *)realloc(buffer->data, new_capacity * sizeof(Token));
        CHECK_ALLOC(new_data);
        buffer->data = new_data;
        buffer->capacity = new_capacity;
    }
    buffer->data[buffer->count++] = token;
}

static void token_buffer_free(TokenBuffer *buffer) {
    free(buffer->data);
    buffer->data = NULL;
    buffer->count = 0;
    buffer->capacity = 0;
}

/* ========================= AST DEFINITIONS ========================= */

typedef struct Expr Expr;

typedef enum {
    EXPR_LITERAL,
    EXPR_GROUPING,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_VARIABLE,
    EXPR_CALL,
    EXPR_LIST,
    EXPR_RECORD,
    EXPR_INDEX
} ExprType;

typedef enum {
    LITERAL_NUMBER,
    LITERAL_STRING,
    LITERAL_BOOL,
    LITERAL_NOTHING
} LiteralType;

typedef struct {
    LiteralType type;
    union {
        double number;
        char *string;
        bool boolean;
    } as;
} Literal;

typedef struct {
    Token op;
    Expr *right;
} UnaryExpr;

typedef struct {
    Expr *left;
    Token op;
    Expr *right;
} BinaryExpr;

typedef struct {
    char *name;
} VariableExpr;

typedef struct {
    Expr *callee;
    Expr **arguments;
    size_t arg_count;
} CallExpr;

typedef struct {
    Expr **elements;
    size_t count;
} ListExpr;

typedef struct {
    char **keys;
    Expr **values;
    size_t count;
} RecordExpr;

typedef struct {
    Expr *collection;
    Expr *index;
} IndexExpr;

struct Expr {
    ExprType type;
    int line;
    union {
        Literal literal;
        Expr *grouping;
        UnaryExpr unary;
        BinaryExpr binary;
        VariableExpr variable;
        CallExpr call;
        ListExpr list;
        RecordExpr record;
        IndexExpr index;
    } as;
};

typedef enum {
    STMT_LET,
    STMT_SET,
    STMT_EXPR,
    STMT_GATHER,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILST,
    STMT_FUNCTION,
    STMT_HALT,
    STMT_BREAK,
    STMT_CONTINUE
} StmtType;

typedef struct Stmt Stmt;

typedef struct {
    char *name;
    Expr *value;
} LetStmt;

typedef struct {
    char *name;
    Expr *value;
} SetStmt;

typedef struct {
    char *module_name;
} GatherStmt;

typedef struct {
    Stmt **statements;
    size_t count;
} BlockStmt;

typedef struct {
    Expr *condition;
    Stmt *then_branch;
    Stmt *else_branch;
} IfStmt;

typedef struct {
    Expr *condition;
    Stmt *body;
} WhilstStmt;

typedef struct {
    char *name;
    char **parameters;
    size_t param_count;
    Stmt *body; /* STMT_BLOCK */
} FunctionStmt;

typedef struct {
    Expr *value; /* optional */
} HaltStmt;

struct Stmt {
    StmtType type;
    int line;
    union {
        LetStmt let_stmt;
        SetStmt set_stmt;
        Expr *expr;
        GatherStmt gather;
        BlockStmt block;
        IfStmt if_stmt;
        WhilstStmt whilst;
        FunctionStmt function;
        HaltStmt halt;
    } as;
};

/* ========================= PARSER ========================= */

typedef struct {
    Token *tokens;
    size_t count;
    size_t current;
    bool had_error;
    int loop_depth;
} Parser;

static void parser_init(Parser *parser, Token *tokens, size_t count) {
    parser->tokens = tokens;
    parser->count = count;
    parser->current = 0;
    parser->had_error = false;
    parser->loop_depth = 0;
}

static Token parser_peek(Parser *parser) {
    if (parser->current >= parser->count) {
        return parser->tokens[parser->count - 1];
    }
    return parser->tokens[parser->current];
}

static bool parser_is_at_end(Parser *parser) {
    return parser_peek(parser).type == TOKEN_EOF;
}

static Token parser_previous(Parser *parser) {
    return parser->tokens[parser->current - 1];
}

static Token parser_advance(Parser *parser) {
    if (!parser_is_at_end(parser)) {
        parser->current++;
    }
    return parser_previous(parser);
}

static bool parser_check(Parser *parser, TokenType type) {
    if (parser_is_at_end(parser)) return false;
    return parser_peek(parser).type == type;
}

static bool parser_match(Parser *parser, TokenType type) {
    if (!parser_check(parser, type)) return false;
    parser_advance(parser);
    return true;
}

static void parser_error_at(Parser *parser, Token token, const char *message) {
    parser->had_error = true;
    const char *lexeme = NULL;
    size_t length = 0;
    if (token.type != TOKEN_EOF) {
        lexeme = token.start;
        length = token.length;
    }
    lie_error_report(LIE_ERROR_PARSE, token.line, lexeme, length, "%s", message);
}

static void parser_consume(Parser *parser, TokenType type, const char *message) {
    if (parser_check(parser, type)) {
        parser_advance(parser);
        return;
    }
    parser_error_at(parser, parser_peek(parser), message);
}

static void parser_skip_newlines(Parser *parser) {
    while (parser_match(parser, TOKEN_NEWLINE)) {
        /* skip */
    }
}

static Expr *parse_expression(Parser *parser);
static Expr *parse_or(Parser *parser);
static Expr *parse_and(Parser *parser);
static Expr *parse_equality(Parser *parser);
static Expr *parse_comparison(Parser *parser);
static Expr *parse_term(Parser *parser);
static Expr *parse_factor(Parser *parser);
static Expr *parse_unary(Parser *parser);
static Expr *parse_call(Parser *parser);
static Expr *parse_primary(Parser *parser);
static Stmt *parse_declaration(Parser *parser);
static Stmt *parse_statement(Parser *parser);
static Stmt *parse_block(Parser *parser);
static Expr *parse_list_literal(Parser *parser, Token left_bracket);
static Expr *parse_record_literal(Parser *parser, Token left_brace);
static Stmt *parse_break_statement(Parser *parser, Token keyword);
static Stmt *parse_continue_statement(Parser *parser, Token keyword);
static Stmt **parse_program(Parser *parser, size_t *out_count);

static Expr *new_expr(ExprType type, int line) {
    Expr *expr = (Expr *)calloc(1, sizeof(Expr));
    CHECK_ALLOC(expr);
    expr->type = type;
    expr->line = line;
    return expr;
}

static Stmt *new_stmt(StmtType type, int line) {
    Stmt *stmt = (Stmt *)calloc(1, sizeof(Stmt));
    CHECK_ALLOC(stmt);
    stmt->type = type;
    stmt->line = line;
    return stmt;
}

static bool identifier_has_namespace(Token token) {
    for (size_t i = 0; i + 1 < token.length; ++i) {
        if (token.start[i] == ':' && token.start[i + 1] == ':') {
            return true;
        }
    }
    return false;
}

static Expr *parse_primary(Parser *parser) {
    Token token = parser_peek(parser);
    if (parser_match(parser, TOKEN_FALSE)) {
        Expr *expr = new_expr(EXPR_LITERAL, token.line);
        expr->as.literal.type = LITERAL_BOOL;
        expr->as.literal.as.boolean = false;
        return expr;
    }
    if (parser_match(parser, TOKEN_TRUE)) {
        Expr *expr = new_expr(EXPR_LITERAL, token.line);
        expr->as.literal.type = LITERAL_BOOL;
        expr->as.literal.as.boolean = true;
        return expr;
    }
    if (parser_match(parser, TOKEN_NOTHING)) {
        Expr *expr = new_expr(EXPR_LITERAL, token.line);
        expr->as.literal.type = LITERAL_NOTHING;
        return expr;
    }
    if (parser_match(parser, TOKEN_NUMBER)) {
        Expr *expr = new_expr(EXPR_LITERAL, token.line);
        expr->as.literal.type = LITERAL_NUMBER;
        char *numeral = copy_string(token.start, token.length);
        expr->as.literal.as.number = strtod(numeral, NULL);
        free(numeral);
        return expr;
    }
    if (parser_match(parser, TOKEN_STRING)) {
        Expr *expr = new_expr(EXPR_LITERAL, token.line);
        expr->as.literal.type = LITERAL_STRING;
        expr->as.literal.as.string = copy_string(token.start + 1, token.length - 2);
        return expr;
    }
    if (parser_match(parser, TOKEN_IDENTIFIER)) {
        Expr *expr = new_expr(EXPR_VARIABLE, token.line);
        expr->as.variable.name = copy_string(token.start, token.length);
        return expr;
    }
    if (parser_match(parser, TOKEN_LEFT_PAREN)) {
        Expr *expr = new_expr(EXPR_GROUPING, token.line);
        expr->as.grouping = parse_expression(parser);
        parser_consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after expression");
        return expr;
    }
    if (parser_match(parser, TOKEN_LEFT_BRACKET)) {
        return parse_list_literal(parser, parser_previous(parser));
    }
    if (parser_match(parser, TOKEN_LEFT_BRACE)) {
        return parse_record_literal(parser, parser_previous(parser));
    }

    parser_error_at(parser, parser_peek(parser), "Expected expression");
    return NULL;
}

static Expr *parse_list_literal(Parser *parser, Token left_bracket) {
    Expr *expr = new_expr(EXPR_LIST, left_bracket.line);
    expr->as.list.elements = NULL;
    expr->as.list.count = 0;
    size_t capacity = 0;

    if (!parser_check(parser, TOKEN_RIGHT_BRACKET)) {
        do {
            Expr *element = parse_expression(parser);
            if (expr->as.list.count + 1 > capacity) {
                size_t new_capacity = capacity < 4 ? 4 : capacity * 2;
                Expr **new_elements = (Expr **)realloc(expr->as.list.elements, new_capacity * sizeof(Expr *));
                CHECK_ALLOC(new_elements);
                expr->as.list.elements = new_elements;
                capacity = new_capacity;
            }
            expr->as.list.elements[expr->as.list.count++] = element;
        } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RIGHT_BRACKET, "Expect ']' after list literal");
    return expr;
}

static Expr *parse_record_literal(Parser *parser, Token left_brace) {
    Expr *expr = new_expr(EXPR_RECORD, left_brace.line);
    expr->as.record.keys = NULL;
    expr->as.record.values = NULL;
    expr->as.record.count = 0;
    size_t capacity = 0;

    if (!parser_check(parser, TOKEN_RIGHT_BRACE)) {
        do {
            Token key_token = parser_peek(parser);
            char *key = NULL;
            if (parser_match(parser, TOKEN_IDENTIFIER)) {
                key = copy_string(key_token.start, key_token.length);
            } else if (parser_match(parser, TOKEN_STRING)) {
                key = copy_string(key_token.start + 1, key_token.length - 2);
            } else {
                parser_error_at(parser, key_token, "Expected field name in record");
                lie_error_hint("Use an identifier or string literal before 'be'.");
                key = duplicate_cstring("");
            }
            parser_consume(parser, TOKEN_BE, "Expect 'be' after record field name");
            Expr *value = parse_expression(parser);

            if (expr->as.record.count + 1 > capacity) {
                size_t new_capacity = capacity < 4 ? 4 : capacity * 2;
                char **new_keys = (char **)realloc(expr->as.record.keys, new_capacity * sizeof(char *));
                CHECK_ALLOC(new_keys);
                expr->as.record.keys = new_keys;
                Expr **new_values = (Expr **)realloc(expr->as.record.values, new_capacity * sizeof(Expr *));
                CHECK_ALLOC(new_values);
                expr->as.record.values = new_values;
                capacity = new_capacity;
            }
            expr->as.record.keys[expr->as.record.count] = key;
            expr->as.record.values[expr->as.record.count] = value;
            expr->as.record.count++;
        } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after record literal");
    return expr;
}

static Stmt *parse_break_statement(Parser *parser, Token keyword) {
    if (parser->loop_depth == 0) {
        parser_error_at(parser, keyword, "'break' outside of a loop");
    }
    Stmt *stmt = new_stmt(STMT_BREAK, keyword.line);
    return stmt;
}

static Stmt *parse_continue_statement(Parser *parser, Token keyword) {
    if (parser->loop_depth == 0) {
        parser_error_at(parser, keyword, "'continue' outside of a loop");
    }
    Stmt *stmt = new_stmt(STMT_CONTINUE, keyword.line);
    return stmt;
}

static Expr *parse_call(Parser *parser) {
    Expr *expr = parse_primary(parser);
    if (expr == NULL) return NULL;

    for (;;) {
        if (parser_match(parser, TOKEN_LEFT_PAREN)) {
            Expr *call = new_expr(EXPR_CALL, parser_previous(parser).line);
            call->as.call.callee = expr;
            call->as.call.arguments = NULL;
            call->as.call.arg_count = 0;
            size_t capacity = 0;

            if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
                do {
                    Expr *argument = parse_expression(parser);
                    if (argument == NULL) {
                        parser_error_at(parser, parser_peek(parser), "Invalid argument");
                    }
                    size_t count = call->as.call.arg_count;
                    if (count + 1 > capacity) {
                        size_t new_capacity = capacity < 4 ? 4 : capacity * 2;
                        Expr **new_args = (Expr **)realloc(call->as.call.arguments, new_capacity * sizeof(Expr *));
                        CHECK_ALLOC(new_args);
                        call->as.call.arguments = new_args;
                        capacity = new_capacity;
                    }
                    call->as.call.arguments[count] = argument;
                    call->as.call.arg_count++;
                } while (parser_match(parser, TOKEN_COMMA));
            }

            parser_consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after arguments");
            expr = call;
            continue;
        }

        if (parser_match(parser, TOKEN_LEFT_BRACKET)) {
            Token bracket = parser_previous(parser);
            Expr *index = parse_expression(parser);
            parser_consume(parser, TOKEN_RIGHT_BRACKET, "Expect ']' after index expression");
            Expr *index_expr = new_expr(EXPR_INDEX, bracket.line);
            index_expr->as.index.collection = expr;
            index_expr->as.index.index = index;
            expr = index_expr;
            continue;
        }

        break;
    }

    return expr;
}

static Expr *parse_unary(Parser *parser) {
    if (parser_match(parser, TOKEN_NOT) || parser_match(parser, TOKEN_MINUS)) {
        Token op = parser_previous(parser);
        Expr *right = parse_unary(parser);
        if (right == NULL) return NULL;
        Expr *expr = new_expr(EXPR_UNARY, op.line);
        expr->as.unary.op = op;
        expr->as.unary.right = right;
        return expr;
    }
    return parse_call(parser);
}

static Expr *parse_factor(Parser *parser) {
    Expr *expr = parse_unary(parser);
    while (parser_match(parser, TOKEN_STAR) || parser_match(parser, TOKEN_SLASH) || parser_match(parser, TOKEN_PERCENT)) {
        Token op = parser_previous(parser);
        Expr *right = parse_unary(parser);
        Expr *binary = new_expr(EXPR_BINARY, op.line);
        binary->as.binary.left = expr;
        binary->as.binary.op = op;
        binary->as.binary.right = right;
        expr = binary;
    }
    return expr;
}

static Expr *parse_term(Parser *parser) {
    Expr *expr = parse_factor(parser);
    while (parser_match(parser, TOKEN_PLUS) || parser_match(parser, TOKEN_MINUS)) {
        Token op = parser_previous(parser);
        Expr *right = parse_factor(parser);
        Expr *binary = new_expr(EXPR_BINARY, op.line);
        binary->as.binary.left = expr;
        binary->as.binary.op = op;
        binary->as.binary.right = right;
        expr = binary;
    }
    return expr;
}

static Expr *parse_comparison(Parser *parser) {
    Expr *expr = parse_term(parser);
    while (parser_match(parser, TOKEN_GREATER) || parser_match(parser, TOKEN_GREATER_EQUAL) ||
           parser_match(parser, TOKEN_LESS) || parser_match(parser, TOKEN_LESS_EQUAL)) {
        Token op = parser_previous(parser);
        Expr *right = parse_term(parser);
        Expr *binary = new_expr(EXPR_BINARY, op.line);
        binary->as.binary.left = expr;
        binary->as.binary.op = op;
        binary->as.binary.right = right;
        expr = binary;
    }
    return expr;
}

static Expr *parse_equality(Parser *parser) {
    Expr *expr = parse_comparison(parser);
    while (parser_match(parser, TOKEN_IS) || parser_match(parser, TOKEN_ISNT)) {
        Token op = parser_previous(parser);
        Expr *right = parse_comparison(parser);
        Expr *binary = new_expr(EXPR_BINARY, op.line);
        binary->as.binary.left = expr;
        binary->as.binary.op = op;
        binary->as.binary.right = right;
        expr = binary;
    }
    return expr;
}

static Expr *parse_and(Parser *parser) {
    Expr *expr = parse_equality(parser);
    while (parser_match(parser, TOKEN_AND)) {
        Token op = parser_previous(parser);
        Expr *right = parse_equality(parser);
        Expr *binary = new_expr(EXPR_BINARY, op.line);
        binary->as.binary.left = expr;
        binary->as.binary.op = op;
        binary->as.binary.right = right;
        expr = binary;
    }
    return expr;
}

static Expr *parse_or(Parser *parser) {
    Expr *expr = parse_and(parser);
    while (parser_match(parser, TOKEN_OR)) {
        Token op = parser_previous(parser);
        Expr *right = parse_and(parser);
        Expr *binary = new_expr(EXPR_BINARY, op.line);
        binary->as.binary.left = expr;
        binary->as.binary.op = op;
        binary->as.binary.right = right;
        expr = binary;
    }
    return expr;
}

static Expr *parse_expression(Parser *parser) {
    return parse_or(parser);
}

static void parser_synchronize(Parser *parser) {
    while (!parser_is_at_end(parser)) {
        if (parser_match(parser, TOKEN_NEWLINE)) return;
        parser_advance(parser);
    }
}

static Stmt *parse_gather_statement(Parser *parser, Token keyword) {
    if (!parser_check(parser, TOKEN_IDENTIFIER)) {
        parser_error_at(parser, parser_peek(parser), "Expected module name after 'gather'");
        lie_error_hint("Use a bare module name such as 'gather io'.");
        return NULL;
    }
    Token name_token = parser_advance(parser);
    Stmt *stmt = new_stmt(STMT_GATHER, keyword.line);
    stmt->as.gather.module_name = copy_string(name_token.start, name_token.length);
    return stmt;
}

static Stmt *parse_let_statement(Parser *parser, Token keyword) {
    if (!parser_check(parser, TOKEN_IDENTIFIER)) {
        parser_error_at(parser, parser_peek(parser), "Expected name after 'let'");
        lie_error_hint("Introduce names with letters/underscores, e.g. 'let mood be ...'.");
        return NULL;
    }
    Token name_token = parser_advance(parser);
    parser_consume(parser, TOKEN_BE, "Expected 'be' after name");
    Expr *value = parse_expression(parser);
    if (value == NULL) return NULL;
    Stmt *stmt = new_stmt(STMT_LET, keyword.line);
    stmt->as.let_stmt.name = copy_string(name_token.start, name_token.length);
    stmt->as.let_stmt.value = value;
    return stmt;
}

static Stmt *parse_set_statement(Parser *parser, Token keyword) {
    if (!parser_check(parser, TOKEN_IDENTIFIER)) {
        parser_error_at(parser, parser_peek(parser), "Expected name after 'set'");
        lie_error_hint("Update a previously bound name, e.g. 'set mood to ...'.");
        return NULL;
    }
    Token name_token = parser_advance(parser);
    parser_consume(parser, TOKEN_TO, "Expected 'to' after name");
    Expr *value = parse_expression(parser);
    if (value == NULL) return NULL;
    Stmt *stmt = new_stmt(STMT_SET, keyword.line);
    stmt->as.set_stmt.name = copy_string(name_token.start, name_token.length);
    stmt->as.set_stmt.value = value;
    return stmt;
}

static Stmt *parse_block(Parser *parser) {
    parser_consume(parser, TOKEN_NEWLINE, "Expect newline after ':'");
    parser_consume(parser, TOKEN_INDENT, "Expect indented block");

    Stmt *stmt = new_stmt(STMT_BLOCK, parser_previous(parser).line);
    stmt->as.block.statements = NULL;
    stmt->as.block.count = 0;
    size_t capacity = 0;

    parser_skip_newlines(parser);

    while (!parser_check(parser, TOKEN_DEDENT) && !parser_is_at_end(parser)) {
        Stmt *inner = parse_declaration(parser);
        if (inner != NULL) {
            if (stmt->as.block.count + 1 > capacity) {
                size_t new_capacity = capacity < 8 ? 8 : capacity * 2;
                Stmt **new_data = (Stmt **)realloc(stmt->as.block.statements, new_capacity * sizeof(Stmt *));
                CHECK_ALLOC(new_data);
                stmt->as.block.statements = new_data;
                capacity = new_capacity;
            }
            stmt->as.block.statements[stmt->as.block.count++] = inner;
        } else {
            parser_synchronize(parser);
        }
        parser_skip_newlines(parser);
    }

    parser_consume(parser, TOKEN_DEDENT, "Expect block to end");
    return stmt;
}

static Stmt *parse_if_statement(Parser *parser, Token keyword) {
    Expr *condition = parse_expression(parser);
    parser_consume(parser, TOKEN_COLON, "Expect ':' after condition");
    Stmt *then_branch = parse_block(parser);

    parser_skip_newlines(parser);
    Stmt *else_branch = NULL;
    if (parser_match(parser, TOKEN_OTHERWISE)) {
        Token otherwise_token = parser_previous(parser);
        if (parser_match(parser, TOKEN_IF)) {
            Token else_if_token = parser_previous(parser);
            else_branch = parse_if_statement(parser, else_if_token);
            else_branch->line = otherwise_token.line;
        } else {
            parser_consume(parser, TOKEN_COLON, "Expect ':' after 'otherwise'");
            else_branch = parse_block(parser);
            else_branch->line = otherwise_token.line;
        }
    }

    Stmt *stmt = new_stmt(STMT_IF, keyword.line);
    stmt->as.if_stmt.condition = condition;
    stmt->as.if_stmt.then_branch = then_branch;
    stmt->as.if_stmt.else_branch = else_branch;
    return stmt;
}

static Stmt *parse_whilst_statement(Parser *parser, Token keyword) {
    parser->loop_depth++;
    Expr *condition = parse_expression(parser);
    parser_consume(parser, TOKEN_COLON, "Expect ':' after condition");
    Stmt *body = parse_block(parser);
    parser->loop_depth--;

    Stmt *stmt = new_stmt(STMT_WHILST, keyword.line);
    stmt->as.whilst.condition = condition;
    stmt->as.whilst.body = body;
    return stmt;
}

static Stmt *parse_halt_statement(Parser *parser, Token keyword) {
    Stmt *stmt = new_stmt(STMT_HALT, keyword.line);
    if (parser_check(parser, TOKEN_NEWLINE) || parser_check(parser, TOKEN_DEDENT) || parser_check(parser, TOKEN_EOF)) {
        stmt->as.halt.value = NULL;
        return stmt;
    }
    Expr *value = parse_expression(parser);
    stmt->as.halt.value = value;
    return stmt;
}

static Stmt *parse_function(Parser *parser, Token keyword) {
    if (!parser_check(parser, TOKEN_IDENTIFIER)) {
        parser_error_at(parser, parser_peek(parser), "Expected function name after 'note'");
        return NULL;
    }
    Token name_token = parser_advance(parser);
    parser_consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after function name");

    char **parameters = NULL;
    size_t param_count = 0;
    size_t capacity = 0;

    if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
        do {
            if (!parser_check(parser, TOKEN_IDENTIFIER)) {
                parser_error_at(parser, parser_peek(parser), "Expected parameter name");
                lie_error_hint("List parameters like 'note f(x, y): ...'.");
                break;
            }
            Token param = parser_advance(parser);
            if (identifier_has_namespace(param)) {
                parser_error_at(parser, param, "Parameters cannot contain '::'");
                lie_error_hint("Parameters should be plain names without module qualifiers.");
                break;
            }
            if (param_count + 1 > capacity) {
                size_t new_capacity = capacity < 4 ? 4 : capacity * 2;
                char **new_params = (char **)realloc(parameters, new_capacity * sizeof(char *));
                CHECK_ALLOC(new_params);
                parameters = new_params;
                capacity = new_capacity;
            }
            parameters[param_count++] = copy_string(param.start, param.length);
        } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameters");
    parser_consume(parser, TOKEN_COLON, "Expect ':' before function body");
    Stmt *body = parse_block(parser);

    Stmt *stmt = new_stmt(STMT_FUNCTION, keyword.line);
    stmt->as.function.name = copy_string(name_token.start, name_token.length);
    stmt->as.function.parameters = parameters;
    stmt->as.function.param_count = param_count;
    stmt->as.function.body = body;
    return stmt;
}

static Stmt *parse_statement(Parser *parser) {
    if (parser_match(parser, TOKEN_IF)) {
        Token keyword = parser_previous(parser);
        return parse_if_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_WHILST)) {
        Token keyword = parser_previous(parser);
        return parse_whilst_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_HALT)) {
        Token keyword = parser_previous(parser);
        return parse_halt_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_BREAK)) {
        Token keyword = parser_previous(parser);
        return parse_break_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_CONTINUE)) {
        Token keyword = parser_previous(parser);
        return parse_continue_statement(parser, keyword);
    }

    Expr *expr = parse_expression(parser);
    if (expr == NULL) return NULL;
    Stmt *stmt = new_stmt(STMT_EXPR, expr->line);
    stmt->as.expr = expr;
    return stmt;
}

static Stmt *parse_declaration(Parser *parser) {
    if (parser_match(parser, TOKEN_GATHER)) {
        Token keyword = parser_previous(parser);
        return parse_gather_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_LET)) {
        Token keyword = parser_previous(parser);
        return parse_let_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_SET)) {
        Token keyword = parser_previous(parser);
        return parse_set_statement(parser, keyword);
    }
    if (parser_match(parser, TOKEN_NOTE)) {
        Token keyword = parser_previous(parser);
        return parse_function(parser, keyword);
    }
    return parse_statement(parser);
}

static Stmt **parse_program(Parser *parser, size_t *out_count) {
    Stmt **statements = NULL;
    size_t count = 0;
    size_t capacity = 0;

    parser_skip_newlines(parser);

    while (!parser_is_at_end(parser)) {
        if (parser_check(parser, TOKEN_DEDENT)) {
            parser_advance(parser);
            continue;
        }
        Stmt *stmt = parse_declaration(parser);
        if (stmt != NULL) {
            if (count + 1 > capacity) {
                size_t new_capacity = capacity < 8 ? 8 : capacity * 2;
                Stmt **new_data = (Stmt **)realloc(statements, new_capacity * sizeof(Stmt *));
                CHECK_ALLOC(new_data);
                statements = new_data;
                capacity = new_capacity;
            }
            statements[count++] = stmt;
        } else {
            parser_synchronize(parser);
        }
        parser_skip_newlines(parser);
    }

    *out_count = count;
    return statements;
}

/* ========================= RUNTIME VALUES ========================= */

typedef struct Interpreter Interpreter;

typedef struct Function Function;

typedef struct Environment Environment;

static Environment *environment_retain(Environment *env);
static Environment *environment_new(Environment *enclosing);
static void environment_release(Environment *env);

typedef enum {
    VALUE_NUMBER,
    VALUE_BOOL,
    VALUE_STRING,
    VALUE_NOTHING,
    VALUE_NATIVE,
    VALUE_FUNCTION,
    VALUE_LIST,
    VALUE_RECORD
} ValueType;

typedef struct Value Value;

typedef Value (*NativeFn)(Interpreter *interp, int arg_count, Value *args, int line);

typedef struct {
    NativeFn fn;
    const char *name;
} Native;

typedef struct {
    Value *items;
    size_t count;
    size_t capacity;
} ListData;

typedef struct {
    char **keys;
    Value *values;
    size_t count;
    size_t capacity;
} RecordData;

struct Value {
    ValueType type;
    union {
        double number;
        bool boolean;
        char *string;
        Native native;
        Function *function;
        ListData list;
        RecordData record;
    } as;
};

struct Function {
    int refcount;
    FunctionStmt *declaration;
    struct Environment *closure;
};

static Function *function_new(FunctionStmt *declaration, struct Environment *closure) {
    Function *fn = (Function *)calloc(1, sizeof(Function));
    CHECK_ALLOC(fn);
    fn->refcount = 1;
    fn->declaration = declaration;
    fn->closure = environment_retain(closure);
    return fn;
}

static Value value_number(double number) {
    Value v;
    v.type = VALUE_NUMBER;
    v.as.number = number;
    return v;
}

static Value value_bool(bool boolean) {
    Value v;
    v.type = VALUE_BOOL;
    v.as.boolean = boolean;
    return v;
}

static Value value_string(const char *text) {
    Value v;
    v.type = VALUE_STRING;
    v.as.string = duplicate_cstring(text);
    return v;
}

static Value value_nothing(void) {
    Value v;
    v.type = VALUE_NOTHING;
    return v;
}

static Value value_list(void) {
    Value v;
    v.type = VALUE_LIST;
    v.as.list.items = NULL;
    v.as.list.count = 0;
    v.as.list.capacity = 0;
    return v;
}

static Value value_record(void) {
    Value v;
    v.type = VALUE_RECORD;
    v.as.record.keys = NULL;
    v.as.record.values = NULL;
    v.as.record.count = 0;
    v.as.record.capacity = 0;
    return v;
}

static void list_append(Value *list, Value value) {
    if (list->type != VALUE_LIST) return;
    if (list->as.list.count + 1 > list->as.list.capacity) {
        size_t new_capacity = list->as.list.capacity < 4 ? 4 : list->as.list.capacity * 2;
        Value *items = (Value *)realloc(list->as.list.items, new_capacity * sizeof(Value));
        CHECK_ALLOC(items);
        list->as.list.items = items;
        list->as.list.capacity = new_capacity;
    }
    list->as.list.items[list->as.list.count++] = value;
}

static ssize_t record_find_index(const RecordData *record, const char *key) {
    for (size_t i = 0; i < record->count; ++i) {
        if (strcmp(record->keys[i], key) == 0) {
            return (ssize_t)i;
        }
    }
    return -1;
}

static void record_append(Value *record, const char *key, Value value) {
    if (record->type != VALUE_RECORD) return;
    if (record->as.record.count + 1 > record->as.record.capacity) {
        size_t new_capacity = record->as.record.capacity < 4 ? 4 : record->as.record.capacity * 2;
        char **keys = (char **)realloc(record->as.record.keys, new_capacity * sizeof(char *));
        CHECK_ALLOC(keys);
        record->as.record.keys = keys;
        Value *values = (Value *)realloc(record->as.record.values, new_capacity * sizeof(Value));
        CHECK_ALLOC(values);
        record->as.record.values = values;
        record->as.record.capacity = new_capacity;
    }
    record->as.record.keys[record->as.record.count] = duplicate_cstring(key);
    record->as.record.values[record->as.record.count] = value;
    record->as.record.count++;
}

static Value value_native(NativeFn fn, const char *name) {
    Value v;
    v.type = VALUE_NATIVE;
    v.as.native.fn = fn;
    v.as.native.name = name;
    return v;
}

static Value value_function(Function *function) {
    Value v;
    v.type = VALUE_FUNCTION;
    v.as.function = function;
    return v;
}

static Value value_copy(Value src) {
    switch (src.type) {
        case VALUE_NUMBER:
            return value_number(src.as.number);
        case VALUE_BOOL:
            return value_bool(src.as.boolean);
        case VALUE_STRING:
            return value_string(src.as.string);
        case VALUE_NOTHING:
            return value_nothing();
        case VALUE_NATIVE:
            return value_native(src.as.native.fn, src.as.native.name);
        case VALUE_FUNCTION:
            src.as.function->refcount++;
            return value_function(src.as.function);
        case VALUE_LIST: {
            Value copy = value_list();
            for (size_t i = 0; i < src.as.list.count; ++i) {
                list_append(&copy, value_copy(src.as.list.items[i]));
            }
            return copy;
        }
        case VALUE_RECORD: {
            Value copy = value_record();
            for (size_t i = 0; i < src.as.record.count; ++i) {
                record_append(&copy, src.as.record.keys[i], value_copy(src.as.record.values[i]));
            }
            return copy;
        }
    }
    return value_nothing();
}

static void function_free(Function *function) {
    if (function->refcount <= 0) {
        environment_release(function->closure);
        free(function);
    }
}

static void value_free(Value value) {
    switch (value.type) {
        case VALUE_STRING:
            free(value.as.string);
            break;
        case VALUE_FUNCTION:
            value.as.function->refcount--;
            if (value.as.function->refcount == 0) {
                function_free(value.as.function);
            }
            break;
        case VALUE_LIST: {
            for (size_t i = 0; i < value.as.list.count; ++i) {
                value_free(value.as.list.items[i]);
            }
            free(value.as.list.items);
            break;
        }
        case VALUE_RECORD: {
            for (size_t i = 0; i < value.as.record.count; ++i) {
                free(value.as.record.keys[i]);
                value_free(value.as.record.values[i]);
            }
            free(value.as.record.keys);
            free(value.as.record.values);
            break;
        }
        default:
            break;
    }
}

static bool value_is_truthy(Value value) {
    switch (value.type) {
        case VALUE_BOOL:
            return value.as.boolean;
        case VALUE_NUMBER:
            return value.as.number != 0.0;
        case VALUE_STRING:
            return value.as.string[0] != '\0';
        case VALUE_NOTHING:
            return false;
        case VALUE_NATIVE:
        case VALUE_FUNCTION:
            return true;
        case VALUE_LIST:
            return value.as.list.count > 0;
        case VALUE_RECORD:
            return value.as.record.count > 0;
    }
    return false;
}

static bool value_equal(Value left, Value right) {
    if (left.type != right.type) {
        if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
            return left.as.number == right.as.number;
        }
        if (left.type == VALUE_BOOL && right.type == VALUE_BOOL) {
            return left.as.boolean == right.as.boolean;
        }
        return false;
    }
    switch (left.type) {
        case VALUE_NUMBER:
            return left.as.number == right.as.number;
        case VALUE_BOOL:
            return left.as.boolean == right.as.boolean;
        case VALUE_STRING:
            return strcmp(left.as.string, right.as.string) == 0;
        case VALUE_NOTHING:
            return true;
        case VALUE_NATIVE:
            return left.as.native.fn == right.as.native.fn;
        case VALUE_FUNCTION:
            return left.as.function == right.as.function;
        case VALUE_LIST:
            if (left.as.list.count != right.as.list.count) {
                return false;
            }
            for (size_t i = 0; i < left.as.list.count; ++i) {
                if (!value_equal(left.as.list.items[i], right.as.list.items[i])) {
                    return false;
                }
            }
            return true;
        case VALUE_RECORD:
            if (left.as.record.count != right.as.record.count) {
                return false;
            }
            for (size_t i = 0; i < left.as.record.count; ++i) {
                const char *key = left.as.record.keys[i];
                ssize_t index = record_find_index(&right.as.record, key);
                if (index < 0) {
                    return false;
                }
                if (!value_equal(left.as.record.values[i], right.as.record.values[index])) {
                    return false;
                }
            }
            return true;
    }
    return false;
}

static char *value_to_string(Value value) {
    switch (value.type) {
        case VALUE_STRING:
            return duplicate_cstring(value.as.string);
        case VALUE_NUMBER: {
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "%g", value.as.number);
            return duplicate_cstring(buffer);
        }
        case VALUE_BOOL:
            return duplicate_cstring(value.as.boolean ? "true" : "false");
        case VALUE_NOTHING:
            return duplicate_cstring("nothing");
        case VALUE_NATIVE:
            return duplicate_cstring(value.as.native.name);
        case VALUE_FUNCTION:
            return duplicate_cstring(value.as.function->declaration->name);
        case VALUE_LIST: {
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "[list %zu]", value.as.list.count);
            return duplicate_cstring(buffer);
        }
        case VALUE_RECORD: {
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "{record %zu}", value.as.record.count);
            return duplicate_cstring(buffer);
        }
    }
    return duplicate_cstring("nothing");
}

/* ========================= ENVIRONMENT ========================= */

typedef struct Binding {
    char *name;
    Value value;
} Binding;

struct Environment {
    int refcount;
    Binding *entries;
    size_t count;
    size_t capacity;
    struct Environment *enclosing;
};

static Environment *environment_retain(Environment *env) {
    if (env != NULL) {
        env->refcount++;
    }
    return env;
}

static Environment *environment_new(Environment *enclosing) {
    Environment *env = (Environment *)calloc(1, sizeof(Environment));
    CHECK_ALLOC(env);
    env->refcount = 1;
    env->entries = NULL;
    env->count = 0;
    env->capacity = 0;
    env->enclosing = enclosing ? environment_retain(enclosing) : NULL;
    return env;
}

static void environment_release(Environment *env) {
    if (env == NULL) return;
    env->refcount--;
    if (env->refcount > 0) {
        return;
    }
    for (size_t i = 0; i < env->count; ++i) {
        free(env->entries[i].name);
        value_free(env->entries[i].value);
    }
    free(env->entries);
    Environment *parent = env->enclosing;
    free(env);
    environment_release(parent);
}

static ssize_t environment_find(Environment *env, const char *name) {
    for (size_t i = 0; i < env->count; ++i) {
        if (strcmp(env->entries[i].name, name) == 0) {
            return (ssize_t)i;
        }
    }
    return -1;
}

static void environment_define(Environment *env, const char *name, Value value) {
    ssize_t index = environment_find(env, name);
    if (index >= 0) {
        value_free(env->entries[index].value);
        env->entries[index].value = value_copy(value);
        return;
    }

    if (env->count + 1 > env->capacity) {
        size_t new_capacity = env->capacity < 8 ? 8 : env->capacity * 2;
        Binding *new_entries = (Binding *)realloc(env->entries, new_capacity * sizeof(Binding));
        CHECK_ALLOC(new_entries);
        env->entries = new_entries;
        env->capacity = new_capacity;
    }
    env->entries[env->count].name = duplicate_cstring(name);
    env->entries[env->count].value = value_copy(value);
    env->count++;
}

static bool environment_assign(Environment *env, const char *name, Value value) {
    for (Environment *current = env; current != NULL; current = current->enclosing) {
        ssize_t index = environment_find(current, name);
        if (index >= 0) {
            value_free(current->entries[index].value);
            current->entries[index].value = value_copy(value);
            return true;
        }
    }
    return false;
}

static bool environment_get(Environment *env, const char *name, Value *out) {
    for (Environment *current = env; current != NULL; current = current->enclosing) {
        ssize_t index = environment_find(current, name);
        if (index >= 0) {
            *out = value_copy(current->entries[index].value);
            return true;
        }
    }
    return false;
}

typedef struct {
    char **items;
    size_t count;
    size_t capacity;
} StringList;

static void string_list_init(StringList *list) {
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void string_list_free(StringList *list) {
    for (size_t i = 0; i < list->count; ++i) {
        free(list->items[i]);
    }
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static bool string_list_contains(const StringList *list, const char *name) {
    for (size_t i = 0; i < list->count; ++i) {
        if (strcmp(list->items[i], name) == 0) {
            return true;
        }
    }
    return false;
}

static void string_list_push(StringList *list, const char *name) {
    if (list->count + 1 > list->capacity) {
        size_t new_capacity = list->capacity < 8 ? 8 : list->capacity * 2;
        char **new_items = (char **)realloc(list->items, new_capacity * sizeof(char *));
        CHECK_ALLOC(new_items);
        list->items = new_items;
        list->capacity = new_capacity;
    }
    list->items[list->count++] = duplicate_cstring(name);
}

static void string_list_pop(StringList *list) {
    if (list->count == 0) return;
    free(list->items[list->count - 1]);
    list->count--;
}

static StringList g_module_paths;
static bool g_module_paths_initialized = false;

static void ensure_global_module_paths(void) {
    if (!g_module_paths_initialized) {
        string_list_init(&g_module_paths);
        g_module_paths_initialized = true;
    }
}

void lie_register_module_path(const char *path) {
    if (path == NULL || path[0] == '\0') {
        return;
    }
    ensure_global_module_paths();
    if (!string_list_contains(&g_module_paths, path)) {
        string_list_push(&g_module_paths, path);
    }
}

typedef struct {
    Stmt **statements;
    size_t count;
} StoredProgram;

typedef struct {
    StoredProgram *items;
    size_t count;
    size_t capacity;
} ProgramStore;

static void free_program(Stmt **statements, size_t count);

static void program_store_init(ProgramStore *store) {
    store->items = NULL;
    store->count = 0;
    store->capacity = 0;
}

static void program_store_free(ProgramStore *store) {
    for (size_t i = 0; i < store->count; ++i) {
        free_program(store->items[i].statements, store->items[i].count);
    }
    free(store->items);
    store->items = NULL;
    store->count = 0;
    store->capacity = 0;
}

static void program_store_push(ProgramStore *store, Stmt **statements, size_t count) {
    if (store->count + 1 > store->capacity) {
        size_t new_capacity = store->capacity < 4 ? 4 : store->capacity * 2;
        StoredProgram *new_items = (StoredProgram *)realloc(store->items, new_capacity * sizeof(StoredProgram));
        CHECK_ALLOC(new_items);
        store->items = new_items;
        store->capacity = new_capacity;
    }
    store->items[store->count].statements = statements;
    store->items[store->count].count = count;
    store->count++;
}

/* ========================= INTERPRETER ========================= */

typedef enum {
    EXEC_FLOW_NONE,
    EXEC_FLOW_RETURN,
    EXEC_FLOW_BREAK,
    EXEC_FLOW_CONTINUE
} ExecFlow;

typedef struct {
    ExecFlow flow;
    Value value;
} ExecResult;

struct Interpreter {
    Environment *globals;
    Environment *current;
    bool had_runtime_error;
   int call_depth;
   int loop_depth;
   StringList loaded_modules;
   StringList loading_modules;
   ProgramStore stored_programs;
    StringList module_paths;
};

static ExecResult exec_result_none(void) {
    ExecResult result;
    result.flow = EXEC_FLOW_NONE;
    result.value = value_nothing();
    return result;
}

static ExecResult exec_result_value(Value value) {
    ExecResult result;
    result.flow = EXEC_FLOW_RETURN;
    result.value = value;
    return result;
}

static void interpreter_add_module_path(Interpreter *interp, const char *path) {
    if (interp == NULL || path == NULL || path[0] == '\0') {
        return;
    }
    if (!string_list_contains(&interp->module_paths, path)) {
        string_list_push(&interp->module_paths, path);
    }
}

static void interpreter_apply_global_module_paths(Interpreter *interp) {
    ensure_global_module_paths();
    for (size_t i = 0; i < g_module_paths.count; ++i) {
        interpreter_add_module_path(interp, g_module_paths.items[i]);
    }
}

static void interpreter_add_script_paths(Interpreter *interp, const char *script_path) {
    if (interp == NULL || script_path == NULL) {
        return;
    }
    char *absolute = resolve_absolute_path(script_path);
    if (absolute == NULL) {
        return;
    }
    char *current = path_dirname_copy(absolute);
    free(absolute);
    if (current == NULL || current[0] == '\0') {
        free(current);
        return;
    }

    while (current != NULL && current[0] != '\0') {
        interpreter_add_module_path(interp, current);
        if (path_is_root(current)) {
            break;
        }
        char *parent = path_dirname_copy(current);
        if (parent == NULL) {
            break;
        }
        free(current);
        current = parent;
    }
    free(current);
}

static void interpreter_init(Interpreter *interp) {
    interp->globals = environment_new(NULL);
    interp->current = interp->globals;
    interp->had_runtime_error = false;
    interp->call_depth = 0;
    interp->loop_depth = 0;
    string_list_init(&interp->loaded_modules);
    string_list_init(&interp->loading_modules);
    program_store_init(&interp->stored_programs);
    string_list_init(&interp->module_paths);
    interpreter_add_module_path(interp, ".");
    interpreter_apply_global_module_paths(interp);
}

static void interpreter_free(Interpreter *interp) {
    environment_release(interp->globals);
    interp->globals = NULL;
    interp->current = NULL;
    string_list_free(&interp->loaded_modules);
    string_list_free(&interp->loading_modules);
    program_store_free(&interp->stored_programs);
    string_list_free(&interp->module_paths);
}

static void runtime_error(Interpreter *interp, int line, const char *message, const char *hint_fmt, ...) {
    if (interp->had_runtime_error) {
        return;
    }
    lie_error_report(LIE_ERROR_RUNTIME, line, NULL, 0, "%s", message);
    if (hint_fmt != NULL) {
        char buffer[256];
        va_list args;
        va_start(args, hint_fmt);
        vsnprintf(buffer, sizeof(buffer), hint_fmt, args);
        va_end(args);
        lie_error_hint("%s", buffer);
    }
    interp->had_runtime_error = true;
}

static Value eval_expression(Interpreter *interp, Expr *expr);
static ExecResult execute_statement(Interpreter *interp, Stmt *stmt);
static ExecResult execute_block(Interpreter *interp, BlockStmt *block);
static int interpret_source(Interpreter *interp, const char *source, bool keep_program);

static Value native_core_write_line(Interpreter *interp, int arg_count, Value *args, int line) {
    UNUSED(interp);
    if (arg_count < 1) {
        runtime_error(interp, line, "core::write_line expects at least 1 argument",
                      "Pass the text you want to display, e.g. core::write_line(\"hello\").");
        return value_nothing();
    }
    for (int i = 0; i < arg_count; ++i) {
        char *text = value_to_string(args[i]);
        fputs(text, stdout);
        if (i + 1 < arg_count) {
            fputc(' ', stdout);
        }
        free(text);
    }
    fputc('\n', stdout);
    return value_nothing();
}

static Value native_core_length(Interpreter *interp, int arg_count, Value *args, int line) {
    if (arg_count != 1) {
        runtime_error(interp, line, "core::length expects exactly 1 argument",
                      "Call it like core::length(value).");
        return value_nothing();
    }
    Value target = args[0];
    switch (target.type) {
        case VALUE_STRING:
            return value_number((double)strlen(target.as.string));
        case VALUE_LIST:
            return value_number((double)target.as.list.count);
        case VALUE_RECORD:
            return value_number((double)target.as.record.count);
        default:
            runtime_error(interp, line, "core::length only supports strings, lists, or records",
                          "Provide a string, list, or record when calling core::length.");
            return value_nothing();
    }
}

static Value native_core_list_push(Interpreter *interp, int arg_count, Value *args, int line) {
    if (arg_count != 2) {
        runtime_error(interp, line, "core::list_push expects exactly 2 arguments",
                      "Usage: core::list_push(list, value).");
        return value_nothing();
    }
    if (args[0].type != VALUE_LIST) {
        runtime_error(interp, line, "core::list_push requires a list as the first argument",
                      "Pass the list you wish to extend.");
        return value_nothing();
    }
    Value result = value_list();
    for (size_t i = 0; i < args[0].as.list.count; ++i) {
        list_append(&result, value_copy(args[0].as.list.items[i]));
    }
    list_append(&result, value_copy(args[1]));
    return result;
}

static Value native_core_record_put(Interpreter *interp, int arg_count, Value *args, int line) {
    if (arg_count != 3) {
        runtime_error(interp, line, "core::record_put expects exactly 3 arguments",
                      "Usage: core::record_put(record, key, value).");
        return value_nothing();
    }
    if (args[0].type != VALUE_RECORD) {
        runtime_error(interp, line, "core::record_put requires a record as the first argument",
                      "Pass the record you wish to extend.");
        return value_nothing();
    }
    if (args[1].type != VALUE_STRING) {
        runtime_error(interp, line, "core::record_put expects a string key",
                      "Convert the key to a string before updating the record.");
        return value_nothing();
    }

    const char *key = args[1].as.string;
    Value result = value_record();
    bool replaced = false;
    for (size_t i = 0; i < args[0].as.record.count; ++i) {
        const char *existing = args[0].as.record.keys[i];
        Value element = args[0].as.record.values[i];
        if (!replaced && strcmp(existing, key) == 0) {
            record_append(&result, existing, value_copy(args[2]));
            replaced = true;
        } else {
            record_append(&result, existing, value_copy(element));
        }
    }
    if (!replaced) {
        record_append(&result, key, value_copy(args[2]));
    }
    return result;
}

static Value native_core_record_keys(Interpreter *interp, int arg_count, Value *args, int line) {
    if (arg_count != 1 || args[0].type != VALUE_RECORD) {
        runtime_error(interp, line, "core::record_keys expects a record argument",
                      "Call it like core::record_keys(record).");
        return value_nothing();
    }
    Value list = value_list();
    for (size_t i = 0; i < args[0].as.record.count; ++i) {
        list_append(&list, value_string(args[0].as.record.keys[i]));
    }
    return list;
}

static Value native_core_read_line(Interpreter *interp, int arg_count, Value *args, int line) {
    UNUSED(args);
    if (arg_count != 0) {
        runtime_error(interp, line, "core::read_line expects no arguments",
                      "Call it without parameters to read a line from input.");
        return value_nothing();
    }

    size_t capacity = 128;
    size_t length = 0;
    char *buffer = (char *)malloc(capacity);
    CHECK_ALLOC(buffer);

    int ch;
    while ((ch = fgetc(stdin)) != EOF && ch != '\n') {
        if (length + 1 >= capacity) {
            capacity *= 2;
            char *new_buffer = (char *)realloc(buffer, capacity);
            CHECK_ALLOC(new_buffer);
            buffer = new_buffer;
        }
        buffer[length++] = (char)ch;
    }

    if (ch == EOF && length == 0) {
        free(buffer);
        runtime_error(interp, line, "End of input reached",
                      "Provide input or redirect a file before calling core::read_line.");
        return value_nothing();
    }

    buffer[length] = '\0';
    Value result;
    result.type = VALUE_STRING;
    result.as.string = buffer;
    return result;
}

static Value native_core_parse_number(Interpreter *interp, int arg_count, Value *args, int line) {
    if (arg_count != 1 || args[0].type != VALUE_STRING) {
        runtime_error(interp, line, "core::parse_number expects a single string argument",
                      "Pass the raw text you wish to convert to a number.");
        return value_nothing();
    }
    errno = 0;
    char *end = NULL;
    double number = strtod(args[0].as.string, &end);
    while (end != NULL && isspace((unsigned char)*end)) {
        end++;
    }
    if (end == args[0].as.string || (end != NULL && *end != '\0') || errno != 0) {
        runtime_error(interp, line, "Unable to parse number",
                      "Ensure the input looks like 42 or 3.14.");
        return value_nothing();
    }
    return value_number(number);
}

static Value native_core_parse_boolean(Interpreter *interp, int arg_count, Value *args, int line) {
    if (arg_count != 1 || args[0].type != VALUE_STRING) {
        runtime_error(interp, line, "core::parse_boolean expects a single string argument",
                      "Use values such as 'true' or 'false'.");
        return value_nothing();
    }
    char *lower = duplicate_cstring(args[0].as.string);
    for (char *p = lower; *p; ++p) {
        *p = (char)tolower((unsigned char)*p);
    }
    Value result;
    if (strcmp(lower, "true") == 0) {
        result = value_bool(true);
    } else if (strcmp(lower, "false") == 0) {
        result = value_bool(false);
    } else {
        free(lower);
        runtime_error(interp, line, "Unable to parse boolean",
                      "Acceptable values are 'true' or 'false'.");
        return value_nothing();
    }
    free(lower);
    return result;
}

static bool load_native_module(Interpreter *interp, const char *name) {
    if (strcmp(name, "core") == 0) {
        Value write = value_native(native_core_write_line, "core::write_line");
        environment_define(interp->globals, "core::write_line", write);
        value_free(write);

        Value length_fn = value_native(native_core_length, "core::length");
        environment_define(interp->globals, "core::length", length_fn);
        value_free(length_fn);

        Value list_push = value_native(native_core_list_push, "core::list_push");
        environment_define(interp->globals, "core::list_push", list_push);
        value_free(list_push);

        Value record_put = value_native(native_core_record_put, "core::record_put");
        environment_define(interp->globals, "core::record_put", record_put);
        value_free(record_put);

        Value record_keys = value_native(native_core_record_keys, "core::record_keys");
        environment_define(interp->globals, "core::record_keys", record_keys);
        value_free(record_keys);

        Value read_line = value_native(native_core_read_line, "core::read_line");
        environment_define(interp->globals, "core::read_line", read_line);
        value_free(read_line);

        Value parse_number = value_native(native_core_parse_number, "core::parse_number");
        environment_define(interp->globals, "core::parse_number", parse_number);
        value_free(parse_number);

        Value parse_boolean = value_native(native_core_parse_boolean, "core::parse_boolean");
        environment_define(interp->globals, "core::parse_boolean", parse_boolean);
        value_free(parse_boolean);

        return true;
    }
    return false;
}

typedef enum {
    MODULE_NOT_FOUND,
    MODULE_LOADED,
    MODULE_FAILED
} ModuleLoadResult;

static char *read_file_internal(const char *path, size_t *out_length, bool suppress_missing, bool *out_missing);

static char *module_candidate_path(const char *base, const char *name) {
    const char *module_dir = "libs/";
    size_t base_len = base ? strlen(base) : 0;
    size_t needs_sep = (base_len > 0 && !is_path_separator(base[base_len - 1])) ? 1 : 0;
    size_t path_len = base_len + needs_sep + strlen(module_dir) + strlen(name) + 3 + 1;
    char *path = (char *)malloc(path_len);
    CHECK_ALLOC(path);
    size_t pos = 0;
    if (base_len > 0) {
        memcpy(path, base, base_len);
        pos += base_len;
        if (needs_sep) {
            path[pos++] = '/';
        }
    }
    memcpy(path + pos, module_dir, strlen(module_dir));
    pos += strlen(module_dir);
    size_t name_len = strlen(name);
    memcpy(path + pos, name, name_len);
    pos += name_len;
    memcpy(path + pos, ".ls", 3);
    pos += 3;
    path[pos] = '\0';
    return path;
}

static ModuleLoadResult load_script_module(Interpreter *interp, const char *name) {
    bool saw_missing = false;
    for (size_t i = 0; i < interp->module_paths.count; ++i) {
        char *candidate = module_candidate_path(interp->module_paths.items[i], name);
        bool missing = false;
        size_t length = 0;
        char *source = read_file_internal(candidate, &length, true, &missing);
        free(candidate);

        if (missing) {
            saw_missing = true;
            continue;
        }
        if (source == NULL) {
            return MODULE_FAILED;
        }

        int status = interpret_source(interp, source, true);
        free(source);
        if (status != 0) {
            interp->had_runtime_error = true;
            return MODULE_FAILED;
        }
        return MODULE_LOADED;
    }
    return saw_missing ? MODULE_NOT_FOUND : MODULE_NOT_FOUND;
}

static bool interpreter_load_module(Interpreter *interp, const char *name, int line) {
    if (interp->had_runtime_error) {
        return false;
    }
    if (string_list_contains(&interp->loaded_modules, name)) {
        return true;
    }
    if (string_list_contains(&interp->loading_modules, name)) {
        lie_error_report(LIE_ERROR_RUNTIME, line, NULL, 0,
                         "Circular gather of '%s'", name);
        lie_error_hint("Review module '%s' to break the loop of gather statements.", name);
        interp->had_runtime_error = true;
        return false;
    }

    string_list_push(&interp->loading_modules, name);

    bool loaded = false;
    if (load_native_module(interp, name)) {
        loaded = true;
    } else {
        ModuleLoadResult result = load_script_module(interp, name);
        if (result == MODULE_LOADED) {
            loaded = true;
        } else if (result == MODULE_FAILED) {
            /* errors already reported */
        }
    }

    string_list_pop(&interp->loading_modules);

    if (loaded) {
        if (!string_list_contains(&interp->loaded_modules, name)) {
            string_list_push(&interp->loaded_modules, name);
        }
        return true;
    }

    if (!interp->had_runtime_error) {
        lie_error_report(LIE_ERROR_RUNTIME, line, NULL, 0,
                         "Unknown library '%s'", name);
        if (interp->module_paths.count > 0) {
            size_t total = 0;
            for (size_t i = 0; i < interp->module_paths.count; ++i) {
                char *candidate = module_candidate_path(interp->module_paths.items[i], name);
                total += strlen(candidate) + 2;
                free(candidate);
            }
            char *locations = (char *)malloc(total + 1);
            CHECK_ALLOC(locations);
            size_t pos = 0;
            for (size_t i = 0; i < interp->module_paths.count; ++i) {
                char *candidate = module_candidate_path(interp->module_paths.items[i], name);
                size_t len = strlen(candidate);
                memcpy(locations + pos, candidate, len);
                pos += len;
                if (i + 1 < interp->module_paths.count) {
                    locations[pos++] = ',';
                    locations[pos++] = ' ';
                }
                free(candidate);
            }
            locations[pos] = '\0';
            lie_error_hint("Searched locations: %s", locations);
            free(locations);
        }
        lie_error_hint("Ensure a matching 'libs/%s.ls' exists in one of the search roots or provide a native module with that name.", name);
        interp->had_runtime_error = true;
    }
    return false;
}

static Value eval_literal(Literal literal) {
    switch (literal.type) {
        case LITERAL_NUMBER:
            return value_number(literal.as.number);
        case LITERAL_STRING:
            return value_string(literal.as.string);
        case LITERAL_BOOL:
            return value_bool(literal.as.boolean);
        case LITERAL_NOTHING:
            return value_nothing();
    }
    return value_nothing();
}

static Value eval_unary(Interpreter *interp, UnaryExpr unary) {
    Value right = eval_expression(interp, unary.right);
    if (interp->had_runtime_error) {
        value_free(right);
        return value_nothing();
    }
    switch (unary.op.type) {
        case TOKEN_MINUS:
            if (right.type != VALUE_NUMBER) {
                runtime_error(interp, unary.op.line, "Unary '-' expects a number",
                                   "Ensure the expression after '-' evaluates to a number.");
                value_free(right);
                return value_nothing();
            }
            right.as.number = -right.as.number;
            return right;
        case TOKEN_NOT: {
            bool truth = value_is_truthy(right);
            value_free(right);
            return value_bool(!truth);
        }
        default:
            break;
    }
    value_free(right);
    return value_nothing();
}

static Value eval_binary(Interpreter *interp, BinaryExpr binary) {
    TokenType op = binary.op.type;
    if (op == TOKEN_AND || op == TOKEN_OR) {
        Value left = eval_expression(interp, binary.left);
        if (interp->had_runtime_error) {
            value_free(left);
            return value_nothing();
        }
        bool left_truth = value_is_truthy(left);
        if (op == TOKEN_OR) {
            if (left_truth) {
                return left;
            }
            value_free(left);
            return eval_expression(interp, binary.right);
        }
        /* TOKEN_AND */
        if (!left_truth) {
            value_free(left);
            return value_bool(false);
        }
        value_free(left);
        Value right = eval_expression(interp, binary.right);
        if (right.type == VALUE_BOOL) {
            bool truth = value_is_truthy(right);
            value_free(right);
            return value_bool(truth);
        }
        return right;
    }

    Value left = eval_expression(interp, binary.left);
    if (interp->had_runtime_error) {
        value_free(left);
        return value_nothing();
    }
    Value right = eval_expression(interp, binary.right);
    if (interp->had_runtime_error) {
        value_free(left);
        value_free(right);
        return value_nothing();
    }

    Value result = value_nothing();

    switch (op) {
        case TOKEN_PLUS:
            if (left.type == VALUE_STRING || right.type == VALUE_STRING) {
                char *left_text = value_to_string(left);
                char *right_text = value_to_string(right);
                size_t len = strlen(left_text) + strlen(right_text);
                char *joined = (char *)malloc(len + 1);
                CHECK_ALLOC(joined);
                strcpy(joined, left_text);
                strcat(joined, right_text);
                result.type = VALUE_STRING;
                result.as.string = joined;
                free(left_text);
                free(right_text);
            } else if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(left.as.number + right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '+' must be numbers or strings",
                                   "Convert both sides to matching types before adding.");
            }
            break;
        case TOKEN_MINUS:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(left.as.number - right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '-' must be numbers",
                                   "Check that both expressions evaluate to numbers before subtracting.");
            }
            break;
        case TOKEN_STAR:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(left.as.number * right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '*' must be numbers",
                                   "Multiply numeric values only; consider converting or validating inputs first.");
            }
            break;
        case TOKEN_SLASH:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                if (right.as.number == 0.0) {
                    runtime_error(interp, binary.op.line, "Division by zero",
                                       "Guard the divisor or ensure it never becomes zero before dividing.");
                } else {
                    result = value_number(left.as.number / right.as.number);
                }
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '/' must be numbers",
                                   "Double-check both sides of the division are numeric.");
            }
            break;
        case TOKEN_PERCENT:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(fmod(left.as.number, right.as.number));
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '%' must be numbers",
                                   "Use numeric values when applying the remainder operator.");
            }
            break;
        case TOKEN_GREATER:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number > right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '>' must be numbers",
                                   "Compare numeric quantities; for strings consider custom comparison helpers.");
            }
            break;
        case TOKEN_GREATER_EQUAL:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number >= right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '>=' must be numbers",
                                   "Ensure both expressions evaluate to numbers before comparing.");
            }
            break;
        case TOKEN_LESS:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number < right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '<' must be numbers",
                                   "Consider converting your values to numbers prior to comparison.");
            }
            break;
        case TOKEN_LESS_EQUAL:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number <= right.as.number);
            } else {
                runtime_error(interp, binary.op.line,
                                   "Operands to '<=' must be numbers",
                                   "Numbers are required on both sides of '<=' comparisons.");
            }
            break;
        case TOKEN_IS:
            result = value_bool(value_equal(left, right));
            break;
        case TOKEN_ISNT:
            result = value_bool(!value_equal(left, right));
            break;
        default:
            runtime_error(interp, binary.op.line, "Unsupported binary operator",
                               "This operator is not implemented yet; consider rewriting the expression.");
            break;
    }

    value_free(left);
    value_free(right);
    return result;
}

static Value eval_variable(Interpreter *interp, VariableExpr variable, int line) {
    Value value;
    if (!environment_get(interp->current, variable.name, &value)) {
        runtime_error(interp, line, "Unknown name",
                           "Declare '%s' with 'let' before using it or gather the module that provides it.",
                           variable.name);
        return value_nothing();
    }
    return value;
}

static Value eval_call(Interpreter *interp, CallExpr call, int line) {
    Value callee = eval_expression(interp, call.callee);
    if (interp->had_runtime_error) {
        value_free(callee);
        return value_nothing();
    }

    Value *args = NULL;
    if (call.arg_count > 0) {
        args = (Value *)malloc(sizeof(Value) * call.arg_count);
        CHECK_ALLOC(args);
    }
    for (size_t i = 0; i < call.arg_count; ++i) {
        args[i] = eval_expression(interp, call.arguments[i]);
        if (interp->had_runtime_error) {
            for (size_t j = 0; j <= i; ++j) {
                value_free(args[j]);
            }
            free(args);
            value_free(callee);
            return value_nothing();
        }
    }

    Value result = value_nothing();
    if (callee.type == VALUE_NATIVE) {
        result = callee.as.native.fn(interp, (int)call.arg_count, args, line);
    } else if (callee.type == VALUE_FUNCTION) {
        Function *function = callee.as.function;
        if (call.arg_count != function->declaration->param_count) {
            runtime_error(interp, line, "Arity mismatch in call",
                          "The routine '%s' expects %zu argument%s but received %zu.",
                          function->declaration->name,
                          function->declaration->param_count,
                          function->declaration->param_count == 1 ? "" : "s",
                          call.arg_count);
        } else {
            Environment *local = environment_new(function->closure);

            for (size_t i = 0; i < call.arg_count; ++i) {
                environment_define(local, function->declaration->parameters[i], args[i]);
            }

            Environment *previous = interp->current;
            interp->current = local;
            interp->call_depth++;

            ExecResult exec = execute_block(interp, &function->declaration->body->as.block);

            interp->call_depth--;
            interp->current = previous;

            if (exec.flow == EXEC_FLOW_RETURN) {
                result = exec.value;
            } else {
                result = value_nothing();
            }

            environment_release(local);
        }
    } else {
        runtime_error(interp, line, "Value is not callable",
                       "Try adding parentheses to call the value or ensure it refers to a routine.");
    }

    for (size_t i = 0; i < call.arg_count; ++i) {
        value_free(args[i]);
    }
    free(args);
    value_free(callee);
    return result;
}

static Value eval_expression(Interpreter *interp, Expr *expr) {
    if (expr == NULL) {
        return value_nothing();
    }
    switch (expr->type) {
        case EXPR_LITERAL:
            return eval_literal(expr->as.literal);
        case EXPR_GROUPING:
            return eval_expression(interp, expr->as.grouping);
        case EXPR_UNARY:
            return eval_unary(interp, expr->as.unary);
        case EXPR_BINARY:
            return eval_binary(interp, expr->as.binary);
        case EXPR_VARIABLE:
            return eval_variable(interp, expr->as.variable, expr->line);
        case EXPR_CALL:
            return eval_call(interp, expr->as.call, expr->line);
        case EXPR_LIST: {
            Value list = value_list();
            for (size_t i = 0; i < expr->as.list.count; ++i) {
                Expr *element_expr = expr->as.list.elements[i];
                Value element = eval_expression(interp, element_expr);
                if (interp->had_runtime_error) {
                    value_free(element);
                    value_free(list);
                    return value_nothing();
                }
                list_append(&list, element);
            }
            return list;
        }
        case EXPR_RECORD: {
            Value record = value_record();
            for (size_t i = 0; i < expr->as.record.count; ++i) {
                const char *key = expr->as.record.keys[i];
                Expr *value_expr = expr->as.record.values[i];
                Value field = eval_expression(interp, value_expr);
                if (interp->had_runtime_error) {
                    value_free(field);
                    value_free(record);
                    return value_nothing();
                }
                record_append(&record, key, field);
            }
            return record;
        }
        case EXPR_INDEX: {
            Value collection = eval_expression(interp, expr->as.index.collection);
            if (interp->had_runtime_error) {
                value_free(collection);
                return value_nothing();
            }
            Value index = eval_expression(interp, expr->as.index.index);
            if (interp->had_runtime_error) {
                value_free(collection);
                value_free(index);
                return value_nothing();
            }

            Value result = value_nothing();
            if (collection.type == VALUE_LIST) {
                if (index.type != VALUE_NUMBER) {
                    runtime_error(interp, expr->line, "List indices must be numbers",
                                  "Pass a numeric index when indexing a list.");
                } else {
                    double idx_double = index.as.number;
                    double floored = floor(idx_double);
                    if (idx_double != floored) {
                        runtime_error(interp, expr->line, "List index must be an integer",
                                      "Use whole numbers when indexing lists.");
                    } else {
                        ssize_t idx = (ssize_t)floored;
                        if (idx < 0 || (size_t)idx >= collection.as.list.count) {
                            runtime_error(interp, expr->line, "List index out of bounds",
                                          "Valid indices range from 0 to %zu.",
                                          collection.as.list.count == 0 ? 0 : collection.as.list.count - 1);
                        } else {
                            result = value_copy(collection.as.list.items[idx]);
                        }
                    }
                }
            } else if (collection.type == VALUE_RECORD) {
                const char *key_ref = NULL;
                char *owned_key = NULL;
                if (index.type == VALUE_STRING) {
                    key_ref = index.as.string;
                } else {
                    owned_key = value_to_string(index);
                    key_ref = owned_key;
                }
                if (key_ref != NULL) {
                    ssize_t entry = record_find_index(&collection.as.record, key_ref);
                    if (entry < 0) {
                        runtime_error(interp, expr->line, "Record key not found",
                                      "No field named '%s' exists in this record.", key_ref);
                    } else {
                        result = value_copy(collection.as.record.values[entry]);
                    }
                }
                free(owned_key);
            } else {
                runtime_error(interp, expr->line, "Cannot index value",
                              "Only lists and records support indexing with '[ ]'.");
            }

            value_free(collection);
            value_free(index);
            return result;
        }
    }
    return value_nothing();
}

static ExecResult execute_statement(Interpreter *interp, Stmt *stmt) {
    if (interp->had_runtime_error) return exec_result_none();
    switch (stmt->type) {
        case STMT_GATHER:
            interpreter_load_module(interp, stmt->as.gather.module_name, stmt->line);
            return exec_result_none();
        case STMT_LET: {
            Value value = eval_expression(interp, stmt->as.let_stmt.value);
            if (!interp->had_runtime_error) {
                environment_define(interp->current, stmt->as.let_stmt.name, value);
            }
            value_free(value);
            return exec_result_none();
        }
        case STMT_SET: {
            Value value = eval_expression(interp, stmt->as.set_stmt.value);
            if (!interp->had_runtime_error) {
                if (!environment_assign(interp->current, stmt->as.set_stmt.name, value)) {
                    runtime_error(interp, stmt->line, "Unknown name in 'set'",
                                   "Create '%s' with 'let' before attempting to update it.",
                                   stmt->as.set_stmt.name);
                }
            }
            value_free(value);
            return exec_result_none();
        }
        case STMT_EXPR: {
            Value value = eval_expression(interp, stmt->as.expr);
            value_free(value);
            return exec_result_none();
        }
        case STMT_BLOCK:
            return execute_block(interp, &stmt->as.block);
        case STMT_IF: {
            Value condition = eval_expression(interp, stmt->as.if_stmt.condition);
            bool truth = value_is_truthy(condition);
            value_free(condition);
            if (interp->had_runtime_error) {
                return exec_result_none();
            }
            if (truth) {
                return execute_statement(interp, stmt->as.if_stmt.then_branch);
            }
            if (stmt->as.if_stmt.else_branch != NULL) {
                return execute_statement(interp, stmt->as.if_stmt.else_branch);
            }
            return exec_result_none();
        }
        case STMT_WHILST: {
            interp->loop_depth++;
            ExecResult loop_result = exec_result_none();
            while (true) {
                Value condition = eval_expression(interp, stmt->as.whilst.condition);
                bool truth = value_is_truthy(condition);
                value_free(condition);
                if (!truth || interp->had_runtime_error) {
                    break;
                }
                ExecResult body_result = execute_statement(interp, stmt->as.whilst.body);
                if (body_result.flow == EXEC_FLOW_RETURN) {
                    loop_result = body_result;
                    break;
                }
                if (body_result.flow == EXEC_FLOW_BREAK) {
                    loop_result = exec_result_none();
                    break;
                }
                if (body_result.flow == EXEC_FLOW_CONTINUE) {
                    continue;
                }
                if (body_result.flow != EXEC_FLOW_NONE) {
                    loop_result = body_result;
                    break;
                }
                if (interp->had_runtime_error) {
                    break;
                }
            }
            interp->loop_depth--;
            return loop_result;
        }
        case STMT_FUNCTION: {
            Function *function = function_new(&stmt->as.function, interp->current);
            Value fn_value = value_function(function);
            environment_define(interp->current, stmt->as.function.name, fn_value);
            value_free(fn_value);
            return exec_result_none();
        }
        case STMT_HALT: {
            if (interp->call_depth == 0) {
                runtime_error(interp, stmt->line, "'halt' outside of a routine",
                               "Use 'halt' inside a routine declared with 'note'.");
                return exec_result_none();
            }
            Value value;
            if (stmt->as.halt.value != NULL) {
                value = eval_expression(interp, stmt->as.halt.value);
            } else {
                value = value_nothing();
            }
            return exec_result_value(value);
        }
        case STMT_BREAK: {
            if (interp->loop_depth == 0) {
                runtime_error(interp, stmt->line, "'break' outside of a loop",
                               "Only use 'break' within 'whilst' blocks.");
                return exec_result_none();
            }
            ExecResult result = exec_result_none();
            result.flow = EXEC_FLOW_BREAK;
            return result;
        }
        case STMT_CONTINUE: {
            if (interp->loop_depth == 0) {
                runtime_error(interp, stmt->line, "'continue' outside of a loop",
                               "Only use 'continue' within 'whilst' blocks.");
                return exec_result_none();
            }
            ExecResult result = exec_result_none();
            result.flow = EXEC_FLOW_CONTINUE;
            return result;
        }
    }
    return exec_result_none();
}

static ExecResult execute_block(Interpreter *interp, BlockStmt *block) {
    Environment *previous = interp->current;
    Environment *local = environment_new(previous);
    interp->current = local;

    ExecResult result = exec_result_none();
    for (size_t i = 0; i < block->count; ++i) {
        result = execute_statement(interp, block->statements[i]);
        if (result.flow != EXEC_FLOW_NONE || interp->had_runtime_error) {
            break;
        }
    }

    interp->current = previous;
    environment_release(local);

    return result;
}

static void execute_program(Interpreter *interp, Stmt **statements, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        ExecResult result = execute_statement(interp, statements[i]);
        if (result.flow == EXEC_FLOW_RETURN) {
            runtime_error(interp, statements[i]->line, "Unexpected 'halt' at top-level",
                           "Only call 'halt' inside routines; wrap top-level work in 'note main(): ...'.");
            value_free(result.value);
            break;
        }
        if (result.flow == EXEC_FLOW_BREAK || result.flow == EXEC_FLOW_CONTINUE) {
            runtime_error(interp, statements[i]->line, "Loop control outside of a loop",
                           "Use 'break' or 'continue' only inside 'whilst' blocks.");
            break;
        }
        if (interp->had_runtime_error) {
            break;
        }
    }
}

/* ========================= CLEANUP HELPERS ========================= */

static void free_expr(Expr *expr) {
    if (expr == NULL) return;
    switch (expr->type) {
        case EXPR_LITERAL:
            if (expr->as.literal.type == LITERAL_STRING) {
                free(expr->as.literal.as.string);
            }
            break;
        case EXPR_GROUPING:
            free_expr(expr->as.grouping);
            break;
        case EXPR_UNARY:
            free_expr(expr->as.unary.right);
            break;
        case EXPR_BINARY:
            free_expr(expr->as.binary.left);
            free_expr(expr->as.binary.right);
            break;
        case EXPR_VARIABLE:
            free(expr->as.variable.name);
            break;
        case EXPR_CALL:
            free_expr(expr->as.call.callee);
            for (size_t i = 0; i < expr->as.call.arg_count; ++i) {
                free_expr(expr->as.call.arguments[i]);
            }
            free(expr->as.call.arguments);
            break;
        case EXPR_LIST:
            for (size_t i = 0; i < expr->as.list.count; ++i) {
                free_expr(expr->as.list.elements[i]);
            }
            free(expr->as.list.elements);
            break;
        case EXPR_RECORD:
            for (size_t i = 0; i < expr->as.record.count; ++i) {
                free(expr->as.record.keys[i]);
                free_expr(expr->as.record.values[i]);
            }
            free(expr->as.record.keys);
            free(expr->as.record.values);
            break;
        case EXPR_INDEX:
            free_expr(expr->as.index.collection);
            free_expr(expr->as.index.index);
            break;
    }
    free(expr);
}

static void free_stmt(Stmt *stmt) {
    if (stmt == NULL) return;
    switch (stmt->type) {
        case STMT_GATHER:
            free(stmt->as.gather.module_name);
            break;
        case STMT_LET:
            free(stmt->as.let_stmt.name);
            free_expr(stmt->as.let_stmt.value);
            break;
        case STMT_SET:
            free(stmt->as.set_stmt.name);
            free_expr(stmt->as.set_stmt.value);
            break;
        case STMT_EXPR:
            free_expr(stmt->as.expr);
            break;
        case STMT_BLOCK:
            for (size_t i = 0; i < stmt->as.block.count; ++i) {
                free_stmt(stmt->as.block.statements[i]);
            }
            free(stmt->as.block.statements);
            break;
        case STMT_IF:
            free_expr(stmt->as.if_stmt.condition);
            free_stmt(stmt->as.if_stmt.then_branch);
            free_stmt(stmt->as.if_stmt.else_branch);
            break;
        case STMT_WHILST:
            free_expr(stmt->as.whilst.condition);
            free_stmt(stmt->as.whilst.body);
            break;
        case STMT_FUNCTION:
            free(stmt->as.function.name);
            for (size_t i = 0; i < stmt->as.function.param_count; ++i) {
                free(stmt->as.function.parameters[i]);
            }
            free(stmt->as.function.parameters);
            free_stmt(stmt->as.function.body);
            break;
        case STMT_HALT:
            free_expr(stmt->as.halt.value);
            break;
        case STMT_BREAK:
        case STMT_CONTINUE:
            break;
    }
    free(stmt);
}

static void free_program(Stmt **statements, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        free_stmt(statements[i]);
    }
    free(statements);
}

/* ========================= DRIVER ========================= */

static char *read_file_internal(const char *path, size_t *out_length, bool suppress_missing, bool *out_missing) {
    if (out_missing) {
        *out_missing = false;
    }
    errno = 0;
    FILE *file = fopen(path, "rb");
    if (!file) {
        if (suppress_missing && errno == ENOENT) {
            if (out_missing) {
                *out_missing = true;
            }
            return NULL;
        }
        lie_error_system(path, errno);
        return NULL;
    }
    if (fseek(file, 0L, SEEK_END) != 0) {
        lie_error_system(path, errno);
        fclose(file);
        return NULL;
    }
    long size = ftell(file);
    if (size < 0) {
        lie_error_system(path, errno);
        fclose(file);
        return NULL;
    }
    rewind(file);
    char *buffer = (char *)malloc((size_t)size + 1);
    CHECK_ALLOC(buffer);
    size_t read = fread(buffer, 1, (size_t)size, file);
    if (ferror(file)) {
        lie_error_system(path, errno);
        fclose(file);
        free(buffer);
        return NULL;
    }
    buffer[read] = '\0';
    fclose(file);
    if (out_length) {
        *out_length = read;
    }
    return buffer;
}

static char *read_file(const char *path, size_t *out_length) {
    return read_file_internal(path, out_length, false, NULL);
}

static int interpret_source(Interpreter *interp, const char *source, bool keep_program) {
    Lexer lexer;
    lexer_init(&lexer, source);

    TokenBuffer tokens;
    token_buffer_init(&tokens);

    for (;;) {
        Token token = lexer_next_token(&lexer);
        token_buffer_push(&tokens, token);
        if (token.type == TOKEN_EOF || token.type == TOKEN_ERROR) {
            break;
        }
    }

    if (tokens.count > 0 && tokens.data[tokens.count - 1].type == TOKEN_ERROR) {
        Token error_token_obj = tokens.data[tokens.count - 1];
        lie_error_report(LIE_ERROR_LEX, error_token_obj.line, NULL, 0, "%s", error_token_obj.message);
        token_buffer_free(&tokens);
        return 65;
    }

    Parser parser;
    parser_init(&parser, tokens.data, tokens.count);
    size_t stmt_count = 0;
    Stmt **statements = parse_program(&parser, &stmt_count);

    if (parser.had_error) {
        free_program(statements, stmt_count);
        token_buffer_free(&tokens);
        return 65;
    }

    execute_program(interp, statements, stmt_count);
    int status = interp->had_runtime_error ? 70 : 0;

    if (keep_program && status == 0) {
        program_store_push(&interp->stored_programs, statements, stmt_count);
    } else {
        free_program(statements, stmt_count);
    }
    token_buffer_free(&tokens);
    return status;
}

int lie_run_file(const char *path) {
    size_t length = 0;
    char *source = read_file(path, &length);
    if (source == NULL) {
        return 74;
    }

    const char *env_home = getenv("LIESEL_HOME");
    if (env_home != NULL && env_home[0] != '\0') {
        lie_register_module_path(env_home);
    }

    Interpreter interp;
    interpreter_init(&interp);
    interpreter_add_script_paths(&interp, path);
    int status = interpret_source(&interp, source, false);
    interpreter_free(&interp);
    free(source);
    return status;
}

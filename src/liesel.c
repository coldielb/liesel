#include "liesel.h"

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#define UNUSED(x) (void)(x)
#define CHECK_ALLOC(ptr)                     \
    do {                                     \
        if ((ptr) == NULL) {                 \
            fprintf(stderr, "Out of memory\n"); \
            exit(74);                        \
        }                                    \
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

/* ========================= LEXER ========================= */

typedef enum {
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
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
    EXPR_CALL
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
    STMT_HALT
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
} Parser;

static void parser_init(Parser *parser, Token *tokens, size_t count) {
    parser->tokens = tokens;
    parser->count = count;
    parser->current = 0;
    parser->had_error = false;
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
    fprintf(stderr, "[line %d] Error", token.line);
    if (token.type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else {
        fprintf(stderr, " at '%.*s'", (int)token.length, token.start);
    }
    fprintf(stderr, ": %s\n", message);
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

    parser_error_at(parser, parser_peek(parser), "Expected expression");
    return NULL;
}

static Expr *parse_call(Parser *parser) {
    Expr *expr = parse_primary(parser);
    if (expr == NULL) return NULL;

    while (parser_match(parser, TOKEN_LEFT_PAREN)) {
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
        return NULL;
    }
    Token name_token = parser_advance(parser);
    if (identifier_has_namespace(name_token)) {
        parser_error_at(parser, name_token, "Bindings cannot contain '::'");
        return NULL;
    }
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
        return NULL;
    }
    Token name_token = parser_advance(parser);
    if (identifier_has_namespace(name_token)) {
        parser_error_at(parser, name_token, "Assignments cannot target '::' names");
        return NULL;
    }
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
        parser_consume(parser, TOKEN_COLON, "Expect ':' after 'otherwise'");
        else_branch = parse_block(parser);
        else_branch->line = otherwise_token.line;
    }

    Stmt *stmt = new_stmt(STMT_IF, keyword.line);
    stmt->as.if_stmt.condition = condition;
    stmt->as.if_stmt.then_branch = then_branch;
    stmt->as.if_stmt.else_branch = else_branch;
    return stmt;
}

static Stmt *parse_whilst_statement(Parser *parser, Token keyword) {
    Expr *condition = parse_expression(parser);
    parser_consume(parser, TOKEN_COLON, "Expect ':' after condition");
    Stmt *body = parse_block(parser);

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
                break;
            }
            Token param = parser_advance(parser);
            if (identifier_has_namespace(param)) {
                parser_error_at(parser, param, "Parameters cannot contain '::'");
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

typedef enum {
    VALUE_NUMBER,
    VALUE_BOOL,
    VALUE_STRING,
    VALUE_NOTHING,
    VALUE_NATIVE,
    VALUE_FUNCTION
} ValueType;

typedef struct Value Value;

typedef Value (*NativeFn)(Interpreter *interp, int arg_count, Value *args, int line);

typedef struct {
    NativeFn fn;
    const char *name;
} Native;

struct Value {
    ValueType type;
    union {
        double number;
        bool boolean;
        char *string;
        Native native;
        Function *function;
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
    fn->closure = closure;
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
    }
    return value_nothing();
}

static void function_free(Function *function) {
    if (function->refcount <= 0) {
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
    }
    return duplicate_cstring("nothing");
}

/* ========================= ENVIRONMENT ========================= */

typedef struct Binding {
    char *name;
    Value value;
} Binding;

typedef struct Environment {
    Binding *entries;
    size_t count;
    size_t capacity;
    struct Environment *enclosing;
} Environment;

static void environment_init(Environment *env, Environment *enclosing) {
    env->entries = NULL;
    env->count = 0;
    env->capacity = 0;
    env->enclosing = enclosing;
}

static void environment_free(Environment *env) {
    for (size_t i = 0; i < env->count; ++i) {
        free(env->entries[i].name);
        value_free(env->entries[i].value);
    }
    free(env->entries);
    env->entries = NULL;
    env->count = 0;
    env->capacity = 0;
    env->enclosing = NULL;
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

/* ========================= INTERPRETER ========================= */

typedef struct {
    bool did_return;
    Value value;
} ExecResult;

struct Interpreter {
    Environment globals;
    Environment *current;
    bool had_runtime_error;
    bool io_loaded;
    int call_depth;
};

static ExecResult exec_result_none(void) {
    ExecResult result;
    result.did_return = false;
    result.value = value_nothing();
    return result;
}

static ExecResult exec_result_value(Value value) {
    ExecResult result;
    result.did_return = true;
    result.value = value;
    return result;
}

static void interpreter_init(Interpreter *interp) {
    environment_init(&interp->globals, NULL);
    interp->current = &interp->globals;
    interp->had_runtime_error = false;
    interp->io_loaded = false;
    interp->call_depth = 0;
}

static void interpreter_free(Interpreter *interp) {
    environment_free(&interp->globals);
}

static void runtime_error(Interpreter *interp, int line, const char *message) {
    if (!interp->had_runtime_error) {
        fprintf(stderr, "[line %d] Runtime error: %s\n", line, message);
    }
    interp->had_runtime_error = true;
}

static Value eval_expression(Interpreter *interp, Expr *expr);
static ExecResult execute_statement(Interpreter *interp, Stmt *stmt);
static ExecResult execute_block(Interpreter *interp, BlockStmt *block);

static Value native_io_echo(Interpreter *interp, int arg_count, Value *args, int line) {
    UNUSED(interp);
    if (arg_count < 1) {
        fprintf(stderr, "[line %d] io::echo expects at least 1 argument\n", line);
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

static void load_io_library(Interpreter *interp) {
    if (interp->io_loaded) return;
    Value echo = value_native(native_io_echo, "io::echo");
    environment_define(&interp->globals, "io::echo", echo);
    value_free(echo);
    interp->io_loaded = true;
}

static void evaluate_gather(Interpreter *interp, GatherStmt *stmt) {
    if (strcmp(stmt->module_name, "io") == 0) {
        load_io_library(interp);
        return;
    }
    fprintf(stderr, "Unknown library '%s'\n", stmt->module_name);
    interp->had_runtime_error = true;
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
                runtime_error(interp, unary.op.line, "Unary '-' expects a number");
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
                runtime_error(interp, binary.op.line, "Operands to '+' must be numbers or strings");
            }
            break;
        case TOKEN_MINUS:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(left.as.number - right.as.number);
            } else {
                runtime_error(interp, binary.op.line, "Operands to '-' must be numbers");
            }
            break;
        case TOKEN_STAR:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(left.as.number * right.as.number);
            } else {
                runtime_error(interp, binary.op.line, "Operands to '*' must be numbers");
            }
            break;
        case TOKEN_SLASH:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                if (right.as.number == 0.0) {
                    runtime_error(interp, binary.op.line, "Division by zero");
                } else {
                    result = value_number(left.as.number / right.as.number);
                }
            } else {
                runtime_error(interp, binary.op.line, "Operands to '/' must be numbers");
            }
            break;
        case TOKEN_PERCENT:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_number(fmod(left.as.number, right.as.number));
            } else {
                runtime_error(interp, binary.op.line, "Operands to '%' must be numbers");
            }
            break;
        case TOKEN_GREATER:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number > right.as.number);
            } else {
                runtime_error(interp, binary.op.line, "Operands to '>' must be numbers");
            }
            break;
        case TOKEN_GREATER_EQUAL:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number >= right.as.number);
            } else {
                runtime_error(interp, binary.op.line, "Operands to '>=' must be numbers");
            }
            break;
        case TOKEN_LESS:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number < right.as.number);
            } else {
                runtime_error(interp, binary.op.line, "Operands to '<' must be numbers");
            }
            break;
        case TOKEN_LESS_EQUAL:
            if (left.type == VALUE_NUMBER && right.type == VALUE_NUMBER) {
                result = value_bool(left.as.number <= right.as.number);
            } else {
                runtime_error(interp, binary.op.line, "Operands to '<=' must be numbers");
            }
            break;
        case TOKEN_IS:
            result = value_bool(value_equal(left, right));
            break;
        case TOKEN_ISNT:
            result = value_bool(!value_equal(left, right));
            break;
        default:
            runtime_error(interp, binary.op.line, "Unsupported binary operator");
            break;
    }

    value_free(left);
    value_free(right);
    return result;
}

static Value eval_variable(Interpreter *interp, VariableExpr variable, int line) {
    Value value;
    if (!environment_get(interp->current, variable.name, &value)) {
        runtime_error(interp, line, "Unknown name");
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
            runtime_error(interp, line, "Arity mismatch in call");
        } else {
            Environment local;
            environment_init(&local, function->closure);

            for (size_t i = 0; i < call.arg_count; ++i) {
                environment_define(&local, function->declaration->parameters[i], args[i]);
            }

            Environment *previous = interp->current;
            interp->current = &local;
            interp->call_depth++;

            ExecResult exec = execute_block(interp, &function->declaration->body->as.block);

            interp->call_depth--;
            interp->current = previous;

            if (exec.did_return) {
                result = exec.value;
            } else {
                result = value_nothing();
            }

            environment_free(&local);
        }
    } else {
        runtime_error(interp, line, "Value is not callable");
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
    }
    return value_nothing();
}

static ExecResult execute_statement(Interpreter *interp, Stmt *stmt) {
    if (interp->had_runtime_error) return exec_result_none();
    switch (stmt->type) {
        case STMT_GATHER:
            evaluate_gather(interp, &stmt->as.gather);
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
                    runtime_error(interp, stmt->line, "Unknown name in 'set'");
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
            if (truth) {
                return execute_statement(interp, stmt->as.if_stmt.then_branch);
            }
            if (stmt->as.if_stmt.else_branch != NULL) {
                return execute_statement(interp, stmt->as.if_stmt.else_branch);
            }
            return exec_result_none();
        }
        case STMT_WHILST: {
            while (true) {
                Value condition = eval_expression(interp, stmt->as.whilst.condition);
                bool truth = value_is_truthy(condition);
                value_free(condition);
                if (!truth || interp->had_runtime_error) {
                    break;
                }
                ExecResult body_result = execute_statement(interp, stmt->as.whilst.body);
                if (body_result.did_return) {
                    return body_result;
                }
            }
            return exec_result_none();
        }
        case STMT_FUNCTION: {
            if (interp->current != &interp->globals) {
                runtime_error(interp, stmt->line, "Functions may currently be declared only at top-level");
                return exec_result_none();
            }
            Function *function = function_new(&stmt->as.function, interp->current);
            Value fn_value = value_function(function);
            environment_define(interp->current, stmt->as.function.name, fn_value);
            value_free(fn_value);
            return exec_result_none();
        }
        case STMT_HALT: {
            if (interp->call_depth == 0) {
                runtime_error(interp, stmt->line, "'halt' outside of a routine");
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
    }
    return exec_result_none();
}

static ExecResult execute_block(Interpreter *interp, BlockStmt *block) {
    for (size_t i = 0; i < block->count; ++i) {
        ExecResult result = execute_statement(interp, block->statements[i]);
        if (result.did_return) {
            return result;
        }
        if (interp->had_runtime_error) {
            return exec_result_none();
        }
    }
    return exec_result_none();
}

static void execute_program(Interpreter *interp, Stmt **statements, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        ExecResult result = execute_statement(interp, statements[i]);
        if (result.did_return) {
            runtime_error(interp, statements[i]->line, "Unexpected 'halt' at top-level");
            value_free(result.value);
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

static char *read_file(const char *path, size_t *out_length) {
    FILE *file = fopen(path, "rb");
    if (!file) {
        fprintf(stderr, "Could not open file '%s': %s\n", path, strerror(errno));
        return NULL;
    }
    if (fseek(file, 0L, SEEK_END) != 0) {
        fclose(file);
        return NULL;
    }
    long size = ftell(file);
    if (size < 0) {
        fclose(file);
        return NULL;
    }
    rewind(file);
    char *buffer = (char *)malloc((size_t)size + 1);
    CHECK_ALLOC(buffer);
    size_t read = fread(buffer, 1, (size_t)size, file);
    buffer[read] = '\0';
    fclose(file);
    if (out_length) {
        *out_length = read;
    }
    return buffer;
}

int lie_run_file(const char *path) {
    size_t length = 0;
    char *source = read_file(path, &length);
    if (source == NULL) {
        return 74;
    }

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
        fprintf(stderr, "[line %d] Lex error: %s\n", tokens.data[tokens.count - 1].line, tokens.data[tokens.count - 1].message);
        free(source);
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
        free(source);
        return 65;
    }

    Interpreter interp;
    interpreter_init(&interp);
    execute_program(&interp, statements, stmt_count);
    int status = interp.had_runtime_error ? 70 : 0;

    interpreter_free(&interp);
    free_program(statements, stmt_count);
    token_buffer_free(&tokens);
    free(source);
    return status;
}

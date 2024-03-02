#include <stdio.h>

#define NOB_IMPLEMENTATION
#include "./nob.h"
#define STB_C_LEXER_IMPLEMENTATION
#include "./stb_c_lexer.h"
#define ARENA_IMPLEMENTATION
#include "./arena.h"

static Arena parser_arena = {0};

typedef struct {
    long token;
    long int_number;
    char *string;
    int string_len;

    const char *path;
    size_t row, column;
} Lexeme;

typedef struct {
    Lexeme *items;
    size_t count;
    size_t capacity;

    size_t pos;
} Lexemes;

typedef struct Expr Expr;

typedef struct {
    Expr **items;
    size_t count;
    size_t capacity;
} Exprs;

typedef enum {
    EXPR_NUMBER,
    EXPR_BINOP,
    EXPR_FUNCALL,
} Expr_Kind;

typedef enum {
    BINOP_PLUS,
    BINOP_MULT,
} Binop_Kind;

typedef struct {
    Binop_Kind kind;
    Expr *lhs;
    Expr *rhs;
} Expr_Binop;

typedef struct {
    Nob_String_View name;
    Exprs args;
} Expr_Funcall;

typedef union {
    int number;
    Expr_Binop binop;
    Expr_Funcall funcall;
} Expr_As;

struct Expr {
    Expr_Kind kind;
    Expr_As as;
};

bool expect_token(Lexemes *lexemes, long token)
{
    if (lexemes->pos >= lexemes->count) {
        fprintf(stderr, "ERROR: expected token %ld, but got the end of the file", token);
        return false;
    }

    Lexeme *lexeme = &lexemes->items[lexemes->pos];
    if (lexeme->token != token) {
        fprintf(stderr, "%s:%d:%d: ERROR: expected token %ld, but got %ld", lexeme->path, lexeme->row, lexeme->column, token, lexeme->token);
        return false;
    }

    return true;
}

Expr *parse_expr(Lexemes *lexemes);

bool parse_args(Lexemes *lexemes, Exprs *args)
{
    if (!expect_token(lexemes, '(')) return false;
    lexemes->pos += 1;

    Expr *arg = parse_expr(lexemes);
    if (arg == NULL) return false;
    nob_da_append(args, arg);

    if (!expect_token(lexemes, ')')) return false;
    lexemes->pos += 1;

    return true;
}

Expr *parse_primary(Lexemes *lexemes)
{
    if (lexemes->pos >= lexemes->count) {
        fprintf(stderr, "ERROR: expected number or identifier, but got the end of the file");
        return NULL;
    }

    Lexeme *lexeme = &lexemes->items[lexemes->pos];
    lexemes->pos += 1;
    switch (lexeme->token) {
        case CLEX_intlit: {
            Expr *num = arena_alloc(&parser_arena, sizeof(Expr));
            num->kind = EXPR_NUMBER;
            num->as.number = lexeme->int_number;
            return num;
        } break;

        case CLEX_id: {
            Expr *funcall = arena_alloc(&parser_arena, sizeof(Expr));
            funcall->kind = EXPR_FUNCALL;
            funcall->as.funcall.name.data = lexeme->string;
            funcall->as.funcall.name.count = lexeme->string_len;
            if (!parse_args(lexemes, &funcall->as.funcall.args)) return NULL;
            return funcall;
        } break;

        default: {
            fprintf(stderr, "%s:%d:%d: Unexpected token\n", lexeme->path, lexeme->row, lexeme->column);
            return NULL;
        }
    }

    return NULL;
}

Expr *parse_expr_mult(Lexemes *lexemes)
{
    Expr *lhs = parse_primary(lexemes);
    if (!lhs) return NULL;
    if (lexemes->pos < lexemes->count && lexemes->items[lexemes->pos].token == '*') {
        lexemes->pos += 1;
        Expr *rhs = parse_primary(lexemes);
        if (!rhs) return NULL;
        Expr *mult = arena_alloc(&parser_arena, sizeof(Expr));
        *mult = (Expr) {
            .kind = EXPR_BINOP,
            .as = {
                .binop = {
                    .kind = BINOP_MULT,
                    .lhs = lhs,
                    .rhs = rhs,
                }
            }
        };
        return mult;
    }
    return lhs;
}

Expr *parse_expr_plus(Lexemes *lexemes)
{
    Expr *lhs = parse_expr_mult(lexemes);
    if (!lhs) return NULL;
    if (lexemes->pos < lexemes->count && lexemes->items[lexemes->pos].token == '+') {
        lexemes->pos += 1;
        Expr *rhs = parse_expr_mult(lexemes);
        if (!rhs) return NULL;
        Expr *plus = arena_alloc(&parser_arena, sizeof(Expr));
        *plus = (Expr) {
            .kind = EXPR_BINOP,
            .as = {
                .binop = {
                    .kind = BINOP_PLUS,
                    .lhs = lhs,
                    .rhs = rhs,
                }
            }
        };
        return plus;
    }
    return lhs;
}

Expr *parse_expr(Lexemes *lexemes)
{
    return parse_expr_plus(lexemes);
}

void compile_expr(FILE *out, Expr *expr, size_t *stack_size)
{
    // stack_size = 5
    // 0 1 2 3 4

    switch (expr->kind) {
        case EXPR_NUMBER: {
            fprintf(out, "    %%s%zu =w copy %d\n", *stack_size, expr->as.number);
            *stack_size += 1;
        } break;
        case EXPR_BINOP: {
            compile_expr(out, expr->as.binop.lhs, stack_size);
            compile_expr(out, expr->as.binop.rhs, stack_size);
            switch (expr->as.binop.kind) {
                case BINOP_PLUS: {
                    assert(*stack_size >= 2);
                    fprintf(out, "    %%s%zu =w add %%s%zu, %%s%zu\n", *stack_size - 2, *stack_size - 2, *stack_size - 1);
                    *stack_size -= 1;
                } break;

                case BINOP_MULT: {
                    assert(*stack_size >= 2);
                    fprintf(out, "    %%s%zu =w mul %%s%zu, %%s%zu\n", *stack_size - 2, *stack_size - 2, *stack_size - 1);
                    *stack_size -= 1;
                } break;
            }
        } break;
        case EXPR_FUNCALL: {
            assert(nob_sv_eq(expr->as.funcall.name, nob_sv_from_cstr("print")));
            assert(expr->as.funcall.args.count == 1);
            compile_expr(out, expr->as.funcall.args.items[0], stack_size);
            fprintf(out, "    call $printf(l $fmt_int, ..., w %%s%zu)\n", *stack_size - 1);
            *stack_size -= 1;
        } break;
    }
}

int main(int argc, char **argv)
{
    const char *program = nob_shift_args(&argc, &argv);

    if (argc <= 0) {
        nob_log(NOB_ERROR, "Usage: %s <input.py> <output.ssa>", program);
        nob_log(NOB_ERROR, "No input is not provided");
        return 1;
    }
    const char *path = nob_shift_args(&argc, &argv);

    if (argc <= 0) {
        nob_log(NOB_ERROR, "Usage: %s <input.py> <output.ssa>", program);
        nob_log(NOB_ERROR, "No output is not provided");
        return 1;
    }
    const char *out_path = nob_shift_args(&argc, &argv);

    Nob_String_Builder sb = {0};
    if (!nob_read_entire_file(path, &sb)) return 0;

    int store_length = 640*1024;
    char *string_store = malloc(store_length);
    assert(string_store != NULL);

    stb_lexer lexer = {0};
    stb_c_lexer_init(&lexer, sb.items, sb.items + sb.count, string_store, store_length);

    FILE *out = fopen(out_path, "wb");
    if (out == NULL) {
        nob_log(NOB_ERROR, "Could not open file %s for writing: %s", out_path, strerror(errno));
        return 1;
    }

    Lexemes lexemes = {0};
    while (stb_c_lexer_get_token(&lexer)) {
        stb_lex_location loc = {0};
        stb_c_lexer_get_location(&lexer, lexer.where_firstchar, &loc);
        loc.line_offset += 1;
        Lexeme lexeme = {
            .token = lexer.token,
            .path = path,
            .row = loc.line_number,
            .column = loc.line_offset,
        };
        switch (lexer.token) {
            case '+': case '(': case '*': case ')': {} break;
            case CLEX_id: {
                size_t string_len = strlen(lexer.string);
                lexeme.string = arena_alloc(&parser_arena, string_len);
                memcpy(lexeme.string, lexer.string, string_len);
                lexeme.string_len = string_len;
            } break;

            case CLEX_intlit: {
                lexeme.int_number = lexer.int_number;
            } break;

            default: {
                assert(0 && "Unexpected token");
            } break;
        }
        nob_da_append(&lexemes, lexeme);
    }


    fprintf(out, "export function w $main() {\n");
    fprintf(out, "@start\n");

    size_t stack_size = 0;
    while (lexemes.pos < lexemes.count) {
        Expr *root = parse_expr(&lexemes);
        if (root == NULL) return 1;
        compile_expr(out, root, &stack_size);
    }

    fprintf(out, "    ret 0\n");
    fprintf(out, "}\n");
    fprintf(out, "data $fmt_int = { b \"%%d\\n\", b 0 }\n");

    fclose(out);

    return 0;
}

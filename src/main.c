#include "stdio.h"
#include "stdlib.h"
#include "ctype.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"


#define STACK_SIZE 1024
#define MAX_TOKENS 512
#define MAX_NAME_LENGTH 16
#define MAX_ARGS_COUNT 16
#define MAX_EXPRESSIONS 1024


typedef struct 
{
    size_t length;
    char data[];
} StringSlice;

typedef enum  
{
    T_NAME,
    T_LITERAL,
    T_BINOP,
    T_COMMA,
    T_SEMI,
    T_OPPAR,
    T_CLPAR,
    T_OPCURL,
    T_CLCURL,
} TokenKind;

typedef enum
{
    BI_PLUS,
    BI_MINUS,
    BI_MULT,
    BI_DIV,
    BI_EQ,
} BinopKind;

typedef enum
{
    EX_CALL,
    EX_BINOP,
    EX_ARG,
} ExpressionKind;

typedef enum 
{
    ARG_LITERAL,
    ARG_VAR,
} ArgumentKind;

typedef struct  
{
    TokenKind kind;
    union 
    {
        int64_t lit;
        char name[MAX_NAME_LENGTH];
    } value;
    struct
    {
        BinopKind kind;
    } binop;
} Token;

typedef struct 
{
    struct 
    {
        size_t length;
        // limit tokens, for now
        Token data[MAX_TOKENS]; 
    } tokens;

    StringSlice *file;
    
    size_t cur;

} Lexer;

typedef struct 
{
    ExpressionKind kind;
    ArgumentKind arg;
    int64_t value;

    struct {
        size_t lhs;
        size_t rhs;
        BinopKind kind;
    } binop;
    
    // in case of a call
    // argc - count of agrs
    size_t argc;
    size_t args[MAX_ARGS_COUNT];

} Expression;

typedef struct 
{
    char name[MAX_NAME_LENGTH];
    
    struct {
        size_t length;
        Expression data[MAX_EXPRESSIONS];
    } expressions;

    struct {
        size_t length;
        char data[STACK_SIZE][MAX_NAME_LENGTH];
    } stackVars;
    
} Function;


typedef struct
{
    struct {
        size_t length;
        Function data[MAX_EXPRESSIONS];
    } functions;
} Program;

StringSlice *readEntireFile();
bool parseCode(Lexer *lex);
void printExpression(Expression expressions[], size_t index);
bool runFunction(Program *program, const char *name);

static Program p = {0};

int main(int argc, char **argv) 
{
    if(argc <= 1) {
        printf("ERROR: no input is provided");
        return -1;
    }

    const char *inputPath = argv[1];

    printf("INFO: Input path: %s\n", inputPath);

    Lexer l = {0};
    
    l.file = readEntireFile(inputPath);

    if(!l.file) {
        return -1;
    }


    parseCode(&l);

    runFunction(&p, "main");

    free(l.file);
    
    return 0;
}

bool runFunction(Program *program, const char *name)
{
    int index = -1;
    for(int i = 0; i < p.functions.length; ++i)
    {
        if(strcmp(p.functions.data[i].name, name) == 0) 
        {
            index = i;
            break;
        }
    }
    if(index < 0)
    {
        printf("ERROR: no such function \"%s\".", name);
        return false;
    }
    
    size_t allocatedSize = p.functions.data[index].stackVars.length;
    int64_t stack[STACK_SIZE];

    int64_t *sbp = ((&stack[STACK_SIZE - 1]) - allocatedSize);
    printf("\nRunning function \"%s\"\n", name);
    printf("INFO: Allocated %zu words\n", allocatedSize);
    printf("INFO: sbp is 0x%p\n", sbp);

    for(int i = 0; i < p.functions.data[index].expressions.length; ++i)
    {
        Expression *expr = &p.functions.data[index].expressions.data[i];
        if(expr->binop.kind == BI_EQ) {
            int64_t *ptr = sbp - p.functions.data[index].expressions.data[expr->binop.lhs].value;
            *ptr = p.functions.data[index].expressions.data[expr->binop.rhs].value;
            continue;
        }

        if(expr->kind == EX_CALL) {
            printf("%ld\n",  *(sbp - p.functions.data[index].expressions.data[expr->args[0]].value));
            continue;
        }
    }

    return true;
}


StringSlice *readEntireFile(const char *path) 
{
    FILE *file = fopen(path, "r");

    if(!file) {
        perror("ERROR");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    rewind(file);

    StringSlice *slice = (StringSlice *) malloc(size + sizeof(size_t));
    slice->length =fread(slice->data, 1, size, file);
    fclose(file);
    return slice;
}

bool getNextToken(Lexer *lex) {

    // Parse code
    char *buff = lex->file->data;
    if(lex->cur >= lex->file->length - 1) {
        // printf("EOF\n");
        return false;
    } 

    for(size_t i = lex->cur; i < lex->file->length; ) 
    {
        uint8_t tokenLen = 0;

        if(isspace(buff[i])) {
            i++;
            continue;
        }
        // Get next token
        char cTkn[256] = {0};
        Token token;
        token.kind = T_LITERAL;
        if(isalnum(buff[i]))
        {
            while(isalnum(buff[i + tokenLen])) {
                cTkn[tokenLen] = buff[i + tokenLen];
                ++tokenLen;
                if(tokenLen > 255) {
                    printf("ERROR: Token must be not be longer than 255 bytes\n");
                    return -1;
                }
            }
            cTkn[tokenLen + 1] = '\0';
            for (size_t j = 0; j < tokenLen; ++j)
            {
                if(isalnum(cTkn[j])) {
                    if(token.kind == T_LITERAL && !isdigit(cTkn[j]))
                    {
                        token.kind = T_NAME;
                        break;
                    }
                    continue;
                }
            }
        }
        else if(!isspace(buff[i]))
        {
            while(!isspace(buff[i + tokenLen]) && !isalnum(buff[i + tokenLen])) {
                cTkn[tokenLen] =buff[i + tokenLen];
                ++tokenLen;
                break;
                if(tokenLen > 255) {
                    printf("ERROR: Token must be not be longer than 255 bytes\n");
                    return -1;
                }
            }
            cTkn[tokenLen + 1] = '\0';

                if(strcmp(cTkn,",") == 0)
                {
                    token.kind = T_COMMA;
                }
                if(strcmp(cTkn,";") == 0)
                {
                    token.kind = T_SEMI;
                }
                if(strcmp(cTkn,"(") == 0)
                {
                    token.kind = T_OPPAR;
                }
                if(strcmp(cTkn,")") == 0)
                {
                    token.kind = T_CLPAR;
                }
                if(strcmp(cTkn,"{") == 0)
                {
                    token.kind = T_OPCURL;
                }
                if(strcmp(cTkn,"}") == 0)
                {
                    token.kind = T_CLCURL;
                }

                if(strcmp(cTkn,"+") == 0)
                {
                    token.binop.kind = BI_PLUS;
                    token.kind = T_BINOP;
                }
                if(strcmp(cTkn,"-") == 0)
                {
                    token.binop.kind = BI_MINUS;
                    token.kind = T_BINOP;
                }
                if(strcmp(cTkn,"*") == 0)
                {
                    token.binop.kind = BI_MULT;
                    token.kind = T_BINOP;
                }
                if(strcmp(cTkn,"/") == 0)
                {
                    token.binop.kind = BI_DIV;
                    token.kind = T_BINOP;
                }
                if(strcmp(cTkn,"=") == 0)
                {
                    token.binop.kind = BI_EQ;
                    token.kind = T_BINOP;
                }
        }

        if(tokenLen > 0) {
            // printf("%d: %s\n", token.kind, cTkn);
            switch (token.kind)
            {
                case T_NAME:
                    memcpy((void *) &token.value.name, cTkn, tokenLen + 1);
                break;
                case T_LITERAL:
                    token.value.lit = atoi(cTkn);
                break;
            }
            lex->tokens.data[lex->tokens.length] = token;
            lex->tokens.length++;
            i += tokenLen;
            lex->cur = i;
            break;
        }
    }
    return true;
}

bool expectToken(Lexer *lex, TokenKind kind) {
    bool valid = lex->tokens.data[lex->tokens.length - 1].kind == kind;

    return valid;
}

bool expectFunctionDecl(Lexer *lex, Function *f)
{
    
    getNextToken(lex);
    if(expectToken(lex, T_NAME))
    {
        strcpy(f->name, lex->tokens.data[lex->tokens.length - 1].value.name);
        getNextToken(lex);
        if(expectToken(lex, T_OPPAR)) {
            while(!expectToken(lex, T_CLPAR)) {
                if(expectToken(lex, T_OPCURL) || expectToken(lex, T_COMMA))
                {
                    char *name = lex->tokens.data[lex->tokens.length - 1].value.name;
                    int index = -1;
                    for (size_t i = 0; i < f->stackVars.length; ++i)
                    {
                        if(strcmp(name, f->stackVars.data[i]) == 0)
                        {
                            index = i;
                            break;
                        }
                    }
                    if(index < 0) {
                        index = f->stackVars.length++;
                        strcpy(f->stackVars.data[index], name);
                    }
                    getNextToken(lex);
                    if(!expectToken(lex, T_NAME)) {
                        return false;
                    }

                }
                getNextToken(lex); 
            }
            if(!expectToken(lex, T_CLPAR))  {
                return false;
            }
        }
    }
    return true;
}

bool parseExpression(Lexer *lex, Function *f)
{   
    getNextToken(lex);
    if(expectToken(lex, T_NAME) || expectToken(lex, T_LITERAL)) {
        
        char *name = lex->tokens.data[lex->tokens.length - 1].value.name;
        uint64_t val = lex->tokens.data[lex->tokens.length - 1].value.lit;
        
        Expression expr = {0};
        expr.kind = EX_ARG;
        expr.arg = expectToken(lex, T_NAME) ? ARG_VAR : ARG_LITERAL;
        
        getNextToken(lex);
        if(expectToken(lex, T_BINOP)) {
            if(expr.arg == ARG_LITERAL) {
                expr.value = val;
            } else {
                int index = -1;
                for (size_t i = 0; i < f->stackVars.length; ++i)
                {
                    if(strcmp(name, f->stackVars.data[i]) == 0)
                    {
                        index = i;
                        break;
                    }
                }
                if(index < 0) {
                    index = f->stackVars.length++;
                    strcpy(f->stackVars.data[index], name);
                }
                expr.value = index;            
            }
            Expression bi = {0};
            bi.kind = EX_BINOP;
            bi.binop.kind = lex->tokens.data[lex->tokens.length - 1].binop.kind;
            bi.binop.lhs = f->expressions.length + 1;
            bi.binop.rhs = f->expressions.length + 2;
            f->expressions.data[f->expressions.length++] = bi;
            f->expressions.data[f->expressions.length++] = expr;
            
            parseExpression(lex, f);
            return true;
        }
        
        if(expectToken(lex, T_OPPAR)) {
            expr.kind = EX_CALL;
            expr.value = 64;
            f->expressions.data[f->expressions.length++] = expr;
            Expression *added = &f->expressions.data[f->expressions.length - 1];
            while(!expectToken(lex, T_CLPAR)) {
                if(!expectToken(lex, T_COMMA) && !(expectToken(lex, T_OPPAR))) {
                    return false;
                }
                if(parseExpression(lex, f)) {
                    added->args[added->argc++] = f->expressions.length - 1;
                }
            }
            getNextToken(lex);
            return true;
        }
        if(expr.arg == ARG_LITERAL) {
            expr.value = val;
        } else {
            int index = -1;
            for (size_t i = 0; i < f->stackVars.length; ++i)
            {
                if(strcmp(name, f->stackVars.data[i]) == 0)
                {
                    index = i;
                    break;
                }
            }
            if(index < 0) {
                index = f->stackVars.length++;
                strcpy(f->stackVars.data[index], name);
            }
            expr.value = index;            
        }
        f->expressions.data[f->expressions.length++] = expr;
    }
    else {
        return false;
    }
    return true;
}

bool expectFuctnionImpl(Lexer *lex, Function *f) 
{
    getNextToken(lex);
    if(!expectToken(lex, T_OPCURL)) {
        printf("ERROR: Expected function implementation\n");
        return false;
    }

    while(!expectToken(lex, T_CLCURL)) {
        if(parseExpression(lex, f)) {
            if(!expectToken(lex, T_SEMI)) {
                printf("ERROR: semicolon expected\n");
                return false;
            }
        }        
    }
    return true;
}

void printExpression(Expression expressions[], size_t index) {
    if (index >= MAX_EXPRESSIONS) {
        printf("Invalid index: %zu\n", index);
        return;
    }

    Expression expr = expressions[index];
    printf("    Expression %zu:\n", index);
    
    switch (expr.kind) {
        case EX_CALL:
            printf("    Kind: EX_CALL\n");
            for (size_t i = 0; i < expr.argc; i++) {
                printf("        Arg: %zu\n", expr.args[i]);
            }
            break;
        case EX_BINOP:
            printf("    Kind: EX_BINOP\n");
            printf("        LHS: %zu, RHS: %zu, Operator: ", expr.binop.lhs, expr.binop.rhs);
            switch (expr.binop.kind) {
                case BI_PLUS: printf("+\n"); break;
                case BI_MINUS: printf("-\n"); break;
                case BI_MULT: printf("*\n"); break;
                case BI_DIV: printf("/\n"); break;
                case BI_EQ: printf("=\n"); break;
            }
            break;
        case EX_ARG:
            printf("    Kind: EX_ARG\n");
            printf("    Argument Type: ");
            switch (expr.arg) {
                case ARG_LITERAL: printf("Literal, Value: %zu\n", expr.value); break;
                case ARG_VAR: printf("Variable, Index: %zu\n", expr.value); break;
            }
            break;
        default:
            printf("    Unknown Expression Kind\n");
            break;
    }
}

bool parseCode(Lexer *lex)
{
    // Function Declaration
    Function f = {0};
    if(!expectFunctionDecl(lex, &f)) 
    {
        printf("ERROR: Expected function declaration\n");
    }
    if(!expectFuctnionImpl(lex, &f))
    {
        
    }
    p.functions.data[p.functions.length++] = f;

    for(int i = 0; i < p.functions.length; ++i)
    {
        printf("Function: %s (%zu)\n", p.functions.data[i].name, p.functions.data[i].stackVars.length);
        for(int j = 0; j < p.functions.data[i].stackVars.length; ++j)
            printf("Var: %s\n",  p.functions.data[i].stackVars.data[j]);
        printf("{\n");
        for(int j = 0; j < p.functions.data[i].expressions.length; ++j)
            printExpression(p.functions.data[i].expressions.data, j); 
        printf("}\n");
    }
}

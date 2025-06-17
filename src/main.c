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
    BI_SET,
    BI_LESS,
} BinopKind;

typedef enum
{
    EX_CALL,
    EX_BINOP,
    EX_ARG,
    EX_ALLOCATE,
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
    struct {
        size_t argc;
        int64_t args[MAX_ARGS_COUNT];
        char name[MAX_NAME_LENGTH];
    } call;

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

// TODO: put result onto the stack
int64_t evalExpression(Function *function, Expression *expr, int64_t *stp)
{
    if(expr->kind == EX_BINOP)
    {
        Expression *lhs = &function->expressions.data[expr->binop.lhs];
        Expression *rhs = &function->expressions.data[expr->binop.rhs];
        if(expr->binop.kind == BI_PLUS) 
        {
            return evalExpression(function, lhs, stp) + evalExpression(function, rhs, stp);
        } 
        if(expr->binop.kind == BI_MINUS) 
        {
            return evalExpression(function, lhs, stp) - evalExpression(function, rhs, stp);
        } 
        if(expr->binop.kind == BI_MULT) 
        {
            return evalExpression(function, lhs, stp) * evalExpression(function, rhs, stp);
        } 
        if(expr->binop.kind == BI_DIV) 
        {
            return evalExpression(function, lhs, stp) / evalExpression(function, rhs, stp);
        } 
        if(expr->binop.kind == BI_LESS)
        {
            return evalExpression(function, lhs, stp) < evalExpression(function, rhs, stp);
        }
    }
    if(expr->kind == EX_ARG)
    {
        if(expr->arg == ARG_VAR) {
            return *(stp - expr->value);
        }
        if(expr->arg == ARG_LITERAL) {
            return expr->value;
        }
    }
    printf("ERROR: Not supported expression %d\n", expr->kind);
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
    
    Function *function = &p.functions.data[index];
    size_t allocatedSize =function->stackVars.length;
    int64_t stack[STACK_SIZE];

    int64_t *stp = ((&stack[STACK_SIZE - 1]));
    int64_t *sbp = ((&stack[STACK_SIZE - 1]) - allocatedSize);
    printf("\nINFO: Running function \"%s\"\n", name);
    printf("INFO: Allocated %zu words\n", allocatedSize);
    printf("INFO: sbp is 0x%p\n\n", sbp);

    for(int i = 0; i < function->expressions.length; ++i)
    {
        Expression *expr = &function->expressions.data[i];
        if(expr->kind == EX_BINOP)
        {
            if (expr->binop.kind == BI_SET)
            {
                Expression *lhs = &function->expressions.data[expr->binop.lhs];
                if(lhs->kind != EX_ARG && lhs->arg != ARG_VAR) {
                    printf("ERROR: lhs must be variable");
                    return false;
                }
                int64_t *ptr = stp - function->expressions.data[expr->binop.lhs].value;
                Expression *rhs = &function->expressions.data[expr->binop.rhs];
                *ptr = evalExpression(function, rhs, stp);
            }   
            
        }

        if(expr->kind == EX_CALL) {
            if(strcmp(expr->call.name, "print") == 0)
            {
                for(int j = 0; j < expr->call.argc; ++j)
                    printf("%ld ",  evalExpression(function, &function->expressions.data[expr->call.args[j]], stp));
                printf("\n");
            }
            else 
            {
                printf("ERROR: unknown function \"%s\"\n", expr->call.name);
                return false;
            }
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

Token *getNextToken(Lexer *lex) {

    // Parse code
    char *buff = lex->file->data;
    if(lex->cur >= lex->file->length - 1) {
        // printf("EOF\n");
        return NULL;
    } 

    for(size_t i = lex->cur; i < lex->file->length; ) 
    {
        uint8_t tokenLen = 0;

        if(isspace(buff[i])) {
            i++;
            continue;
        }
        // Get next token
        Token token;
        char cTkn[256] = {0};
        token.kind = T_LITERAL;
        if(isalnum(buff[i]))
        {
            while(isalnum(buff[i + tokenLen])) {
                cTkn[tokenLen] = buff[i + tokenLen];
                ++tokenLen;
                if(tokenLen > 255) {
                    printf("ERROR: Token must be not be longer than 255 bytes\n");
                    return NULL;
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
                    return NULL;
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
                    token.binop.kind = BI_SET;
                    token.kind = T_BINOP;
                }

                if(strcmp(cTkn,"<") == 0)
                {
                    token.binop.kind = BI_LESS;
                    token.kind = T_BINOP;
                }
        }

        if(tokenLen > 0) {
            printf("%d: %s\n", token.kind, cTkn);
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
            i += tokenLen;
            lex->cur = i;
            return &lex->tokens.data[lex->tokens.length++];
        }
    }
}

bool expectToken(Lexer *lex, TokenKind kind) {
    bool valid = lex->tokens.data[lex->tokens.length - 1].kind == kind;

    return valid;
}

int addVariable(Function *f, const char* name)
{
    int index = f->stackVars.length++;
    strcpy(f->stackVars.data[index], name);
    f->stackVars.data[index];

    return index;
}

bool findVariableName(Function *f, const char *name, int *index)
{
    *index = -1;
    for (int i = 0; i < f->stackVars.length; ++i)
    {
        if(strcmp(name, f->stackVars.data[i]) == 0)
        {
            *index = i;
            return true;
        }
    }
    return false;  
}

bool expectFunctionDecl(Lexer *lex, Function *f)
{
    getNextToken(lex);
    if(expectToken(lex, T_NAME))
    {
        strcpy(f->name, lex->tokens.data[lex->tokens.length - 1].value.name);
        Token *lastToken = getNextToken(lex);
        if(expectToken(lex, T_OPPAR)) {
            while(!expectToken(lex, T_CLPAR)) {
                if(expectToken(lex, T_OPPAR) || expectToken(lex, T_COMMA))
                {
                    lastToken = getNextToken(lex);
                    if(expectToken(lex, T_NAME))
                    {
                        char *name = lastToken->value.name;
                        int index;
                        if(!findVariableName(f, name, &index)) {
                            addVariable(f, name);
                        } else {
                            printf("ERROR: Argument %s is already defined\n", name);
                            return false;
                        }
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

size_t parseNextExpression(Lexer *lex, Function *f)
{   
    Token *lastToken = getNextToken(lex);
    Expression expr = {0};

    // if(expectToken(lex, T_OPPAR))
    // {
    //     parseNextExpression(lex, f);
    // }

    int index;
    if(expectToken(lex, T_NAME)) 
    {
        char *name = lastToken->value.name;
        if(strcmp(name, "var") == 0)
        {   
            Token *var = getNextToken(lex);
            if(!expectToken(lex, T_NAME)) 
            {
                printf("ERROR: expected variable name\n");
                return 0;
            }
            if(!findVariableName(f, var->value.name, &index)) 
            {
                index = addVariable(f, var->value.name);
                expr.kind = EX_ALLOCATE;
                expr.arg = ARG_VAR;
                expr.value = index;
                f->expressions.data[f->expressions.length++] = expr;
                return parseNextExpression(lex, f);
            }
            else 
            {
                printf("ERROR: Variable %s is already defined\n", var->value.name);
                return 0;
            }
        }
        else if(findVariableName(f, lastToken->value.name, &index))
        {
            expr.kind = EX_ARG;
            expr.arg = ARG_VAR;
            expr.value = index;
            f->expressions.data[f->expressions.length++] = expr;
            return parseNextExpression(lex, f);
        }
        Token *n = getNextToken(lex);
        if(expectToken(lex, T_OPPAR))
        {
            strcpy(expr.call.name, lastToken->value.name);
            expr.kind = EX_CALL;
            while(!expectToken(lex, T_CLPAR)) {
                if(!expectToken(lex, T_COMMA) && !(expectToken(lex, T_OPPAR))) {
                    break;;
                }
                size_t next = parseNextExpression(lex, f);
                if(next > 0) {
                    expr.call.args[expr.call.argc++] = next;
                    getNextToken(lex);
                }
            }
            f->expressions.data[f->expressions.length++] = expr;
            getNextToken(lex);
            return f->expressions.length - 1;
        }
        printf("ERROR: Invalid syntax\n");
        return 0;
    }

    if(expectToken(lex, T_BINOP))
    {
        Expression *lhs = &f->expressions.data[f->expressions.length - 1];
        if(lastToken->binop.kind == BI_SET)
        {
            if(lhs->kind != EX_ALLOCATE && lhs->kind != EX_ARG) 
            {
                printf("ERROR: LHS expression expected\n");
                return 0;
            }
        }
        expr.kind = EX_BINOP;
        expr.binop.kind = lastToken->binop.kind;
        expr.binop.lhs = f->expressions.length - 1;
        expr.binop.rhs = parseNextExpression(lex, f);
        f->expressions.data[f->expressions.length++] = expr;
        return f->expressions.length - 1;
    }

    if(expectToken(lex, T_LITERAL))
    {
        int64_t value = lastToken->value.lit;
        expr.kind = EX_ARG;
        expr.arg = ARG_LITERAL;
        expr.value = value;
        f->expressions.data[f->expressions.length++] = expr;
        getNextToken(lex);
        return f->expressions.length - 1;
    }

    return f->expressions.length - 1;
}

bool expectFuctnionImpl(Lexer *lex, Function *f) 
{
    getNextToken(lex);
    if(!expectToken(lex, T_OPCURL)) {
        printf("ERROR: Expected function implementation\n");
        return false;
    }

    while(!expectToken(lex, T_CLCURL)) {
        const int r = parseNextExpression(lex, f);
        if(r > 0) {
            if(!expectToken(lex, T_SEMI)) {
                printf("ERROR: semicolon expected\n");
                return false;
            }
        } else return false;        
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
            printf("    Kind: EX_CALL %s\n", expr.call.name);
            for (size_t i = 0; i < expr.call.argc; i++) {
                printf("        Arg: %zu\n", expr.call.args[i]);
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
                case BI_SET: printf("=\n"); break;
                case BI_LESS: printf("<\n"); break;
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
        case EX_ALLOCATE:
            printf("    Kind: EX_ALLOCATE\n");
            printf("    Stack index: %d\n", expr.value);
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

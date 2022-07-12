    /* cs152-miniL phase2 */
%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char *msg);
extern int currLine;
extern int currPos;
FILE * yyin;
%}

%union{
  /* put your types here */
  int num_val;
  char* id_val;
}

%error-verbose
%start prog_start
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ENDIF 
ELSE FOR WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN SUB ADD MULT DIV MOD EQ NEQ LT GT LTE GTE
SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN END
%token <id_val> IDENT
%token <num_val> NUMBER
%right ASSIGN
%left OR
%left AND
%right NOT
%left EQ NEQ LT GT LTE GTE
%left SUB ADD
%left MULT DIV MOD
%right UMINUS
%left L_SQUARE_BRACKET R_SQUARE_BRACKET
%left L_PAREN R_PAREN

/* %start program */

%% 

/* write your rules here */
prog_start:   functions                       {printf("prog_start -> functions\n");}
              ;

functions:    /*empty*/                       {printf("functions -> epsilon\n");}
              | function functions            {printf("functions -> function functions\n");}
              ;

function:     FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
              {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
              ;

ident:        IDENT                           {printf("ident -> IDENT %f\n", $1);}
              ;

declarations: /*empty*/                       {printf("declarations -> epsilon\n");}
              | declaration SEMICOLON declarations  {printf("declarations -> declaration SEMICOLON declarations\n");}
              ;

statements:   /*empty*/                       {printf("statements -> empty\n");}
              |statement SEMICOLON statements {printf("statements -> statement SEMICOLON statements\n");}
              ;

declaration:  identifiers COLON INTEGER       {printf("declaration -> identifiers COLON INTEGER\n");}
              | identifiers COLON ENUM L_PAREN identifiers R_PAREN  {printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n");}
              | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER   {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
              ;              

identifiers:  ident                           {printf("identifiers -> ident\n");}
              | ident COMMA identifiers       {printf("identifiers -> ident COMMA identifiers\n");}
              ;

statement:    var ASSIGN expression           {printf("statement -> var ASSIGN expression\n");}
              | IF bool_exp THEN statements ENDIF   {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
              | IF bool_exp THEN statements ELSE statements ENDIF   {printf("statement -> IF bool_exp THEN statements ELSE statements ENDIF\n");}
              | WHILE bool_exp BEGINLOOP statements ENDLOOP   {printf("statement -> WHILE bool_exp BEGINLOOP statements ENDLOOP\n");}
              | DO BEGINLOOP statements ENDLOOP WHILE bool_exp    {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp\n");}
              | READ vars                     {printf("statement -> READ vars\n");}
              | WRITE vars                    {printf("statement -> WRITE vars\n");}
              | CONTINUE                      {printf("statement -> CONTINUE\n");}
              | RETURN expression             {printf("statement -> RETURN expression\n");}
              ;

vars:         var                             {printf("vars -> var\n");}
              |var COMMA vars                 {printf("vars -> var COMMA vars\n");}
              ;

bool_exp:     relation_and_exp                {printf("bool_exp -> relation_and_exp\n");}
              |relation_and_exp OR bool_exp   {printf("bool_exp -> relation_and_exp OR bool_exp\n");}
              ;

relation_and_exp: relation_exp                {printf("relation_and_exp -> relation_exp\n");}
                  | relation_exp AND relation_and_exp   {printf("relation_and_exp -> relation_exp AND relation_and_exp\n");}
                  ;

relation_exp: NOT relation_exp                {printf("relation_exp -> NOT relation_exp\n");}
              | expression comp expression    {printf("relation_exp -> expression comp expression\n");}
              | TRUE                          {printf("relation_exp -> TRUE\n");}
              | FALSE                         {printf("relation_exp -> FALSE\n");}
              | L_PAREN bool_exp R_PAREN      {printf("relation_exp -> L_PAREN bool_exp R_PAREN\n");}
              ;

comp:         EQ                              {printf("comp -> EQ\n");}
              | NEQ                           {printf("comp -> NEQ\n");}
              | LT                            {printf("comp -> LT\n");}
              | GT                            {printf("comp -> GT\n");}
              | LTE                           {printf("comp -> LTE\n");}
              | GTE                           {printf("comp -> GTE\n");}
              ;

expression:   multiplicative_expression       {printf("expression -> multiplicative_expression\n");}
              | multiplicative_expression ADD expression  {printf("expression -> multiplicative_expression ADD expression\n");}
              | multiplicative_expression SUB expression  {printf("expression -> multiplicative_expression SUB expression\n");}
              ;

multiplicative_expression:  term              {printf("multiplicative_expression -> term\n");}
                            | term MULT multiplicative_expression {printf("multiplicative_expression -> term MULT multiplicative_expression\n");}
                            | term DIV multiplicative_expression  {printf("multiplicative_expression -> term DIV multiplicative_expression\n");}
                            | term MOD multiplicative_expression  {printf("multiplicative_expression -> term MOD multiplicative_expression");}
                            ;

term:         MINUS term %prec UMINUS         {printf("term -> UMINUS term\n");}
              | var                           {printf("term -> var\n");}
              | NUMBER                        {printf("term -> NUMBER\n");}
              | L_PAREN expression R_PAREN    {printf("term -> L_PAREN expression R_PAREN\n");}  
              | ident L_PAREN expressions R_PAREN   {printf("term -> ident L_PAREN expressions R_PAREN\n");}
              ;

expressions:  expression COMMA expressions    {printf("expressions -> expression COMMA expressions\n");}
              | /*empty*/                     {printf("expressions -> epsilon\n");}
              ;

var:          ident                           {printf("var -> ident\n");}
              | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET  {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
              ;


%% 

int main(int argc, char **argv) {
  if (argc > 1) {
    yyin = fopen(argv[1], "r");
    if (yyin == NULL){
      printf("This is not a valid file name: %s filename\n", argv[0]);
      exit(0);
    }
  }
  yyparse();
  return 0;
}

void yyerror(const char *msg) {
/* implement your error handling */
  printf("Error at line %d, position %d: %s\n", currLine, currPos, msg);
  exit(0);
}
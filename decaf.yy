/* $Id$ -*- mode: c++ -*- */
/** \file decaf.yy Contains the example Bison parser source */

%{ /*** C/C++ Declarations ***/

#include <iostream>
#include <string>
#include <vector>
#include "ast.h"
#include "scanner.h"
#include "driver.h"
#include "types.h"

using namespace std;

%}

/*** yacc/bison Declarations ***/

/* Require bison 2.3 or later */
%require "2.3"

/* add debug output code to generated parser. disable this for release
 * versions. */
%debug

/* start symbol is named "start" */
%start program

/* write out a header file containing the token defines */
%defines

/* use newer C++ skeleton file */
%skeleton "lalr1.cc"

/* namespace to enclose parser in */
%name-prefix="decaf"

/* set the parser's class identifier */
%define "parser_class_name" "Parser"

/* keep track of the current position within the input */
%locations
%initial-action
{
    // initialize the initial location object
    @$.begin.filename = @$.end.filename = &driver.streamname;
};

/* The driver is passed by reference to the parser and to the scanner. This
 * provides a simple but effective pure interface, not relying on global
 * variables. */
%parse-param { class Driver& driver }

/* verbose error messages */
%error-verbose
%code requires {
    #include "types.h"
}

%union
{
    Type some_type;

    AssignOp some_assign_op;
    BinaryOp some_binary_op;
    UnaryOp some_unary_op;

    int intLit;
    std::string* strLit;
    bool booleanLit;

    std::vector<std::string*>* strlist;

    class ASTNode* astnode;

    class Field* field;
    std::vector<Field*>* fieldlist;

    class Method* method;
    std::vector<Method*>* methodlist;

    class Identifier* identity;
    std::vector<Identifier*>* identitylist;

    class Argument* arg;
    std::vector<Argument*>* arglist;

    class Block* block;

    class Variable* var;
    std::vector<Variable*>* varlist;

    class Statement* statement;
    std::vector<Statement*>* statementlist;

    class Location* location_type;

    class MethodCall* method_call_type;

    class Expression* expression_type;
    std::vector<Expression*>* expression_list;

    class Literal* literal_type;
}

//%destructor { delete $$; } expr

%{

/* this "connects" the bison parser in the driver to the flex scanner class
 * object. it defines the yylex() function call to pull the next token from the
 * current lexer object of the driver context. */
#undef yylex
#define yylex driver.lexer->lex

%}

%token END 0
/* reserved words*/
%token BREAK CALLOUT CLASS CONTINUE ELSE FOR IF RET
%token <some_type> VOID
/* special word */
%token PROG
/* literal types supported by decaf */
%token <some_type>  TYPE
%token <intLit>     INT
%token <booleanLit> BOOL
%token <strLit>     CHAR STR

%nonassoc   <some_assign_op> ASS_EQ ASS_DEC ASS_INC

/* conditional */
%left   <some_binary_op> OR
%left   <some_binary_op> AND

/* operators with precedence and associativity */
%nonassoc   <some_binary_op> ISEQ NEQ
%left       <some_binary_op> GT LT GE LE
%left       <some_binary_op> ADD SUB
%left       <some_binary_op> MUL DIV MOD
/* unary not */
%nonassoc EXCL

/* open and close brackets */
%token OPAREN CPAREN
%token OBOX CBOX
%token OCURL CCURL

/* identifiers */
%token <strLit> IDENT
/* some chars */
%token EOL COMMA SEMICOL

%type <astnode>         program
%type <field>           fieldDec
%type <method>          methodDec
%type <fieldlist>       fieldDecList
%type <methodlist>      methodDecList
%type <identity>        identItem
%type <identitylist>    identList
%type <arglist>         methodDecArgs
%type <block>           block
%type <var>             varDec
%type <varlist>         varDecList
%type <strlist>         blockIdentList
%type <statement>       stmt
%type <statementlist>   stmtList
%type <location_type>   location
%type <expression_type> expr calloutArg
%type <expression_list> squareList exprList calloutArgs methodArgs
%type <literal_type>    literal
%type <some_assign_op>  assignOp
%type <method_call_type> methodCall

%%

program: CLASS PROG OCURL fieldDecList methodDecList CCURL {
    $$ = new ProgramNode ($4, $5);
    driver.ast.pRoot = $$;
};

fieldDecList: fieldDecList fieldDec { $$ = $1; $$->push_back($2); }
  | /* epsilon */                   { $$ = new vector<Field*> (); }
  ;
fieldDec: TYPE identList SEMICOL    { $$ = new Field($1, $2); };
identList: identItem                { $$ = new vector<Identifier*> ({$1}); }
  | identList COMMA identItem       { $$ = $1; $$->push_back($3); }
  ;
identItem: IDENT        { $$ = new Identifier($1); }
  | IDENT OBOX INT CBOX { $$ = new Identifier($1, $3); }
  ;

methodDecList: methodDec methodDecList  { $$ = $2; $$->insert($$->begin(), $1);}
  | /* epsilon */                       { $$ = new vector<Method*> (); }
  ;
methodDec: VOID IDENT OPAREN CPAREN block   {
    $$ = new Method($1, $2, $5);
}
  | VOID IDENT OPAREN methodDecArgs CPAREN block    {
    $$ = new Method($1, $2, $4, $6);
}
  | TYPE IDENT OPAREN CPAREN block  {
    $$ = new Method($1, $2, $5);
}
  | TYPE IDENT OPAREN methodDecArgs CPAREN block    {
    $$ = new Method($1, $2, $4, $6);
}
  ;
methodDecArgs: TYPE IDENT   {
    $$ = new vector<Argument*>({new Argument($1, $2)});
}
  | TYPE IDENT COMMA methodDecArgs  {
    $$ = $4; $$->insert($$->begin(), new Argument($1, $2));
}
  ;

block: OCURL varDecList stmtList CCURL  { $$ = new Block($2, $3); };

/* WARNING: this identList must be different from the one above
 * because this one cannot be of the form id[int]
 */
varDecList: /* epsilon */   { $$ = new vector<Variable*>(); }
  | varDec varDecList       { $$ = $2; $$->push_back($1); }
  ;
varDec: TYPE blockIdentList SEMICOL { $$ = new Variable($1, $2); };
blockIdentList: IDENT           { $$ = new vector<string*>({$1}); }
  | blockIdentList COMMA IDENT  { $$ = $1; $$->push_back($3); }
  ;

stmtList: /* epsilon */ { $$ = new vector<Statement*>(); }
  | stmt stmtList       { $$ = $2; $$->insert($$->begin(), $1); }
  ;
stmt: location assignOp expr SEMICOL        {
    $$ = new AssignStatement($1, $2, $3);
}
  | methodCall SEMICOL                      {
    $$ = new MethodCallStatement($1);
}
  | IF OPAREN expr CPAREN block             {
    $$ = new IfStatement($3, $5);
}
  | IF OPAREN expr CPAREN block ELSE block  {
    $$ = new IfStatement($3, $5, $7);
}
  | FOR IDENT ASS_EQ expr COMMA expr block  {
    $$ = new ForStatement($2, $4, $6, $7);
}
  | RET SEMICOL                             {
    $$ = new ReturnStatement();
}
  | RET expr SEMICOL                        {
    $$ = new ReturnStatement($2);
}
  | BREAK SEMICOL                           { $$ = new BreakStatement(); }
  | CONTINUE SEMICOL                        { $$ = new ContinueStatement(); }
  | block                                   { $$ = new BlockStatement($1); }
  ;
assignOp: ASS_EQ | ASS_DEC | ASS_INC;

expr: location          { $$ = new LocationExpr($1); }
  | methodCall          { $$ = new MethodCallExpr($1); }
  | literal             { $$ = new LiteralExpr($1); }
/* For precedence rules to work, we need the actual terminals to be present in
 * the ambiguous production. Grouping terminals into non-terminals won't work.
 * https://stackoverflow.com/a/13567518/6479208 .
 */
/* Arithmetic Operations: */
  | expr ADD expr       { $$ = new BinaryExpr($1, $2, $3); }
  | expr SUB expr       { $$ = new BinaryExpr($1, $2, $3); }
  | expr MUL expr       { $$ = new BinaryExpr($1, $2, $3); }
  | expr DIV expr       { $$ = new BinaryExpr($1, $2, $3); }
  | expr MOD expr       { $$ = new BinaryExpr($1, $2, $3); }
/* Relational Operations: */
  | expr LT expr        { $$ = new BinaryExpr($1, $2, $3); }
  | expr GT expr        { $$ = new BinaryExpr($1, $2, $3); }
  | expr LE expr        { $$ = new BinaryExpr($1, $2, $3); }
  | expr GE expr        { $$ = new BinaryExpr($1, $2, $3); }
/* Equality Checks: */
  | expr ISEQ expr      { $$ = new BinaryExpr($1, $2, $3); }
  | expr NEQ expr       { $$ = new BinaryExpr($1, $2, $3); }
/* Conditional Operators: */
  | expr AND expr       { $$ = new BinaryExpr($1, $2, $3); }
  | expr OR expr        { $$ = new BinaryExpr($1, $2, $3); }
/* We introduce the unary operator on our own since SUB is already defined as
 * a binary operator.
 */
  | SUB expr            { $$ = new UnaryExpr(UnaryOp::SUB, $2); }
  | EXCL expr           { $$ = new UnaryExpr(UnaryOp::EXCL, $2); }
  | OPAREN expr CPAREN  { $$ = $2; }
  ;

literal: INT    { $$ = new IntegerLiteral($1); }
  | CHAR        { $$ = new StringLiteral($1); }
  | BOOL        { $$ = new BooleanLiteral($1); }
  ;

location: IDENT squareList  { $$ = new Location($1, $2); };
squareList: /* epsilon */       { $$ = new vector<Expression*>(); }
  | OBOX expr CBOX squareList   {
    $$ = $4; $$->insert($$->begin(), $2);
}
  ;

methodCall: IDENT OPAREN methodArgs CPAREN  { $$ = new MethodCall($1, $3); }
  | CALLOUT OPAREN STR calloutArgs CPAREN   { $$ = new Callout($3, $4); }
  ;

methodArgs: /* epsilon */   { $$ = new vector<Expression*>(); }
  | exprList                { $$ = $1; }
  ;
exprList: expr              { $$ = new vector<Expression*>({$1}); }
  | expr COMMA exprList     { $$ = $3; $$->insert($$->begin(), $1); }
  ;

calloutArgs: /* epsilon */          { $$ = new vector<Expression*>(); }
  | COMMA calloutArg calloutArgs    { $$ = $3; $$->insert($$->begin(), $2); }
  ;
calloutArg: expr    { $$ = $1; }
  | STR             { $$ = new LiteralExpr(new StringLiteral($1)); }
  ;

%%

void decaf::Parser::error(const Parser::location_type& l,
                          const std::string& m)
{
    driver.error(l, m);
}

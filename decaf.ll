/* $Id$ -*- mode: c++ -*- */
/** \file decaf.ll Define the example Flex lexical scanner */

%{ /*** C/C++ Declarations ***/

#include <string>
#include "scanner.h"
#include "types.h"

/* import the parser's token type into a local typedef */
typedef decaf::Parser::token token;
typedef decaf::Parser::token_type token_type;

/* By default yylex returns int, we use token_type. Unfortunately yyterminate
 * by default returns 0, which is not of token_type. */
#define yyterminate() return token::END

/* This disables inclusion of unistd.h, which is not available under Visual C++
 * on Win32. The C++ scanner uses STL streams instead. */
#define YY_NO_UNISTD_H

%}

/*** Flex Declarations and Options ***/

/* enable c++ scanner class generation */
%option c++

/* change the name of the scanner class. results in "decafFlexLexer" */
%option prefix="decaf"

/* the manual says "somewhat more optimized" */
%option batch

/* enable scanner to generate debug output. disable this for release
 * versions. */
%option debug

/* no support for include files is planned */
%option noyywrap nounput

/* enables the use of start condition stacks */
%option stack

/* The following paragraph suffices to track locations accurately. Each time
 * yylex is invoked, the begin position is moved onto the end position. */
%{
    #define YY_USER_ACTION  yylloc->columns(yyleng);
%}

SINGLE [\x20\x21\x23-\x26\x28-\x5b\x5d-\x7e]
ADDONS \\[nt\'\"\\]

/* Random comment with single quote ' for correct code highlighting */

%% /*** Regular Expressions Part ***/

 /* code to place at the beginning of yylex() */
%{
    // reset location
    yylloc->step();
%}

"+" { yylval->some_binary_op = BinaryOp::ADD; return token::ADD; }
"-" { yylval->some_binary_op = BinaryOp::SUB; return token::SUB; }
"*" { yylval->some_binary_op = BinaryOp::MUL; return token::MUL; }
"/" { yylval->some_binary_op = BinaryOp::DIV; return token::DIV; }
"%" { yylval->some_binary_op = BinaryOp::MOD; return token::MOD; }
"(" { return token::OPAREN; }
")" { return token::CPAREN; }
"[" { return token::OBOX; }
"]" { return token::CBOX; }
"{" { return token::OCURL; }
"}" { return token::CCURL; }
"," { return token::COMMA; }
";" { return token::SEMICOL; }

"boolean"   { yylval->some_type = Type::BOOL; return token::TYPE; }
"break"     { return token::BREAK; }
"callout"   { return token::CALLOUT; }
"class"     { return token::CLASS; }
"continue"  { return token::CONTINUE; }
"else"      { return token::ELSE; }
"false"     { yylval->booleanLit = false; return token::BOOL; }
"for"       { return token::FOR; }
"if"        { return token::IF; }
"int"       { yylval->some_type = Type::INT; return token::TYPE; }
"return"    { return token::RET; }
"true"      { yylval->booleanLit = true; return token::BOOL; }
"void"      { yylval->some_type = Type::VOID; return token::VOID; }
"Program"   { return token::PROG; }


[a-zA-Z][a-zA-Z0-9]* { yylval->strLit = new std::string(yytext, yyleng);
                       return token::IDENT; }
-?0x[a-fA-F0-9]+  { yylval->intLit = strtol(yytext, 0, 16); return token::INT; }
-?[0-9]+          { yylval->intLit = strtol(yytext, 0, 10); return token::INT; }

"="   { yylval->some_assign_op = AssignOp::ASS_EQ; return token::ASS_EQ; }
"-="  { yylval->some_assign_op = AssignOp::ASS_DEC; return token::ASS_DEC; }
"+="  { yylval->some_assign_op = AssignOp::ASS_INC; return token::ASS_INC; }

"<"   { yylval->some_binary_op = BinaryOp::LT; return token::LT; }
">"   { yylval->some_binary_op = BinaryOp::GT; return token::GT; }
"<="  { yylval->some_binary_op = BinaryOp::LE; return token::LE; }
">="  { yylval->some_binary_op = BinaryOp::GE; return token::GE; }

"==" { yylval->some_binary_op = BinaryOp::ISEQ; return token::ISEQ; }
"!=" { yylval->some_binary_op = BinaryOp::NEQ; return token::NEQ; }

"&&"  { yylval->some_binary_op = BinaryOp::AND; return token::AND; }
"||"  { yylval->some_binary_op = BinaryOp::OR; return token::OR; }

"!" { return token::EXCL; }

[ \t\r]+  { yylloc->step(); }
\n        { yylloc->lines(yyleng); yylloc->step(); }

\'({SINGLE}|{ADDONS})\'   {
    /* Random comment with single quote ' for correct code highlighting */
    yylval->strLit = new std::string(yytext+1, yyleng-2);
    return token::CHAR;
}
\"({SINGLE}|{ADDONS})*\"  {
    /* Random comment with double quote " for correct code highlighting */
    yylval->strLit = new std::string(yytext+1, yyleng-2);
    return token::STR;
}

\/\/.*\n {}

<<EOF>> { yyterminate(); }
.       {
          std::cout << "Unmatched text " << yytext << '\n';
          return static_cast<token_type>(*yytext);
        }

%% /*** Additional Code ***/

namespace decaf {
Scanner::Scanner(std::istream* in, std::ostream* out)
        : decafFlexLexer(in, out) {}

Scanner::~Scanner() {}

void Scanner::set_debug(bool b){
  yy_flex_debug = b;
}

} /* end namespace decaf */

/* This implementation of DecafFlexLexer::yylex() is required to fill the
 * vtable of the class DecafFlexLexer. We define the scanner's main yylex
 * function via YY_DECL to reside in the Scanner class instead. */

#ifdef yylex
#undef yylex
#endif

int decafFlexLexer::yylex()
{
    std::cerr << "in decafFlexLexer::yylex() !" << std::endl;
    return 0;
}

/* When the scanner receives an end-of-file indication from YY_INPUT, it then
 * checks the yywrap() function. If yywrap() returns false (zero), then it is
 * assumed that the function has gone ahead and set up `yyin' to point to
 * another input file, and scanning continues. If it returns true (non-zero),
 * then the scanner terminates, returning 0 to its caller.
 * We do not need this as it is defined in the generated scanner.cc
 */
/*
int decafFlexLexer::yywrap()
{
    return 1;
}
*/

%option noyywrap
%{
#include <stdio.h>
#include "header.h"

int linenumber;
symtab * lookup();
symtab * ptr;
void     insertID();
void    printSymTab();

#define INT       10
#define FLOAT     11
#define STRING    12
#define COMMENT   13
#define OP_ADD    14
#define OP_SUB    15
#define OP_DIV    16
#define OP_MUL    17
#define OP_LT     18
#define OP_GT     19
#define OP_GE     20
#define OP_LE     21
#define OP_NE     22
#define OP_EQ     23
#define OP_OR     24
#define OP_AND    25
#define OP_NOT    26
#define OP_ASSIGN 27
#define OP_DOT    28
#define MK_LPAREN 29
#define MK_RPAREN 30
#define MK_LBRACK 31
#define MK_RBRACK 32
#define MK_LBRACE 33
#define MK_RBRACE 34
#define ERROR     100

/* #define DBGRE(t)  printf("[DEBUG] %s: %s\n", t, yytext) */
#define DBGRE(t)

%}

letter          [A-Za-z]
digit           [0-9]
hex             [a-fA-F0-9]
hexprefix       (0x|0X)
exp             [Ee][+-]?{digit}+
bin_exp         [Pp][+-]?{digit}+
fsuffix         (f|F|l|L)
ID              {letter}({letter}|{digit}|"_")*
WS              [ \t]+
Int_constant    {digit}+
Float_dec1      {digit}+{exp}
Float_dec2      {digit}*"."{digit}+({exp})?
Float_dec3      {digit}+"."{digit}*({exp})?
Float_hex1      {hexprefix}{hex}+{bin_exp}
Float_hex2      {hexprefix}{hex}*"."{hex}+({bin_exp})?
Float_hex3      {hexprefix}{hex}+"."{hex}*({bin_exp})?
Float_constant  ({Float_dec1}|{Float_dec2}|{Float_dec3}|{Float_hex1}|{Float_hex2}|{Float_hex3})
String_constant \"[^\"\n]*\"
comment         ("/*"[^*]*[*]+([^*/][^*]*[*]+)*"/"|"//".*)

OP_add          "+"
OP_sub          "-"
OP_div          "/"
OP_mul          "*"
OP_lt           "<"
OP_gt           ">"
OP_ge           ">="
OP_le           "<="
OP_ne           "!="
OP_eq           "=="
OP_or           "||"
OP_and          "&&"
OP_not          "!"
OP_assign       "="
OP_dot          "."

newline         "\n"

DL_lparen       "("
DL_rparen       ")"
DL_lbrack       "["
DL_rbrack       "]"
DL_lbrace       "{"
DL_rbrace       "}"
DL_comma        ","
DL_semicol      ";"
DL_dot          "."

error           .

%%

{WS}              {}
{Int_constant}    DBGRE("INT"); /* return INT; */
{Float_constant}  DBGRE("FLOAT"); /* return FLOAT; */
{String_constant} DBGRE("STRING"); /* return STRING; */
{comment}         DBGRE("COMMENT"); puts(yytext);/* return COMMENT; */
{ID}              {
  DBGRE("ID");
  ptr = lookup(yytext);
  if (ptr == NULL)
    insertID(yytext);
  else
    ptr->counter++;
                  }

{OP_add}          DBGRE("OP_ADD"); /* return OP_ADD; */
{OP_sub}          DBGRE("OP_SUB"); /* return OP_SUB; */
{OP_div}          DBGRE("OP_DIV"); /* return OP_DIV; */
{OP_mul}          DBGRE("OP_MUL"); /* return OP_MUL; */
{OP_lt}           DBGRE("OP_LT"); /* return OP_LT; */
{OP_gt}           DBGRE("OP_GT"); /* return OP_GT; */
{OP_ge}           DBGRE("OP_GE"); /* return OP_GE; */
{OP_le}           DBGRE("OP_LE"); /* return OP_LE; */
{OP_ne}           DBGRE("OP_NE"); /* return OP_NE; */
{OP_eq}           DBGRE("OP_EQ"); /* return OP_EQ; */
{OP_or}           DBGRE("OP_OR"); /* return OP_OR; */
{OP_and}          DBGRE("OP_AND"); /* return OP_AND; */
{OP_not}          DBGRE("OP_NOT"); /* return OP_NOT; */
{OP_assign}       DBGRE("OP_ASSIGN"); /* return OP_ASSIGN; */
{OP_dot}          DBGRE("OP_DOT"); /* return OP_DOT; */
{DL_lparen}       DBGRE("MK_LPAREN"); /* return MK_LPAREN; */
{DL_rparen}       DBGRE("MK_RPAREN"); /* return MK_RPAREN; */
{DL_lbrack}       DBGRE("MK_LBRACK"); /* return MK_LBRACK; */
{DL_rbrack}       DBGRE("MK_RBRACK"); /* return MK_RBRACK; */
{DL_lbrace}       DBGRE("MK_LBRACE"); /* return MK_LBRACE; */
{DL_rbrace}       DBGRE("MK_RBRACE"); /* return MK_RBRACE; */
{DL_comma}        {}
{DL_semicol}      {}
{newline}         linenumber += 1;
{error}           {
  printf("ERR %s\n", yytext);
  /* exit(ERROR); */
  return ERROR;
                  }


%%

main(int argc, char **argv)
{
  argc--; ++argv;
  if (argc > 0)
    yyin = fopen(argv[0], "r");
  else
    yyin = stdin;
  yylex();
  printSymTab();
}


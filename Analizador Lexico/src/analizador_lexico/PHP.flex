
package analizador_lexico;
/*you shoukd import your enum file with all the symbols */ 
import static analizador_lexico.Token.*;

%% 
/* Lexical functions */ 
%class PHPLexer
%type Token
%column
%line

%{
    public String token; 
%}

/*Regular Expresions */
identifier = [a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]* 
decimalS = [1-9][1-9]* | 0
hexa = 0[xX][0-9a-fA-F]+
octal = 0[0-7]+
binary = 0[bB][01]+
integer = [+-]?{decimalS} | [+-]?{hexa} | [+-]?{octal} | [+-]?{binary}
decimal = ({digit}*[\.]{digit}+) | ({digit}+[\.]({digit})*)
number = [0-9]+
decimal = [+-]?([0-9]*[\.]{number}) | ({number}[\.][0-9]*)
exponent = [+-]?(({number} | {decimal}) [eE][+-]?{number})
comments = (((\#)|(\/\/))[^\r\n]*|\/\*[\s\S]*?\*\/)
whiteSpace =  [\n\r\t]|(" ")|(\r\n)|(\n) 
lineBreak = [\n]
stringCharacter = [^\u2028\u2029\u000A\u000B\u000C\u000D\u0085\"\\]
escapeSequence = \\[^\u2028\u2029\u000A\u000B\u000C\u000D\u0085]|\\
string = (\"|\')({stringCharacter} | {escapeSequence})*(\"|\')
/*string = ((\"(([^\\\"])|([\\\"]))*\") | (\'(([^\\\"])|([\\\"]))*\'))*/
compileConstant = (_)(CLASS|DIR|FILE|FUNCTION|LINE|METHOD|NAMESPACE|TRAIT)(_)
 
%%

/* Patterns */

/*Keywords*/
"__halt_compiler" {token = yytext(); return HALT_COMPILER;}
"abstract" { token = yytext(); return ABSTRACT;}
"and" {token = yytext(); return AND;}
"array" {token = yytext(); return ARRAY;}
"as" {token = yytext(); return AS;}
"break" {token = yytext(); return BREAK;}
"callable" {token = yytext(); return CALLABLE;}
"case" {token = yytext(); return CASE;}
"catch" {token = yytext(); return CATCH;}
"class" {token = yytext(); return CLASS;}
"clone" {token = yytext(); return CLONE;}
"const" {token = yytext();return CONST;}
"continue" {token = yytext(); return CONTINUE;}
"declare" {token = yytext(); return DECLARE;}
"default" {token = yytext(); return DEFAULT;}
"die" {token = yytext(); return DIE;}
"do" {token = yytext(); return DO;}
"echo" {token = yytext(); return ECHO;}
"else" {token = yytext(); return ELSE;}
"elseif" | "else if" {token = yytext(); return ELSEIF;}
"empty" {token = yytext(); return EMPTY;}
"enddeclare" {token = yytext(); return ENDDECLARE;}
"endfor" {token = yytext(); return ENDFOR;}
"endforeach" {token = yytext(); return ENDFOREACH;}
"endif" {token = yytext(); return ENDIF;}
"endswitch" {token = yytext(); return ENDSWITCH;}
"endwhile" {token = yytext(); return ENDWHILE;}
"eval" {token = yytext(); return EVAL;}
"exit" {token = yytext(); return EXIT;}
"extends" {token = yytext(); return EXTENDS;}
"final" {token = yytext(); return FINAL;}
"for" {token = yytext(); return FOR;}
"foreach" {token = yytext(); return FOREACH;}
"function" {token = yytext(); return FUNCTION;}
"global" {token = yytext(); return GLOBAL;}
"goto" {token = yytext(); return GOTO;}
"if" {token = yytext(); return IF;}
"implements" {token = yytext(); return IMPLEMENTS;}
"include" {token = yytext(); return INCLUDE;}
"include_once" {token = yytext(); return INCLUDE_ONCE;}
"instanceof" {token = yytext(); return INSTANCEOF;}
"insteadof" {token = yytext(); return INSTEADOF;}
"interface" {token = yytext(); return INTERFACE;}
"isset" {token = yytext(); return ISSET;}
"list" {token = yytext(); return LIST;}
"namespace" {token = yytext(); return NAMESPACE;}
"new" {token = yytext(); return NEW;}
"or" {token = yytext();return OR;}
"print" {token = yytext(); return PRINT;}
"private" {token = yytext(); return PRIVATE;}
"protected" {token = yytext(); return PROTECTED;}
"public" {token = yytext(); return PUBLIC;}
"require" {token = yytext(); return REQUIRE;}
"require_once" {token = yytext(); return REQUIRE_ONCE;}
"return" {token = yytext(); return RETURN;}
"static" {token = yytext(); return STATIC;}
"switch" {token = yytext(); return SWITCH;}
"throw" {token = yytext(); return THROW;}
"trait" {token = yytext(); return TRAIT;}
"try" {token = yytext();return TRY;}
"unset" {token = yytext(); return UNSET;}
"use" {token = yytext(); return USE;}
"var" {token = yytext(); return VAR;}
"while" {token = yytext(); return WHILE;}
"xor" {token = yytext(); return XOR;}
{whiteSpace}  {/*Nothing jaja*/}
{lineBreak}  { token = yytext(); return LINEBREAK;}

/*Special Characters*/
"::" {token = yytext(); return DOUBLE_COLON;}
"=>" {token = yytext(); return ARROW;}
"++" {token = yytext(); return INCR;}
"--" {token = yytext(); return DECR;}
"===" {token = yytext(); return IDENTICAL;}
"==" {token = yytext(); return EQUAL;}
"!==" {token = yytext(); return NOT_IDENTICAL;}
"!=" {token = yytext(); return NOT_EQUAL;}
">=" {token = yytext(); return GREATER_EQUAL;}
"<=" {token = yytext(); return LESS_EQUAL;}
"+=" {token = yytext(); return PLUSS_EQUAL;}
"-=" {token = yytext(); return MINUS_EQUAL;}
"*=" {token = yytext(); return MULT_EQUAL;}
"/=" {token = yytext();return DIV_EQUAL;}
"**=" {token = yytext(); return POW_EQUAL;}
".=" {token = yytext(); return CONCAT_EQUAL;}
"%=" {token = yytext(); return MOD_EQUAL;}
"<<" {token = yytext();return SL;}
">>" {token = yytext(); return SR;}
"<<=" {token = yytext(); return SL_EQUAL;}
">>=" {token = yytext(); return SR_EQUAL;}
"&=" {token = yytext(); return AND_EQUAL;}
"|=" {token = yytext(); return OR_EQUAL;}
"^=" {token = yytext(); return XOR_EQUAL;}
"||" {token = yytext(); return SYM_OR;}
"&&" {token = yytext(); return SYM_AND;}
";" {token = yytext(); return SEMICOLON;}
":" {token = yytext(); return COLON;}
"," {token = yytext(); return COMMA;}
"." {token = yytext(); return DOT;}
"[" {token = yytext(); return RECT_OPEN_BRACE;}
"]" {token = yytext(); return RECT_CLOSE_BRACE;}
"(" {token = yytext(); return OPEN_BRACE;}
")" {token = yytext(); return CLOSE_BRACE;}
"{" {token = yytext(); return OPEN_CURLY_BRACE;}
"}" {token = yytext(); return CLOSE_CURLY_BRACE;}
"|" {token = yytext(); return BIT_OR;}
"^" {token = yytext(); return BIT_XOR;}
"&" {token = yytext(); return BIT_AND;}
"+" {token = yytext(); return PLUS;}
"-" {token = yytext(); return MINUS;}
"/" {token = yytext(); return DIV;}
"*" {token = yytext(); return MULT;}
"**" {token = yytext(); return POW;}
"=" {token = yytext(); return ASSING;}
"%" {token = yytext(); return MOD;}
"!" {token = yytext(); return NOT;}
"~" {token = yytext(); return BIT_NOT;}
"$" {token = yytext(); return DOLLAR;}
"<" {token = yytext(); return LESS;}
">" {token = yytext(); return GREATER;}
"@" {token = yytext(); return AT;}
"<?php" {token = yytext(); return INI;}
"?>" {token = yytext();return CLOSE;}


/*Numbers, Id*/
{number}|{integer} {token = yytext(); return NUMBER;}
{decimal}|{exponent} {token = yytext(); return REAL_NUM;}
{identifier} {token = yytext(); return ID;}
{comments} {token = yytext(); return COMMENT;}
{string}  {token = yytext(); return STRING;}

/* Compile-time constants*/
{compileConstant} = {token = yytext(); return C_CONSTANT;}

. {token = yytext(); return ERROR;}

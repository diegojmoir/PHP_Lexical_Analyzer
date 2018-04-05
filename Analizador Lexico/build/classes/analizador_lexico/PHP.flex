
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
integer = [+-]?decimalS | [+-]?hexa | [+-]?octal | [+-]?binary
decimal = ({digit}*[\.]{digit}+) | ({digit}+[\.]({digit})*)
number = [0-9]+
decimal = [+-]?([0-9]*[\.]{number}) | ({number}[\.][0-9]*)
exponent = [+-]?(({number} | {decimal}) [eE][+-]?{number})
multiLineComment = (\/\/)[^\r\n]*|\/\*[\s\S]*?\*\/
numComment = (#)(.)+
comments = {multiLineComment}{numComment}
whiteSpace =  [ \n\r\t]+
stringCharacter = [^\u2028\u2029\u000A\u000B\u000C\u000D\u0085\"\\]
escapeSequence = \\[^\u2028\u2029\u000A\u000B\u000C\u000D\u0085]|\\
string = \"({stringCharacter} | {escapeSequence})*\"
/*string = ((\"(([^\\\"])|([\\\"]))*\") | (\'(([^\\\"])|([\\\"]))*\'))*/
compileConstant = (_)(CLASS|DIR|FILE|FUNCTION|LINE|METHOD|NAMESPACE|TRAIT)(_)
 
%%

/* Patterns */

/*Keywords*/
"__halt_compiler" {return HALT_COMPILER;}
"abstract" { return ABSTRACT;}
"and" {return AND;}
"array" {return ARRAY;}
"as" {return AS;}
"break" {return BREAK;}
"callable" {return CALLABLE;}
"case" {return CASE;}
"catch" {return CATCH;}
"class" {return CLASS;}
"clone" {return CLONE;}
"const" {return CONST;}
"continue" {return CONTINUE;}
"declare" {return DECLARE;}
"default" {return DEFAULT;}
"die" {return DIE;}
"do" {return DO;}
"echo" {return ECHO;}
"else" {return ELSE;}
"elseif" | "else if" {return ELSEIF;}
"empty" {return EMPTY;}
"enddeclare" {return ENDDECLARE;}
"endfor" {return ENDFOR;}
"endforeach" {return ENDFOREACH;}
"endif" {return ENDIF;}
"endswitch" {return ENDSWITCH;}
"endwhile" {return ENDWHILE;}
"eval" {return EVAL;}
"exit" {return EXIT;}
"extends" {return EXTENDS;}
"final" {return FINAL;}
"for" {return FOR;}
"foreach" {return FOREACH;}
"function" {return FUNCTION;}
"global" {return GLOBAL;}
"goto" {return GOTO;}
"if" {return IF;}
"implements" {return IMPLEMENTS;}
"include" {return INCLUDE;}
"include_once" {return INCLUDE_ONCE;}
"instanceof" {return INSTANCEOF;}
"insteadof" {return INSTEADOF;}
"interface" {return INTERFACE;}
"isset" {return ISSET;}
"list" {return LIST;}
"namespace" {return NAMESPACE;}
"new" {return NEW;}
"or" {return OR;}
"print" {return PRINT;}
"private" {return PRIVATE;}
"protected" {return PROTECTED;}
"public" {return PUBLIC;}
"require" {return REQUIRE;}
"require_once" {return REQUIRE_ONCE;}
"return" {return RETURN;}
"static" {return STATIC;}
"switch" {return SWITCH;}
"throw" {return THROW;}
"trait" {return TRAIT;}
"try" {return TRY;}
"unset" {return UNSET;}
"use" {return USE;}
"var" {return VAR;}
"while" {return WHILE;}
"xor" {return XOR;}

/*Special Characters*/
"::" {return DOUBLE_COLON;}
"=>" {return ARROW;}
"++" {return INCR;}
"--" {return DECR;}
"===" {return IDENTICAL;}
"==" {return EQUAL;}
"!==" {return NOT_IDENTICAL;}
"!=" {return NOT_EQUAL;}
">=" {return GREATER_EQUAL;}
"<=" {return LESS_EQUAL;}
"+=" {return PLUSS_EQUAL;}
"-=" {return MINUS_EQUAL;}
"*=" {return MULT_EQUAL;}
"/=" {return DIV_EQUAL;}
"**=" {return POW_EQUAL;}
".=" {return CONCAT_EQUAL;}
"%=" {return MOD_EQUAL;}
"<<" {return SL;}
">>" {return SR;}
"<<=" {return SL_EQUAL;}
">>=" {return SR_EQUAL;}
"&=" {return AND_EQUAL;}
"|=" {return OR_EQUAL;}
"^=" {return XOR_EQUAL;}
"||" {return SYM_OR;}
"&&" {return SYM_AND;}
";" {return SEMICOLON;}
":" {return COLON;}
"," {return COMMA;}
"." {return DOT;}
"[" {return RECT_OPEN_BRACE;}
"]" {return RECT_CLOSE_BRACE;}
"(" {return OPEN_BRACE;}
")" {return CLOSE_BRACE;}
"{" {return OPEN_CURLY_BRACE;}
"}" {return CLOSE_CURLY_BRACE;}
"|" {return BIT_OR;}
"^" {return BIT_XOR;}
"&" {return BIT_AND;}
"+" {return PLUS;}
"-" {return MINUS;}
"/" {return DIV;}
"*" {return MULT;}
"**" {return POW;}
"=" {return ASSING;}
"%" {return MOD;}
"!" {return NOT;}
"~" {return BIT_NOT;}
"$" {return DOLLAR;}
"<" {return LESS;}
">" {return GREATER;}
"@" {return AT;}
"<?php" {return INI;}
"?>" {return CLOSE;}
. {return ERROR;}
/*Numbers, Id*/
{number}|{integer} {token = yytext(); return NUMBER;}
{decimal}|{exponent} {token = yytext(); return REAL_NUM;}
{identifier} {token = yytext(); return ID;}
{comments} {token = yytext(); return COMMENT;}
{whiteSpace} = {/*Nothing jaja*/}
{string} = {token = yytext(); return STRING;}

/* Compile-time constants*/
{compileConstant} = {return C_CONSTANT;}

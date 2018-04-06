
package analizador_lexico;
/*you shoukd import your enum file with all the symbols */ 
import static analizador_lexico.Token.*;

%% 
/* Lexical functions */ 
%class PHPLexer
%type Token
%column
%line
%ignorecase

%{
    public String token; 
    public int lineNumber; 
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
comments = (.*\/\*[^\/]+\/\s)|(\/\/.*)|(\#.*)
whiteSpace =  [\n\r\t]|(" ")|(\r\n)|(\n) 
lineBreak = [\n]
stringCharacter = [^\u2028\u2029\u000A\u000B\u000C\u000D\u0085\"\\]
escapeSequence = \\[^\u2028\u2029\u000A\u000B\u000C\u000D\u0085]|\\
string = (\"|\')({stringCharacter} | {escapeSequence})*(\"|\')
compileConstant = (_)(CLASS|DIR|FILE|FUNCTION|LINE|METHOD|NAMESPACE|TRAIT)(_)
varNum = (\$)[0-9]+
variable = (\$){identifier}
dataBase = (\$)(recordset)(\[\')[a-zA-Z_0-9]+(\'\])
escape = (\\)
questionMark = \?
quotation = \"
 
%%

/* Patterns */

/*Some erros and validations  that must be first */
{varNum} {token = yytext(); lineNumber = yyline; return ERROR;}
{variable}  {token = yytext(); lineNumber = yyline; return VAR;}
{dataBase} {token = yytext(); lineNumber = yyline; return DB; }
{escape} {token = yytext(); lineNumber = yyline; return ESC; }
{questionMark}  {token = yytext(); lineNumber = yyline; return QUESTION;}
{quotation} {token = yytext(); lineNumber = yyline; return QUOTATION; }

/*Keywords*/
"__halt_compiler" {token = yytext(); lineNumber = yyline; return HALT_COMPILER;}
"abstract" { token = yytext(); lineNumber = yyline; return ABSTRACT;}
"and" {token = yytext(); lineNumber = yyline; return AND;}
"array" {token = yytext(); lineNumber = yyline; return ARRAY;}
"as" {token = yytext(); lineNumber = yyline; return AS;}
"break" {token = yytext().toLowerCase(); lineNumber = yyline; return BREAK;}
"callable" {token = yytext(); lineNumber = yyline; return CALLABLE;}
"case" {token = yytext(); lineNumber = yyline; return CASE;}
"catch" {token = yytext(); lineNumber = yyline; return CATCH;}
"class" {token = yytext(); lineNumber = yyline; return CLASS;}
"clone" {token = yytext(); lineNumber = yyline; return CLONE;}
"const" {token = yytext(); lineNumber = yyline; return CONST;}
"continue" {token = yytext().toLowerCase(); lineNumber = yyline; return CONTINUE;}
"declare" {token = yytext(); lineNumber = yyline; return DECLARE;}
"default" {token = yytext(); lineNumber = yyline; return DEFAULT;}
"die" {token = yytext(); lineNumber = yyline; return DIE;}
"do" {token = yytext().toLowerCase(); lineNumber = yyline; return DO;}
"echo" {token = yytext(); lineNumber = yyline; return ECHO;}
"else" {token = yytext().toLowerCase(); lineNumber = yyline; return ELSE;}
"elseif" | "else if" {token = yytext().toLowerCase(); lineNumber = yyline; return ELSEIF;}
"empty" {token = yytext(); lineNumber = yyline; return EMPTY;}
"enddeclare" {token = yytext(); lineNumber = yyline; return ENDDECLARE;}
"endfor" {token = yytext(); lineNumber = yyline; return ENDFOR;}
"endforeach" {token = yytext(); lineNumber = yyline; return ENDFOREACH;}
"endif" {token = yytext(); lineNumber = yyline; return ENDIF;}
"endswitch" {token = yytext(); lineNumber = yyline; return ENDSWITCH;}
"endwhile" {token = yytext(); lineNumber = yyline; return ENDWHILE;}
"eval" {token = yytext(); lineNumber = yyline; return EVAL;}
"exit" {token = yytext(); lineNumber = yyline; return EXIT;}
"extends" {token = yytext(); lineNumber = yyline; return EXTENDS;}
"final" {token = yytext(); lineNumber = yyline; return FINAL;}
"for" {token = yytext().toLowerCase(); lineNumber = yyline; return FOR;}
"foreach" {token = yytext().toLowerCase(); lineNumber = yyline; return FOREACH;}
"function" {token = yytext(); lineNumber = yyline; return FUNCTION;}
"global" {token = yytext(); lineNumber = yyline; return GLOBAL;}
"goto" {token = yytext(); lineNumber = yyline; return GOTO;}
"if" {token = yytext().toLowerCase(); lineNumber = yyline; return IF;}
"implements" {token = yytext(); lineNumber = yyline; return IMPLEMENTS;}
"include" {token = yytext().toLowerCase(); lineNumber = yyline; return INCLUDE;}
"include_once" {token = yytext(); lineNumber = yyline; return INCLUDE_ONCE;}
"instanceof" {token = yytext(); lineNumber = yyline; return INSTANCEOF;}
"insteadof" {token = yytext(); lineNumber = yyline; return INSTEADOF;}
"interface" {token = yytext(); lineNumber = yyline; return INTERFACE;}
"isset" {token = yytext(); lineNumber = yyline; return ISSET;}
"list" {token = yytext(); lineNumber = yyline; return LIST;}
"namespace" {token = yytext(); lineNumber = yyline; return NAMESPACE;}
"new" {token = yytext(); lineNumber = yyline; return NEW;}
"or" {token = yytext(); lineNumber = yyline; return OR;}
"print" {token = yytext(); lineNumber = yyline; return PRINT;}
"private" {token = yytext(); lineNumber = yyline; return PRIVATE;}
"protected" {token = yytext(); lineNumber = yyline; return PROTECTED;}
"public" {token = yytext(); lineNumber = yyline; return PUBLIC;}
"require" {token = yytext(); lineNumber = yyline; return REQUIRE;}
"require_once" {token = yytext(); lineNumber = yyline; return REQUIRE_ONCE;}
"return" {token = yytext().toLowerCase(); lineNumber = yyline; return RETURN;}
"static" {token = yytext(); lineNumber = yyline; return STATIC;}
"switch" {token = yytext().toLowerCase(); lineNumber = yyline; return SWITCH;}
"throw" {token = yytext(); lineNumber = yyline; return THROW;}
"trait" {token = yytext(); lineNumber = yyline; return TRAIT;}
"try" {token = yytext(); lineNumber = yyline; return TRY;}
"unset" {token = yytext(); lineNumber = yyline; return UNSET;}
"use" {token = yytext(); lineNumber = yyline; return USE;}
"var" {token = yytext(); lineNumber = yyline; return VAR;}
"while" {token = yytext().toLowerCase(); lineNumber = yyline; return WHILE;}
"xor" {token = yytext(); lineNumber = yyline; return XOR;}
{whiteSpace}  {/*Nothing jaja*/}
{lineBreak}  { token = yytext(); lineNumber = yyline; return LINEBREAK;}

/*Special Characters*/
"::" {token = yytext(); lineNumber = yyline; return DOUBLE_COLON;}
"=>" {token = yytext(); lineNumber = yyline; return ARROW;}
"++" {token = yytext(); lineNumber = yyline; return INCR;}
"--" {token = yytext(); lineNumber = yyline; return DECR;}
"===" {token = yytext(); lineNumber = yyline; return IDENTICAL;}
"==" {token = yytext(); lineNumber = yyline; return EQUAL;}
"!==" {token = yytext(); lineNumber = yyline; return NOT_IDENTICAL;}
"!=" {token = yytext(); lineNumber = yyline; return NOT_EQUAL;}
">=" {token = yytext(); lineNumber = yyline; return GREATER_EQUAL;}
"<=" {token = yytext(); lineNumber = yyline; return LESS_EQUAL;}
"+=" {token = yytext(); lineNumber = yyline; return PLUSS_EQUAL;}
"-=" {token = yytext(); lineNumber = yyline; return MINUS_EQUAL;}
"*=" {token = yytext(); lineNumber = yyline; return MULT_EQUAL;}
"/=" {token = yytext(); lineNumber = yyline; return DIV_EQUAL;}
"**=" {token = yytext(); lineNumber = yyline; return POW_EQUAL;}
".=" {token = yytext(); lineNumber = yyline; return CONCAT_EQUAL;}
"%=" {token = yytext(); lineNumber = yyline; return MOD_EQUAL;}
"<<" {token = yytext(); lineNumber = yyline; return SL;}
">>" {token = yytext(); lineNumber = yyline; return SR;}
"<<=" {token = yytext(); lineNumber = yyline; return SL_EQUAL;}
">>=" {token = yytext(); lineNumber = yyline; return SR_EQUAL;}
"&=" {token = yytext(); lineNumber = yyline; return AND_EQUAL;}
"|=" {token = yytext(); lineNumber = yyline; return OR_EQUAL;}
"^=" {token = yytext(); lineNumber = yyline; return XOR_EQUAL;}
"||" {token = yytext(); lineNumber = yyline; return SYM_OR;}
"&&" {token = yytext(); lineNumber = yyline; return SYM_AND;}
";" {token = yytext(); lineNumber = yyline; return SEMICOLON;}
":" {token = yytext(); lineNumber = yyline; return COLON;}
"," {token = yytext(); lineNumber = yyline; return COMMA;}
"." {token = yytext(); lineNumber = yyline; return DOT;}
"[" {token = yytext(); lineNumber = yyline; return RECT_OPEN_BRACE;}
"]" {token = yytext(); lineNumber = yyline; return RECT_CLOSE_BRACE;}
"(" {token = yytext(); lineNumber = yyline; return OPEN_BRACE;}
")" {token = yytext(); lineNumber = yyline; return CLOSE_BRACE;}
"{" {token = yytext(); lineNumber = yyline; return OPEN_CURLY_BRACE;}
"}" {token = yytext(); lineNumber = yyline; return CLOSE_CURLY_BRACE;}
"|" {token = yytext(); lineNumber = yyline; return BIT_OR;}
"^" {token = yytext(); lineNumber = yyline; return BIT_XOR;}
"&" {token = yytext(); lineNumber = yyline; return BIT_AND;}
"+" {token = yytext(); lineNumber = yyline; return PLUS;}
"-" {token = yytext(); lineNumber = yyline; return MINUS;}
"/" {token = yytext(); lineNumber = yyline; return DIV;}
"*" {token = yytext(); lineNumber = yyline; return MULT;}
"**" {token = yytext(); lineNumber = yyline; return POW;}
"=" {token = yytext(); lineNumber = yyline; return ASSING;}
"%" {token = yytext(); lineNumber = yyline; return MOD;}
"!" {token = yytext(); lineNumber = yyline; return NOT;}
"~" {token = yytext(); lineNumber = yyline; return BIT_NOT;}
/*"$" {token = yytext(); lineNumber = yyline; return DOLLAR;}*/
"<" {token = yytext(); lineNumber = yyline; return LESS;}
">" {token = yytext(); lineNumber = yyline; return GREATER;}
"@" {token = yytext(); lineNumber = yyline; return AT;}
"<?php" {token = yytext(); lineNumber = yyline; return INI;}
"?>" {token = yytext(); lineNumber = yyline; return CLOSE;}


/*Numbers, Id*/
{number}|{integer} {token = yytext(); lineNumber = yyline; return NUMBER;}
{decimal}|{exponent} {token = yytext(); lineNumber = yyline; return REAL_NUM;}
{identifier} {token = yytext(); lineNumber = yyline; return ID;}
{comments} {token = yytext(); lineNumber = yyline; return COMMENT;}
{string}  {token = yytext(); lineNumber = yyline; return STRING;}

/* Compile-time constants*/
{compileConstant} = {token = yytext(); lineNumber = yyline; return C_CONSTANT;}

. {token = yytext(); lineNumber = yyline; return ERROR;}

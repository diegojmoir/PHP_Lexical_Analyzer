
package analizador_lexico;
//you shoukd import your enum file with all the symbols 
// import analizador_lexico.Token.*;

%% 
// funciones lexicas 
%class PHPLexer
%type Token
%column
%line

//Regular Expresions
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
comments = (?:#|//)[^\r\n]*|/\*[\s\S]*?\*/
%{
    public String token; 
%}


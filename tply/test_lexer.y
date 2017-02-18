/* test_lexer.pp: test application for the pascal lexer
   To compile: plex paslex
               pyacc test_lexer
               fpc test_lexer
*/

%{

uses LexLib, YaccLib;

var filename : string;

procedure yyerror(msg : string);
  begin
    writeln(filename, ': ', yylineno, ': ',
            msg, ' at or before `', yytext, '''.')
  end(*yyerror*);
%}

%token _AND _ARRAY ASSIGNMENT _BEGIN _CASE CHARACTER_STRING COLON COMMA _CONST DIGSEQ
%token _DIV _DO DOT DOTDOT _DOWNTO _ELSE _END EQUAL _EXTERNAL _FILE _FOR _FORWARD _FUNCTION
%token GE _GOTO GT IDENTIFIER _IF _IN _LABEL LBRAC LE LPAREN LT MINUS _MOD _NIL _NOT
%token NOTEQUAL _OF _OR _OTHERWISE _PACKED PLUS _PROCEDURE _PROGRAM RBRAC
%token REALNUMBER _RECORD _REPEAT RPAREN SEMICOLON _SET SLASH STAR STARSTAR _THEN
%token _TO _TYPE _UNTIL UPARROW _VAR _WHILE _WITH

%token ILLEGAL

%%

comma : COMMA
    {writeln('comma');};

%%

{$i paslex.pas}

var
  ntoken: integer;

begin
  filename := paramStr(1);
  if filename='' then
    begin
      write('input file: ');
      readln(filename);
    end;
  assign(yyinput, filename);
  reset(yyinput);

  ntoken := yylex;
  while ntoken <> 0 do
  begin
    //write the token number and token text
    writeln(ntoken, ': ', yytext);

    //get next token
    ntoken := yylex;
  end;
end.

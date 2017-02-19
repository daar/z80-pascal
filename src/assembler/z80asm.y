/* test_lexer.pp: test application for the pascal lexer
   To compile: plex paslex
               pyacc test_lexer
               fpc test_lexer
*/

%{

uses
  SysUtils, LexLib, YaccLib;

var
  filename : string;

procedure yyerror(msg : string);
var
  fname: string;
begin
  fname := ExtractFileName(filename);
  writeln(Format('%s(%d,%d): %s at or before `%s`.', [fname, yycolno, yylineno, msg, yytext]));
end;

%}

%start input

%token
  COMMENT_          /* comments in the assembler */
  LABEL_            /* labels */
  LPAREN_           /* left parenthesis */
  RPAREN_           /* right parenthesis */
  PLUS_             /* plus sign */
  INSTRUCTION_      /* all assembler instructions */
  REGISTER8_        /* 8bit register */
  REGISTER16_       /* 16bit register */
  DECIMAL_          /* decimal values */
  HEXADECIMAL_      /* hexadecimal values */
  COMMA_            /* comma separator */

%%
input
  : /* empty */
  | input line
  ;

line
  : comment
  | label
  | instruction
  ;

comment
  : COMMENT_
      {
        writeln('comment found ', yytext);
      }
  ;

label
  : LABEL_
      {
        writeln('label found ', yytext);
      }

instruction
  : INSTRUCTION_
      {
        writeln('instruction found ', yytext);
      }
  ;

register
  : REGISTER8_
  | REGISTER16_
  ;

%%

{$i z80asmlex.pas}

var
  ntoken: integer;

begin
  //filename := paramStr(1);
  filename := 'toggle.asm';
  if filename='' then
    begin
      write('input file: ');
      readln(filename);
    end;
  assign(yyinput, filename);
  reset(yyinput);

  if yyparse=0 then
    { done };

{
  ntoken := yylex;
  while ntoken <> 0 do
  begin
    //write the token number and token text
    writeln(ntoken, ': ', yytext);

    //get next token
    ntoken := yylex;
  end;
}
end.

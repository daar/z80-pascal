/* test_lexer.pp: test application for the pascal lexer
   To compile: plex paslex
               pyacc test_lexer
               fpc test_lexer
*/

%{

uses
  SysUtils, StrUtils, LexLib, YaccLib;

var
  filename : string;

procedure yyerror(msg : string);
var
  fname: string;
begin
  fname := ExtractFileName(filename);
  writeln(Format('%s(%d,%d) %s: at or before "%s".', [fname, yylineno, yycolno, msg, yytext]));
end;

procedure EmitOpcode(opcode: byte);
begin
  writeln('debug: EmitOpCode(', opcode,')');
  write(yyoutput, Chr(opcode));
end;

procedure EmitOpcode(opcode: byte; hex: string);
var
  w: word;
begin
  w := Hex2Dec(hex);

  if length(hex) = 3 then
  begin
    writeln('debug: EmitOpCode(', opcode,', ', Lo(w),')');
    write(yyoutput, Chr(opcode), Lo(w));
  end
  else
  begin
    writeln('debug: EmitOpCode(', opcode,', ', Lo(w), ', ', Hi(w),')');
    write(yyoutput, Chr(opcode), Chr(Lo(w)), Chr(Hi(w)));
  end;
end;

%}

%token
  COMMENT_          /* comments in the assembler */
  LABEL_            /* labels */
  LPAREN_           /* left parenthesis */
  RPAREN_           /* right parenthesis */
  PLUS_             /* plus sign */
  INSTRUCTION_      /* all assembler instructions */
  IN_ZERO_          /* instruction with no arguments */
  REG8_             /* 8bit register */
  REG16_            /* 16bit register */
  DECIMAL_          /* decimal values */
  HEX8_             /* 8 bit hexadecimal values */
  HEX16_            /* 16 bit hexadecimal values */
  COMMA_            /* comma separator */

  %type <String> instruction
  %type <String> register16
  %type <String> hexadecimal16

%%
input
  : /* empty */
  | input line
  ;

line
  : comment
  | label
  | instruction
  | register
  | register value
  ;

comment
  : COMMENT_
  ;

label
  : label ":"
    {
      writeln('label start found ', yytext);
    }
  | LABEL_
    {
      writeln('label reference found ', yytext);
    }

instruction
  : instruction register "," value
    {
      writeln('instruction with register and value found ', yytext, ' ', $1);
    }
  | instruction register16 "," hexadecimal16
    {
      writeln('instruction with register found $ ', $1, ' ', $2, ' ', yytext);

      case $1 of
        'LD':
          case $2 of
            'BC': EmitOpCode(1, yytext);
          end;
      end;
    }
  | instruction "(" register16 ")" "," register8
    {
      writeln('instruction with register found $ ', $1, ' ', $3, ' ', yytext);

      case $1 of
        'LD':
          case $3 of
            'BC': EmitOpCode(2);
          end;
      end;
    }
  | instruction register
    {
      writeln('instruction with register found ', yytext, '-', $1);
    }
  | instruction label
    {
      writeln('instruction with label found ', yytext, '-', $1);
    }
  | INSTRUCTION_
    {
      //no argument instruction
      case yytext of
        'NOP': EmitOpCode(0);
        'RLCA': EmitOpCode(7);
        'RRCA': EmitOpCode(15);
        'RLA': EmitOpCode(23);
        'RRA': EmitOpCode(31);
        'DAA': EmitOpCode(39);
        'CPL': EmitOpCode(47);
        'SCF': EmitOpCode(55);
        'CCF': EmitOpCode(63);
        'HALT': EmitOpCode(118);
        'RET': EmitOpCode(201);
        'EXX': EmitOpCode(217);
        'DI': EmitOpCode(243);
        'EI': EmitOpCode(251);
      end;
    }
  ;

register8
  : REG8_
  ;

register16
  : REG16_
  ;

value
  : DECIMAL_
  | HEX8_
  ;

hexadecimal16
  : "(" hexadecimal16 ")"
  | HEX16_
  ;
%%

{$i z80asmlex.pas}

var
  comp_time: double;

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

  assign(yyoutput, ChangeFileExt(filename, '.obj'));
  rewrite(yyoutput);

  comp_time := Now;
  if yyparse=0 then
  begin
    writeln(num_lines, ' lines compiled, ', (Now - comp_time) * 24 * 3600:0:1, ' sec');
  end;
end.

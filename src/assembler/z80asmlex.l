%{

var
  num_lines: integer = 0;
%}

digit			[0-9]
alpha			[a-fA-F]

{ Z80 instruction set }

{ instructions that do not take any parameters }
INSTR_0 ("CCF"|"CPL"|"DAA"|"DI"|"EI"|"EXX"|"HALT"|"NOP"|"RLA"|"RLCA"|"RRA"|"RRCA"|"SCF")

{ instructions that take either zero or one parameter }
INSTR_0_1 ("RET")

{ instructions that do take one parameter }
INSTR_1 ("AND"|"CP"|"DEC"|"DJNZ"|"INC"|"OR"|"POP"|"PUSH"|"RST"|"XOR")

{ instructions that take either one or two parameters }
INSTR_1_2 ("CALL"|"JP"|"JR")

{ instructions that do take two parameters }
INSTR_2 ("ADC"|"ADD"|"EX"|"IN"|"LD"|"OUT"|"SBC"|"SUB")

Reg8 ("A"|"B"|"C"|"D"|"E"|"F"|"H"|"L"|"I"|"R"|"IXH"|"IXL"|"IYH"|"IYL")
Reg16 ("AF"|"BC"|"DE"|"HL"|"PC"|"SP"|"IX"|"IY")

%%

\n              begin inc(num_lines); {writeln(num_lines);} end;
[ \t]           ;
";".*$					return(COMMENT_);

{Reg8}					begin yylval.yyString := yytext; return(REG8_); end;
{Reg16}					begin yylval.yyString := yytext; return(REG16_); end;

[$]({digit}|{alpha}){2}
                begin yylval.yyString := yytext; return(HEX8_); end;
[$]({digit}|{alpha}){4}
                begin yylval.yyString := yytext; return(HEX16_); end;
{digit}*				begin yylval.yyString := yytext; return(DECIMAL_); end;

{INSTR_0}  			begin yylval.yyString := yytext; return(INSTRUCTION_); end;
{INSTR_0_1}			begin yylval.yyString := yytext; return(INSTRUCTION_); end;
{INSTR_1}  			begin yylval.yyString := yytext; return(INSTRUCTION_); end;
{INSTR_1_2}			begin yylval.yyString := yytext; return(INSTRUCTION_); end;
{INSTR_2}  			begin yylval.yyString := yytext; return(INSTRUCTION_); end;

[a-zA-Z][a-z|A-Z|0-9]+
 								return(LABEL_);

"("             returnc(yytext[1]);
")"             returnc(yytext[1]);
"+"             return(PLUS_);
","             returnc(yytext[1]);
":"             returnc(yytext[1]);

.	 							begin
 									writeln('fatal error: unknown token `', yytext,'`');
									halt(-1);
								end;

%%

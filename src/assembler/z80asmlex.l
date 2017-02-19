%{

var
  hex: string[5];
  locctr: integer;
  i: integer = 0;

%}

digit			[0-9]
alpha			[a-fA-F]
hextail		({digit}|{alpha}){2}
hex				[$]{hextail}

Instr ("LD"|"INC"|"JR")
Reg8 ("A"|"B"|"C"|"D"|"E"|"F"|"H"|"L"|"I"|"R"|"IXH"|"IXL"|"IYH"|"IYL")
Reg16 ("AF"|"BC"|"DE"|"HL"|"PC"|"SP"|"IX"|"IY")

%%

[ \t\n]         ;
";".*$					return(COMMENT_);

[a-zA-Z][a-z|A-Z|0-9]+:
 								return(LABEL_);
{hex}						return(HEXADECIMAL_);
{digit}*				return(DECIMAL_);
{Instr}    			return(INSTRUCTION_);
{Reg8}					return(REGISTER8_);
{Reg16}					return(REGISTER16_);

"("             return(LPAREN_);
")"             return(RPAREN_);
"+"             return(PLUS_);
","             return(COMMA_);

.	 							begin
 									writeln('fatal error: unknown token `', yytext,'`');
									halt(-1);
								end;

%%

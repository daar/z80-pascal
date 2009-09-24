(* This is part of the Pascal compiler.

   It defines the assembler used at the ASM .. END block. *)



(* ASMCompoundStatement ::= "ASM" [ASMStatementSequence] "END" .
   ASMStatementSequence ::= Z80Statement [ ";" Z80Statement ]* . *)
PROCEDURE TPascalCompiler.ASMCompoundStatement;
BEGIN
  IF fScanner.LastToken <> 'ASM' THEN
    RAISE CompilationException.Expected ('ASM', fScanner.LastToken);
  WHILE fScanner.GetToken <> 'END' DO
    IF fScanner.LastToken <> ';' THEN
      Z80Statement;
END;



(* Actually this is a very simple Z80 assembler.
   It is so simple that it allows weird Z80 statements as "CALL (HL), NZ"
   and such. *)
PROCEDURE TPascalCompiler.Z80Statement;

(* Looks for an item in the given list.
   Returns the item index or (-1) if not found. *)
  FUNCTION FindItem (Item: STRING; ItemList: ARRAY OF STRING): INTEGER;
  VAR
    Index: INTEGER;
  BEGIN
    RESULT := -1;
    FOR Index := 0 TO (Length (ItemList) - 1) DO
      IF Item = ItemList[Index] THEN RESULT := Index;
  END;



(* Extracts and returns a valid parameter. *)
  FUNCTION Z80Parameter: STRING;
  VAR
  { This parameter list includes conditionals. }
    ParameterList: ARRAY [0..21] OF STRING = (
      'A',  'AF', 'B', 'BC', 'C',  'D',  'DE', 'E',  'H',  'HL',
      'IX', 'IY', 'L', 'M',  'NC', 'NZ', 'P',  'PC', 'PE', 'PO',
      'SP', 'Z'
    );
    Tmp, TmpNum: STRING;
  BEGIN
    IF fScanner.IsAlpha (fScanner.Lookahead) THEN
      RESULT := fScanner.GetIdentifier
    ELSE IF (fScanner.Lookahead = '$')
         OR fScanner.IsDigit (fScanner.Lookahead) THEN
      RESULT := fScanner.GetNumber
    ELSE IF fScanner.Lookahead = '(' THEN
      BEGIN
	fScanner.SkipCharacter; { Skips '(' }
	Tmp := fScanner.GetToken;
	IF NOT (fScanner.LastTokenId IN [tiIdentifier, tiInteger, tiHexInteger]) THEN
	  RAISE CompilationException.Create ('Bad ASM parameter');
	IF fScanner.LastTokenId = tiHexInteger THEN
	  Tmp[1] := '#';
	IF fScanner.Lookahead = '+' THEN
	BEGIN
	  fScanner.SkipCharacter; { Skips '+' }
	  TmpNum := fScanner.GetNumber;
	  IF fScanner.LastTokenId = tiHexInteger THEN
	    TmpNum[1] := '#';
	  Tmp := Tmp + '+' + TmpNum;
	END;
	IF fScanner.Lookahead <> ')' THEN
	  RAISE CompilationException.Expected (')', fScanner.GetToken);
	fScanner.SkipCharacter; { Skips ')' }
        RESULT := '('+Tmp+')';
      END
    ELSE
      RAISE CompilationException.Expected ('Z80Parameter', fScanner.GetToken);
  END;



VAR
{ List of all Z80 instruction set. }
  Z80Instruction: ARRAY [0..64] OF STRING = (
    'ADC',  'ADD',  'AND', 'BIT',  'CALL', 'CCF',   'CP',   'CPD',  'CPDR', 'CPI',
    'CPIR', 'CPL',  'DAA', 'DEC',  'DI',   'DJNZ,', 'EI',   'EX',   'EXX',  'HALT',
    'IM',   'IN',   'INC', 'IND',  'INDR', 'INI',   'INIR', 'JP',   'JR',   'LD',
    'LDD',  'LDDR', 'LDI', 'LDIR', 'NEG',  'NOP',   'OR',   'OTDR', 'OTIR', 'OUT',
    'OUTD', 'OUTI', 'POP', 'PUSH', 'RES',  'RET',   'RETI', 'RETN', 'RL',   'RLA',
    'RLC',  'RLCA', 'RLD', 'RR',   'RRA',  'RRC',   'RRD',  'RST',  'SCF',  'SET',
    'SLA',  'SRA',  'SBC', 'SUB',  'XOR');
{ To build the instruction. }
  Instruction: STRING;
BEGIN
{ Z80Statement ::= Z80Instruction [ Z80Parameter [ "," Z80Parameter ] ] . }
  IF FindItem (fScanner.LastToken, Z80Instruction) < 0 THEN
    RAISE CompilationException.Expected ('Z80Instruction', fScanner.LastToken);
  Instruction := fScanner.LastToken;
  IF fScanner.Lookahead <> ';' THEN
  BEGIN
    Instruction := Instruction + ' ' + Z80Parameter;
    IF fScanner.Lookahead = ',' THEN
    BEGIN
	fScanner.SkipCharacter; { Skips ',' }
      Instruction := Instruction + ', ' + Z80Parameter;
    END;
  END;
  fOutput.Emit (Instruction);
END;

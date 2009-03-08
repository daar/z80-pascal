UNIT Compiler;
(*< Defines the Pascal compiler, that loads a file, parses it and saves the
    result in another file. *)

INTERFACE

USES
  ULexScan,
  Classes, sysutils;



TYPE
(* The actual compiler. *)
  TPascalCompiler = CLASS
  PRIVATE
    fScanner: TLexicalScanner;
    fFileName: STRING;
    fOutput: TStringList;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;

  (* Entry point for compilation. *)
    PROCEDURE Compile (aFilename: STRING);

  (* Set the input file. *)
    PROPERTY FileName: STRING READ fFileName;
  (* Compilation result. *)
    PROPERTY Output: TStringList READ fOutput;
  PRIVATE
  { Recursive compiler. }
    PROCEDURE PascalProgram;
    PROCEDURE Block;
    PROCEDURE DeclarationPart;
    PROCEDURE StatementPart;
    PROCEDURE CompoundStatement;

  { Assembler. }
    PROCEDURE ASMCompoundStatement;
    PROCEDURE Z80Statement;
  END;



(* To manage compilation exceptions. *)
  CompilationException = CLASS (Exception)
  PUBLIC
    CONSTRUCTOR Expected (fExpected, fFound: STRING);
  END;



VAR
(* THE compiler. *)
  PascalCompiler: TPascalCompiler;



IMPLEMENTATION

(* Something expected but otherthing found. *)
CONSTRUCTOR CompilationException.Expected (fExpected, fFound: STRING);
BEGIN
  INHERITED Create (''''+fExpected+''' expected but '''+fFound+''' found.');
END;



(************
 * Compiler *
 ************)

(* Constructor. *)
CONSTRUCTOR TPascalCompiler.Create;
BEGIN
  fOutput := TStringList.Create;
END;



(* Destructor. *)
DESTRUCTOR TPascalCompiler.Destroy;
BEGIN
  IF fScanner <> NIL THEN fScanner.Free;
  IF fOutput  <> NIL THEN fOutput .Free;
  INHERITED;
END;



(* Entry point for compilation. *)
PROCEDURE TPascalCompiler.Compile (aFileName: STRING);
VAR
  Token: STRING;
BEGIN
{ Set up the scanner. }
  IF fScanner <> NIL THEN
    RAISE CompilationException.Create ('''Compile'' should be called only once!');
  fScanner := TLexicalScanner.Create (aFileName);
{ BNF: Z80pas ::= PascalProgram .
       PascalProgram ::= PascalProgram .
}
  Token := fScanner.GetToken;
  IF Token = 'PROGRAM' THEN
    SELF.PascalProgram
  ELSE
    RAISE CompilationException.Expected ('PROGRAM', Token);
END;



(**********************
 * Recursive compiler *
 **********************)

(* PascalProgram ::= "PROGRAM" NewIdent ";" Block "." . *)
PROCEDURE TPascalCompiler.PascalProgram;
VAR
  ProgramIdentifier: STRING;
BEGIN
  IF fScanner.LastToken <> 'PROGRAM' THEN
    RAISE CompilationException.Expected ('PROGRAM', fScanner.LastToken);
  ProgramIdentifier := fScanner.GetIdentifier; { NewIdent ::= identifier . }
  WriteLn ('Program name ''', ProgramIdentifier, '''');
  IF fScanner.GetToken <> ';' THEN
    RAISE CompilationException.Expected (';', fScanner.LastToken);
{ TODO: Program prolog. }
  SELF.Block;
  IF fScanner.GetToken <> '.' THEN
    RAISE CompilationException.Expected ('.', fScanner.LastToken);
{ TODO: Program epilog. }
END;



(* Block ::= DeclarationPart StatementPart . *)
PROCEDURE TPascalCompiler.Block;
BEGIN
  SELF.DeclarationPart;
  SELF.StatementPart;
END;



(* At the moment, DeclarationPart is empty. *)
PROCEDURE TPascalCompiler.DeclarationPart;
BEGIN
  ;
END;



(* StatementPart ::= CompoundStatement | ASMCompoundStatement . *)
PROCEDURE TPascalCompiler.StatementPart;
BEGIN

  IF fScanner.GetToken = 'ASM' THEN
    SELF.ASMCompoundStatement
  ELSE
    SELF.CompoundStatement;
END;



(* CompoundStatement ::= "BEGIN" StatementSequence "END" .
   StatementSequence ::= Statement [ ";" Statement ]* . *)
PROCEDURE TPascalCompiler.CompoundStatement;
BEGIN
  IF fScanner.LastToken <> 'BEGIN' THEN
    RAISE CompilationException.Expected ('BEGIN', fScanner.LastToken);
  WHILE fScanner.GetToken <> 'END' DO
  BEGIN
    IF fScanner.LastToken = 'ASM' THEN
      SELF.ASMCompoundStatement
    ELSE IF fScanner.LastToken = ';' THEN
    { This allows empty statements. }
      CONTINUE
    ELSE
      RAISE CompilationException.Create ('Synax error.');
  END;
END;



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
	fScanner.GetChar; fScanner.SkipWhite; { Skips '(' }
	Tmp := fScanner.GetToken;
	IF NOT (fScanner.LastTokenId IN [tiIdentifier, tiInteger, tiHexInteger]) THEN
	  RAISE CompilationException.Create ('Bad ASM parameter');
	IF fScanner.LastTokenId = tiHexInteger THEN
	  Tmp[1] := '#';
	IF fScanner.Lookahead = '+' THEN
	BEGIN
	  fScanner.GetChar; fScanner.SkipWhite; { Skips '+' }
	  TmpNum := fScanner.GetNumber;
	  IF fScanner.LastTokenId = tiHexInteger THEN
	    TmpNum[1] := '#';
	  Tmp := Tmp + '+' + TmpNum;
	END;
	IF fScanner.Lookahead <> ')' THEN
	  RAISE CompilationException.Expected (')', fScanner.GetToken);
	fScanner.GetChar; fScanner.SkipWhite; { Skips ')' }
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
	fScanner.GetChar; fScanner.SkipWhite; { Skips ',' }
      Instruction := Instruction + ', ' + Z80Parameter;
    END;
  END;
  fOutput.Add (Instruction);
END;



INITIALIZATION
  PascalCompiler := TPascalCompiler.Create;



FINALIZATION
  IF PascalCompiler <> NIL THEN
    PascalCompiler.Free;
END.

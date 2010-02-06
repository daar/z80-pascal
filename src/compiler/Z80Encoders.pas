UNIT Z80Encoders;
(*<Expands the abstract Pascal encoder defining platform targets.  At the
   moment only a generic Z-80 is defined. *)
(*
  Copyright (c) 2009, 2010 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
*)
INTERFACE

USES
  PasCompiler,
  Classes;



TYPE
(* Implmements a generic Z-80 encoder.  Actually it translates P-Code items to
   Z-80 Assembler code.  It also parses "ASM .. END" blocks.
   @seealso(TPascalEncoder) *)
  TZ80Encoder = CLASS (TPascalEncoder)
  PRIVATE
    fOutputStrings: TStringList;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create; OVERRIDE;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Add a coment line to the output. *)
    PROCEDURE AddComment (Comment: STRING); OVERRIDE;
  (* Emits program prologue. *)
    PROCEDURE ProgramProlog (ProgramName: STRING); OVERRIDE;
  (* Emits program Epilogue. *)
    PROCEDURE ProgramEpilog; OVERRIDE;
  (* It's called by the compiler when it finds an "ASM .. END" block.  It
     uses the RefScanner, the first token should be "ASM" and it parses until
     the first END token. *)
    PROCEDURE ParseASM; OVERRIDE;
  (* Actually this is a very simple Z80 assembler.
     It is so simple that it allows weird Z80 statements as "CALL (HL), NZ"
     and such. *)
    PROCEDURE Z80Statement;



  (* Adds an assembler line. *)
    PROCEDURE Emit (aLine: STRING); OVERRIDE;
  (* Saves the output. *)
    PROCEDURE SaveToFile (Filename: STRING);
  END;



IMPLEMENTATION

(***************
 * TZ80Encoder *
 ***************)

(* Constructor. *)
  CONSTRUCTOR TZ80Encoder.Create;
  BEGIN
    INHERITED Create;
    fOutputStrings := TStringList.Create;
  END;



(* Destructor. *)
  DESTRUCTOR TZ80Encoder.Destroy;
  BEGIN
    IF fOutputStrings <> NIL THEN
      fOutputStrings.Destroy;
    INHERITED Destroy;
  END;



(* Add a coment line to the output. *)
  PROCEDURE TZ80Encoder.AddComment (Comment: STRING);
  BEGIN
    fOutputStrings.Add ('; '+Comment);
  END;



(* Emits program prologue. *)
  PROCEDURE TZ80Encoder.ProgramProlog (ProgramName: STRING);
  BEGIN
    SELF.AddComment ('> Assembler file created by Z80-Pascal.');
    SELF.AddComment ('>');
// TODO    SELF.AddComment ('> File: '+fFileName);
    SELF.AddComment ('> Program name: '+ProgramName);
    SELF.Emit ('');
    SELF.AddComment ('This orig was set staticly.');
    SELF.Emit ('ORG #F000');
    SELF.Emit ('');
  END;



(* Emits program epilogue. *)
  PROCEDURE TZ80Encoder.ProgramEpilog;
  BEGIN
    SELF.AddComment ('> Program finishes here.');
    SELF.Emit ('RET');
  END;



(* ASMCompoundStatement ::= "ASM" [ASMStatementSequence] "END" .
   ASMStatementSequence ::= Z80Statement [ ";" Z80Statement ]* . *)
  PROCEDURE TZ80Encoder.ParseASM;
  BEGIN
    IF RefScanner.LastToken <> 'ASM' THEN
      RAISE CompilationException.Expected ('ASM', RefScanner.LastToken);
    WHILE RefScanner.GetToken <> 'END' DO
      IF RefScanner.LastToken <> ';' THEN
	Z80Statement;
  END;



(* Actually this is a very simple Z80 assembler.
   It is so simple that it allows weird Z80 statements as "CALL (HL), NZ"
   and such. *)
  PROCEDURE TZ80Encoder.Z80Statement;

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
      Tmp, TmpNum: STRING;
    BEGIN
      IF RefScanner.IsAlpha (RefScanner.Lookahead) THEN
	RESULT := RefScanner.GetIdentifier
      ELSE IF (RefScanner.Lookahead = '$')
	   OR RefScanner.IsDigit (RefScanner.Lookahead) THEN
	RESULT := RefScanner.GetNumber
      ELSE IF RefScanner.Lookahead = '(' THEN
      BEGIN
	RefScanner.SkipCharacter; { Skips '(' }
	Tmp := RefScanner.GetToken;
	IF NOT (RefScanner.LastTokenType IN [pttIdentifier, pttInteger, pttHexInteger])
	THEN
	  RAISE CompilationException.Create ('Bad ASM parameter');
	IF RefScanner.LastTokenType = pttHexInteger THEN
	  Tmp[1] := '#';
	IF RefScanner.Lookahead = '+' THEN
	BEGIN
	  RefScanner.SkipCharacter; { Skips '+' }
	  TmpNum := RefScanner.GetNumber;
	  IF RefScanner.LastTokenType = pttHexInteger THEN
	    TmpNum[1] := '#';
	  Tmp := Tmp + '+' + TmpNum;
	END;
	IF RefScanner.Lookahead <> ')' THEN
	  RAISE CompilationException.Expected (')', RefScanner.GetToken);
	RefScanner.SkipCharacter; { Skips ')' }
        RESULT := '('+Tmp+')';
      END
      ELSE
	RAISE CompilationException.Expected ('Z80Parameter', RefScanner.GetToken);
    END;



  VAR
  { List of all Z80 instruction set. }
    Z80Instruction: ARRAY [0..64] OF STRING = (
      'ADC',  'ADD',  'AND',  'BIT',  'CALL', 'CCF',  'CP',   'CPD',
      'CPDR', 'CPI',  'CPIR', 'CPL',  'DAA',  'DEC',  'DI',   'DJNZ,',
      'EI',   'EX',   'EXX',  'HALT', 'IM',   'IN',   'INC',  'IND',
      'INDR', 'INI',  'INIR', 'JP',   'JR',   'LD',   'LDD',  'LDDR',
      'LDI',  'LDIR', 'NEG',  'NOP',  'OR',   'OTDR', 'OTIR', 'OUT',
      'OUTD', 'OUTI', 'POP',  'PUSH', 'RES',  'RET',  'RETI', 'RETN',
      'RL',   'RLA',  'RLC',  'RLCA', 'RLD',  'RR',   'RRA',  'RRC',
      'RRD',  'RST',  'SCF',  'SET',  'SLA',  'SRA',  'SBC',  'SUB',  'XOR');
  { To build the instruction. }
    Instruction: STRING;
  BEGIN
  { Z80Statement ::= Z80Instruction [ Z80Parameter [ "," Z80Parameter ] ] . }
    IF FindItem (RefScanner.LastToken, Z80Instruction) < 0 THEN
      RAISE CompilationException.Expected ('Z80Instruction',
					   RefScanner.LastToken);
    Instruction := RefScanner.LastToken;
    IF RefScanner.Lookahead <> ';' THEN
    BEGIN
      Instruction := Instruction + ' ' + Z80Parameter;
      IF RefScanner.Lookahead = ',' THEN
      BEGIN
	RefScanner.SkipCharacter; { Skips ',' }
	Instruction := Instruction + ', ' + Z80Parameter;
      END;
    END;
    SELF.Emit (Instruction);
  END;



(* Adds a raw assembler line. *)
  PROCEDURE TZ80Encoder.Emit (aLine: STRING);
  BEGIN
    fOutputStrings.Add (''+^I+aLine);
  END;



(* Saves the output. *)
  PROCEDURE TZ80Encoder.SaveToFile (Filename: STRING);
  BEGIN
    fOutputStrings.SaveToFile (Filename);
  END;

END.

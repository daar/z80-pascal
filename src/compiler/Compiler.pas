(* Defines the Pascal compiler, that loads a file, parses it and saves the
   result in another file. *)
UNIT Compiler;

INTERFACE

USES
  ULexScan, UEncoder,
  Classes, sysutils;



TYPE
(* The actual compiler. *)
  TPascalCompiler = CLASS
  PRIVATE
    fScanner: TLexicalScanner;
    fFileName: STRING;
    fOutput: TEncoder;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;

  (* Entry point for compilation. *)
    PROCEDURE Compile (aFilename: STRING);
  (* Saves the result on a file. *)
    PROCEDURE SaveToFile (aFilename: STRING);

  (* Set the input file. *)
    PROPERTY FileName: STRING READ fFileName;
  PRIVATE
  { Recursive compiler. }
    PROCEDURE PascalProgram;
    PROCEDURE Block;
    PROCEDURE DeclarationPart;
    PROCEDURE StatementPart;
    PROCEDURE CompoundStatement;

  { Assembler.  Actually implemented on file 'z80asm.inc' }
    PROCEDURE ASMCompoundStatement;
    PROCEDURE Z80Statement;
  END;



(* To manage compilation exceptions. *)
  CompilationException = CLASS (Exception)
  PUBLIC
  (* Something expected but anotherthing found. *)
    CONSTRUCTOR Expected (fExpected, fFound: STRING);
  END;



VAR
(* THE compiler. *)
  PascalCompiler: TPascalCompiler;



IMPLEMENTATION

(* Something expected but anotherthing found. *)
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
    fOutput := TEncoder.Create;
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
  { Z80pas ::= PascalProgram .
    PascalProgram ::= PascalProgram . }
    Token := fScanner.GetToken;
    IF Token = 'PROGRAM' THEN
      SELF.PascalProgram
    ELSE
      RAISE CompilationException.Expected ('PROGRAM', Token);
  END;



(* Saves the result on a file. *)
  PROCEDURE TPascalCompiler.SaveToFile (aFilename: STRING);
  BEGIN
    fOutput.SaveToFile (aFilename);
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
  { TODO: May be it should store the program identifier in a symbol list to
    prevent duplicate definitions. }
    WriteLn ('Program name ''', ProgramIdentifier, '''');
    IF fScanner.GetToken <> ';' THEN
      RAISE CompilationException.Expected (';', fScanner.LastToken);
  { Program prolog. }
    fOutput.AddProlog (fFileName, ProgramIdentifier);
    SELF.Block;
    IF fScanner.GetToken <> '.' THEN
      RAISE CompilationException.Expected ('.', fScanner.LastToken);
    fOutput.AddEpilog;
  END;



(* Block ::= DeclarationPart StatementPart . *)
  PROCEDURE TPascalCompiler.Block;
  BEGIN
    SELF.DeclarationPart;
    fOutput.AddComment ('; ');
    fOutput.AddComment ('; Program starts here');
    fOutput.AddComment ('; ');
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



(* The ASM .. END block is in another file for make it easy to read. *)
{$include z80asm.inc}



INITIALIZATION
  PascalCompiler := TPascalCompiler.Create;



FINALIZATION
  IF PascalCompiler <> NIL THEN
    PascalCompiler.Free;
END.

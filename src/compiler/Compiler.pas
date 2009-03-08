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

    PROCEDURE ASMCompoundStatement;
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

(* Something expected bot otherthing found. *)
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
{ BNF: Z80pas = PascalProgram .
       PascalProgram = "PROGRAM" ...
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
    ;
END;



INITIALIZATION
  PascalCompiler := TPascalCompiler.Create;



FINALIZATION
  IF PascalCompiler <> NIL THEN
    PascalCompiler.Free;
END.

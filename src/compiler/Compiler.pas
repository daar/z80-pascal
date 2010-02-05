(* Defines the Pascal compiler, that loads a file, parses it and saves the
   result in another file. *)
UNIT Compiler;

INTERFACE

USES
  PasCompiler, UEncoder,
  Classes, sysutils;



TYPE
(* The actual compiler. *)
  TPascalCompiler = CLASS
  PRIVATE
    fScanner: TPascalLexicalScanner;
    fFileName: STRING;
    fOutput: TEncoder;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;

  (* Entry point for compilation. *)
    PROCEDURE Compile (aFileName: STRING);
  (* Saves the result on a file. *)
    PROCEDURE SaveToFile (aFileName: STRING);

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

  { Output. }
    PROCEDURE DispatchLines;
    PROCEDURE ProgramProlog (ProgramName: STRING);
    PROCEDURE ProgramEpilog;

    PROCEDURE AddComment (Comment: STRING);
    PROCEDURE Emit (aLine: STRING);
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

USES
  Configuration;



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
    IF fOutput  <> NIL THEN fOutput.Free;
    INHERITED;
  END;



(* Entry point for compilation. *)
  PROCEDURE TPascalCompiler.Compile (aFileName: STRING);
  VAR
    Token: STRING;
    aFile: TFileStream;
  BEGIN
  { Set up the scanner. }
    IF fScanner <> NIL THEN
      RAISE CompilationException.Create ('''Compile'' should be called only once!');
    aFile := TFileStream.Create (aFileName, fmOpenRead);
    TRY
      fScanner := TPascalLexicalScanner.Create (aFile, Configuration.ListsComments);
    { Z80pas ::= PascalProgram .
      PascalProgram ::= PascalProgram . }
      Token := fScanner.GetToken;
      IF Token = 'PROGRAM' THEN
        SELF.PascalProgram
      ELSE
        RAISE CompilationException.Expected ('PROGRAM', Token);
    FINALLY
      aFile.Free;
    END;
  END;



(* Saves the result on a file. *)
  PROCEDURE TPascalCompiler.SaveToFile (aFileName: STRING);
  BEGIN
    fOutput.SaveToFile (aFileName);
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
    SELF.ProgramProlog (ProgramIdentifier);
    SELF.Block;
    IF fScanner.GetToken <> '.' THEN
      RAISE CompilationException.Expected ('.', fScanner.LastToken);
    SELF.ProgramEpilog;
  END;



(* Block ::= DeclarationPart StatementPart . *)
  PROCEDURE TPascalCompiler.Block;
  BEGIN
    SELF.DeclarationPart;
    SELF.AddComment ('; ');
    SELF.AddComment ('; Program starts here');
    SELF.AddComment ('; ');
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



(**********
 * Output *
 **********)

  PROCEDURE TPascalCompiler.DispatchLines;
  VAR
    Ndx: INTEGER;
  BEGIN
    IF Configuration.ListsComments AND (fScanner.Comments.Count > 0) THEN
    BEGIN
      FOR Ndx := 0 TO fScanner.Comments.Count - 1 DO
	fOutput.AddComment (fScanner.Comments.Strings[Ndx]);
      fScanner.Comments.Clear;
    END;
  END;



  PROCEDURE TPascalCompiler.ProgramProlog (ProgramName: STRING);
  BEGIN
    SELF.AddComment ('; Assembler file created by Z80-Pascal.');
    SELF.AddComment (';');
    SELF.AddComment ('; File: '+fFileName);
    SELF.AddComment ('; Program name: '+ProgramName);
    SELF.Emit ('');
    SELF.AddComment ('This orig was set staticly.');
    SELF.Emit ('ORG #F000');
    SELF.Emit ('');
  END;



  PROCEDURE TPascalCompiler.ProgramEpilog;
  BEGIN
    SELF.AddComment ('; Program finishes here.');
    SELF.Emit ('RET');
  END;



  PROCEDURE TPascalCompiler.AddComment (Comment: STRING);
  BEGIN
    IF Configuration.ListsComments AND (fScanner.Comments.Count > 0) THEN
      DispatchLines;
    fOutput.AddComment (Comment);
  END;



  PROCEDURE TPascalCompiler.Emit (aLine: STRING);
  BEGIN
    IF Configuration.ListsComments AND (fScanner.Comments.Count > 0) THEN
      DispatchLines;
    fOutput.Emit (aLine);
  END;



INITIALIZATION
  PascalCompiler := TPascalCompiler.Create;



FINALIZATION
  IF PascalCompiler <> NIL THEN
    PascalCompiler.Free;
END.

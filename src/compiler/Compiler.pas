UNIT Compiler;
(*< Defines the Pascal compiler, that loads a file, parses it and saves the
    result in another file. *)

INTERFACE

USES
  Classes;



TYPE
(* The actual compiler. *)
  TPascalCompiler = CLASS
  PRIVATE
  (* From -> To. *)
    InputStream, OutputStream: TFileStream;
    fInputFilename, fOutputFileName: STRING;
  (* To open the files. *)
    PROCEDURE PutInputFilename (aNewFileName: STRING);
    PROCEDURE PutOutputFilename (aNewFileName: STRING);
  PUBLIC
  (* Destructor *)
    DESTRUCTOR Destroy; OVERRIDE;

    PROPERTY InputFileName: STRING READ fInputFileName WRITE PutInputFilename;
    PROPERTY OutputFileName: STRING READ fOutputFileName WRITE PutOutputFilename;
  END;



VAR
(* THE compiler. *)
  PascalCompiler: TPascalCompiler;



IMPLEMENTATION

USES
  sysutils;



(* Assigns the input file. *)
PROCEDURE TPascalCompiler.PutInputFileName (aNewFileName: STRING);
BEGIN
  IF SELF.InputStream <> NIL THEN
    RAISE Exception.Create ('Input stream setted twice!');
  SELF.InputStream := TFileStream.Create (aNewFileName, fmOpenRead, 0);
END;



(* Assigns the Output file. *)
PROCEDURE TPascalCompiler.PutOutputFileName (aNewFileName: STRING);
BEGIN
  IF SELF.OutputStream <> NIL THEN
    RAISE Exception.Create ('Output stream setted twice!');
  SELF.OutputStream := TFileStream.Create (aNewFileName, fmCreate, 0);
END;



(* Destructor *)
DESTRUCTOR TPascalCompiler.Destroy;
BEGIN
{ Release resources. }
  IF SELF.InputStream <> NIL THEN
    SELF.InputStream.Free;
  IF SELF.OutputStream <> NIL THEN
    SELF.OutputStream.Free;
  INHERITED;
END;



INITIALIZATION
  PascalCompiler := TPascalCompiler.Create;



FINALIZATION
  IF PascalCompiler <> NIL THEN
    PascalCompiler.Free;
END.

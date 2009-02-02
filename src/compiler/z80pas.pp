(* Compiler main module. *)
PROGRAM Z80Pas;

{$mode objfpc} {$H+}

USES
  Error, Encoder,
  Z80PasS, Z80PasP;



VAR
  InputFileName: STRING;



(* Shows a "banner" with the name and copyright. *)
  PROCEDURE Title;
  BEGIN
    WriteLn ('Z80 Pascal compiler (WIP version)');
    WriteLn ('(c) 2009 Guillermo Martínez');
    WriteLn;
  END;



(* Shows a help message with description of available options. *)
  PROCEDURE Help;
  BEGIN
    WriteLn ('Usage:');
    WriteLn ('  z80pas <inputfile> <outputfile>');
  END;



(* Checks the parameter list and sets the configuration values. *)
  PROCEDURE CheckParameterList;
  BEGIN
  { TODO: Options. }
  { Input file. }
    InputFileName := ParamStr(1);
  END;



BEGIN
  Title;
{ Checks on correct parameter usage. }
  IF ParamCount < 1 THEN
  BEGIN
    WriteLn('No input file specified');
    Help;
    Exit;
  END;
  CheckParameterList;
{ Opens the sourcefile. }
  Assign (src, InputFileName); {$I-} Reset(src, 1); {$I+}
  IF IOResult <> 0 THEN
  BEGIN
    WriteLn('Could not open input file');
    Exit;
  END;
{ Innitializes the modules. }
  Error.InitModule;
{ Instigate the compilation }
  WriteLn ('Parsing ', InputFileName); Error.CurrentFile := InputFileName;
  Parse;
{ Shows the result. }
  PrintErrors;
  IF Successful THEN
    WriteLn ('Parsed correctly.');
END.

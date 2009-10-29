(* Compiler main module. *)
PROGRAM Z80Pas;

USES
  Compiler, (* THE compiler *)
  sysutils;



VAR
(* Temporal storage for filenames. *)
  OutputFileName, InputFileName: STRING;



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
    IF ParamCount < 2 THEN
      Help;
  { TODO: Options. }
  { Input file. }
    InputFileName := ParamStr(1);
    OutputFileName := ParamStr(2);
  END;


BEGIN
  TRY
    Title;
    CheckParameterList;
    PascalCompiler.Compile (InputFileName);
    WriteLn ('Compilation finished.');
    PascalCompiler.SaveToFile (OutputFileName);
    WriteLn ('File saved at '''+OutputFileName+'''.');
  EXCEPT
    ON Error: Exception DO
      WriteLn (Error.Message);
  END;
END.

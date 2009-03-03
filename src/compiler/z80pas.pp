(* Compiler main module. *)
PROGRAM Z80Pas;

USES
  Compiler, sysutils;



VAR
  OutputFileName, InputFileName: STRING;
(* The compiler. *)
  PascalCompiler: TPascalCompiler;



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
    OutputFileName := ParamStr(0);
  END;



BEGIN
  TRY
    CheckParameterList;
    PascalCompiler.InputFileName := InputFileName;
    PascalCompiler.OutputFileName := OutputFileName;
  EXCEPT
    ON Error: Exception DO
      WriteLn (Error.Message);
  END;
END.

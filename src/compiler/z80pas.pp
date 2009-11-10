(* Compiler main module. *)
PROGRAM Z80Pas;

USES
  Compiler, (* THE compiler *)
  Configuration,
  sysutils;



(* Shows a "banner" with the name and copyright. *)
  PROCEDURE Title;
  BEGIN
    WriteLn ('Z80 Pascal compiler (WIP version)');
    WriteLn ('(c) 2009 Guillermo Martínez');
    WriteLn;
  END;



BEGIN
  TRY
    Title;
    Configuration.Load;
    PascalCompiler.Compile (Configuration.InputFileName);
    WriteLn ('Compilation finished.');
    PascalCompiler.SaveToFile (Configuration.OutputFileName);
    WriteLn ('File saved at '''+Configuration.OutputFileName+'''.');
  EXCEPT
    ON Error: Exception DO
      WriteLn (Error.Message);
  END;
  Configuration.Unload;
END.

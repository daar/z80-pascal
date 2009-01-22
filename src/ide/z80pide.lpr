(* An uncomplete IDE for the Z80-Pascal compiler. *)
PROGRAM z80pide;

{$mode objfpc}{$H+}

USES
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, UnitMainWindow;

BEGIN
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
END.


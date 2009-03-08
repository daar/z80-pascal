UNIT UEncoder;
(*< Defines an "encoder".  It gets the output from the compiler and saves it
    in the output file. *)

INTERFACE

USES
  Classes;



TYPE
(* The encoder. *)
  TEncoder = CLASS (TStringList)
  PUBLIC
  (* Adds a program prologue. *)
    PROCEDURE AddProlog (FileName, ProgramName: STRING);
  (* Adds a program epilogue. *)
    PROCEDURE AddEpilog;

  (* Add a coment line to the output. *)
    PROCEDURE AddComment (Comment: STRING);
  (* Adds an assembler line. *)
    PROCEDURE Emit (aLine: STRING);
  END;



IMPLEMENTATION

(* Adds a program prologue. *)
PROCEDURE TEncoder.AddProlog (FileName, ProgramName: STRING);
BEGIN
  SELF.AddComment ('; Assembler file created by Z80-Pascal.');
  SELF.AddComment (';');
  SELF.AddComment ('; File: '+FileName);
  SELF.AddComment ('; Program name: '+ProgramName);
  SELF.Add ('');
  SELF.AddComment ('This orig was set staticly.');
  SELF.Add ('ORG #F000');
  SELF.Add ('');
END;



(* Adds a program epilogue. *)
PROCEDURE TEncoder.AddEpilog;
BEGIN
  SELF.AddComment ('; Program finishes here.');
  SELF.Emit ('RET');
END;



(* Add a coment line to the output. *)
PROCEDURE TEncoder.AddComment (Comment: STRING);
BEGIN
  SELF.Add ('; '+Comment);
END;



(* Adds a raw assembler line. *)
PROCEDURE TEncoder.Emit (aLine: STRING);
BEGIN
  SELF.Add (''+^I+aLine);
END;



END.

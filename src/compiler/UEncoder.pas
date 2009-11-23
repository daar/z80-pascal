(* Defines an "encoder".  It gets the output from the compiler and saves it
   in the output file. *)
UNIT UEncoder;

INTERFACE

USES
  Classes;



TYPE
(* The encoder. *)
  TEncoder = CLASS (TStringList)
  PUBLIC
  (* Add a coment line to the output. *)
    PROCEDURE AddComment (Comment: STRING);
  (* Adds an assembler line. *)
    PROCEDURE Emit (aLine: STRING);
  END;



IMPLEMENTATION

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

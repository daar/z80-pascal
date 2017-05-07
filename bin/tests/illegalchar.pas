(* A bad program with illegal characters.

   Tests scanner hability to find and show illegal characters. *)
PROGRAM IllegalChar;

  VAR
    Coraçao,          { <-- Non ASCII-7 character. }
    Corazón: INTEGER; { <-- Non ASCII-7 character. }
BEGIN
  WriteLn ("Hello, World!") { <-- Double quote. }
END.

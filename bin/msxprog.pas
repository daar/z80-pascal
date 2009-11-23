(* A simple example program. *)
PROGRAM msxprog;
{ This is all the compiler can handle at the
  moment.  You must wait some months until the
  compiler starts to be usable. }
ASM
  LD A, 65;
  CALL $00A2
END.

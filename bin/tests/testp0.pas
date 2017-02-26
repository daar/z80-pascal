PROGRAM testp0;
(* File to test the parser and the scanner, and even the code generator.

   This file contains most language stuff defined in docs/internal/minipas.atg
   so it doesn't contains all Pascal language.

   The file should be parser without errors and it may generate an executable
   result (once the compiler is complete).

   This file isn't complete (it doesn't contains all productions) so feel free
   to modify and add anything that would help to complete it.
 *)

{ This is to test alternative syntax for comments. }

{
  Not sure how this comment should be managed.  As for ISO it looks that it
  should be managed as if the comment starts and finishes with same comment
  marker (need to read it again though).
*)

(*
  Anyway, I add this other comment.  Note that this may be identified as a
  "nested comment" and so it may be managed as an error.
}

{ In any case, note there are not any ugly C++ style comment. ;) }

(* Test constant definitions (ConstDefinitions). *)
  CONST
    NumericConstant = 10;
    NegativeConstant = -12345;
    AnotherNumeric = -NumericConstant;
    NilConstant = NIL;
    StringConstant = 'A constant'; { Strings will be tested below too. }

(* Test type definitions (TypeDefinitions). *)
  TYPE
    SimpleType = INTEGER;
    ArrayType = ARRAY [0..10] OF INTEGER;
    OtherArrType  = ARRAY [0..NumericConstant, 10..100] OF SimpleType;

(* Test variable definitions (VarDeclarations). *)
  VAR
    Variable: INTEGER;
    ArrayVariable1: Array [0..10] OF INTEGER;
    ArrayVariable2: Array [AnotherNumeric..0] OF ArrayType;

(* Test forwarded procedure. *)
  PROCEDURE ForwardedProcedure; FORWARD;

(* Test forwarded function. *)
  FUNCTION ForwardedFunction: BOOLEAN; FORWARD;




(* The procedure. *)
  PROCEDURE CompleteProcedure (Param1: INTEGER; Param2: ArrayVariable1);
  { Note that any of these parts are optional. }
  CONST
    LocalConstant = 'Example of string';
  VAR
    lVariable, Cnt: INTEGER;
  BEGIN
  { Assignment with integer expression and function use. }
    lVariable := Param1 * Length (Param2);
  { While loop. }
    WHILE lVariable > 0 DO DEC (lVariable);
  { Repeat statement. }
    REPEAT INC (lVariable) UNTIL lVariable > Param1;
  { If statements. }
    IF lVariable = Param1 * Length (Param2) THEN
      WriteLn ('lVariable is big.')
    ELSE
    { Write and WriteLn are special cases. }
      WriteLn ('lVariable is small -> ', lVariable);
    IF lVariable < Length (Param2) THEN
    BEGIN
    { For statement. }
      FOR Cnt := 1 TO Length (Param2) THEN { TODO: DOWNTO loops? }
	Write (Param2[Cnt], '. ');
      WriteLn;
      WriteLn ('lVariable is too small.')
    { See there's no final ";".  Actually Pascal BNF tells there's no such
      character at the end of a Block but modern compilers admits it by adding
      a NOP production to the Statement.  May be I should add it... }
    END
  END;



(* The function. *)
  FUNCTION CompleteFunction (Param1: STRING): STRING;
  { Note that any of these parts are optional. }
  CONST
  { Let's test the quoted string here. }
    ReturnedValue = 'This has ''quotes''.';
  VAR
  { This also tests different variables with same name (see CompleteProcedure).}
    lVariable: STRING;
  BEGIN
  { Assignment with string expression. }
    lVariable := ReturnedValue + StringConstant;
  { Call }
    ForwardedProcedure;
  { Returning by fake procedure call. }
    EXIT (ReturnedValue)
  END;



(* The forwarded procedure body (and without optional parts).

   Note we play a bit with spaces and so.*)
  PROCEDURE
		ForwardedProcedure
  ;
  BEGIN
  { Empty procedure. }
  END;



(* The forwarded function body (without optional parts too). *)
  FUNCTION ForwardedFunction
{ Again playing with spaces
                            just to be sure it works properly. }
  :	BOOLEAN  ; BEGIN
  { MUST return something allways.  This also tests alternative return value
    assignation (Should we add "RESULT" too?). }
    ForwardedFunction
:=
		CompleteFunction
	=      'Hello, World!'
  END;

{ Note current BNF description doesn't allow to add variables here!!!
  See file TODO for more information about this issue. }

BEGIN
  IF NOT ForwardedFunction THEN WriteLn ('Hello, World!')
END.

Any text before the "dot" should be ignored by compiler.

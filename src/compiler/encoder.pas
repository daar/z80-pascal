(* Receives the compiled code and builds the output code. *)
UNIT Encoder;

{$mode objfpc} {$H+}

INTERFACE

(* Adds the given code to the output code. *)
PROCEDURE Emit (Code: STRING);

(* Get all emited code, builds the output code and saves it in the given
   file. *)
PROCEDURE Build (FileName: STRING);

IMPLEMENTATION

USES
  Classes, sysutils;



TYPE
(* Forward declaration needed by TTypeDescription. *)
  TVariableDescription = CLASS;

(* Stores information about a TYPE. *)
  TTypeDescription = CLASS
  PUBLIC
    Name: STRING;
    Fields: ARRAY OF TVariableDescription;
  END;

(* Stores information about a variable. *)
  TVariableDescription = CLASS
  PUBLIC
    Name: STRING;
    DataType: TTypeDescription;
  END;

(* Stores information about a parameter. *)
  TParameterDescription = TVariableDescription;

(* Stores information about a constant. *)
  TConstantDescription = CLASS
  PUBLIC
    Name: STRING;
    Content: STRING;
  END;

(* Stores information about a procedure or function. *)
  TProcedureDescription = CLASS
  PUBLIC
  (* Public description of the procedure or function. *)
    Name: STRING;
    ParameterList: ARRAY OF TParameterDescription;
    ReturnType: TTypeDescription;
  (* Executable code of the procedure or function. *)
    Code: TStringList;
  END;



(* Stores information of an unit. *)
  TUnitDescription = CLASS
  PUBLIC
    Name: STRING;
    TypeList: ARRAY OF TTypeDescription;
    VariableList: ARRAY OF TVariableDescription;
    ConstantList: ARRAY OF TConstantDescription;
    ProcedureList: ARRAY OF TProcedureDescription;
  END;



(* Stores all available units. *)
  TUnitList = CLASS
  PRIVATE
    fList: ARRAY OF TUnitDescription;
  PUBLIC
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
  (* Looks for an unit.  Returns the reference or NIL if it doesn't exists. *)
    FUNCTION FindUnit (aName: STRING): TUnitDescription;
  END;



VAR
(* The actual code. *)
  UnitList: TUnitList;



(* Adds the given code to the output code. *)
PROCEDURE Emit (Code: STRING);
BEGIN
END;



(* Get all emited code, builds the output code and saves it in the given
   file. *)
PROCEDURE Build (FileName: STRING);
BEGIN
END;



(*************
 * TUnitList *
 *************)

(* Constructor. *)
CONSTRUCTOR TUnitList.Create;
BEGIN
  INHERITED Create;
  SetLength (SELF.fList, 1);
  SELF.fList[0] := TUnitDescription.Create;
  SELF.fList[0].Name := UpperCase ('system');
END;



(* Destructor. *)
DESTRUCTOR TUnitList.Destroy;
VAR
  Cnt: INTEGER;
BEGIN
  FOR Cnt := 0 TO (Length (SELF.fList) - 1) DO
    IF SELF.fList[Cnt] <> NIL THEN
      SELF.fList[Cnt].Free;
  INHERITED Destroy;
END;



(* Looks for an unit.  Returns the reference or NIL if it doesn't exists. *)
FUNCTION TUnitList.FindUnit (aName: STRING): TUnitDescription;
VAR
  Cnt: INTEGER;
BEGIN
  FindUnit := NIL;
  FOR Cnt := 0 TO (Length (SELF.fList) - 1) DO
    IF SELF.fList[Cnt].Name = UpperCase (aName) THEN
      FindUnit := SELF.fList[Cnt];
END;



INITIALIZATION
  UnitList := TUnitList.Create;

FINALIZATION
  IF UnitList <> NIL THEN
    UnitList.Free;
END.

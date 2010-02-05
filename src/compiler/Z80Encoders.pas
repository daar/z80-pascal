UNIT Z80Encoders;
(*<Expands the abstract Pascal encoder defining platform targets.  At the
   moment only a generic Z-80 is defined. *)

INTERFACE

USES
  PasCompiler,
  Classes;



TYPE
(* Generiz Z-80 encoder.  Actually it translates P-Code items to Z-80 Assembler
   code.  It also parses "ASM .. END" blocks.
   @seealso(TPascalEncoder) *)
  TZ80Encoder = CLASS (TPascalEncoder)
  PRIVATE
    fOutputStrings: TStringList;
  PUBLIC
  (* Constructor.
     @param(aScanner The scanner.  See @link(TPascalEncoder.Create).) *)
    CONSTRUCTOR Create (aScanner: TPascalLexicalScanner); OVERRIDE;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Add a coment line to the output. *)
    PROCEDURE AddComment (Comment: STRING); OVERRIDE;



  (* Adds an assembler line. *)
    PROCEDURE Emit (aLine: STRING); OVERRIDE;
  (* Saves the output. *)
    PROCEDURE SaveToFile (Filename: STRING);
  END;



IMPLEMENTATION

(***************
 * TZ80Encoder *
 ***************)

(* Constructor.
   @param(aScanner The scanner.  See @link(TPascalEncoder.Create).) *)
  CONSTRUCTOR TZ80Encoder.Create (aScanner: TPascalLexicalScanner);
  BEGIN
    INHERITED Create (aScanner);
    fOutputStrings := TStringList.Create;
  END;



(* Destructor. *)
  DESTRUCTOR TZ80Encoder.Destroy;
  BEGIN
    IF fOutputStrings <> NIL THEN
      fOutputStrings.Destroy;
    INHERITED Destroy;
  END;



(* Add a coment line to the output. *)
  PROCEDURE TZ80Encoder.AddComment (Comment: STRING);
  BEGIN
    fOutputStrings.Add ('; '+Comment);
  END;



(* Adds a raw assembler line. *)
  PROCEDURE TZ80Encoder.Emit (aLine: STRING);
  BEGIN
    fOutputStrings.Add (''+^I+aLine);
  END;

(* Saves the output. *)
  PROCEDURE TZ80Encoder.SaveToFile (Filename: STRING);
  BEGIN
    fOutputStrings.SaveToFile (Filename);
  END;

END.

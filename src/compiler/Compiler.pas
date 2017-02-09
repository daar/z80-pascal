UNIT Compiler;
(*<Defines the Pascal compiler, that loads a file, parses it and saves the
   result in another file. *)
(*
  Copyright (c) 2009, 2010 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
*)
INTERFACE

USES
  PasCompiler,
  Classes,
  Configuration;



TYPE
(* Makes the compiler generate Z-80 Assembler code. *)
  TCompiler = CLASS (TPascalCompiler)
  PRIVATE
  (* The stream to read from. *)
    fFileName: STRING;
    fInputStream: TFileStream;
  (* Puts the input filename, opening and assigning the input stream. *)
    PROCEDURE PutFileName (aFileName: STRING);
  PROTECTED
  (* Implements the Verbose procedure. *)
    PROCEDURE Verbose (Level: VERBOSITY_LEVELS; aMessage: STRING); OVERRIDE;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* It's called by the Encoder when it finds a comment line.
     This inserts the comment to the Assembler output if
     @link(Configuration.ListsComments) is @true. *)
    PROCEDURE AddComment (Comment: STRING); OVERRIDE;
  (* Saves the result on a file. *)
    PROCEDURE SaveToFile (aFileName: STRING);

  (* The Input file. *)
    PROPERTY FileName: STRING READ fFileName WRITE PutFileName;
  END;

  TVerboseMsg = PROCEDURE(Level: VERBOSITY_LEVELS; aMessage: STRING);

VAR
  VerboseMsg: TVerboseMsg;

IMPLEMENTATION

USES
  Z80Encoders;

PROCEDURE VerboseMsg_(Level: VERBOSITY_LEVELS; aMessage: STRING);
BEGIN
  IF Level IN Configuration.Verbose THEN
    WriteLn(aMessage);
END;


(************
 * Compiler *
 ************)

(* Puts the input filename, opening and assigning the input stream. *)
  PROCEDURE TCompiler.PutFileName (aFileName: STRING);
  BEGIN
    IF fInputStream <> NIL THEN
      fInputStream.Free;
    fInputStream := TFileStream.Create (aFileName, fmOpenRead);
    SELF.Scanner.InputStream := fInputStream;
  END;



(* Implements the Verbose procedure. *)
  PROCEDURE TCompiler.Verbose (Level: VERBOSITY_LEVELS; aMessage: STRING);
  BEGIN
    VerboseMsg(Level, aMessage);
  END;



(* Constructor. *)
  CONSTRUCTOR TCompiler.Create;
  BEGIN
  { Creates the object assigning the encoder. }
    INHERITED Create (TZ80Encoder.Create);
  END;



(* Destructor. *)
  DESTRUCTOR TCompiler.Destroy;
  BEGIN
    IF fInputStream <> NIL THEN
      fInputStream.Free;
    INHERITED Destroy;
  END;



(* It's called by the Encoder when it finds a comment line.
   This inserts the comment to the Assembler output if
   @link(Configuration.ListsComments) is @true. *)
  PROCEDURE TCompiler.AddComment (Comment: STRING);
  VAR
    CommentLines: TStringList;
    Cnt: INTEGER;
  BEGIN
    IF Configuration.ListsComments THEN
    BEGIN
    { This way if comment has multiple lines it was inserted correctly. }
      CommentLines := TStringList.Create;
      TRY
	CommentLines.Text := Comment;
	FOR Cnt := 0 TO CommentLines.Count -1 DO
	  Encoder.AddComment (CommentLines[Cnt]);
      FINALLY
	CommentLines.Free;
      END;
    END;
  END;



(* Saves the result on a file. *)
  PROCEDURE TCompiler.SaveToFile (aFileName: STRING);
  BEGIN
    (Encoder AS TZ80Encoder).SaveToFile (aFileName);
  END;

INITIALIZATION
  VerboseMsg := @VerboseMsg_;

END.

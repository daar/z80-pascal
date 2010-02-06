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
  Classes;



TYPE
(* The actual compiler. *)
  TCompiler = CLASS (TPascalCompiler)
  PRIVATE
    fFileName: STRING;
    fInputStream: TFileStream;
  (* Puts the input filename, opening and assigning the input stream. *)
    PROCEDURE PutFileName (aFileName: STRING);
  PROTECTED
  (* Implements the inform procedure. *)
    PROCEDURE Inform (aMessage: STRING); OVERRIDE;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Saves the result on a file. *)
    PROCEDURE SaveToFile (aFileName: STRING);

  (* The Input file. *)
    PROPERTY FileName: STRING READ fFileName WRITE PutFileName;
  END;



IMPLEMENTATION

USES
  Z80Encoders, Configuration;



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



(* Implements the inform procedure. *)
  PROCEDURE TCompiler.Inform (aMessage: STRING);
  BEGIN
    WriteLn (aMessage);
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



(* Saves the result on a file. *)
  PROCEDURE TCompiler.SaveToFile (aFileName: STRING);
  BEGIN
    (Encoder AS TZ80Encoder).SaveToFile (aFileName);
  END;

END.

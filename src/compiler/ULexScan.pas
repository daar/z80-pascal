UNIT ULexScan;
(*< Defines a lexical scanner. *)

INTERFACE

USES
  Classes;



TYPE
(* Lexical scanner. *)
  TLexicalScanner = CLASS
  PRIVATE
    fInputStream: TFileStream;
  (* Lookahead character. *)
    fLookahead: CHAR;
  PUBLIC
  (* Constructor.  Opens the given file as input stream.
     @raises (an exception if file doesn't exists or can't open it.) *)
    CONSTRUCTOR Create (aFileName: STRING);
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;

  (* Gets next character from the stream.  Ignores control characters. *)
    PROCEDURE GetChar;

  (* Checks if the given character is a leter. *)
    FUNCTION isAlpha (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is a digit. *)
    FUNCTION isDigit (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is an hexagesimal digit. *)
    FUNCTION isHexDigit (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is alphanumeric. *)
    FUNCTION isAlphaNum (aChar: CHAR): BOOLEAN;
  (* Checks if the given character is a white space. *)
    FUNCTION isWhite (aChar: CHAR): BOOLEAN;

  (* Skip over leading white spaces. *)
    PROCEDURE SkipWhite;
  (* Skips CR/CF. (TEST) *)
    PROCEDURE Fin;

  (* Returns a name. (TEST) *)
    FUNCTION GetName: STRING;
  (* Returns a number. (TEST) *)
    FUNCTION GetNumber: STRING;
  (* Returns a token. (TEST) *)
    FUNCTION GetToken: STRING;
  END;


CONST
(* Fool constants for control characters. (TEST) *)
  TAB = ^I;
  CR = ^M;
  LF = ^J;

IMPLEMENTATION

USES
  sysutils;


(* Constructor.  Opens the given file as input stream. *)
CONSTRUCTOR TLexicalScanner.Create (aFileName: STRING);
BEGIN
  SELF.fInputStream := TFileStream.Create (aFileName, fmOpenRead);
  SELF.GetChar;
END;



(* Destructor. *)
DESTRUCTOR TLexicalScanner.Destroy;
BEGIN
  IF SELF.fInputStream <> NIL THEN
    SELF.fInputStream.Free;
  INHERITED;
END;



(* Gets a simple character from the stream. *)
PROCEDURE TLexicalScanner.GetChar;
VAR
  Character: BYTE;
BEGIN
  Character := SELF.fInputStream.ReadByte;
  SELF.fLookahead := CHAR (Character);
END;



(* Checks if the given character is a leter. *)
FUNCTION TLexicalScanner.isAlpha (aChar: CHAR): BOOLEAN;
BEGIN
  RESULT := aChar IN ['A'..'Z', 'a'..'z'];
END;



(* Checks if the given character is a digit. *)
FUNCTION TLexicalScanner.isDigit (aChar: CHAR): BOOLEAN;
BEGIN
  RESULT := aChar IN ['0'..'9'];
END;



(* Checks if the given character is an hexagesimal digit. *)
FUNCTION TLexicalScanner.isHexDigit (aChar: CHAR): BOOLEAN;
BEGIN
  RESULT := SELF.isDigit (aCHAR) OR (aChar IN ['A'..'F', 'a'..'f']);
END;



(* Checks if the given character is alphanumeric. *)
FUNCTION TLexicalScanner.isAlphaNum (aChar: CHAR): BOOLEAN;
BEGIN
  RESULT := SELF.isAlpha (aChar) OR SELF.isDigit (aChar);
END;



(* Checks if the given character is a white space. *)
FUNCTION TLexicalScanner.isWhite (aChar: CHAR): BOOLEAN;
BEGIN
  RESULT := aChar IN [' ', TAB];
END;



(* Skip over leading white spaces. *)
PROCEDURE TLexicalScanner.SkipWhite;
BEGIN
  WHILE SELF.isWhite (fLookahead) DO
    SELF.GetChar;
END;



(* Skips CR/CF. (TEST) *)
PROCEDURE TLexicalScanner.Fin;
BEGIN
  WHILE fLookahead IN [CR, LF] DO
    SELF.GetChar;
END;

(* Returns a name. (TEST) *)
FUNCTION TLexicalScanner.GetName: STRING;
VAR
  Tmp: STRING;
BEGIN
  Tmp := '';
  IF NOT SELF.isAlpha (fLookahead) THEN
    RAISE Exception.Create ('Name expected!');
  REPEAT
    Tmp := Tmp + UpCase (fLookahead);
    SELF.GetChar;
  UNTIL NOT SELF.isAlphaNum (fLookahead);
  RESULT := Tmp;
END;



(* Returns a number. (TEST) *)
FUNCTION TLexicalScanner.GetNumber: STRING;
VAR
  Tmp: STRING;
BEGIN
  Tmp := '';
  IF NOT SELF.isDigit (fLookahead) THEN
    RAISE Exception.Create ('Number expected!');
  REPEAT
    Tmp := Tmp + fLookahead;
    SELF.GetChar;
  UNTIL NOT SELF.isDigit (fLookahead);
  RESULT := Tmp;
END;



(* Returns a token. (TEST) *)
FUNCTION TLexicalScanner.GetToken: STRING;
BEGIN
  WHILE (fLookahead = CR) OR (fLookahead = LF) DO Fin;
  IF SELF.isAlpha (fLookahead) THEN
    RESULT := SELF.GetName
  ELSE IF SELF.isDigit (fLookahead) THEN
    RESULT := SELF.GetNumber
  ELSE BEGIN
    RESULT := fLookahead;
    SELF.GetChar;
  END;
  SkipWhite;
END;



END.

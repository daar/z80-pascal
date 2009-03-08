UNIT ULexScan;
(*< Defines a lexical scanner. *)

INTERFACE

USES
  Classes;



TYPE
(* Token identificators. *)
  TTokenId = ( tiIdentifier, tiInteger, tiHexInteger, tiReal, tiString,
		tiOther );

(* Lexical scanner.
   It recognices the first part of the BNF definition.
   All identifiers converted to all-uppercase. *)
  TLexicalScanner = CLASS
  PRIVATE
    fInputStream: TFileStream;
  (* Lookahead character. *)
    fLookahead: CHAR;
  (* Last token read. *)
    fLastTokenString: STRING;
    fLastTokenId: TTokenId;
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

  (* Returns any token. *)
    FUNCTION GetToken: STRING;
  (* Returns an identifier. *)
    FUNCTION GetIdentifier: STRING;
  (* Returns a number. *)
    FUNCTION GetNumber: STRING;

  (* Returns the last character read. *)
    PROPERTY Lookahead: CHAR READ fLookahead;
  (* Returns the last parsed token. *)
    PROPERTY LastToken: STRING READ fLastTokenString;
  (* Returns the last parsed token. *)
    PROPERTY LastTokenId: TTokenId READ fLastTokenId;
  END;



IMPLEMENTATION

USES
  sysutils;



CONST
(* Fool constants for control characters. (TEST) *)
  TAB = ^I;
  CR = ^M;
  LF = ^J;



(* Constructor.  Opens the given file as input stream. *)
CONSTRUCTOR TLexicalScanner.Create (aFileName: STRING);
BEGIN
  SELF.fInputStream := TFileStream.Create (aFileName, fmOpenRead);
  SELF.GetChar;
  SkipWhite;
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
  //WriteLn ('Lh := ''', fLookahead, '''');
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
  //WriteLn ('SkipWhite...');
  WHILE SELF.isWhite (fLookahead) DO
    SELF.GetChar;
END;



(* Skips CR/CF. (TEST) *)
PROCEDURE TLexicalScanner.Fin;
BEGIN
  WHILE fLookahead IN [CR, LF] DO
    SELF.GetChar;
  SELF.SkipWhite;
END;



(* Returns any token. *)
FUNCTION TLexicalScanner.GetToken: STRING;
BEGIN
  WHILE (fLookahead = CR) OR (fLookahead = LF) DO Fin;
  IF SELF.isAlpha (Lookahead) THEN
    RESULT := SELF.GetIdentifier
  ELSE IF SELF.isDigit (Lookahead) OR (Lookahead = '$') THEN
    RESULT := SELF.GetNumber
  ELSE BEGIN
    SELF.fLastTokenString := fLookahead;
    SELF.fLastTokenId := tiOther;
    RESULT := SELF.fLastTokenString;
    SELF.GetChar;
    SELF.SkipWhite;
    //WriteLn ('Last Token : ''', fLastTokenString, '''');
  END;
END;



(* Returns an identifier. *)
FUNCTION TLexicalScanner.GetIdentifier: STRING;
BEGIN
  WHILE (fLookahead = CR) OR (fLookahead = LF) DO Fin;
  fLastTokenString := '';
  IF NOT SELF.isAlpha (fLookahead) THEN
    RAISE Exception.Create ('Name expected!');
  fLastTokenId := tiIdentifier;
  REPEAT
    fLastTokenString := fLastTokenString + UpCase (fLookahead);
    SELF.GetChar;
  UNTIL NOT SELF.isAlphaNum (fLookahead);
  RESULT := fLastTokenString;
  SkipWhite;
  //WriteLn ('Last Token : ''', fLastTokenString, '''');
END;



(* Returns a number. *)
FUNCTION TLexicalScanner.GetNumber: STRING;

  PROCEDURE HexNum;
  BEGIN
    REPEAT
      fLastTokenString := fLastTokenString + fLookahead;
      SELF.GetChar;
    UNTIL NOT SELF.isHexDigit (fLookahead);
  END;


  PROCEDURE IntegerNum;
  BEGIN
    REPEAT
      fLastTokenString := fLastTokenString + fLookahead;
      SELF.GetChar;
    UNTIL NOT SELF.isDigit (fLookahead);
  END;

BEGIN
  WHILE (fLookahead = CR) OR (fLookahead = LF) DO Fin;
  fLastTokenString := '';
  IF NOT SELF.isDigit (fLookahead) AND (fLookahead <> '$') THEN
    RAISE Exception.Create ('Number expected!');
  IF fLookahead = '$' THEN
  BEGIN
    fLastTokenId := tiHexInteger;
    HexNum;
  END
  ELSE BEGIN
    fLastTokenId := tiInteger;
    IntegerNum;
  END;
  RESULT := fLastTokenString;
  SkipWhite;
  //WriteLn ('Last Token : ''', fLastTokenString, '''');
END;



END.

(* Defines a lexical scanner. *)
UNIT ULexScan;

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
    fInputStream: TStream;
  (* Lookahead character. *)
    fLookahead: CHAR;
  (* Advance next character. *)
    fNext: STRING;
  (* Last token read. *)
    fLastTokenString: STRING;
    fLastTokenId: TTokenId;
  (* To store comments. *)
    fComments: TStringList;

  (* The "StoreComments" property. *)
    FUNCTION GetStoreComments: BOOLEAN;
    PROCEDURE PutStoreComments (Value: BOOLEAN);
  (* To get the next character. *)
    FUNCTION GetNextCharacter: CHAR;
  PUBLIC
  (* Constructor.  Uses the given stream as input.
     @param(aStoreComments sets the StoreComments property.
       Default to @false). *)
    CONSTRUCTOR Create (aStream: TStream; aStoreComments: BOOLEAN = FALSE);
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

  (* Skip over leading white spaces and comments. *)
    PROCEDURE SkipWhite;
  (* Skips CR/CF. (TEST) *)
    PROCEDURE Fin;
  (* Skips a separator (i.e.: ',', '+', '(', etc. *)
    PROCEDURE SkipCharacter;

  (* Returns any token. *)
    FUNCTION GetToken: STRING;
  (* Returns an identifier. *)
    FUNCTION GetIdentifier: STRING;
  (* Returns a number. *)
    FUNCTION GetNumber: STRING;

  (* Returns the last character read. *)
    PROPERTY Lookahead: CHAR READ fLookahead;
  (* Returns the next character to read.  Needed for some tokens (i.e. := and comments) *)
    PROPERTY NextCharacter: CHAR READ GetNextCharacter;
  (* Returns the last parsed token. *)
    PROPERTY LastToken: STRING READ fLastTokenString;
  (* Returns the last parsed token. *)
    PROPERTY LastTokenId: TTokenId READ fLastTokenId;
  (* Tells the scanner if should store the comments it finds. *)
    PROPERTY StoreComments: BOOLEAN READ GetStoreComments WRITE PutStoreComments;
  (* If property StoreComments is @true, it stores the comments it finds
     here. *)
    PROPERTY Comments: TStringList READ fComments;
  END;



IMPLEMENTATION

USES
  sysutils;



CONST
(* Fool constants for control characters. (TEST) *)
  TAB = ^I;
  CR = ^M;
  LF = ^J;



(* Returns TRUE if it stores comments. *)
  FUNCTION TLexicalScanner.GetStoreComments: BOOLEAN;
  BEGIN
    RESULT := fComments <> NIL;
  END;



(* Activates or deactivates the comment storage. *)
  PROCEDURE TLexicalScanner.PutStoreComments (Value: BOOLEAN);
  BEGIN
    IF Value THEN
    BEGIN
      IF fComments = NIL THEN
	fComments := TStringList.Create;
    END
    ELSE IF fComments <> NIL THEN
    BEGIN
      fComments.Free;
      fComments := NIL;
    END;
  END;



(* To get the next character. *)
  FUNCTION TLexicalScanner.GetNextCharacter: CHAR;
  VAR
    Character: BYTE;
  BEGIN
    IF fNext = '' THEN
    BEGIN
      Character := fInputStream.ReadByte;
      fNext := ' ';
      fNext[1] := CHAR (Character);
    END;
    RESULT := fNext[1];
  END;



(* Constructor.  Uses the given stream as input. *)
  CONSTRUCTOR TLexicalScanner.Create (aStream: TStream; aStoreComments: BOOLEAN);
  BEGIN
    PutStoreComments (aStoreComments);
    fInputStream := aStream;
    SELF.GetChar;
    SkipWhite;
  END;



(* Destructor. *)
  DESTRUCTOR TLexicalScanner.Destroy;
  BEGIN
    IF fComments <> NIL THEN
      fComments.Free;
    INHERITED;
  END;



(* Gets a simple character from the stream. *)
  PROCEDURE TLexicalScanner.GetChar;
  VAR
    Character: BYTE;
  BEGIN
  { Checks if it has read the next character yet. }
    IF fNext = '' THEN
      Character := fInputStream.ReadByte
    ELSE BEGIN
      Character := BYTE (fNext[1]);
      fNext := '';
    END;
    fLookahead := CHAR (Character);
  // To help debug.  Should be deleted on final release.
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

  (* Checks and skips a comment. *)
    PROCEDURE SkipComment;

      FUNCTION IsBeginComment: BOOLEAN;
      BEGIN
	RESULT := (fLookahead = '{')
		OR ((fLookahead = '(') AND (SELF.NextCharacter = '*'));
	IF RESULT THEN
	BEGIN
	  SELF.GetChar;
	  IF fLookahead = '*' THEN
	    SELF.GetChar;
	END;
      END;

      FUNCTION IsEndComment: BOOLEAN;
      BEGIN
	RESULT := (fLookahead = '}')
		OR ((fLookahead = '*') AND (SELF.NextCharacter = ')'));
	IF RESULT THEN
	BEGIN
	  SELF.GetChar;
	  IF fLookahead = ')' THEN
	    SELF.GetChar;
	END;
      END;

    VAR
      Comment, Tmp: STRING;
    BEGIN
      Comment := '';
      Tmp := ' ';
      WHILE IsBeginComment DO
	REPEAT
	  IF fComments <> NIL THEN
	  BEGIN
	    Tmp[1] := fLookahead;
	    Comment := Comment + Tmp;
	  END;
	  SELF.GetChar;
	UNTIL IsEndComment;
      IF (fComments <> NIL) AND (Comment <> '') THEN
	fComments.Text := Comment;
    END;

  BEGIN
    SkipComment;
    WHILE SELF.isWhite (fLookahead) DO
    BEGIN
      SELF.GetChar;
      SkipComment;
    END;
  END;



(* Skips CR/CF. (TEST) *)
  PROCEDURE TLexicalScanner.Fin;
  BEGIN
    WHILE fLookahead IN [CR, LF] DO
      SELF.GetChar;
    SELF.SkipWhite;
  END;



(* Skips a separator (i.e.: ',', '+', '(', etc. *)
  PROCEDURE TLexicalScanner.SkipCharacter;
  BEGIN
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
      fLastTokenString := fLookahead;
      fLastTokenId := tiOther;
      RESULT := fLastTokenString;
      SELF.GetChar;
      SELF.SkipWhite;
    // To help debug.  Should be deleted on final release.
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
  // To help debug.  Should be deleted on final release.
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
  // To help debug.  Should be deleted on final release.
  //WriteLn ('Last Token : ''', fLastTokenString, '''');
  END;

END.
